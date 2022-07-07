library(rtweet)
library(curl)
library(dplyr)
library(jsonlite)
library(DBI)
library(RSQLite)

# Ideas:
# https://public.graphext.com/f1dd11b4acb1e181/index.html

# Auth bearer token
auth <- rtweet::rtweet_app(bearer_token = Sys.getenv("BEARER_TOKEN"))

#---------------
# MEANINGCLOUD
#---------------

# Función MeaningCloud
meaningcloud <- function(texto, token = Sys.getenv("MEANING_TOKEN")) {
  # Post
  h <- curl::new_handle()
  curl::handle_setform(h,
                       key = token,
                       txt = texto,
                       lang = "auto")
  
  # print(texto)
  
  req <- curl::curl_fetch_memory("https://api.meaningcloud.com/reputation-2.0", handle = h)
  
  req <- req$content %>%
    rawToChar() %>%
    jsonlite::prettify() %>%
    jsonlite::fromJSON() %>%
    as.data.frame
  
  polarity <- req$entity_list.category_list[[1]]$polarity[1]
  dimension <- req$entity_list.category_list[[1]]$code[1]
  
  df <- data.frame(matrix(ncol = 2, nrow = 1))
  colnames(df) <- c("polarity", "dimension")
  df$polarity <- polarity
  df$dimension <- dimension
  
  return(df)
}


# Función de descarga
download_t <- function(company = "repsol",
                       id_company = 1,
                       competence =  c("Iberdrola", "Naturgy", "Endesa"),
                       id_competence = c(100000, 100001, 100002),
                       type,
                       n = 300,
                       test = FALSE) {
  
  if(type == "company") {
    
    # Descarga de tweets de la empresa y datos de usuarios de cada tweet
    tweets <- rtweet::search_tweets(paste0("@", company), token = auth, include_rts = FALSE,  n = n, type = "recent")
    
    # Identificador de la compañía
    tweets$company_id <- id_company
    
  }
  
  
  if(type == "competence") {
    
    tweets <- vector("list", length = length(competence))
    
    for(i in 1:length(competence)){
      
      tweets[[i]] <- rtweet::search_tweets(paste0("@", competence[i]), token = auth, include_rts = FALSE,  n = n, type = "recent")
      tweets[[i]]$competitor_id <- id_competence[i]
      
    }
    
    tweets <- do.call(rbind, tweets)
    
    # Descarga de tweets de competencia y datos de usuarios de cada tweet
    # tweets <- rtweet::search_tweets2(paste0("@", competence), token = auth, include_rts = FALSE, n = n, type = "recent")
  }
  
  # a <- data.table::rbindlist(tweets$metadata)
  
  users <- rtweet::users_data(tweets)
  
  # Cambiamos los nombres de columna para que no se repitan
  colnames(users) <- paste0(colnames(users), "_user")
  
  # Unimos
  data_tweets <- cbind(tweets, users)
  
  # Nueva variable con los links de los tweets
  data_tweets$tweet_link <- paste0("https://twitter.com/", data_tweets$screen_name_user, "/status/", data_tweets$id_str)
  
  # Modificamos la columna source
  source <- lapply(data_tweets$source,
                   FUN = function(x)
                     strsplit(x, split = ">")[[1]][2])
  source <- gsub('for ', '', gsub('Twitter ', '', source))
  source <- gsub('</a', '', source)
  
  data_tweets$source <- source
  
  # Cambiamos el nombre para evitar conflictos con MySQL
  colnames(data_tweets)[9] <- "tweet_source"
  
  # Fechas UTC
  attr(data_tweets$created_at, "tzone") <- "UTC"
  
  # Eliminamos posibles duplicados
  new_data <- data_tweets[!duplicated(data_tweets), ]
  
  
  # Obtenemos las dimensiones y polaridades
  #pol <- apply(new_data[, c("full_text"), drop = FALSE], 1, meaningcloud)
  #pol <- dplyr::bind_rows(pol)
  
  pol <- data.frame(polarity = NA, dimension = NA)               
                   
  # Unimos con el resto de datos
  new_data <- cbind(new_data, pol)
  
  # Convertimos las listas en JSON
  new_data <- data.frame(apply(new_data, 2, function(y) sapply(y, function(x) ifelse(is.list(x), toJSON(x), x))))
  class(new_data$created_at) <- c('POSIXt','POSIXct')
  
  
  #-------------
  # Guardamos
  #-------------
  
  if(type == "company") {
    
    # Reordenamos la columna company_id
    new_data <- new_data %>%
      relocate(company_id, .after = last_col())
    
    
    orden <- c("created_at",
               "id",	
               "id_str",
               "full_text"	,
               "truncated",
               "display_text_range",
               "entities",
               "metadata",
               "tweet_source",
               "in_reply_to_status_id",
               "in_reply_to_status_id_str",
               "in_reply_to_user_id",
               "in_reply_to_user_id_str",
               "in_reply_to_screen_name",
               "geo",
               "coordinates",
               "place",
               "contributors",
               "is_quote_status",	
               "retweet_count",	
               "favorite_count",
               "favorited",
               "retweeted",
               "lang",
               "possibly_sensitive",
               "quoted_status_id",
               "quoted_status_id_str",
               "quoted_status",
               "text",
               "favorited_by",
               "scopes",
               "display_text_width",
               "retweeted_status",
               "quoted_status_permalink",
               "quote_count",
               "timestamp_ms",
               "reply_count",
               "filter_level",
               "query",
               "withheld_scope",
               "withheld_copyright",
               "withheld_in_countries",
               "possibly_sensitive_appealable",
               "id_user",
               "id_str_user",
               "name_user",
               "screen_name_user",
               "location_user",
               "description_user",
               "url_user",
               "protected_user",
               "followers_count_user",
               "friends_count_user",
               "listed_count_user",
               "created_at_user",
               "favourites_count_user",
               "verified_user",
               "statuses_count_user",
               "profile_image_url_https_user",
               "profile_banner_url_user",
               "default_profile_user",
               "default_profile_image_user",
               "withheld_in_countries_user",
               "derived_user",
               "withheld_scope_user",
               "entities_user",
               "tweet_link",
               "polarity",
               "dimension",
               "company_id")
    
    
    
    remove <- which(!(orden %in% colnames(new_data)))
    
    if(length(remove) > 0) {
      
      orden <- orden[-remove]
    }
    
    
    new_data <- new_data[ , orden]
    
    
    
    write.csv2(new_data, file = paste0("archivos/", "company", format(Sys.time(),'_%Y_%m_%d_%H_%M_%S'), ".csv"), row.names = FALSE, na = "")
    
    
    # Guardamos en una base de datos SQL
    mydb <- dbCoCnnect(RSQLite::SQLite(), "archivos/db_tweets.sqlite", extended_types = TRUE)
    
    # Si la tabla ya existe agregamos las nuevas filas, sino creamos la tabla
    if("tweets" %in% dbListTables(mydb)) {
    #   
      dbAppendTable(mydb, "tweets", new_data, row.names = NULL, append = TRUE)
    #   
    } else {
    #   
    #   # Tabla data, data frame new_data
      dbWriteTable(mydb, "tweets", new_data)
    #   
    }
    # 
    # # Desconectamos
    dbDisconnect(mydb)
    # 
    
    
  } else {
    
    # Reordenamos la columna competitor_id
    new_data <- new_data %>%
      relocate(competitor_id, .after = last_col())
    
    
    
    orden <- c("created_at",
               "id",	
               "id_str",
               "full_text"	,
               "truncated",
               "display_text_range",
               "entities",
               "metadata",
               "tweet_source",
               "in_reply_to_status_id",
               "in_reply_to_status_id_str",
               "in_reply_to_user_id",
               "in_reply_to_user_id_str",
               "in_reply_to_screen_name",
               "geo",
               "coordinates",
               "place",
               "contributors",
               "is_quote_status",	
               "retweet_count",	
               "favorite_count",
               "favorited",
               "retweeted",
               "lang",
               "possibly_sensitive",
               "quoted_status_id",
               "quoted_status_id_str",
               "quoted_status",
               "text",
               "favorited_by",
               "scopes",
               "display_text_width",
               "retweeted_status",
               "quoted_status_permalink",
               "quote_count",
               "timestamp_ms",
               "reply_count",
               "filter_level",
               "query",
               "withheld_scope",
               "withheld_copyright",
               "withheld_in_countries",
               "possibly_sensitive_appealable",
               "id_user",
               "id_str_user",
               "name_user",
               "screen_name_user",
               "location_user",
               "description_user",
               "url_user",
               "protected_user",
               "followers_count_user",
               "friends_count_user",
               "listed_count_user",
               "created_at_user",
               "favourites_count_user",
               "verified_user",
               "statuses_count_user",
               "profile_image_url_https_user",
               "profile_banner_url_user",
               "default_profile_user",
               "default_profile_image_user",
               "withheld_in_countries_user",
               "derived_user",
               "withheld_scope_user",
               "entities_user",
               "tweet_link",
               "polarity",
               "dimension",
               "competitor_id")
    
    
    remove <- which(!(orden %in% colnames(new_data)))
    
    if(length(remove) > 0) {
      
      orden <- orden[-remove]
    }
    
    
    new_data <- new_data[ ,orden]
    
    
    
    
    write.csv2(new_data, file = paste0("archivos/", "competence", format(Sys.time(),'_%Y_%m_%d_%H_%M_%S'), ".csv"), row.names = T, na = "")
    
    
    # Guardamos en una base de datos SQL
    mydb_c <- dbConnect(RSQLite::SQLite(), "archivos/db_competencia.sqlite", extended_types = TRUE)

    # Si la tabla ya existe agregamos las nuevas filas, sino creamos la tabla
    if("tweets_competencia" %in% dbListTables(mydb_c)) {

      dbAppendTable(mydb_c, "tweets_competencia", new_data, row.names = NULL, append = TRUE)
    #
    } else {
    #
      # Tabla data, data frame new_data
      dbWriteTable(mydb_c, "tweets_competencia", new_data)
    #
    }

    # Desconectamos
    dbDisconnect(mydb_c)
    
    
  }
  
}


# Ejecutamos
download_t(type = "company", n = 250)
download_t(type = "competence", n = 250)
