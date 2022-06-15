
library(rtweet)
library(curl)
library(dplyr)
library(jsonlite)
# library(DBI)

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

  req <- curl::curl_fetch_memory(Sys.getenv("ENDPOINT_TOKEN"), handle = h)

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
                       id_competence_base = 100000,
                       type,
                       test = FALSE) {

  if(type == "company") {

    # Descarga de tweets de la empresa y datos de usuarios de cada tweet
    tweets <- rtweet::search_tweets(paste0("@", company), token = auth, include_rts = FALSE,  n = 300, type = "recent")

  }

  if(type == "competence") {

    # Descarga de tweets de competencia y datos de usuarios de cada tweet
    tweets <- rtweet::search_tweets2(paste0("@", competence), token = auth, include_rts = FALSE, n = 300, type = "recent")
  }

  # a <- data.table::rbindlist(tweets$metadata)

  users <- rtweet::users_data(tweets)

  # Cambiamos los nombres de columna para que no se repitan
  colnames(users) <- paste0(colnames(users), "_user")

  # Unimos
  data <- cbind(tweets, users)

  # Nueva variable con los links de los tweets
  data$tweet_link <- paste0("https://twitter.com/", data$screen_name_user, "/status/", data$id_str)

  # Modificamos la columna source
  source <- lapply(data$source,
                   FUN = function(x)
                     strsplit(x, split = ">")[[1]][2])
  source <- gsub('for ', '', gsub('Twitter ', '', source))
  source <- gsub('</a', '', source)

  data$source <- source

  # Cambiamos el nombre para evitar conflictos con MySQL
  colnames(data)[9] <- "tweet_source"

  # Fechas UTC
  attr(data$created_at, "tzone") <- "UTC"

  # Eliminamos posibles duplicados
  new_data <- data[!duplicated(data), ]


  if(test == TRUE) {

    new_data$polarity <- sample(c("N+", "N", "NEU", "NONE", "P", "P+", NA), nrow(new_data), replace = TRUE)
    new_data$dimension <- sample(c("Gobernanza", "Innovación", "Ciudadanía",
                                   "Productos y servicios", "Lugar de trabajo",
                                   "Liderazgo", "Rendimiento", NA), nrow(new_data), replace = TRUE)

  } else {

    # Obtenemos las dimensiones y polaridades
    pol <- apply(new_data[, c("full_text"), drop = FALSE], 1, meaningcloud)
    pol <- dplyr::bind_rows(pol)

    # Unimos con el resto de datos
    new_data$polarity <- pol$polarity
    new_data$dimension <- pol$dimension

  }

  if(type == "company") {

    # Identificador de la compañía
    new_data$company_id <- id_company

  } else {

    # Generamo los ID de la competencia
    new_data$competitor_id <- NULL

    for(i in 1:length(competence)) {

      comp_index <- grepl(competence[i], rownames(new_data), fixed = TRUE)

      new_data$competitor_id[comp_index] <- id_competence_base - 1 + i

    }

  }

  # Convertimos las listas en JSON
  new_data <- data.frame(apply(new_data, 2, function(y) sapply(y, function(x) ifelse(is.list(x), toJSON(x), x))))
  class(new_data$created_at) <- c('POSIXt','POSIXct')


  # Guardamos

  if(type == "company") {

    write.csv2(new_data, file = paste0("/archivos/", gsub("@", "", company), format(Sys.time(),'_%Y_%m_%d_%H_%M_%S'), ".csv"), row.names = FALSE, na = "")

  } else {

    write.csv2(new_data, file = paste0("/archivos/", "competence", format(Sys.time(),'_%Y_%m_%d_%H_%M_%S'), ".csv"), row.names = FALSE, na = "")


  }

}


# Ejecutamos
download_t(type = "company", test = T)
download_t(type = "competence", test = T)
