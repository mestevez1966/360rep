library(rtweet)
library(curl)
library(dplyr)
library(jsonlite)
library(DBI)
library(RSQLite)

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


# Descarga de tweets de la empresa y datos de usuarios de cada tweet
tweets <- rtweet::search_tweets(paste0("@", "repsol"), token = auth, include_rts = FALSE,  n = 30, type = "recent")

pol <- apply(tweets[, c("full_text"), drop = FALSE], 1, meaningcloud)
pol <- dplyr::bind_rows(pol)


 write.csv2(pol, file = paste0("archivos/", "test", format(Sys.time(),'_%Y_%m_%d_%H_%M_%S'), ".csv"), row.names = FALSE, na = "")
