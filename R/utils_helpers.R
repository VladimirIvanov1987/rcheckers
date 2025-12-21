#' @import shiny
NULL

# Локализация ====

#' Get Localized String
#'
#' Retrieves a localized string based on the provided key and locale.
#' Reads from inst/i18n/locales.csv.
#'
#' @param key The identifier for the string.
#' @param locale The language code ("en" or "ru"). Defaults to "ru".
#'
#' @return A character string.
#' @export
#' @importFrom utils read.csv
get_localization <- function(key, locale = "ru") {
  # Путь к файлу внутри установленного пакета

  loc_file <- system.file("i18n", "locales.csv", package = "rcheckers")

  # Если файл не найден (например, при разработке до установки), ищем локально
  if (loc_file == "") {
    loc_file <- "inst/i18n/locales.csv"
  }

  if (!file.exists(loc_file)) {
    warning("Localization file not found.")
    return(key)
  }


  df <- read.csv(loc_file, stringsAsFactors = FALSE,encoding = "UTF-8")
  row <- df[df$key == key, ]

  if (nrow(row) == 0) {
    warning(paste("Key not found:", key))
    return(key)
  }

  if (locale %in% names(row)) {
    return(row[[locale]])
  } else {
    return(row[["en"]]) # Fallback to English
  }
}
