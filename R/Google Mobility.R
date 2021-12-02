library(magrittr)

fetch_google_mobility_data <- function(path = "data") {
  urls <- list()
  urls$global_csv <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
  urls$region_zip <- "https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip"

  # create tempdir and download zipped region CSVs
  tempdir <- tempdir()
  google_zip_path <- paste0(tempdir, "/", basename(urls$region_zip))
  download.file(urls$region_zip, google_zip_path)

  # get contents of zip folder and identify paths of GB data
  zipped_files <- unzip(google_zip_path, list = TRUE)
  gb_files <- zipped_files$Name[grepl("GB", zipped_files$Name)]

  # unzip only GB data and extract to path, creating if necessary
  if (!dir.exists(path)) {
    dir.create(path)
  }

  google_csv_path <- paste0(path, "/", basename(tools::file_path_sans_ext(google_zip_path)))
  unzip(google_zip_path, files = gb_files, exdir = google_csv_path)
}


process_google_mobility_data <- function(update = FALSE, path = NULL) {
  if (update) {
    fetch_google_mobility_data()
  }

  if (is.null(path)) {
    path <- "data/Region_Mobility_Report_CSVs"
  }

  data <- lapply(list.files(path, full.names = TRUE),
                 function(f) {
                   readr::read_csv(f, col_types = "ccccccccDnnnnnn")
                 }
  ) %>%
    dplyr::bind_rows() %>%
    tidyr::pivot_longer(cols = 10:15, names_to = "type") %>%
    dplyr::mutate(type = type %>%
                    stringr::str_remove("_percent_change_from_baseline") %>%
                    stringr::str_replace_all("_", " ") %>%
                    stringr::str_to_sentence()
    )
  google_mobility <- list()
  google_mobility$data <- data
  google_mobility$subtitle <- paste("Relative to median value from 3 January to 6 February 2020, Mondays to Fridays only.\nLast date in data:", format(max(google_mobility$data$date), "%A %e %B %Y"))
  google_mobility$caption <- paste("Google LLC Google COVID-19 Community Mobility Reports https://www.google.com/covid19/mobility/", "\nAccessed:", format(Sys.Date(), "%e %B %Y"))
  return(google_mobility)
}
