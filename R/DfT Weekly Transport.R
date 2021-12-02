library(magrittr)

fetch_dft_mobility_data <- function() {
  dft_mobility_url <- "https://www.gov.uk/government/statistics/transport-use-during-the-coronavirus-covid-19-pandemic"

  dft_data_url <- rvest::read_html(dft_mobility_url) %>%
    rvest::html_element(".thumbnail") %>%
    rvest::html_attr("href")

  # check only one url collected and is .ods
  if (length(dft_data_url) == 1 &
      tools::file_ext(dft_data_url) == "ods") {
    if (!dir.exists("data")) {
      dir.create("data")
    }

    # Depending on OS platform you may need to change the method.
    # See ?download.file for more information
    download.file(dft_data_url, "data/dft.ods", method = "curl")
  } else {
    stop("More than one URL collected or not a .ODS file")
  }
}

process_dft_mobility_data <- function(update = FALSE, path = NULL) {
  if (update) {
    fetch_dft_mobility_data()
  }

  if (is.null(path)) {
    path <- "data/dft.ods"
  }

  data <- readODS::read_ods(path, skip = 6, na = "..") %>%
    tibble::tibble()
  names(data) <- stringr::str_sub(names(data), 1, stringr::str_locate(names(data), stringr::fixed("["))[, 1] - 1)
  if (any(is.na(as.Date(data$Date, format = "%d/%m/%Y")))) {
    data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
  } else {
    data$Date <- as.Date(data$Date, format = "%d/%m/%Y")
  }

  dft <- list()
  dft$data <- data
  dft$subtitle <- paste("Relative to pre-Covid equivalents.\nLast date in data:",
                        format(max(dft$data$Date), "%A %e %B %Y"))
  dft$caption <- paste("Source: Department for Transport: Transport use during the coronavirus (COVID-19) pandemic.\nAccessed", format(Sys.Date(), "%e %B %Y"))

  return(dft)
}
