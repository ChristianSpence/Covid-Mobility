library(magrittr)

google_mobility_summary <- function(sub_region_1_name = NULL, min_date = NULL) {
  if (!exists("google_mobility")) {
    google_mobility <- process_google_mobility_data()
  }

  if (is.null(sub_region_1_name)) {
    sub_region_1_name <- "Greater Manchester"
  }

  if (is.null(min_date)) {
    min_date <- min(google_mobility$data$date)
  } else {
    min_date <- as.Date(min_date)
  }

  ggplot_theme <- ggplot2::theme(
    panel.background   = ggplot2::element_blank(),
    panel.grid         = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(linetype = "dotted"),
    legend.position    = "top",
    axis.line.y.right  = NULL,
    axis.line          = ggplot2::element_line(),
    strip.background   = ggplot2::element_blank()
  )

  ons_lookup <- readr::read_csv("https://raw.githubusercontent.com/datasciencecampus/google-mobility-reports-data/master/geography/google_mobility_lad_lookup_200903.csv") %>%
    # create a single field with the lowest of the two geography levels available
    dplyr::mutate(area = dplyr::coalesce(sub_region_2, sub_region_1))

  ggplot2::ggplot(google_mobility$data %>%
                    dplyr::filter(date >= min_date,
                                  sub_region_1 %in% sub_region_1_name,
                                  !type %in% c("Parks", "Residential"),
                                  lubridate::wday(date) %in% 2:6 # Mon - Fri only
                    ) %>%
                    dplyr::mutate(area = dplyr::coalesce(sub_region_2, sub_region_1)) %>%
                    dplyr::left_join(ons_lookup, by = "area", suffix = c("", "_ons")) %>%
                    dplyr::mutate(la_name = ifelse(is.na(la_name), sub_region_1_name, la_name)),
                  ggplot2::aes(x = date, y = value, colour = type)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::facet_wrap(~ la_name) +
    ggplot2::labs(title = "Google Mobility Data",
                  subtitle = google_mobility$subtitle,
                  x = "",
                  y = "Change on baseline (%)",
                  caption = google_mobility$caption,
                  colour = "Place type"
    ) +
    ggplot_theme
}
