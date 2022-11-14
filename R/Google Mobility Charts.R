library(magrittr)

if (!exists("google_mobility")) {
  google_mobility <- process_google_mobility_data()
}

ons_lookup <- readr::read_csv("https://raw.githubusercontent.com/datasciencecampus/google-mobility-reports-data/master/geography/google_mobility_lad_lookup_200903.csv") %>%
  # create a single field with the lowest of the two geography levels available
  dplyr::mutate(area = dplyr::coalesce(sub_region_2, sub_region_1))

google_mobility$data <- google_mobility$data %>%
  # create a single field with the lowest of the two geography levels available
  dplyr::mutate(area = dplyr::coalesce(sub_region_2, sub_region_1)) %>%
  dplyr::left_join(ons_lookup, by = "area", suffix = c("", "_ons")) %>%
  dplyr::mutate(la_name = ifelse(is.na(la_name), sub_region_1, la_name))

ggplot_theme <- ggplot2::theme(
  panel.background   = ggplot2::element_blank(),
  panel.grid         = ggplot2::element_blank(),
  panel.grid.major.y = ggplot2::element_line(linetype = "dotted"),
  legend.position    = "top",
  axis.line.y.right  = NULL,
  axis.line          = ggplot2::element_line(),
  strip.background   = ggplot2::element_blank()
)

google_mobility_summary_uk <- function(sub_region_1_name = "Greater Manchester", min_date = NULL) {

  if (is.null(min_date)) {
    min_date <- min(google_mobility$data$date)
  } else {
    min_date <- as.Date(min_date)
  }

  ggplot2::ggplot(google_mobility$data %>%
                    dplyr::filter(date >= min_date,
                                  is.na(sub_region_1),
                                  !type %in% c("Parks", "Residential"),
                                  lubridate::wday(date) %in% 2:6 # Mon - Fri only
                    ),
                  ggplot2::aes(x = date, y = value, colour = type)) +
    ggplot2::geom_line(size = 1) +
    # ggplot2::facet_wrap(~ la_name) +
    ggplot2::labs(title = "Google Mobility Data",
                  subtitle = paste(google_mobility$subtitle, "(Mon-Fri only)"),
                  x = "",
                  y = "Change on baseline (%)",
                  caption = google_mobility$caption,
                  colour = "Place type"
    ) +
    ggplot_theme
}


google_mobility_summary <- function(sub_region_1_name = "Greater Manchester", min_date = NULL) {

  if (is.null(min_date)) {
    min_date <- min(google_mobility$data$date)
  } else {
    min_date <- as.Date(min_date)
  }

  ggplot2::ggplot(google_mobility$data %>%
                    dplyr::filter(date >= min_date,
                                  sub_region_1 %in% sub_region_1_name,
                                  !type %in% c("Parks", "Residential"),
                                  lubridate::wday(date) %in% 2:6 # Mon - Fri only
                    ),
                  ggplot2::aes(x = date, y = value, colour = type)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::facet_wrap(~ la_name) +
    ggplot2::labs(title = "Google Mobility Data",
                  subtitle = paste(google_mobility$subtitle, "(Mon-Fri only)"),
                  x = "",
                  y = "Change on baseline (%)",
                  caption = google_mobility$caption,
                  colour = "Place type"
    ) +
    ggplot_theme
}

google_workplaces_by_day_of_week <- function(sub_region_1_name = "Greater Manchester", min_date = "2021-09-01") {

  ggplot2::ggplot(google_mobility$data %>%
           dplyr::filter(date >= min_date,
                  sub_region_1 %in% sub_region_1_name,
                  type %in% c("Workplaces"),
                  lubridate::wday(date) %in% 2:6 # Mon-Fri only
           ),
         ggplot2::aes(
           x = date,
           y = value,
           fill = lubridate::wday(date, label = TRUE))
  ) +
    ggplot2::geom_col() +
    ggplot2::facet_wrap(~ la_name) +
    ggplot2::labs(
      title = "Google Mobility Data (workplaces only)",
      subtitle = paste(google_mobility$subtitle, "(Mon-Fri only)"),
         x = "",
         y = "Change on baseline (%)",
         caption = google_mobility$caption,
         fill = "Day of week"
    ) +
    ggplot_theme
}

google_transit_by_day_of_week <- function(sub_region_1_name = "Greater Manchester", min_date = "2021-09-01") {

  ggplot2::ggplot(google_mobility$data %>%
                    dplyr::filter(date >= min_date,
                                  sub_region_1 %in% sub_region_1_name,
                                  type %in% c("Transit stations"),
                                  lubridate::wday(date) %in% 2:6 # Mon-Fri only
                    ),
                  ggplot2::aes(
                    x = date,
                    y = value,
                    fill = lubridate::wday(date, label = TRUE))
  ) +
    ggplot2::geom_col() +
    ggplot2::facet_wrap(~ la_name) +
    ggplot2::labs(
      title = "Google Mobility Data (transit stations only)",
      subtitle = paste(google_mobility$subtitle, "(Mon-Fri only)"),
      x = "",
      y = "Change on baseline (%)",
      caption = google_mobility$caption,
      fill = "Day of week"
    ) +
    ggplot_theme
}

google_leisure_by_day_of_week <- function(sub_region_1_name = "Greater Manchester", min_date = "2021-09-01") {

  ggplot2::ggplot(google_mobility$data %>%
                    dplyr::filter(date >= min_date,
                                  sub_region_1 %in% sub_region_1_name,
                                  type %in% c("Retail and recreation"),
                                  # lubridate::wday(date) %in% 2:6 # Mon-Fri only
                    ),
                  ggplot2::aes(
                    x = date,
                    y = value,
                    fill = lubridate::wday(date, label = TRUE))
  ) +
    ggplot2::geom_col() +
    ggplot2::facet_wrap(~ la_name) +
    ggplot2::labs(
      title = "Google Mobility Data (retail and recreation only)",
      subtitle = google_mobility$subtitle,
      x = "",
      y = "Change on baseline (%)",
      caption = google_mobility$caption,
      fill = "Day of week"
    ) +
    ggplot_theme
}

google_workplaces_by_core_cities_vs_others <- function(min_date = "2021-09-01") {

  min_date <- as.Date(min_date)

  urban_las <- c("Belfast", "Birmingham", "Bristol, City of", "Cardiff", "Glasgow City", "Leeds", "Liverpool", "Manchester", "Newcastle upon Tyne", "Nottingham", "Sheffield") # Core Cities UK

  google_mobility$data %>%
    dplyr::filter(date >= min_date,
                  sub_region_1 != "Greater London",
                  type == "Workplaces",
                  lubridate::wday(date) %in% 2:6 # Mon-Fri only
    ) %>%
    dplyr::mutate(la_type = ifelse(la_name %in% urban_las, "Core City", "Other")) %>%
    dplyr::group_by(date, la_type) %>%
    dplyr::summarise(average = mean(value)) %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = average, colour = la_type)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::labs(title = "Workplace mobility",
                  subtitle = google_mobility$subtitle,
                  caption = google_mobility$caption,
                  colour = "Local Authority type") +
    ggplot_theme
}

google_leisure_by_core_cities_vs_others <- function(min_date = "2021-09-01") {

  min_date <- as.Date(min_date)

  urban_las <- c("Belfast", "Birmingham", "Bristol, City of", "Cardiff", "Glasgow City", "Leeds", "Liverpool", "Manchester", "Newcastle upon Tyne", "Nottingham", "Sheffield") # Core Cities UK

  google_mobility$data %>%
    dplyr::filter(date >= min_date,
                  sub_region_1 != "Greater London",
                  type == "Retail and recreation"#,
                  # lubridate::wday(date) %in% 2:6 # Mon-Fri only
    ) %>%
    dplyr::mutate(la_type = ifelse(la_name %in% urban_las, "Core City", "Other")) %>%
    dplyr::group_by(date, la_type) %>%
    dplyr::summarise(average = mean(value, na.rm = TRUE)) %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = average, colour = la_type)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::labs(title = "Retail and recreation mobility",
                  subtitle = google_mobility$subtitle,
                  caption = google_mobility$caption,
                  colour = "Local Authority type") +
    ggplot_theme
}

google_transport_by_core_cities_vs_others <- function(min_date = "2021-09-01") {

  min_date <- as.Date(min_date)

  urban_las <- c("Belfast", "Birmingham", "Bristol, City of", "Cardiff", "Glasgow City", "Leeds", "Liverpool", "Manchester", "Newcastle upon Tyne", "Nottingham", "Sheffield") # Core Cities UK

  google_mobility$data %>%
    dplyr::filter(date >= min_date,
                  sub_region_1 != "Greater London",
                  type == "Transit stations"#,
                  # lubridate::wday(date) %in% 2:6 # Mon-Fri only
    ) %>%
    dplyr::mutate(la_type = ifelse(la_name %in% urban_las, "Core City", "Other")) %>%
    dplyr::group_by(date, la_type) %>%
    dplyr::summarise(average = mean(value, na.rm = TRUE)) %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = average, colour = la_type)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::labs(title = "Transit stations mobility",
                  subtitle = google_mobility$subtitle,
                  caption = google_mobility$caption,
                  colour = "Local Authority type") +
    ggplot_theme
}

google_leisure_and_workplace_by_core_cities_vs_others <- function(min_date = "2021-09-01") {

  min_date <- as.Date(min_date)

  urban_las <- c("Belfast", "Birmingham", "Bristol, City of", "Cardiff", "Glasgow City", "Leeds", "Liverpool", "Manchester", "Newcastle upon Tyne", "Nottingham", "Sheffield") # Core Cities UK

  google_mobility$data %>%
    dplyr::filter(date >= min_date,
                  sub_region_1 != "Greater London",
                  type %in% c("Retail and recreation", "Workplaces"),
                  lubridate::wday(date) %in% 2:6 # Mon-Fri only
    ) %>%
    dplyr::mutate(la_type = ifelse(la_name %in% urban_las, "Core City", "Other")) %>%
    dplyr::group_by(date, la_type, type) %>%
    dplyr::summarise(average = mean(value, na.rm = TRUE)) %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = average, colour = la_type)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::facet_wrap(~ type) +
    ggplot2::labs(title = "Retail and recreation vs workplaces mobility",
                  subtitle = google_mobility$subtitle,
                  caption = google_mobility$caption,
                  colour = "Local Authority type") +
    ggplot_theme
}
