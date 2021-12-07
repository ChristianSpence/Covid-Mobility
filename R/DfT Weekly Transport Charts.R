library(magrittr)

if (!exists("dft")) {
  dft <- process_dft_mobility_data()
}

dft_transport_summary <- function(min_date = NULL) {

  if (is.null(min_date)) {
    min_date <- min(dft$data$Date)
  } else {
    min_date <- as.Date(dft$data$Date)
  }

  ggplot2::ggplot(dft$data %>%
                    dplyr::select(1:10) %>%
                    tidyr::pivot_longer(-1, names_to = "type") %>%
                    dplyr::filter(type != "Cycling",
                                  Date >= "2021-09-01",
                                  lubridate::wday(Date) %in% 2:6 # Mon-Fri only
                    ),
                  ggplot2::aes(x = Date,
                               y = value * 100)
  ) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~ type) +
    ggplot2::labs(
      title = "Domestic transport use by mode",
      subtitle = paste(dft$subtitle, "(Mon-Fri only)"),
      x = "",
      y = "Change on baseline",
      caption = dft$caption) +
    ggplot2::theme(
      panel.background   = ggplot2::element_blank(),
      panel.grid         = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(linetype = "dotted"),
      legend.position    = "top",
      axis.line.y.right  = NULL,
      axis.line          = ggplot2::element_line(),
      strip.background   = ggplot2::element_blank()
    )
}
