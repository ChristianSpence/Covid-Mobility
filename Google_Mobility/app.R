library(shiny)
library(tidyverse)
library(lubridate)

ui <- fluidPage(

  titlePanel("Google Mobility Data Exploration Tool"),

  sidebarLayout(
    sidebarPanel(width = 2,
                 uiOutput("sub_region_1"),
                 uiOutput("sub_region_2"),
                 uiOutput("type"),
                 uiOutput("omit_weekends"),
                 uiOutput("date"),
                 p("A quick and dirty app by @ChristianSpence")
    ),

    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {

  google_mobility <- readr::read_rds("google_mobility.rds")
  google_mobility$data <- google_mobility$data %>%
    mutate(sub_region_2 = ifelse(is.na(sub_region_2),
                                 paste(sub_region_1, "aggregate"),
                                 sub_region_2))

  output$sub_region_1 <- renderUI({
    selectInput("sub_region_1",
                "Sub Region 1",
                choices = unique(google_mobility$data$sub_region_1),
                multiple = TRUE,
                selected = "Greater Manchester"
    )
  })

  output$sub_region_2 <- renderUI({
    selectInput("sub_region_2",
                "Sub Region 2",
                choices = unique(google_mobility$data$sub_region_2[google_mobility$data$sub_region_1 %in% input$sub_region_1]),
                multiple = TRUE,
                selected = unique(google_mobility$data$sub_region_2[google_mobility$data$sub_region_1 %in% input$sub_region_1])
    )
  })

  output$type <- renderUI({
    selectInput("type",
                "Place type",
                choices = unique(google_mobility$data$type),
                selected = "Workplaces",
                multiple = TRUE
    )
  })

  output$omit_weekends <- renderUI({
    checkboxInput("omit_weekends",
                  "Omit weekends",
                  value = TRUE)
  })

  output$date <- renderUI({
    min = min(google_mobility$data$date)
    max = max(google_mobility$data$date)
    sliderInput("date",
                "Date range",
                min = min,
                max = max,
                value = c(as.Date("2021-09-01"), max),
                timeFormat = "%e %b %Y"
    )
  })

  days_of_week <- function(df) {

    if (input$omit_weekends) {
      df <- filter(df, lubridate::wday(date) %in% 2:6)
    }

    return(df)
  }

  output$plot <- renderPlot({
    req(input$sub_region_1, input$sub_region_2, input$type, input$date)
    ggplot2::ggplot(google_mobility$data %>%
                      dplyr::filter(date >= input$date[1],
                                    date <= input$date[2],
                                    sub_region_1 %in% input$sub_region_1,
                                    sub_region_2 %in% input$sub_region_2,
                                    type %in% input$type) %>%
                      days_of_week(),

                    ggplot2::aes(x = date, y = value, colour = type)) +
      ggplot2::geom_line(size = 1) +
      ggplot2::facet_wrap(~ sub_region_2) +
      ggplot2::labs(title = "Google Mobility Data",
                    subtitle = paste(google_mobility$subtitle, "(Mon-Fri only)"),
                    x = "",
                    y = "Change on baseline (%)",
                    caption = google_mobility$caption,
                    colour = "Place type"
      ) +
      ggplot2::theme(
        panel.background   = ggplot2::element_blank(),
        panel.grid         = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(linetype = "dotted"),
        legend.position    = "top",
        axis.line.y.right  = NULL,
        axis.line          = ggplot2::element_line(),
        strip.background   = ggplot2::element_blank()
      )

  }, height = 600)
}

shinyApp(ui = ui, server = server)
