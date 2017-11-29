library(shiny)

ui <- fluidPage(
    fluidRow(
        column(5, tags$div(class = "header", checked = NA,
                           tags$h4("Click the map below to query that county's\naverage weekly wages by job class.")))
    ),
    fluidRow(
        column(5, plotOutput("map_plot",  click = "click_plot")),
        column(7, plotOutput("line_plot"))
    )
)
