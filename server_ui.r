rm(list = ls())

library(rgdal)
library(sf)
library(data.table)
library(shiny)
library(ggplot2)

## read in county polygons and melt
counties <- st_read("./data_input", "cb_2016_us_county_20m", quiet = T)
counties <- st_transform(counties, 2285)

counties <- counties[counties$STATEFP == 53,]
setnames(counties, tolower(names(counties)))

counties.points <- rbindlist(mapply(function(x, geoid, name){
                                        x <- data.table(x[[1]][[1]])
                                        setnames(x, c("x", "y"))
                                        x[,geoid:=geoid]
                                        x[,name:=name]
                                        return(x)},
                                    counties$geometry,
                                    counties$geoid,
                                    counties$name,
                                    SIMPLIFY = F))

## read in base data
dat <- readRDS("./data_output/washington_qcew.rds")

## ## define plotting functions
## x <- 1465873.67877331
## y <- 416762.061959529
## x <- NULL
## y <- NULL

crp <- colorRampPalette(c("red",
                          "yellow",
                          "green",
                          "blue"))

selectGeoid <- function(x, y){
    if(is.null(x)){
        return("No county selected.")
    }
    counties$geoid[st_intersects(counties, st_point(c(x, y)), sparse = F)]
}

jobPlot <- function(geoid){
    countyName <- dat[area_fips == geoid & periodName == "Annual", area_title, by = area_title]
    ggplot(dat[area_fips == geoid & periodName == "Annual",]) +
    geom_line(aes(year, value, group = industry_title, colour = industry_title, linetype = industry_title)) +
    theme_bw() +
    scale_color_manual(values=crp(length(unique(dat$industry_title)))) + 
    scale_linetype_manual(values=rep(c("solid", "dotdash", "dashed"), 4)) +
    labs(title = "Average Weekly Income",
         subtitle = paste("All Industry Groups Average Weekly Income -",  countyName),
         caption = "Source: Bureau of Labor Statistics",
         x = "Year",
         y = "Average Weekly Income in Dollars")

}

washMap <- function(){
    eb <- element_blank()
    ggplot(counties.points) +
        geom_polygon(aes(x, y, group = name), fill = "white", color = "black") +
        coord_equal() +
        guides(fill = F) +
        title("County") + 
        theme(panel.border = eb,
              panel.grid = eb,
              panel.background = eb,
              axis.title = eb,
              axis.text  = eb,
              axis.ticks = eb)
}

## run server
ui <- fluidPage(
    fluidRow(
        column(6, plotOutput("map_plot",  click = "maplocation")),
        column(6, plotOutput("line_plot"))
       ),
    fluidRow(verbatimTextOutput("info"))
)

server <- function(input, output){
    output$map_plot <- renderPlot(
        washMap()
    )
    output$line_plot <- renderPlot(
        jobPlot(selectGeoid(input$maplocation$x, input$maplocation$y))
    )
    output$info <- renderText(
        paste(selectGeoid(input$maplocation$x, input$maplocation$y))
    )
}

shinyApp(ui, server)
