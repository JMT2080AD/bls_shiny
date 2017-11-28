rm(list = ls())

library(rgdal)
library(sf)
library(data.table)
library(shiny)
library(ggplot2)

## read in county polygons and melt
counties <- st_read("./data_input", "cb_2016_us_county_20m", quiet = T, stringsAsFactors = F)
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

## build color ramp palette
crp <- colorRampPalette(c("red",
                          "yellow",
                          "green",
                          "blue"))

selectGeoid <- function(x, y){
    if(is.null(x)){
        return(53000)
    }
    geoid_select <- counties$geoid[st_intersects(counties, st_point(c(x, y)), sparse = F)]
    if(length(geoid_select) == 0){
        return(53000)
    }
    return(geoid_select)

}

jobPlot <- function(geoid_select){
    countyName <- dat[area_fips == geoid_select & periodName == "Annual", area_title, by = area_title]
    ggplot(dat[area_fips == geoid_select & periodName == "Annual",]) +
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

washMap <- function(geoid_select){
    if(geoid_select == 53000){
        countyName <- "No County Selected"
    }else{
        countyName <- unique(dat[area_fips == geoid_select & periodName == "Annual"]$area_title)
    }
    eb <- element_blank()
    ggplot() +
        geom_polygon(data = counties.points[geoid != geoid_select,],
                     aes(x, y, group = name),
                     fill = "white",
                     color = "black") +
        geom_polygon(data = counties.points[geoid == geoid_select,],
                     aes(x, y, group = name),
                     fill = "grey",
                     color = "black") + 
        coord_equal() +
        ggtitle(countyName) + 
        guides(fill = F) +
        theme(plot.title       = element_text(hjust = 0.5),
              panel.border     = eb,
              panel.grid       = eb,
              panel.background = eb,
              axis.title       = eb,
              axis.text        = eb,
              axis.ticks       = eb)
}

ui <- fluidPage(
    fluidRow(
        column(6, plotOutput("map_plot",  click = "click_plot")),
        column(6, plotOutput("line_plot"))
       ),
    fluidRow(verbatimTextOutput("info"))
)

server <- function(input, output, session){
    session$onSessionEnded(stopApp)

    start <- reactiveValues(
        state = 0,
        last.click.x = 0,
        last.click.y = 0
    )

    geoid_select      <- reactive({selectGeoid(input$click_plot$x, input$click_plot$y)})
    geoid_select_last <- reactive({selectGeoid(start$last.click.x, start$last.click.y)})

    observeEvent(input$click_plot, {
        if(length(input$click_plot$x) == 0){
            start$state <- 0
        }else{
            start$state <- 1
        }
    })
    
    output$map_plot <- renderPlot({
        if(start$state == 1){
            if(length(input$click_plot$x) != 0){
                start$last.click.x <- input$click_plot$x
                start$last.click.y <- input$click_plot$y
                return(washMap(geoid_select()))
            }else{
                return(washMap(geoid_select_last()))
            }
        }
        if(start$state == 0){
            return(washMap(53000))
        }
    })

    output$line_plot <- renderPlot({
        if(start$state == 1){
            if(length(input$click_plot$x) != 0){
                start$last.click.x <- input$click_plot$x
                start$last.click.y <- input$click_plot$y
                return(jobPlot(geoid_select()))
            }else{
                return(jobPlot(geoid_select_last()))
            }
        }
        if(start$state == 0){
            return(jobPlot(53000))
        }
    })
}

shinyApp(ui, server)
