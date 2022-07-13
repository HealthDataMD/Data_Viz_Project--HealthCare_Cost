#222222

library(readr)
library(tidyverse)
library(dplyr)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus) 
library(bs4Dash)
library(leaflet)
library(DT)
library(plotly)
library(ggplot2)
library(ggthemes)
library(leaflet)
library(geojsonio)
library(tidyjson)

md_19 <- read_csv("md_19.csv")
md_18 <- read_csv("md_18.csv")
md_17 <- read_csv("md_17.csv")


color_scale = c("C", "P", "MP")
color_choice = c("YlOrRd", "Greens", "Blues")

ui <- dashboardPage(
    dashboardHeader(title = "Healthcare Costs"),
    dashboardSidebar(
        minified = TRUE,
        collapsed = TRUE,
        sidebarMenu(
            menuItem("Home", tabName = "page1", icon = icon("info")),
            menuItem("Cost by Demographic", tabName = "page2", icon = icon("area-chart")),
            menuItem("Regional Cost", tabName = "page3", icon = icon("map-o")),
            menuItem("Data",tabName = "page4",icon=icon("database"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "page1", 
                    navbarPage(
                        "title",
                        fluidRow(
                            box(
                                title = "Intro",
                                solidHeader = TRUE,
                                status = "secondary",   # statuses are: primary, secondary, info, success, warning, danger, gray-dark, gray, white, indigo, lightblue, navy, purple, fuchsia, pink, maroon, orange, lime, teal, olive.
                                width = 12,
                                collapsible = TRUE,
                                column(
                                    12,
                                    # Lets figure out rendertext instead of inserting text here
                                    tags$div(
                                        h4("header")
                                    ),
                                    tags$body(
                                        h1('My first heading'),
                                        p('My first paragraph, with some ', strong('bold'), '
                                          dsfadf
                                          asdftext.'),
                                        div(
                                            id = 'myDiv', class = 'simpleDiv',
                                            'Here is a div with some attributes.'
                                        )
                                    ),
                                    style = "font-size:14px"
                                )
                            ),
                            position = c("static-top", "fixed-top", "fixed-bottom"),
                            tabPanel(icon("info"), "Introduction",
                                     "asdfadfasdf"),
                            tabPanel(icon("info"), "ShinyApp"),
                            tabPanel(icon("info"), "About The Team",
                                     "asdfasdf")
                        )
                    )
            ),
            tabItem(tabName = "page2"
                    , checkboxInput("checkboxInput", label = "Show holidays", value = FALSE)
                    # , sliderInput("year", "sliderInput", min = 2014, max = 2020, value = 1,
                    #   step = 1, animate = animationOptions(interval = 2000, loop = FALSE))
            ),
            tabItem(tabName = "page3",
                        fluidRow(column(width = 6, 
                                selectInput(
                                        "color",
                                        label = h2("Select Color"),
                                        choices =color_choice
                                    )) ,    
                             (column(width = 6,
                                selectInput(
                                    "Color_Scale",
                                    label = h2("Select Color Scale"),
                                    choices =color_scale
                                )))),
                    h2("2019"),
                    br(),
                    fluidRow(column(width = 12, leafletOutput("md_19", height = 400))),
                    br(),
                    fluidRow(column(width = 6, leafletOutput("md_18", height = 300)),
                            (column(width = 6, leafletOutput("md_17", height = 300))))
                    
                    #          column(width = 6, leafletOutput("state", height = 200)))
                    # column(width = 6, 
                    #       leafletOutput("md_19", width="100%")),
                    # 
                    # column(width = 6, 
                    #               leafletOutput("md_19", width="100%"))
                    
            )
        )
    )
)

server <- function(input, output, session) {
    

        # md_19 <- read_csv("md_19.csv")
        # md_18 <- read_csv("md_18.csv")
        # md_17 <- read_csv("md_17.csv") 
    # Gets file From http://leafletjs.com/examples/choropleth/us-states.js
    states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")
    # remove puerto rico because CMS data does not have it
    states <- states[-52,]       


    output$md_19 = renderLeaflet({ 
       
         # For the legand box SCALE(bin) & Color and DOMAIN column
        bins <- c(0, 30000, 50000, 75000, 100000, 125000, 150000, 200000, Inf)
        pal <- colorBin((input$color_choice), domain = md_19$C_19, bins = bins)
        
        
        # # ORIGINAL For the legand box
        # bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
        # pal <- colorBin("YlOrRd", domain = states$density, bins = bins)
        
        # Displayed Box
        labels <- sprintf(
            "<strong>%s</strong>
            <br/>Discharged %g
            <br/>Submitted $%g
            <br/>Paid $%g
            <br/>Paid CMS $%g
            <br/>Density %g",
            md_19$state_19, md_19$D_19, md_19$C_19, md_19$P_19, md_19$MP_19, states$density
        ) %>% lapply(htmltools::HTML)
        
        leaflet(states) %>%
            setView(-96, 37.8, 4) %>%
            addProviderTiles("MapBox", options = providerTileOptions(
                id = "mapbox.light",
                accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
            addPolygons(
                fillColor = ~pal(md_19$C_19),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) 
        %>%
        ###location of legand box #####input
        addLegend(pal = pal, values = ~md_19$C_19, opacity = 0.7, title = NULL, position = "bottomright")
})
        
        output$md_18 = renderLeaflet({ 
            
            # For the legand box SCALE(bin) & Color and DOMAIN column
            bins <- c(0, 30000, 50000, 75000, 100000, 125000, 150000, 200000, Inf)
            pal <- colorBin((input$color_choice), domain = md_18$C_18, bins = bins)
            
            
            # # ORIGINAL For the legand box
            # bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
            # pal <- colorBin("YlOrRd", domain = states$density, bins = bins)
            
            # Displayed Box
            labels <- sprintf(
                "<strong>%s</strong>
            <br/>Discharged %g
            <br/>Submitted $%g
            <br/>Paid $%g
            <br/>Paid CMS $%g
            <br/>Density %g",
            md_18$state_18, md_18$D_18, md_18$C_18, md_18$P_18, md_18$MP_18, states$density
            ) %>% lapply(htmltools::HTML)
            
            leaflet(states) %>%
                setView(-96, 37.8, 3.1) %>%
                addProviderTiles("MapBox", options = providerTileOptions(
                    id = "mapbox.light",
                    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
                addPolygons(
                    fillColor = ~pal(md_18$C_18),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    highlightOptions = highlightOptions(
                        weight = 5,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = labels,
                    labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto"))
            
            
            
                            `#locat      o                                                                                                                                                                                                                                                                                                                                                                                                                   n of legand box #####input
                # addLegend(pal = pal, values = ~md_18$C_18, opacity = 0.7, title = NULL,
                #           position = "bottomleft")
            
            
            ######################## attempt to syn
            # latticeView(md_19, md_18, ncol = 2, sync = "all", sync.cursor = TRUE,
            #             no.initial.sync = FALSE)
            # latticeview(md_19, md_18)
            # sync(md_19, md_18, ncol = 2, sync = "all", sync.cursor = TRUE,
            #      no.initial.sync = FALSE)
    
    })
        output$md_17 = renderLeaflet({ 
            
            # For the legand box SCALE(bin) & Color and DOMAIN column
            bins <- c(0, 30000, 50000, 75000, 100000, 125000, 150000, 200000, Inf)
            pal <- colorBin((input$color_choice), domain = md_17$C_17, bins = bins)
            
            
            # # ORIGINAL For the legand box
            # bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
            # pal <- colorBin("YlOrRd", domain = states$density, bins = bins)
            
            # Displayed Box
            labels <- sprintf(
                "<strong>%s</strong>
            <br/>Discharged %g
            <br/>Submitted $%g
            <br/>Paid $%g
            <br/>Paid CMS $%g
            <br/>Density %g",
            md_17$state_17, md_17$D_17, md_17$C_17, md_17$P_17, md_17$MP_17, states$density
            ) %>% lapply(htmltools::HTML)
            
            leaflet(states) %>%
                setView(-96, 37.8, 3.1) %>%
                addProviderTiles("MapBox", options = providerTileOptions(
                    id = "mapbox.light",
                    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
                addPolygons(
                    fillColor = ~pal(md_17$C_17),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    highlightOptions = highlightOptions(
                        weight = 5,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = labels,
                    labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto")) 
            # #location of legand box #####input
            # addLegend(pal = pal, values = ~md_17$C_17, opacity = 0.7, title = NULL,
            #           position = "bottomright")
            
        })
}

shinyApp(ui = ui, server = server)


##pkgs <- as.data.frame(installed.packages(), stringsAsFactors = FALSE, row.names = FALSE)
##pkgs[, c("Package", "Version", "Built")]

