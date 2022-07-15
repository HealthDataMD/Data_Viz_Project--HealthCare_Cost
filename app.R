# # leaflet from https://rstudio.github.io/leaflet/json.html
# ### Gets file From http://leafletjs.com/examples/choropleth/us-states.js

# install.packages("readr")
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("shinydashboardPlus") 
# install.packages("bs4Dash")
# install.packages("leaflet")
# install.packages("DT")
# install.packages("plotly")
# install.packages("ggplot2")
# install.packages("ggthemes")
# install.packages("leaflet")
# install.packages("geojsonio")
# install.packages("tidyjson")
# install.packages("wordcloud")
# install.packages("RColorBrewer")
# install.packages("wordcloud2)
# install.packages("tm")


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
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(tm)


######### ISAAC
insurance <- read_csv("insurance.csv")
gender <- insurance %>% distinct(sex) %>% pull(sex)
children <- insurance %>% distinct(children) %>%
    arrange(children, desc(children)) %>%
    pull(children)
insurance = insurance %>%
    mutate(bmi_label = case_when(bmi>=40~"morbidly obese",
                                 bmi>=30~ "obese",
                                 bmi>25~"overweight",
                                 bmi>=18.5~"healthy",
                                 bmi<18.5~"underweight"
    ))
bmi_label_list = c("morbidly obese","obese","overweight","healthy","underweight")
age <- insurance %>%
    mutate(age_range = case_when(age<=40~"18-40",
                                 age>=41~"41-64"))
age_list = c("18-40", "41-64")
############ END of ISAAC

Color = c("YlOrRd", "Greens", "Blues")
mapd <- read_csv("md_all.csv")  
### Gets file From http://leafletjs.com/examples/choropleth/us-states.js
states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")
### remove puerto rico because CMS data does not have it
states <- states[-52,]  


ui <- dashboardPage(
    dashboardHeader(title = "Healthcare Costs"),
    dashboardSidebar(
        minified = TRUE,
        collapsed = F,
        sidebarMenu(
            menuItem("Home", tabName = "page1_Home", icon = icon("info")),
            menuItem("Cost by Demographics", tabName = "page2", icon = icon("area-chart")),
            menuItem("Regional Cost", tabName = "page3_leaflet", icon = icon("area-chart")),
            menuItem("Inpatient Procedures", tabName = "page4_word_cloud", icon = icon("map-o")),
            menuItem("National Health Expenditure",tabName = "page5_NHE",icon=icon("database")),
            menuItem("Data",tabName = "page6_Data",icon=icon("database"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "page1_Home", 
                    fluidRow(
                        box(
                            title = "Introduction",
                            solidHeader = TRUE,
                            status = "indigo",
                            width = 12,
                            collapsible = TRUE,
                            column(
                                12,
                                tags$div(
                                    "The cost of healthcare is a hot topic for beneficiaries, payers, providers and other stakeholders in the 
                        healthcare industry. With various sectors, stakeholders, and environmental factors impacting and influencing
                        each other, and the lack of availability of service charge data, it's important to provide a resource that 
                        may provide insight to what a beneficiary may pay for service(s).",
                        br(),
                        br(),
                        "This app will provide users with the ability to view discharge amounts billed from hospitals across four 
                        regions by demographics, as well as a map showing medical costs billed by an insurer to beneficiaries." 
                                ),
                        style = "font-size:14px"
                            )
                        )
                    ),
                    
                    fluidRow(
                        box(
                            title = "Purpose and Datasets",
                            solidHeader = TRUE,
                            status = "olive",
                            width = 12,
                            collapsible = TRUE,
                            column(
                                12,
                                tags$div(
                                    "The purpose of this app is for exploratory purposes. Individuals interested in exploring medical 
                      costs billed by insurers can find this information based on several factors including, age, gender, and BMI 
                      and for older adults interested in Medicare billed costs based on service and region."),
                      br(),
                      br(),
                      "We wanted to explore the healthcare costs billed to beneficiaries and used data from CMS and Kaggle to 
                      explore two different datasets.",
                      br(),
                      br()
                      # "1. Medical costs personal dataset - This dataset comes from the book, "Machine Learning with R" by 
                      #   Brett Lantz. The data highlights the individual medical cost billed by an insurer for a beneficiary. It 
                      #   includes beneficiary information such as age, sex, BMI, smoking status, number of children, and 
                      #   regional location."),
                            ),
                      style = "font-size:14px"
                        )
                    )),
            # tabName( = "page1_Home", 
            #             h1("Healthcare Costs"),
            #             column(width = 12,      
            #                     box(
            #                     title = ("Intro"),
            #                     solidHeader = TRUE,
            #                     status = "secondary",   # statuses are: primary, secondary, info, success, warning, danger, gray-dark, gray, white, indigo, lightblue, navy, purple, fuchsia, pink, maroon, orange, lime, teal, olive.
            #                     width = NULL,
            #                     collapsible = TRUE,
            #                     tags$h5(tags$strong("Project Description")),
            #                     tags$h5(
            #                         "PARAGRAPH TEXT HERE WILL ALLOW MULTIPLE LINES."  
            #                          
            #                     ))),
            #         column(width = 12,      
            #                box(
            #                    title = "Why HealthCare Cost?",
            #                    solidHeader = TRUE,
            #                    status = "secondary",   # statuses are: primary, secondary, info, success, warning, danger, gray-dark, gray, white, indigo, lightblue, navy, purple, fuchsia, pink, maroon, orange, lime, teal, olive.
            #                    width = NULL,
            #                    collapsible = TRUE,
            #                    tags$h5(tags$strong("Project Description")),
            #                    tags$h5(
            #                        "PARAGRAPH TEXT HERE WILL ALLOW MULTIPLE LINES."  
            #                        
            #                    ))),
            #         column(width = 12,      
            #                box(
            #                    title = "Team members",
            #                    solidHeader = TRUE,
            #                    status = "secondary",   # statuses are: primary, secondary, info, success, warning, danger, gray-dark, gray, white, indigo, lightblue, navy, purple, fuchsia, pink, maroon, orange, lime, teal, olive.
            #                    width = NULL,
            #                    collapsible = TRUE,
            #                    tags$h5(tags$strong("Project Description")),
            #                    tags$h5(
            #                        "PARAGRAPH TEXT HERE WILL ALLOW MULTIPLE LINES."  
            #                        
            #                    )))
            #         
            #         
            #             #         column(
            #             #             12,
            #             #             # Lets figure out rendertext instead of inserting text here
            #             #             tags$div(
            #             #                 h4("header")
            #             #             ),
            #             #             tags$body(
            #             #                 h1('My first heading'),
            #             #                 p('My first paragraph, with some ', strong('bold'), '
            #             #                   dsfadf
            #             #                   asdftext.'),
            #             #                 div(
            #             #                     id = 'myDiv', class = 'simpleDiv',
            #             #                     'Here is a div with some attributes.'
            #             #                 )
         
            tabItem(tabName = "page2"
                    ,fluidPage(
                        box(
                            title = "Instructions",
                            solidHeader = TRUE,
                            status = "secondary",
                            width = 12,
                            collapsible = FALSE,
                            column(
                                12,
                                tags$div(
                                    "Please select your gender, number of children, BMI category, and age group below, and check the box 
                      if you are a smoker to find out the average healthcare cost depending on your region."
                                )
                            )
                        ),
                      fluidRow(
                          column(4, 
                                 selectInput(
                                     "sex",
                                     label = h6("Select Gender"),
                                     choices = gender),
                                 
                                 selectInput(
                                     "children",
                                     label = h6("Number of Children (Max is 5)"),
                                     choices = children),
                                 
                                 selectInput(
                                     "bmi",
                                     label = h6("BMI Category"),
                                     choices = bmi_label_list),
                                 
                                 selectInput(
                                     "age",
                                     label = h6("Age"),
                                     choice = age_list
                                 ),
                                 
                                 checkboxInput("smoker", label = "Smoker", value = FALSE)
                          ),
                          
                          
                          column(8, plotOutput("plot1", height = 500)
                          ))
                    )
            ),
            # , sliderInput("year", "sliderInput", min = 2014, max = 2020, value = 1,
            #   step = 1, animate = animationOptions(interval = 2000, loop = FALSE))            
            tabItem(tabName = "page3_leaflet",
                    fluidRow(column(width = 6, 
                                    selectInput(
                                        "Color",
                                        label = h2("Select Color"),
                                        choices =Color
                                    )) ,    
                             (column(width = 6
                                     # ,selectInput(
                                     #     "Color_Scale",
                                     #     label = h2("Select Color Scale"),
                                     #     choices =Color_Scale)
                                     ))),
                    h2("2019"),
                    br(),
                    fluidRow(column(width = 12, leafletOutput("md_19", height = 400))),
                    br(),
                    fluidRow(
                        column(width = 6,
                                    h2("2018"),
                               leafletOutput("md_18", height = 300)),
                             (column(width = 6,
                                            h2("Population Density"),
                                leafletOutput("md_17", height = 300))))
            ),
            tabItem(tabName = "page4_word_cloud"
            ),
            tabItem(tabName = "page5_NHE"
            ),
            tabItem(tabName = "page6_data"
            )

)))

server <- function(input, output, session) {
    
    
    output$plot1 = renderPlot({
        
        if(input$smoker){
            insurance1 = insurance %>%
                filter(smoker=="yes")
        } else{
            insurance1 = insurance 
        }
        
        
        if(input$age == "18-40"){
            insurance1 = insurance1 %>%
                filter(age <=40 & age>=18)
        } else if(input$age == "41-64"){
            insurance1 = insurance1 %>%
                filter(age <=64 & age>40)
        }
        
        return(
            insurance1 %>%
                filter(sex ==input$sex) %>%
                filter(bmi_label==input$bmi)%>%
                filter(children == input$children) %>%
                group_by(region) %>%
                summarise(avg_charges = mean(charges_annual)) %>%
                ggplot(mapping = aes(x=region, y= avg_charges, fill= region))+
                geom_col()
            
        )
        
    })
    

    output$md_19 = renderLeaflet({ 
        # For the legand box SCALE(bin) & Color and DOMAIN column
        bins <- c(0, 50, 100, 200, 400, 600, 800, 1000, Inf)
        pal <- colorBin((paste0(input$Color)), domain = mapd$D_19, bins = bins)
        
        # # ORIGINAL For the legand box
        # bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
        # pal <- colorBin("YlOrRd", domain = states$density, bins = bins)
        
        # Displayed Box
        labels <- sprintf("<strong>%s</strong><br/>Discharged %g<br/>Submitted $%g<br/>Paid $%g<br/>Paid CMS $%g<br/>Density %g",
            mapd$state_19, mapd$D_19, mapd$C_19, mapd$P_19, mapd$MP_19, states$density) %>% lapply(htmltools::HTML)
        
        leaflet(states) %>%
            setView(-96, 37.8, 4) %>%
            addProviderTiles("MapBox", options = providerTileOptions(
                id = "mapbox.light",
                accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
            addPolygons(
                fillColor = ~pal(mapd$C_19),
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
                    direction = "auto")) %>%
            ###location of legand box #####input
            addLegend(pal = pal, values = ~mapd$C_19, opacity = 0.7, title = NULL, position = "bottomright")
    })
    
    output$md_18 = renderLeaflet({ 
        # For the legand box SCALE(bin) & Color and DOMAIN column
        bins <- c(0, 30000, 50000, 75000, 100000, 125000, 150000, 200000, Inf)
        pal <- colorBin((paste0(input$Color)), domain = mapd$C_18, bins = bins)
        # Displayed Box
        labels <- sprintf("<strong>%s</strong><br/>Discharged %g<br/>Submitted $%g<br/>Paid $%g<br/>Paid CMS $%g<br/>Density %g",
            mapd$state_18, mapd$D_18, mapd$C_18, mapd$P_18, mapd$MP_18, states$density) %>% 
            lapply(htmltools::HTML)
        
        leaflet(states) %>%
            setView(-96, 37.8, 3.1) %>%
            addProviderTiles("MapBox", options = providerTileOptions(
                id = "mapbox.light",
                accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
            addPolygons(
                fillColor = ~pal(mapd$C_18),
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
    })
    
    output$md_17 = renderLeaflet({ 
        # # ORIGINAL For the legand box
        bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
        pal <- colorBin((paste0(input$Color)), domain = states$density, bins = bins)
        
        # Displayed Box
        labels <- sprintf("<strong>%s</strong><br/>Discharged %g<br/>Submitted $%g<br/>Paid $%g<br/>Paid CMS $%g<br/>Density %g",
            mapd$state_17, mapd$D_17, mapd$C_17, mapd$P_17, mapd$MP_17, states$density) %>% 
            lapply(htmltools::HTML)
        
        leaflet(states) %>%
            setView(-96, 37.8, 3.1) %>%
            addProviderTiles("MapBox", options = providerTileOptions(
                id = "mapbox.light",
                accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
            addPolygons(
                fillColor = ~pal(states$density),
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
    })
    
    
    #  ######################### word cloud        

    
    # ###### Word Cloud
    # wcdata <- read_csv("Inpatient services &amp; mdcr pmt 17 to 19.csv")
    # ### Create a vector containing only the text
    # wc17 <- wcdata$`Pr _17`
    # wc18 <- wcdata$`Pr _18`
    # wc19 <- wcdata$`Pr _19`
    # ### Create a corpus  
    # docs <- Corpus(VectorSource(wc17))
    # 
    # wcdata$`Pr _17`=tolower(wcdata$`Pr _17`)
    # query17=wcdata%>%
    #   group_by(wcdata$`Pr _17`)%>%
    #   summarise(N=n())
    # 
    # output$plot4 <- renderPlot({
    #   cal=switch(input$method,
    #              square=(query17$N)^2,
    #              sqrt=sqrt(query17$N),
    #              none=query17$N
    #   )
    #   frequency=round(cal,0)
    #   set.seed(10)
    #   wordcloud(words = query$query, 
    #             freq = frequency,
    #             min.freq = 10,
    #             max.words=input$max,
    #             colors=brewer.pal(8, input$color),
    #             scale = c(3,0.5),
    #             random.order=F)
    # }) 
    
    
}

shinyApp(ui = ui, server = server)

