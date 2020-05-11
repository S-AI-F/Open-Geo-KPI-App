
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# load packages

library(shiny)
library(plotly)
library(leaflet)
library(plyr)
library(dplyr)
library(ggplot2)
library(rgdal)
library(shinyWidgets)
library(eurostat)


###########################
#### Load data
###########################

tps00001_data <- eurostat::get_eurostat("tps00001",
                                        time_format = "date",
                                        stringsAsFactors = FALSE) %>% 
    dplyr::select(geo,time,values) %>% 
    dplyr::rename(demography = values) 

tps00203_data <- eurostat::get_eurostat("tps00203",
                                        time_format = "date",
                                        stringsAsFactors = FALSE)  %>% 
    dplyr::filter(unit == "PC_ACT") %>%
    dplyr::select(geo,time,values) %>%
    dplyr::rename(unemployement_rate = values) 

tps00199_data <- eurostat::get_eurostat("tps00199",
                                        time_format = "date",
                                        stringsAsFactors = FALSE) %>% 
    dplyr::select(geo,time,values) %>%
    dplyr::rename(fertility_rate = values) 

# get Greenhouse gas emissions data
sdg_13_10_data <- eurostat::get_eurostat("sdg_13_10",
                                         time_format = "date",
                                         stringsAsFactors = FALSE,
                                         filters = list(indic_env = "GHG_T_HAB")) %>% 
    dplyr::select(geo,time,values) %>%
    dplyr::rename(Greenhouse_Gaz_Emission = values) 

# get exposure to air pollution data

sdg_11_50_data <- eurostat::get_eurostat("sdg_11_50",
                                         time_format = "date",
                                         stringsAsFactors = FALSE,
                                         filters = list(airpol = "PM10")) %>% 
    dplyr::select(geo,time,values) %>%
    dplyr::rename(air_pollution_exposure = values)

# get eneregy consumption data
ten00124_data <- eurostat::get_eurostat("ten00124",
                                        time_format = "date",
                                        stringsAsFactors = FALSE,
                                        filters = list(nrg_bal = "FC_E")) %>% 
    dplyr::select(geo,time,values) %>%
    dplyr::rename(Energy_consumption = values)

# get research and development expenditure
tsc00001_data <- eurostat::get_eurostat("tsc00001",
                                        time_format = "date",
                                        stringsAsFactors = FALSE,
                                        filters = list(sectperf = "TOTAL"))%>% 
    dplyr::select(geo,time,values) %>%
    dplyr::rename('Research_and_development_expenditure' = values)

# get total length of motorways
ttr00002_data <- eurostat::get_eurostat("ttr00002",
                                        time_format = "date",
                                        stringsAsFactors = FALSE,
                                        filters = list(tra_infr = "MWAY")) %>% 
    dplyr::select(geo,time,values) %>%
    dplyr::rename('Motorways_length' = values)

# join data

attribute_data <- tps00001_data %>% 
    full_join(tps00203_data, by = c("geo","time")) %>% 
    full_join(tps00199_data, by = c("geo","time")) %>% 
    full_join(sdg_13_10_data, by = c("geo","time")) %>%
    full_join(sdg_11_50_data, by = c("geo","time")) %>%
    full_join(ten00124_data, by = c("geo","time")) %>% 
    full_join(tsc00001_data, by = c("geo","time")) %>% 
    full_join(ttr00002_data, by = c("geo","time"))


# Download geospatial data from GISCO
geodata <- get_eurostat_geospatial(output_class = "spdf",
                                   resolution = "60",
                                   nuts_level = 0,
                                   year = 2013)

# merge with attribute data with geodata
map_data = sp::merge(geodata,
                     attribute_data,
                     by = "geo",
                     duplicateGeoms = TRUE)
# rename country 
map_data@data$CNTR_NAME = label_eurostat(map_data@data$geo, dic = "geo", countrycode = "country.name")


##########################
#### prepare plot
###########################



date_vector = seq.Date(from = min(map_data@data$time),
                       to = max(map_data@data$time),
                       by = "years")

##########################
#### ui
###########################


ui = navbarPage("OpenGeoKPI",
                ### Europe panel ----
                tabPanel("Europe",
                         ### First row ----
                         fluidRow(
                             ### Specify filters ----
                             column(3,
                                    ### Specify kpi filter ----
                                    selectInput("selectKPI",
                                                "Select indicator:",
                                                list(`Population`  = list("Demography"="demography",
                                                                          "Fertility rate"="fertility_rate"),
                                                     `Economy`     = list("Unemployement rate" = "unemployement_rate"),
                                                     `Environment` = list("Greenhouse Gaz Emission" = "Greenhouse_Gaz_Emission",
                                                                          "Exposure to air pollution (PM10)" = "air_pollution_exposure"),
                                                     `Energy`      = list("Eergy consumption (Thousand tonnes of oil)" = "Energy_consumption"),
                                                     `Science and technology`   = list("R&D expenditure (% of GDP)" = "Research_and_development_expenditure"),
                                                     `Transport`      = list("Total length of motorways (km)" = "Motorways_length"))),
                                    
                                    ### Specify country filter ----
                                    selectInput("selectCountry",
                                                "Select country for time series:",
                                                unique(map_data@data$CNTR_NAME),
                                                selected = "France",
                                                multiple = TRUE)
                             ),
                             ### Specify plots ----
                             column(8, 
                                    tabsetPanel(type = "tabs",
                                                ### Map plot ----
                                                tabPanel("Map", leafletOutput("my_leaf")),
                                                ### timeseirs plot ----
                                                tabPanel("Time serie", plotlyOutput("my_plot")))
                             )
                         ),
                         
                         ### Second row ----
                         fluidRow(
                             column(8, 
                                    offset = 3,
                                    wellPanel(
                                        ### Specify time filter animation ----
                                        # sliderInput("animation", "Looping Animation:",
                                        #             min = min(date_vector),
                                        #             max = max(date_vector),
                                        #             value = min(date_vector), step = 1,
                                        #             animate = animationOptions(interval = 1000, 
                                        #                                        loop = FALSE))
                                        sliderTextInput(
                                            inputId = "animation",
                                            label = "Year range slider:",
                                            choices = date_vector,
                                            selected = date_vector[1],
                                            animate = animationOptions(interval = 1000, 
                                                                       loop = FALSE)
                                        )
                                    )
                             )
                         ),
                         
                         ### Thirs row ----
                         fluidRow(
                             column(12, offset = 0, h4("Observations"), DT::dataTableOutput("table"))
                         )
                ),
                
                ### World panel ----
                tabPanel("World",
                         verbatimTextOutput("summary"))
)

##########################
#### server
###########################

server <- function(input, output) {
    
    
    ### leaflet map definition ----
    output$my_leaf <- renderLeaflet({
        leaflet() %>%
            setView(lng = 2.846874031249995, lat = 46.99079193796217, zoom = 3)
    })
    
    ### summary definition ----
    output$summary = renderPrint({
        summary(map_data@data)
    })
    
    ### reactive plots definition ----
    observe({
        
        # get selected date from animation
        selected_date = input$animation
        
        # get selected kpi from kpi filters
        selected_kpi = input$selectKPI
        
        # split kpi values into categories for color attribution
        binpal <- colorBin("YlOrRd", 
                           map_data@data[, selected_kpi],
                           5, 
                           pretty = TRUE)
        
        # filter map data based on selected filters
        map_data_selected = map_data[map_data@data$time == selected_date, ]
        
        # generte filteres map
        leafletProxy(mapId = "my_leaf", data = map_data_selected)  %>%
            clearControls() %>%
            addTiles() %>%
            addPolygons(stroke = TRUE,
                        smoothFactor = 0.3,
                        fillOpacity = 0.8,
                        color = "gray",
                        dashArray = "3",
                        weight = 1,
                        opacity = 1,
                        fillColor = ~binpal(map_data_selected@data[,selected_kpi]),
                        highlightOptions = highlightOptions(weight = 2, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
                        label = ~paste0(CNTR_NAME,": ",prettyNum(map_data_selected@data[ ,selected_kpi], 
                                                                 format = "f",
                                                                 big.mark = ","))) %>%
            addLegend(pal = binpal,
                      values = ~ map_data_selected@data[ ,selected_kpi],
                      opacity = 0.7,
                      title = selected_kpi)
        
        # filter timeserie data based on selected date
        ts_data_selected = map_data@data[map_data@data$time <= selected_date, ]
        
        # filter timeserie data based on selected country
        selected_country = input$selectCountry
        ts_data_selected = ts_data_selected[ts_data_selected$CNTR_NAME %in% selected_country, ]
        
        # sort data by date
        ts_data_selected = ts_data_selected[order(ts_data_selected$time), ]
        
        # generate timeserie plot
        output$my_plot <- renderPlotly({
            
            fig = plot_ly(x = ~ts_data_selected$time,  
                          y = ~ts_data_selected[ ,selected_kpi],
                          color = ~ts_data_selected$CNTR_NAME,
                          mode = "lines+markers")
            
            fig = fig %>% layout(title = paste("Indicateur: ", selected_kpi, sp = ""),
                                 xaxis = list(title = "Dates",
                                              zeroline = TRUE),
                                 yaxis = list(title =  selected_kpi,
                                              zeroline = TRUE))
            fig
        })
        
        # prepare table to plot
        # df = map_data_selected@data
        df = map_data@data
        
        # plot table
        output$table <- DT::renderDataTable(
            DT::datatable(df, options = list(pageLength = 25))
        )
        
    })
}

##########################
##### Run the application
###########################


shinyApp(ui = ui, server = server)

