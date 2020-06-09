
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
library(rnaturalearth)

########### Load Europe data ----

# get demography data

tps00001_data <- eurostat::get_eurostat("tps00001",
                                        time_format = "date",
                                        stringsAsFactors = FALSE) %>% 
  dplyr::select(geo,time,values) %>% 
  dplyr::rename(demography = values) 

# get unemployement rate data

tps00203_data <- eurostat::get_eurostat("tps00203",
                                        time_format = "date",
                                        stringsAsFactors = FALSE)  %>% 
  dplyr::filter(unit == "PC_ACT") %>%
  dplyr::select(geo,time,values) %>%
  dplyr::rename(unemployement_rate = values) 

# get fertility rate data

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




########### Load World data ----

# get map data
geodata_world <- ne_countries()
# rename country code and name variables
names(geodata_world)[names(geodata_world) == "iso_a3"] <- "iso3c"
names(geodata_world)[names(geodata_world) == "name"] <- "NAME"

# get worldbank data

attribute_data_world <- wb(
  indicator = c(# people indicators
    "SP.POP.TOTL","SE.PRM.ENRR","SL.TLF.CACT.ZS","SH.STA.MMRT",
    # porverty and inequality indicators
    "SI.POV.NAHC", "SI.POV.GINI",
    # Environment
    "AG.LND.AGRI.ZS","EN.ATM.CO2E.PC","EN.ATM.PM25.MC.M3",
    # Economy
    "NY.GDP.MKTP.CD","NE.GDI.TOTL.KD.ZG","SL.GDP.PCAP.EM.KD",
    # States and markets
    "GC.REV.XGRT.CN", "GC.XPN.TOTL.CN","MS.MIL.XPND.GD.ZS","GB.XPD.RSDV.GD.ZS",
    # Global links
    "DT.DOD.DECT.CD"),
  return_wide = TRUE,
  POSIXct = TRUE
)

# rename indicators
attribute_data_world = attribute_data_world %>% 
  dplyr::rename(# people indicators
    Population_Total = SP.POP.TOTL,
    School_enrollment_primary = SE.PRM.ENRR,
    Labor_force_participation_rate = SL.TLF.CACT.ZS,
    Maternal_mortality_ratio = SH.STA.MMRT,
    # porverty and inequality indicators
    Poverty_headcount_ratio = SI.POV.NAHC,
    GINI_index = SI.POV.GINI,
    # Environment
    Agricultural_land = AG.LND.AGRI.ZS,
    CO2_emission = EN.ATM.CO2E.PC,
    PM2.5_air_pollution = EN.ATM.PM25.MC.M3,
    # Economy
    GDP = NY.GDP.MKTP.CD,
    Gross_capital_formation = NE.GDI.TOTL.KD.ZG,
    GDP_per_person_employed = SL.GDP.PCAP.EM.KD,
    # States and markets
    Government_revenue = GC.REV.XGRT.CN,
    Government_Expense  = GC.XPN.TOTL.CN,
    Military_expenditure = MS.MIL.XPND.GD.ZS,
    Research_and_development_expenditure = GB.XPD.RSDV.GD.ZS,
    # Global Links
    External_debt_stocks = DT.DOD.DECT.CD)


# merge with attribute data with geodata
map_data_world = sp::merge(geodata_world,
                           attribute_data_world,
                           by = "iso3c",
                           duplicateGeoms = TRUE)

map_data_world = map_data_world[!is.na(map_data_world@data$date_ct), ]

##########################
#### prepare plot
###########################


date_vector = seq.Date(from = min(map_data@data$time),
                       to = max(map_data@data$time),
                       by = "years")
AnimationEnd_Europe = length(date_vector) - 1

date_vector_world = seq.Date(from = min(map_data_world@data$date_ct),
                             to = max(map_data_world@data$date_ct),
                             by = "years")

AnimationEnd_world = length(date_vector_world) - 1

##########################
#### ui
###########################


ui = navbarPage("OpenGeoKPI",
                
                ### World panel ----
                tabPanel("World",
                         ### First row ----
                         fluidRow(
                           ### Specify filters ----
                           column(3,
                                  ### Specify kpi filter ----
                                  selectInput("selectKPI_world",
                                              "Select indicator:",
                                              list(`People` = list("Total population" = "Population_Total",
                                                                   "School enrollment primary" = "School_enrollment_primary",
                                                                   "Labor force participation rate" = "Labor_force_participation_rate",
                                                                   "Maternal mortality ratio" = "Maternal_mortality_ratio"),
                                                   `porverty and inequality` = list("Poverty headcount ratio" = "Poverty_headcount_ratio",
                                                                                    "GINI index" = "GINI_index"),
                                                   `Environment` = list("Agricultural land" = "Agricultural_land",
                                                                        "CO2 emission" = "CO2_emission",
                                                                        "PM2.5 air pollution" = "PM2.5_air_pollution"),
                                                   `Economy` = list("GDP" = "GDP",
                                                                    "Gross capital formation" = "Gross_capital_formation",
                                                                    "GDP per person employed" = "GDP_per_person_employed"),
                                                   `States and markets` = list("Government revenue" = "Government_revenue",
                                                                               "Government Expense" = "Government_Expense",
                                                                               "Military expenditure" = "Military_expenditure",
                                                                               "R&D expenditure" = "Research_and_development_expenditure"),
                                                   `Global Links` = list("External debt stocks" = "External_debt_stocks")),
                                              selected = "Population_Total"
                                  ),
                                  
                                  ### Specify country filter ----
                                  selectInput("selectCountry_world",
                                              "Select country for time series:",
                                              unique(map_data_world@data$country),
                                              selected = "France",
                                              multiple = TRUE),
                                  
                                  ### Specify discretizariton method ----
                                  selectInput("selectDiscretization", 
                                              "Select disretization method:", 
                                              choices = list("Linear" = "KPIcolor_binpal",
                                                             "Quantile" = "KPIcolor_qpal"),
                                              selected = "KPIcolor_binpal")
                           ),
                           ### Specify plots ----
                           column(8, 
                                  tabsetPanel(type = "tabs",
                                              ### Map plot ----
                                              tabPanel("Map World", leafletOutput("my_leaf_world")),
                                              ### timeseirs plot ----
                                              tabPanel("Time serie World", plotlyOutput("my_plot_world")))
                           )
                         ),
                         
                         ### Second row ----
                         fluidRow(
                           column(8, 
                                  offset = 3,
                                  wellPanel(
                                    sliderTextInput(
                                      inputId = "animation_world",
                                      label = "Year range slider:",
                                      choices = date_vector_world,
                                      selected = date_vector_world[AnimationEnd_world],
                                      animate = animationOptions(interval = 1000, 
                                                                 loop = FALSE)
                                    ),
                                    verbatimTextOutput("date_range")
                                  )
                           )
                         )
                ),
                
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
                                    sliderTextInput(
                                      inputId = "animation",
                                      label = "Year range slider:",
                                      choices = date_vector,
                                      selected = date_vector[AnimationEnd_Europe],
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
                )
                

)

##########################
#### server
###########################

server <- function(input, output, session) {
  
  
  ### leaflet map definition ----
  ### Europe 
  output$my_leaf <- renderLeaflet({
    leaflet() %>%
      setView(lng = 2.846874031249995, lat = 46.99079193796217, zoom = 3)
  })
  ### world 
  output$my_leaf_world <- renderLeaflet({
    leaflet() %>%
      setView(lng = 2.846874031249995, lat = 46.99079193796217, zoom = 1)
  })

  # output$date_range = renderText({
  #   paste("Start date:", min(date_vector_world), "End date:", max(date_vector_world))
  # })
  
  
  
  ### reactive plots definition ----
  observe({
    
    ### Europe ----
    # get selected date from animation
    selected_date = input$animation
    
    # get selected kpi from kpi filters
    selected_kpi = input$selectKPI
    
    # split kpi values into categories for color attribution
    binpal <- colorBin("YlOrRd", 
                       map_data@data[, selected_kpi],
                       5, 
                       pretty = FALSE)
    
    
    
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
    
    
    
    
    
    ### WOrld ----
    # get selected date from animation
    selected_date_world = input$animation_world

    # get selected kpi from kpi filters
    selected_kpi_world = input$selectKPI_world

    # split kpi values into categories for color attribution
    
    discr_param = input$selectDiscretization_world
    
    binpal_world <- colorBin("YlOrRd", 
                       map_data_world@data[, selected_kpi_world],
                       5, 
                       pretty = FALSE)
    
    qpal_world = colorQuantile(palette = "YlOrRd",
                         domain = map_data_world@data[, selected_kpi_world],
                         n = 5)
    
    
    Specify_legend_colpal = function(discr_param){
      if(discr_param == "KPIcolor_binpal"){
        legend_pal = binpal
      }
      else if(discr_param == "KPIcolor_qpal"){
        legend_pal = qpal
      }
      
      return(legend_pal)
    }
    
    legend_pal = Specify_legend_colpal(discr_param = discr_param)

    
    # filter map data based on selected filters
    map_data_selected_world  = map_data_world[map_data_world@data$date_ct == selected_date_world, ]
    

    # generte filteres map
    leafletProxy(mapId = "my_leaf_world", data = map_data_selected_world)  %>%
      clearControls() %>%
      addTiles() %>%
      addPolygons(stroke = TRUE,
                  smoothFactor = 0.3,
                  fillOpacity = 0.8,
                  color = "gray",
                  dashArray = "3",
                  weight = 1,
                  opacity = 0.8,
                  fillColor = ~legend_pal(map_data_selected_world@data[,selected_kpi_world]),
                  highlightOptions = highlightOptions(weight = 2, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
                  label = ~paste0(NAME,": ",prettyNum(map_data_selected_world@data[ ,selected_kpi_world],
                                                           format = "f",
                                                           big.mark = ","))) %>%
      addLegend(pal = legend_pal,
                values = ~ map_data_selected_world@data[ ,selected_kpi_world],
                opacity = 0.7,
                title = selected_kpi_world)
    
    # filter timeserie data based on selected date
    ts_data_selected_world = map_data_world@data[map_data_world@data$date_ct <= selected_date_world, ]

    # filter timeserie data based on selected country
    selected_country_world = input$selectCountry_world
    ts_data_selected_world = ts_data_selected_world[ts_data_selected_world$country %in% selected_country_world, ]

    # sort data by date
    ts_data_selected_world = ts_data_selected_world[order(ts_data_selected_world$date_ct), ]
    
    

    # generate timeserie plot
    output$my_plot_world <- renderPlotly({

      fig = plot_ly(x = ~ts_data_selected_world$date_ct,
                    y = ~ts_data_selected_world[ ,selected_kpi_world],
                    color = ~ts_data_selected_world$country,
                    mode = "lines+markers")

      fig = fig %>% layout(title = paste("Indicator: ", selected_kpi_world, sp = ""),
                           xaxis = list(title = "Dates",
                                        zeroline = TRUE),
                           yaxis = list(title =  selected_kpi_world,
                                        zeroline = TRUE))
      fig
    })
    
    # # prepare table to plot
    # # df = map_data_selected@data
    # df = map_data_world@data
    # 
    # # plot table
    # output$table <- DT::renderDataTable(
    #   DT::datatable(df, options = list(pageLength = 25))
    # )
    
  })
}

##########################
##### Run the application
###########################


shinyApp(ui = ui, server = server)

