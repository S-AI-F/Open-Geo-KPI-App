
library(shinyjs)

install.packages("shinyjs")

# get map data
geodata_world <- ne_countries()
# rename country code and name variables
names(geodata_world)[names(geodata_world) == "iso_a3"] <- "iso3c"
names(geodata_world)[names(geodata_world) == "name"] <- "NAME"

# get worldbank data
attribute_data <- wb(
  indicator = c("EN.ATM.PM25.MC.M3", "SP.POP.TOTL"),
  return_wide = TRUE,
  POSIXct = TRUE
)



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
    "DT.DOD.DECT.CD","SM.POP.NETM"),
  return_wide = TRUE,
  POSIXct = TRUE
)

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
    External_debt_stocks = DT.DOD.DECT.CD,
    Net_migration = SM.POP.NETM)

head(attribute_data)


# get worldbank data
exposure <- wb(
  indicator = "EN.ATM.PM25.MC.M3",
  return_wide = TRUE,
  POSIXct = TRUE
)
head(exposure)

EN.ATM.PM25.MC.M3 <- wb(
  indicator = "EN.ATM.PM25.MC.M3",
  return_wide = TRUE,
  POSIXct = TRUE
)

population <- wb(
  indicator = "SP.POP.TOTL",
  return_wide = TRUE,
  POSIXct = TRUE
)
head(population)

attribute_data_world <- exposure %>% 
  full_join(population[,c("iso3c","date","SP.POP.TOTL")], by = c("iso3c","date"))

KPI_start_end_date_fill = function(KPIlist){
  KPI_start_end_date = data.frame(indicator = KPIlist)
}
KPI_start_end_date = data.frame(indicator = c("EN.ATM.PM25.MC.M3", "SP.POP.TOTL"))
KPI_start_end_date[KPI_start_end_date$indicator == "EN.ATM.PM25.MC.M3", "stratdate"] = min(EN.ATM.PM25.MC.M3$date)
KPI_start_end_date[KPI_start_end_date$indicator == "EN.ATM.PM25.MC.M3", "enddate"] = max(EN.ATM.PM25.MC.M3$date)

KPI_start_end_date = data.frame(KPI = as.character(),
                                startdate = as.character(),
                                enddate = as.character())

kpi_list = c("EN.ATM.PM25.MC.M3","SP.POP.TOTL")
for(i in 1:length(kpi_list)){
  KPI_name = kpi_list[1]
  KPI_start_end_date[,"KPI"] = KPI_name
  KPI_start_end_date[,"startdate"] = min()
}

# merge with attribute data with geodata
map_data_world = sp::merge(geodata_world,
                           attribute_data_world,
                     by = "iso3c",
                     duplicateGeoms = TRUE)

map_data_world = map_data_world[!is.na(map_data_world@data$date_ct), ]

date_vector_world = seq.Date(from = min(map_data_world@data$date_ct),
                       to = max(map_data_world@data$date_ct),
                       by = "years")



# get selected date from animation
selected_date_world = date_vector_world[10]

# get selected kpi from kpi filters
selected_kpi_world = "SP.POP.TOTL"

# discretization param

# discr_param = "KPIcolor_binpal"
discr_param = "KPIcolor_qpal"

binpal <- colorBin("YlOrRd", 
                   map_data_world@data[, selected_kpi_world],
                   5, 
                   pretty = FALSE)

qpal = colorQuantile(palette = "YlOrRd",
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

toto = map_data_world@data[ ,selected_kpi_world]


# discretize indicator value and fill color

map_data_selected_world@data$KPIcolor_qpal = qpal(map_data_selected_world@data[,selected_kpi_world])
map_data_selected_world@data$KPIcolor_binpal = binpal(map_data_selected_world@data[,selected_kpi_world])


# generte filteres map

leaflet(map_data_selected_world)  %>%
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
              # fillColor = ~binpal(map_data_selected_world@data[,selected_kpi_world]),
              highlightOptions = highlightOptions(weight = 2, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
              label = ~paste0(NAME,": ",prettyNum(map_data_selected_world@data[ ,selected_kpi_world],
                                                  format = "f",
                                                  big.mark = ","))) %>% 
  addLegend(pal = legend_pal,
            values = ~ map_data_selected_world@data[ ,selected_kpi_world],
            opacity = 0.5,
            title = selected_kpi_world)

  addLegend(pal = binpal,
            values = ~ map_data_selected_world@data[ ,selected_kpi_world],
            opacity = 0.5,
            title = selected_kpi_world)

  



