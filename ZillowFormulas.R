library(tidyverse)
library(stringr)
library(DT)
library(tidyquant)
library(plotly)
library(igraph)
library(visNetwork)
library(ggiraph)
library(ggplot2)
library(DBI)
library(ggmap)
library(leaflet)

#import data
data <- read.csv('Files/zillow.csv', stringsAsFactors = F)

data$zip <- as.character(data$Zip) #If I don't want it in the list, put this line after the CharacterFields assignment

CharacterFields <- data %>% select_if(is.character) %>%
  names()

#For color coding. Can I make this dynamic by creating this selection from dt within Select function? 
percentiles <- data$MedianPrice %>% quantile(c(.25,.75),na.rm=T) 
percentiles2 <- data$ForecastYoYPctChange %>% quantile(c(.25,.75),na.rm=T) 



stateSelect <- function(dt,stateFilter){
  dt <- if(stateFilter == "NO FILTER"){
    dt <- dt
  } else {
    dt <- dt[dt$State == stateFilter,]
  }
}



plotScatter <- function(dt) {
  fit <- lm(MedianPrice ~ PriceToRentRatio, data = dt)
  ggplotly(ggplot(dt, aes(x=PriceToRentRatio, y=MedianPrice, 
                          text=paste("Metro:",dt$Metro, "\nCounty:",dt$County, "\nZip:",dt$Zip, 
                                     "\nMedian Price($):",dt$MedianPrice,"\nMedian Price Per Square Feet($):",
                                     dt$MedianPricePerSqft,"\nZillow Home Value Index(ZHVI):",dt$Zhvi,
                                     "\nOne Year Forecast(% Change):",dt$ForecastYoYPctChange ), group=1)) +
             geom_jitter(mapping=aes(color=ForecastYoYPctChange), size=4, alpha=0.7) +
             #geom_point_interactive(aes(data_id=dt, onclick= dt$onclick)) +
             ggtitle(paste(dt$State,"relationship between Median Price(y) and Price to Rent Ratio(x) color coded by Zillow Forecast(YoY)")) +
             scale_color_gradient(low="red", high="green") +
             #geom_smooth(method = lm) +
             labs(x="Price to Rent Ratio",y="Median Price($)",color="One Year Forecast(%Change)"))
}

plotScatterPlotly <- function(dt){
  plot_ly(data = dt, x=dt$PriceToRentRatio, y=dt$MedianPrice, color= ~ForecastYoYPctChange,
          text = ~paste("Metro:",dt$Metro, "\nCounty:",dt$County, "\nZip:",dt$Zip, "\nMedian Price($):",
                        dt$MedianPrice,"\nMedian Price Per Square Feet($):",dt$MedianPricePerSqft,
                        "\nZillow Home Value Index(ZHVI):",dt$Zhvi,"\nOne Year Forecast(% Change):",
                        dt$ForecastYoYPctChange )) %>% 
          layout(title='Relationship between Median Price(y) and Price to Rent Ratio(x) color coded by Zillow Forecast(YoY)',
                 xaxis=list(title='Price to Rent Ratio'),
                 yaxis=list(title='Median'))
}



plot3d <- function(dt) {
  plot_ly(dt, x =~PriceToRentRatio, y =~MarketHealthIndex, z = ~MedianPricePerSqft, 
          color = ~ForecastYoYPctChange,colors=c("red","green"),
          text = paste("Metro:",dt$Metro, "\nCounty:",dt$County, "\nZip:",dt$Zip,"\nMedian Price($):",
                       dt$MedianPrice,"\nMedian Price Per Square Feet:",dt$MedianPricePerSqft,
                       "\nZillow Home Value Index(ZHVI):",dt$Zhvi,"\nOne Year Forecast(% Change):",
                       dt$ForecastYOYPctChange )) %>%
    add_markers(opacity=0.7) %>%
    layout(title=paste(dt$State,"3d chart colored by Zillow Forecast(YoY)"), 
           scene = list(xaxis = list(title = "Price to Rent Ratio"),
                        yaxis = list(title = 'Market Health Index'),
                        zaxis = list(title = 'Median Price Per Sqft($)')))
}


map <- function(dt) {
  leaflet() %>%
    addTiles() %>%
    addMarkers(lng = dt$Longitude, lat = dt$Latitude, clusterOptions = markerClusterOptions(), 
               popup = paste("Zip:",dt$Zip, "<br/>Metro:",dt$Metro, "<br/>County:",dt$County, 
                             "<br/>Median Price($):",dt$MedianPrice,"<br/>Zillow Home Value Index(ZHVI):",
                             dt$Zhvi,"<br/>One Year Forecast(% Change):",dt$ForecastYoYPctChange),
               group='markers') %>%
    addProviderTiles(providers$Stamen.Terrain, group='terrain') %>%
    addProviderTiles(providers$Stamen.Watercolor, group='watercolor') %>%
    addProviderTiles(providers$Esri.WorldImagery, group='satellite') %>%
    addProviderTiles(providers$NASAGIBS.ModisTerraLSTDay, group='land surface temp') %>%
    addProviderTiles(providers$JusticeMap.income, group='income(zoom out)') %>%
    addTiles( group='normal') %>%
    #addProviderTiles(providers$OpenInfraMap.Power, group='power grid') %>%
    #addProviderTiles(providers$OpenInfraMap.Telecom, group='telecom') %>%
    addProviderTiles(providers$OpenRailwayMap, group='railway') %>%
    addProviderTiles(providers$JusticeMap.asian, group='asian population') %>%
    addProviderTiles(providers$JusticeMap.black, group='african american population') %>%
    addProviderTiles(providers$JusticeMap.hispanic, group='hispanic population') %>%
    addProviderTiles(providers$JusticeMap.white, group='white population') %>%
    addProviderTiles(providers$JusticeMap.nonWhite, group='non-white population') %>%
    addLayersControl(
      baseGroups = c('normal','satellite','terrain','watercolor'),
      overlay = c('railway','income(zoom out)','asian population','african american population',
                  'hispanic population',
                  'white population','non-white population', 'land surface temp','markers'),
      options = layersControlOptions(collapsed = FALSE)
    ) %>% hideGroup(c('land surface temp',"income(zoom out)",'asian population','african american population','hispanic population',
                      'white population','non-white population','railway'))
    
}

mapPres <- function(dt) {
  leaflet() %>%
    addTiles() %>%
    addMarkers(lng = dt$Longitude, lat = dt$Latitude, clusterOptions = markerClusterOptions(), 
               popup = paste("Zip:",dt$Zip, "<br/>Metro:",dt$Metro, "<br/>County:",dt$County, 
                             "<br/>Median Price($):",dt$MedianPrice,"<br/>Zillow Home Value Index(ZHVI):",
                             dt$Zhvi,"<br/>One Year Forecast(% Change):",dt$ForecastYoYPctChange),
               group='markers')

}

#Can I make a button that shows the different type of map?
mapSat <- function(dt) {
  leaflet() %>%
    addProviderTiles("Esri.WorldImagery") %>%
    addMarkers(lng = dt$Longitude, lat = dt$Latitude, clusterOptions = markerClusterOptions(), 
               popup = paste("Zip:",dt$Zip, "<br/>Metro:",dt$Metro,  "<br/>County:",dt$County, 
                             "<br/>Median Price($):",dt$MedianPrice,"<br/>Zillow Home Value Index(ZHVI):",
                             dt$Zhvi,"<br/>One Year Forecast(% Change):",dt$ForecastYoYPctChange))
}


NewMap <- function(dt) {
  bc_bbox <- make_bbox(lat = Latitude, lon = Longitude, data = dt)
  bc_big <- get_map(location = bc_bbox, source = "google", maptype = "hybrid")
  ggmap(bc_big) + 
    geom_point(data = data, mapping = aes(x = Longitude, y = Latitude, color = ForecastYoYPctChange),size=4)+
    scale_colour_gradient(low = "red", high="green")+
    labs(x="lon",y="lat",color="One Year Forecast(%Change)")
  
  }

#ggplot map. This is a good link: http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
ggplot(data = data, mapping = aes(x = Longitude, y = Latitude, group=SizeRank)) + 
  geom_polygon(color = "black", fill = "gray")

