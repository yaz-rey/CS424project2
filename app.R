library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(shinyjs)

# https://stackoverflow.com/questions/32474823/applying-custom-functions-to-each-row-in-a-dataframe-in-r
# function to create message of the plant name, energy generation, and percent breakdown
genMsg <- function(row){
  # collect the plant name
  msg <- row[1]
  # start building messages
  generations <- "GENERATION (MWh)"
  percents <- "PERCENT BREAKDOWN"
  # add the generation
  # format(1e6, big.mark=",", scientific=FALSE)
  if((!is.na(row[2])) & (as.numeric(row[2]) > 0))
    generations <- paste(generations, "Coal Generation:", format(as.numeric(row[2]), big.mark=",", scientific=FALSE))
  if((!is.na(row[3])) & (as.numeric(row[3]) > 0))
    generations <- paste(generations, "Oil Generation:", format(as.numeric(row[3]), big.mark=",", scientific=FALSE))
  if((!is.na(row[4])) & (as.numeric(row[4]) > 0))
    generations <- paste(generations, "Gas Generation:", format(as.numeric(row[4]), big.mark=",", scientific=FALSE))
  if((!is.na(row[5])) & (as.numeric(row[5]) > 0))
    generations <- paste(generations, "Nuclear Generation:", format(as.numeric(row[5]), big.mark=",", scientific=FALSE))
  if((!is.na(row[6])) & (as.numeric(row[6]) > 0))
    generations <- paste(generations, "Hydro Generation:", format(as.numeric(row[6]), big.mark=",", scientific=FALSE))
  if((!is.na(row[7])) & (as.numeric(row[7]) > 0))
    generations <- paste(generations, "Biomass Generation:", format(as.numeric(row[7]), big.mark=",", scientific=FALSE))
  if((!is.na(row[8])) & (as.numeric(row[8]) > 0))
    generations <- paste(generations, "Wind Generation:", format(as.numeric(row[8]), big.mark=",", scientific=FALSE))
  if((!is.na(row[9])) & (as.numeric(row[9]) > 0))
    generations <- paste(generations, "Solar Generation:", format(as.numeric(row[9]), big.mark=",", scientific=FALSE))
  if((!is.na(row[10])) & (as.numeric(row[10]) > 0))
    generations <- paste(generations, "Geothermal Generation:", format(as.numeric(row[10]), big.mark=",", scientific=FALSE))
  if((!is.na(row[11])) & (as.numeric(row[11]) > 0))
    generations <- paste(generations, "Other Generation:", format(as.numeric(row[11]), big.mark=",", scientific=FALSE))
  
  # message for the percent breakdown
  if(!is.na(row[12]))
    percents <- paste(percents, "Renewable Energy:", paste(as.numeric(row[12])*100, "%", sep=""))
  if(!is.na(row[13]))
    percents <- paste(percents, "Non-Renewable Energy:", paste(as.numeric(row[13])*100, "%", sep=""))
  
  # return full string
  return (paste(msg, "//", generations, "//", percents))
}

# read in plant data for 2018 and clean up numbers
plant18 <- read.csv(file = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/egrid2018_data_v2.csv", sep = ",", header = TRUE) 
plant18$PLGENACL <- as.numeric(gsub(",", "", plant18$PLGENACL))
plant18$PLGENAOL <- as.numeric(gsub(",", "", plant18$PLGENAOL))
plant18$PLGENAGS <- as.numeric(gsub(",", "", plant18$PLGENAGS))
plant18$PLGENANC <- as.numeric(gsub(",", "", plant18$PLGENANC))
plant18$PLGENAHY <- as.numeric(gsub(",", "", plant18$PLGENAHY))
plant18$PLGENABM <- as.numeric(gsub(",", "", plant18$PLGENABM))
plant18$PLGENAWI <- as.numeric(gsub(",", "", plant18$PLGENAWI))
plant18$PLGENASO <- as.numeric(gsub(",", "", plant18$PLGENASO))
plant18$PLGENAGT <- as.numeric(gsub(",", "", plant18$PLGENAGT))
plant18$PLGENAOT <- as.numeric(gsub(",", "", plant18$PLGENAOT))

# generate total generation, renewable generation, non-renewable generation, the percent of renewable and non-renewable from total
plant18$TOTAL <- (plant18$PLGENACL * (plant18$PLGENACL > 0)) + (plant18$PLGENAOL * (plant18$PLGENAOL > 0)) + 
  (plant18$PLGENAGS * (plant18$PLGENAGS > 0)) + (plant18$PLGENANC * (plant18$PLGENANC > 0)) + 
  (plant18$PLGENAHY * (plant18$PLGENAHY > 0)) + (plant18$PLGENABM * (plant18$PLGENABM > 0)) +
  (plant18$PLGENAWI * (plant18$PLGENAWI > 0)) + (plant18$PLGENASO * (plant18$PLGENASO > 0)) + 
  (plant18$PLGENAGT * (plant18$PLGENAGT > 0)) + (plant18$PLGENAOT * (plant18$PLGENAOT > 0))
plant18$RENEW <- (plant18$PLGENAHY * (plant18$PLGENAHY > 0)) + (plant18$PLGENABM * (plant18$PLGENABM > 0)) +
  (plant18$PLGENAWI * (plant18$PLGENAWI > 0)) + (plant18$PLGENASO * (plant18$PLGENASO > 0)) + 
  (plant18$PLGENAGT * (plant18$PLGENAGT > 0))
plant18$NONRENEW <-  (plant18$PLGENACL * (plant18$PLGENACL > 0)) + (plant18$PLGENAOL * (plant18$PLGENAOL > 0)) + 
  (plant18$PLGENAGS * (plant18$PLGENAGS > 0)) + (plant18$PLGENANC * (plant18$PLGENANC > 0)) + 
  (plant18$PLGENAOT * (plant18$PLGENAOT > 0))
plant18$R.PERCENT <- round(plant18$RENEW/plant18$TOTAL,3)
plant18$NR.PERCENT <- round(plant18$NONRENEW/plant18$TOTAL,3)

# generate messages for 2018 plant data 
plant18$MSG <- apply(plant18[,c("PNAME","PLGENACL","PLGENAOL", "PLGENAGS", "PLGENANC", "PLGENAHY", "PLGENABM", "PLGENAWI", "PLGENASO", "PLGENAGT", "PLGENAOT", "R.PERCENT", "NR.PERCENT")], MARGIN = 1, genMsg)

# read in and clean up 2000 plant data
plant00 <- read.csv(file = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/eGRID2000_plant.csv", sep = ",", header = TRUE) 
plant00$LAT <- as.numeric(gsub(",", "", plant00$LAT))
plant00$LON <- as.numeric(gsub(",", "", plant00$LON))
plant00$LON <- plant00$LON * -1

# clean up 2000 plant data by changing into numeric values
plant00$PLGENACL <- as.numeric(gsub(",", "", plant00$PLGENACL))
plant00$PLGENAOL <- as.numeric(gsub(",", "", plant00$PLGENAOL))
plant00$PLGENAGS <- as.numeric(gsub(",", "", plant00$PLGENAGS))
plant00$PLGENANC <- as.numeric(gsub(",", "", plant00$PLGENANC))
plant00$PLGENAHY <- as.numeric(gsub(",", "", plant00$PLGENAHY))
plant00$PLGENABM <- as.numeric(gsub(",", "", plant00$PLGENABM))
plant00$PLGENAWI <- as.numeric(gsub(",", "", plant00$PLGENAWI))
plant00$PLGENASO <- as.numeric(gsub(",", "", plant00$PLGENASO))
plant00$PLGENAGT <- as.numeric(gsub(",", "", plant00$PLGENAGT))
plant00$PLGENAOT <- as.numeric(gsub(",", "", plant00$PLGENAOT))

# generate total generation, renewable generation, non-renewable generation, and relative percents
plant00$TOTAL <- (plant00$PLGENACL * (plant00$PLGENACL > 0)) + (plant00$PLGENAOL * (plant00$PLGENAOL > 0)) + 
  (plant00$PLGENAGS * (plant00$PLGENAGS > 0)) + (plant00$PLGENANC * (plant00$PLGENANC > 0)) + 
  (plant00$PLGENAHY * (plant00$PLGENAHY > 0)) + (plant00$PLGENABM * (plant00$PLGENABM > 0)) +
  (plant00$PLGENAWI * (plant00$PLGENAWI > 0)) + (plant00$PLGENASO * (plant00$PLGENASO > 0)) + 
  (plant00$PLGENAGT * (plant00$PLGENAGT > 0)) + (plant00$PLGENAOT * (plant00$PLGENAOT > 0))
plant00$RENEW <- (plant00$PLGENAHY * (plant00$PLGENAHY > 0)) + (plant00$PLGENABM * (plant00$PLGENABM > 0)) +
  (plant00$PLGENAWI * (plant00$PLGENAWI > 0)) + (plant00$PLGENASO * (plant00$PLGENASO > 0)) + 
  (plant00$PLGENAGT * (plant00$PLGENAGT > 0))
plant00$NONRENEW <-  (plant00$PLGENACL * (plant00$PLGENACL > 0)) + (plant00$PLGENAOL * (plant00$PLGENAOL > 0)) + 
  (plant00$PLGENAGS * (plant00$PLGENAGS > 0)) + (plant00$PLGENANC * (plant00$PLGENANC > 0)) + 
  (plant00$PLGENAOT * (plant00$PLGENAOT > 0))
plant00$R.PERCENT <- round(plant00$RENEW/plant00$TOTAL,3)
plant00$NR.PERCENT <- round(plant00$NONRENEW/plant00$TOTAL,3)

# generate messages 
plant00$MSG <- apply(plant00[,c("PNAME","PLGENACL","PLGENAOL", "PLGENAGS", "PLGENANC", "PLGENAHY", "PLGENABM", "PLGENAWI", "PLGENASO", "PLGENAGT", "PLGENAOT", "R.PERCENT", "NR.PERCENT")], MARGIN = 1, genMsg)

# read in and clean up 2010 data
plant10 <- read.csv(file = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/eGRID2010_Data.csv", sep = ",", header = TRUE) 
plant10$PLGENACL <- as.numeric(gsub(",", "", plant10$PLGENACL))
plant10$PLGENAOL <- as.numeric(gsub(",", "", plant10$PLGENAOL))
plant10$PLGENAGS <- as.numeric(gsub(",", "", plant10$PLGENAGS))
plant10$PLGENANC <- as.numeric(gsub(",", "", plant10$PLGENANC))
plant10$PLGENAHY <- as.numeric(gsub(",", "", plant10$PLGENAHY))
plant10$PLGENABM <- as.numeric(gsub(",", "", plant10$PLGENABM))
plant10$PLGENAWI <- as.numeric(gsub(",", "", plant10$PLGENAWI))
plant10$PLGENASO <- as.numeric(gsub(",", "", plant10$PLGENASO))
plant10$PLGENAGT <- as.numeric(gsub(",", "", plant10$PLGENAGT))
plant10$PLGENAOT <- as.numeric(gsub(",", "", plant10$PLGENAOT))

# generate total generation, renewable generation, non-renewable generation, and relative percents
plant10$TOTAL <- (plant10$PLGENACL * (plant10$PLGENACL > 0)) + (plant10$PLGENAOL * (plant10$PLGENAOL > 0)) + 
  (plant10$PLGENAGS * (plant10$PLGENAGS > 0)) + (plant10$PLGENANC * (plant10$PLGENANC > 0)) + 
  (plant10$PLGENAHY * (plant10$PLGENAHY > 0)) + (plant10$PLGENABM * (plant10$PLGENABM > 0)) +
  (plant10$PLGENAWI * (plant10$PLGENAWI > 0)) + (plant10$PLGENASO * (plant10$PLGENASO > 0)) + 
  (plant10$PLGENAGT * (plant10$PLGENAGT > 0)) + (plant10$PLGENAOT * (plant10$PLGENAOT > 0))
plant10$RENEW <- (plant10$PLGENAHY * (plant10$PLGENAHY > 0)) + (plant10$PLGENABM * (plant10$PLGENABM > 0)) +
  (plant10$PLGENAWI * (plant10$PLGENAWI > 0)) + (plant10$PLGENASO * (plant10$PLGENASO > 0)) + 
  (plant10$PLGENAGT * (plant10$PLGENAGT > 0))
plant10$NONRENEW <-  (plant10$PLGENACL * (plant10$PLGENACL > 0)) + (plant10$PLGENAOL * (plant10$PLGENAOL > 0)) + 
  (plant10$PLGENAGS * (plant10$PLGENAGS > 0)) + (plant10$PLGENANC * (plant10$PLGENANC > 0)) + 
  (plant10$PLGENAOT * (plant10$PLGENAOT > 0))
plant10$R.PERCENT <- round(plant10$RENEW/plant10$TOTAL,3)
plant10$NR.PERCENT <- round(plant10$NONRENEW/plant10$TOTAL,3)

# generate messages
plant10$MSG <- apply(plant10[,c("PNAME","PLGENACL","PLGENAOL", "PLGENAGS", "PLGENANC", "PLGENAHY", "PLGENABM", "PLGENAWI", "PLGENASO", "PLGENAGT", "PLGENAOT", "R.PERCENT", "NR.PERCENT")], MARGIN = 1, genMsg)

# generate Illinois data for first page (2018, 2000, 2010)
il18 <- subset(plant18, PSTATABB=="IL")
il18$PSTATABB <- factor(il18$PSTATABB)
il18$PNAME<- factor(il18$PNAME)

il00 <- subset(plant00, PSTATABB=="IL")
il00$PSTATABB <- factor(il00$PSTATABB)
il00$PNAME<- factor(il00$PNAME)

il10 <- subset(plant10, PSTATABB=="IL")
il10$PSTATABB <- factor(il10$PSTATABB)
il10$PNAME<- factor(il10$PNAME)

# mapping list that converts state name to abbreviation
states <- c(state.name)
abb <- c(state.abb)
states_map <- c(states=abb)

# mapping list that converts state abbreviation to name
abb1 <- c(state.abb, "U.S.")
state1 <- c(state.name, "United States")
abb_map <- c(state1=abb1)

# color map to energy source
color <- c("#cf3c29", "#38a9db", "#71ae26", "#d152b8", "#ff8ee9", "#ffca90", "#303030", "#6f7f23", "#0066a2", "#9d3135","#a3a3a3")
source <- c("Coal", "Oil", "Gas", "Nuclear", "Hydro", "Biomass", "Wind", "Solar", "Geothermal", "Other", "No data available")
color_map <- c(source = color)

# reference to generate colors for markers
# https://rstudio.github.io/leaflet/markers.html
# https://github.com/JackDougherty/leaflet-maps-with-google-sheets-dev/issues/5

# function to generate a map for Illinois data
genMap<-function(input, data){
  map <- leaflet()
  map <- addTiles(map)
  # collect input based off of UI 
  addtomap <- input
  if (is.null(input)){
    return (map)
  }
  # find the values we will need to plot on the map
  if("All" %in% input){
    addtomap <- c(source)
  }
  if("Renewable" %in% input){
    addtomap <- c("Hydro", "Biomass", "Wind", "Solar", "Geothermal")
  }
  if("Non-Renewable" %in% input){
    addtomap <- c("Coal", "Oil", "Gas", "Nuclear", "Other")
  }
  
  # check values that we need to plot
  # add markers depending on the energy source
  if("Coal" %in% addtomap){
    map <- addMarkers(map, lng = subset(data, PLGENACL > 0)$LON, lat = subset(data, PLGENACL > 0)$LAT, popup = subset(data, PLGENACL > 0)$PNAME, 
                      icon = icons(
                        iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/red.png", 
                        iconWidth = 30, iconHeight = 40,
                      ))
  }
  if("Oil" %in% addtomap){
    map <- addMarkers(map, lng = subset(data, PLGENAOL > 0)$LON, lat = subset(data, PLGENAOL > 0)$LAT, popup = subset(data, PLGENAOL > 0)$PNAME, 
                      icon = icons(
                        iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/blue.png", 
                        iconWidth = 30, iconHeight = 40,
                      ))
  }
  if("Gas" %in% addtomap){
    map <- addMarkers(map, lng = subset(data, PLGENAGS > 0)$LON, lat = subset(data, PLGENAGS > 0)$LAT, popup = subset(data, PLGENAGS > 0)$PNAME,
                             icon = icons(
                               iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/green.png", 
                               iconWidth = 30, iconHeight = 40,
                             ))
  }
  if("Nuclear" %in% addtomap){
    map <- addMarkers(map, lng = subset(data, PLGENANC > 0)$LON, lat = subset(data, PLGENANC > 0)$LAT, popup = subset(data, PLGENANC > 0)$PNAME, 
                             icon = icons(
                               iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/purple.png", 
                               iconWidth = 30, iconHeight = 40,
                             ))
  }
  if("Hydro" %in% addtomap){
    map <- addMarkers(map, lng = subset(data, PLGENAHY > 0)$LON, lat = subset(data, PLGENAHY > 0)$LAT, popup = subset(data, PLGENAHY > 0)$PNAME, 
                             icon = icons(
                               iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/pink.png", 
                               iconWidth = 30, iconHeight = 40,
                             ))
  }
  if("Biomass" %in% addtomap){
    map <- addMarkers(map, lng = subset(data, PLGENABM > 0)$LON, lat = subset(data, PLGENABM > 0)$LAT, popup = subset(data, PLGENABM > 0)$PNAME, 
                             icon = icons(
                               iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/beige.png", 
                               iconWidth = 30, iconHeight = 40,
                             ))
  }
  if("Wind" %in% addtomap){
    map <- addMarkers(map, lng = subset(data, PLGENAWI > 0)$LON, lat = subset(data, PLGENAWI > 0)$LAT, popup = subset(data, PLGENAWI > 0)$PNAME, 
                             icon = icons(
                               iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/black.png", 
                               iconWidth = 30, iconHeight = 40,
                             ))
  }
  if("Solar" %in% addtomap){
    map <- addMarkers(map, lng = subset(data, PLGENASO > 0)$LON, lat = subset(data, PLGENASO > 0)$LAT, popup = subset(data, PLGENASO > 0)$PNAME, 
                             icon = icons(
                               iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/darkgreen.png", 
                               iconWidth = 30, iconHeight = 40,
                             ))
  }
  if("Geothermal" %in% addtomap){
    map <- addMarkers(map, lng = subset(data, PLGENAGT > 0)$LON, lat = subset(data, PLGENAGT > 0)$LAT, popup = subset(data, PLGENAGT > 0)$PNAME, 
                             icon = icons(
                               iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/darkblue.png", 
                               iconWidth = 30, iconHeight = 40,
                             ))
  }
  if("Other" %in% addtomap){
    map <- addMarkers(map, lng = subset(data, PLGENAOT > 0)$LON, lat = subset(data, PLGENAOT > 0)$LAT, popup = subset(data, PLGENAOT > 0)$PNAME, 
                             icon = icons(
                               iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/darkred.png", 
                               iconWidth = 30, iconHeight = 40,
                             ))
  }
  if("No data available" %in% addtomap){
    map <- addMarkers(map, lng = subset(data, is.na(PLGENACL))$LON, lat = subset(data, is.na(PLGENACL))$LAT, popup = subset(data, is.na(PLGENACL))$PNAME, 
                             icon = icons(
                               iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/lightgray.png", 
                               iconWidth = 30, iconHeight = 40,
                             ))
  }
  
  # add a legend with all values based on the input
  map <- addLegend(map, title="Energy Source", "bottomright", colors = subset(color_map, source %in% addtomap), labels=addtomap, opacity = 1)

  return (map)
  
}

# https://rstudio.github.io/leaflet/basemaps.html
genMap2<-function(input, data, layout){
  map <- leaflet()
  # select layout for map
  if (layout=="Default"){
    map <- addTiles(map)
  }
  if (layout=="Toner"){
    map <- addProviderTiles(map, providers$Stamen.Toner)
  }
  if (layout=="Terrain"){
    map <- addProviderTiles(map, providers$Stamen.Terrain)
  }
  
  # collect input to find sources to plot
  addtomap <- input
  if (is.null(input)){
    return (map)
  }
  
  if("All" %in% input){
    addtomap <- c(source)
  }
  if("Renewable" %in% input){
    addtomap <- c("Hydro", "Biomass", "Wind", "Solar", "Geothermal")
  }
  if("Non-Renewable" %in% input){
    addtomap <- c("Coal", "Oil", "Gas", "Nuclear", "Other")
  }
 
  # plot markers for each energy source
  # change the size of icon depending on the total generation
  if("Coal" %in% addtomap){
    d <- subset(data, PLGENACL > 0)
    map <- addMarkers(map, lng = d$LON, lat = d$LAT, popup = d$MSG,
                             icon = icons(
                               iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/red.png", 
                               iconWidth = log(d$PLGENACL)*3, iconHeight = log(d$PLGENACL)*4,
                             ))
  }
  if("Oil" %in% addtomap){
    d <- subset(data, PLGENAOL > 0)
    map <- addMarkers(map, lng = d$LON, lat = d$LAT, popup = d$MSG, 
                             icon = icons(
                               iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/blue.png", 
                               iconWidth = log(d$PLGENAOL)*3, iconHeight = log(d$PLGENAOL)*4,
                             ))
  }
  if("Gas" %in% addtomap){
    d <- subset(data, PLGENAGS > 0)
    map <- addMarkers(map, lng = d$LON, lat = d$LAT, popup = d$MSG, 
                             icon = icons(
                               iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/green.png", 
                               iconWidth = log(d$PLGENAGS)*3, iconHeight = log(d$PLGENAGS)*4
                             ))
  }
  if("Nuclear" %in% addtomap){
    d <- subset(data, PLGENANC > 0)
    map <- addMarkers(map, lng = d$LON, lat = d$LAT, popup = d$MSG, 
                             icon = icons(
                               iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/purple.png", 
                               iconWidth = log(d$PLGENANC)*3, iconHeight = log(d$PLGENANC)*4
                             ))
  }
  if("Hydro" %in% addtomap){
    d <- subset(data, PLGENAHY > 0)
    map <- addMarkers(map, lng = d$LON, lat = d$LAT, popup = d$MSG, 
                             icon = icons(
                               iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/pink.png", 
                               iconWidth = log(d$PLGENAHY)*3, iconHeight = log(d$PLGENAHY)*4
                             ))
  }
  if("Biomass" %in% addtomap){
    d <- subset(data, PLGENABM > 0)
    map <- addMarkers(map, lng = d$LON, lat = d$LAT, popup = d$MSG, 
                             icon = icons(
                               iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/beige.png", 
                               iconWidth = log(d$PLGENABM)*3, iconHeight = log(d$PLGENABM)*4
                             ))
  }
  if("Wind" %in% addtomap){
    d <- subset(data, PLGENAWI > 0)
    map <- addMarkers(map, lng = d$LON, lat = d$LAT, popup = d$MSG, 
                             icon = icons(
                               iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/black.png", 
                               iconWidth = log(d$PLGENAWI)*3, iconHeight = log(d$PLGENAWI)*4
                             ))
  }
  if("Solar" %in% addtomap){
    d <- subset(data, PLGENASO > 0)
    map <- addMarkers(map, lng = d$LON, lat = d$LAT, popup = d$MSG, 
                             icon = icons(
                               iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/darkgreen.png", 
                               iconWidth = log(d$PLGENASO)*3, iconHeight = log(d$PLGENASO)*4
                             ))
  }
  if("Geothermal" %in% addtomap){
    d <- subset(data, PLGENAGT > 0)
    map <- addMarkers(map, lng = d$LON, lat = d$LAT, popup = d$MSG, 
                             icon = icons(
                               iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/darkblue.png", 
                               iconWidth = log(d$PLGENAGT)*3, iconHeight = log(d$PLGENAGT)*4
                             ))
  }
  if("Other" %in% addtomap){
    d <- subset(data, PLGENAOT > 0)
    map <- addMarkers(map, lng = d$LON, lat = d$LAT, popup = d$MSG, 
                             icon = icons(
                               iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/darkred.png", 
                               iconWidth = log(d$PLGENAOT)*3, iconHeight = log(d$PLGENAOT)*4
                             ))
  }
  if("No data available" %in% addtomap){
    map <- addMarkers(map, lng = subset(data, is.na(PLGENACL))$LON, lat = subset(data, is.na(PLGENACL))$LAT, popup = subset(data, is.na(PLGENACL))$MSG, 
                             icon = icons(
                               iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/lightgray.png", 
                               iconWidth = 30, iconHeight = 40,
                             ))
  }
  
  # plot legend
  map <- addLegend(map, title="Energy Source", "bottomright", colors = subset(color_map, source %in% addtomap), labels=addtomap, opacity = 1)

  return (map)
}

# https://stackoverflow.com/questions/53038090/r-leaflet-limitations-how-many-markers-does-a-leaflet-map-support
genMap3<-function(input, data, vals){
  # use canvas for optimization
  map <- leaflet(options = leafletOptions(preferCanvas = TRUE))
  map <- addTiles(map)
  
  # plot within the slider input
  addtomap <- input
  
  # check input to plot select sources
  if (is.null(input)){
    return (map)
  }
  if("All" %in% input){
    addtomap <- c(source)
  }
  if("Renewable" %in% input){
    addtomap <- c("Hydro", "Biomass", "Wind", "Solar", "Geothermal")
  }
  if("Non-Renewable" %in% input){
    addtomap <- c("Coal", "Oil", "Gas", "Nuclear", "Other")
  }
  
  # plot individual markers
  # have a cluster option for optimization
  if("Coal" %in% addtomap){
    d <- subset(data, PLGENACL > 0 & (PLGENACL > vals[1] & PLGENACL < vals[2]))
    map <- addMarkers(map, lng = d$LON, lat = d$LAT, popup = d$MSG, clusterOptions = markerClusterOptions(),
                      icon = icons(
                        iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/red.png", 
                        iconWidth = 30, iconHeight = 40,
                      ))
  }
  if("Oil" %in% addtomap){
    d <- subset(data, PLGENAOL > 0 & (PLGENAOL > vals[1] & PLGENAOL < vals[2]))
    map <- addMarkers(map, lng = d$LON, lat = d$LAT, popup = d$MSG, clusterOptions = markerClusterOptions(),
                      icon = icons(
                        iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/blue.png", 
                        iconWidth = 30, iconHeight = 40,
                      ))
  }
  if("Gas" %in% addtomap){
    d <- subset(data, PLGENAGS > 0 & (PLGENAGS > vals[1] & PLGENAGS < vals[2]))
    map <- addMarkers(map, lng = d$LON, lat = d$LAT, popup = d$MSG, clusterOptions = markerClusterOptions(),
                      icon = icons(
                        iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/green.png", 
                        iconWidth = 30, iconHeight = 40
                      ))
  }
  if("Nuclear" %in% addtomap){
    d <- subset(data, PLGENANC > 0 & (PLGENANC > vals[1] & PLGENANC < vals[2]))
    map <- addMarkers(map, lng = d$LON, lat = d$LAT, popup = d$MSG, clusterOptions = markerClusterOptions(),
                      icon = icons(
                        iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/purple.png", 
                        iconWidth = 30, iconHeight = 40
                      ))
  }
  if("Hydro" %in% addtomap){
    d <- subset(data, PLGENAHY > 0 & (PLGENAHY > vals[1] & PLGENAHY < vals[2]))
    map <- addMarkers(map, lng = d$LON, lat = d$LAT, popup = d$MSG, clusterOptions = markerClusterOptions(),
                      icon = icons(
                        iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/pink.png", 
                        iconWidth = 30, iconHeight = 40
                      ))
  }
  if("Biomass" %in% addtomap){
    d <- subset(data, PLGENABM > 0 & (PLGENABM > vals[1] & PLGENABM < vals[2]))
    map <- addMarkers(map, lng = d$LON, lat = d$LAT, popup = d$MSG, clusterOptions = markerClusterOptions(),
                      icon = icons(
                        iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/beige.png", 
                        iconWidth = 30, iconHeight = 40
                      ))
  }
  if("Wind" %in% addtomap){
    d <- subset(data, PLGENAWI > 0 & (PLGENAWI > vals[1] & PLGENAWI < vals[2]))
    map <- addMarkers(map, lng = d$LON, lat = d$LAT, popup = d$MSG, clusterOptions = markerClusterOptions(),
                      icon = icons(
                        iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/black.png", 
                        iconWidth = 30, iconHeight = 40
                      ))
  }
  if("Solar" %in% addtomap){
    d <- subset(data, PLGENASO > 0 & (PLGENASO > vals[1] & PLGENASO < vals[2]))
    map <- addMarkers(map, lng = d$LON, lat = d$LAT, popup = d$MSG, clusterOptions = markerClusterOptions(),
                      icon = icons(
                        iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/darkgreen.png", 
                        iconWidth = 30, iconHeight = 40
                      ))
  }
  if("Geothermal" %in% addtomap){
    d <- subset(data, PLGENAGT > 0 & (PLGENAGT > vals[1] & PLGENAGT < vals[2]))
    map <- addMarkers(map, lng = d$LON, lat = d$LAT, popup = d$MSG, clusterOptions = markerClusterOptions(),
                      icon = icons(
                        iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/darkblue.png", 
                        iconWidth = 30, iconHeight = 40
                      ))
  }
  if("Other" %in% addtomap){
    d <- subset(data, PLGENAOT > 0 & (PLGENAOT > vals[1] & PLGENAOT < vals[2]))
    map <- addMarkers(map, lng = d$LON, lat = d$LAT, popup = d$MSG, clusterOptions = markerClusterOptions(),
                      icon = icons(
                        iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/darkred.png", 
                        iconWidth = 30, iconHeight = 40
                      ))
  }
  if("No data available" %in% addtomap){
    map <- addMarkers(map, lng = subset(data, is.na(PLGENACL))$LON, lat = subset(data, is.na(PLGENACL))$LAT, popup = subset(data, is.na(PLGENACL))$MSG, clusterOptions = markerClusterOptions(),
                      icon = icons(
                        iconUrl = "https://raw.githubusercontent.com/yreyes6/CS424project2/main/lightgray.png", 
                        iconWidth = 30, iconHeight = 40,
                      ))
  }
  
  # plot two legends to describe the cluster sizes and for the energy sources
  map <- addLegend(map, title="Energy Source", "bottomright", colors = subset(color_map, source %in% addtomap), labels=addtomap, opacity = 1)
  map <- addLegend(map, title="Cluster Size", "bottomright", colors = c("lightgreen", "yellow", "orange"), labels=c("Small", "Medium", "Large"), opacity = 1)
  
  return (map)
}

# start of UI 
ui <- dashboardPage(
  dashboardHeader(title="CS 424 Project 2"),
  dashboardSidebar(
    sidebarMenu(
      # three pages for project 2
      menuItem("Illinois Power Plants", tabName = "page1", icon = icon("industry")),
      menuItem("State Power Plant Comparison", tabName = "page2", icon = icon("map-marker")),
      menuItem("U.S. Power Plants", tabName = "page3", icon = icon("flag-usa")),
      menuItem("About", tabName = "about", icon = icon("question-circle"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(tabName = "page1",
              fluidRow(
                box(title = "Illinois Power Plants in 2018", solidHeader = TRUE, width=12,height="800px", leafletOutput("leaf1", height = 600),
                    checkboxGroupInput("choice1", "Filter Energy Source:", inline=TRUE, 
                                       choices =
                                         list("Coal", "Oil", "Gas", "Nuclear", "Hydro",
                                              "Biomass", "Wind", "Solar", "Geothermal", "Other", "Renewable", 
                                              "Non-Renewable", "All"), selected = "All"
                    ),
                    fluidRow(column(12,"Note: All checkboxes in this application (all pages) require 'All', 'Renewable', and 'Non-Renewable' to be the only checked box to display correctly Uncheck any other energy sources.
                                    Similarly, uncheck 'All', 'Renewable', and 'Non-Renewable' when isolating individual energy sources.")),
                    actionButton("reset1", "RESET")
                )
              )
            
      ), # end Illinois level tab
      tabItem(tabName = "page2",
              fluidRow(
                box(
                    fluidRow(
                      column(6, radioButtons("mapType", label ="Select Map Layout", choices=c("Default", "Toner", "Terrain"), inline=TRUE, 
                                                    selected="Default")),
                      column(6, checkboxInput("link", label = "Link Checkboxes", value = FALSE))
                      
                    ), 
                    width=12, height="75px"),
                
                box(title = "State 1 Power Plants", solidHeader = TRUE, width=6,height="700px", 
                    fluidRow(
                      column(6, selectInput("loc1", "Select a State", choices=states, selected = "Illinois")),
                      column(6, selectInput("yr1", "Select a Year", choices=c("2000", "2010", "2018"), selected = "2000")),
                    ),
                    leafletOutput("leaf2", height = 450),
                    checkboxGroupInput("choice2", "Filter Energy Source:", inline=TRUE, 
                                       choices =
                                         list("Coal", "Oil", "Gas", "Nuclear", "Hydro",
                                              "Biomass", "Wind", "Solar", "Geothermal", "Other", "Renewable", 
                                              "Non-Renewable", "All"), selected = "All"
                    ), actionButton("reset2", "RESET")
                ),
                box(title = "State 2 Power Plants", solidHeader = TRUE, width=6,height="700px", 
                    fluidRow(
                      column(6, selectInput("loc2", "Select a State", choices=states, selected = "Illinois")),
                      column(6, selectInput("yr2", "Select a Year", choices=c("2000", "2010", "2018"), selected = "2018")),
                    ),
                    
                    leafletOutput("leaf3", height = 450),
                    checkboxGroupInput("choice3", "Filter Energy Source:", inline=TRUE, 
                                       choices =
                                         list("Coal", "Oil", "Gas", "Nuclear", "Hydro",
                                              "Biomass", "Wind", "Solar", "Geothermal", "Other", "Renewable", 
                                              "Non-Renewable", "All"), selected = "All"
                    ), actionButton("reset3", "RESET")
                )
              )
    
      ), # end of STATE level tab
      tabItem(tabName="page3",
              fluidRow(
                box(title = "U.S. Power Plants", solidHeader = TRUE, width=12,height="800px", 
                    fluidRow(
                      column(6, selectInput("yr3", "Select a Year", choices=c("2000", "2010", "2018"), selected = "2018")),
                      column(6, sliderInput("s1", "Filter Generation Amount (MWh)", min=0,max=32000000, value=c(0,32000000)))
                    ),
                    leafletOutput("leaf4", height = 500),
                    checkboxGroupInput("choice4", "Filter Energy Source:", inline=TRUE, 
                                       choices =
                                         list("Coal", "Oil", "Gas", "Nuclear", "Hydro",
                                              "Biomass", "Wind", "Solar", "Geothermal", "Other", "Renewable", 
                                              "Non-Renewable", "All")
                    ),  
                    fluidRow(column(12,"Note: Plants with more than one energy source will have multiple markers and counted in different cluster markers.")),
                    actionButton("reset4", "RESET"),
                )
              )
      ), # end of U.S. level page
      tabItem(tabName = "about",
              box(title="About the Data", solidHeader=TRUE, HTML("The data is provided by the United States Environmental Protection Agency. 
                  The data is a  'comprehensive source of data on the environmental characteristics of almost all electric power generated in the United States.' 
                  The link to the data can be found here: https://www.epa.gov/egrid/download-data <br/><br/> 
                  This application has made use of the following files listed on the site: <br/>
                                                                 <b>eGRID2018v2 Data File</b><br/>
                                                                 <b>Download eGRID historial files (1996-2016):</b> Specifically 2000 and 2010.<br/><br/>
                                                                 The data was cleaned for further exploration in R and Shiny.")),
              box(title="About the Application", solidHeader=TRUE, HTML("This application was written by Yazmin Reyes. Its objective is to visualize power plants (location, energy sources, generation) across the United States within 2000, 2010, and 2018. It was written in R and Shiny with the assistance of the following: <br/>
                                                                        <b>Professor Andrew Johnson's Shiny App Example:</b> https://www.evl.uic.edu/aej/424/evlWeatherForR.zip <br/>
                                                                        <b>Shiny Dashboard Library:</b> https://rstudio.github.io/shinydashboard/ <br/>
                                                                        <b>Leaflet for R Documentation:</b> https://rstudio.github.io/leaflet/ <br/>
                                                                        References to <b>Stack Overflow</b> and <b>Piazza</b> for specific R/Shiny/data usage <br/><br/>
                                                                        Last Revised: 3/13/2021"))
      ) # end of ABOUT tab
    )
  )
)

server <- function(input, output, session) { 
  # collect information on the year and location
  stateReactive1 <- reactive({
    plant <- plant18
    if(input$yr1 == "2000"){
      plant <- plant00
    }
    else if(input$yr1 == "2010"){
      plant <- plant10
    }
    else{
      plant <- plant18
    }
    
    subset(plant, PSTATABB==states_map[states==input$loc1])
    })

  stateReactive2 <- reactive({
    # collect year and location and return for comparison
    plant <- plant18
    if(input$yr2 == "2000"){
      plant <- plant00
    }
    else if(input$yr2 == "2010"){
      plant <- plant10
    }
    else{
      plant <- plant18
    }
    
    subset(plant, PSTATABB==states_map[states==input$loc2])
  })
  
  # collect year for U.S. data 
  usReactive <- reactive({
    plant <- plant18
    if(input$yr3 == "2000"){
      plant <- plant00
    }
    else if(input$yr3 == "2010"){
      plant <- plant10
    }
    else{
      plant <- plant18
    }
    
    plant
  })
  
  # https://shiny.rstudio.com/articles/action-buttons.html
  # page 1
  observeEvent(input$reset1, {
    # update UI elements to reset view
    updateCheckboxGroupInput(session, "choice1", "Filter Energy Source:", inline=TRUE, 
                               choices =
                               list("Coal", "Oil", "Gas", "Nuclear", "Hydro",
                                    "Biomass", "Wind", "Solar", "Geothermal", "Other", "Renewable", 
                                    "Non-Renewable", "All"), selected = "All")
    
    req("All" %in% input$choice1)
    output$leaf1 <- renderLeaflet({
      map <- genMap(input$choice1, il18)
      map <- setView(map, lng = -88.993217, lat = 40.477089, zoom = 6) 
      map
      
    })
  })
  
  # update UI elements to reset view
  # page 2
  observeEvent(input$reset2, {
    updateSelectInput(session, "loc1", "Select a State", choices=states, selected="Illinois")
    updateSelectInput(session, "yr1", "Select a Year", choices=c("2000", "2010", "2018"), selected="2000")
    updateCheckboxGroupInput(session, "choice2", "Filter Energy Source:", inline=TRUE, 
                             choices =
                               list("Coal", "Oil", "Gas", "Nuclear", "Hydro",
                                    "Biomass", "Wind", "Solar", "Geothermal", "Other", "Renewable", 
                                    "Non-Renewable", "All"), selected = "All")
    
    req("All" %in% input$choice2)
    output$leaf2 <- renderLeaflet({
      plant <- stateReactive1()
      genMap2(input$choice2, plant, input$mapType)
    })
  })
  
  # update UI elements to reset view
  # page 2
  observeEvent(input$reset3, {
    updateSelectInput(session, "loc2", "Select a State", choices=states, selected="Illinois")
    updateSelectInput(session, "yr2", "Select a Year", choices=c("2000", "2010", "2018"), selected="2018")
    updateCheckboxGroupInput(session, "choice3", "Filter Energy Source:", inline=TRUE, 
                             choices =
                               list("Coal", "Oil", "Gas", "Nuclear", "Hydro",
                                    "Biomass", "Wind", "Solar", "Geothermal", "Other", "Renewable", 
                                    "Non-Renewable", "All"), selected = "All")
    req("All" %in% input$choice3)
    output$leaf3 <- renderLeaflet({
      plant <- stateReactive2()
      genMap2(input$choice3, plant, input$mapType)
    })
  })
  
  # update UI elements to reset view
  # page 3
  observeEvent(input$reset4, {
    updateSelectInput(session, "yr3", "Select a Year", choices=c("2000", "2010", "2018"), selected="2018")
    updateCheckboxGroupInput(session, "choice4", "Filter Energy Source:", inline=TRUE, 
                             choices =
                               list("Coal", "Oil", "Gas", "Nuclear", "Hydro",
                                    "Biomass", "Wind", "Solar", "Geothermal", "Other", "Renewable", 
                                    "Non-Renewable", "All"))
    
      output$leaf4 <- renderLeaflet({
      plant <- usReactive()
      map <- genMap3(input$choice4, plant, input$s1)
      map <- setView(map, lng = -95.712891, lat = 37.09024, zoom = 4) 
      map
    })
  })
  
  # when link mode on, update both checkbox inputs to match 
  # page 2
  observeEvent(input$choice2, {
    req(input$link)
    updateCheckboxGroupInput(session, "choice3", "Filter Energy Source:", inline=TRUE, 
                             choices =
                               list("Coal", "Oil", "Gas", "Nuclear", "Hydro",
                                    "Biomass", "Wind", "Solar", "Geothermal", "Other", "Renewable", 
                                    "Non-Renewable", "All"), selected = input$choice2)
  })
  
  # when link mode on, update both checkbox inputs to match 
  # page 2
  observeEvent(input$choice3, {
    req(input$link)
    updateCheckboxGroupInput(session, "choice2", "Filter Energy Source:", inline=TRUE, 
                             choices =
                               list("Coal", "Oil", "Gas", "Nuclear", "Hydro",
                                    "Biomass", "Wind", "Solar", "Geothermal", "Other", "Renewable", 
                                    "Non-Renewable", "All"), selected = input$choice3)
  })
  
  # update the slider input min and max when year changed
  # update other UI
  # page 3
  observeEvent(input$yr3, {
    plant <- plant18
    if(input$yr3 == "2000"){
      plant <- plant00
    }
    else if(input$yr3 == "2010"){
      plant <- plant10
    }
    else{
      plant <- plant18
    }
    
    # https://stat.ethz.ch/R-manual/R-devel/library/base/html/Round.html
    l = ceiling(max(subset(plant, TOTAL > 0)$TOTAL)) + 1
    updateSliderInput(session, "s1", "Filter Generation Amount (MWh)", min=0,max=l, value=c(0,l))
    updateCheckboxGroupInput(session, "choice4", "Filter Energy Source:", inline=TRUE, 
                             choices =
                               list("Coal", "Oil", "Gas", "Nuclear", "Hydro",
                                    "Biomass", "Wind", "Solar", "Geothermal", "Other", "Renewable", 
                                    "Non-Renewable", "All"))
  })
  
  # update UI when energy source is filtered - slider input 
  observeEvent(input$choice4, {
    plant <- plant18
    if(input$yr3 == "2000"){
      plant <- plant00
    }
    else if(input$yr3 == "2010"){
      plant <- plant10
    }
    else{
      plant <- plant18
    }
    vals <- c()
    
    # collect input
    addtomap <- input$choice4
    print(addtomap)
    # break down input to display different min and max values
    if("All" %in% input$choice4){
      addtomap <- c(source)
    }
    if("Renewable" %in% input$choice4){
      addtomap <- c("Hydro", "Biomass", "Wind", "Solar", "Geothermal")
    }
    if("Non-Renewable" %in% input$choice4){
      addtomap <- c("Coal", "Oil", "Gas", "Nuclear", "Other")
    }
    if("Coal" %in% addtomap){
      vals <- c(vals, na.omit(plant$PLGENACL))
    }
    if("Oil" %in% addtomap){
      vals <- c(vals, na.omit(plant$PLGENAOL))
    }
    if("Gas" %in% addtomap){
      vals <- c(vals, na.omit(plant$PLGENAGS))
    }
    if("Nuclear" %in% addtomap){
      vals <- c(vals, na.omit(plant$PLGENANC))
    }
    if("Hydro" %in% addtomap){
      vals <- c(vals, na.omit(plant$PLGENAHY))
    }
    if("Biomass" %in% addtomap){
      vals <- c(vals, na.omit(plant$PLGENABM))
    }
    if("Wind" %in% addtomap){
      vals <- c(vals, na.omit(plant$PLGENAWI))
    }
    if("Solar" %in% addtomap){
      vals <- c(vals, na.omit(plant$PLGENASO))
    }
    if("Geothermal" %in% addtomap){
      vals <- c(vals, na.omit(plant$PLGENAGT))
    }
    if("Other" %in% addtomap){
      vals <- c(vals, na.omit(plant$PLGENAOT))
    }
    print(vals)
    print(max(vals))
    l = ceiling(max(vals)) + 1
    updateSliderInput(session, "s1", "Filter Generation Amount (MWh)", min=0,max=l, value=c(0,l))
  })
  
  # leaflet output - page 1
  # include view for IL
  output$leaf1 <- renderLeaflet({
    map <- genMap(input$choice1, il18)
    map <- setView(map, lng = -88.993217, lat = 40.477089, zoom = 6) 
    map
  })
 
  # leaflet output - page 2 - state 1
  output$leaf2 <- renderLeaflet({
    plant <- stateReactive1()
    genMap2(input$choice2, plant, input$mapType)
  })
  
  # leaflet output - page 2 - state 2
   output$leaf3 <- renderLeaflet({
    plant <- stateReactive2()
    genMap2(input$choice3, plant, input$mapType)
  })
  
   # leaflet output - page 3
   # set view for U.S. 
  output$leaf4 <- renderLeaflet({
    plant <- usReactive()
    map <- genMap3(input$choice4, plant, input$s1)
    map <- setView(map, lng = -95.712891, lat = 37.09024, zoom = 4) 
    map
  })

}

shinyApp(ui, server)

