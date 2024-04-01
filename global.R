# load libraries
library(shiny)
library(zip)
library(lubridate)
library(dplyr)
library(viridis)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(waiter)
library(sf)
library(ggiraph)
library(ggplot2)
library(plotly)
library(patchwork)
library(leaflet)
library(pals)

library(htmlwidgets)
library(terra)
library(leaflet.extras)
library(leaflet.extras2, include.only = c("easyprintOptions"))
library(shinyWidgets)


# sourcing fonctions
source("functions_shiny.R")

# Settings
drns_long <- c(" ", "Albarine (France)","Bükkösdi (Hungary)","Butižnica (Croatia)","Genal (Spain)","Lepsämänjoki (Finland)","Velička (Czech Rep.)")
drns_short <- c(" ", "Albarine","Bukkosdi","Butiznica","Genal","Lepsamaanjoki","Velicka")
drns_countries <- c(" ", "France", "Hungary", "Croatia", "Spain", "Finland", "Czech")
indicators_long <- c(
  "RelInt: Proportion of river length with intermittent conditions [%]",
  "RelFlow: Proportion of river length with flowing conditions [%]",
  "PatchC: Proportion of reach length with changing flowing and intermittent conditions compared to adjacent downstream reaches [%]",
  "conD: Number of days with dry conditions",
  "conF: Number of days with flowing conditions",
  "durD: Maximum number of consecutive days with dry conditions",
  "durF: Maximum number of consecutive days with flowing conditions",
  "numFreDr: Absolute number of drying events",
  "numFreRW: Absolute number of rewetting events",
  "FstDrE: Julian day of first drying event per year [1-366]",
  "Temp: Temperature [°C]",
  "Precip: Precipitation [mm]",
  "ET: Evapotranspiration [mm]"
)
indicators_short <- c( "RelInt","RelFlow","PatchC",
                       "conD","conF","durD","durF","numFreDr","numFreRW",
                       "FstDrE",
                       "Temp","Precip","ET")

variables_short <- c(" ", "pred_rich", "pred_simpson", "pred_FD",	
                      "pred_Fric", "pred_richHOC", "pred_richEPT",
                      "pred_SimpHOC", "pred_SimpEPT")



network_ind_Y <- c("LengthD", "LengthF", "PatchC", "RelFlow", "RelInt", "Temp","Precip","ET")
network_ind_M <- c("PatchC", "RelFlow", "RelInt", "Temp","Precip","ET")
reach_ind_Y <- c("conD", "conF", "durD", "durF", "numFreDr", "numFreRW", "percFreDr", "percFreRW", "FstDrE")
reach_ind_M <- c("conD", "conF", "durD", "durF", "numFreDr", "numFreRW", "percFreDr", "percFreRW")
ssp_long <- c("SSP1-2.6 Sustainability","SSP3-7.0 Regional rivalry","SSP5-8.5 Fossil-fuelled development")
ssp_short <- c("ssp126","ssp370","ssp585")
gcm_long <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")
gcm_short <- c("gfdl-esm4", "ipsl-cm6a-lr", "mpi-esm1-2-hr", "mri-esm2-0", "ukesm1-0-ll")

### R functions
setShapeStyle <- function( map, data = getMapData(map), layerId,
                           stroke = NULL, color = NULL,
                           weight = NULL, opacity = NULL,
                           fill = NULL, fillColor = NULL,
                           fillOpacity = NULL, dashArray = NULL,
                           smoothFactor = NULL, noClip = NULL,
                           options = NULL, bringToFront = NULL,
                           sendToBack = NULL, popup = NULL
){
  options <- c(list(layerId = layerId),
               options,
               filterNULL(list(stroke = stroke, color = color,
                               weight = weight, opacity = opacity,
                               fill = fill, fillColor = fillColor,
                               fillOpacity = fillOpacity, dashArray = dashArray,
                               smoothFactor = smoothFactor, noClip = noClip,
                               bringToFront = bringToFront,
                               sendToBack = sendToBack,
                               popup = popup
               )))
  # evaluate all options
  options <- evalFormula(options, data = data)
  # make them the same length (by building a data.frame)
  options <- do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))
  
  layerId <- options[[1]]
  style <- options[-1] # drop layer column
  
  #print(list(style=style))
  leaflet::invokeMethod(map, data, "setStyle", "shape", layerId, style);
}

# sourcing app
source('server.R')
source('ui.R')

# Lancer l'app
shinyApp(ui=ui, server=server)
