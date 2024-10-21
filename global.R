# load libraries
library(shiny)
library(zip)
library(dplyr)
library(shinydashboard)
library(shinyjs)
library(waiter)
library(sf)
library(ggplot2)
library(plotly)
library(patchwork)
library(leaflet)
library(pals)
library(gridExtra)

library(htmlwidgets)
library(terra)
library(leaflet.extras)
library(leaflet.extras2, include.only = c("easyprintOptions"))
library(shinyWidgets)


#RCBC---------------------------------------------------------------------------
# 
# # Run system commands to install required libraries
# system("apt-get install -y coinor-libcbc-dev coinor-libclp-dev")
# 
# 
# if (!require(remotes))
#   install.packages("remotes")
# remotes::install_github("dirkschumacher/rcbc")
# https://optimapp.shinyapps.io/OptimApp/


#-------------------------------------------------------------------------------


library(prioritizr)
library(Rsymphony)
library(stringr)

library(sass)
library(shiny)
library(glue)
library(tibble)
library(stats)
library(scales)
library(shinyFeedback)
library(conflicted)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("box", "shinydashboard")
# conflict_prefer("dashboardPage", "shinydashboardPlus")
# conflict_prefer("dashboardHeader", "shinydashboard")
# conflict_prefer("dashboardSiderbar", "shinydashboard")
conflict_prefer("layout", "plotly")



# sourcing fonctions
source("functions_shiny.R")

# Settings

#drns
drns_long <- c(" ","Albarine (France)","Bükkösdi (Hungary)","Butižnica (Croatia)","Genal (Spain)","Lepsämänjoki (Finland)","Velička (Czech Rep.)")
drns_short <- c(" ","Albarine","Bukkosdi","Butiznica","Genal","Lepsamaanjoki","Velicka")
drns_countries <- c(" ","France", "Hungary", "Croatia", "Spain", "Finland", "Czech")

# drns_long <- c(" ","Albarine (France)")
# drns_short <- c(" ","Albarine")
# drns_countries <- c(" ","France")

variables_short <- c(" ","FD",	
                     "FR", "temp_beta", "beta",
                     "dem", "co2",
                     "dr_lr", "dr_ss", "er_rip",
                     "er_sl", "flo_rip", "flo_sl")

variables_short_percentile <- c("FD", "FR", "temp_beta", "beta", "dem")
variables_short_sd <- c("FD", "FR", "dem", "co2")

variables_short_biodiversity <- c("FD", "FR", "temp_beta", "beta")
variables_short_ecological_functions <- c("dem", "co2")
variables_short_ecosystem_services <- c("dr_lr", "dr_ss", "er_rip", "er_sl", "flo_rip", "flo_sl")

variables_long <- c(" ","Predicted functional diversity",	
                    "Predicted functional richness",
                    "Temporal Beta diversity",
                    "Beta diversity",
                    "Leaf litter decomposition",
                    "Co2 sequestration",
                    "Drought local recharge index",
                    "Drought surface storage index",
                    "Erosion regulation in riparian areas",
                    "Erosion regulation in slopes",
                    "Flood regulation in riparian areas",
                    "Flood regulation in slopes")

scale_list <- c("Current (2021)", "Projection (2040 - 2070)")
scale_list_short <- c("current", "future")

campaign_list <- c("Jan-Feb", "Mar-Apr", "May-Jun", "Jul-Aug", "Sep-Oct", "Nov-Dec")

sync_first <- TRUE



network_ind_Y <- c("LengthD", "LengthF", "PatchC", "RelFlow", "RelInt", "Temp","Precip","ET")
network_ind_M <- c("PatchC", "RelFlow", "RelInt", "Temp","Precip","ET")
reach_ind_Y <- c("conD", "conF", "durD", "durF", "numFreDr", "numFreRW", "percFreDr", "percFreRW", "FstDrE")
reach_ind_M <- c("conD", "conF", "durD", "durF", "numFreDr", "numFreRW", "percFreDr", "percFreRW")
ssp_long <- c("SSP1-2.6 Sustainability","SSP3-7.0 Regional rivalry","SSP5-8.5 Fossil-fuelled development")
ssp_short <- c("ssp126","ssp370","ssp585")
gcm_long <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")
gcm_short <- c("gfdl-esm4", "ipsl-cm6a-lr", "mpi-esm1-2-hr", "mri-esm2-0", "ukesm1-0-ll")
last_sol = c()
last_model = c()
cons_optimize = 0


css <- sass(sass_file("www/css/styles.scss"))

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

weightedPickerInput <- function(
    id,
    choices,
    label = NULL,
    selected = NULL,
    min_weight = 0,
    max_weight = 1,
    default_weight = 0.2,
    step = 0.01
) {
  constants <- glue::glue(
    "{
      minWeight: {{ min_weight }},
      maxWeight: {{ max_weight }},
      defaultWeight: {{ default_weight }},
      step: {{ step }},
    }",
    .open = "{{",
    .close = "}}"
  )
  # 1 dimension vector should be wrapped into the list
  if (is.null(names(choices))) {
    choices = list(` ` = choices)
  }
  div(
    class = "weighted-stats-selector",
    pickerInput(
      inputId = id,
      label = label,
      choices = choices,
      multiple = TRUE,
      selected = selected,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 2")
    ),
    tags$script(
      glue("initStatsWeightModifier('{id}', {constants})")
    )
  )
}


# sourcing app
source('server.R')
source('ui.R')

# Lancer l'app
shinyApp(ui=ui, server=server)
