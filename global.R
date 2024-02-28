library(dplyr)
library(shinyWidgets)

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

# Add directory of static resources to Shiny's web server
addResourcePath(prefix = "imgResources", directoryPath = "www")
myImgResources <- c("white.png", "dryver-logo.png")
myShp <- c("Mitchell_pu.shp", "Mitchell_pu.qmd", "Mitchell_pu.prj", "Mitchell_pu.dbf", "Mitchell_pu.cpg",
           "Mitchell_pu.shx")


#global variables
shape.subset <- ""
a <- ""
csv_input_charged <- FALSE
final_shape <- c()
first_plot <- 0
number_of_shapes <- 0
shapes_storage <- c()
names_shapes <- c()
colours_shapes <- c("black","green", "red", "gray")
fill_shapes <- c("transparent","green", "red", "gray")
solution_names <- c()
names_solutions_actions <- ""
names_solutions_benefits <- ""
solution_created <- FALSE

#Do we exploration marxan inputs or prioriactions inputs?
marxan <- TRUE
