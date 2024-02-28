library(shiny)
library(leaflet)
library(dplyr)
library(terra)
library(viridis)
library(tidyr)
library(stringr)
library(leaflet.extras)
library(leaflet.extras2)
library(scales)
library(rcbc)
library(leafem)
library(prioritizr)

function(input, output, session){
  
  #maximum size of files
  options(shiny.maxRequestSize=1000*1024^2)
  
  ## Interactive Map 
  # Create the map -------------------------------------------------------------
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
      addProviderTiles(providers$Esri.WorldShadedRelief, group = "Elevation") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      setView(lng = -50, lat = 18, zoom = 3) %>%
      onRender(
        "function(el, x) {
          L.control.zoom({
            position:'bottomright'
          }).addTo(this);
        }") %>%
      addLayersControl(
        baseGroups = c("CartoDB", "Elevation", "Satellite","Without background"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% leafem::addMouseCoordinates() 
  })
  
  #Uploading maps --------------------------------------------------------------
  shape <- reactive({
    
    req(input$filemap)
    
    # shpdf is a data.frame with the name, size, type and
    # datapath of the uploaded files
    shpdf <- input$filemap
    
    # Construct the path to the shapefile
    path_shp <- paste0("data/", shpdf, "/river_network_corrected.shp")
    
    # Read and preprocess the shapefile
    shape <- terra::vect(path_shp)
    shape <- project(shape,  crs("+proj=longlat +datum=WGS84 +no_defs"))
    shape <- terra::aggregate(shape, by = "cat", count=FALSE, overwrite=TRUE, dissolve=FALSE)
    shape$cat <- as.character(shape$cat)
    
    #To simplify the geom if that is the case
    #shape <- simplifyGeom(shape, preserveTopology = TRUE)
    
    if("id" %in% names(shape)){
      shape$id <- as.character(shape$id)
      names_shapes <<- c(names_shapes, "Base")
      #shapes_storage <<- c(shapes_storage, shape)
      #number_of_shapes <<- number_of_shapes + 1
      
      disable("filemap")
    }
    else{
      shape$id <- shape$cat
    }
    
    validate(need("cat" %in% names(shape), "The shapefile must be a 'id' column"))
    
    # Assuming the ID attribute is named "ID," replace it with the actual name in your shapefile
    missing_ids <- is.na(shape$cat)
    shape <- shape[!missing_ids, ]
    
    shape
    
  })
  
  #Updating map ----------------------------------------------------------------
  plot_shape <- reactive({
    
    shape <- shape()
    
    longlat <- terra::geom(shape)
    long_shape <- mean(longlat[,3])
    lat_shape <- mean(longlat[,4])
    type_shape <- terra::geomtype(shape)
    colour_boundary <- "black"
    colour_fill <- "transparent"
    
    
    if("cat" %in% names(shape)){
      popup_pu <- sprintf(
        "<strong>ID %s</strong><br/>",
        shape$cat) %>% 
        lapply(htmltools::HTML)
    }
    else{
      popup_pu <- NA
    }
    
    if(type_shape == "lines"){
      
      leafletProxy("map", data = shape) %>%
        clearControls() %>%
        setView(lng = long_shape, lat = lat_shape, zoom = 8) %>%
        addPolylines(group = "Base",
                     weight = 2, 
                     color = "black",
                     fill = FALSE, 
                     opacity = input$range,
                     popup = popup_pu, 
                     layerId = ~cat, 
                     highlightOptions = highlightOptions(
                       weight = 5,
                       bringToFront = TRUE,
                       sendToBack = TRUE)
        )%>%
        addDrawToolbar(
          position = "bottomright",
          editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
        )  %>%
        addStyleEditor(position = "bottomright", 
                       openOnLeafletDraw = FALSE) %>%
        addLayersControl(
          baseGroups = c("CartoDB", "Elevation", "Satellite","Without background"),
          options = layersControlOptions(collapsed=FALSE)) %>%
        leaflet.extras2::addEasyprint(options = easyprintOptions(
          title = 'Print map',
          position = 'bottomright',
          exportOnly = TRUE, 
          sizeModes = "A4Landscape"))
    }
  })
  
  observe({
    if (!is.null(session$clientData$url_search)) {
      #print(number_of_shapes)
      if(number_of_shapes > 0){
        disable("filemap")
        
        plot_shape()
        enable("range")
      }
    }
  })
  
  
  #Change opacity---------------------------------------------------------------
  observeEvent(input$range, {
    
    shape <- shape()
    
    leafletProxy("map", data = shape) %>%
      setShapeStyle(layerId = ~cat, 
                    opacity = input$range,
                    options = list(group = "Base"))
    
  })
  
  #Change shapefile initial----------------------------------------------------
  observeEvent(input$filemap, {
    
    if(input$filemap != ""){
      # Remove the Leaflet map
      leafletProxy("map") %>% clearMarkers() %>% clearShapes()
    }
    
    plot_shape()
    enable("range")
    enable("specific_plot")
    
    if(input$filemap == "Albarine"){
      updateSelectInput(session = session, "specific_plot",
                        choices = list(`Costs` = list(),
                                       `Features distribution` = list("Drought Regulation", 
                                                                      "Erosion Control", 
                                                                      "Flood Regulation (slopes)",
                                                                      "Thermal Regulation (REST)",
                                                                      "Thermal Regulation (CONS)")),
                        selected = "total_distribution_features"
      )
    }
    else{
      updateSelectInput(session = session, "specific_plot",
                        choices = list(`Costs` = list(),
                                       `Features distribution` = list()),
                        selected = ""
      )
    }
  }) 
  
  
  #csv -------------------------------------------------------------------------
  csv_files <- reactive({
    
    req(input$filemap)
    
    if(input$filemap == "Albarine"){
      
      # List all files in the directory
      all_files <- list.files("data/Albarine", full.names = TRUE, recursive = TRUE)
      merged_shp_files <- grep("merged\\.shp$", all_files, value = TRUE)
      
      
      for(i in 1:length(merged_shp_files)){
        
        shp <- terra::vect(merged_shp_files[i])
        crs(shp) <- "+proj=longlat +datum=WGS84 +no_defs"
        if(i == 1){
          val <- values(shp)
          val <- val[,c("cat", "total")]
          
          result <- val %>%
            group_by(cat) %>%
            summarize(mean = mean(total))
          
          dist_features_data <- result
          colnames(dist_features_data) <- c("cat", "mean")
          dist_features_data$feature <- i
        }
        else{
          val <- values(shp)
          val <- val[,c("cat", "_mean")]
          colnames(val) <- c("cat", "value")
          
          result <- val %>%
            group_by(cat) %>%
            summarize(mean = mean(value))
          
          result$feature <- i
          dist_features_data <- rbind(dist_features_data, result)
          
        }
      }
      
      
      pu_data <- data.frame(id = unique(dist_features_data$cat),
                            cost = 1,
                            status = 0)
      
      features_data <- data.frame(id = seq(merged_shp_files),
                                  target = 0,
                                  name = c("Drought Regulation", 
                                           "Erosion Control", 
                                           "Flood Regulation (slopes)",
                                           "Thermal Regulation (REST)",
                                           "Thermal Regulation (CONS)"))
      
      colnames(dist_features_data) <- c("pu", "amount", "feature")
      
      files <- list(pu_data, features_data, dist_features_data)
    }
    else{
      files <- list(pu_data = data.frame(id = 1,
                                         monitoring_cost = 1,
                                         status = 0),
                    features_data = data.frame(id = 1,
                                               target_recovery = 1,
                                               status = 0),
                    dist_features_data = data.frame(pu = 1,
                                                    feature = 1,
                                                    amount = 0),
                    threats = data.frame(id = 1,
                                         name = 1,
                                         cost = 0),
                    dist_threats = data.frame(pu = 1,
                                              threat = 1,
                                              action_cost = 0,
                                              amount = 1))
    }
    
    enable("specific_plot")
    
    files
  })
  
  #Change opacity---------------------------------------------------------------
  observeEvent(input$filecsv, {
    
    files <- csv_files()
    shape <- shape_with_csv()
    
    if(!is.null(files) && !is.null(shape)){
      
      if(isTRUE(marxan)){
        popup_map <- sprintf(
          "<strong>ID %s</strong><br/>
         Monitoring cost: %g <br/>
         Features: %g <br/>",
          shape[[1]]$id, 
          shape[[1]]$monitoring_cost,
          shape[[1]]$total_distribution_features) %>% 
          lapply(htmltools::HTML)
      }
      else{
        popup_map <- sprintf(
          "<strong>ID %s</strong><br/>
         Monitoring cost: %g <br/>
         Features: %g <br/>
         Threats: %g <br/>",
          shape[[1]]$id, 
          shape[[1]]$monitoring_cost,
          shape[[1]]$total_distribution_features,
          shape[[1]]$total_distribution_threats) %>% 
          lapply(htmltools::HTML)
      }
      
      type_shape <- terra::geomtype(shape[[1]])
      
      if(type_shape == "lines"){
        
        leafletProxy("map", data = shape[[1]]) %>%
          clearControls() %>%
          leaflet::clearGroup("Base") %>% 
          addPolylines(group = "Base",
                       weight = 2, 
                       color = "transparent",
                       opacity = 1,
                       fillOpacity = input$range,
                       popup = popup_map, 
                       fillColor = "white", 
                       layerId = ~id
          ) 
      }
      else if(type_shape == "polygons"){
        leafletProxy("map", data = shape[[1]]) %>%
          clearControls() %>%
          addPolygons(group = "Base",
                      weight = 0.5,
                      color = "black",
                      smoothFactor = 0.3,
                      fillOpacity = input$range,
                      popup = popup_map,
                      fillColor = "white",
                      layerId = ~id,
                      highlightOptions = highlightOptions(
                        color = "black",
                        weight = 5,
                        fillOpacity = NULL,
                        bringToFront = TRUE,
                        sendToBack = TRUE))
      }
      else if(type_shape == "points"){
        leafletProxy("map", data = shape[[1]]) %>%
          clearControls() %>%
          addCircles(group = "Base",
                     weight = 0.5,
                     color = "black",
                     smoothFactor = 0.3,
                     fillOpacity = input$range,
                     popup = popup_map,
                     fillColor = "white",
                     layerId = ~id,
                     highlightOptions = highlightOptions(
                       color = "black",
                       weight = 5,
                       fillOpacity = NULL,
                       bringToFront = TRUE,
                       sendToBack = TRUE))
      }
      
      if(isTRUE(marxan)){
        updateSelectInput(session = session, "specific_plot",
                          choices = list(`Costs` = list(shape[[2]]),
                                         `Features distribution` = shape[[3]],
                                         `Threats distribution` = list(""),
                                         `Threats x Features` = list("")),
                          selected = "total_distribution_features"
        )
      }
      else{
        updateSelectInput(session = session, "specific_plot",
                          choices = list(`Costs` = shape[[2]],
                                         `Features distribution` = shape[[3]],
                                         `Threats distribution` = shape[[4]],
                                         `Threats x Features` = shape[[5]]),
                          selected = "total_distribution_features"
        )
      }
      
      enable("solutionsImport")
    }
    csv_input_charged <<- TRUE
  })
  
  # Change map filtered---------------------------------------------------------
  observeEvent(input$specific_plot, {
    
    plot_maps_explorer()
    
  }) 
  
  # Updating maps with data frames----------------------------------------------
  shape_with_csv <- reactive({
    
    files <- csv_files()
    shape <- shape()
    
    if(!is.null(files) && !is.null(shape)){
      
      shapeData <- terra::merge(shape, files[[1]], by_sp = "id", by_df = "id")
      
      
      #features_data
      if(!"name" %in% colnames(files[[2]])){
        files[[2]]$name <- paste0("id_", files[[2]]$id)
      }
      
      df_aux_features <- dplyr::right_join(files[[3]], files[[2]], by = c("feature" = "id"))
      
      #files[[3]]$feature <- df_aux_features$name
      files[[3]] <- tidyr::pivot_wider(df_aux_features, 
                                       names_from = name, 
                                       values_from = amount, 
                                       values_fill = 0,
                                       names_sort = TRUE)
      
      if(NCOL(files[[3]][,-1]) == 1){
        files[[3]]$total_distribution_features <- as.numeric(files[[3]][,-1]  > 0)
      }else{
        files[[3]]$total_distribution_features <- rowSums(files[[3]][,-1] > 0)
      }
      
      shapeData <- terra::merge(shapeData, files[[3]], by.x = "id", by.y = "pu")
      
      shape <- list(shapeData,  
                    c("monitoring_cost"),
                    c(unique(df_aux_features$name), "total_distribution_features"))
      
      
      shape
    }
  })
  

  #Plotting maps explorer-------------------------------------------------------
  plot_maps_explorer <- reactive({
    
    req(input$specific_plot)
    shape <- shape_with_csv()
    index <- which(names(shape[[1]]) == input$specific_plot)
    
    if(input$specific_plot %in% shape[[3]]){
      if(input$specific_plot == "total_distribution_features"){
        
        #shape[[1]][[index]][,1] <- dplyr::na_if(shape[[1]][[index]][,1], 0)
        shape[[1]][[index]][,1] <- ifelse(shape[[1]][[index]][,1] == 0, NA, shape[[1]][[index]][,1])
        
        pal <- colorBin(palette="viridis", domain=shape[[1]][[index]][,1], na.color="#e3e3e3")
        title_legend <- "Number of features"
      }
      else{
        factor_feat <- factor(shape[[1]][[index]][,1])
        #shape[[1]][[index]][,1] <- dplyr::na_if(shape[[1]][[index]][,1], 0)
        shape[[1]][[index]][,1] <- ifelse(shape[[1]][[index]][,1] == 0, NA, shape[[1]][[index]][,1])
        
        if(length(levels(factor_feat)) > 5){
          pal <- colorNumeric(palette="viridis", domain=shape[[1]][[index]][,1], na.color="#e3e3e3")
        }else{
          pal <- colorFactor(palette="viridis", domain=shape[[1]][[index]][,1], na.color="#e3e3e3")
        }
        title_legend <- "Feature"
      }
    }
    else if(isTRUE(marxan)){
      
      shape[[1]][[index]][,1] <- dplyr::na_if(shape[[1]][[index]][,1], 0)
      pal <- colorNumeric( palette="plasma", domain=shape[[1]][[index]][,1], na.color="transparent")
      title_legend <- "Cost"
    }
    
    
    if(isTRUE(marxan)){
      popup_map <- sprintf(
        "<strong>ID %s</strong><br/>
         Value: %g <br/>",
        shape[[1]]$id, 
        shape[[1]][[index]][,1]) %>% 
        lapply(htmltools::HTML)
    }
    
    type_shape <- terra::geomtype(shape[[1]])
    
    if(type_shape == "lines"){
      
      leafletProxy("map", data = shape[[1]]) %>%
        clearControls() %>%
        setShapeStyle(layerId = ~id,
                      color = ~pal(shape[[1]][[index]][,1]),
                      bringToFront = TRUE,
                      sendToBack = TRUE,
                      options = list(group = "Base")) %>%
        addLegend(group = "Base",
                  pal = pal,
                  title = title_legend,
                  values = shape[[1]][[index]][,1],
                  position = "topright") %>%
        leaflet.extras2::addEasyprint(options = easyprintOptions(
          title = 'Print map',
          position = 'bottomright',
          exportOnly = TRUE, 
          sizeModes = "A4Landscape"))
      
    }
    
    enable("validate")
  })
  #---------------------------------------------------------------------------------------
  #PRIORITIZR-----------------------------------------------------------------------------
  #---------------------------------------------------------------------------------------
  
  #Step 1-----------------------------------------------------------------------
  step1_data <- reactive({
    files <- csv_files()
    
    if(!is.null(files)){
      
      rij <- (files[[3]])
      
      colnames(rij) <- c("pu", "amount", "species")
      
      rij <- na.omit(rij)
      
      output$step1 <- renderPrint({
        
        tryCatch({ 
          validated <- prioritizr::problem(files[[1]], 
                                           files[[2]], 
                                           cost_column = "cost", 
                                           rij = rij)
    
         print("Inputs charged");
        }, 
        error = function(e) {
          validated <- prioritizr::problem(files[[1]], 
                                           files[[2]], 
                                           cost_column = "cost", 
                                           rij = rij)
        })
      })
      validated <- prioritizr::problem(files[[1]], 
                                       files[[2]], 
                                       cost_column = "cost", 
                                       rij = rij)
    }
  })
  
  #Step 2-----------------------------------------------------------------------
  step2_data <- reactive({
    data_step1 <- step1_data()
    
    output$step2 <- renderPrint({
      tryCatch({ 
        validated <- data_step1 %>%
          add_min_set_objective() %>%
          add_relative_targets(input$range) %>%
          add_binary_decisions() %>%
          add_cbc_solver(gap = 0)
        print("Model valided");
      }, 
      error = function(e) {
        validated <- data_step1 %>%
          add_min_set_objective() %>%
          add_relative_targets(input$range) %>%
          add_binary_decisions() %>%
          add_cbc_solver(gap = 0)
      })
    })
    validated <- data_step1 %>%
      add_min_set_objective() %>%
      add_relative_targets(input$range) %>%
      add_binary_decisions() %>%
      add_cbc_solver(gap = 0)
    
  })
  
  #Step 3-----------------------------------------------------------------------
  step3_data <- reactive({
    data_step2 <- step1_data()
    
    #validated <- prioriactions::solve(
    #   a = data_step2, gap_limit = 0.1, solver = "cbc",cores = 10, output_file = FALSE)
    
    print(input$target)
    
    validated <- data_step2 %>%
      add_min_set_objective() %>%
      add_relative_targets(input$target) %>%
      add_binary_decisions() %>%
      add_cbc_solver(gap = 0)
    
    print(validated)
    
    validated <- solve(validated, force = TRUE)
    
    
    updateTabsetPanel(session, "plot_tabs", selected = "Explore solutions")
    disable("solutionsImport")
    enable("downloadData")
    
   
    
    return(validated)
  })
  
  #Validation step 1------------------------------------------------------------
  observeEvent(input$validate, {
    enable("create")
    enable("target")
    step1_data()
  }) 
  
  #Validation step 2------------------------------------------------------------
  observeEvent(input$create, {
    enable("run")
    step2_data()
  })
  
  #Validation step 3------------------------------------------------------------
  observeEvent(input$run, {
    
    solutions <- step3_data()
    print(solutions)
    
    files <- csv_files()
    shape <- shape_with_csv()
    
    #actions <- prioriactions::getActions(solutions, format = "large")
    #solution_names <- unique(actions$solution_name)
    solution_names <- "sol"
    
    #actions$action <- paste0("action_", actions$action)
    # actions <- tidyr::pivot_wider(actions[, -1], 
    #                    names_from = action, 
    #                    values_from = solution, 
    #                    values_fill = 0,
    #                    names_sort = TRUE)
    
    enable("solution_name")
    enable("solution_plot")
    
    updateSelectInput(session = session, "solution_name",
                      choices = solution_names,
                      selected = solution_names[1]
    )
    
    #updateRadioGroupButtons(session = session, inputId = "radio", selected = 2)
    
    names_solutions_actions <<- c("selected")

    
    updateSelectInput(session = session, "solution_plot",
                      choices = list(`Solutions` = names_solutions_actions),
                      selected = "selected"
    )
  })
  
  
  #Plotting maps explorer-------------------------------------------------------
  save_final_shape <- reactive({
    
    final_shape <- c()
    shape <- map_solutions()
    
      if(csv_input_charged){
        index <- which(names(shape[[1]]) == input$solution_plot)
        
        map_with_na <- shape[[1]][[index]]
        shape.subset <- shape[[1]][!is.na(map_with_na), ]
        final_shape <- shape.subset
      }
      else{
        index <- which(names(shape) == input$solution_plot)
        
        map_with_na <- shape[[index]]
        shape.subset <- shape[!is.na(map_with_na), ]
        final_shape <- shape.subset
      }
      final_shape
      
  })
  
  plot_maps_solutions <- reactive({
    #final_shape <- save_final_shape()

      shape <- map_solutions()

       if(csv_input_charged){
          if(input$solution_plot == "total_acts"){
            index <- which(names(shape[[1]]) == input$solution_plot)
            
            map_with_na <- shape[[1]][[index]]

            if(isTRUE(marxan)){
              shape.subset <<- shape[[1]][map_with_na, ]
            }
            else{
              shape.subset <<- shape[[1]][!is.na(map_with_na), ]
              #shape.subset <<- shape[[1]][map_with_na, ]
            }
          }
          else{
            index <- which(names(shape[[1]]) == input$solution_plot)
            map_with_na <- shape.subset[[index]]
          }
       }
       else{
         if(input$solution_plot == "total_acts"){
           index <- which(names(shape) == input$solution_plot)
           
           map_with_na <- shape[[index]]
           shape.subset <<- shape[!is.na(map_with_na), ]
         }
         else{
           index <- which(names(shape) == input$solution_plot)
           map_with_na <- shape.subset[[index]]
         }
       }
      
      
      if(input$solution_plot %in% names_solutions_actions){
        if(input$solution_plot == "total_acts"){
          title_legend <- "Number of actions"
        }
        else{
          title_legend <- "Action"
        }
        
        factor_thr <- factor(shape.subset[[index]][,1], exclude = NULL)
        if(length(levels(factor_thr)) > 5){
            pal <- colorNumeric(palette="inferno", 
                                domain=ifelse(shape.subset[[index]][,1] == 0, NA, shape.subset[[index]][,1]), 
                                na.color="#e3e3e3")
        }
        else{
          pal <- colorFactor(palette="inferno", 
                             domain=ifelse(shape.subset[[index]][,1] == 0, NA, shape.subset[[index]][,1]), 
                             na.color="#e3e3e3")
        }
      }
      
      type_shape <- terra::geomtype(shape[[1]])

      if(type_shape == "lines"){
        
        leafletProxy("map", data = shape.subset) %>%
          clearControls() %>%
          setShapeStyle(layerId = ~id, 
                        color = ~pal(shape.subset[[index]][,1]),
                        bringToFront = TRUE,
                        sendToBack = TRUE,
                        options = list(group = "Base")) %>%
          addLegend(group = "Base",
                    pal = pal, 
                    title = title_legend,
                    values = shape.subset[[index]][,1], 
                    position = "topright")
      }
      else if(type_shape == "polygons"){
        leafletProxy("map", data = shape.subset) %>%
          clearControls() %>%
          setShapeStyle(layerId = ~id, 
                        fillColor = ~pal(shape.subset[[index]][,1]),
                        bringToFront = TRUE,
                        sendToBack = TRUE,
                        options = list(group = "Base")) %>%
          addLegend(group = "Base",
                    pal = pal, 
                    title = title_legend,
                    values = shape.subset[[index]][,1], 
                    position = "topright")
      }
      
      # leafletProxy("map", data = shape.subset) %>%
      #       clearControls() %>%
      #       setShapeStyle(layerId = ~id,
      #                     fillColor = ~pal(shape.subset[[index]][,1]),
      #                     bringToFront = TRUE,
      #                     sendToBack = TRUE,
      #                     options = list(group = "Base")) %>%
      #       addLegend(group = "Base",
      #                 pal = pal,
      #                 title = title_legend,
      #                 values = shape.subset[[index]][,1],
      #                 position = "topright")
        
        solution_created <<- TRUE
  })
  
  map_solutions <- reactive({
    
    if(is.null(input$solutionsImport)){
      solutions <- step3_data()
      
      actions <- solutions
      
      files_outputs <- list()
      actions <- append(files_outputs, list(actions))

      solution_names <<- c(solution_names, "sol")
    }
    else{
      files_outputs <- solution_import_data()
      actions <- files_outputs
      
    }
    
    if(isTRUE(marxan)){
      actions_filtered <- actions[[which(solution_names %in% input$solution_name)]]
      colnames(actions_filtered) <- c("pu", "total_acts")
      
      if(csv_input_charged){
        map <- shape_with_csv()
        files <- csv_files()
        
        #actions
        
        map[[1]] <- terra::merge(map[[1]], actions_filtered, by.x = "id", by.y = "pu")
        map[[1]] <- map[[1]][, !(names(map[[1]]) %in% c("pu"))]
        
      }
    }
    else{
      actions_filtered <- actions[[which(solution_names %in% input$solution_name)]]
      actions_filtered$action <- paste0("action_", actions_filtered$action)
      
      actions_filtered <- tidyr::pivot_wider(actions_filtered[, -1], 
                                             names_from = action, 
                                             values_from = solution, 
                                             values_fill = 0,
                                             names_sort = TRUE)
      
      if(NCOL(actions_filtered[,-1]) == 1){
        actions_filtered$total_acts <- actions_filtered[,-1]
      }
      else{
        actions_filtered$total_acts <- rowSums(actions_filtered[,-1], na.rm = TRUE)
      }
      
      if(csv_input_charged){
        map <- shape_with_csv()
        files <- csv_files()
        
        #actions
        
        map[[1]] <- terra::merge(map[[1]], actions_filtered, by.x = "id", by.y = "pu")
        map[[1]] <- map[[1]][, !(names(map[[1]]) %in% c("pu"))]
      }
      else{
        map <- shape()
        
        #actions
        map <- terra::merge(map, actions_filtered, by_sp = "id", by_df = "pu")
        
        #benefit
        benefit_filtered <- benefit[benefit$solution_name == input$solution_name, ]
        
        benefit_filtered_con <- benefit_filtered
        benefit_filtered_rec <- benefit_filtered
        
        benefit_filtered_con$benefit.conservation <- round(benefit_filtered_con$benefit.conservation, 3)
        benefit_filtered_rec$benefit.recovery <- round(benefit_filtered_rec$benefit.recovery, 3)
        
        benefit_filtered_con$feature <- paste0(benefit_filtered_con$name, "_con")
        benefit_filtered_rec$feature <- paste0(benefit_filtered_rec$name, "_rec")
        benefit_filtered_con <- tidyr::pivot_wider(benefit_filtered_con[, -1], 
                                                   names_from = feature, 
                                                   values_from = benefit.conservation, 
                                                   values_fill = 0,
                                                   names_sort = TRUE)
        
        benefit_filtered_rec <- tidyr::pivot_wider(benefit_filtered_rec[, -1], 
                                                   names_from = feature, 
                                                   values_from = benefit.recovery, 
                                                   values_fill = 0,
                                                   names_sort = TRUE)
        
        
        
        if(NCOL(benefit_filtered_con[,-1]) == 1){
          benefit_filtered_con$total_benefit_conservation <- benefit_filtered_con[,-1]
          benefit_filtered_rec$total_benefit_recovery <- benefit_filtered_rec[,-1]
        }
        else{
          benefit_filtered_con$total_benefit_conservation <- rowSums(benefit_filtered_con[,-1])
          benefit_filtered_rec$total_benefit_recovery <- rowSums(benefit_filtered_rec[,-1])
        }
        
        benefit_filtered_rec$total_benefit <- benefit_filtered_con$total_benefit_conservation +
          benefit_filtered_rec$total_benefit_recovery
        
        map <- terra::merge(map, benefit_filtered_con, by_sp = "id", by_df = "pu")
        map <- terra::merge(map, benefit_filtered_rec, by_sp = "id", by_df = "pu")
      }
    }

    
    map
  })
  
  observeEvent(input$solution_plot, {
    
    plot_maps_solutions()

    if(grepl( "action", input$solution_plot, fixed = TRUE)){
      #shinyjs::show("pie_features_sol")
      #shinyjs::show("pie_threats_sol")
    }
    else{
      #shinyjs::hide("pie_features_sol")
      #shinyjs::hide("pie_threats_sol")
    }
    #shinyjs::hide("pie_features_sol")
    #shinyjs::hide("pie_threats_sol")
  }) 
  
  observeEvent(input$solution_name, {
    
      shape <- map_solutions()
      
      if(csv_input_charged){
        index <- which(names(shape[[1]]) == input$solution_plot)
        
        map_with_na <- shape[[1]][[index]]

        if(isTRUE(marxan)){
          
          shape.subset <- shape[[1]]
        }
        else{
          shape.subset <- shape[[1]][!is.na(map_with_na), ]
        }
        #shape.subset <- shape[[1]][map_with_na, ]
        
        popup_map <- sprintf(
          "<strong>id %s</strong><br/>
           Monitoring cost: %g <br/>
           Features: %g <br/>
           Threats: %g <br/>",
          shape.subset$id, 
          shape.subset$monitoring_cost,
          shape.subset$total_distribution_features,
          shape.subset$total_distribution_threats) %>% 
          lapply(htmltools::HTML)
      }
      else{
        index <- which(names(shape) == input$solution_plot)
        
        map_with_na <- shape[[index]]
        shape.subset <- shape[!is.na(map_with_na), ]
        
        popup_map <- sprintf(
          "<strong>id %s</strong><br/>",
          shape$id) %>% 
          lapply(htmltools::HTML)
      }
      
      if(input$solution_plot %in% names_solutions_actions){
        if(input$solution_plot == "total_acts"){
          
          filtered_shape <- ifelse(shape.subset[[index]][,1] == 0, NA, shape.subset[[index]][,1])

          pal <- colorFactor(palette="inferno", 
                             domain=filtered_shape, 
                             na.color="#e3e3e3")
          title_legend <- "Number of actions"
        }
        else{

          factor_thr <- factor(shape.subset[[index]])
          
          if(length(levels(factor_thr)) > 5){
            
            pal <- colorNumeric(palette="inferno", 
                                domain=ifelse(shape.subset[[index]][,1] == 0, NA, shape.subset[[index]][,1]), 
                                na.color="#e3e3e3")
          }
          else{
            pal <- colorFactor(palette="inferno", 
                               domain=ifelse(shape.subset[[index]][,1] == 0, NA, shape.subset[[index]][,1]), 
                               na.color="#e3e3e3")
          }
          title_legend <- "Action"
        }
      }
      
      type_shape <- terra::geomtype(shape[[1]])
      
      if(type_shape == "lines"){
        
        leafletProxy("map", data = shape.subset) %>%
          clearControls() %>%
          setShapeStyle(layerId = ~id, 
                        color = ~pal(shape.subset[[index]][,1]),
                        bringToFront = TRUE,
                        sendToBack = TRUE,
                        options = list(group = "Base")) %>%
          addLegend(group = "Base",
                    pal = pal, 
                    title = title_legend,
                    values = shape.subset[[index]][,1], 
                    position = "topright")
      }
      else if(type_shape == "polygons"){
        leafletProxy("map", data = shape.subset) %>%
          clearControls() %>%
          setShapeStyle(layerId = ~id, 
                        fillColor = ~pal(shape.subset[[index]][,1]),
                        bringToFront = TRUE,
                        sendToBack = TRUE,
                        options = list(group = "Base")) %>%
          addLegend(group = "Base",
                    pal = pal, 
                    title = title_legend,
                    values = shape.subset[[index]][,1], 
                    position = "topright")
      }
      
      
      # leafletProxy("map", data = shape.subset) %>%
      #   clearControls() %>%
      #   setShapeStyle(layerId = ~id,
      #                 fillColor = ~pal(shape.subset[[index]][,1]),
      #                 bringToFront = TRUE,
      #                 sendToBack = TRUE,
      #                 options = list(group = "Base")) %>%
      #   addLegend(group = "Base",
      #             pal = pal,
      #             title = title_legend,
      #             values = shape.subset[[index]][,1],
      #             position = "topright")
      # 
      
      
      solution_created <<- TRUE
      final_shape <- save_final_shape()
      first_plot <- 1

  }) 
  
  # Highchart features ---------------------------------------------------------
  # output$pie_features <- renderHighchart({
  #   
  #   files <- csv_files()
  #   map <- shape_with_csv()
  #   
  #   if(!is.null(files)){
  #     if(!"name" %in% colnames(files[[2]])){
  #       files[[2]]$name <- paste0("id_", files[[2]]$id)
  #     }
  #     
  #     df_features <- right_join(files[[3]], files[[2]], by = c("feature" = "id"))
  #     df_features <- df_features[df_features$pu == 1, ]
  #     
  #     if(nrow(df_features) == 0){
  #       df_features[1, ] <- 0
  #       df_features[1, ]$pu <- 1
  #       df_features[1, ]$name <- "none"
  #       df_features[1, ]$amount <- 0
  #     }
  #     
  #     hchart(df_features,"pie", hcaes(x = name, y = amount), size = 200, 
  #            name = "Amount") %>%
  #     hc_title(text = paste0("Features in pu #", 1),
  #              align = "center") %>%
  #     hc_add_theme(hc_theme_bloom()) %>%
  #     #hc_colors(viridis(max(map[[1]]$total_distribution_features))) %>%
  #     hc_tooltip(pointFormat = "<b>Value:</b> {point.y} <br>
  #                <b>Percentage</b> {point.percentage:,.2f}%")
  #   }
  # })
  
  # Highchart threats ----------------------------------------------------------
  # output$pie_threats <- renderHighchart({
  #   
  #   files <- csv_files()
  #   map <- shape_with_csv()
  #   
  #   if(!is.null(files) && !isTRUE(marxan)){
  #     if(!"name" %in% colnames(files[[4]])){
  #       files[[4]]$name <- paste0("id_", files[[4]]$id)
  #     }
  #     
  #     df_threats <- right_join(files[[5]], files[[4]], by = c("threat" = "id"))
  #     df_threats <- df_threats[df_threats$pu == 1, ]
  #     
  #     if(nrow(df_threats) == 0){
  #       df_threats[1, ] <- 0
  #       df_threats[1, ]$pu <- 1
  #       df_threats[1, ]$name <- "none"
  #       df_threats[1, ]$amount <- 0
  #     }
  # 
  #     hchart(df_threats,"pie", hcaes(x = name, y = amount), size = 200, 
  #            name = "Amount") %>%
  #     hc_title(text = paste0("Threats in pu #", 1),
  #              align = "center") %>%
  #     hc_colors(inferno(max(map[[1]]$total_distribution_threats, na.rm = TRUE))) %>%
  #     hc_add_theme(hc_theme_bloom()) %>%
  #     hc_tooltip(pointFormat = "<b>Value:</b> {point.y} <br>
  #                <b>Percentage</b> {point.percentage:,.2f}%")
  #   }
  # })

  # Updating pie plot when click shapes-----------------------------------------
  observeEvent(input$map_shape_click, { 
      p <- input$map_shape_click
      files <- csv_files()
      
      if (is.null(p$id))
        return()
      else{
        if(!is.null(files)){
          
          #features
          if(!"name" %in% colnames(files[[2]])){
            files[[2]]$name <- paste0("id_", files[[2]]$id)
          }
          df_features <- right_join(files[[3]], files[[2]], by = c("feature" = "id"))
          df_features_filtered <- df_features[df_features$pu == p$id, ]
          
          if(nrow(df_features_filtered) == 0){
            df_features_filtered[1, ] <- 0
            df_features_filtered[1, ]$pu <- p$id
            df_features_filtered[1, ]$name <- "none"
            df_features_filtered[1, ]$amount <- 0
          }
          
          # highchartProxy("pie_features") %>%
          # hcpxy_set_data(
          #   type = "pie",
          #   data = df_features_filtered,
          #   mapping = hcaes(x = name, y = amount),
          #   redraw = TRUE
          # ) %>%
          #   hcpxy_update(
          #     title = list(text = paste0("<b>Features in pu #", p$id, "</b>"), align = "center")
          #   )
          
          if(!isTRUE(marxan)){
            #threats
            if(!"name" %in% colnames(files[[4]])){
              files[[4]]$name <- paste0("id_", files[[4]]$id)
            }
            df_threats <- right_join(files[[5]], files[[4]], by = c("threat" = "id"))
            df_threats_filtered <- df_threats[df_threats$pu == p$id, ]
            
            if(nrow(df_threats_filtered) == 0){
              df_threats_filtered[1, ] <- 0
              df_threats_filtered[1, ]$pu <- p$id
              df_threats_filtered[1, ]$name <- "none"
              df_threats_filtered[1, ]$amount <- 0
            }
            
            # highchartProxy("pie_threats") %>%
            #   hcpxy_set_data(
            #     type = "pie",
            #     data = df_threats_filtered,
            #     mapping = hcaes(x = name, y = amount),
            #     redraw = TRUE
            #   ) %>%
            #   hcpxy_update(
            #     title = list(text = paste0("<b>Threats in pu #", p$id, "</b>"), align = "center")
            #   )
          }
        }
        else{
          return()
        }
      }
  })
  
  # Highchart Solutions-----------------------------------------------------------
  # Highchart features -----------------------------------------------------------
  # output$pie_features_sol <- renderHighchart({
  #   
  #   files <- csv_files()
  #   map <- shape_with_csv()
  #   
  #   if(!is.null(files)){
  #     #if(!"name" %in% colnames(files[[2]])){
  #     #  files[[2]]$name <- paste0("id_", files[[2]]$id)
  #     #}
  #     
  #     #df_features <- right_join(files[[3]], files[[2]], by = c("feature" = "id"))
  #     #df_features <- df_features[df_features$pu == 1, ]
  #     df_features <- data.frame (name  ="none",
  #                                         amount = 0
  #     )
  #     
  #     hchart(df_features,"pie", hcaes(x = name, y = amount), size = 200, 
  #            name = "Amount") %>%
  #       #hc_title(text = paste0("Features beneficiated with this action in pu #", 1),
  #       #         align = "center") %>%
  #       hc_add_theme(hc_theme_bloom()) %>%
  #       #hc_colors(viridis(max(map[[1]]$total_distribution_features))) %>%
  #       hc_tooltip(pointFormat = "<b>Value:</b> {point.y} <br>
  #                <b>Percentage</b> {point.percentage:,.2f}%")
  #   }
  # })
  
  # Highchart threats ----------------------------------------------------------
  # output$pie_threats_sol <- renderHighchart({
  #   
  #   files <- csv_files()
  #   map <- shape_with_csv()
  #   
  #   if(!is.null(files)){
  #     #if(!"name" %in% colnames(files[[4]])){
  #     #  files[[4]]$name <- paste0("id_", files[[4]]$id)
  #     #}
  #     
  #     #df_threats <- right_join(files[[5]], files[[4]], by = c("threat" = "id"))
  #     #df_threats <- df_threats[df_threats$pu == 1, ]
  #     df_threats <- data.frame (name  ="none",
  #                                amount = 0
  #     )
  #     
  #     hchart(df_threats,"pie", hcaes(x = name, y = amount), size = 200, 
  #            name = "Amount") %>%
  #       #hc_title(text = paste0("Threats in pu #", 1),
  #       #         align = "center") %>%
  #       #hc_colors(inferno(max(map[[1]]$total_distribution_threats))) %>%
  #       hc_add_theme(hc_theme_bloom()) %>%
  #       hc_tooltip(pointFormat = "<b>Value:</b> {point.y} <br>
  #                <b>Percentage</b> {point.percentage:,.2f}%")
  #   }
  # })
  
  # Updating pie plot when click shapes-----------------------------------------
  observeEvent(input$map_shape_click, { 
    p <- input$map_shape_click
    files <- csv_files()
    
    if (is.null(p$id) || is.null(files))
      return()
    else{
      if(grepl( "action", input$solution_plot, fixed = TRUE)){
        shape <- map_solutions()
        shape <- shape[[1]]
        #print(shape)
        index <- which(names(shape) == input$solution_plot)
        map <- shape[[index]]
        index_pu <- which(shape$id == p$id)
        
        if(!is.na(map[index_pu,1])){
          
          #shinyjs::show("pie_features_sol")
          #shinyjs::show("pie_threats_sol")
          
          #features
          if(!"name" %in% colnames(files[[2]])){
            files[[2]]$name <- paste0("id_", files[[2]]$id)
          }
          df_features <- right_join(files[[3]], files[[2]], by = c("feature" = "id"))
          df_features_filtered <- df_features[df_features$pu == p$id, ]
          
          threat_index <- readr::parse_number(input$solution_plot)
          features_sensitivities <- files[[6]][files[[6]]$threat == threat_index,]
          df_features_filtered <- subset(df_features_filtered, feature %in% features_sensitivities$feature)
          
          # highchartProxy("pie_features_sol") %>%
          #   hcpxy_set_data(
          #     type = "pie",
          #     data = df_features_filtered,
          #     mapping = hcaes(x = name, y = amount),
          #     redraw = TRUE
          #   ) %>%
          #   hcpxy_update(
          #     title = list(text = paste0("<b>Features beneficiated with this action in pu #", p$id, "</b>"), align = "center")
          #   )
          
          #threats
          if(!"name" %in% colnames(files[[4]])){
            files[[4]]$name <- paste0("id_", files[[4]]$id)
          }
          df_threats <- right_join(files[[5]], files[[4]], by = c("threat" = "id"))
          df_threats_filtered <- df_threats[df_threats$pu == p$id, ]
          df_threats_filtered <- df_threats_filtered[df_threats_filtered$threat == threat_index, ]

          # highchartProxy("pie_threats_sol") %>%
          #   hcpxy_set_data(
          #     type = "pie",
          #     data = df_threats_filtered,
          #     mapping = hcaes(x = name, y = amount),
          #     redraw = TRUE
          #   ) %>%
          #   hcpxy_update(
          #     title = list(text = paste0("<b>Threats abated by this action in pu #", p$id, "</b>"), align = "center")
          #   )
        }
        else{
          #shinyjs::hide("pie_features_sol")
          #shinyjs::hide("pie_threats_sol")
        }
      }
    }
  })
  
  #Solutions import ------------------------------------------------------------
  observeEvent(input$solutionsImport, { 
    first_plot <- 0
    solution_import_data()
    
    disable("solutionsImport")
  })
  
  solution_import_data <- reactive({
    #req(input$solutionsImport)
    
    if(!is.null(req(input$solutionsImport))){
      req(input$solutionsImport)
      sol_data <- input$solutionsImport
      tempdirname <- dirname(sol_data$datapath[1])
      
      files_outputs <- list()
      action_names <- c()
      
      
      # Rename files
      for (i in 1:nrow(sol_data)) {

        ext <- tools::file_ext(sol_data$datapath[i])
        shiny::validate(need(ext %in% c("txt","dat", "csv"), "Please upload an txt, dat, or csv file"))
        
        action_data <- data.table::fread(file = sol_data$datapath[i],
                                       data.table = FALSE)
        
        if(isTRUE(marxan)){
          if(all(c("PUID", "SOLUTION") %in% colnames(action_data))){
            action_data$SOLUTION[action_data$SOLUTION == 0] <- NA
            name <- tools::file_path_sans_ext(sol_data$name[i])
            solution_names <<- c(solution_names, name)
            action_names <-  c(action_names, "Solution")
            #files_outputs <- list(files_outputs, action_data)
            files_outputs <- append(files_outputs, list(action_data))
          
            names_solutions_actions <<- c("total_acts")
            }
        }
        else{
          if(all(c("pu", "action") %in% colnames(action_data))){
            action_data$solution[action_data$solution == 0] <- NA
            name <- tools::file_path_sans_ext(sol_data$name[i])
            solution_names <<- c(solution_names, name)
            action_names <-  c(action_names, unique(paste0("action_", action_data$action)))
            #files_outputs <- list(files_outputs, action_data)
            files_outputs <- append(files_outputs, list(action_data))
            
            names_solutions_actions <<- c(unique(action_names), "total_acts")
          }
          else{
            #error
          }
        }
      }
      
      enable("solution_name")
      enable("solution_plot")
      
      updateSelectInput(session = session, "solution_name",
                        choices = solution_names,
                        selected = solution_names[1]
      )

      # if(csv_input_charged){
      #   files <- csv_files()
      #   features_con_and_rec <- stringr::str_sort(c(paste0(unique(files[[2]]$name),"_con"), paste0(unique(files[[2]]$name),"_rec")))
      # }
      # else{
      #   features_con_and_rec <- stringr::str_sort(c(paste0(unique(benefit_data$feature),"_con"), paste0(unique(benefit_data$feature),"_rec")))
      # }
      
      
      
      #names_solutions_benefits <<- c(features_con_and_rec,
      #                               "total_benefit_recovery",
      #                               "total_benefit_conservation",
      #                               "total_benefit")
      
      updateSelectInput(session = session, "solution_plot",
                        choices = list(`Solutions` = names_solutions_actions),
                        selected = "total_acts"
      )
      files_outputs

    }
    else{
      return(NULL)
    }
  })
}