# Defini la partie server
server <- function(input, output, session) {

  # Display Sk8 info
  output$SK8Image <- renderImage({
    list(src= normalizePath(file.path("www/images","SK8.png")),
         contentType = 'image/png',
         width = 200,
         alt = "Logo SK8")
  }, deleteFile = FALSE)
  

  ### DRYvER case studies
  ## Display DRN information
  selected_drn_info <- reactiveVal("")
  observeEvent(input$drn_info_Albarine, {
    selected_drn_info("Albarine")
  })
  observeEvent(input$drn_info_Bukkosdi, {
    selected_drn_info("Bukkosdi")
  })
  observeEvent(input$drn_info_Butiznica, {
    selected_drn_info("Butiznica")
  })
  observeEvent(input$drn_info_Genal, {
    selected_drn_info("Genal")
  })
  observeEvent(input$drn_info_Lepsamanjoki, {
    selected_drn_info("Lepsamanjoki")
  })
  observeEvent(input$drn_info_Velicka, {
    selected_drn_info("Velicka")
  })
  
  output[["presentation_drn"]] <- renderUI({
    if(selected_drn_info() != "")
    {
      if(selected_drn_info() == "Albarine")
      {
        box(title = "Albarine river (France)", status = "primary", solidHeader = TRUE,
            leafletOutput("map_drn"),
            #HTML('<center><img src="albarine_map.png" width="70%"></center>'),
            HTML("<ul><li>Drainage area approximately 354 km2</li>
            <li>Jura Mountains, Pre-alpine climate, Continental ecoregion</li>
            <li>More than 80 km (~25 % of the catchment) is intermittent network. The river contains some protected invertebrate and fish (salmonids) species. Anthropogenic alterations include some weirs and point-source sewage. Irrigation, human consomption, fish, and recreation are the most relevant ecosystem services in the area.</li></ul>"),
            HTML('<center><img src="albarine_photo.png" width="70%"></center>')
        )
      }else if(selected_drn_info() == "Bukkosdi")
      {
        box(title = "Bükkösdi river (Hungary)", status = "primary", solidHeader = TRUE,
            leafletOutput("map_drn"),
            #HTML('<center><img src="bukkosdi_map.png" width="70%"></center>'),
            HTML("<ul><li>Drainage area approximately 185 km2</li>
            <li>Mecsek Mountains, Continental climate, Pannonian ecoregion</li>
            <li>Approximately 40-50 % of the stream network is intermittent, mostly the smaller tributaries are prone to drying. Drying occurs mostly in late summer and early autumn. Pollution sources are mostly the municipalities and agriculture. The potential ecosystem services can be agriculture, livestock breeding, irrigation, forestry, recreation, and fishing.</li></ul>"),
            HTML('<center><img src="bukkosdi_photo.png" width="70%"></center>')
        )
      }else if(selected_drn_info() == "Butiznica")
      {
        box(title = "Butižnica river (Croatia)", status = "primary", solidHeader = TRUE,
            leafletOutput("map_drn"),
            #HTML('<center><img src="butiznica_map.png" width="70%"></center>'),
            HTML("<ul><li>Drainage area approximately 225 km2</li>
            <li>Dinaric Mountains, Mediterranean climate, Balkanic ecoregion</li>
            <li>Seasonal drying occurs mostly in summer (from June to November). Very little is known from the catchment. Irrigation, electricity, human consumption (water, fish) and recreation are the most prominent ecosystem services in the catchment area.</li></ul>"),
            HTML('<center><img src="butiznica_photo.png" width="70%"></center>')
        )
      }else if(selected_drn_info() == "Genal")
      {
        box(title = "Genal river (Spain)", status = "primary", solidHeader = TRUE,
            leafletOutput("map_drn"),
            #HTML('<center><img src="genal_map.png" width="70%"></center>'),
            HTML("<ul><li>Drainage area approximately 343 km2</li>
            <li>Penibaetic Mountains, Mediterranean climate, Mediterranean ecoregion</li>
            <li>Lies in area of intermittent streams but no existing records of flow intermittence measures/observations are available yet. Anthropogenic alterations are riverbank modifications for bathing in the headwaters, and water abstractions for agriculture, diffuse pollution from agriculture, and invasive species in the lowlands. Relevant ecosystem services are bathing, hiking, surface water for drinking, fishing in former times (mainly eels; populations have dropped), cultivated crops, aesthetic values, and rural tourism.</li></ul>"),
            HTML('<center><img src="genal_photo.png" width="70%"></center>')
        )
      }else if(selected_drn_info() == "Lepsamanjoki")
      {
        box(title = "Lepsämänjoki river (Finland)", status = "primary", solidHeader = TRUE,
            leafletOutput("map_drn"),
            #HTML('<center><img src="lepsamanjoki_map.png" width="70%"></center>'),
            HTML("<ul><li>Drainage area approximately 208 km2</li>
            <li>Cold temperate climate, Boreal ecoregion</li>
            <li>Some headwater streams dry up during late summer, but records of drying area are not yet available. Anthropogenic alterations include weirs, hydromorphological alterations and diffuse pollution mainly from agriculture. Recreation, fishing, cultural landscape and flood protection are examples of ecosystem services in the area.</li></ul>"),
            HTML('<center><img src="lepsamanjoki_photo.png" width="70%"></center>')
        )
      }else if(selected_drn_info() == "Velicka")
      {
        box(title = "Velička river (Czech Republic)", status = "primary", solidHeader = TRUE,
            leafletOutput("map_drn"),
            #HTML('<center><img src="velicka_map.png" width="70%"></center>'),
            HTML("<ul><li>Drainage area approximately 172 km2</li>
            <li>White Carpathian Mountains, Continental humid climate, Continental biogeographical region</li>
            <li>The small tributaries are more prominent to dry up, while the main stem dries once in ~10 years. The lowland parts of the river are regulated, and water abstraction and pollution (diffuse pollution sources from agriculture and point sources from water treatment plants) is also present. Irrigation and fishing are the most relevant ecosystem services in the area.</li></ul>"),
            HTML('<center><img src="velicka_photo.png" width="70%"></center>')
        )
      }
    }
  })
  
  ## Plot interactive map of the river network
  output[["map_drn"]] <- renderLeaflet({
    p_map <- figure_map(selected_drn_info())
    p_map
  })
  
  # DATA VISUALIZATION----------------------------------------------------------

  # Plot interactive map of the river network (data)
  output[["map_data"]] <- renderLeaflet({

    enable("drn_map")
    
    p <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
      addProviderTiles(providers$Esri.WorldShadedRelief, group = "Elevation") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      setView(lng = -40, lat = 18, zoom = 3) %>%
      onRender(
        "function(el, x) {
          L.control.zoom({
            position:'bottomright'
          }).addTo(this);
        }") %>%
      addLayersControl(
        baseGroups = c("CartoDB", "Elevation", "Satellite","Without background"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>% leafem::addMouseCoordinates()
    
    updateSelectInput(session, "drn_map", choices = drns_long[-1], selected = "Albarine (France)")
    updatePickerInput(session, "Variable", choices = list('Aquatic Macroinvertebrates biodiversity' = variables_long[2:5],
                                                          'Ecological Functions' = variables_long[6:7],
                                                          'Ecosystem Services' = variables_long[8:13]),
                      selected = " ")
    
    return(p)
  })

  ## Select bottom River network in Data exploration tab
  observeEvent(input$drn_map, {
    

    if(input$drn_map != " "){
      
      # data tab
      # Remove previous Leaflet map
      leafletProxy("map_data") %>% clearMarkers() %>% 
        clearShapes() %>% 
        clearPopups() %>% 
        clearControls()  # Para remover leyendas u otros controles añadidos
      
      clicked(NULL) 
      
      # Plot Leaflet map 
      print_shape(drns_short[which(drns_long == input$drn_map)],
                  "map_data")

      # Enable SelectInput "Variable"
      shinyjs::enable("Variable")
      
      # Enable SelectInput "Scale"
      shinyjs::enable("Scale")
      
      updatePickerInput(session, "Variable", choices = list('Aquatic Macroinvertebrates biodiversity' = variables_long[2:5],
                                                            'Ecological Functions' = variables_long[6:7],
                                                            'Ecosystem Services' = variables_long[8:13]),
                        selected = " ")
    
    }
  })
  
  ## Select bottom River network in Optimization tab
  observeEvent(input$drn_opt, {
    
    if(input$drn_opt != " "){
      
      # data tab
      # Remove previous Leaflet map
      leafletProxy("map_opt") %>% clearMarkers() %>% clearShapes()
      
      # Plot Leaflet map 
      print_shape(drns_short[which(drns_long == input$drn_opt)],
                  "map_opt")
      
      # Enable SelectInput "Variable"
      shinyjs::enable("optimize")
      
      shinyjs::enable("downloadPDF")
  
      cons_optimize <<- 0
    }
  })
  
  ## Update range
  observeEvent(input$range, {
    
    if(input$drn_map != " " && !is.null(input$Variable) && input$Variable != " "){
      #update_range(drns_short[which(drns_long == input$drn)], input$range)
      
      id_range = which(input$range == campaign_list)
      drns = drns_short[which(drns_long == input$drn_map)]
      period = scale_list_short[which(scale_list == input$Scale)]

      update_map_data(drns, 
                      variables_short[which(variables_long == input$Variable)],
                      id_range,
                      period,
                      input$Compare)
      
      click_data <- clicked()  # Obtener el clic actual
      
      # Caso 1: Si se hizo clic en un tramo de río (polígono)
      if(!is.null(click_data[['id']])){  # Verificar si el clic tiene un 'id' (tramo de río)

        update_map_data_click(drns,
                              variables_short[which(variables_long == input$Variable)],
                              id_range,
                              period,
                              input$Compare,
                              click_data,
                              red = TRUE)
      }
    }
  })
  
  ## Update Variable SelectInput
  observeEvent(input$Variable, {

    if(input$Variable != " " & input$drn_map != " "){
      
      # shinyjs::show("plotly1")
      drns = drns_short[which(drns_long == input$drn_map)]
      period = scale_list_short[which(scale_list == input$Scale)]
      
      # Enable range bar
      enable("range")
      
      # Enable compare bottom
      #if(period != scale_list_short[1]){
      enable("Compare")
      #}
      
      id_range = which(input$range == campaign_list)
      
      update_map_data(drns, 
                   variables_short[which(variables_long == input$Variable)],
                   id_range,
                   period,
                   input$Compare)

      # Show SelectInput "Scale"
      shinyjs::show("Scale")
      
      click_data <- clicked()  # Obtener el clic actual

      # Caso 1: Si se hizo clic en un tramo de río (polígono)
      if(!is.null(click_data[['id']])){  # Verificar si el clic tiene un 'id' (tramo de río)

        update_map_data_click(drns,
                              variables_short[which(variables_long == input$Variable)],
                              id_range,
                              period,
                              input$Compare,
                              click_data,
                              red = TRUE)
      }
    }
    
  })
  
  ## Update Scale SelectInput
  observeEvent(input$Scale, {
    
    period = scale_list_short[which(scale_list == input$Scale)]
    
    # Enable compare bottom
    #if(period != scale_list_short[1]){
    shinyjs::enable("Compare")
    #}
    #else{
    #  updateCheckboxInput(session, "Compare", value = FALSE)
    #  shinyjs::disable("Compare")
    #}
    
    if(input$Variable != " " && !is.null(input$Variable) && input$drn_map != " "){
      
      drns = drns_short[which(drns_long == input$drn_map)]
      period = scale_list_short[which(scale_list == input$Scale)]
      
      id_range = which(input$range == campaign_list)
      
      update_map_data(drns, 
                      variables_short[which(variables_long == input$Variable)],
                      id_range,
                      period,
                      input$Compare)
      
      click_data <- clicked()  # Obtener el clic actual
      
      # Caso 1: Si se hizo clic en un tramo de río (polígono)
      if(!is.null(click_data[['id']])){  # Verificar si el clic tiene un 'id' (tramo de río)
        
        update_map_data_click(drns,
                              variables_short[which(variables_long == input$Variable)],
                              id_range,
                              period,
                              input$Compare,
                              click_data,
                              red = TRUE)
      }
    }
    
  })
  
  
  ## Update Compare SelectInput
  observeEvent(input$Compare, {
    #if(input$Compare){
    if(input$Variable != " " && !is.null(input$Variable) && input$drn_map != " "){
      
      drns = drns_short[which(drns_long == input$drn_map)]
      #period = scale_list_short[which(scale_list == input$Scale)]
      period = "future"
      id_range = which(input$range == campaign_list)
      
      update_map_data(drns, 
                      variables_short[which(variables_long == input$Variable)],
                      id_range,
                      period,
                      input$Compare)
      
      click_data <- clicked()  # Obtener el clic actual
      
      # Caso 1: Si se hizo clic en un tramo de río (polígono)
      if(!is.null(click_data[['id']])){  # Verificar si el clic tiene un 'id' (tramo de río)
      
        update_map_data_click(drns,
                              variables_short[which(variables_long == input$Variable)],
                              id_range,
                              period,
                              input$Compare,
                              click_data,
                              red = TRUE)
        }
    }
  })
  
  
  ## Crear una variable reactiva para guardar la selección
  clicked <- reactiveVal()
  
  ## Manejar clic en un polígono (tramo de río)
  observeEvent(input$map_data_shape_click, {
    freezeReactiveValue(input, 'map_data_click')  # Evitar que el clic en el fondo lo sobreescriba
    clicked(input$map_data_shape_click)           # Guardar clic en el polígono
  })
  
  ## Manejar clic en el fondo (fuera de los tramos de río)
  observeEvent(input$map_data_click, {
    clicked(input$map_data_click)                 # Guardar clic en el fondo
  })
  

  ## Network time series (observed period)
  output[["plotly2"]] <- renderPlotly({
    
    # Verificar si los inputs relevantes están presentes
    if(input$Variable != " " && !is.null(input$Variable) && input$drn_map != " " && input$Scale != " "){
      
      click_data <- clicked()  # Obtener el clic actual

      # Caso 1: Si se hizo clic en un tramo de río (polígono)
      if(!is.null(click_data[['id']])){  # Verificar si el clic tiene un 'id' (tramo de río)
        
        # Obtener el rango de la campaña
        id_range = which(input$range == campaign_list)
        
        # Verificar si el clic está dentro del mapa y es válido
        is_in_map <- is_id(drns_short[which(drns_long == input$drn_map)], 
                           variables_short[which(variables_long == input$Variable)],
                           id_range,
                           click_data$id,
                           scale_list_short[which(scale_list == input$Scale)])
        
        if(is_in_map){
          # Generar la gráfica
          p1 <- figure_time_serie(variables_short[which(variables_long == input$Variable)], 
                                  variables_long[which(variables_long == input$Variable)], 
                                  click_data$id, 
                                  drns_short[which(drns_long == input$drn_map)],
                                  id_range, 
                                  scale_list_short[which(scale_list == input$Scale)],
                                  input$Compare)
          
          # Renderizar la gráfica dependiendo si está en modo de comparación
          if(input$Compare){
            return(ggplotly(p1, width = 500, height = 300))
          } else {
            return(ggplotly(p1, width = 450, height = 300))
          }
        }
      } 
      
      # Caso 2: Si se hizo clic en el fondo (sin tramo seleccionado)
      else {
        
        drns = drns_short[which(drns_long == input$drn_map)]
        period = scale_list_short[which(scale_list == input$Scale)]
        id_range = which(input$range == campaign_list)
        
        update_map_data_click(drns,
                              variables_short[which(variables_long == input$Variable)],
                              id_range,
                              period,
                              input$Compare,
                              selected_line(),
                              red = FALSE)
        
        
        # Renderizar un gráfico en blanco cuando se hace clic en el fondo
        blank_plot <- ggplot() + 
          theme_minimal() +
          ggtitle("Please select a river reach")  # Un mensaje opcional cuando no se selecciona nada

        return(ggplotly(blank_plot, width = 450, height = 300))
      }
    }
  })
  
  # Variable reactiva para almacenar la ID de la capa seleccionada
  selected_line <- reactiveVal(NULL)
  
  # Escucha los eventos de clic en el mapa
  observeEvent(input$map_data_shape_click, {
    # Obtén la ID de la línea que fue seleccionada
    
    clicked_id <- input$map_data_shape_click$id
    
    click_data <- clicked() 
    
   
    
    if(!is.null(click_data[['id']])){
      
      if(input$Variable != " " && !is.null(input$Variable)){
      
        drns = drns_short[which(drns_long == input$drn_map)]
        period = scale_list_short[which(scale_list == input$Scale)]
        id_range = which(input$range == campaign_list)
        
        if (!is.null(selected_line())) {
          update_map_data_click(drns, 
                                variables_short[which(variables_long == input$Variable)],
                                id_range,
                                period,
                                input$Compare,
                                selected_line(),
                                red = FALSE)
        }
        
        update_map_data_click(drns,
                              variables_short[which(variables_long == input$Variable)],
                              id_range,
                              period,
                              input$Compare,
                              click_data,
                              red = TRUE)
      }
      
    }

    # Actualiza la ID de la línea seleccionada
    selected_line(clicked_id)

  })
  
  
  # OPTIMIZATION----------------------------------------------------------------

  # #Synchronize the maps using leafletProxy (map_data)
  # observe({
  # 
  #   leafletProxy("map_data", data = leafletProxy("map_opt", session)) %>%
  #     setView(lng = input$map_data_center$lng, lat = input$map_data_center$lat, zoom = input$map_data_zoom)
  # 
  #   updateCoords(input$tabs, input$map_data_center$lng, input$map_data_center$lat, input$map_data_zoom)
  # })
  # 
  # #Synchronize the maps using leafletProxy (map_opt)
  # observe({
  #   leafletProxy("map_opt", data = leafletProxy("map_data", session)) %>%
  #     setView(lng = input$map_opt_center$lng, lat = input$map_opt_center$lat, zoom = input$map_opt_zoom)
  # 
  #   updateCoords(input$tabs, input$map_opt_center$lng, input$map_opt_center$lat, input$map_opt_zoom)
  # })

  ## Crear una variable reactiva para guardar la selección
  clicked_opt <- reactiveVal()
  
  ## Manejar clic en un polígono (tramo de río)
  observeEvent(input$map_opt_shape_click, {
    freezeReactiveValue(input, 'map_opt_click')  # Evitar que el clic en el fondo lo sobreescriba
    clicked_opt(input$map_opt_shape_click)           # Guardar clic en el polígono
  })
  
  ## Manejar clic en el fondo (fuera de los tramos de río)
  observeEvent(input$map_opt_click, {
    clicked_opt(input$map_opt_click)                 # Guardar clic en el fondo
  })
  
  
  ## Plot interactive map of the river network (data)
  output[["map_opt"]] <- renderLeaflet({

    p <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
      addProviderTiles(providers$Esri.WorldShadedRelief, group = "Elevation") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      setView(lng = -40, lat = 18, zoom = 3) %>%
      onRender(
        "function(el, x) {
          L.control.zoom({
            position:'bottomright'
          }).addTo(this);
        }") %>%
      addLayersControl(
        baseGroups = c("CartoDB", "Elevation", "Satellite","Without background"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>% leafem::addMouseCoordinates() 
    
    
    
    if(sync_first){
      
      #updateSelectInput(session, "drn", choices = drns_long[-1], selected = "Albarine (France)")

      # leafletProxy("map_opt", data = leafletProxy("map_data", session)) %>%
      #   setView(lng = input$map_data_center$lng, lat = input$map_data_center$lat, zoom = input$map_data_zoom)
      # 
      # updateCoords(input$tabs, input$map_opt_center$lng, input$map_opt_center$lat, input$map_opt_zoom)
      # 
      sync_first <<- FALSE
    }

    updateSelectInput(session, "drn_opt", choices = drns_long[-1], selected = drns_long[1])
    
    return(p)

  })
  
  
  ## Select bottom target
  # observeEvent(input$features, {
  #   
  #   print(input$features)
  # })
  

  ## Select bottom River network
  observeEvent(input$optimize, {

    if(input$drn_opt != " "){
      
      state <- input$`features-stats_weights`
      weights <- state$weights
      
      if(!is.null(weights) && is.list(weights) && length(weights) != 0){
        optimizing(drns_short[which(drns_long == input$drn_opt)],
                   weights,
                   input$blm)
        
        cons_optimize <<- 1
        
      }
      resetLoadingButton("optimize")
    }
  })
  
  # Generate and download shapefile
  output$downloadData <- downloadHandler(
    filename = function() { paste0("shapefile.zip")},
    content = function(file) {
      if (length(Sys.glob("shapefile.*"))>0){
        file.remove(Sys.glob("shapefile.*"))
      }
      drn = drns_short[which(drns_long == input$drn_map)]
      var = variables_short[which(variables_long == input$Variable)]
      period = scale_list_short[which(scale_list == input$Scale)]
      id_range = which(input$range == campaign_list)
      
      shape = get_shape_download(drn, var, period, id_range)
      
      file_name = paste0(drn, "_", var, "_", period, "_camp", id_range, ".shp")
      
      terra::writeVector(shape, filename = "shapefile.shp")
      utils::zip(zipfile='shapefile.zip', files=Sys.glob("shapefile.*"))
      file.copy("shapefile.zip", file)
      if (length(Sys.glob("shapefile.*"))>0){
        file.remove(Sys.glob("shapefile.*"))
      }
    }
  )
  
  ## Network time series (observed period)
  # output[["plotly3"]] <- renderPlotly({
  #   
  #   click_shape <- input$map_opt_shape_click
  #   
  #   if(!is.null(click_shape) && input$drn_opt != " " && cons_optimize == 1){
  # 
  #     state <- input$`features-stats_weights`
  #     weights <- state$weights
  #     
  #     if(!is.null(weights)){
  #       p1 <- figure_targets_reached(drns_short[which(drns_long == input$drn_opt)],
  #                               weights, 
  #                               click_shape$id)
  #       
  #       if (!is.null(p1)) {
  #         ggplotly(p1, width = 550, height = 300) %>%
  #           layout(
  #             margin = list(
  #               b = 0  # bottom margin
  #             )
  #           )
  #       }
  #     }
  #   }
  # })
  
  
  
  ## Network time series (observed period)
  output[["plotly3"]] <- renderPlotly({
    
    click_data <- clicked_opt()  # Obtener el clic actual
    
    if(!is.null(click_data[['id']]) && input$drn_opt != " " && cons_optimize == 1){
      
      state <- input$`features-stats_weights`
      weights <- state$weights
      
      if(!is.null(weights)){
        p1 <- figure_targets_reached(drns_short[which(drns_long == input$drn_opt)],
                                     weights, 
                                     click_data$id)
        
        if (!is.null(p1)) {
          ggplotly(p1, width = 550, height = 300) %>%
            layout(
              margin = list(
                b = 0  # bottom margin
              )
            )
        }
      }
    }else {
      
      state <- input$`features-stats_weights`
      weights <- state$weights
      
      if(input$drn_opt != " " && cons_optimize == 1 && !is.null(selected_line_opt())){
        update_map_opt_click(drns_short[which(drns_long == input$drn_opt)],
                              last_sol,
                              weights,
                              selected_line_opt(),
                              red = FALSE)
        
      }
        
        # Renderizar un gráfico en blanco cuando se hace clic en el fondo
        blank_plot <- ggplot() + 
          theme_minimal() +
          ggtitle("Please select a river reach after optimization")  # Un mensaje opcional cuando no se selecciona nada
        
        return(ggplotly(blank_plot, width = 500, height = 300))
      }
  })
  
  # Variable reactiva para almacenar la ID de la capa seleccionada
  selected_line_opt <- reactiveVal(NULL)
  
  # Escucha los eventos de clic en el mapa
  observeEvent(input$map_opt_shape_click, {
    # Obtén la ID de la línea que fue seleccionada
    clicked_id <- input$map_opt_shape_click$id
    click_data <- clicked_opt() 

    if(!is.null(click_data[['id']])){

      if(input$drn_opt != " " && cons_optimize == 1){
        
        state <- input$`features-stats_weights`
        weights <- state$weights

        if (!is.null(selected_line_opt())) {
          update_map_opt_click(drns_short[which(drns_long == input$drn_opt)],
                                last_sol,
                                weights,
                                selected_line_opt(),
                                red = FALSE)
        }
        
        update_map_opt_click(drns_short[which(drns_long == input$drn_opt)],
                              last_sol,
                              weights,
                              click_data,
                              red = TRUE)
      }
      
    }
    
    # Actualiza la ID de la línea seleccionada
    selected_line_opt(clicked_id)
    
  })
  
  
  
  
  ## Network time series (observed period)
  output[["plotly4"]] <- renderPlotly({
    
    click_shape <- input$optimize
    
    if(!is.null(click_shape) && input$drn_opt != " " && cons_optimize == 1){
      
      p1 <- figure_inter_reached(drns_short[which(drns_long == input$drn_opt)])

    }
    else{
      # Renderizar un gráfico en blanco cuando se hace clic en el fondo
      p1 <- ggplotly(ggplot() + 
        theme_minimal() +
        ggtitle("Please optimize to know the % by selected river reaches type"), width = 550, height = 300)  # Un mensaje opcional cuando no se selecciona nada
    }
    
    return(p1)
  })

 
  output$downloadPDF <- downloadHandler(
    filename = function() {
      paste("plots-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      
      if(cons_optimize == 1){
        
        sol = last_sol[, c("solution_1")]
        print(last_model)
        
        repr <- last_model$feature_abundances_in_planning_units()
        targ <- last_model$feature_targets()

        repr = as.data.frame(repr)
        repr$cost <- round(repr$cost, 3)
        repr$variable <- rownames(repr)
        repr$target = round(targ$value, 3)
        
        repr <- repr %>%
          mutate(
            current = if_else(!grepl("_future$", variable), cost, NA_real_), 
            future = if_else(grepl("_future$", variable), cost, NA_real_)  
          ) %>%
          mutate(variable = sub("_future$", "", variable)) %>%
          group_by(variable) %>%
          summarise(
            current = max(current, na.rm = TRUE), 
            future = max(future, na.rm = TRUE),
            target = max(target, na.rm = TRUE)
          ) %>%
          mutate(variable_long = variables_long[match(variable, variables_short)]) %>%
          select(variable_long, current, future, target) %>%
          rename(Variable = variable_long)
        
        features_names <- last_model$feature_names()
        features_names <- variables_long[match(features_names, variables_short)] 
        features_names <- paste0(na.omit(features_names), collapse = ", ")
        
        n_line = -3.5
        
        pdf(file)
        
        # Insertar texto entre gráficos
        plot.new()  # Crear un nuevo lienzo para el texto
        title("REPORT", cex = 1.5, line = -1, col = "black") # Texto centrado en la página
        
        mtext(paste0("River network: ", input$drn_opt), side = 3, line = n_line, adj = 0)
        
        n_line = n_line-1.2
        mtext(paste0("Number of planning units: ", last_model$number_of_planning_units()), side = 3, line = n_line, adj = 0)

        wrapped_text <- strwrap(paste0("Features selected: ", features_names), width = 80)
        n_line = n_line-1.2
        for (i in seq_along(wrapped_text)) {
          mtext(wrapped_text[i], side = 3, line = n_line, adj = 0)
          n_line = n_line- 1
        }
        
        n_line = n_line-0.2
        mtext(paste0("Aggregation level: ", input$blm), side = 3, line = n_line, adj = 0)
        
        
        #mtext(paste0("Features selected: ", features_names), side = 3, line = -3.5, adj = 0)
        
        
        
        plot.new() 
        # Insertar la tabla en el PDF
        mtext(paste0("Representativeness "), side = 3, line = -3.5, adj = 0)
        grid.table(repr)
        
        state <- input$`features-stats_weights`
        weights <- state$weights
        
        p1 <- update_map_opt_ggplot(drns_short[which(drns_long == input$drn_opt)],
                              last_sol,
                              weights)

        print(p1)
        
        p1 <- figure_inter_reached_ggplot(drns_short[which(drns_long == input$drn_opt)])
        
        print(p1)
        
        click_shape <- input$map_opt_shape_click
        
        if(!is.null(click_shape) && input$drn_opt != " "){
          
          state <- input$`features-stats_weights`
          weights <- state$weights
          
          if(!is.null(weights)){
            p2 <- figure_targets_reached(drns_short[which(drns_long == input$drn_opt)],
                                         weights, 
                                         click_shape$id)
          }
          
          print(p2)
        }
        
        dev.off()
      } else {
        showNotification("Imposible to generate PDF: You need to run first a model.", type = "error")
      }
    }
  )
}

