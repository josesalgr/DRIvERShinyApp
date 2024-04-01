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
  
  ### Results exploration
  ## Selected options (find short names)
  selected_drn <- reactive({
    k_drn <- which(drns_long == input$drn)
    drns_short[k_drn]
  })
  
  selected_indicator <- reactive({
    k_ind <- which(indicators_long == input$indicator)
    indicators_short[k_ind]
  })
  
  selected_ssp_plot3 <- reactive({
    k_ssp <- which(ssp_long == input$ssp_plot3)
    ssp_short[k_ssp]
  })
  
  selected_ssp_plot10 <- reactive({
    k_ssp <- which(ssp_long == input$ssp_plot10)
    ssp_short[k_ssp]
  })
  
  selected_gcm_plot10 <- reactive({
    k_gcm <- which(gcm_long == input$gcm_plot10)
    gcm_short[k_gcm]
  })
  
  toListen <- reactive({
    list(input$indicator,input$drn,input$dataset)
  })
  
  observeEvent(toListen(), {
    session$sendCustomMessage(type = 'plot6_set', message = character(0))
  })
  
  observeEvent(toListen(), {
    session$sendCustomMessage(type = 'plot10_set', message = character(0))
  })
  
  ## Dynamic boxes
  # Box 1 : interannual variability (Network indicators)
  output[["box1"]] <- renderUI({
    if(selected_indicator() %in% network_ind_Y)
    {
      if(input$dataset == "Observed (1960-2021)"){
        box(title = "Inter-annual variability", width = 6, status = "primary", solidHeader = TRUE,
            plotlyOutput("plot1")
        )
      }else{
        tabBox(title = "Inter-annual variability", side="right",
               tabPanel("Continuous",
                        plotlyOutput("plot2")),
               tabPanel("By periods",
                        selectInput("ssp_plot3", label = "Scenario", choices = ssp_long),
                        plotOutput("plot3"))
        )
      }
    }
  })
  
  # Box 2 : seasonal pattern (Network indicators)
  output[["box2"]] <- renderUI({
    if(selected_indicator() %in% network_ind_M)
    {
      box(title = "Seasonal pattern", width = 6, status = "primary", solidHeader = TRUE,
          if(input$dataset == "Observed (1960-2021)"){
            plotlyOutput("plot4")
          }else{
            plotlyOutput("plot5")
          }
      )
    }
  })
  
  # Box 3 : spatial pattern (Reach indicators)
  output[["box3"]] <- renderUI({
    if((selected_indicator() %in% reach_ind_Y) & input$dataset == "Observed (1960-2021)")
    {
      if(selected_indicator() %in% reach_ind_M)
      {
        tabBox(title = "Spatial pattern", side="right",
               tabPanel("Time aggregation",
                        selectInput("period_aggr_plot6", label = "Period", choices = c("Annual","January","February","March","April","May","June","July","August","September","October","November","December")),
                        girafeOutput("plot6"),
                        "Click on a reach to plot its flow intermittence pattern."
               ),
               tabPanel("By date",
                        fluidRow(
                          column(width=3,selectInput("year_plot7", label = "Year", choices = 1960:2021)),
                          column(width=3,selectInput("month_plot7", label = "Month", choices = c("Annual","January","February","March","April","May","June","July","August","September","October","November","December")))
                        ),
                        girafeOutput("plot7"))
        )
      }else{
        tabBox(title = "Spatial pattern", side="right",
               tabPanel("Time aggregation",
                        girafeOutput("plot6"),
                        "Click on a reach to plot its flow intermittence pattern."),
               tabPanel("By date",
                        selectInput("year_plot7", label = "Year", choices = 1960:2021),
                        girafeOutput("plot7"))
        )
      }
    }else if ((selected_indicator() %in% reach_ind_Y) & input$dataset == "Projections (1985-2100)")
    {
      box(title = "Evolution of the spatial pattern", width = 12, status = "primary", solidHeader = TRUE,
          fluidRow(
            column(width=6,selectInput("ssp_plot10", label = "Scenario", choices = ssp_long)),
            column(width=3,selectInput("gcm_plot10", label = "GCM", choices = gcm_long))
          ),
          fluidRow(
            column(width = 12,
                   girafeOutput("plot10", width="auto", height="auto"))
          ),
          "Click on a reach to plot its evolution under climate change.")
    }
  })
  
  # Box 4 : inter-annual variability and seasonal pattern for a specific reach (Reach indicators)
  output[["box4"]] <- renderUI({
    if(length(input$plot6_selected) > 0)
    {
      tabBox(title = paste0("Reach ",input$plot6_selected), side="right",
             tabPanel("Inter-annual variability",
                      plotlyOutput("plot8")),
             tabPanel("Seasonal pattern",
                      if(selected_indicator() %in% reach_ind_M)
                      {
                        plotlyOutput("plot9")
                      }else{
                        textOutput("text9")
                      }
             )
      )
    }else if(length(input$plot10_selected) > 0)
    {
      tabBox(title = paste0("Reach ",input$plot10_selected," projections"), side="right",
             tabPanel("Continuous",
                      plotlyOutput("plot11")),
             tabPanel("By period",
                      plotOutput("plot12")),
             tabPanel("Seasonal pattern",
                      if(selected_indicator() %in% reach_ind_M)
                      {
                        plotlyOutput("plot13")
                      }else{
                        textOutput("text9")
                      })
      )
    }
  })
  
  ## Plots
  # Network yearly time series (observed period)
  output[["plot1"]] <- renderPlotly({
    p1 <- figure_1(selected_drn(), input$drn, selected_indicator(), input$indicator)
    ggplotly(p1)
  })
  
  # Network yearly time series (projections)
  output[["plot2"]] <- renderPlotly({
    p2 <- figure_2(selected_drn(), input$drn, selected_indicator(), input$indicator)
    ggplotly(p2)
  })
  
  # Network yearly by periods (projections)
  output[["plot3"]] <- renderPlot({
    p3 <- figure_3(selected_drn(), input$drn, selected_indicator(), input$indicator, selected_ssp_plot3())
    p3
  })
  
  # Network seasonal pattern (observed period)
  output[["plot4"]] <- renderPlotly({
    p4 <- figure_4(selected_drn(), input$drn, selected_indicator(), input$indicator)
    ggplotly(p4)
  })
  
  # Network seasonal pattern (projections)
  output[["plot5"]] <- renderPlotly({
    p5 <- figure_5(selected_drn(), input$drn, selected_indicator(), input$indicator)
    ggplotly(p5)
  })
  
  # Spatial pattern aggregated (observed period)
  output[["plot6"]] <- renderGirafe({
    if(selected_indicator()  %in% reach_ind_M)
    {
      p6 <- figure_6(selected_drn(), input$drn, selected_indicator(), input$indicator, input$period_aggr_plot6)
    }else{
      p6 <- figure_6_FstDrE(selected_drn(), input$drn, selected_indicator(), input$indicator)
    }
    
    x6 <- girafe(ggobj = p6)
    x6 <- girafe_options(x6,
                         opts_selection(type = "single",
                                        css = "color:red;stroke:red;r:5pt;"))
  })
  
  # Spatial pattern by date (observed period)
  output[["plot7"]] <- renderGirafe({
    if(selected_indicator()  %in% c("conD","conF","numFreDr","numFreRW"))
    {
      p7 <- figure_7(selected_drn(), input$drn, selected_indicator(), input$indicator, input$year_plot7, input$month_plot7)
    }else if(selected_indicator()  %in% c("durD","durF")){
      p7 <- figure_7_dur(selected_drn(), input$drn, selected_indicator(), input$indicator, input$year_plot7, input$month_plot7)
    }else{
      p7 <- figure_7_FstDrE(selected_drn(), input$drn, selected_indicator(), input$indicator, input$year_plot7)
    }
    
    x7 <- girafe(ggobj = p7)
    x7 <- girafe_options(x7,
                         opts_selection(type = "none",
                                        css = "color:red;stroke:red;r:5pt;"))
  })
  
  # Reach yearly time series (observed period)
  output[["plot8"]] <- renderPlotly({
    p8 <- figure_8(selected_drn(), input$drn, selected_indicator(), input$indicator, input$plot6_selected)
    ggplotly(p8)
  })
  
  # Reach seasonal pattern (observed period)
  output[["plot9"]] <- renderPlotly({
    p9 <- figure_9(selected_drn(), input$drn, selected_indicator(), input$indicator, input$plot6_selected)
    ggplotly(p9)
  })
  output$text9 <- renderText({"There is no seasonal pattern for this indicator."})
  
  # Spatial pattern yearly aggregated (projections)
  output[["plot10"]] <- renderGirafe({
    p10 <- figure_10(selected_drn(), input$drn, selected_indicator(), input$indicator, selected_ssp_plot10(), selected_gcm_plot10())
    
    # x10 <- wrap_plots(p10)
    x10 <- p10[[1]]+p10[[2]]
    x10 <- girafe(ggobj = x10)
    x10 <- girafe_options(x10,
                          opts_selection(type = "single",
                                         css = "color:red;stroke:red;r:5pt;"))
    x10
  })
  
  # Reach yearly time series (projections)
  output[["plot11"]] <- renderPlotly({
    p11 <- figure_11(selected_drn(), input$drn, selected_indicator(), input$indicator, input$plot10_selected)
    ggplotly(p11)
  })
  
  # Reach yearly by periods (projections)
  output[["plot12"]] <- renderPlot({
    p12 <- figure_12(selected_drn(), input$drn, selected_indicator(), input$indicator, input$plot10_selected, selected_ssp_plot10())
    p12
  })
  
  # Reach seasonal pattern (projections)
  output[["plot13"]] <- renderPlotly({
    p13 <- figure_13(selected_drn(), input$drn, selected_indicator(), input$indicator, input$plot10_selected)
    ggplotly(p13)
  })
  
  

  # OPTIMIZATION----------------------------------------------------------------

  ## Plot interactive map of the river network
  output[["map_opt"]] <- renderLeaflet({
    
    enable("drn")
    
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
        options = layersControlOptions(collapsed = TRUE)
      ) %>% leafem::addMouseCoordinates() 
  })
  
  
  ## Select bottom River network
  observeEvent(input$drn, {
    
    if(input$drn != " "){
      
      # Remove previous Leaflet map
      leafletProxy("map_opt") %>% clearMarkers() %>% clearShapes()
      
      # Plot Leaflet map 
      print_shape(drns_short[which(drns_long == input$drn)])
      
      # Show SelectInput "Variable"
      shinyjs::show("Variable")
      
      # Update Variable SelectInput
      updateSelectInput(session, "Variable", selected = " ")
      
    }
  })
  
  ## Update range
  observeEvent(input$range, {
    
    if(input$drn != " " && input$Variable != " "){
      #update_range(drns_short[which(drns_long == input$drn)], input$range)
      
      update_map(drns_short[which(drns_long == input$drn)], 
                 input$Variable,
                 drns_countries[which(drns_long == input$drn)],
                 input$range)
    }
    
  })
  
  ## Update Variable SelectInput
  observeEvent(input$Variable, {
    
    if(input$Variable != " "){
      
      # Enable range bar
      enable("range")
      
      update_map(drns_short[which(drns_long == input$drn)], 
                 input$Variable,
                 drns_countries[which(drns_long == input$drn)],
                 input$range)
    }
    
  })
  
  ## Network time series (observed period)
  output[["plotly"]] <- renderPlotly({
    
    click_shape <- input$map_opt_shape_click

    if(input$Variable != " " && input$drn != " "){
      
      is_in_map <- is_id(drns_short[which(drns_long == input$drn)], 
                         input$Variable,
                         drns_countries[which(drns_long == input$drn)],
                         input$range,
                         click_shape$id)
      
      if(!is.null(click_shape) && is_in_map){
        p1 <- figure_in_map(input$Variable, click_shape$id, drns_countries[which(drns_long == input$drn)])
        
        ggplotly(p1)
      }
    }
  })
  
}

