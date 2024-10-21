
read_shp <- function(DRN){
  # path to indicator file
  path <- paste0("data/shp/",DRN,"/small_river_network.shp")
  
  # read shapefile
  shp <- st_read(path)
  
  return(shp)
}

########## plots ###############################
# Plot interactive map of the river network and watershed
figure_map <- function(drn){
  if(drn == "Lepsamanjoki"){
    river_network <- read_shp("Lepsamaanjoki")
  }else{
    river_network <- read_shp(drn)
  }
  
  river_network <- st_transform(river_network, 4326)
  
  p_map <-leaflet() %>%
    addTiles() %>%
    addProviderTiles(providers$OpenStreetMap) %>%
    addPolylines(data = river_network, color = "darkblue",opacity = 1, weight=2) %>%
    addScaleBar(position="bottomright")
  return(p_map)
}

################################################################################
#---------------OPTIMIZATION----------------------------------------------------
################################################################################

get_shape <- function(drn){
  # shpdf is a data.frame with the name, size, type and
  # datapath of the uploaded files
  shpdf <- drn
  
  # Construct the path to the shapefile
  path_shp <- paste0("data/shp/", shpdf, "/river_network.shp")
  
  # Read and preprocess the shapefile
  shape <- terra::vect(path_shp)
  shape <- project(shape,  crs("+proj=longlat +datum=WGS84 +no_defs"))
  shape <- terra::aggregate(shape, by = "ID", count=FALSE, overwrite=TRUE, dissolve=FALSE)
  shape$ID <- as.character(shape$ID)
  
  validate(need("ID" %in% names(shape), "The shapefile must be a 'id' column"))
  
  # Assuming the ID attribute is named "ID," replace it with the actual name in your shapefile
  missing_ids <- is.na(shape$ID)
  shape <- shape[!missing_ids, ]
  
  return(shape)
}

get_shape_download <- function(drn, var, period, camp){
  # shpdf is a data.frame with the name, size, type and
  # datapath of the uploaded files
  shpdf <- drn
  
  # Construct the path to the shapefile
  path_shp <- paste0("data/shp/", shpdf, "/river_network.shp")
  
  # Read and preprocess the shapefile
  shape <- terra::vect(path_shp)
  shape <- project(shape,  crs("+proj=longlat +datum=WGS84 +no_defs"))
  shape <- terra::aggregate(shape, by = "ID", count=FALSE, overwrite=TRUE, dissolve=FALSE)
  shape$ID <- as.character(shape$ID)
  
  validate(need("ID" %in% names(shape), "The shapefile must be a 'id' column"))
  
  # Assuming the ID attribute is named "ID," replace it with the actual name in your shapefile
  missing_ids <- is.na(shape$ID)
  shape <- shape[!missing_ids, ]
  
  # Var data
  data <- get_bioinformation(var, drn, period)
  
  data_filtered <- data %>%
    filter(campaign == camp) %>%
    select(WP4, "value") #%>%
  
  shape_var <- terra::merge(shape, data_filtered, by.x = "ID", by.y = "WP4", all.x=TRUE)
  
  return(shape_var)
}


get_bioinformation <- function(var, drn, period){
  
  # read data
  # IF biodiversity variable
  if(var %in% variables_short_biodiversity){
    file_name = paste0("data/data/biodiversity/", drn, "/", period, "/", var, ".csv")
  }
  else if(var %in% variables_short_ecological_functions){
    file_name = paste0("data/data/ecological_functions/", drn, "/", period, "/", var, ".csv")
  }
  else if(var %in% variables_short_ecosystem_services){
    file_name = paste0("data/data/ecosystem_services/", drn, "/", period, "/", var, ".csv")
  }
  
  data_p <- read.table(file_name, sep = ",", header = TRUE)
  data_p$DRN = drn
  
  # if(var == "co2"){
  #   data_p[["value"]] <- data_p[["value"]]/1000
  # }
  
  return(data_p)
}

get_domain_bioinformation <- function(var, drn){
  
  #variables
  #"rich", "simp", "FD", "FR", "alpha", "beta", "gamma", "dem", "co2", "dr_lr", "dr_ss", "er_rip", 
  #"er_sl", "flo_rip", "flo_sl", "th_reg"
  
  # read data
  # IF biodiversity variable
  if(var %in% variables_short_biodiversity){
    
    file_name_per1 = paste0("data/data/biodiversity/", drn, "/", scale_list_short[1], "/", var, ".csv")
    file_name_per2 = paste0("data/data/biodiversity/", drn, "/", scale_list_short[2], "/", var, ".csv")
  }
  else if(var %in% variables_short_ecological_functions){
    
    file_name_per1 = paste0("data/data/ecological_functions/", drn, "/", scale_list_short[1], "/", var, ".csv")
    file_name_per2 = paste0("data/data/ecological_functions/", drn, "/", scale_list_short[2], "/", var, ".csv")
  }
  else if(var %in% variables_short_ecosystem_services){
    
    file_name_per1 = paste0("data/data/ecosystem_services/", drn, "/", scale_list_short[1], "/", var, ".csv")
    file_name_per2 = paste0("data/data/ecosystem_services/", drn, "/", scale_list_short[2], "/", var, ".csv")
  }

  data_per1 <- read.table(file_name_per1, sep = ",", header = TRUE)
  data_per2 <- read.table(file_name_per2, sep = ",", header = TRUE)
  
  data_per1 <- data_per1 %>%
    select("WP4", "campaign", "value", "pu")
  
  data_per2 <- data_per2 %>%
    select("WP4", "campaign", "value", "pu")

  data = rbind(data_per1, data_per2)
  
  id_var = which(colnames(data) == "value")
  
  # if(var == "co2"){
  #   data[["value"]] <- data[["value"]]/1000
  # }
  
  data_summary  <- data %>%
    summarize(min_var = min(data[id_var]),
              max_var = max(data[id_var]))
  
  return(data_summary)
}

get_difference_domain_bioinformation <- function(var, drn){
  
  #variables
  #"rich", "simp", "FD", "FR", "alpha", "beta", "gamma", "dem", "co2", "dr_lr", "dr_ss", "er_rip", 
  #"er_sl", "flo_rip", "flo_sl", "th_reg"
  
  # read data
  # IF biodiversity variable
  if(var %in% variables_short_biodiversity){
    
    file_name_per1 = paste0("data/data/biodiversity/", drn, "/", scale_list_short[1], "/", var, ".csv")
    file_name_per2 = paste0("data/data/biodiversity/", drn, "/", scale_list_short[2], "/", var, ".csv")
  }
  else if(var %in% variables_short_ecological_functions){
    file_name_per1 = paste0("data/data/ecological_functions/", drn, "/", scale_list_short[1], "/", var, ".csv")
    file_name_per2 = paste0("data/data/ecological_functions/", drn, "/", scale_list_short[2], "/", var, ".csv")
  }
  else if(var %in% variables_short_ecosystem_services){
    
    file_name_per1 = paste0("data/data/ecosystem_services/", drn, "/", scale_list_short[1], "/", var, ".csv")
    file_name_per2 = paste0("data/data/ecosystem_services/", drn, "/", scale_list_short[2], "/", var, ".csv")
  }
  
  data_per1 <- read.table(file_name_per1, sep = ",", header = TRUE)
  data_per2 <- read.table(file_name_per2, sep = ",", header = TRUE)
  
  # if(var == "co2"){
  #   data_per1[["value"]] <- data_per1[["value"]]/1000
  #   data_per2[["value"]] <- data_per2[["value"]]/1000
  # }
  
  data_per1 <- data_per1 %>%
    select("WP4", "campaign", "value")
  
  data_per2 <- data_per2 %>%
    select("WP4", "campaign", "value")
  

  data <- data_per1 %>%
    left_join(data_per2, by = c("WP4", "campaign"))
  
  data$dif = data[,4] - data[,3]
  
  data_summary  <- data %>%
    summarize(min_var = min(data$dif),
              max_var = max(data$dif))
  
  return(data_summary)
}


print_shape <- function(drn, map_name){

  shape <- get_shape(drn)
  
  longlat <- terra::geom(shape)
  long_shape <- mean(longlat[,3])*0.98
  lat_shape <- mean(longlat[,4])
  zoom_shape <- 10
  type_shape <- terra::geomtype(shape)
  colour_boundary <- "black"
  colour_fill <- "transparent"
  
  if("ID" %in% names(shape)){
    popup_pu <- sprintf(
      "<strong>ID %s</strong><br/>",
      shape$ID) %>% 
      lapply(htmltools::HTML)
  }
  else{
    popup_pu <- NA
  }
  

  leafletProxy(map_name, data = shape) %>%
      clearControls() %>%
      setView(lng = long_shape, lat = lat_shape, zoom = 10) %>%
      addPolylines(group = "Base",
                   weight = 2, 
                   color = "black",
                   fill = FALSE, 
                   opacity = 1,
                   popup = popup_pu, 
                   layerId = ~ID, 
                   highlightOptions = highlightOptions(
                     weight = 5,
                     bringToFront = TRUE,
                     sendToBack = TRUE)
      )%>%
      addDrawToolbar(
        position = "bottomright",
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
      )  %>%
      addLayersControl(
        baseGroups = c("CartoDB", "Elevation", "Satellite","Without background"),
        options = layersControlOptions(collapsed=TRUE))

}

# Network yearly time series (observed period)
figure_time_serie <- function(var, var_long, id, drn, camp, period, compare){
  
  # Automatically wrap the title based on a maximum width
  max_width <- 50
  
  if(var_long == "Co2 sequestration"){
    y_units = "gC-co2/m2"
  }
  else{
    y_units = "Value"
  }

  
  if(compare){
    data_ini <- get_bioinformation(var, drn, "current")
    data_final <- get_bioinformation(var, drn, period)
    
    long_title = paste0("Comparation of the temporal trend of ",var_long, " between ", "2021 and ", period)
    wrapped_title <- str_wrap(long_title, width = max_width)
    dom_summary = get_difference_domain_bioinformation(var, drn)
    
    data_ini_filtered <- data_ini %>%
      filter(WP4 == id) %>%
      mutate(period = "2021")
    
    data_final_filtered <- data_final %>%
      filter(WP4 == id) %>%
      mutate(period = period)
    
    if("value_sd" %in% colnames(data_final_filtered)){

      data_ini_filtered <- data_ini_filtered %>%
        select(campaign, value, period) %>%
        mutate(value_sd = 0)
      
      data_final_filtered <- data_final_filtered %>%
        select(campaign, value, value_sd, period)
      
      data_filtered = rbind(data_ini_filtered, data_final_filtered)
      
      p1 <- ggplot(data_filtered, aes(x=campaign, y=value, group = period, color = period))+
        geom_line()+
        geom_point() +
        geom_ribbon(aes(ymin = value - value_sd, ymax = value + value_sd, fill = period), alpha = 0.2, color = NA) +  # Shaded ribbon for SD
        scale_color_manual(values = c("black", "forestgreen"), name = " ")+
        theme_bw()+
        ylab(y_units)+xlab("Campaign")+
        ggtitle(wrapped_title)+
        scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6), labels = campaign_list) +
        #scale_y_continuous(limits=c(dom_summary$min_var, dom_summary$max_var)) +
        theme(legend.title = element_blank(),
              plot.title = element_text(size = 10, hjust = 0.5, lineheight = 0.5))  # Color first axis text blue)
      
    }
    else{
      data_ini_filtered <- data_ini_filtered %>%
        select(campaign, value, period)
    
      data_final_filtered <- data_final_filtered %>%
        select(campaign, value, period)
    
      data_filtered = rbind(data_ini_filtered, data_final_filtered)
      
      p1 <- ggplot(data_filtered, aes(x=campaign, y=value, group = period, color = period))+
        geom_line()+
        #geom_point(aes(colour = ifelse(campaign == camp, TRUE, FALSE)), size = 2, show.legend = FALSE) +
        geom_point() +
        scale_color_manual(values = c("black", "forestgreen"), name = " ")+
        theme_bw()+
        ylab(y_units)+xlab("Campaign")+
        ggtitle(wrapped_title)+
        scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6), labels = campaign_list) +
        #scale_y_continuous(limits=c(dom_summary$min_var, dom_summary$max_var)) +
        theme(legend.title = element_blank(),
              plot.title = element_text(size = 10, hjust = 0.5, lineheight = 0.5))  # Color first axis text blue)
      
    }
  }
  else{
    data <- get_bioinformation(var, drn, period)
    
    data_filtered <- data %>%
      filter(WP4 == id)
    
    long_title = paste0("Temporal trend of ",var_long, " in ", period)
    wrapped_title <- str_wrap(long_title, width = max_width)
    dom_summary = get_domain_bioinformation(var, drn)
    
    if("value_sd" %in% colnames(data_filtered)){
      p1 <- ggplot(data_filtered, aes(x=campaign, y=value))+
        geom_line()+
        geom_point(aes(colour = ifelse(campaign == camp, TRUE, FALSE)), size = 2, show.legend = FALSE) +
        geom_ribbon(aes(ymin = value - value_sd, ymax = value + value_sd, fill = "forestgreen"), alpha = 0.2, color = NA) +  # Shaded ribbon for SD
        #geom_errorbar(aes(ymin = value - value_sd, ymax = value + value_sd), width = 0.2) + 
        scale_color_manual(values = c("black", "forestgreen"))+
        theme_bw()+
        ylab(y_units)+xlab("Campaign")+
        ggtitle(wrapped_title)+
        scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6), labels = campaign_list) +
        #scale_y_continuous(limits=c(dom_summary$min_var, dom_summary$max_var)) +
        theme(legend.position = "none",
              plot.title = element_text(size = 10, hjust = 0.5, lineheight = 0.5))
    }
    else{
      p1 <- ggplot(data_filtered, aes(x=campaign, y=value))+
        geom_line()+
        geom_point(aes(colour = ifelse(campaign == camp, TRUE, FALSE)), size = 2, show.legend = FALSE) +
        scale_color_manual(values = c("black", "forestgreen"))+
        theme_bw()+
        ylab(y_units)+xlab("Campaign")+
        ggtitle(wrapped_title)+
        scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6), labels = campaign_list) +
        #scale_y_continuous(limits=c(dom_summary$min_var, dom_summary$max_var)) +
        theme(legend.position = "none",
              plot.title = element_text(size = 10, hjust = 0.5, lineheight = 0.5))
    }
  }

  return(p1)
}


# Update the map using the information of Indicator
update_map_data <- function(drn, var, camp, period, compare){
  
  shape <- get_shape(drn)
  
  if(compare){
    data_ini <- get_bioinformation(var, drn, "current")
    data_final <- get_bioinformation(var, drn, period)

    data_ini_filtered <- data_ini %>%
      filter(campaign == camp) %>%
      select(WP4, "value")
    
    data_final_filtered <- data_final %>%
      filter(campaign == camp) %>%
      select(WP4, "value")
    
    data_filtered = data_final_filtered %>%
      left_join(data_ini_filtered, by = "WP4")
    
    data_filtered = data_filtered %>%
      mutate(diff = data_filtered[,2] - data_filtered[,3])%>%
      mutate(ID = WP4)

    shape_var <- terra::merge(shape, data_filtered, by.x = "ID", by.y = "WP4", all.x=TRUE)
    
    int_curr <- read.table(paste0("data/data/regimes/", drn, "/current/intermit.csv"), sep = ",", header = TRUE)
    id_col_reg = which(colnames(int_curr) == "Regime")
    colnames(int_curr)[id_col_reg] = "RegimeC"
    
    int_future <- read.table(paste0("data/data/regimes/", drn, "/future/intermit.csv"), sep = ",", header = TRUE)
    id_col_reg = which(colnames(int_future) == "Regime")
    colnames(int_future)[id_col_reg] = "RegimeF"
    
    b_int <- left_join(data_filtered,int_curr, by="WP4") #join intermittence with freq 
    b_int <- left_join(b_int,int_future, by="WP4") #join intermittence with freq 
    
    shape_var <- terra::merge(shape, b_int, by.x = "ID", by.y = "WP4", all.x=TRUE)
    shape_var$inter = ifelse(shape_var$RegimeC == "perennial" & shape_var$RegimeF == "perennial", 1, 
                             ifelse(shape_var$RegimeC == "perennial" & shape_var$RegimeF != "perennial", "1,5",
                                    10))
    
    dom_summary = get_difference_domain_bioinformation(var, drn)
    
    pal <- colorNumeric(palette="RdYlBu", domain = c(dom_summary$min_var, dom_summary$max_var), na.color="orange")

    title_legend <- ""
    
    popup_map <- sprintf(
      "<strong>ID %s</strong><br/>
         Difference: %g <br/>
         Type: %s <br/>",
      shape_var$ID, 
      shape_var$diff,
      shape_var$RegimeC) %>% 
      lapply(htmltools::HTML)
    
    
    leafletProxy("map_data", data = shape_var) %>%
      clearControls() %>%
      addPolylines(group = "Base",
                   weight = 3, 
                   color = ~pal(shape_var$diff),
                   fill = FALSE, 
                   opacity = 1,
                   popup = popup_map, 
                   layerId = ~ID, 
                   dashArray = ~inter,
                   highlightOptions = highlightOptions(
                     weight = 5,
                     color = "red",
                     bringToFront = TRUE,
                     sendToBack = TRUE)
      )%>%
      addLegend(group = "Base",
                pal = pal,
                title = title_legend,
                values = shape_var$diff,
                labels = c("Dashed Line", "Solid Line"),
                opacity = 1,
                bins = 10,
                labFormat = labelFormat(digits = 5),
                position = "topright") %>%
      # Add Custom Legend for Line Patterns
      addControl(html = "<div style='background: white; padding: 5px;'>
                        <div><svg height='10' width='20'><line x1='0' y1='5' x2='20' y2='5' style='stroke:black; stroke-width:2;' /></svg> Perennial</div>
                        <div><svg height='10' width='20'><line x1='0' y1='5' x2='20' y2='5' style='stroke:black; stroke-width:2; stroke-dasharray:1,5;' /></svg> Perennial to Intermittent</div>
                        <div><svg height='10' width='20'><line x1='0' y1='5' x2='20' y2='5' style='stroke:black; stroke-width:2; stroke-dasharray:5,5;' /></svg> Intermittent</div>
                      </div>", 
                 position = "topright")
    
    
    
  }
  else{
    data <- get_bioinformation(var, drn, period)
    
    data_filtered <- data %>%
      filter(campaign == camp) %>%
      select(WP4, "value") %>%
      mutate(ID = WP4)#%>%
    #distinct(ID, .keep_all = TRUE)
    
    
    shape_var <- terra::merge(shape, data_filtered, by.x = "ID", by.y = "WP4", all.x=TRUE)
    
    int_curr <- read.table(paste0("data/data/regimes/", drn, "/current/intermit.csv"), sep = ",", header = TRUE)
    id_col_reg = which(colnames(int_curr) == "Regime")
    colnames(int_curr)[id_col_reg] = "RegimeC"
    
    int_future <- read.table(paste0("data/data/regimes/", drn, "/future/intermit.csv"), sep = ",", header = TRUE)
    id_col_reg = which(colnames(int_future) == "Regime")
    colnames(int_future)[id_col_reg] = "RegimeF"
    
    b_int <- left_join(data_filtered,int_curr, by = "WP4") #join intermittence with freq 
    b_int <- left_join(b_int,int_future, by = "WP4")  #join intermittence with freq 
    
    shape_var <- terra::merge(shape, b_int, by.x = "ID", by.y = "WP4", all.x=TRUE)
    shape_var$inter = ifelse(shape_var$RegimeC == "perennial" & shape_var$RegimeF == "perennial", 1, 
                             ifelse(shape_var$RegimeC == "perennial" & shape_var$RegimeF != "perennial", "1,5",
                                    10))
    
    dom_summary = get_domain_bioinformation(var, drn)

    pal <- colorNumeric(palette="BrBG", domain=c(dom_summary$min_var, dom_summary$max_var), na.color="orange")
    
    title_legend <- ""
    
    popup_map <- sprintf(
      "<strong>ID %s</strong><br/>
         Value: %g <br/>
         Type: %s <br/>",
      shape_var$ID, 
      shape_var[["value"]][,1],
      shape_var$RegimeC) %>% 
      lapply(htmltools::HTML)
    
    
    leafletProxy("map_data", data = shape_var) %>%
      clearControls() %>%
      # Add Custom Legend for Line Patterns
      addControl(html = "<div style='background: white; padding: 5px;'>
                        <div><svg height='10' width='20'><line x1='0' y1='5' x2='20' y2='5' style='stroke:black; stroke-width:2;' /></svg> Perennial</div>
                        <div><svg height='10' width='20'><line x1='0' y1='5' x2='20' y2='5' style='stroke:black; stroke-width:2; stroke-dasharray:1,5;' /></svg> Perennial to Intermittent</div>
                        <div><svg height='10' width='20'><line x1='0' y1='5' x2='20' y2='5' style='stroke:black; stroke-width:2; stroke-dasharray:5,5;' /></svg> Intermittent</div>
                      </div>", 
                 position = "topright") %>%
      addPolylines(group = "Base",
                   weight = 3, 
                   color = ~pal(shape_var[["value"]][,1]),
                   fill = FALSE, 
                   opacity = 1,
                   dashArray = ~inter,
                   popup = popup_map, 
                   layerId = ~ID, 
                   highlightOptions = highlightOptions(
                     weight = 5,
                     color = "red",
                     bringToFront = TRUE,
                     sendToBack = TRUE)
      )%>%
      addLegend(group = "Base",
                pal = pal,
                title = title_legend,
                values = shape_var[["value"]][,1],
                labFormat = labelFormat(digits = 5),
                opacity = 1,
                bins = 10,
                position = "topright")
  }
}


# The shape clicked is valid?
is_id <- function(drn, var, camp, id, period){
  
  shape <- get_shape(drn)
  data <- get_bioinformation(var, drn, period)
  
  data_filtered <- data %>%
    filter(campaign == camp) %>%
    select(WP4, "value") %>%
    distinct(WP4, .keep_all = TRUE)
  
  shape_var <- terra::merge(shape, data_filtered, by.x = "ID", by.y = "WP4", all.x=TRUE)
  
  row_index = which(data$WP4 == id)
  
  return(any(row_index))
}

updateCoords <- function(name, long, latt, zoom_map){
  
  if(name == "tab_map"){
    leafletProxy("map_opt") %>%
      setView(lng = long, lat = latt, zoom = zoom_map)
  }
  else{
    leafletProxy("map_data") %>%
      setView(lng = long, lat = latt, zoom = zoom_map)
  }
}

calculate_opt_inputs <- function(drn, var){ 
  
  #parameters
  multiplier <- 100000
  
  shape <- get_shape(drn)
  
  ##############################################################################
  #Current-------------------------------------------------------------------------
  ##############################################################################
  period = "current"
  
  #pu dat-----------------------------------------------------------------------
  pu_dat = as.data.frame(shape)
  pu_dat <- pu_dat[,c("ID", "mean_AREA_SQKM")]
  colnames(pu_dat) <- c("id", "cost")
  pu_dat$id <- as.numeric(pu_dat$id)
  pu_dat$cost <- 1
  pu_dat$status <- 0
  pu_dat$id_old <- pu_dat$id
  pu_dat$period <- period
  
  # Create a list of data frames, each multiplied by a different value
  dfs <- purrr::map(1:6, ~ mutate(pu_dat, id = id + (.x * multiplier)))
  
  # Add a column "campaign" with values 1 and 2 to each dataframe in dfs
  dfs <- purrr::map2(dfs, 1:6, ~ mutate(.x, campaign = .y))
  
  # Combine the data frames in the list into a single data frame
  pu_dat <- bind_rows(dfs)
  
  #puvspr_dat-------------------------------------------------------------------
  set_var <- c()
  
  for(i in 1:length(var)){
    
    var_short = variables_short[which(variables_long == names(var)[i])]
    set_var <- c(set_var, var_short)
    
    if(i == 1){
      data <- get_bioinformation(var_short, drn, period)
      
      data$id_old <- data$WP4
      
      data_filtered <- data %>%
        #filter(campaign == 6) %>%
        select(WP4, id_old, campaign, "value") %>%
        mutate(!!sym("WP4") := WP4 + campaign*multiplier) %>%
        select(WP4, id_old, campaign, "value") %>%
        rename(!!var_short := value)
      
      if(var_short %in% variables_short_percentile){
        
        quantiles_var <- quantile(data_filtered[[var_short]], probs = c(0.5, 0.75, 0.90), na.rm = TRUE)

        data_filtered <- data_filtered %>%
          mutate(value = ifelse(data_filtered[[var_short]] > quantiles_var[1], 1, 0)) %>%
          #select(-c(!!var_short)) %>%
          rename(!!paste0(var_short, "_original") := var_short) %>%
          rename(!!var_short := value)
      }
    }
    else{
      data_aux <- get_bioinformation(var_short, drn, period)

      data_aux$id_old <- data_aux$WP4
      
      data_aux_filtered <- data_aux %>%
        #filter(campaign == 6) %>%
        select(WP4, id_old, campaign, "value") %>%
        mutate(!!sym("WP4") := WP4 + campaign*multiplier) %>%
        select(WP4, id_old, campaign, "value") %>%
        rename(!!var_short := value)
      
      if(var_short %in% variables_short_percentile){
        
        quantiles_var <- quantile(data_aux_filtered[[var_short]], probs = c(0.5, 0.75, 0.90), na.rm = TRUE)
        
        data_aux_filtered <- data_aux_filtered %>%
          mutate(value = ifelse(data_aux_filtered[[var_short]] > quantiles_var[1], 1, 0)) %>%
          #select(-c(!!var_short)) %>%
          rename(!!paste0(var_short, "_original") := var_short) %>%
          rename(!!var_short := value)
      }
      
      data_filtered = data_aux_filtered %>%
        left_join(data_filtered, by = c("WP4", "id_old", "campaign"))
    }
  }

  puvspr_dat_current <- data_filtered %>%
    tidyr::pivot_longer(-c(WP4, id_old, campaign), names_to = "species", values_to = "amount") %>%
    filter(amount != 0)
  
  colnames(puvspr_dat_current) <- c("pu", "id_old", "campaign", "species", "amount")
  puvspr_dat_current$pu <- as.numeric(puvspr_dat_current$pu)
  puvspr_dat_current <- na.omit(puvspr_dat_current)

  #spec_dat---------------------------------------------------------------------
  names(var) = set_var
  
  spec_dat <- data.frame(id = seq(var),
                         name = names(var),
                         prop = unlist(var, use.names = FALSE))
  
  puvspr_dat_current_filtered <- puvspr_dat_current %>%
    filter(!grepl("original", species)) %>%
    left_join(spec_dat, by = c("species" = "name")) %>%
    select(pu, id, amount, campaign, id_old)
  
  colnames(puvspr_dat_current_filtered) <- c("pu", "species", "amount", "campaign", "id_old")
  
  #boundary---------------------------------------------------------------------
  boundary_dat <- read.table(paste0("data/data/boundaries/", drn, "/bound_mat.csv"), sep = ",", header = TRUE)
  colnames(boundary_dat) <- c("id1", "id2", "boundary")

  boundary_dat <- boundary_dat %>%
    filter(!is.na(boundary)) %>%
    filter(boundary > 0.5)

  ##############################################################################
  #Future-------------------------------------------------------------------------
  ##############################################################################
  period = "future"
  
  #pu dat-----------------------------------------------------------------------
  pu_dat_future = pu_dat
  pu_dat_future$id <- 600000 + pu_dat$id
  pu_dat_future$period <- period
  pu_dat <- rbind(pu_dat, pu_dat_future)
  
  #puvspr_dat-------------------------------------------------------------------
  
  for(i in 1:length(var)){

    var_short = set_var[i]
  
    if(i == 1){
      data <- get_bioinformation(var_short, drn, period)
      
      data$id_old <- data$WP4
      
      data_filtered <- data %>%
        mutate(!!sym("WP4") := WP4 + campaign*multiplier + 600000) %>%
        select(WP4, id_old, campaign, "value", any_of("value_sd")) %>%
        rename(!!var_short := value) 
      
      # Cambiar el nombre de "value_sd" si existe
      if ("value_sd" %in% names(data_filtered)) {
        data_filtered <- data_filtered %>%
          rename(!!paste0(var_short, "_sd") := value_sd)
      }
      
      if(var_short %in% variables_short_percentile){
        
        quantiles_var <- quantile(data_filtered[[var_short]], probs = c(0.5, 0.75, 0.90), na.rm = TRUE)
        
        data_filtered <- data_filtered %>%
          mutate(value = ifelse(data_filtered[[var_short]] > quantiles_var[1], 1, 0)) %>%
          #select(-c(!!var_short)) %>%
          rename(!!paste0(var_short, "_original") := var_short) %>%
          rename(!!var_short := value)
      }
    }
    else{
      data_aux <- get_bioinformation(var_short, drn, period)
      data_aux$id_old <- data_aux$WP4
      
      data_aux_filtered <- data_aux %>%
        mutate(!!sym("WP4") := WP4 + campaign*multiplier + 600000) %>%
        select(WP4, id_old, campaign, "value", any_of("value_sd")) %>%
        rename(!!var_short := value)
      
      # Cambiar el nombre de "value_sd" si existe
      if ("value_sd" %in% names(data_aux_filtered)) {
        data_aux_filtered <- data_aux_filtered %>%
          rename(!!paste0(var_short, "_sd") := value_sd)
      }
      
      if(var_short %in% variables_short_percentile){
        
        quantiles_var <- quantile(data_aux_filtered[[var_short]], probs = c(0.5, 0.75, 0.90), na.rm = TRUE)
        
        data_aux_filtered <- data_aux_filtered %>%
          mutate(value = ifelse(data_aux_filtered[[var_short]] > quantiles_var[1], 1, 0)) %>%
          #select(-c(!!var_short)) %>%
          rename(!!paste0(var_short, "_original") := var_short) %>%
          rename(!!var_short := value)
      }
      
      data_filtered = data_aux_filtered %>%
        left_join(data_filtered, by = c("WP4", "id_old", "campaign"))
    }
  }
  
  puvspr_dat_future_sd <- data_filtered %>%
    tidyr::pivot_longer(-c(WP4, id_old, campaign), names_to = "species", values_to = "amount")
  
  colnames(puvspr_dat_future_sd) <- c("pu", "id_old", "campaign","species", "amount")
  puvspr_dat_future_sd$pu <- as.numeric(puvspr_dat_future_sd$pu)
  puvspr_dat_future_sd <- na.omit(puvspr_dat_future_sd)
  puvspr_dat_future_sd$species <- str_c(puvspr_dat_future_sd$species, "_future")
  

  puvspr_dat_future <- data_filtered %>%
    select(-ends_with("_sd")) %>%
    tidyr::pivot_longer(-c(WP4, id_old, campaign), names_to = "species", values_to = "amount")
  
  colnames(puvspr_dat_future) <- c("pu", "id_old", "campaign","species", "amount")
  puvspr_dat_future$pu <- as.numeric(puvspr_dat_future$pu)
  puvspr_dat_future <- na.omit(puvspr_dat_future)
  puvspr_dat_future$species <- str_c(puvspr_dat_future$species, "_future")
  
  #spec_dat---------------------------------------------------------------------
  names(var) = set_var
  
  spec_dat_future <- data.frame(id = seq(var) + max(spec_dat$id),
                         name = str_c(names(var), "_future"),
                         prop = unlist(var, use.names = FALSE))
  
  puvspr_dat_future_filtered <- puvspr_dat_future %>%
    filter(!grepl("original", species)) %>%
    left_join(spec_dat_future, by = c("species" = "name")) %>%
    select(pu, id, amount, campaign, id_old)
  
  colnames(puvspr_dat_future_filtered) <- c("pu", "species", "amount", "campaign", "id_old")
  
  puvspr_dat <- rbind(puvspr_dat_current_filtered, puvspr_dat_future_filtered)
  spec_dat <- rbind(spec_dat, spec_dat_future)
  
  input_data <- list(pu_dat, spec_dat, puvspr_dat, boundary_dat, puvspr_dat_current, puvspr_dat_future, puvspr_dat_future_sd)
  
  # Assign names to the list elements using setNames
  input_data <- setNames(input_data, c("pu_dat", "spec_dat", "puvspr_dat", "boundary_dat",
                                       "puvspr_dat_current",
                                       "puvspr_dat_future", "puvspr_dat_future_sd"))
  
  return(input_data)
}


optimizing <- function(drn, var, blm){

  input_data <- calculate_opt_inputs(drn, var)
  
  p <- prioritizr::marxan_problem(input_data$pu_dat, 
                                  input_data$spec_dat, 
                                  input_data$puvspr_dat, 
                                  input_data$boundary_dat, 
                                  blm = blm) %>%
    add_rsymphony_solver(gap = 0.10, time_limit = 50)
  
  sol <- solve(p, force = TRUE)
  
  last_sol <<- sol
  last_model <<- p
  
  update_map_opt(drn, sol, var)
  
  calculate_cons_rest(drn, var)
  
}


calculate_cons_rest <- function(drn, var){
  
  input_data <- calculate_opt_inputs(drn, var)
  
  data_filtered_future <- input_data$puvspr_dat_future %>%
    filter(!species %in% paste0(variables_short_percentile, "_future") | grepl("_original_", species)) %>%
    mutate(species = ifelse(grepl(paste0(variables_short_percentile, collapse = "|"), species) & grepl("_original_", species),
                            sub("_original_", "_", species), species)) %>%
    rename(WP4 = id_old) %>%
    rename(name = species) %>%
    select(WP4, name, amount, campaign) %>%
    mutate(period = "future") %>%
    left_join(input_data$spec_dat, by = c("name" = "name")) %>%
    mutate(name = gsub("_future$", "", name))
  
  data_filtered_current <- input_data$puvspr_dat_current %>%
      filter(!species %in% variables_short_percentile | grepl("_original", species)) %>%
      mutate(species = ifelse(grepl(paste0(variables_short_percentile, collapse = "|"), species) & grepl("_original", species),
                              sub("_original", "", species), species)) %>%
      rename(WP4 = id_old) %>%
      rename(name = species) %>%
      select(WP4, name, amount, campaign) %>%
      mutate(period = "current") %>%
      left_join(input_data$spec_dat, by = c("name" = "name")) 

  data_species <- rbind(data_filtered_future, data_filtered_current)
  
  # Group by WP4, campaign, name, and period, then summarize the amount
  df_summarized <- data_species %>%
    select(WP4, name, amount, campaign, period) %>%
    group_by(WP4, campaign, name, period) %>%
    summarize(total_amount = sum(amount)) 
  
  df_wide <- df_summarized %>%
    tidyr::pivot_wider(names_from = period, values_from = total_amount, names_prefix = "period_") %>%
    mutate(difference = period_future - period_current) %>%
    group_by(WP4, name) %>%
    summarize(total_difference = mean(difference), .groups = 'drop') 
    
  # Spread the periods into separate columns to calculate the difference
  df_wide <- df_wide %>%
    group_by(WP4) %>%
    summarize(total_difference_all = sum(total_difference, na.rm = T), .groups = 'drop')

  return(df_wide)
}


update_map_opt <- function(drn, sol, var){
  
  shape <- get_shape(drn)
  cons_rest <- calculate_cons_rest(drn, var)
  
  data_filtered <- sol %>%
    select(id, solution_1, id_old, campaign) %>%
    rename(WP4 = id_old)

  data_filtered <- data_filtered %>%
      group_by(WP4) %>%
      summarise(solution_1 = sum(solution_1)) %>%
      select(WP4, solution_1) %>%
      rename(ID = WP4)
  
  
  int_curr <- read.table(paste0("data/data/regimes/", drn, "/current/intermit.csv"), sep = ",", header = TRUE)
  id_col_reg = which(colnames(int_curr) == "Regime")
  colnames(int_curr)[id_col_reg] = "RegimeC"
  
  int_future <- read.table(paste0("data/data/regimes/", drn, "/future/intermit.csv"), sep = ",", header = TRUE)
  id_col_reg = which(colnames(int_future) == "Regime")
  colnames(int_future)[id_col_reg] = "RegimeF"

  b_int <- left_join(data_filtered, int_curr, by = c("ID" = "WP4")) #join intermittence with freq 
  b_int <- left_join(b_int, int_future, by = c("ID" = "WP4")) #join intermittence with freq 
  
  shape_var <- terra::merge(shape, b_int, by.x = "ID", by.y = "ID", all.x=TRUE)
  
  shape_var$inter = ifelse(shape_var$RegimeC == "perennial" & shape_var$RegimeF == "perennial", 1, 
                           ifelse(shape_var$RegimeC == "perennial" & shape_var$RegimeF != "perennial", "1,5",
                           10))
  
  shape_var$regine_rea = ifelse(shape_var$RegimeC == "perennial" & shape_var$RegimeF == "perennial", "perennial", 
                           ifelse(shape_var$RegimeC == "perennial" & shape_var$RegimeF != "perennial", "perennial -> intermittent",
                                  "intermittent"))
  
  shape_var <- terra::merge(shape_var, cons_rest, by.x = "ID", by.y = "WP4", all.x=TRUE)
  
  ##palette
  #levels(shape_var$solution_1) <- c(0,1)

  # Define two palettes
  shape_var$manage <- ifelse(shape_var$total_difference_all*shape_var$solution_1 > 0, "Conservation", 
                             ifelse(shape_var$total_difference_all*shape_var$solution_1 < 0, "Restoration", "No recomendation"))
  
  
  # Subset the data using terra
  conservation_data <- subset(shape_var, shape_var$manage == "Conservation")
  restauration_data <- subset(shape_var, shape_var$manage == "Restoration")
  Norecomendation_data <- subset(shape_var, shape_var$manage == "No recomendation")
  
  pal_cons <- colorFactor(palette="YlGn", domain=conservation_data$solution_1, na.color="orange")
  pal_rec <- colorFactor(palette="Oranges", domain=restauration_data$solution_1, na.color="orange")
  
  
  pal_cons_modified <- ifelse(conservation_data$manage == "Conservation", pal_cons(conservation_data$solution_1), "gray30")
  pal_rec_modified <- ifelse(restauration_data$manage == "Restoration", pal_rec(restauration_data$solution_1), "transparent")
 
  title_legend <- ""
  

  popup_map_rec <- sprintf(
    "<strong>ID %s</strong><br/>
         Value: %g <br/>
         Type: %s <br/>
         Potential for: %s <br/>",
    restauration_data$ID, 
    restauration_data$solution_1,
    restauration_data$regine_rea,
    restauration_data$manage) %>% 
    lapply(htmltools::HTML)
  
  popup_map_cons <- sprintf(
    "<strong>ID %s</strong><br/>
         Value: %g <br/>
         Type: %s <br/>
         Potential for: %s <br/>",
    conservation_data$ID, 
    conservation_data$solution_1,
    conservation_data$regine_rea,
    conservation_data$manage) %>% 
    lapply(htmltools::HTML)
  
  popup_map_no_recomendation <- sprintf(
    "<strong>ID %s</strong><br/>
         Value: %g <br/>
         Type: %s <br/>
         Potential for: %s <br/>",
    Norecomendation_data$ID, 
    Norecomendation_data$solution_1,
    Norecomendation_data$regine_rea,
    Norecomendation_data$manage) %>% 
    lapply(htmltools::HTML)


  leafletProxy("map_opt") %>% 
    clearControls() %>%
    addPolylines(data = Norecomendation_data,
                 group = "Norecomendation",
                 weight = 3,
                 color = "black",
                 fill = FALSE,
                 opacity = 1,
                 popup = popup_map_no_recomendation, # Popup for conservation data
                 dashArray = ~inter,
                 layerId = ~ID,
                 highlightOptions = highlightOptions(
                   weight = 5,
                   bringToFront = TRUE,
                   sendToBack = TRUE)
    ) %>%
    addPolylines(data = conservation_data,
                 group = "Conservation",
                 weight = 3,
                 color = pal_cons_modified,
                 fill = FALSE,
                 opacity = 1,
                 popup = popup_map_cons, # Popup for conservation data
                 dashArray = ~inter,
                 layerId = ~ID,
                 highlightOptions = highlightOptions(
                   weight = 5,
                   bringToFront = TRUE,
                   sendToBack = TRUE)
    ) %>%
    # Add Custom Legend for Line Patterns
    addControl(html = "<div style='background: white; padding: 5px;'>
                        <div><svg height='10' width='20'><line x1='0' y1='5' x2='20' y2='5' style='stroke:black; stroke-width:2;' /></svg> Perennial</div>
                        <div><svg height='10' width='20'><line x1='0' y1='5' x2='20' y2='5' style='stroke:black; stroke-width:2; stroke-dasharray:1,5;' /></svg> Perennial to Intermittent</div>
                        <div><svg height='10' width='20'><line x1='0' y1='5' x2='20' y2='5' style='stroke:black; stroke-width:2; stroke-dasharray:5,5;' /></svg> Intermittent</div>
                      </div>", 
               position = "topright") %>%
    addLegend(group = "Conservation",
              pal = pal_cons,
              title = "Conservation (frec)",
              values = conservation_data$solution_1,
              opacity = 1,
              position = "topright") %>%
    addPolylines(data = restauration_data,
                 group = "Restoration",
                 weight = 3,
                 color = pal_rec_modified,
                 fill = FALSE,
                 opacity = 1,
                 popup = popup_map_rec, # Popup for conservation data
                 dashArray = ~inter,
                 layerId = ~ID,
                 highlightOptions = highlightOptions(
                   weight = 5,
                   bringToFront = TRUE,
                   sendToBack = TRUE)
    ) %>%
    addLegend(group = "Restoration",
              pal = pal_rec,
              title = "Restoration (frec)",
              values = restauration_data$solution_1,
              opacity = 1,
              position = "topright") %>%
    # # Add Custom Legend for Line Patterns
    # addControl(html = "<div style='background: white; padding: 5px;'>
    #                     <div><svg height='10' width='20'><line x1='0' y1='5' x2='20' y2='5' style='stroke:black; stroke-width:2;' /></svg> Perennial</div>
    #                     <div><svg height='10' width='20'><line x1='0' y1='5' x2='20' y2='5' style='stroke:black; stroke-width:2; stroke-dasharray:1,5;' /></svg> Perennial to Intermittent</div>
    #                     <div><svg height='10' width='20'><line x1='0' y1='5' x2='20' y2='5' style='stroke:black; stroke-width:2; stroke-dasharray:5,5;' /></svg> Intermittent</div>
    #                   </div>", 
    #            position = "topright") %>%
      addLayersControl(
        #overlayGroups = c("Conservation", "Restauration"),
        baseGroups = c("CartoDB", "Elevation", "Satellite","Without background"),
        options = layersControlOptions(collapsed = TRUE)
      )
}

update_map_opt_ggplot <- function(drn, sol, var){
  
  shape <- get_shape(drn)
  cons_rest <- calculate_cons_rest(drn, var)
  
  data_filtered <- sol %>%
    select(id, solution_1, id_old, campaign) %>%
    rename(WP4 = id_old)
  
  data_filtered <- data_filtered %>%
    group_by(WP4) %>%
    summarise(solution_1 = sum(solution_1)) %>%
    select(WP4, solution_1) %>%
    rename(ID = WP4)
  
  
  int_curr <- read.table(paste0("data/data/regimes/", drn, "/current/intermit.csv"), sep = ",", header = TRUE)
  id_col_reg = which(colnames(int_curr) == "Regime")
  colnames(int_curr)[id_col_reg] = "RegimeC"
  
  int_future <- read.table(paste0("data/data/regimes/", drn, "/future/intermit.csv"), sep = ",", header = TRUE)
  id_col_reg = which(colnames(int_future) == "Regime")
  colnames(int_future)[id_col_reg] = "RegimeF"
  
  b_int <- left_join(data_filtered, int_curr, by = c("ID" = "WP4")) #join intermittence with freq 
  b_int <- left_join(b_int, int_future, by = c("ID" = "WP4")) #join intermittence with freq 
  
  shape_var <- terra::merge(shape, b_int, by.x = "ID", by.y = "ID", all.x=TRUE)
  
  shape_var$inter = ifelse(shape_var$RegimeC == "perennial" & shape_var$RegimeF == "perennial", 1, 
                           ifelse(shape_var$RegimeC == "perennial" & shape_var$RegimeF != "perennial", "1,5",
                                  10))
  
  shape_var$regine_rea = ifelse(shape_var$RegimeC == "perennial" & shape_var$RegimeF == "perennial", "perennial", 
                                ifelse(shape_var$RegimeC == "perennial" & shape_var$RegimeF != "perennial", "perennial -> intermittent",
                                       "intermittent"))
  
  shape_var <- terra::merge(shape_var, cons_rest, by.x = "ID", by.y = "WP4", all.x=TRUE)
  
  ##palette
  #levels(shape_var$solution_1) <- c(0,1)
  
  # Define two palettes
  shape_var$manage <- ifelse(shape_var$total_difference_all*shape_var$solution_1 > 0, "Conservation", 
                             ifelse(shape_var$total_difference_all*shape_var$solution_1 < 0, "Restoration", "No recomendation"))
  
  shape_var = st_as_sf(shape_var)
  
  p1 <- ggplot(shape_var) +
    geom_sf(aes(color = manage, fill = manage, linetype = regine_rea), size = 0.5) +
    scale_color_manual(values = c("Conservation" = "green", "Restoration" = "orange", "No recommendation" = "black")) +
    scale_fill_manual(values = c("Conservation" = "green", "Restoration" = "orange", "No recommendation" = "black")) +
    scale_linetype_manual(values = c("perennial" = "solid", 
                                     "intermittent" = "dashed", 
                                     "perennial -> intermittent" = "dotted")) + 
    labs(title = "Map of Management Recommendations",
         color = "Management Type",
         fill = "Management Type",
         linetype = "Regime Type") +
    theme_minimal() +
    theme(legend.position = "right") +
    scale_size_continuous(range = c(0.5, 5)) +
    guides(size = guide_legend(title = "Intermittence"))
  
  return(p1)
}

# Network yearly time series (observed period)
figure_targets_reached <- function(drn, var, id){
  
  input_data <- calculate_opt_inputs(drn, var)
  
  sol_df <- last_sol %>%
    select(id, solution_1, id_old, campaign, period) %>%
    rename(WP4 = id_old) %>%
    group_by(WP4) %>%
    summarise(solution_1 = sum(solution_1)) %>%
    select(WP4, solution_1) %>%
    rename(ID = WP4)
  
  result <- sol_df %>%
    filter(ID == as.numeric(id)) %>%
    pull(solution_1) > 0
  
  if(result){
    data_filtered_future <- input_data$puvspr_dat_future_sd %>%
      rename(WP4 = id_old) %>%
      select(WP4, species, amount, campaign) %>%
      filter((!species %in% paste0(variables_short_percentile, "_future") | grepl("_original_", species)) & !grepl("_sd", species)) %>%
      mutate(species = ifelse(grepl(paste0(variables_short_percentile, collapse = "|"), species) & grepl("_original_", species),
                              sub("_original_", "_", species), species)) %>%
      mutate(period = "future")
    
    data_filtered_future_sd <- input_data$puvspr_dat_future_sd %>%
      rename(WP4 = id_old) %>%
      select(WP4, species, amount, campaign) %>%
      filter((!species %in% paste0(variables_short_percentile, "_future") | grepl("_original_", species)) & grepl("_sd", species)) %>%
      mutate(species = ifelse(grepl(paste0(variables_short_percentile, collapse = "|"), species) & grepl("_original_", species),
                              sub("_original_", "_", species), species)) %>%
      mutate(species = sub("_sd", "", species)) %>%
      select(WP4, species, campaign, amount) %>%
      rename(amount_sd = amount)
    
    data_filtered_future <- data_filtered_future %>%
      left_join(data_filtered_future_sd, by = c("WP4" = "WP4", "campaign" = "campaign", "species" = "species")) %>%
      mutate(amount_sd = tidyr::replace_na(amount_sd, 0))
    
    
    data_filtered_current <- input_data$puvspr_dat_current %>%
      rename(WP4 = id_old) %>%
      select(WP4, species, amount, campaign) %>%
      filter(!species %in% variables_short_percentile | grepl("_original", species)) %>%
      mutate(species = ifelse(grepl(paste0(variables_short_percentile, collapse = "|"), species) & grepl("_original", species),
                              sub("_original", "", species), species)) %>%
      mutate(period = "current")
    
    #Calculate % proportion by year
    #current
    total_sum_species_current  <- data_filtered_current %>%
      group_by(species, period) %>%
      summarise(total_sum_species  = sum(amount))
    
    sum_species_by_wp4_current  <- data_filtered_current %>%
      filter(WP4 == id) %>%
      group_by(species, WP4) %>%
      summarise(sum_species_wp4  = sum(amount))
    
    proportion_data_current <- sum_species_by_wp4_current %>%
      left_join(total_sum_species_current, by = "species") 
    
    #future
    total_sum_species_future  <- data_filtered_future %>%
      select(WP4, species, amount, campaign, period) %>%
      group_by(species, period) %>%
      summarise(total_sum_species  = sum(amount)) 
    
    sum_species_by_wp4_future  <- data_filtered_future %>%
      filter(WP4 == id) %>%
      group_by(species, WP4) %>%
      summarise(sum_species_wp4  = sum(amount),
                sum_species_wp4_sd  = sum(amount_sd))
    
    proportion_data_future <- sum_species_by_wp4_future %>%
      left_join(total_sum_species_future, by = "species")
    
    prop_data <- rbind(proportion_data_current, proportion_data_future)
    
    prop_data <- prop_data %>%
      left_join(input_data$spec_dat, by = c("species" = "name")) %>%
      mutate(species = gsub("_future$", "", species)) %>%
      mutate(contribution = sum_species_wp4/(total_sum_species*prop)) %>%
      mutate(contribution_sd = sum_species_wp4_sd/(total_sum_species*prop))
    
    variables <- data.frame(
      species = variables_short,
      long_name = variables_long
    )
    
    # Perform the left join
    prop_data <- prop_data %>%
      left_join(variables, by = "species")
    
    # long_title = paste0("Comparation of the temporal trend of ",var_long, " between ", "2020 and ", period)
    # wrapped_title <- str_wrap(long_title, width = max_width)
    # dom_summary = get_difference_domain_bioinformation(var, drn)
    
    # p1 <- ggplot(prop_data, aes(x = reorder(factor(long_name), contribution), y = contribution, 
    #                             fill = factor(period))) +
    #   geom_bar(stat = "identity", position = "dodge") +
    #   geom_errorbar(data = prop_data %>% filter(period == "future"),
    #                 aes(ymin = contribution - contribution_sd, ymax = contribution + contribution_sd),
    #                 width = 0.2, position = position_dodge(0.9)) +
    #   labs(
    #     title = "Contribution of the river section to the target",
    #     x = "Features",
    #     y = "% of Contribution",
    #     fill = ""
    #   ) +
    #   coord_flip() +
    #   theme_minimal() +
    #   scale_fill_manual(values = c("black", "forestgreen"))+
    #   theme_bw()+
    #   scale_y_continuous(labels = scales::percent_format(scale = 100)) +
    #   theme(plot.title = element_text(size = 12, hjust = 0.5, lineheight = 0.5))
    
    # Supongamos que prop_data es tu dataframe
    # Aquí está el código simplificado para agregar barras de error
    
    p1 <- ggplot(prop_data, aes(x = reorder(factor(long_name), contribution), y = contribution, 
                                fill = factor(period))) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
      geom_pointrange(aes(ymin = contribution - contribution_sd, ymax = contribution + contribution_sd),
                      position = position_dodge(width = 0.9), 
                      size = 0.5) +
      labs(
        title = "Contribution of the river section to the target",
        x = "",
        y = "% of Contribution",
        fill = ""
      ) +
      coord_flip() +
      theme_bw() +
      scale_fill_manual(values = c("black", "forestgreen")) +
      scale_y_continuous(labels = scales::percent_format(scale = 100)) +
      theme(plot.title = element_text(size = 12, hjust = 0.5, lineheight = 0.5))
  
  }else{
    p1 <- NULL   
  }
  
  return(p1)
}



# Network yearly time series (observed period)
figure_inter_reached <- function(drn){
  
  shape <- get_shape(drn)
  
  data_filtered <- last_sol %>%
    select(id, solution_1, id_old, campaign, period) %>%
    rename(WP4 = id_old)

  data_filtered <- data_filtered %>%
    group_by(WP4, period) %>%
    summarise(solution_1 = sum(solution_1)) %>%
    select(WP4, solution_1, period) %>%
    rename(ID = WP4) %>%
    tidyr::pivot_wider(names_from = period, values_from = solution_1)
  
  int_curr <- read.table(paste0("data/data/regimes/", drn, "/current/intermit.csv"), sep = ",", header = TRUE)
  id_col_reg = which(colnames(int_curr) == "Regime")
  colnames(int_curr)[id_col_reg] = "RegimeC"
  
  int_future <- read.table(paste0("data/data/regimes/", drn, "/future/intermit.csv"), sep = ",", header = TRUE)
  id_col_reg = which(colnames(int_future) == "Regime")
  colnames(int_future)[id_col_reg] = "RegimeF"
  
  b_int <- left_join(data_filtered, int_curr, by = c("ID" = "WP4")) #join intermittence with freq 
  b_int <- left_join(b_int, int_future, by = c("ID" = "WP4")) #join intermittence with freq 
  
  data_filtered <- b_int %>%
    #mutate(Regime = ifelse(regimecurrent == regime2100 & `2021`+`2100` > 0, regime2021, 0))
    mutate(Regime = ifelse(current + future > 0, 1, 0)) %>%
    mutate(inter = ifelse(RegimeC == "perennial" & RegimeF == "perennial", "perennial", 
                          ifelse(RegimeC == "perennial" & RegimeF != "perennial", "perennial to intermittent",
                                        "intermittent")))
          
  b_int <- data_filtered %>%
    group_by(inter) %>%
    summarize(total_solution_1 = sum(Regime))
  
  p1 <- plot_ly(type='pie', 
                labels=b_int$inter, 
                values=b_int$total_solution_1, 
                textinfo='percent',
                insidetextorientation='radial', 
                width = 350, 
                height = 300,
                marker = list(colors = c("#D3D3D3", "#808080"))  # Shades of gray
  ) %>%
    layout(
      title = list(
        text = "% Regime",
        font = list(size = 16), 
        x = 0.5,  # Centra el título
        xanchor = "center"
      ),
      margin = list(
        l = 0,  # left margin
        r = 0,  # right margin
        b = 0,  # bottom margin
        t = 50   # top margin
      ),
    showlegend = TRUE 
    )
  
  return(p1)
}


# Network yearly time series (observed period)
figure_inter_reached_ggplot <- function(drn){
  
  shape <- get_shape(drn)
  
  data_filtered <- last_sol %>%
    select(id, solution_1, id_old, campaign, period) %>%
    rename(WP4 = id_old)
  
  data_filtered <- data_filtered %>%
    group_by(WP4, period) %>%
    summarise(solution_1 = sum(solution_1)) %>%
    select(WP4, solution_1, period) %>%
    rename(ID = WP4) %>%
    tidyr::pivot_wider(names_from = period, values_from = solution_1)
  
  int_curr <- read.table(paste0("data/data/regimes/", drn, "/current/intermit.csv"), sep = ",", header = TRUE)
  id_col_reg = which(colnames(int_curr) == "Regime")
  colnames(int_curr)[id_col_reg] = "RegimeC"
  
  int_future <- read.table(paste0("data/data/regimes/", drn, "/future/intermit.csv"), sep = ",", header = TRUE)
  id_col_reg = which(colnames(int_future) == "Regime")
  colnames(int_future)[id_col_reg] = "RegimeF"
  
  b_int <- left_join(data_filtered, int_curr, by = c("ID" = "WP4")) #join intermittence with freq 
  b_int <- left_join(b_int, int_future, by = c("ID" = "WP4")) #join intermittence with freq 
  
  data_filtered <- b_int %>%
    #mutate(Regime = ifelse(regimecurrent == regime2100 & `2021`+`2100` > 0, regime2021, 0))
    mutate(Regime = ifelse(current + future > 0, 1, 0)) %>%
    mutate(inter = ifelse(RegimeC == "perennial" & RegimeF == "perennial", "perennial", 
                          ifelse(RegimeC == "perennial" & RegimeF != "perennial", "perennial to intermittent",
                                        "intermittent")))
  
  b_int <- data_filtered %>%
    group_by(inter) %>%
    summarize(total_solution_1 = sum(Regime))
  
  p1 <- ggplot(b_int, aes(x = "", y = total_solution_1, fill = inter)) +
    geom_bar(stat = "identity", width = 1) +  # Gráfico de barras, con width = 1
    coord_polar("y") +  # Transformar a gráfico de pastel
    scale_fill_manual(values = c("#D3D3D3", "#808080", "blue", "red")) +  # Colores personalizados
    labs(title = "% Regime", fill = "Regime") +  # Título y leyenda
    theme_minimal() +  # Tema minimalista
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 16),  # Centrar y ajustar el tamaño del título
      legend.position = "right"  # Posición de la leyenda
    )
  
  return(p1)
}
