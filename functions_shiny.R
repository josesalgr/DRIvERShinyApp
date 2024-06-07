
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
  path_shp <- paste0("data/shp/", shpdf, "/small_river_network.shp")
  
  # Read and preprocess the shapefile
  shape <- terra::vect(path_shp)
  shape <- project(shape,  crs("+proj=longlat +datum=WGS84 +no_defs"))
  shape <- terra::aggregate(shape, by = "ID", count=FALSE, overwrite=TRUE, dissolve=FALSE)
  shape$ID <- as.character(shape$ID)
  
  #To simplify the geom if that is the case
  #shape <- simplifyGeom(shape, preserveTopology = TRUE)
  
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
  path_shp <- paste0("data/shp/", shpdf, "/small_river_network.shp")
  
  # Read and preprocess the shapefile
  shape <- terra::vect(path_shp)
  shape <- project(shape,  crs("+proj=longlat +datum=WGS84 +no_defs"))
  shape <- terra::aggregate(shape, by = "ID", count=FALSE, overwrite=TRUE, dissolve=FALSE)
  shape$ID <- as.character(shape$ID)
  
  #To simplify the geom if that is the case
  #shape <- simplifyGeom(shape, preserveTopology = TRUE)
  
  validate(need("ID" %in% names(shape), "The shapefile must be a 'id' column"))
  
  # Assuming the ID attribute is named "ID," replace it with the actual name in your shapefile
  missing_ids <- is.na(shape$ID)
  shape <- shape[!missing_ids, ]
  
  # Var data
  data <- get_bioinformation(var, drn, period)
  
  data_filtered <- data %>%
    filter(campaign == camp) %>%
    select(WP4, var) #%>%
  
  #distinct(ID, .keep_all = TRUE)
  
  shape_var <- terra::merge(shape, data_filtered, by.x = "ID", by.y = "WP4", all.x=TRUE)
  
  return(shape_var)
}


get_bioinformation <- function(var, drn, period){
  
  #variables
  #"rich", "simp", "FD", "FR", "alpha", "beta", "gamma", "dem", "co2", "dr_lr", "dr_ss", "er_rip", 
  #"er_sl", "flo_rip", "flo_sl", "th_reg"
  
  # read data
  # IF biodiversity variable
  if(var %in% c("rich", "simp", "FD", "FR")){
    
    file_name = paste0("data/data/biodiversity/", drn, "/", period, "/bio_data.csv")
  }
  else if(var %in% c("temp_beta")){
    file_name = paste0("data/data/biodiversity/", drn, "/", period, "/temp_beta_data.csv")
  }
  else if(var %in% c("alpha", "beta", "gamma")){
    
    file_name = paste0("data/data/biodiversity/", drn, "/", period, "/biobeta_data.csv")
  }
  else if(var %in% c("dem", "co2")){
    
    file_name = paste0("data/data/ecological_functions/", drn, "/", period, "/", var, "_data.csv")
  }
  else if(var %in% c("dr_lr", "dr_ss", "er_rip", "er_sl", "flo_rip", "flo_sl", "th_reg")){
    
    file_name = paste0("data/data/ecosystem_services/", drn, "/", period, "/", var, ".csv")
    
  }
  
  data_p <- read.table(file_name, sep = ",", header = TRUE)
  data_p$DRN = drn
  
  if(var == "co2"){
    data_p[[var]] <- data_p[[var]]/1000
  }
  
  return(data_p)
}

get_domain_bioinformation <- function(var, drn){
  
  #variables
  #"rich", "simp", "FD", "FR", "alpha", "beta", "gamma", "dem", "co2", "dr_lr", "dr_ss", "er_rip", 
  #"er_sl", "flo_rip", "flo_sl", "th_reg"
  
  # read data
  # IF biodiversity variable
  if(var %in% c("rich", "simp", "FD", "FR")){
    
    file_name_per1 = paste0("data/data/biodiversity/", drn, "/", scale_list_short[1], "/bio_data.csv")
    file_name_per2 = paste0("data/data/biodiversity/", drn, "/", scale_list_short[2], "/bio_data.csv")
  }
  else if(var %in% c("temp_beta")){
    file_name_per1 = paste0("data/data/biodiversity/", drn, "/", scale_list_short[1], "/temp_beta_data.csv")
    file_name_per2 = paste0("data/data/biodiversity/", drn, "/", scale_list_short[2], "/temp_beta_data.csv")
  }
  else if(var %in% c("alpha", "beta", "gamma")){
    
    file_name_per1 = paste0("data/data/biodiversity/", drn, "/", scale_list_short[1], "/biobeta_data.csv")
    file_name_per2 = paste0("data/data/biodiversity/", drn, "/", scale_list_short[2], "/biobeta_data.csv")
  }
  else if(var %in% c("dem", "co2")){
    
    file_name_per1 = paste0("data/data/ecological_functions/", drn, "/", scale_list_short[1], "/", var, "_data.csv")
    file_name_per2 = paste0("data/data/ecological_functions/", drn, "/", scale_list_short[2], "/", var, "_data.csv")
  }
  else if(var %in% c("dr_lr", "dr_ss", "er_rip", "er_sl", "flo_rip", "flo_sl", "th_reg")){
    
    file_name_per1 = paste0("data/data/ecosystem_services/", drn, "/", scale_list_short[1], "/", var, ".csv")
    file_name_per2 = paste0("data/data/ecosystem_services/", drn, "/", scale_list_short[2], "/", var, ".csv")
    
  }

  data_per1 <- read.table(file_name_per1, sep = ",", header = TRUE)
  data_per2 <- read.table(file_name_per2, sep = ",", header = TRUE)

  data = rbind(data_per1, data_per2)
  id_var = which(colnames(data) == var)
  
  if(var == "co2"){
    data[[var]] <- data[[var]]/1000
  }
  
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
  if(var %in% c("rich", "simp", "FD", "FR")){
    
    file_name_per1 = paste0("data/data/biodiversity/", drn, "/", scale_list_short[1], "/bio_data.csv")
    file_name_per2 = paste0("data/data/biodiversity/", drn, "/", scale_list_short[2], "/bio_data.csv")
  }
  else if(var %in% c("temp_beta")){
    file_name_per1 = paste0("data/data/biodiversity/", drn, "/", scale_list_short[1], "/temp_beta_data.csv")
    file_name_per2 = paste0("data/data/biodiversity/", drn, "/", scale_list_short[2], "/temp_beta_data.csv")
  }
  else if(var %in% c("alpha", "beta", "gamma")){
    
    file_name_per1 = paste0("data/data/biodiversity/", drn, "/", scale_list_short[1], "/biobeta_data.csv")
    file_name_per2 = paste0("data/data/biodiversity/", drn, "/", scale_list_short[2], "/biobeta_data.csv")
  }
  else if(var %in% c("dem", "co2")){
    
    file_name_per1 = paste0("data/data/ecological_functions/", drn, "/", scale_list_short[1], "/", var, "_data.csv")
    file_name_per2 = paste0("data/data/ecological_functions/", drn, "/", scale_list_short[2], "/", var, "_data.csv")
  }
  else if(var %in% c("dr_lr", "dr_ss", "er_rip", "er_sl", "flo_rip", "flo_sl", "th_reg")){
    
    file_name_per1 = paste0("data/data/ecosystem_services/", drn, "/", scale_list_short[1], "/", var, ".csv")
    file_name_per2 = paste0("data/data/ecosystem_services/", drn, "/", scale_list_short[2], "/", var, ".csv")
    
  }
  
  data_per1 <- read.table(file_name_per1, sep = ",", header = TRUE)
  data_per2 <- read.table(file_name_per2, sep = ",", header = TRUE)
  
  if(var == "co2"){
    data_per1[[var]] <- data_per1[[var]]/1000
    data_per2[[var]] <- data_per2[[var]]/1000
  }
  
  data_per1 <- data_per1 %>%
    select("WP4", "campaign", var)
  
  data_per2 <- data_per2 %>%
    select("WP4", "campaign", var)
  

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
    y_units = "kgC-co2/m2"
  }
  else{
    y_units = "Value"
  }

  
  if(compare){
    data_ini <- get_bioinformation(var, drn, "2021")
    data_final <- get_bioinformation(var, drn, period)
    
    data_ini_filtered <- data_ini %>%
      filter(WP4 == id) %>%
      select(campaign, var)
    
    data_ini_filtered$period = "2021"
    
    data_final_filtered <- data_final %>%
      filter(WP4 == id) %>%
      select(campaign, var)
    
    data_final_filtered$period = period
    
    data_filtered = rbind(data_ini_filtered, data_final_filtered)
    
    long_title = paste0("Comparation of the temporal trend of ",var_long, " between ", "2020 and ", period)
    wrapped_title <- str_wrap(long_title, width = max_width)
    dom_summary = get_difference_domain_bioinformation(var, drn)
    
    p1 <- ggplot(data_filtered, aes(x=campaign, y=data_filtered[,var], group = period, color = period))+
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
  else{
    data <- get_bioinformation(var, drn, period)
    
    data_filtered <- data %>%
      filter(WP4 == id)
    
    long_title = paste0("Temporal trend of ",var_long, " in ", period)
    wrapped_title <- str_wrap(long_title, width = max_width)
    dom_summary = get_domain_bioinformation(var, drn)
    
    p1 <- ggplot(data_filtered, aes(x=campaign, y=data_filtered[,var]))+
      geom_line()+
      geom_point(aes(colour = ifelse(campaign == camp, TRUE, FALSE)), size = 2, show.legend = FALSE) +
      scale_color_manual(values = c("black", "forestgreen"))+
      theme_bw()+
      ylab(y_units)+xlab("Campaign")+
      ggtitle(wrapped_title)+
      scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6), labels = campaign_list) +
      scale_y_continuous(limits=c(dom_summary$min_var, dom_summary$max_var)) +
      theme(legend.position = "none",
            plot.title = element_text(size = 10, hjust = 0.5, lineheight = 0.5))
  }

  return(p1)
}

# Percentiles in a map
figure_percentile <- function(drn, var, drn_country, camp, per){
  
  shape <- get_shape(drn)
  data <- get_bioinformation(var, drn_country)
  
  data_filtered <- data %>%
    filter(campaign == camp) %>%
    select(WP4, var)
  
  shape_var <- terra::merge(shape, data_filtered, by.x = "ID", by.y = "WP4", all.x=TRUE)
  
  # Calculate the xth percentile of the 'var' column
  percentile <- quantile(shape_var[[var]][,1], probs = per, na.rm = TRUE)
  
  # Create a new column indicating whether each observation is below or above the xth percentile
  shape_var$below_percentile <- ifelse(shape_var[[var]][,1] < percentile, "Below", "Above")
  
  # Convert the data to sf object
  sf_obj <- st_as_sf(shape_var)
  
  # Plot the lines with different colors based on whether they are below or above the xth percentile
  p1 <- ggplot(sf_obj, aes(color = below_percentile)) + 
    geom_sf_interactive(linewidth = 1.2) + 
    scale_color_manual(values = c("Below" = "gray", "Above" = "forestgreen")) + 
    theme_bw() +
    theme(legend.position = "none",
          plot.title = element_text(size = 20)) +
    ggtitle(paste0("Percentile", per*100))
  
  return(p1)
}


# Update the map using the information of Indicator
update_map_data <- function(drn, var, camp, period, compare){
  
  shape <- get_shape(drn)
  
  if(compare){
    data_ini <- get_bioinformation(var, drn, "2021")
    data_final <- get_bioinformation(var, drn, period)

    data_ini_filtered <- data_ini %>%
      filter(campaign == camp) %>%
      select(WP4, var)
    
    data_final_filtered <- data_final %>%
      filter(campaign == camp) %>%
      select(WP4, var)
    
    data_filtered = data_final_filtered %>%
      left_join(data_ini_filtered, by = "WP4")
    
    data_filtered = data_filtered %>%
      mutate(diff = data_filtered[,2] - data_filtered[,3])

    shape_var <- terra::merge(shape, data_filtered, by.x = "ID", by.y = "WP4", all.x=TRUE)
    shape_var$inter = ifelse(shape_var$regine_rea == "perennial", 1, 5)
    dom_summary = get_difference_domain_bioinformation(var, drn)
    
    pal <- colorNumeric(palette="RdYlBu", domain = c(dom_summary$min_var, dom_summary$max_var), na.color="orange")


    title_legend <- ""
    
    popup_map <- sprintf(
      "<strong>ID %s</strong><br/>
         Difference: %g <br/>
         Type: %s <br/>",
      shape_var$ID, 
      shape_var$diff,
      shape_var$regine_rea) %>% 
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
                position = "topright")
    
    
    
  }
  else{
    data <- get_bioinformation(var, drn, period)
    
    data_filtered <- data %>%
      filter(campaign == camp) %>%
      select(WP4, var) #%>%
    #distinct(ID, .keep_all = TRUE)
    
    
    shape_var <- terra::merge(shape, data_filtered, by.x = "ID", by.y = "WP4", all.x=TRUE)
    
    shape_var$inter = ifelse(shape_var$regine_rea == "perennial", 1, 5)
    
    dom_summary = get_domain_bioinformation(var, drn)
    
    pal <- colorNumeric(palette="BrBG", domain=c(dom_summary$min_var, dom_summary$max_var), na.color="orange")
    
    title_legend <- ""
    
    popup_map <- sprintf(
      "<strong>ID %s</strong><br/>
         Value: %g <br/>
         Type: %s <br/>",
      shape_var$ID, 
      shape_var[[var]][,1],
      shape_var$regine_rea) %>% 
      lapply(htmltools::HTML)
    
    
    leafletProxy("map_data", data = shape_var) %>%
      clearControls() %>%
      addPolylines(group = "Base",
                   weight = 3, 
                   color = ~pal(shape_var[[var]][,1]),
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
                values = shape_var[[var]][,1],
                labFormat = labelFormat(digits = 5),
                opacity = 1,
                bins = 10,
                position = "topright")
  }
  
  # shape_var <- terra::merge(shape, data_filtered, by.x = "ID", by.y = "WP4", all.x=TRUE)
  # 
  # 
  # ##palette
  # #pal <- colorNumeric(palette="Blues", domain=shape_var[[var]][,1], na.color="orange")
  # 
  # dom_summary = get_domain_bioinformation(var, drn)
  # 
  # #pal <- colorNumeric(palette="RdYlBu", domain=c(dom_summary$min_var, dom_summary$max_var), na.color="orange")
  # pal <- colorNumeric(palette="BrBG", domain=c(dom_summary$min_var, dom_summary$max_var), na.color="orange")
 
}


# The shape clicked is valid?
is_id <- function(drn, var, camp, id, period){
  
  shape <- get_shape(drn)
  data <- get_bioinformation(var, drn, period)
  
  data_filtered <- data %>%
    filter(campaign == camp) %>%
    select(WP4, var) %>%
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
  #2021-------------------------------------------------------------------------
  ##############################################################################
  period = "2021"
  
  
  #pu dat-----------------------------------------------------------------------
  pu_dat = as.data.frame(shape)
  pu_dat <- pu_dat[,c("ID", "mean_AREA_SQKM")]
  colnames(pu_dat) <- c("id", "cost")
  pu_dat$id <- as.numeric(pu_dat$id)
  pu_dat$cost <- 1
  pu_dat$status <- 0
  pu_dat$id_old <- pu_dat$id
  
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
        select(WP4, id_old, campaign, var_short) %>%
        mutate(!!sym("WP4") := WP4 + campaign*multiplier) %>%
        select(WP4, id_old, campaign, var_short)
      
      if(var_short %in% c("rich", "FD", "FR", "simp", "temp_beta", "alpha", "beta", "gamma", "dem")){
        
        quantiles_var <- quantile(data_filtered[[var_short]], probs = c(0.5, 0.75, 0.90), na.rm = TRUE)

        data_filtered <- data_filtered %>%
          mutate(!!var_short := ifelse(data_filtered[[var_short]] > quantiles_var[1], 1, 0))
        
      }
    }
    else{
      data_aux <- get_bioinformation(var_short, drn, period)

      data_aux$id_old <- data_aux$WP4
      
      data_aux_filtered <- data_aux %>%
        #filter(campaign == 6) %>%
        select(WP4, id_old, campaign, var_short) %>%
        mutate(!!sym("WP4") := WP4 + campaign*multiplier) %>%
        select(WP4, id_old, campaign, var_short)
      
      if(var_short %in% c("rich", "FD", "FR", "simp", "temp_beta", "alpha", "beta", "gamma", "dem")){
        
        quantiles_var <- quantile(data_aux_filtered[[var_short]], probs = c(0.5, 0.75, 0.90), na.rm = TRUE)
        
        data_aux_filtered <- data_aux_filtered %>%
          mutate(!!var_short := ifelse(data_aux_filtered[[var_short]] > quantiles_var[1], 1, 0))
      }
      
      data_filtered = data_aux_filtered %>%
        left_join(data_filtered, by = c("WP4", "id_old", "campaign"))
    }
  }

  #shape_var <- terra::merge(shape, data_filtered, by.x = "ID", by.y = "WP4", all.x=TRUE)
  
  #puvspr_dat <- as.data.frame(shape_var)
  #puvspr_dat <- puvspr_dat[, c("ID", set_var)]
  
  puvspr_dat <- data_filtered %>%
    tidyr::pivot_longer(-c(WP4, id_old, campaign), names_to = "species", values_to = "amount")
  
  colnames(puvspr_dat) <- c("pu", "id_old", "campaign", "species", "amount")
  puvspr_dat$pu <- as.numeric(puvspr_dat$pu)
  
  puvspr_dat <- na.omit(puvspr_dat)
  
  #spec_dat---------------------------------------------------------------------
  names(var) = set_var
  
  spec_dat <- data.frame(id = seq(var),
                         name = names(var),
                         prop = unlist(var, use.names = FALSE))
  
  puvspr_dat_2021 <- puvspr_dat %>%
    left_join(spec_dat, by = c("species" = "name")) %>%
    select(pu, id, amount, campaign, id_old)
  
  colnames(puvspr_dat_2021) <- c("pu", "species", "amount", "campaign", "id_old")
  
  #boundary---------------------------------------------------------------------
  #boundary_dat <- read.table("data/data/STcon_pseudo_6.csv", sep = ";", header = TRUE)
  boundary_dat <- read.table("data/data/boundary.csv", sep = ",", header = TRUE)
  colnames(boundary_dat) <- c("id1", "id2", "boundary")
  
  boundary_dat <- boundary_dat %>%
    filter(!is.na(boundary))
    
  
  ##############################################################################
  #2100-------------------------------------------------------------------------
  ##############################################################################
  period = "2100"
  
  #pu dat-----------------------------------------------------------------------
  pu_dat_2100 = pu_dat
  pu_dat_2100$id <- 600000 + pu_dat$id
  pu_dat <- rbind(pu_dat, pu_dat_2100)
  
  #puvspr_dat-------------------------------------------------------------------
  
  for(i in 1:length(var)){

    var_short = set_var[i]
  
    if(i == 1){
      data <- get_bioinformation(var_short, drn, period)
      
      data$id_old <- data$WP4
      
      data_filtered <- data %>%
        select(WP4, id_old, campaign, var_short) %>%
        mutate(!!sym("WP4") := WP4 + campaign*multiplier + 600000) %>%
        select(WP4, id_old, campaign, var_short)
      
      if(var_short %in% c("rich", "FD", "FR", "simp", "temp_beta", "alpha", "beta", "gamma", "dem")){
        
        quantiles_var <- quantile(data_filtered[[var_short]], probs = c(0.5, 0.75, 0.90), na.rm = TRUE)
        
        data_filtered <- data_filtered %>%
          mutate(!!var_short := ifelse(data_filtered[[var_short]] > quantiles_var[1], 1, 0))
      }
    }
    else{
      data_aux <- get_bioinformation(var_short, drn, period)
      data_aux$id_old <- data_aux$WP4
      
      data_aux_filtered <- data_aux %>%
        #filter(campaign == 6) %>%
        select(WP4, id_old, campaign, var_short) %>%
        mutate(!!sym("WP4") := WP4 + campaign*multiplier + 600000) %>%
        select(WP4, id_old, campaign, var_short)
      
      
      if(var_short %in% c("rich", "FD", "FR", "simp", "temp_beta", "alpha", "beta", "gamma", "dem")){
        
        quantiles_var <- quantile(data_aux_filtered[[var_short]], probs = c(0.5, 0.75, 0.90), na.rm = TRUE)
        
        data_aux_filtered <- data_aux_filtered %>%
          mutate(!!var_short := ifelse(data_aux_filtered[[var_short]] > quantiles_var[1], 1, 0))
      }
      
      data_filtered = data_aux_filtered %>%
        left_join(data_filtered, by = c("WP4", "id_old", "campaign"))
    }
  }
  
 
  puvspr_dat_2100 <- data_filtered %>%
    tidyr::pivot_longer(-c(WP4, id_old, campaign), names_to = "species", values_to = "amount")
  
  colnames(puvspr_dat_2100) <- c("pu", "id_old", "campaign","species", "amount")
  puvspr_dat_2100$pu <- as.numeric(puvspr_dat_2100$pu)
  puvspr_dat_2100 <- na.omit(puvspr_dat_2100)
  puvspr_dat_2100$species <- str_c(puvspr_dat_2100$species, "_2100")
  
  
  #spec_dat---------------------------------------------------------------------
  names(var) = set_var
  
  spec_dat_2100 <- data.frame(id = seq(var) + max(spec_dat$id),
                         name = str_c(names(var), "_2100"),
                         prop = unlist(var, use.names = FALSE))
  
  
  puvspr_dat_2100 <- puvspr_dat_2100 %>%
    left_join(spec_dat_2100, by = c("species" = "name")) %>%
    select(pu, id, amount, campaign, id_old)
  
  colnames(puvspr_dat_2100) <- c("pu", "species", "amount", "campaign", "id_old")
  
  puvspr_dat <- rbind(puvspr_dat_2021, puvspr_dat_2100)
  spec_dat <- rbind(spec_dat, spec_dat_2100)
  
  input_data <- list(pu_dat, spec_dat, puvspr_dat, boundary_dat, puvspr_dat_2021, puvspr_dat_2100)
  
  # Assign names to the list elements using setNames
  input_data <- setNames(input_data, c("pu_dat", "spec_dat", "puvspr_dat", "boundary_dat",
                                       "puvspr_dat_2021",
                                       "puvspr_dat_2100"))
  
  return(input_data)
}


optimizing <- function(drn, var, blm){

  input_data <- calculate_opt_inputs(drn, var)
  
  p <- prioritizr::marxan_problem(input_data$pu_dat, 
                                  input_data$spec_dat, 
                                  input_data$puvspr_dat, 
                                  input_data$boundary_dat, 
                                  blm = blm) %>%
    add_cbc_solver(gap = 0.10, time_limit = 50)
  
  sol <- solve(p, force = TRUE)
  
  last_sol <<- sol
  
  update_map_opt(drn, sol, drn_country, var)
  
  calculate_cons_rest(drn, var)
  
}


calculate_cons_rest <- function(drn, var){
  
  input_data <- calculate_opt_inputs(drn, var)
  
  # data_filtered_2100 <- input_data$puvspr_dat_2100 %>%
  #   mutate(WP4 = as.numeric(substr(pu, nchar(pu) - 3, nchar(pu)))) %>%
  #   mutate(campaign = substr(pu, nchar(1), nchar(1))) %>%
  #   select(WP4, species, amount, campaign, id_old)
  # 
  data_filtered_2100 <- input_data$puvspr_dat_2100 %>%
    rename(WP4 = id_old) %>%
    #mutate(campaign = substr(pu, nchar(1), nchar(1))) %>%
    select(WP4, species, amount, campaign)
  
  data_filtered_2100$period <- "2100"
  
  data_filtered_2100 <- data_filtered_2100 %>%
    left_join(input_data$spec_dat, by = c("species" = "id")) %>%
    mutate(name = gsub("_2100$", "", name))
  
  
  data_filtered_2021 <- input_data$puvspr_dat_2021 %>%
    rename(WP4 = id_old) %>%
    #mutate(WP4 = as.numeric(substr(pu, nchar(pu) - 3, nchar(pu)))) %>%
    #mutate(campaign = substr(pu, nchar(1), nchar(1))) %>%
    select(WP4, species, amount, campaign)
  
  data_filtered_2021$period <- "2021"
  
  data_filtered_2021 <- data_filtered_2021 %>%
    left_join(input_data$spec_dat, by = c("species" = "id")) %>%
    mutate(name = gsub("_2100$", "", name))
  
  
   data_species <- rbind(data_filtered_2100, data_filtered_2021)
  

  # Group by WP4, campaign, name, and period, then summarize the amount
  df_summarized <- data_species %>%
    group_by(WP4, campaign, name, period) %>%
    summarize(total_amount = sum(amount)) 
  
  # Spread the periods into separate columns to calculate the difference
  df_wide <- df_summarized %>%
    tidyr::pivot_wider(names_from = period, values_from = total_amount, names_prefix = "period_") %>%
    mutate(difference = period_2100 - period_2021) %>%
    group_by(WP4, name) %>%
    summarize(total_difference = sum(difference), .groups = 'drop')
  
  df_wide <- df_wide %>%
    group_by(WP4) %>%
    summarize(total_difference_all = sum(total_difference), .groups = 'drop')

  
  return(df_wide)
}


update_map_opt <- function(drn, sol, drn_country, var){
  
  shape <- get_shape(drn)
  cons_rest <- calculate_cons_rest(drn, var)
  
  data_filtered <- sol %>%
    select(id, solution_1, id_old, campaign) %>%
    rename(WP4 = id_old)
      #mutate(WP4 = as.numeric(substr(id, nchar(id) - 3, nchar(id)))) %>%
      #mutate(campaign = substr(id, nchar(1), nchar(1)))

  data_filtered <- data_filtered %>%
      group_by(WP4) %>%
      summarise(solution_1 = sum(solution_1)) %>%
      select(WP4, solution_1) %>%
      rename(ID = WP4)
  
  
  int <- read.table("data/data/regine_reaches.csv", sep = ",", header = TRUE)
  b_int <- left_join(data_filtered,int, by="ID") #join intermittence with freq 
  
  shape_var <- terra::merge(shape, data_filtered, by.x = "ID", by.y = "ID", all.x=TRUE)
  shape_var$inter = ifelse(shape_var$regine_rea == "perennial", 1, 5)
  
  shape_var <- terra::merge(shape_var, cons_rest, by.x = "ID", by.y = "WP4", all.x=TRUE)
  
  ##palette
  levels(shape_var$solution_1) <- c(0,1)
  pal <- colorFactor(palette="Reds", domain=shape_var[["solution_1"]][,1], na.color="orange")
  #pal <- colorBin(palette="Blues", domain=data[,solution_1], na.color="orange")
  
  
  # Define two palettes
  shape_var$manage <- ifelse(shape_var$total_difference_all > 0, "Conservation", ifelse(shape_var$total_difference_all < 0, "Restauration", "No recomendation"))
  
  title_legend <- ""
  
  
  popup_map <- sprintf(
    "<strong>ID %s</strong><br/>
         Value: %g <br/>
         Type: %s <br/>
         Potential for: %s <br/>",
    shape_var$ID, 
    shape_var[["solution_1"]][,1],
    shape_var$regine_rea,
    shape_var$manage) %>% 
    lapply(htmltools::HTML)


  leafletProxy("map_opt", data = shape_var) %>%
    clearControls() %>%
    addPolylines(group = "Base",
                 weight = 3,
                 color = ~pal(shape_var$solution_1),
                 fill = FALSE,
                 opacity = 1,
                 popup = popup_map,
                 dashArray = ~inter,
                 layerId = ~ID,
                 highlightOptions = highlightOptions(
                   weight = 5,
                   bringToFront = TRUE,
                   sendToBack = TRUE)
    )%>%
    addLegend(group = "Base2",
              pal = pal,
              title = title_legend,
              values = shape_var$solution_1,
              opacity = 1,
              position = "topright")
}


# Network yearly time series (observed period)
figure_targets_reached <- function(drn, var, id){
  
  input_data <- calculate_opt_inputs(drn, var)
  
  data_filtered_2100 <- input_data$puvspr_dat_2100 %>%
    rename(WP4 = id_old) %>%
    #filter(pu >= 600000) %>%
    # mutate(WP4 = as.numeric(substr(pu, nchar(pu) - 3, nchar(pu)))) %>%
    # mutate(campaign = substr(pu, nchar(1), nchar(1))) %>%
    select(WP4, species, amount, campaign)
  
  data_filtered_2100$period <- "2100"

  data_filtered_2021 <- input_data$puvspr_dat_2021 %>%
    rename(WP4 = id_old) %>%
    #filter(pu < 600000) %>%
    # mutate(WP4 = as.numeric(substr(pu, nchar(pu) - 3, nchar(pu)))) %>%
    # mutate(campaign = substr(pu, nchar(1), nchar(1))) %>%
    select(WP4, species, amount, campaign)
  
  data_filtered_2021$period <- "2021"
  
  #Calculate % proportion by year
  #2021
  total_sum_species_2021  <- data_filtered_2021 %>%
    group_by(species, period) %>%
    summarise(total_sum_species  = sum(amount))

  sum_species_by_wp4_2021  <- data_filtered_2021 %>%
    filter(WP4 == id) %>%
    group_by(species, WP4) %>%
    summarise(sum_species_wp4  = sum(amount))

  proportion_data_2021 <- sum_species_by_wp4_2021 %>%
    left_join(total_sum_species_2021, by = "species") 
  
  #2100
  total_sum_species_2100  <- data_filtered_2100 %>%
    group_by(species, period) %>%
    summarise(total_sum_species  = sum(amount))

  sum_species_by_wp4_2100  <- data_filtered_2100 %>%
    filter(WP4 == id) %>%
    group_by(species, WP4) %>%
    summarise(sum_species_wp4  = sum(amount))
  
  proportion_data_2100 <- sum_species_by_wp4_2100 %>%
    left_join(total_sum_species_2100, by = "species")

  prop_data <- rbind(proportion_data_2021, proportion_data_2100)

  prop_data <- prop_data %>%
    left_join(input_data$spec_dat, by = c("species" = "id")) %>%
    mutate(name = gsub("_2100$", "", name)) %>%
    mutate(contribution = sum_species_wp4/(total_sum_species*prop))
  
  variables <- data.frame(
    name = variables_short,
    long_name = variables_long
  )
  
  # Perform the left join
  prop_data <- prop_data %>%
    left_join(variables, by = "name")

  # long_title = paste0("Comparation of the temporal trend of ",var_long, " between ", "2020 and ", period)
  # wrapped_title <- str_wrap(long_title, width = max_width)
  # dom_summary = get_difference_domain_bioinformation(var, drn)
  #   
  p1 <- ggplot(prop_data, aes(x = factor(long_name), y = contribution, fill = factor(period))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = "Contribution of the river section to the conservation target",
      x = "Features",
      y = "% of Contribution",
      fill = ""
    ) +
    theme_minimal() +
    scale_fill_manual(values = c("black", "forestgreen"))+
    theme_bw()+
    scale_y_continuous(labels = scales::percent_format(scale = 100)) +
    theme(plot.title = element_text(size = 10, hjust = 0.5, lineheight = 0.5))
  
  return(p1)
}



# Network yearly time series (observed period)
figure_inter_reached <- function(drn){
  
  shape <- get_shape(drn)
  
  data_filtered <- last_sol %>%
    select(id, solution_1, id_old, campaign) %>%
    rename(WP4 = id_old)

  # data_filtered <- data_filtered %>%
  #   mutate(WP4 = as.numeric(substr(id, nchar(id) - 3, nchar(id)))) %>%
  #   mutate(campaign = substr(id, nchar(1), nchar(1)))
  
  data_filtered <- data_filtered %>%
    group_by(WP4, campaign) %>%
    summarise(solution_1 = sum(solution_1)) %>%
    select(WP4, solution_1) %>%
    rename(ID = WP4)
  
  int <- read.table("data/data/regine_reaches.csv", sep = ",", header = TRUE)
  b_int <- left_join(data_filtered,int, by="ID") #join intermittence with freq 
  b_int <- b_int %>%
    group_by(Regime) %>%
    summarize(total_solution_1 = sum(solution_1))
  
  # # Create the pie chart
  # p1 <- ggplot(b_int, aes(x = "", y = total_solution_1, fill = Regime)) +
  #   geom_bar(stat = "identity", width = 1) +
  #   theme_void() +
  #   theme(legend.title = element_blank())
  # 
  
  # labels = c('Oxygen','Hydrogen','Carbon_Dioxide','Nitrogen')
  # values = c(4500, 2500, 1053, 500)
  # 
  p1 <- plot_ly(type='pie', labels=b_int$Regime, values=b_int$total_solution_1, 
                 textinfo='label+percent',
                 insidetextorientation='radial', width = 350, height = 300) %>%
    layout(
      margin = list(
        l = 0,  # left margin
        r = 0,  # right margin
        b = 0,  # bottom margin
        t = 0   # top margin
      )
    )
  
  return(p1)
}
