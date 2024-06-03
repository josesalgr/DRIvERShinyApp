########## Read and import data ###############################
read_network_file <- function(data_path){
  # Uncompress file
  unzip(data_path, exdir=dirname(data_path), junkpaths=TRUE)
  
  # read .dat file
  data_dat <- gsub("\\.zip$", ".dat", data_path)
  lines <- readLines(data_dat) # read all lines in file
  
  # get dates
  dates <- lines[grepl("TimeLoop",lines)][-1] # find lines with dates
  dates <- gsub("TimeLoop\t", "", dates) # remove first column
  dates <- as.Date(dates, format="%Y-%m-%d %H:%M") # convert into R date format
  
  # get values
  value <- lines[grepl("-1\t",lines)] # find lines with results
  value <- gsub("-1\t", "", value) # remove first column
  value <- as.numeric(value) # convert into number
  
  # create a dataframe
  df <- data.frame(date=dates,
                   value=value)
  
  # Remove file .dat
  file.remove(data_dat)
  
  return(df)
}

read_reach_file <- function(data_path){
  # Uncompress file
  unzip(data_path, exdir=dirname(data_path), junkpaths=TRUE)
  
  # read .dat file
  data_dat <- gsub("\\.zip$", ".dat", data_path)
  lines <- readLines(data_dat) # read all lines in file
  
  # get dates
  dates <- lines[grepl("TimeLoop",lines)][-1] # find lines with dates
  dates <- gsub("TimeLoop\t", "", dates) # remove first column
  dates <- as.Date(dates, format="%Y-%m-%d %H:%M") # convert into R date format
  
  # get data
  data <- lines[grepl("^[[:digit:]]+", lines)] # find lines with results
  data <- strsplit(data,"\t")
  data <- do.call(rbind, lapply(data, as.character))
  
  # get reaches
  n_reaches <- length(unique(data[,1]))
  reachID <- as.numeric(data[1:n_reaches,1])
  
  # get values
  value <- matrix(as.numeric(data[,2]), nrow=n_reaches)
  value[value==-9999] <- NA
  value <- as.data.frame(value)
  rownames(value) <- reachID
  colnames(value) <- dates 
  
  # Remove file .dat
  file.remove(data_dat)
  
  return(value)
}

read_network_present <- function(DRN, time_scale, indicator){
  
  # path to indicator file
  data_path <- paste0("data/Results_present_period/intermittence_indicators/",DRN,"/Network/",time_scale,"/",indicator,".zip")
  
  df <- read_network_file(data_path)
  
  return(df)
}

read_network_gcm <- function(DRN, time_scale, indicator, period){
  # path to indicator file
  path <- paste0("data/Results_projection/flow_intermittence_indicators/",DRN,"/")
  
  models <- list.files(path)
  models <- models[grepl(period,models)]
  
  df_models <- c()
  for (i in 1:length(models))
  {
    data_path <- paste0(path,models[i],"/Aggr/Network/",time_scale,"/",indicator,".zip")
    df <- read_network_file(data_path)
    
    if(period=="1985_2014"){
      df$model <- strsplit(models[i],"_")[[1]][3]
      df$scenario <- "historical"
    }else{
      df$model <- strsplit(models[i],"_")[[1]][5]
      df$scenario <- strsplit(models[i],"_")[[1]][3]
    }
    df_models <- rbind(df_models, df)
  }
  
  return(df_models)
}

read_reach_present <- function(DRN, time_scale, indicator){
  # path to indicator file
  data_path <- paste0("data/Results_present_period/intermittence_indicators/",DRN,"/Reach/",time_scale,"/",indicator,".zip")
  
  # open data
  shp <- read_reach_file(data_path)
  return(shp)
}

read_reach_gcm <- function(DRN, time_scale, indicator, gcm, ssp, period){
  # path to indicator file
  path <- paste0("data/Results_projection/flow_intermittence_indicators/",DRN,"/")
  
  if(period=="1985_2014")
  {
    gcm_folder <- paste0(DRN,"_model_",gcm,"_Member_10_1985_2014/")
  }else{
    gcm_folder <- paste0(DRN,"_scenario_",ssp,"_model_",gcm,"_Member_10_2015_2100/")
  }
  
  data_path <- paste0(path,gcm_folder,"Aggr/Reach/",time_scale,"/",indicator,".zip")
  shp <- read_reach_file(data_path) # open data
  return(shp)
}

read_one_reach_gcm <- function(DRN, time_scale, indicator, reachID){
  data_gcm_hist <- c()
  data_gcm_proj <- c()
  for (gcm in c("gfdl-esm4", "ipsl-cm6a-lr", "mpi-esm1-2-hr", "mri-esm2-0", "ukesm1-0-ll")){
    data <- read_reach_gcm(DRN, time_scale, indicator, gcm,"","1985_2014") # read file
    k_reach <- which(rownames(data)==reachID)
    data_gcm_hist <- rbind(data_gcm_hist,
                           data.frame(date=colnames(data),
                             value=unname(t(data[k_reach,])),
                             model=gcm,
                             scenario="historical"))
    for (ssp in c("ssp126","ssp370","ssp585")){
      data <- read_reach_gcm(DRN, time_scale, indicator, gcm,ssp,"2015_2100") # read file
      k_reach <- which(rownames(data)==reachID)
      data_gcm_proj <- rbind(data_gcm_proj,
                             data.frame(date=colnames(data),
                                        value=unname(t(data[k_reach,])),
                                        model=gcm,
                                        scenario=ssp))
    }
  }
  data <- rbind(data_gcm_hist, data_gcm_proj)
  return(data)
}

read_shp <- function(DRN){
  # path to indicator file
  path <- paste0("data/shp/",DRN,"/small_river_network.shp")
  
  # read shapefile
  shp <- st_read(path)
  
  return(shp)
}

read_climate_present <- function(DRN, time_scale, indicator){
  
  # path to indicator file
  data_path <- paste0("data/Results_present_period/climate/",DRN,"/",DRN,"_ERA5land_1960_2021/",time_scale,"/",indicator,".zip")
  
  # Uncompress file
  unzip(data_path, exdir=dirname(data_path), junkpaths=TRUE)
  
  # read .dat file
  data_dat <- gsub("\\.zip$", ".dat", data_path)
  df <- read.table(data_dat, header = TRUE, sep="\t")
  df$date <- as.Date(df$date)
  
  # Remove file .dat
  file.remove(data_dat)
  
  return(df)
}

read_climate_gcm <- function(DRN, time_scale, indicator, period){
  
  # path to indicator file
  path <- paste0("data/Results_projection/climate/",DRN,"/")
  
  models <- list.files(path)
  models <- models[grepl(period,models)]
  
  df_models <- c()
  for (i in 1:length(models))
  {
    data_path <- paste0(path,models[i],"/",time_scale,"/",indicator,".zip")
    
    # Uncompress file
    unzip(data_path, exdir=dirname(data_path), junkpaths=TRUE)
    
    # read .dat file
    data_dat <- gsub("\\.zip$", ".dat", data_path)
    df <- read.table(data_dat, header = TRUE, sep="\t")
    df$date <- as.Date(df$date)
    
    # Remove file .dat
    file.remove(data_dat)
    
    if(period=="1985_2014"){
      df$model <- strsplit(models[i],"_")[[1]][3]
      df$scenario <- "historical"
    }else{
      df$model <- strsplit(models[i],"_")[[1]][5]
      df$scenario <- strsplit(models[i],"_")[[1]][3]
    }
    df_models <- rbind(df_models, df)
  }
 
  return(df_models)
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
# Network yearly time series (observed period)
figure_1 <- function(drn_short_name, drn_long_name, indicator_short_name, indicator_long_name){
  # read data
  if(indicator_short_name %in% c("Temp","Precip","ET"))
  {
    data_Y <- read_climate_present(drn_short_name, "Yearly", indicator_short_name)
  }else{
    data_Y <- read_network_present(drn_short_name, "Yearly", indicator_short_name) 
  }
  
  # plot
  p1 <- ggplot(data_Y, aes(x=date, y=value))+
    geom_line()+
    theme_bw()+
    ylab(indicator_short_name)+xlab("")+
    ggtitle(paste0("Evolution of mean annual ",indicator_short_name," during the period 1960-2021"))
  p1
}

# Network yearly time series (projections)
figure_2 <- function(drn_short_name, drn_long_name, indicator_short_name, indicator_long_name){
  # read data
  if(indicator_short_name %in% c("Temp","Precip","ET"))
  {
    data_gcm_hist <- read_climate_gcm(drn_short_name, "Yearly", indicator_short_name,"1985_2014") # read hindcast files (1985-2014)
    data_gcm_proj <- read_climate_gcm(drn_short_name, "Yearly", indicator_short_name,"2015_2100") # read projections files (2015-2100)
  }else{
    data_gcm_hist <- read_network_gcm(drn_short_name, "Yearly", indicator_short_name,"1985_2014") # read hindcast files (1985-2014)
    data_gcm_proj <- read_network_gcm(drn_short_name, "Yearly", indicator_short_name,"2015_2100") # read projections files (2015-2100)
  }
  
  # Combine the dataframes
  data <- rbind(data_gcm_hist, data_gcm_proj)
  
  # plot the long-term evolution of yearly RelInt
  colors <- c("darkgrey",plasma(3))
  names(colors) <- c("historical",unique(data_gcm_proj$scenario))
  p2 <- ggplot(data, aes(x=date, y=value, color=scenario, group=interaction(model,scenario))) +
    geom_line()+
    theme_bw()+
    ylab(indicator_short_name)+xlab("")+
    scale_color_manual(values = colors, name="SSP scenario")+
    ggtitle(paste0("Evolution of mean annual ",indicator_short_name," under climate change scenarios"))
  p2
}

# Network yearly by periods (projections)
figure_3 <- function(drn_short_name, drn_long_name, indicator_short_name, indicator_long_name, ssp_short_name){
  # read data
  if(indicator_short_name %in% c("Temp","Precip","ET"))
  {
    data_gcm_hist <- read_climate_gcm(drn_short_name, "Yearly", indicator_short_name,"1985_2014") # read hindcast files (1985-2014)
    data_gcm_proj <- read_climate_gcm(drn_short_name, "Yearly", indicator_short_name,"2015_2100") # read projections files (2015-2100)
  }else{
    data_gcm_hist <- read_network_gcm(drn_short_name, "Yearly", indicator_short_name,"1985_2014") # read hindcast files (1985-2014)
    data_gcm_proj <- read_network_gcm(drn_short_name, "Yearly", indicator_short_name,"2015_2100") # read projections files (2015-2100)
  }
  
  # Combine the dataframes
  data <- rbind(data_gcm_hist, data_gcm_proj)
  
  # Compare the evolution of yearly RelInt for the 5 GCMs for one SSP scenario
  data_Y <- data %>%
    mutate(year=year(date),
           period=ifelse(year %in% 1985:2014,"1985-2014",
                         ifelse(year %in% 2041:2070,"2041-2070","2071-2100")))
  
  p3 <- ggplot(data_Y[data_Y$scenario %in% c("historical",ssp_short_name),], aes(x=model,y=value,fill=model))+
    geom_boxplot()+
    facet_wrap(. ~ period)+
    theme_bw()+
    ylab(indicator_short_name)+xlab("GCM")+
    scale_fill_viridis_d(option="turbo",name="GCM")+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          )+
    ggtitle(paste0("Comparison of the distributions of annual ",indicator_short_name," during the periods\n1985-2014, 2041-2070, and 2071-2100, for the scenario ", ssp_short_name))
  p3 
}

# Network seasonal pattern (observed period)
figure_4 <- function(drn_short_name, drn_long_name, indicator_short_name, indicator_long_name){
  # read data
  if(indicator_short_name %in% c("Temp","Precip","ET"))
  {
    data_M <- read_climate_present(drn_short_name, "Monthly", indicator_short_name)
  }else{
    data_M <- read_network_present(drn_short_name, "Monthly", indicator_short_name)  
  }
  
  # Computation of the monthly average
  data_M_average <- data_M %>%
    mutate(month=month(date)) %>% # create a column for the month
    group_by(month) %>% # group by month
    summarise(median = median(value),
              q25 = quantile(value, probs=0.25),
              q75 = quantile(value, probs=0.75),
              q10 = quantile(value, probs=0.1),
              q90 = quantile(value, probs=0.9)) %>% # compute statistics for each month
    ungroup()
  
  # plot data
  if(indicator_short_name %in% c("RelInt","PatchC","Temp","Precip","ET"))
  {
    min <- min(data_M_average$q10)
    max <- max(data_M_average$q90)
    p4 <- ggplot()+
      geom_ribbon(data=data_M_average, mapping=aes(x=month,ymin=q10,ymax=q90), fill="gray60", alpha=0.5) +
      geom_ribbon(data=data_M_average, mapping=aes(x=month,ymin=q25,ymax=q75), fill="gray25", alpha=0.5) +
      geom_line(data_M_average, mapping=aes(x=month, y=median), color="darkred")+
      theme_bw()+
      ylab(indicator_short_name)+xlab("Month")+
      scale_x_continuous(breaks=1:12)+
      annotate(geom="text", x=2, y=max-(max-min)*0.05, label="Median", color="darkred")+
      annotate(geom="text", x=2, y=max-(max-min)*0.10, label="Q25-Q75", color="gray25")+
      annotate(geom="text", x=2, y=max-(max-min)*0.15, label="Q10-Q90", color="gray60")+
      ggtitle(paste0("Average seasonal pattern of ",indicator_short_name," during the period 1960-2021"))
  }else if(indicator_short_name == "RelFlow"){
    min <- min(data_M_average$q10)
    max <- max(data_M_average$q90)
    p4 <- ggplot()+
      geom_ribbon(data=data_M_average, mapping=aes(x=month,ymin=q10,ymax=q90), fill="gray60", alpha=0.5) +
      geom_ribbon(data=data_M_average, mapping=aes(x=month,ymin=q25,ymax=q75), fill="gray25", alpha=0.5) +
      geom_line(data_M_average, mapping=aes(x=month, y=median), color="darkred")+
      theme_bw()+
      ylab(indicator_short_name)+xlab("Month")+
      scale_x_continuous(breaks=1:12)+
      annotate(geom="text", x=2, y=min+(max-min)*0.15, label="Median", color="darkred")+
      annotate(geom="text", x=2, y=min+(max-min)*0.10, label="Q25-Q75", color="gray25")+
      annotate(geom="text", x=2, y=min+(max-min)*0.05, label="Q10-Q90", color="gray60")+
      ggtitle(paste0("Average seasonal pattern of ",indicator_short_name," during the period 1960-2021"))
  }
  p4
}

# Network seasonal pattern (projections)
figure_5 <- function(drn_short_name, drn_long_name, indicator_short_name, indicator_long_name){
  # read data
  if(indicator_short_name %in% c("Temp","Precip","ET"))
  {
    data_era5land <- read_climate_present(drn_short_name, "Monthly", indicator_short_name) # read era5land file (2006-2021)
    data_gcm_hist <- read_climate_gcm(drn_short_name, "Monthly", indicator_short_name,"1985_2014") # read hindcast files (1985-2014)
    data_gcm_proj <- read_climate_gcm(drn_short_name, "Monthly", indicator_short_name,"2015_2100") # read projections files (2015-2100)
  }else{
    data_era5land <- read_network_present(drn_short_name, "Monthly", indicator_short_name) # read era5land file (2006-2021)
    data_gcm_hist <- read_network_gcm(drn_short_name, "Monthly", indicator_short_name, "1985_2014") # read hindcast files (1985-2014)
    data_gcm_proj <- read_network_gcm(drn_short_name, "Monthly", indicator_short_name, "2015_2100") # read projections files (2015-2100)
  }
  
  # Combine the dataframes
  data_era5land$model <- "ERA5land"
  data_era5land$scenario <- "ERA5land"
  data <- rbind(data_era5land, data_gcm_hist, data_gcm_proj)
  
  # plot the evolution of the seasonal pattern of RelInt
  data_M <- data %>%
    mutate(month=month(date),
           year=year(date),
           period=ifelse(model=="ERA5land","1985-2014 (ERA5-land)",
                         ifelse(year %in% 1985:2014,"1985-2014 (GCMs)",
                                ifelse(year %in% 2041:2070,"2041-2070","2071-2100")))) %>%
    group_by(month, model, scenario, period) %>%
    summarise(value=mean(value)) %>%
    ungroup()
  data_M <- rbind(data_M[data_M$period %in% c("2041-2070","2071-2100"),],
                  data_M[!(data_M$period %in% c("2041-2070","2071-2100")),] %>% mutate(scenario = "ssp126"),
                  data_M[!(data_M$period %in% c("2041-2070","2071-2100")),] %>% mutate(scenario = "ssp370"),
                  data_M[!(data_M$period %in% c("2041-2070","2071-2100")),] %>% mutate(scenario = "ssp585")
  )
  
  p5 <- ggplot(data_M, aes(x=month,y=value,color=period,group=interaction(model,period)))+
    geom_line()+
    facet_wrap(. ~ scenario)+
    theme_bw()+
    ylab(indicator_short_name)+xlab("Month")+
    scale_x_continuous(breaks=1:12)+
    scale_color_manual(name="Period",
                       values = c("1985-2014 (ERA-land)"="black","1985-2014 (GCMs)"="grey","2041-2070"="royalblue","2071-2100"="orange"))+
    ggtitle(paste0("Average seasonal pattern of ",indicator_short_name," under climate change scenarios"))
  p5
}

# Spatial pattern aggregated (observed period)
figure_6 <- function(drn_short_name, drn_long_name, indicator_short_name, indicator_long_name, aggregation_period){
  if(aggregation_period == "Annual")
  {
    # read file
    shp_Y <- read_reach_present(drn_short_name, "Yearly", indicator_short_name) 
    shp_Y[is.na(shp_Y)] <- 0 # set NA values to 0
    
    # Computation of the mean annual spatial pattern
    shp_Y_average <- shp_Y %>% # for each reach computes the mean of all years
      rowwise() %>%
      mutate(Y_average = mean(c_across(colnames(shp_Y)))) %>%
      select(Y_average)
    
  }else{
    # read file
    shp_Y <- read_reach_present(drn_short_name, "Monthly", indicator_short_name) 
    shp_Y[is.na(shp_Y)] <- 0 # set NA values to 0
    
    # Computation of the mean annual spatial pattern
    m <- which(c("Annual","January","February","March","April","May","June","July","August","September","October","November","December") == aggregation_period)-1
    col_years <- colnames(shp_Y)[month(colnames(shp_Y))==m] # find all the columns for each years
    shp_Y_average <- shp_Y %>% # for each reach computes the mean of all years
      rowwise() %>%
      mutate(Y_average = mean(c_across(col_years))) %>%
      select(Y_average)
  }
  
  shp_Y_average$cat <- as.numeric(rownames(shp_Y))
  
  # read shapefile and join data
  shp <- read_shp(drn_short_name)
  shp_Y_average <- shp %>% left_join(shp_Y_average, by='cat')
  
  p6 <- ggplot(shp_Y_average, mapping = aes(color=Y_average,
                                            data_id = cat,
                                            tooltip = round(Y_average, digits = 1))) + 
    geom_sf_interactive(linewidth=1.2) + 
    theme_bw() +
    scale_color_viridis_c(name=indicator_short_name)+
    ggtitle(paste0("Mean ",aggregation_period," spatial pattern of ",indicator_short_name," during 1960-2021"))
  p6
}

figure_6_FstDrE <- function(drn_short_name, drn_long_name, indicator_short_name, indicator_long_name){
  # read file
  shp_Y <- read_reach_present(drn_short_name, "Yearly", indicator_short_name) 
  # shp_Y[is.na(shp_Y)] <- 0 # set NA values to 0
  
  # Computation of the mean annual spatial pattern
  shp_Y_average <- shp_Y %>% # for each reach computes the mean of all years
    rowwise() %>%
    mutate(Y_average = mean(c_across(colnames(shp_Y)), na.rm=TRUE)) %>%
    select(Y_average)
  
  shp_Y_average$cat <- as.numeric(rownames(shp_Y))
  
  # read shapefile and join data
  shp <- read_shp(drn_short_name)
  shp_Y_average <- shp %>% left_join(shp_Y_average, by='cat')
  
  p6 <- ggplot(shp_Y_average, mapping = aes(color=Y_average,
                                            data_id = cat,
                                            tooltip = round(Y_average, digits = 1))) + 
    geom_sf_interactive(linewidth=1.2) + 
    theme_bw() +
    scale_color_viridis_c(name=indicator_short_name, limits=c(0,366))+
    ggtitle(paste0("Mean annual spatial pattern of ",indicator_short_name," during 1960-2021"))
  p6
}

# Spatial pattern by date (observed period)
figure_7 <- function(drn_short_name, drn_long_name, indicator_short_name, indicator_long_name, selected_year, selected_month){
  # read file
  shp_Y <- read_reach_present(drn_short_name, "Monthly", indicator_short_name) 
  shp_Y[is.na(shp_Y)] <- 0 # set NA values to 0
  
  if(selected_month == "Annual")
  {
    # Computation of the annual spatial pattern
    col_years <- colnames(shp_Y)[year(colnames(shp_Y))==as.numeric(selected_year)] # find all the columns for each years
    shp_Y_average <- shp_Y %>% # for each reach computes the mean of all years
      rowwise() %>%
      mutate(Y_average = sum(c_across(all_of(col_years)))) %>%
      select(Y_average)
    title <- paste0("Spatial pattern of ",indicator_short_name," in ", selected_year)
    
  }else{
    # Finds selected month
    m <- which(c("Annual","January","February","March","April","May","June","July","August","September","October","November","December") == selected_month)-1
    col_month<- colnames(shp_Y)[(year(colnames(shp_Y))==as.numeric(selected_year)) & (month(colnames(shp_Y))==m)]
    shp_Y_average <- shp_Y %>%
      select(all_of(col_month))
    colnames(shp_Y_average) <- "Y_average"
    title <- paste0("Spatial pattern of ",indicator_short_name," in ", selected_month," ",selected_year)
  }
  
  shp_Y_average$cat <- as.numeric(rownames(shp_Y))
  
  # read shapefile and join data
  shp <- read_shp(drn_short_name)
  shp_Y_average <- shp %>% left_join(shp_Y_average, by='cat')
  
  p7 <- ggplot(shp_Y_average, mapping = aes(color=Y_average,
                                            data_id = cat,
                                            tooltip = round(Y_average, digits = 1))) + 
    geom_sf_interactive(linewidth=1.2) + 
    theme_bw() +
    scale_color_viridis_c(name=indicator_short_name)+
    ggtitle(title)
  p7
}

figure_7_dur <- function(drn_short_name, drn_long_name, indicator_short_name, indicator_long_name, selected_year, selected_month){
  
  
  if(selected_month == "Annual")
  {
    # read file
    shp_Y <- read_reach_present(drn_short_name, "Yearly", indicator_short_name) 
    shp_Y[is.na(shp_Y)] <- 0 # set NA values to 0
    
    # Computation of the annual spatial pattern
    col_year <- colnames(shp_Y)[year(colnames(shp_Y))==as.numeric(selected_year)] # find all the columns for each years
    shp_Y_average <- shp_Y %>%
      select(all_of(col_year))
    colnames(shp_Y_average) <- "Y_average"
    title <- paste0("Spatial pattern of ",indicator_short_name," in ", selected_year)
    
  }else{
    # read file
    shp_Y <- read_reach_present(drn_short_name, "Monthly", indicator_short_name) 
    shp_Y[is.na(shp_Y)] <- 0 # set NA values to 0
    
    # Finds selected month
    m <- which(c("Annual","January","February","March","April","May","June","July","August","September","October","November","December") == selected_month)-1
    col_month<- colnames(shp_Y)[(year(colnames(shp_Y))==as.numeric(selected_year)) & (month(colnames(shp_Y))==m)]
    shp_Y_average <- shp_Y %>%
      select(all_of(col_month))
    colnames(shp_Y_average) <- "Y_average"
    title <- paste0("Spatial pattern of ",indicator_short_name," in ", selected_month," ",selected_year)
  }
  
  shp_Y_average$cat <- as.numeric(rownames(shp_Y))
  
  # read shapefile and join data
  shp <- read_shp(drn_short_name)
  shp_Y_average <- shp %>% left_join(shp_Y_average, by='cat')
  
  p7 <- ggplot(shp_Y_average, mapping = aes(color=Y_average,
                                            data_id = cat,
                                            tooltip = round(Y_average, digits = 1))) + 
    geom_sf_interactive(linewidth=1.2) + 
    theme_bw() +
    scale_color_viridis_c(name=indicator_short_name)+
    ggtitle(title)
  p7
}

figure_7_FstDrE <- function(drn_short_name, drn_long_name, indicator_short_name, indicator_long_name, selected_year){
  # read file
  shp_Y <- read_reach_present(drn_short_name, "Yearly", indicator_short_name) 
  #shp_Y[is.na(shp_Y)] <- 0 # set NA values to 0
  
  # Finds selected month
  col_year<- colnames(shp_Y)[year(colnames(shp_Y))==as.numeric(selected_year)]
  shp_Y_average <- shp_Y %>%
    select(all_of(col_year))
  colnames(shp_Y_average) <- "Y_average"
  title <- paste0("Spatial pattern of ",indicator_short_name," in ", selected_year)
  
  shp_Y_average$cat <- as.numeric(rownames(shp_Y))
  
  # read shapefile and join data
  shp <- read_shp(drn_short_name)
  shp_Y_average <- shp %>% left_join(shp_Y_average, by='cat')
  
  p7 <- ggplot(shp_Y_average, mapping = aes(color=Y_average,
                                            data_id = cat,
                                            tooltip = round(Y_average, digits = 1))) + 
    geom_sf_interactive(linewidth=1.2) + 
    theme_bw() +
    scale_color_viridis_c(name=indicator_short_name, limits=c(0,366))+
    ggtitle(title)
  p7
}

# Reach yearly time series (observed period)
figure_8 <- function(drn_short_name, drn_long_name, indicator_short_name, indicator_long_name, reachID){
  # read data
  data_Y <- read_reach_present(drn_short_name, "Yearly", indicator_short_name) 
  
  # extract data for reachID
  k_reach <- which(rownames(data_Y)==as.character(reachID))
  data_reach <- data.frame(date=as.Date(colnames(data_Y)),
                   value=unname(t(data_Y[k_reach,])))
  
  # plot
  if(indicator_short_name == "FstDrE")
  {
    p8 <- ggplot(data_reach, aes(x=date, y=value))+
      geom_point(size=2)+
      theme_bw()+
      ylim(0,366)+
      ylab(indicator_short_name)+xlab("")+
      ggtitle(paste0("Evolution of mean annual ",indicator_short_name," during the period 1960-2021"))
  }else{
    data_reach$value[is.na(data_reach$value)] <- 0 # set value to 0 if NA (ex : durD, durF, numFreDr, numFreRW)
    p8 <- ggplot(data_reach, aes(x=date, y=value))+
      geom_line()+
      theme_bw()+
      ylab(indicator_short_name)+xlab("")+
      ggtitle(paste0("Evolution of mean annual ",indicator_short_name," during the period 1960-2021"))
  }
  p8
}

# Reach seasonal pattern (observed period)
figure_9 <- function(drn_short_name, drn_long_name, indicator_short_name, indicator_long_name, reachID){
  # read data
  data_M <- read_reach_present(drn_short_name, "Monthly", indicator_short_name) 
  
  # extract data for reachID
  k_reach <- which(rownames(data_M)==as.character(reachID))
  data_reach <- data.frame(date=colnames(data_M),
                           value=unname(t(data_M[k_reach,])))
  
  data_reach$value[is.na(data_reach$value)] <- 0 # set value to 0 if NA (ex : durD, durF, numFreDr, numFreRW)
  
  # Computation of the monthly average
  data_reach_average <- data_reach %>%
    mutate(month=month(date)) %>% # create a column for the month
    group_by(month) %>% # group by month
    summarise(median = median(value),
              q25 = quantile(value, probs=0.25),
              q75 = quantile(value, probs=0.75),
              q10 = quantile(value, probs=0.1),
              q90 = quantile(value, probs=0.9)) %>% # compute statistics for each month
    ungroup()
  
  # plot data
  if(indicator_short_name %in% c("conD","durD","numFreDr"))
  {
    min <- min(data_reach_average$q10)
    max <- max(data_reach_average$q90)
    p9 <- ggplot()+
      geom_ribbon(data=data_reach_average, mapping=aes(x=month,ymin=q10,ymax=q90), fill="gray60", alpha=0.5) +
      geom_ribbon(data=data_reach_average, mapping=aes(x=month,ymin=q25,ymax=q75), fill="gray25", alpha=0.5) +
      geom_line(data_reach_average, mapping=aes(x=month, y=median), color="darkred")+
      theme_bw()+
      ylab(indicator_short_name)+xlab("Month")+
      scale_x_continuous(breaks=1:12)+
      annotate(geom="text", x=2, y=max-(max-min)*0.05, label="Median", color="darkred")+
      annotate(geom="text", x=2, y=max-(max-min)*0.10, label="Q25-Q75", color="gray25")+
      annotate(geom="text", x=2, y=max-(max-min)*0.15, label="Q10-Q90", color="gray60")+
      ggtitle(paste0("Average seasonal pattern of ",indicator_short_name," during the period 1960-2021"))
  }else if(indicator_short_name %in% c("conF","durF","numFreRW")){
    min <- min(data_reach_average$q10)
    max <- max(data_reach_average$q90)
    p9 <- ggplot()+
      geom_ribbon(data=data_reach_average, mapping=aes(x=month,ymin=q10,ymax=q90), fill="gray60", alpha=0.5) +
      geom_ribbon(data=data_reach_average, mapping=aes(x=month,ymin=q25,ymax=q75), fill="gray25", alpha=0.5) +
      geom_line(data_reach_average, mapping=aes(x=month, y=median), color="darkred")+
      theme_bw()+
      ylab(indicator_short_name)+xlab("Month")+
      scale_x_continuous(breaks=1:12)+
      annotate(geom="text", x=2, y=min+(max-min)*0.15, label="Median", color="darkred")+
      annotate(geom="text", x=2, y=min+(max-min)*0.10, label="Q25-Q75", color="gray25")+
      annotate(geom="text", x=2, y=min+(max-min)*0.05, label="Q10-Q90", color="gray60")+
      ggtitle(paste0("Average seasonal pattern of ",indicator_short_name," during the period 1960-2021"))
  }
  p9
}

# Spatial pattern yearly aggregated (projections)
figure_10 <- function(drn_short_name, drn_long_name, indicator_short_name, indicator_long_name, ssp, gcm){
  # read projections data
  shp_gcm_hist <- read_reach_gcm(drn_short_name, "Yearly", indicator_short_name, gcm,"","1985_2014") # read file
  shp_gcm_proj <- read_reach_gcm(drn_short_name, "Yearly", indicator_short_name, gcm, ssp,"2015_2100") # read file
  
  shp_gcm_hist[is.na(shp_gcm_hist)] <- 0 # set NA values to 0
  shp_gcm_proj[is.na(shp_gcm_proj)] <- 0 # set NA values to 0
  
  # read shapefile of river network
  shp <- read_shp(drn_short_name)
  
  # Computation of the mean annual conD indicator for the historical period
  shp_hist_average <- shp_gcm_hist %>% # for each reach computes the mean of all years
    rowwise() %>%
    mutate(avg_hist = mean(c_across(colnames(shp_gcm_hist)), na.rm=TRUE)) %>%
    select(avg_hist)
  shp_hist_average$cat <- as.numeric(rownames(shp_gcm_hist))
  
  # Computation of the mean annual conD indicator for the 2071-2100 period
  col_years <- colnames(shp_gcm_proj)[year(colnames(shp_gcm_proj)) %in% 2071:2100] # names of the columns with data for the 2041-2070 period
  shp_2071_2100_average <- shp_gcm_proj %>% # for each reach computes the mean of all years
    rowwise() %>%
    mutate(avg_2071_2100 = mean(c_across(col_years), na.rm=TRUE)) %>%
    select(avg_2071_2100)
  shp_2071_2100_average$cat <- as.numeric(rownames(shp_gcm_proj))
  
  # Group results in one sf object
  shp_res <- shp %>% left_join(shp_hist_average, by='cat')
  shp_res <- shp_res %>% left_join(shp_2071_2100_average, by='cat')
  
  # Difference of the numbers of dry days (conD) between the future and the historical period
  shp_res$delta_2070_hist <- shp_res$avg_2071_2100 - shp_res$avg_hist
  
  # plot data
  #shp_res$avg_hist[shp_res$avg_hist==0] <- NA # to display perennial reaches in grey
  p10a <- ggplot(shp_res, mapping=aes(color=avg_hist,
                                      data_id = cat,
                                      tooltip = round(avg_hist, digits = 1))) + 
    geom_sf_interactive(linewidth=0.8) + 
    theme_bw() +
    scale_color_viridis_c(name=indicator_short_name)+
    theme(legend.title=element_blank(),
          text = element_text(size=5)) +
    ggtitle("1985-2014")
  
  lmt <- max(abs(shp_res$delta_2070_hist), na.rm=TRUE)
  p10b <- ggplot(shp_res, mapping=aes(color=delta_2070_hist,
                                     data_id = cat,
                                     tooltip = round(delta_2070_hist, digits = 1))) + 
    geom_sf_interactive(linewidth=0.8) + 
    theme_bw() +
    theme(legend.title=element_blank(),
          text = element_text(size=5)) + 
    scale_color_stepsn(colors=c('#2166ac','#67a9cf','#d1e5f0',
                                '#f7f7f7','#fddbc7','#ef8a62','#b2182b'),
                       n.breaks=10, limits=c(-lmt,lmt), show.limits=T) +
    ggtitle("Difference between\n2071-2100 and 1985-2014")
  
  p10 <- list(p10a, p10b)
}

# Reach yearly time series (projections)
figure_11 <- function(drn_short_name, drn_long_name, indicator_short_name, indicator_long_name, reachID){
  # read projections data
  data <- read_one_reach_gcm(drn_short_name, "Yearly", indicator_short_name, reachID)
  data$date <- as.Date(data$date)
  
  data$value[is.na(data$value)] <- 0 # set NA values to 0
  
  # plot the long-term evolution of yearly indicator
  colors <- c("darkgrey",plasma(3))
  names(colors) <- unique(data$scenario)
  p11 <- ggplot(data, aes(x=date, y=value, color=scenario, group=interaction(model,scenario))) +
    geom_line()+
    theme_bw()+
    ylab(indicator_short_name)+xlab("")+
    scale_color_manual(name="SSP scenario",values = colors)+
    ggtitle(paste0("Evolution of mean annual ",indicator_short_name," under climate change scenarios"))
  p11
}

# Reach yearly by periods (projections)
figure_12 <- function(drn_short_name, drn_long_name, indicator_short_name, indicator_long_name, reachID, ssp_short_name){
  # read projections data
  data <- read_one_reach_gcm(drn_short_name, "Yearly", indicator_short_name, reachID)
  data$date <- as.Date(data$date)
  
  data$value[is.na(data$value)] <- 0 # set NA values to 0
  
  # Compare the evolution of yearly indicators for the 5 GCMs for one SSP scenario
  data_Y <- data %>%
    mutate(year=year(date),
           period=ifelse(year %in% 1985:2014,"1985-2014",
                         ifelse(year %in% 2041:2070,"2041-2070","2071-2100")))
  
  p12 <- ggplot(data_Y[data_Y$scenario %in% c("historical",ssp_short_name),], aes(x=model,y=value,fill=model))+
    geom_boxplot()+
    facet_wrap(. ~ period)+
    theme_bw()+
    ylab(indicator_short_name)+xlab("GCM")+
    scale_fill_viridis_d(name="GCM",option="turbo")+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
    ggtitle(paste0("Comparison of the distributions of annual ",indicator_short_name," during \nthe periods 1985-2014, 2041-2070, and 2071-2100, for the scenario ", ssp_short_name))
  p12 
}

# Reach seasonal pattern (projections)
figure_13 <- function(drn_short_name, drn_long_name, indicator_short_name, indicator_long_name, reachID){
  # read data projections
  data <- read_one_reach_gcm(drn_short_name, "Monthly", indicator_short_name, reachID)
  data$date <- as.Date(data$date)
  
  data$value[is.na(data$value)] <- 0 # set NA values to 0
  
  # plot the evolution of the seasonal pattern of RelInt
  data_M <- data %>%
    mutate(month=month(date),
           year=year(date),
           period=ifelse(year %in% 1985:2014,"1985-2014",
                                ifelse(year %in% 2041:2070,"2041-2070","2071-2100"))) %>%
    group_by(month, model, scenario, period) %>%
    summarise(value=mean(value)) %>%
    ungroup()
  data_M <- rbind(data_M[data_M$period %in% c("2041-2070","2071-2100"),],
                  data_M[!(data_M$period %in% c("2041-2070","2071-2100")),] %>% mutate(scenario = "ssp126"),
                  data_M[!(data_M$period %in% c("2041-2070","2071-2100")),] %>% mutate(scenario = "ssp370"),
                  data_M[!(data_M$period %in% c("2041-2070","2071-2100")),] %>% mutate(scenario = "ssp585")
  )
  
  p13 <- ggplot(data_M, aes(x=month,y=value,color=period,group=interaction(model,period)))+
    geom_line()+
    facet_wrap(. ~ scenario)+
    theme_bw()+
    ylab(indicator_short_name)+xlab("Month")+
    scale_x_continuous(breaks=1:12)+
    scale_color_manual(name="Period",
                       values = c("1985-2014 (GCMs)"="grey","2041-2070"="royalblue","2071-2100"="orange"))+
    ggtitle(paste0("Average seasonal pattern of ",indicator_short_name," under climate change scenarios"))
  p13
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
      addStyleEditor(position = "bottomright", 
                     openOnLeafletEditable = TRUE, 
                     openOnLeafletDraw = FALSE, useGrouping = FALSE) %>%
      addLayersControl(
        baseGroups = c("CartoDB", "Elevation", "Satellite","Without background"),
        options = layersControlOptions(collapsed=TRUE))%>%
    leaflet.extras2::addEasyprint(options = easyprintOptions(
      title = 'Print map',
      position = 'bottomright',
      exportOnly = TRUE, 
      sizeModes = "A4Landscape"))

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
         Difference: %g <br/>",
      shape_var$ID, 
      shape_var$diff) %>% 
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
                position = "topright") %>%
      leaflet.extras2::addEasyprint(options = easyprintOptions(
        title = 'Print map',
        position = 'bottomright',
        exportOnly = TRUE, 
        sizeModes = "A4Landscape"))
    
    
    
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
         Value: %g <br/>",
      shape_var$ID, 
      shape_var[[var]][,1]) %>% 
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
                opacity = 1,
                bins = 10,
                position = "topright") %>%
      leaflet.extras2::addEasyprint(options = easyprintOptions(
        title = 'Print map',
        position = 'bottomright',
        exportOnly = TRUE, 
        sizeModes = "A4Landscape"))
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


optimizing <- function(drn, var, blm, period){
  
  shape <- get_shape(drn)
  
  #pu dat-----------------------------------------------------------------------
  pu_dat = as.data.frame(shape)
  pu_dat <- pu_dat[,c("ID", "mean_AREA_SQKM")]
  colnames(pu_dat) <- c("id", "cost")
  pu_dat$id <- as.numeric(pu_dat$id)
  pu_dat$status <- 0
  
  #puvspr_dat-------------------------------------------------------------------
  set_var <- c()
  
  for(i in 1:length(var)){
    
    var_short = variables_short[which(variables_long == names(var)[i])]
    set_var <- c(set_var, var_short)
    
    if(i == 1){
      data <- get_bioinformation(var_short, drn, period)
      
      data_filtered <- data %>%
        filter(campaign == 6) %>%
        select(WP4, var_short)
    }
    else{
      data_aux <- get_bioinformation(var_short, drn, period)

      data_aux_filtered <- data_aux %>%
        filter(campaign == 6) %>%
        select(WP4, var_short)
      
      data_filtered = data_aux_filtered %>%
        left_join(data_filtered, by = "WP4")
    }
  }
  
  # print(data_filtered)

  # data_filtered <- data %>%
  #   filter(campaign == 6) %>%
  #   select(WP4, var)
  
  shape_var <- terra::merge(shape, data_filtered, by.x = "ID", by.y = "WP4", all.x=TRUE)
  
  puvspr_dat <- as.data.frame(shape_var)
  puvspr_dat <- puvspr_dat[, c("ID", set_var)]
  
  puvspr_dat <- puvspr_dat %>%
    tidyr::pivot_longer(!ID, names_to = "species", values_to = "amount")
  
  colnames(puvspr_dat) <- c("pu", "species", "amount")
  puvspr_dat$pu <- as.numeric(puvspr_dat$pu)
  
  puvspr_dat <- na.omit(puvspr_dat)
  print(puvspr_dat)
  
  
  #spec_dat---------------------------------------------------------------------
  names(var) = set_var
  
  spec_dat <- data.frame(id = seq(var),
                         name = names(var),
                         prop = unlist(var, use.names = FALSE))
  
  print(spec_dat)
  
  puvspr_dat <- puvspr_dat %>%
    left_join(spec_dat, by = c("species" = "name")) %>%
    select(pu, id, amount)
  colnames(puvspr_dat) <- c("pu", "species", "amount")
  

  # spec_dat <- data.frame(id = 1,
  #                        prop = target,
  #                        name = var)
  
  
  # colnames(puvspr_dat) <- c("pu", "amount")
  # puvspr_dat$pu <- as.numeric(puvspr_dat$pu)
  # puvspr_dat$species <- 1
  # puvspr_dat <- na.omit(puvspr_dat)
  
  # p <- prioritizr::problem(pu_dat, spec_dat, cost_column = "cost", rij = puvspr_dat) %>%
  #   add_min_set_objective() %>%
  #   add_relative_targets(0.1) %>%
  #   add_binary_decisions() %>%
  #   add_cbc_solver(gap = 0)
  
  #boundary---------------------------------------------------------------------
  boundary_dat <- read.table("data/data/STcon_pseudo_6.csv", sep = ";", header = TRUE)
  colnames(boundary_dat) <- c("id1", "id2", "boundary")
  
  p <- prioritizr::marxan_problem(pu_dat, spec_dat, puvspr_dat, boundary_dat, blm = blm) %>%
    add_cbc_solver(gap = 0)
  
  sol <- solve(p)

  
  update_map_opt(drn, sol, drn_country)
  
}



# Update the map using the information of Indicator
update_map_opt <- function(drn, sol, drn_country){
  
  shape <- get_shape(drn)

  data_filtered <- sol %>%
    select(id, solution_1)
  
  shape_var <- terra::merge(shape, data_filtered, by.x = "ID", by.y = "id", all.x=TRUE)
  
  
  ##palette
  levels(shape_var$solution_1) <- c(0,1)
  pal <- colorFactor(palette="Reds", domain=shape_var[["solution_1"]][,1], na.color="orange")
  #pal <- colorBin(palette="Blues", domain=data[,solution_1], na.color="orange")
  title_legend <- ""
  
  
  popup_map <- sprintf(
    "<strong>ID %s</strong><br/>
         Value: %g <br/>",
    shape_var$ID, 
    shape_var[["solution_1"]][,1]) %>% 
    lapply(htmltools::HTML)
  
  leafletProxy("map_opt", data = shape_var) %>%
    clearControls() %>%
    addPolylines(group = "Base",
                 weight = 3, 
                 color = ~pal(shape_var[["solution_1"]][,1]),
                 fill = FALSE, 
                 opacity = 1,
                 popup = popup_map, 
                 layerId = ~ID, 
                 highlightOptions = highlightOptions(
                   weight = 5,
                   bringToFront = TRUE,
                   sendToBack = TRUE)
    )%>%
    addLegend(group = "Base",
              pal = pal,
              title = title_legend,
              values = shape_var[["solution_1"]][,1],
              opacity = 1,
              position = "topright") %>%
    leaflet.extras2::addEasyprint(options = easyprintOptions(
      title = 'Print map',
      position = 'bottomright',
      exportOnly = TRUE, 
      sizeModes = "A4Landscape"))
}
