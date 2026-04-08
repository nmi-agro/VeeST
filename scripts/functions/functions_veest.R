# Import funs ------------------------------------
## load rtkgps data dxf format -----------------------------------------------------
importGPS <- function(inputdir = paste0(workspace,"./GPS")){
  # load all results from aquo kit from input dir
  gps <- list.files(path= paste0(inputdir), pattern=".dxf", full.names =  T)
  names <- gsub(paste0(inputdir,'/'), "", gps)
  names <- gsub(paste0('.dxf'), "", names)
  dt.names <- data.table(ID = 1:length(names), name = names)
  gps <- lapply(gps, st_read, as_tibble = FALSE, stringsAsFactors = TRUE)
  gps <- rbindlist(gps, fill =T, use.names = T, idcol = "ID")
  gps <- merge(dt.names, gps , by = "ID")
  gps <- st_as_sf(gps, crs = 28992)
  gps <- st_zm(gps, crs = 28992)
  return(gps)
}
## load gps in csv format ----------------------------------------------------------
importGPS2 <- function(inputdir = paste0(workspace,"./GPS"), gpsid = 36){
  # load all results from aquo kit from input dir
  gps2 <- list.files(path= paste0(inputdir), pattern=".csv", full.names =  T)
  names <- gsub(paste0(inputdir,'/'), "", gps2)
  names <- gsub(paste0('.csv'), "", names)
  dt.names <- data.table(ID = 1:length(names), name = names)
  gps2 <- lapply(gps2, fread, stringsAsFactors = FALSE)
  gps2 <- rbindlist(gps2, fill =T, use.names = T, idcol = "ID")
  gps2 <- merge(dt.names, gps2 , by = "ID")
  gps2[,ID := ID+gpsid]
  gps2 <- st_as_sf(gps2, coords = c('x','y','z'), crs = 28992)
  return(gps2)
}
## load gps profielen     ----------------------------------------------------------
importGPSprof <- function(inputdir = paste0(workspace,"./GPS slootprofielen"), gpsid = 36){
  # load all results from aquo kit from input dir
  prof <- list.files(path= paste0(inputdir), pattern=".csv", full.names =  T)
  names <- gsub(paste0(inputdir,'/'), "", prof)
  names <- gsub(paste0('.csv'), "", names)
  dt.names <- data.table(ID = 1:length(names), name = names)
  prof <- lapply( prof, fread, stringsAsFactors = FALSE)
  prof <- rbindlist( prof, fill =T, use.names = T, idcol = "ID")
  prof <- merge(dt.names,  prof , by = "ID")
  prof <- st_as_sf(prof, coords = c('x','y','z'), crs = 28992, remove = FALSE)
  return(prof)
}
## Import penetrometerdata in txt format -------------------------------------------
importPen <- function(inputdir = paste0(workspace,"./Penetrometer")){
  
  pen <- list.files(path= paste0(inputdir), pattern=".TXT", full.names =  T)
  
  # include filesname
  names <- gsub(paste0(inputdir,'/'), "", pen)
  names <- gsub(paste0('.TXT'), "", names)
  dt.names <- data.table(ID = 1:length(names), name = names)
  # include metadata
  penheader <- lapply(pen, fread, sep=';', nrows = 13)
  penheader <- rbindlist(penheader, fill =F, use.names = F, idcol = "ID")
  penheader <- penheader[,metadata:= sapply(strsplit(`INSTRUMENT EIJKELKAMP PENETROLOGGER SN       0`,':'), `[`,2)]
  penheader <- penheader[,header:= sapply(strsplit(`INSTRUMENT EIJKELKAMP PENETROLOGGER SN       0`,':'), `[`,1)]
  penheader <- penheader[!is.na(metadata),]
  # trim spaces
  penheader <- penheader[,metadata := gsub(' $','',metadata)]
  penheader <- penheader[,header := gsub(' $','',header)]
  # trim character columns from starting and ending space
  cols <- colnames(penheader)[sapply(penheader, is.character)] # which colnames are character
  penheader <- penheader[,(cols) := lapply(.SD, function(x) gsub("^\\s+|\\s+$", "", x)),.SDcols = cols]
  penheader <- data.table(ID = penheader$ID, header = rep(penheader$header[penheader$ID == 1], uniqueN(penheader$ID)), metadata = penheader$metadata)
  penheader <- dcast(penheader, ID ~ header, value.var = 'metadata')
  penheader <- merge(penheader, dt.names, by = 'ID')
  # import data
  pen <- lapply(pen, fread, sep=';', skip = 14)
  pen <- rbindlist(pen, fill =F, use.names = F, idcol = "ID")
  pen <- pen[, c("Diept", "Pen01" ,"Pen02" ,"Pen03" ,"Pen04", "Pen05", "Pen06", "Pen07", "Pen08", "Pen09" ,"Pen10") := tstrsplit(`Diept	Pen01	Pen02	Pen03	Pen04	Pen05	Pen06	Pen07	Pen08	Pen09	Pen10`, " ", fixed=TRUE)]
  
  # merge with metadata
  pen <- merge(pen, penheader, by = 'ID')
  pen <- pen[,-c('Diept	Pen01	Pen02	Pen03	Pen04	Pen05	Pen06	Pen07	Pen08	Pen09	Pen10')]
  
  # convert character 2 number
  changeCols <- colnames(pen[,2:12])
  pen <- pen[,(changeCols):= lapply(.SD, as.numeric), .SDcols = changeCols]
  
  # change format
  pen <- melt.data.table(pen, id.vars = c('ID','Plotnaam','name','Diept','Conustype','Plotdatum','Penetratie snelheid'), 
                          measure.vars = c( "Pen01" ,"Pen02" ,"Pen03" ,"Pen04", "Pen05", "Pen06", "Pen07", "Pen08", "Pen09" ,"Pen10"),
                          variable.name = 'Pen', value.name = 'indringingsweerstand')
  
  return(pen)
}

importPen2 <- function(inputdir = paste0(workspace,"./Penetrometer")){
  
  pen <- list.files(path= paste0(inputdir), pattern=".txt", full.names =  T)
  
  # include filesname
  names <- gsub(paste0(inputdir,'/'), "", pen)
  names <- gsub(paste0('.txt'), "", names)
  dt.names <- data.table(ID = 1:length(names), name = names)
  # include metadata
  penheader <- lapply(pen, fread, sep=';', nrows = 13)
  penheader <- rbindlist(penheader, fill =F, use.names = F, idcol = "ID")
  penheader <- penheader[,metadata:= sapply(strsplit(`Eijkelkamp Penetro Viewer Vs. 6.08`,':'), `[`,2)]
  penheader <- penheader[,header:= sapply(strsplit(`Eijkelkamp Penetro Viewer Vs. 6.08`,':'), `[`,1)]
  penheader <- penheader[!is.na(metadata),]
  # trim spaces
  penheader <- penheader[,metadata := gsub(' $','',metadata)]
  penheader <- penheader[,header := gsub(' $','',header)]
  # Trim character columns from starting and ending space
  cols <- colnames(penheader)[sapply(penheader, is.character)]
  penheader <- penheader[, (cols) := lapply(.SD, function(x) gsub("^\\s+|\\s+$", "", x)), .SDcols = cols]
  penheader <- data.table(ID = penheader$ID, header = rep(penheader$header[penheader$ID == 1], uniqueN(penheader$ID)), metadata = penheader$metadata)
  penheader <- dcast(penheader, ID ~ header, value.var = 'metadata')
  penheader <- merge(penheader, dt.names, by = 'ID')
  # import data
  pen <- list.files(path= paste0(inputdir), pattern=".txt", full.names =  T)
  pen <- lapply(pen, fread, skip = 13, na.strings = c("", "NA", -1, "NaN"))
  pen <- rbindlist(pen, fill =F, use.names = F, idcol = "ID")
  rename_pen_columns <- function(pen_data) {
    # Kolommen hernoemen
    # Eerste 4 kolommen: ID, penplot, coords, start = 0
    # Rest: 00, 01, 02, ..., 80
    
    n_depth_cols <- ncol(pen_data) - 4
    new_names <- c("ID", "penplot", "coords", "start", 
                   sprintf("%02d", 0:(n_depth_cols - 1)))
    
    setnames(pen_data, new_names)
    pen_data
  }
  pen <- rename_pen_columns(pen)
  pen <-pen[,-  c('coords','start')]
  # Melt: zet alle dieptekolommen in 1 kolom zoals eerder format
  pen_long <- melt(pen, 
                   id.vars = c("ID", "penplot"),
                   measure.vars = sprintf("%02d", 0:80),
                   variable.name = "Diept",
                   value.name = "waarde")

  # merge with metadata
  pen <- merge(pen_long, penheader, by = 'ID')
  
  # change format
  pen <- pen[, c("Plotnaam", "Pen") := tstrsplit(penplot, "\\.", type.convert = TRUE)]
  pen <- pen[, indringingsweerstand:= as.numeric(waarde)]
  
  return(pen)
}

# Proces funs-----------------------
# get wind direction
get_cardinal_direction <- function(profiel_nr) {
  #copy the transect of the oever
  dt1 <- setDT(profiel_nr)
  
  #select first and last point
  first_point <- dt1[Puntnummer == min(Puntnummer)] |> st_as_sf() |> st_transform(4326) 
  last_point <- dt1[Puntnummer == max(Puntnummer)] |> st_as_sf() |> st_transform(4326)
  
  #calculate the azimuth in degrees
  azimuth <- st_azimuth(first_point, last_point)
  
  if (azimuth < 0) {
    azimuth <- azimuth + 360  # Ensure azimuth is positive
  }
  
  #recalculate to a direction
  if (azimuth >= 0 && azimuth < 22.5){
    transect_direction <- "Noord"
  }
  if (azimuth >= 22.5 && azimuth < 67.5){
    transect_direction <- "Noordoost"
  }
  if (azimuth >= 67.5 && azimuth < 112.5){
    transect_direction <- "Oost"
  }
  if (azimuth >= 112.5 && azimuth < 157.5){
    transect_direction <- "Zuidoost"
  }
  if (azimuth >= 157.5 && azimuth < 202.5){
    transect_direction <- "Zuid"
  }
  if (azimuth >= 202.5 && azimuth < 247.5){
    transect_direction <- "Zuidwest"
  }
  if (azimuth >= 247.5 && azimuth < 292.5){
    transect_direction <- "West"
  }
  if (azimuth >= 292.5 && azimuth < 337.5){
    transect_direction <- "Noordwest"
  }
  if (azimuth >= 337.5 && azimuth < 361){
    transect_direction <- "Noord"
  }
  
  #return
  return(transect_direction)
}

# calculate different slopes
calc_taludhoek <- function(profiel_nr){
  #copy the trasnect of the oever
  dt1 <- setDT(profiel_nr)

  #select first and last point first shore (3 meters from shoreline)
  min_dist <- min(dt1[Opmerking == "waterlijn",'dist']) - 3
  first_point <- dt1[sectie == 'oever' & dist > min_dist] 
  first_point <- first_point[Puntnummer == min(Puntnummer)]
  last_point <- dt1[sectie == 'water']
  last_point <- last_point[Puntnummer == min(Puntnummer)]
  # calc angle
  tldk_bvwtr_perc_1 <-  100*(first_point$z-last_point$z) / (last_point$dist-first_point$dist)
  tldk_bvwtr_graden_1 <-  atan((first_point$z-last_point$z) / (last_point$dist-first_point$dist))*(180/pi)
  
  #select first and last point second shore
  max_dist <- max(dt1[Opmerking == "waterlijn",'dist']) + 3
  first_point <- dt1[sectie == 'oever' & dist < max_dist]
  first_point <- first_point[Puntnummer == max(Puntnummer)]  
  last_point <- dt1[sectie == 'water']
  last_point <- last_point[Puntnummer == max(Puntnummer)] 
  # calc angle
  tldk_bvwtr_perc_2 <-  100*(first_point$z-last_point$z) / (first_point$dist-last_point$dist)
  tldk_bvwtr_graden_2 <-  atan((first_point$z-last_point$z) / (first_point$dist-last_point$dist))*(180/pi)
  
  #select first and last point first shoreline
  first_point <- dt1[sectie == 'water'] 
  first_point <- first_point[Puntnummer == min(Puntnummer)] 
  last_point <- dt1[sectie == 'water' & dist - first_point$dist < 1]
  # selecteer een locatie minder dan 1 meter verder dan de waterlijn
  last_point <- last_point[Puntnummer == max(Puntnummer)] 
  # calc angle bovenkant slib
  tldk_ondwtr_perc_1 <-  100*(first_point$z-last_point$z) / (last_point$dist-first_point$dist)
  tldk_ondwtr_graden_1 <-  atan((first_point$z-last_point$z) / (last_point$dist-first_point$dist))*(180/pi) 
  # calc angle onderkant slib
  tldk_vastbodem_perc_1 <-  100*((first_point$z-first_point$slib) -(last_point$z-last_point$slib)) / (last_point$dist - first_point$dist)
  tldk_vastbodem_graden_1 <-  atan(((first_point$z-first_point$slib) -(last_point$z-last_point$slib)) / (last_point$dist-first_point$dist))*(180/pi) 
  
  #select first and last point second shoreline
  first_point <- dt1[sectie == 'water'] 
  first_point <- first_point[Puntnummer == max(Puntnummer)]  
  last_point <- dt1[sectie == 'water' & first_point$dist - dist < 1]
  # selecteer een locatie minder dan 50 cm verder dan de waterlijn
  last_point <- last_point[Puntnummer == min(Puntnummer)] 
  # calc angle
  tldk_ondwtr_perc_2 <-  100*(first_point$z-last_point$z) / (first_point$dist-last_point$dist)
  tldk_ondwtr_graden_2 <-  atan((first_point$z-last_point$z) / (first_point$dist-last_point$dist))*(180/pi) 
  # calc angle onderkant slib
  tldk_vastbodem_perc_2 <-  100*((first_point$z-first_point$slib) -(last_point$z-last_point$slib)) / (first_point$dist-last_point$dis)
  tldk_vastbodem_graden_2 <-  atan(((first_point$z-first_point$slib) -(last_point$z-last_point$slib)) / (first_point$dist-last_point$dis))*(180/pi) 
  
  #select first and last point watersurface
  waterlijn <- dt1[sectie == 'water'] 
  waterlijn <- waterlijn[Puntnummer == min(Puntnummer)] # waterlijn sectie 1
  last_point <- dt1[z > waterlijn$z - 0.35 & sectie == 'water' & sectie_2 == 1]
  last_point <- last_point[Puntnummer == max(Puntnummer)]
  first_point <- dt1[z < waterlijn$z +0.35 & sectie == 'oever' & sectie_2 == 1]
  first_point <- first_point[Puntnummer == min(Puntnummer)]
  # calc angle
  tldk_wtrwtr_perc_1 <-  100*(waterlijn$z-last_point$z) / (last_point$dist-waterlijn$dist) # hoek rond waterlijn, het water in 
  tldk_oevrwtr_perc_1 <-  100*(first_point$z-waterlijn$z) / (waterlijn$dist-first_point$dist) # hoek rond waterlijn, oever op
  
  #select first and last point watersurface
  waterlijn <- dt1[sectie == 'water'] 
  waterlijn <- waterlijn[Puntnummer == max(Puntnummer)] # waterlijn sectie 1
  last_point <- dt1[z > waterlijn$z -0.35 & sectie == 'water' & sectie_2 == 2]
  last_point <- last_point[Puntnummer == min(Puntnummer)]
  first_point <- dt1[z < waterlijn$z +0.35 & sectie == 'oever' & sectie_2 == 2]
  first_point <- first_point[Puntnummer == max(Puntnummer)]
  # calc angle
  tldk_wtrwtr_perc_2 <-  100*(waterlijn$z-last_point$z) / (waterlijn$dist-last_point$dist) # hoek rond waterlijn, het water in 
  tldk_oevrwtr_perc_2 <-  100*(first_point$z-waterlijn$z) / (first_point$dist-waterlijn$dist) # hoek rond waterlijn, oever op
  
    # Check if any variables are zero-length and replace with NA_real_
  if(length(tldk_bvwtr_perc_1) == 0) tldk_bvwtr_perc_1 <- NA_real_
  if(length(tldk_bvwtr_perc_2) == 0) tldk_bvwtr_perc_2 <- NA_real_
  if(length(tldk_ondwtr_perc_1) == 0) tldk_ondwtr_perc_1 <- NA_real_
  if(length(tldk_ondwtr_perc_2) == 0) tldk_ondwtr_perc_2 <- NA_real_
  if(length(tldk_wtrwtr_perc_1) == 0) tldk_wtrwtr_perc_1 <- NA_real_
  if(length(tldk_wtrwtr_perc_2) == 0) tldk_wtrwtr_perc_2 <- NA_real_
  if(length(tldk_oevrwtr_perc_1) == 0) tldk_oevrwtr_perc_1 <- NA_real_
  if(length(tldk_oevrwtr_perc_2) == 0) tldk_oevrwtr_perc_2 <- NA_real_
  if(length(tldk_vastbodem_perc_1) == 0) tldk_vastbodem_perc_1 <- NA_real_
  if(length(tldk_vastbodem_perc_2) == 0) tldk_vastbodem_perc_2 <- NA_real_
  
  talud <- data.table(tldk_bvwtr_perc_1,tldk_bvwtr_perc_2,
             tldk_ondwtr_perc_1,tldk_ondwtr_perc_2,
             tldk_wtrwtr_perc_1, tldk_wtrwtr_perc_2,
             tldk_oevrwtr_perc_1,  tldk_oevrwtr_perc_2,
             tldk_vastbodem_perc_1,tldk_vastbodem_perc_2,
             keep.rownames=T)
  print(unique(dt1$name))
  return(talud)

}

# Utility: compute R-squared between two variables in a data.frame / data.table
# returns NA_real_ if columns are missing or insufficient observations
get_r_squared <- function(dt, x_col, y_col) {
  # accept data.frame or data.table
  if (!is.data.frame(dt)) return(NA_real_)
  if (!(x_col %in% names(dt) && y_col %in% names(dt))) return(NA_real_)
  x <- dt[[x_col]]
  y <- dt[[y_col]]
  ok <- !is.na(x) & !is.na(y)
  if (sum(ok) < 3) return(NA_real_)
  # fit simple linear model on the vectors
  fit <- try(stats::lm(y ~ x), silent = TRUE)
  if (inherits(fit, "try-error")) return(NA_real_)
  s <- summary(fit)$r.squared
  return(as.numeric(s))
}

# Visualise profielen-------------------------------------------------------------
visualise_profiel<- function(proftest){
  # i <- unique(profiel$ID)[181]
  proftest <- profiel[ID == i,]
  # proftest <- proftest[!is.na(SlootID),]
  setDT(proftest)
  proftest <- proftest[sectie %in% c('oever','water')]
  
  zmin <- min(proftest$z-proftest$slib)

  ggplot() +
    geom_line(data = proftest, aes(x = midpoint_dist, y = z)) +
    geom_ribbon(data=proftest, aes(x= midpoint_dist,ymin=zmin, ymax=z-slib), fill = 'lightgreen', alpha =0.5)+
    geom_line(data = proftest, aes(x = midpoint_dist, y = z-slib)) +
    geom_ribbon(data=proftest, aes(x= midpoint_dist,ymin=z-slib, ymax=z), fill = 'brown', alpha =0.5)+
    geom_line(data = proftest[!is.na(wl),], aes(x = midpoint_dist, y = wl)) + 
    geom_ribbon(data=proftest[!is.na(wl),], aes(x= midpoint_dist, ymin=z, ymax=wl), fill = 'lightblue', alpha =0.5)+
    coord_fixed(ratio = 1)+
    # annotate('text', x = -4, y = -0.3,label = proftest$azimuth) +
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size=15), 
      strip.text.y = element_text(size = 15), 
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size= 15),
      axis.ticks =  element_line(colour = "black"),
      plot.title = element_text(size =15, face="bold", hjust = 0.5),
      panel.background = element_blank(),
      plot.background = element_blank(),
    )+
    ggtitle(paste0("Dwarsprofiel oever en sloot op locatie ",proftest$name)) +
    labs(x= "afstand in meters",y="diepte in mNAP")
  
  ggsave(file=paste0('/output/',unique(proftest$gebied[!is.na(proftest$gebied)]),'/profielen/profiel_',unique(proftest$SlootID[!is.na(proftest$gebied)]),"_",unique(proftest$ID[!is.na(proftest$gebied)]),'.png'),width = 25,height = 10,units='cm',dpi=800)
  
}

# Function to calculate RMSE as percentage of mean
calculate_rmse_percentage <- function(data, target_var, predictors) {
  # Select variables for model
  model_vars <- c("SlootID", target_var, predictors)
  model_data <- data[complete.cases(data[, ..model_vars]), ..model_vars]
  
  # Convert factors to numeric
  factor_cols <- names(model_data)[sapply(model_data, is.character)]
  factor_cols_2 <- names(model_data)[sapply(model_data, is.factor)]
  factor_cols <- c(factor_cols, factor_cols_2)
  if(length(factor_cols) > 0) {
    model_data[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]
    model_data[, (factor_cols) := lapply(.SD, as.numeric), .SDcols = factor_cols]
  }
  
  # Get model
  model <- xgb_models[[target_var]]
  
  # Prepare X data
  predictors_clean <- predictors[predictors %in% colnames(model_data)]
  X_data <- as.matrix(model_data[, ..predictors_clean])
  y_actual <- model_data[, get(target_var)]
  
  # Get predictions and calculate RMSE
  y_pred <- predict(model, X_data)
  rmse <- sqrt(mean((y_actual - y_pred)^2, na.rm = TRUE))
  
  # Calculate as percentage of mean
  mean_val <- mean(y_actual, na.rm = TRUE)
  rmse_pct <- (rmse / mean_val) * 100
  
  rmse_pct
}

# Function to get correlation direction between predictor and target
get_correlation_direction <- function(target_var, predictor_var, data) {
  corr <- cor(data[[target_var]], data[[predictor_var]], use = "complete.obs")
  ifelse(corr > 0, "+", "-")
}
