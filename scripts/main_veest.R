
# 1. Load packages -----------------------------------------------------------
library(data.table)
library(sf)
library(R.utils)
library(dplyr)
library(ggplot2)
library(nngeo) #azimuth
library(readxl)
library(plotly)
library(RColorBrewer)

# 2. Settings ----------------------------------------------------------------
workspace <- paste0(Sys.getenv("NMI-SITE"), 'O 1900 - O 2000/1922.N.23 VeeST vwsloot vd toekomst/05. Data/')

# Load custom functions-----------------------------------------------------
source(paste0("scripts/functions/functions_veest.R"))

# 3. Load hulpdata -------------------------------------------------
## 3.1 clusters ---------------------------------------
cluster <- read_sf(paste0(workspace,"./GIS/clusters_versie20240318.gpkg"))
aan <- read_sf(paste0(workspace,"./GIS/AAN_niveau3b.shp"))
## 3.2. Load was/ wordt locaties-----------------------------------------
locaties <- readxl::read_excel(paste0(workspace, 'GIS/wp_locaties_naam_correcties.xlsx'))
setDT(locaties)
locaties[,gebied := sapply(strsplit(SlootID, '_'), `[`, 1)]
locaties[,sloot := sapply(strsplit(SlootID, '_'), `[`, 2)]
locaties[,behandeling := sapply(strsplit(SlootID, '_'), `[`, 3)]
locaties[,oever := sapply(strsplit(SlootID, '_'), `[`, 4)]
locaties[,SlootID_kort := paste0(gebied, '_',sloot, '_',behandeling)]
# locaties$geom <- sprintf("LINESTRING(%s %s, %s %s)", locaties$Start_traject_long,locaties$Start_traject_lat,locaties$End_traject_long,locaties$End_traject_lat)
locaties$geom <- sprintf("LINESTRING(%s %s, %s %s)", locaties$Start_traject_lat,locaties$Start_traject_long,locaties$End_traject_lat,locaties$End_traject_long)
locaties <- locaties[geom == "LINESTRING(NA NA, NA NA)", geom:= "LINESTRING(0 0, 0 0)" ]
locaties <- st_as_sf(locaties, wkt = "geom", crs = 4326)
locaties <- st_as_sf(locaties) %>% st_transform(crs = 28992)
setDT(locaties)
## 3.3 create cluster per loc data---------------------------
locaties <- st_as_sf(locaties) %>% st_transform(crs = 28992)
clusters_locs <- st_join(locaties, cluster, st_nearest_feature)
# sel verschillende indicatoren
# afwatopp: oppvl/ (omtrek_nat/ 2) brede percelen met weinig sloten is een hoog getal, smalle percelen met veel sloten is laag
clusters_locs <- unique(clusters_locs[,c('SlootID','clusters','trofie','afwatopp','drlg','breedtewl','text')])
clusters_locs <- st_join(clusters_locs, aan, st_nearest_feature)
# 3.4 load loc correctie oeverzijde------------------------------------
gpsoevers <- st_read(paste0(workspace,'gps_penetrometer_oevers.gpkg'))

# 4. Penetrometer----------------------------------------------
## import----------------------------------------------
### import gps dxf files------------------------------------------------------
gps <-  importGPS(inputdir = paste0(workspace,"./GPS"))
gps <- st_collection_extract(gps, type = c("POINT"))
### proces dxf files ---------------------------------------------------------
setDT(gps)
gps[,Text := tolower(Text)]
gps[,Puntnummer := rep(seq_len(.N), each = 15, length.out = .N)]
gps<- gps[,Text := as.character(Text)]
# remove empty columns
gps <- gps[,-c('PaperSpace','SubClasses','Linetype')]
# create gps table with missing penetrometer ref
gps1 <- gps
# filter gps with penetrometer ref
gps <- gps[Layer == 'apglos_atts' & !is.na(Text) & !Text == "" & grepl('p', Text),]
# create gps table with missing penetrometer ref
gps1 <- gps1[!(ID %in% unique(gps$ID) & Puntnummer %in% unique(gps$Puntnummer)),]
gps1 <- gps1[Layer == 'apglos_att_nr',]
# RH_1_noord zou moeten starten bij plot8pen2, maar deze is er niet in de set van die datum, dus deze ontbreekt
gps1[name == 'Rh_10_M_Af' & EntityHandle == 'AAA33', Opmerking := 'plot4pen8']
gps1[name == 'Rh_10_M_Af' & EntityHandle == 'AAA50', Opmerking := 'plot4pen9']
gps1[name == 'Rh_10_M_Af' & EntityHandle == 'AAA67', Opmerking := 'plot4pen10']
gps1[name == 'Rh_10_M_Af' & EntityHandle == 'AAA84', Opmerking := 'plot5pen1']
gps1[name == 'Rh_10_M' & EntityHandle == 'AAA33', Opmerking := 'plot5pen2']
gps1[name == 'Rh_10_M' & EntityHandle == 'AAA50', Opmerking := 'plot5pen3']
gps1[name == 'Rh_10_M' & EntityHandle == 'AAA67', Opmerking := 'plot5pen4']
gps1[name == 'Rh_10_M' & EntityHandle == 'AAA84', Opmerking := 'plot5pen5']
gps1[name == 'Rh_10_M' & EntityHandle == 'AAA101', Opmerking := 'plot5pen6']
gps1[name == 'Rh_10_R' & EntityHandle == 'AAA50', Opmerking := 'plot5pen7']
gps1[name == 'Rh_10_R' & EntityHandle == 'AAA67', Opmerking := 'plot5pen8']
gps1[name == 'Rh_10_R' & EntityHandle == 'AAA84', Opmerking := 'plot5pen9']
gps1[name == 'Rh_10_R' & EntityHandle == 'AAA101', Opmerking := 'plot5pen10']
gps1[name == 'Rh_7_M_Af_Kr' & EntityHandle == 'AAA33', Opmerking := 'plot9pen3']
gps1[name == 'Rh_7_M_Af_Kr' & EntityHandle == 'AAA50', Opmerking := 'plot9pen4']
gps1[name == 'Rh_7_M_Af_Kr' & EntityHandle == 'AAA67', Opmerking := 'plot9pen5']
gps1[name == 'Rh_7_M_Af_Kr' & EntityHandle == 'AAA84', Opmerking := 'plot9pen6']
gps1[name == 'Rh_7_M_Af_Kr' & EntityHandle == 'AAA101', Opmerking := 'plot9pen7']
gps1[name == 'Rh_4_M_Af' & EntityHandle == 'AAA33', Opmerking := 'plot5pen7']
gps1[name == 'Rh_4_M_Af' & EntityHandle == 'AAA50', Opmerking := 'plot5pen8']
gps1[name == 'Rh_4_M_Af' & EntityHandle == 'AAA67', Opmerking := 'plot5pen9']
gps1[name == 'Rh_4_M_Af' & EntityHandle == 'AAA84', Opmerking := 'plot5pen10']
gps1[name == 'Rh_4_M_Af' & EntityHandle == 'AAA101', Opmerking := 'plot6pen1']
gps1[name == 'Rh_4_M_Af' & EntityHandle == 'AAA118', Opmerking := 'plot6pen2']
gps1[name == 'Sw_3_M' & EntityHandle == 'AAA33', Opmerking := 'plot4pen4']
gps1[name == 'Sw_3_M' & EntityHandle == 'AAA50', Opmerking := 'plot4pen5']
gps1[name == 'Sw_3_M' & EntityHandle == 'AAA67', Opmerking := 'plot4pen6']
gps1 <- gps1[!is.na(Opmerking),]
gps[,Opmerking := Text]
gps <- rbind(gps,gps1)

# correct data: filter dubbele coördinaten in Zegveld eruit
gps <- setorder(gps, ID)
gps <- gps[!(name %in% c('ZG3c','ZG3b','ZG3a') & EntityHandle %in% unique(gps$EntityHandle[name == 'ZG3d'])),]
gps <- gps[!(name %in% c('ZG3b','ZG3a') & EntityHandle %in% unique(gps$EntityHandle[name == 'ZG3c'])),]
gps <- gps[!(name %in% c('ZG3b') & EntityHandle %in% unique(gps$EntityHandle[name == 'ZG3a'])),]
gps <- gps[!(name %in% c('ZG2a') & EntityHandle %in% unique(gps$EntityHandle[name == 'ZG1b'])),]
gps <- gps[!(name %in% c('ZG1b') & EntityHandle %in% unique(gps$EntityHandle[name == 'ZG1a'])),]

# add additional info
# rondehoep sloot 1 tm 4, 7, 8, 10, zegveld 1 en 2, spaarnwoude is tweezijdig of twee transecten
setDT(gps)
gps[,trajecten := 2]

# correctie data: correct typos
gps[name == 'Rh_3_R_Kr' & EntityHandle == 'AAA193', Opmerking := 'plot4pen3']
gps[name == 'Rh_3_R_Kr' & EntityHandle == 'AAA23', Opmerking := 'plot3pen3']
gps[name == 'Sw_2_M' & EntityHandle == 'AAA125', Opmerking := 'plot4pen3']
gps[name == 'ZG1a' & EntityHandle == 'AAA772', Opmerking := 'plot1pen3']
gps[name == 'Sw_1_M' & EntityHandle == 'AAA125', Opmerking := 'plot2pen1']
gps[name == 'ZG3c' & EntityHandle == 'AAA143', Opmerking := 'plot19pen2']

# voeg volgnummer toe voor bepaling afstand tot sloot
# 1 = perceel, 2 = insteek, 3 tm 12 = oever en insteek en perceel andere oeverzijde
gps <- gps[,dist_id := frank(EntityHandle, ties.method = 'dense'), by = 'ID']

### import gps csv data--------------------------------------------------------
gps2 <-  importGPS2(inputdir = paste0(workspace,"./GPS"), gpsid = max(gps$ID))
gps3 <- st_transform(gps2[gps2$name == 'KW_add', ], crs = 28992)

setDT(gps2)
gps2[,trajecten := 1]
# correctie handmatig ingevoerd data
gps2[name == 'KW_add', name:= Laagnaam]
#correctie logging pendata
gps2[name == 'KW_1_WP1' & Puntnummer == 1, Opmerking := 'plot3pen4']
gps2[name == 'RH_11_R_n' & Puntnummer == 3, Opmerking := 'plot3pen1']
gps2[name == 'SW_5_M_z' & Puntnummer == 3, Opmerking := 'plot1pen8']
gps2[name == 'SW_4_M_zo' & Puntnummer == 3, Opmerking := 'plot3pen1']
gps2[name == 'SW_4_M_zo' & Puntnummer == 4, Opmerking := 'plot3pen2']
gps2[name == 'SW_4_M_zo' & Puntnummer == 5, Opmerking := 'plot3pen3']
gps2[name == 'SW_5_M_z' & Puntnummer == 3, Opmerking := 'plot1pen8']

# voeg volgnummer toe voor bepaling afstand tot sloot
# 1 = perceel, 2 = insteek, 3 tm 12 = oever en insteek en perceel andere oeverzijde
gps2 <- gps2[,dist_id := frank(Puntnummer, ties.method = 'dense'), by = 'ID']

### merge csv and dxf--------------------------------------------------------
gps <- gps[,c('ID','name','Opmerking','trajecten','dist_id','geometry')]
gps2 <- gps2[,c('ID','name','Opmerking','trajecten','dist_id','geometry')]
gps <- rbind(gps,gps2)
# create id for filtering
coords <- data.frame(st_coordinates(st_zm(gps$geometry)))
gps <- cbind(gps,coords)
gps[,ident:= paste0(X,'_',Y)]

### import penetrometer data ----------------------------------------------
pen <- importPen(inputdir = paste0(workspace,"./Penetrometer"))
pen <- pen[!is.na(indringingsweerstand),]
# make unique name 4 matching
pen[ ,plot:= as.numeric(gsub("^PLOTX*", "", Plotnaam))]
pen[ ,pen := as.numeric(gsub("^Pen*", "", as.character(Pen)))]
pen[ ,plot:= paste0('plot',plot,'pen',pen)]
pen[ ,gebied:= tstrsplit(name, "_")[1]]
pen[ ,gebied:=tolower(gebied)]
pen[ ,extragebied:= tstrsplit(name, "_")[3]]
pen[ ,extragebied:= gsub('[<>.]','',extragebied)]

# correct files with multiple gebieden
pen[name == 'MB_plot2_wp1' & plot %in% c('plot2pen3','plot2pen4','plot2pen5'), gebied := 'sv']
pen[name == 'MB_plot2_wp1' & plot %in% c('plot2pen3','plot2pen4','plot2pen5'), extragebied := 'sv_wp1']

# proces gps files

## proces ----------------------------------------------
### process gps files--------------------------------------------------------
## add location info for filtering and correcting
gps[, gebied:= tstrsplit(name, "_")[1]]
gps[, sloot:= tstrsplit(name, "_")[2]]
gps[ID %in% 29:36, sloot:= sub(".*?(\\d+).*", "\\1", name)]
gps[ID %in% 29:36, gebied:= tstrsplit(name, sloot)[1]]
# onderstaande sloten en opnamen wel eenzijdig (opvallend dat bij sommige sloten soms een, soms twee zijden penetrometer is gedaan)
gps[gebied == 'Rh' & sloot %in% c('1','5','9')| name %in% c('Sw_1_M_Af', 'Sw_3_M')| gebied == "Kw" , trajecten := 1]
# correct dist id for multiple transects (2 oevers or 2 transects)
gps[,max_dis_id := max(dist_id), by = 'name']
gps[trajecten == 2 & dist_id > (floor(max_dis_id/2)), dist_id := dist_id-floor(max_dis_id/2)]
# make unique name 4 matching
gps[,Opmerking := tolower(Opmerking)]
gps[,Opmerking := lapply(.SD, function(x) gsub("\\s+|\\s+", "",x)),.SDcols = c('Opmerking')]
gps[,plot := gsub('insteek','', Opmerking)]
gps[,plot := gsub('slootafgegraven','', plot)]
gps[,plot := gsub("[<>+]", "", plot)]
gps[,plot := gsub("^p", "plot", plot)]
gps[,plot := gsub("plotlot", "plot", plot)]
gps[,gebied:= tolower(gebied)]
gps[gebied == 'rh' & sloot %in% c(1,6,7,8,9,10), extragebied:= 'sloot6tm10'] #gps bevat alleen data van sloot 1 zuid, die hoort bij sloot6tm10
gps[gebied == 'rh' & sloot %in% c(2,3,4,5), extragebied:= 'sloot1tm5']
gps[gebied == 'rh' & sloot %in% c(11), extragebied:= 'sloot11']
gps[gebied == 'sw' & sloot %in% c(4), extragebied:= 'sloot4']
gps[gebied == 'sw' & sloot %in% c(5), extragebied:= 'sloot5']
gps[gebied == 'sw' & sloot %in% c(6), extragebied:= 'sloot6']
gps[name %in% c('RH_1_R_n','RH_1_R-AF_n','RH_1_M-AF_n','RH_1_M_n','RH_4_R_n'), extragebied:= 'sloot1'] #aanvullende gps gegevens van noorden ronde hoep
gps[grepl('WP1', name), extragebied:= 'wp1']
gps[gebied == 'md', extragebied:='wp1']
gps[name == 'SV_2_WP1_n', extragebied := 'sv_wp1']

# correctie dist_id
# 1 = perceel, 2 = insteek, 3 tm 12 = oever en insteek en perceel andere oeverzijde
gps[name == 'ZG1b' & plot == 'plot6pen1', dist_id := 1]
gps[name == 'ZG1b' & plot == 'plot6pen2', dist_id := 2]
gps[name == 'ZG1b' & plot == 'plot6pen3', dist_id := 3]
gps[name == 'ZG1b' & plot == 'plot5pen2', dist_id := 2]# is geen punt op perceel
gps[name == 'ZG1b' & plot == 'plot5pen3', dist_id := 3]
gps[name == 'ZG1a' & plot == 'plot2pen3', dist_id := 3]# alleenstaand punt in oever
gps[name == 'ZG1a' & plot == 'plot1pen1', dist_id := 1]
gps[name == 'ZG1a' & plot == 'plot1pen2', dist_id := 2]
gps[name == 'ZG1a' & plot == 'plot1pen3', dist_id := 3]
gps[name == 'ZG2a' & plot == 'plot10pen1', dist_id := 1]
gps[name == 'ZG2a' & plot == 'plot10pen2', dist_id := 2]
gps[name == 'ZG2a' & plot == 'plot10pen3', dist_id := 3]
gps[name == 'ZG2a' & plot == 'plot11pen1', dist_id := 1]
gps[name == 'ZG2a' & plot == 'plot11pen2', dist_id := 2]
gps[name == 'ZG2a' & plot == 'plot11pen3', dist_id := 3]
gps[name == 'ZG2b' & plot == 'plot16pen1', dist_id := 1]
gps[name == 'ZG2b' & plot == 'plot16pen2', dist_id := 2]
gps[name == 'ZG2b' & plot == 'plot16pen3', dist_id := 3]
gps[name == 'ZG2b' & plot == 'plot14pen1', dist_id := 1]
gps[name == 'ZG2b' & plot == 'plot14pen2', dist_id := 2]
gps[name == 'ZG2b' & plot == 'plot14pen3', dist_id := 3]
gps[name == 'Rh_2_M_Af' & Opmerking == 'plot2pen7', dist_id := 6]
gps[name == 'Rh_2_M_Af' & Opmerking == 'plot2pen8', dist_id := 1]
gps[name == 'Rh_2_M_Af' & Opmerking == 'plot2pen9', dist_id := 2]
gps[name == 'Rh_2_M_Af' & Opmerking == 'plot2pen10', dist_id := 3]
gps[name == 'Rh_2_M_Af' & Opmerking == 'plot3pen1', dist_id := 4]
gps[name == 'Rh_2_M_Af' & Opmerking == 'plot3pen2', dist_id := 5]
gps[name == 'Rh_4_R' & Opmerking == 'plot6pen8', dist_id := 6]
gps[name == 'Rh_4_R' & Opmerking == 'plot6pen9', dist_id := 1]
gps[name == 'Rh_4_R' & Opmerking == 'plot6pen10', dist_id := 2]
gps[name == 'Rh_4_R' & grepl('plot7', Opmerking), dist_id := dist_id-1]
gps[name == 'Rh_10_M' & grepl('plot5pen6', Opmerking), dist_id := 5]
gps[name == 'Rh_10_M' & grepl('plot6pen7', Opmerking), dist_id := 1]
gps[name == 'Rh_10_M' & grepl('plot6pen8', Opmerking), dist_id := 2]
gps[name == 'Rh_10_M' & grepl('plot6pen9', Opmerking), dist_id := 3]
gps[name == 'Rh_10_M' & grepl('plot6pen10', Opmerking), dist_id := 4]
gps[name == 'RH_11_M-AF_z' & grepl('plot1pen7', Opmerking), dist_id := 1]
gps[name == 'RH_11_M-AF_z' & grepl('plot1pen8', Opmerking), dist_id := 2]
gps[name == 'Sw_3_M_Af' & grepl('plot5pen1', Opmerking), dist_id := 5]
gps[name == 'Sw_3_M_Af' & grepl('plot5pen2', Opmerking), dist_id := 1]
gps[name == 'Sw_3_M_Af' & grepl('plot5pen3', Opmerking), dist_id := 2]
gps[name == 'Sw_3_M_Af' & grepl('plot5pen4', Opmerking), dist_id := 3]
gps[name == 'Sw_3_M_Af' & grepl('plot5pen5', Opmerking), dist_id := 4]
gps[name == 'Sw_2_M' & grepl('plot3pen9', Opmerking), dist_id := 4]
gps[name == 'Sw_2_M' & grepl('plot3pen10', Opmerking), dist_id := 1]
gps[name == 'Sw_2_M' & ident == '108210.754_489348.47', dist_id := 2]
gps[name == 'Sw_2_M' & ident == '108211.472_489348.753', dist_id := 3]
gps[name == 'Sw_2_M_Af' & grepl('plot3pen2', Opmerking), dist_id := 4]
gps[name == 'Sw_2_M_Af' & grepl('plot3pen3', Opmerking), dist_id := 1]
gps[name == 'Sw_2_M_Af' & grepl('plot3pen4', Opmerking), dist_id := 2]
gps[name == 'Sw_2_M_Af' & grepl('plot3pen5', Opmerking), dist_id := 3]
# select only locs which contain penetrometer reference
gps <- gps[grepl('^plot*', plot),]
gps <- gps[!grepl('plotenetrometer', plot),]

### postprocess penetrometer data ---------------------------------------------
# merge gps with pen
penmerge <- merge(pen, gps, by=c('gebied','plot','extragebied'), all.x = TRUE, all.y=FALSE, allow.cartesian = FALSE, suffixes = c('_pen','_gps'))
# add oever 2 data
penmerge <- merge(penmerge, gpsoevers[,c('name','plot','oever')], by.x = c('name_gps','plot'),by.y = c('name','plot'), all.x =TRUE, allow.cartesian = TRUE)
penmerge[is.na(oever), oever := sapply(strsplit(name_gps, '_'), `[`, 4)]
penmerge[,oever:= toupper(oever)]
penmerge[name_gps == 'Rh_4_R' & oever == 'O',oever:= 'Z']
penmerge[name_gps == 'Rh_4_R' & oever == 'W',oever:= 'N']
penmerge[name_gps == 'Rh_4_M_Af' & oever == 'O',oever:= 'Z']
penmerge[name_gps == 'Rh_4_M_Af' & oever == 'W',oever:= 'N']
penmerge[name_gps == 'Rh_4_M_Af' & oever == 'AF', oever:= 'Z']
penmerge[name_gps == 'Rh_5_M_Af_Kr' & oever == 'O', oever:= 'Z']
# add missing oevers
penmerge[name_gps == 'Kw_1_M' & is.na(oever), oever:= 'O']
penmerge[name_gps == 'Kw_1_R' & is.na(oever), oever:= 'O']
penmerge[name_gps == 'Kw_2_M' & is.na(oever), oever:= 'W']
penmerge[name_gps == 'Kw_2_R' & is.na(oever), oever:= 'W']
penmerge[name_gps == 'Sw_2_M' & is.na(oever), oever:= 'W']
penmerge[name_gps == 'Sw_1_M' & is.na(oever), oever:= 'W']
penmerge[name_gps == 'Sw_3_M' & is.na(oever), oever:= 'N']
penmerge[name_gps == 'ZG1a' & is.na(oever), oever:= 'O']
penmerge[name_gps == 'ZG3c' & is.na(oever), oever:= 'W']
penmerge[name_gps == 'Kw_2_M_Af' & oever == 'AF', oever:= 'W']
penmerge[name_gps == 'Kw_3_M_Af' & oever == 'AF', oever:= 'W']
penmerge[name_gps == 'Rh_2_M_Af' & oever == 'AF', oever:= 'O']
penmerge[name_gps == 'Rh_3_R_Kr' & oever == 'KR', oever:= 'O']
penmerge[name_gps == 'Rh_7_R_Kr' & oever == 'AF', oever:= 'W']
penmerge[name_gps == 'Rh_7_M_Af_Kr' & oever == 'AF', oever:= 'W']
penmerge[name_gps == 'Rh_10_R' & is.na(oever), oever:= 'N']
penmerge[name_gps == 'Rh_10_M' & is.na(oever), oever:= 'N']
penmerge[name_gps == 'Rh_10_M_Af' & oever == 'AF', oever:= 'N']

# merge with locationcorrection
setDT(locaties)
penmerge <- merge(penmerge, locaties, 
                  by.x = c('name_gps','oever'), by.y = c('SlootID_old_pengps','oever'), all.x = TRUE, allow.cartesian = TRUE, suffixes = c('_pen','_locs'))
# 1 = perceel, 2 = insteek, 3 tm 12 = oever en insteek en perceel andere oeverzijde
penmerge[,sectie:= 'oever']
penmerge[dist_id==1,sectie:= 'perceel']
penmerge[dist_id==2,sectie:= 'insteek']
penmerge[,dieptebin := cut(Diept, breaks = c('0','10','20','30','40','50','85'), include.lowest = TRUE)]
penmerge_wide <- dcast(penmerge[!is.na(Diept),], SlootID~sectie+dieptebin, value.var = c('indringingsweerstand'), fun.aggregate = mean, na.rm = TRUE, fill = FALSE, drop = TRUE)


# 5. Abiotiek ---------------------------------------------------------
## import ----------------------------------------------------------
inputdir <- paste0(workspace,"./ODK_abiotiek")
abio <- file.info(list.files(path= paste0(inputdir), pattern=".csv", full.names =  T))
abio <- rownames(abio)[which.max(abio$mtime)]
abio <- fread(abio, dec = ',', na.strings = c(999,9999,-999,-99,'999,0','NA','999'))
abio[, datum := as.POSIXct(Date_start_auto) ]
#replace 999
abio[abio == 999 ] <- NA
abio[datum < "2024-05-03" & abio == 0 ] <- NA

# remove columns without information
cols <- colnames(abio)[unlist(abio[,lapply(.SD,function(x) sum(is.na(x))==nrow(abio))])]
cols <- c(cols,"Date_start_auto","Date_end_auto","Device_ID","Datemanual","Waarnemer","Start_traject","End_traject",
          "Start_eindpunt_container","Afstand_startpunt_eindpunt_m","akkoord_met_te_kortlang", "uitleg_trajectlengte","Start_traject_accur_m")
abio[,c(cols):= NULL]
# set data type
abio[, water_redox := gsub(',','.', water_redox)]
abio[, water_redox := as.numeric(water_redox)]

# import onderholling
oh <- readxl::read_excel(paste0(workspace, './ODK_abiotiek/Onderholling RH.xlsx'))
setDT(oh)
oh[,SlootID := paste0(Sloot, '_', Behandeling,'_', Zijde)]
oh[SlootID == 'RH_4_R_z', SlootID := 'Rh_4_R']
oh[SlootID == 'RH_4_M-AF_z', SlootID := 'Rh_4_M_Af']
abio <- merge(abio, oh[,c('SlootID','Onderholling')], by = 'SlootID',all.x = TRUE)
abio[!is.na(Onderholling), holleoever := Onderholling]
abio[,Onderholling:= NULL]

## process abio-----------------------------------------------------
# correct Mijnden
abio[datum %in% '2024-07-24 11:42:55' & SlootID == 'MD_8_NVO_n', SlootID := 'MD_8_NVO1_n']
abio[SlootID == 'MD_1_NVO_z_twee', SlootID := 'MD_1_NVO1_z']
abio[,SlootID:= gsub('_twee0','', SlootID)] 
abio[,SlootID:= gsub('_twee','', SlootID)] 
# correct RH
abio[photo1 == "1715925906560.jpg" & SlootID == 'Rh_8_R', SlootID := 'RH_9_R_n']
abio[photo1 ==  "1715927119454.jpg" & SlootID == 'Rh_8_M_Af', SlootID := 'RH_9_M-AF_n']
# merge with unique/ koppelnames
setDT(locaties)
abio <- merge(abio, locaties , by.x ='SlootID', by.y = 'SlootID_old_abio', all.x = TRUE, all.y = FALSE)
abio[, SlootID_old_abio := SlootID]
abio[, SlootID := SlootID.y]

# aggregate numeric columns by sloot en gebied
cols_num <- colnames(abio)[sapply(abio, is.numeric)]
cols_num <- cols_num[!grepl('traject',cols_num)&!grepl('subsamples',cols_num)]
cols_wat <- cols_num[grepl('*water*',cols_num)]
cols_wat <- c(cols_wat, 'doorzicht2_mid_cm')
cols_wat <- cols_wat[!cols_wat%in%c("waterdiepte1_mid_cm","waterdiepte2_mid_cm","beheersporen_water1_2a_hoeveel")]
# 1 per sloot, gebied, behandeling (kan verschillen als behandeling verschilt per oeverzijde dan is projectie nodig)
cols_slib <- cols_num[grepl('*slib*',cols_num)]
# anders per slootID/ monster
cols_overig <-  c("datum","holleoever","uitraster_perc","uitraster_afstand_sloot_m","afscheur_veg_lengte_perc","afscheur_veg_breedte_cm",
                  "landgebruik_traject","landgebruik_overkant",
                  "beheersporen_water1_2a_hoeveel","beheersporen_water1_2a_welke","beheersporen_water1_2a_overig",
                  "beheersporen_oever2b_3_hoeveel","beheersporen_oever2b_3_welke","beheersporen_oever2b_3_overig",
                  "peilsporen_hoeveel","peilsporen_richting","peilsporen_uitleg","ondergrondse_drainage")  
cols_overig <- cols_overig[!cols_overig %in% cols_wat&!cols_overig %in% cols_slib]
abio_overig <- abio[,c('SlootID','gebied','sloot','behandeling','Oeverzijde',cols_overig), with=FALSE]
# watersamples 1 per sloot
abio_wat_agg <- abio[,lapply(.SD,mean,na.rm=TRUE),.SDcols=cols_wat,by=c('gebied','sloot')]
# slibsamples 1 per sloot and behandeling
abio_slib_agg <- abio[,lapply(.SD,mean,na.rm=TRUE),.SDcols=cols_slib,by=c('gebied','sloot','behandeling')]
# als NA dan waarde zelfde gebied, sloot en meest gelijkende behandeling projecteren
abio_slib_agg[,behandeling_1 := sapply(strsplit(behandeling, '-'), `[`, 1)]
abio_slib_agg[,behandeling_2 := ifelse(grepl('AF', behandeling),"AF",NA)]
setDT(abio_slib_agg)
abio_mis <- abio_slib_agg[is.na(slib_redox_mgL),]
abio_mis <- merge(abio_mis,abio_slib_agg[!is.na(slib_redox_mgL),], by = c('gebied','sloot','behandeling_1','behandeling_2'), suffixes =c('_mis',''))
abio_mis[,behandeling:=behandeling_mis]
abio_mis <-  abio_mis[,!grepl('_mis',colnames(abio_mis)), with = FALSE]
abio_slib_agg <- rbind(abio_slib_agg[!is.na(slib_redox_mgL),],abio_mis, fill=TRUE)

# 6. Slootprofielen ---------------------------------------------------
profiel <-  importGPSprof(inputdir = paste0(workspace,"./GPS slootprofielen"))
## Postprocess profielen-----------------------------------------------------

### correct Mijnden -------------------------------------
setDT(profiel)
profiel[name == 'MD_1_NVO' & Puntnummer == 11, Opmerking := 'waterlijn']

### process --------------
setDT(profiel)
#extract label waterlijn
profiel[grepl('waterlijn', Opmerking), wl:= z]
profiel[, wl := mean(wl, na.rm = TRUE), by = "ID"]
profiel[grepl('waterlijn', Opmerking), numwl := Puntnummer]
profiel[, numwl_min := min(numwl, na.rm = TRUE), by = "ID"]
profiel[, numwl_max := max(numwl, na.rm = TRUE), by = "ID"]
profiel[Puntnummer > numwl_max | Puntnummer < numwl_min, wl := NA]

#extract slibdikte
profiel[, slib := as.numeric(Opmerking)/100]
profiel[is.na(slib), slib:= 0]

# afstand in meters toevoegen
profiel[, dist := sqrt((x[Puntnummer == 1]-x)^2+(y[Puntnummer == 1]-y)^2), by ='ID'] 
profiel[, rel_dist := dist - shift(dist,-1), by ='ID'] 
profiel[rel_dist > 0 , rel_dist := rel_dist *-1]
profiel[, midpoint := mean(dist[!is.na(wl)]), by ='ID']
profiel[, midpoint_dist := dist - midpoint]
profiel[, mean_rel_dist := mean(rel_dist, na.rm =TRUE), by ='ID']
# add sectie sloot, oever, perceel
profiel[!is.na(wl), sectie := 'water', by ='ID']
profiel[rel_dist <= mean(rel_dist, na.rm =TRUE) & is.na(wl), sectie := 'perceel', by ='ID']
profiel[rel_dist > mean(rel_dist, na.rm =TRUE) & is.na(wl), sectie := 'oever', by ='ID']
profiel[,sectie_2 := ifelse(midpoint_dist < 0,1,2)]
# correct first point
profiel[dist == 0, sectie := 'perceel']
profiel[is.na(rel_dist), sectie := 'perceel']
# correct last point
profiel[sectie == 'perceel' & shift(sectie,+1) == 'oever', sectie := 'oever']

# windrichting toevoegen
for(i in unique(profiel$ID)){
  profiel_nr <- profiel[profiel$ID == i,]
  transect_direction <- get_cardinal_direction(profiel_nr)
  profiel[profiel$ID == i, azimuth := transect_direction]
}
# max waterdiepte
profiel[, wtd := wl - z]
profiel[wtd < 0, wtd := 0]
profiel[,max_wtd := max(wtd, na.rm = T), by = 'ID']
# max slibdikte
profiel[,max_slib := max(slib, na.rm = T), by = 'ID']
# breedte
profiel[, watbte := dist[Puntnummer == numwl_max]-dist[Puntnummer == numwl_min], by ='ID']
# taludhoek
profiel[sectie_2 == 1,talud := 100*((z- shift(z,-1))/ (-1*(dist - shift(dist,-1)))), by ='ID']
profiel[sectie_2 == 2,talud := 100*((z- shift(z,+1))/ (dist - shift(dist,+1))), by ='ID']
profiel[,onder_slib := z-slib]
profiel[sectie_2 == 1,talud_os := 100*((onder_slib- shift(onder_slib,-1))/ (-1*(dist - shift(dist,-1)))), by ='ID']
profiel[sectie_2 == 2,talud_os := 100*((onder_slib- shift(onder_slib,+1))/ (dist - shift(dist,+1))), by ='ID']
profiel[,mean_talud := mean(talud, na.rm = TRUE), by = c('ID','sectie','sectie_2')]
profiel[,mean_talud_os := mean(talud_os, na.rm = TRUE), by = c('ID','sectie','sectie_2')]
profiel[,median_talud := median(talud, na.rm = TRUE), by = c('ID','sectie','sectie_2')]
profiel[,median_talud_os := median(talud_os, na.rm = TRUE), by = c('ID','sectie','sectie_2')]
# profiel[sectie_2==1,talud_wl_or := talud[Puntnummer == min(Puntnummer[sectie == 'water']-1)], by ='ID']
# profiel[sectie_2==1,talud_wl_wt := talud[Puntnummer == min(Puntnummer[sectie == 'water'])], by ='ID']
# profiel[sectie_2==2,talud_wl_or_2 := talud[Puntnummer == max(Puntnummer[sectie == 'water'])], by ='ID']
# profiel[sectie_2==2,talud_wl_wt_2 := talud[Puntnummer == max(Puntnummer[sectie == 'water']-1)], by ='ID']

for(i in unique(profiel$ID)){
  profiel_nr <- profiel[profiel$ID == i,]
  talud_calc <- calc_taludhoek(profiel_nr)
  talud_calc_1 <- talud_calc[,c('tldk_bvwtr_perc_1','tldk_ondwtr_perc_1','tldk_wtrwtr_perc_1','tldk_oevrwtr_perc_1','tldk_vastbodem_perc_1')]
  profiel[profiel$ID == i & sectie_2 ==1, c('tldk_bvwtr_perc', 'tldk_ondwtr_perc','tldk_wtrwtr_perc','tldk_oevrwtr_perc','tldk_vastbodem_perc') := talud_calc_1]
  talud_calc_2 <- talud_calc[,c('tldk_bvwtr_perc_2','tldk_ondwtr_perc_2','tldk_wtrwtr_perc_2','tldk_oevrwtr_perc_2','tldk_vastbodem_perc_2')]
  profiel[profiel$ID == i & sectie_2 ==2, c('tldk_bvwtr_perc', 'tldk_ondwtr_perc','tldk_wtrwtr_perc','tldk_oevrwtr_perc','tldk_vastbodem_perc') := talud_calc_2]
}

# drooglegging
setDT(profiel)
profiel[sectie == 'oever', max_hgt_or := max(z, na.rm =T), by = 'ID']
profiel[, drglg := max_hgt_or - mean(wl, na.rm = TRUE), by = 'ID']
# aggregate and transpose
# add begin and end coords 4 intersection with locations
profiel[, c('x_begin', 'y_begin') := list(x[Puntnummer == min(Puntnummer)],y[Puntnummer == min(Puntnummer)]), by = c('ID','sectie_2')]
profiel[, c('x_eind', 'y_eind') := list(x[Puntnummer == max(Puntnummer)],y[Puntnummer == max(Puntnummer)]), by = c('ID','sectie_2')]
profiel[, geom := sprintf("LINESTRING(%s %s, %s %s)", x_begin, y_begin, x_eind, y_eind)]

profiel_wide <- dcast(profiel,name+geom+sectie_2~., value.var=c('max_slib','max_wtd','watbte','drglg',
                                                                'tldk_bvwtr_perc','tldk_ondwtr_perc','tldk_wtrwtr_perc','tldk_oevrwtr_perc',
                                                                'tldk_vastbodem_perc'), 
                      fun.aggregate = mean, na.rm = TRUE, fill = FALSE, drop = TRUE)
profiel_wide_2 <- dcast(profiel,name+sectie_2~sectie, value.var=c('mean_talud','mean_talud_os'), 
                      fun.aggregate = mean, na.rm = TRUE, fill = FALSE, drop = TRUE)
profiel_wide <- merge(profiel_wide,profiel_wide_2, by=c('name','sectie_2'))
profiel_wide <- profiel_wide[,-c('mean_talud_os_oever','mean_talud_os_perceel')]
# select best talud
setDT(profiel_wide)
profiel_wide[is.na(tldk_wtrwtr_perc), tldk_wtrwtr_perc := tldk_ondwtr_perc]
profiel_wide[is.na(tldk_oevrwtr_perc), tldk_oevrwtr_perc := tldk_bvwtr_perc]
profiel_wide <- st_as_sf(profiel_wide, wkt = "geom", crs = 28992)
profiel_wide <- profiel_wide[,c('name','sectie_2','max_slib','max_wtd','watbte','drglg',
                                'tldk_wtrwtr_perc','tldk_oevrwtr_perc',
                                'tldk_vastbodem_perc','geom')]

# intersect locations with profiel_wide
locaties <- st_as_sf(locaties) %>% st_transform(crs = 28992)
locs_prof <- st_intersection(profiel_wide, locaties[!is.na(locaties$SlootID_old_profiel),])
# Difference check
locs_prof_diff <- profiel_wide[!profiel_wide$name %in% locs_prof$name,]
# Check double values, when present an error in veseq is given when profiel en prof_locs are merged
setDT(locs_prof)
uniqueN(locs_prof[,c('name','sectie_2')])
locs_prof_double <- table(locs_prof[,c('name','sectie_2')])
locs_prof_double <- as.data.table(table(locs_prof[,c('name','sectie_2')]))
# DR_4_M-AUG wordt twee keer doorkruist door een profiel: mogelijk later een gemiddelde van talud etc. per slootID nemen
# add loc code
setDT(profiel)
setDT(locs_prof)
profiel <- merge(profiel, locs_prof, by.x =c('name','sectie_2'), by.y = c('name','sectie_2'), all.x = TRUE, all.y = FALSE, suffixes= c('','_locs'))

# 7. Vegetatie --------------------------------------------------------
inputdir <- paste0(workspace,"./ODK_vegetatie")
veg <- file.info(list.files(path= paste0(inputdir), pattern=".csv", full.names =  T))
veg <- rownames(veg)[which.max(veg$mtime)]
veg <- fread(veg, sep= ',', dec = '.', na.strings = c(999,9999,-999,-99,'999,0','NA','999'), fill = TRUE)
veg[, datum := as.Date(Date_start_auto) ]
# locatie correctie Floron: komt niet overeen met SlootID
veg_loc_cor <- read_sf(paste0(workspace,"./GIS/locaties_opnames_veest_devel.gpkg"), layer = 'VEG')
setDT(veg_loc_cor)
check <- veg_loc_cor[!SlootID %in% locaties$SlootID,]
veg_loc_cor[SlootID =='DR_3_M-JUL_N', SlootID:= 'DR_3_M-AUG_N']
veg_loc_cor[SlootID =='DR_3_M-AUG_N', SlootID:= 'DR_3_M-JUL_N']
veg_loc_cor[SlootID =='KW11_WP1_Z', SlootID:= 'KW_11_WP1_Z']
veg_loc_cor[SlootID =='RH_3_R_KR_O', SlootID:= 'RH_3_R-KR_O']
veg_loc_cor[SlootID =='SW_3_M_NVO_Z', SlootID:= 'SW_3_M_NVO_Z']
veg_loc_cor[SlootID =='SW_4_M-AF-NVO_NW', SlootID:= 'SW_4_M-NVO-AF_NW']
veg_loc_cor[SlootID =='SW_3_M-AF_Z', SlootID:= 'SW_3_M-NVO-AF_Z']
veg_loc_cor[SlootID =='SW_3_M_Z', SlootID:= 'SW_3_M-NVO_Z']
veg_loc_cor[SlootID =='WL_2_M-AF_W', SlootID:= 'WL_2_M-AF_O']
veg_loc_cor[SlootID =='WL_2_M_W', SlootID:= 'WL_2_M_O']
# merge with unique/ koppelnames
setDT(locaties)
veg <- merge(veg, veg_loc_cor , by ='instanceID', all.x = TRUE, all.y = FALSE, suffixes = c('_veg','_locs'))
veg[, SlootID := SlootID_locs]
veg <- veg[!is.na(SlootID),]

# 8. Waterbodemdata ---------------------------------------------------
# 9. Bodemgegevens oever ----------------------------------------------

# 10. Waterkwaliteit--------------------------------------------------

# 11. Validatie --------------------------------------------------
## 11.1 validatie regels penetrometer -------------------------
# check 4 double coordinates in pen_gps
gps <- st_as_sf(gps)
gps2 <- st_zm(gps, crs = 28992)
coords <- as.data.table(st_coordinates(gps2))
dubbel <- gps[which(duplicated(coords)),]
# check double plots in penetrometerdata 
setDT(gps)
checkgps <- gps[, nunique := uniqueN(ident), by = c('gebied','plot','extragebied')]
checkgps <- checkgps[nunique>1,]
# check wel gps geen pen
checkgps <- merge(gps[,c('name','gebied','plot','extragebied')],pen[,c('name','gebied','plot','extragebied')], by = c('gebied','plot','extragebied'), all.x = TRUE)
checkgps <- checkgps[is.na(name.y),]
# oever present in al pen/gps data
checkgps <- unique(penmerge[is.na(oever)&!is.na(name_gps), name_gps])
# check if gps is present 4 penetrometer data
penmerge[,uni_plots := uniqueN(plot), by = c('name_pen','gebied_pen','sloot_pen','extragebied','trajecten')]
penmergecheck_misgps <- unique(penmerge[is.na(name_gps),c('name_pen','gebied_pen','sloot_pen','extragebied','trajecten','uni_plots')])
# check if all locs are present
penmergecheck <- unique(penmerge[,c('SlootID','name_pen','name_gps','gebied_pen','sloot_pen','extragebied','trajecten','oever')])
locs_pen <- merge(unique(locaties[,c('SlootID','oever')]), penmergecheck, by = c('SlootID','oever'), all.x =TRUE, suffixes = c('_locs','_pen'))
# koppeltabelSlootIDpenID (alle penetro zit hierin, koppellocs kunnen missen)
slootID_penetrometerID <- dcast(penmerge, SlootID+name_gps+oever~.) # check unique loc combos, soms kan een pen 2 keer worden gekoppeld (RH_9_R) daarom allow cartesian

## 11.2 validatie abiotiek -------------------------------
## check dubbele codes
checkabioloc <- dcast(abio, SlootID~datum)
### make matrix wq by ditch ID
loc.wq <- unique(abio[,c('SlootID','gebied','sloot',"Oevermonster","bodemmonster_oeverzone2b3","bodemmonster_oeverzone2b3_subsamples", "bodemmonster_oever2b3_boven",  
                         "bodemmonster_oever2b3_boven_subsamples"  ,  "bodemmonster_oeverzone2b3_diep", "bodemmonster_oeverzone2b3_diep_subsamples", "slibmonster","slibmonster_subsamples","bodemmonster_water","porievochtmonster",                        
                         "porievochtmonster_subsamples","porievochtmonster_code","Waterkwaliteitmonster", "watermonster_subsamples","watermonster_code")])
locs_abio <- merge(unique(locaties[,c('SlootID','oever')]), abio[,c('SlootID','oever','SlootID_old_abio','instanceID')], by = 'SlootID', all.x =TRUE, suffixes = c('_locs','_abio'))
# check if all abio locs are present in locaties
locs_abio <- unique(abio[,c('SlootID','SlootID_old_abio')])
## 11.3 check voor dubbele locatiecodes in profielen ------------------
uniqueN(profiel$name) == uniqueN(profiel$ID)
checkloc <- dcast(profiel, name~., value.var = 'ID', fun.aggregate = uniqueN)
# check 4 floating secties
check_floating_secties <- profiel[!sectie == shift(sectie,-1) & !sectie == shift(sectie,+1)]
# check if all locs are present
setDT(profiel)
setDT(locaties)
# locs_prof <- merge(unique(locaties[,c('SlootID','oever')]), unique(profiel[,c('SlootID','oever','SlootID_old_profiel')]), by = 'SlootID', all.x =TRUE, suffixes = c('_locs','_prof'))

# 12. Create database/ merge gegevens ----------------------------------------
## 12.1 pen wide and prof wide met kentallen per unieke slootID --------------
# projectie van water en slibmonsters
abio_proj <- merge(abio_overig, abio_wat_agg, by = c('gebied','sloot'), all.x = T)
abio_proj <- merge(abio_proj, abio_slib_agg, by = c('gebied','sloot','behandeling'), all.x = T)
abio_proj <- merge(abio_proj, locs_prof[,-c('gebied','sloot','behandeling','Oeverzijde')], by = 'SlootID', all.x = T)
abio_proj <- merge(abio_proj, penmerge_wide, by = 'SlootID', all.x = T)
abio_proj <- merge(abio_proj, clusters_locs, by = c('SlootID'), all.x = T)
veg[,maand:= month(datum)]
abio_proj[,maand:= month(datum)]
# Dit gaat niet goed nu
abio_proj <- merge(abio_proj, veg, by = c('SlootID', 'maand'), all.x = T)
## 12.2 cluster analyse --------------------------------------------------------
setDT(abio_proj)
clust_abio <- abio_proj[,c('SlootID','drglg','watbte','trofie','afwatopp')]
clust_abio <- clust_abio[complete.cases(clust_abio)]
clust_abio_s <- scale(clust_abio[,c('drglg','watbte','trofie','afwatopp')])
km <- kmeans(clust_abio_s, 6)
# Within cluster sum of squares by cluster:
round(km$betweenss / km$totss,4) * 100
plot(clust_abio[,2:5], col = km$cluster)
clust_abio <- cbind(clust_abio, km$cluster)
# maak nieuwe clusters gesorteerd op mediane drooglegging
clust_abio <- clust_abio[,median_drglg:= median(drglg), by='V2']
setorder(clust_abio, cols = "median_drglg") 
map <- unique(clust_abio$V2)
map <- as.data.table(cbind(map, seq(1:6)))
clust_abio <- clust_abio[map, clusters := V2, on = c(V2 = "map")]
clust_abio <- unique(clust_abio[,c('SlootID','V2')])
## 12.3 merge with data ----------------------
abio_proj <- merge(abio_proj, clust_abio, by = c('SlootID'), all.x = T, suffixes = c('','_clustber'))
abio_proj$clust_ber<- abio_proj$V2
# add grouping vars
abio_proj[text == "Hoogheemraadschap De Stichtse Rijnlanden",waterschap := 'HDSR']
abio_proj[text == "Hoogheemraadschap Hollands Noorderkwartier" ,waterschap := 'HHNK']
abio_proj[text == "Waterschap Amstel, Gooi en Vecht",waterschap := 'AGV']
abio_proj[text == "Hoogheemraadschap van Rijnland",waterschap := 'Rijnland']
abio_proj[text == "Wetterskip Fryslân",waterschap := 'Fryslân']
abio_proj[text == "Waterschap Drents Overijsselse Delta",waterschap := 'WDOD']
abio_proj[text == "Hoogheemraadschap van Schieland en de Krimpenerwaard",waterschap := 'HHSK']
# veentype
abio_proj[grepl('b$',BODEMCODE), veentype:= 'broekveen']
abio_proj[grepl('s$',BODEMCODE), veentype:= 'veenmosveen']
abio_proj[grepl('c$',BODEMCODE), veentype:= 'zeggeveen_rietzeggeveen_broekveen']
abio_proj[grepl('r$',BODEMCODE), veentype:= 'zeggerietveen_rietveen']
abio_proj[grepl('d$',BODEMCODE), veentype:= 'bagger_verslagenveen_gyttja_anders']
# beheer
abio_proj[,beheer := 'regulier']
abio_proj[grepl('AF', behandeling),beheer := 'afrastering']
abio_proj[grepl('NVO', behandeling),beheer := 'NVO']
abio_proj[grepl('M', behandeling),beheer := 'Minimaal']

# 13. RF model (to do - just example code here) ----------------------------------------
## Preparation 
# Column with point ID names
var_id <- "locatiecode"
# fraction of training set
fr_train <- 0.8
##  Define response & explanatory variables 
# response variable
var_res <- "med_wd"
# covariables
var_cov <- c("breedte", "PEIL", "soiltypen", "KWEL", "pnt_ahn", "A_OS_GV") 
# make a dataset with relevant columns and rows, split test & training sets
setDT(loc_sf)
# Select complete records
var <- c(var_id, var_res, var_cov)
loc_t <- loc_sf[complete.cases(loc_sf[, ..var]), ..var]
# change column name of response variables
setnames(loc_t, old = var_res, new = "varres")

# Split training and test dataset
set.seed(123)
train <- sample(1:nrow(loc_t),nrow(loc_t) * fr_train)
# add train or not
loc_t[train, set := "training"]
loc_t[!train, set := "test"]
loc_t$set <- as.factor(loc_t$set)

print(paste0("training N = ", loc_t[set == "training", .N], 
             ", testing N = ", loc_t[set == "test", .N]))

## Random forest 
# Run random forest
ls_rf <- fun_rf(loc_t, var_id, var_cov, 
                check_mtry = FALSE, 
                #check_mtry = TRUE, # Checking differetn mtry. this costs extra 30 sec
                mtry = round(length(var_cov)/3, 0),
                ntree = 400)

# merge prediction and residuals to sf object
cols <- c("locatiecode", "pred_rf", "resid_rf", "set")
loc_sf <- merge(loc_sf, ls_rf$loc_t[, ..cols], by = "locatiecode", all.x = T)
# compute R2 of test set
test_r2_rf <- test_r2(loc_sf, var_res, "pred_rf")



# 14. Visualisaties ---------------------------------------------------------
## 14.1 plot profiel--------------
### pairs ------------------
setDT(profiel_wide)
pairs(profiel_wide[,3:9])
### loop profielplaatjes ------------------
for(i in unique(profiel$ID)){
  setDT(profiel)
  visualise_profiel(profiel[ID == i,])
  print(i)
}
# als water niet aansluit dan is Z op oever lager dan de waterlijn



## 14.2 plot clusters------------
setDT(clusters_locs)
clusters_locs$gebied<- tolower(clusters_locs$gebied)
### clust by loc x, y------------------
# clusters_locs clust_abio
ggplot() +
  geom_boxplot(data = abio_proj, aes(x = gebied, y = clusters)) + 
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size=10), 
    strip.text.y = element_text(size = 10), 
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size= 12),
    axis.ticks =  element_line(colour = "black"),
    plot.title = element_text(size =12, face="bold", hjust = 0.5),
    panel.background = element_blank(),
    plot.background = element_blank(),
  )+
  ggtitle("") +
  labs(x= "gebied",y="veensloottype")
ggsave(file=paste0('output/clusters/clusters_hist.png'),width = 40,height = 15,units='cm',dpi=1000)

### clust by data -------------------------
# melt data table voor boxplots
melt <- melt(setDT(clust_abio), id.vars = c("clusters"), 
             measure.vars = c('drglg','watbte','trofie','afwatopp'))
melt$clusters <- as.factor(melt$clusters)
namenind <- c("drooglegging", "slootbreedte","trofiegraad veen","afwateringsbreedte")
melt[, level := factor(variable, labels = namenind)]
setDT(melt)
d1 <-  melt[,.(percentiel25 = quantile(value, probs =c(0.25), na.rm = TRUE, digits = 2),mediaan = quantile(value, probs =c(0.5), na.rm = TRUE, digits = 2),percentiel75 = quantile(value, probs =c(0.75), na.rm =TRUE, digits = 2)), by = c('clusters','variable','level')]
d1 <- dcast(d1,clusters ~ variable, value.var = c('percentiel25','mediaan','percentiel75'), fun.aggregate = mean)

p<- melt %>% 
  ggplot(aes(x= clusters, y= value))+
  geom_boxplot() +
  coord_flip()+
  facet_wrap(~level, scales = "free")+
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size = 10), 
    strip.text.y = element_text(size = 10), 
    axis.text.x = element_text(size = 10,angle=90,hjust=1),
    axis.text.y = element_text(size = 10),
    axis.ticks =  element_line(colour = "black"),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.position="none"
  )+
  guides(fill="none")+
  ggtitle('') +
  #guides(col = '') +
  labs(x= '' , y= '')
ggplotly(p)

# clust by drglg (later ook trofie veen, fe, org, slootbreedte, oeverbreedte, kwel/wegzijging)
ggplot() +
  geom_boxplot(data = abio_proj, aes(x = gebied, y = drglg)) + 
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size=12), 
    strip.text.y = element_text(size = 12), 
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size= 14),
    axis.ticks =  element_line(colour = "black"),
    plot.title = element_text(size =12, face="bold", hjust = 0.5),
    panel.background = element_blank(),
    plot.background = element_blank(),
  )+
  ggtitle("") +
  labs(x= "gebied",y="drooglegging (m)")
ggsave(file=paste0('output/clusters/drooglegging_gebied.png'),width = 20,height = 15,units='cm',dpi=1000)

### kaart --------------------------------------
# kaartweergave
clust <- merge(locaties, clust_abio[,c('clusters', 'SlootID')], by = 'SlootID', all.x = TRUE)
st_as_sf(clust)
st_write(clust , paste0(workspace,'clusters_20241107.gpkg')) #1904
library(RColorBrewer)
filcol <- colorRampPalette(brewer.pal(10, 'Spectral'))
clust$clusters <- as.factor(clust$clusters)
nmi_data <- paste0(gsub('\\\\', '/', Sys.getenv('USERPROFILE')), '/SPRINGG/Sven Verweij - NMI-DATA/')
wschp <- st_read(paste0(nmi_data, "topo/waterschappen/raw/2019_waterschappen_grenzen.gpkg"))%>% st_transform(28992)
clust_map <- clust[!is.na(clust$Start_traject_lat)&!clust$Start_traject_long==0,]

ggplot() +
  geom_sf(data = wschp, aes(), col = 'blue', fill = NA) +
  geom_sf(data = clust_map, aes(fill = clusters, col = clusters), linewidth = 10) +
  scale_fill_manual(values = rev(filcol(8)),
                    na.value="white",  drop = FALSE, name = "") +
  scale_size_identity()+
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = 'white'),  ## azure
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0), # left-align
        legend.position="bottom",
        legend.title=element_text(size=20),
        legend.text=element_text(size=18)) +  
  guides(fill = guide_legend("clusters"))
ggsave(filename = paste0("kaart_clusters.jpeg"), width = 60, height = 65, unit = "cm")

## 14.3 plot relaties database --------------------
### pairs ---------------------------
pairs(abio_proj[,c("holleoever","water_conductiviteit_uS_cm","water_redox","water_pH","max_slib","max_wtd","slib_pH","oever_[0,10]" ,"oever_(30,40]" ) ,with=FALSE])
pairs(abio_proj[,c("holleoever","water_pH","max_slib","max_wtd","oever_(30,40]", "slib_redox_mgL"),with=FALSE], col = abio_proj$clust_ber)
pairs(abio_proj[,c("holleoever","oever_(30,40]","Oeverzone_2b_emers_perc","max_slib","max_wtd","Waterzone_1_subm_tot_perc"),with=FALSE], col = abio_proj$clust_ber)
pairs(abio_proj[,c("drglg","Oeverzone_2b_breedte_cm","Oeverzone_2a_breedte_cm"),with=FALSE], col = abio_proj$clust_ber)
# slib en redox, onderholling en waterdiepte
qplot(slib_redox_mgL, Waterzone_1_subm_tot_perc, data = abio_proj)

### correlation plot--------------------
library(corrplot)
cols_num <- colnames(abio_proj)[sapply(abio_proj, is.numeric)]
cols_num <-cols_num[10:100]
cols_num <- c("watertemp_C","water_conductiviteit_uS_cm","water_redox","water_pH","water_O2_mgL","doorzicht2_mid_cm",
              "slib_conductiviteit_uS_cm","slib_pH","slib_O2_mgL","slib_redox_mgL",
              "max_slib" ,"max_wtd","watbte","drglg",
              "tldk_wtrwtr_perc","tldk_oevrwtr_perc","tldk_vastbodem_perc",
              "oever_[0,10]","oever_(10,20]" ,"oever_(20,30]","oever_(30,40]","oever_(40,50]",
              "perceel_[0,10]","perceel_(10,20]","perceel_(20,30]","perceel_(30,40]",
              "Waterzone_1_breedte_cm", "Oeverzone_2a_breedte_cm","Oeverzone_2b_breedte_cm","Terzone_3_breedte_cm",
              "Waterzone_1_kaal_perc","Waterzone_1_subm_tot_perc", "Waterzone_1_natans_perc","Waterzone_1_kroos_perc",
              "Oeverzone_2a_kaal_perc","Oeverzone_2a_subm_tot_perc" ,"Oeverzone_2a_natans_perc","Oeverzone_2a_emers_perc","Oeverzone_2a_ter_gras_kruid_perc", 
              "Oeverzone_2b_kaal_perc","Oeverzone_2b_emers_perc","Oeverzone_2b_ter_gras_kruid_perc", 
              "Terzone_3_kaal_perc", "Terzone_3_ter_gras_kruid_perc","Terzone_3_emers_perc"       
              )
cols_num <- c("doorzicht2_mid_cm",
              "slib_redox_mgL",
              "max_slib" ,"max_wtd","watbte","drglg",
              "oever_[0,10]","oever_(10,20]" ,"oever_(30,40]","oever_(40,50]",
              "perceel_[0,10]","perceel_(10,20]","perceel_(30,40]",
              "Oeverzone_2a_breedte_cm","Oeverzone_2b_breedte_cm","Terzone_3_breedte_cm",
              "Waterzone_1_subm_tot_perc", "Waterzone_1_kroos_perc",
              "Oeverzone_2a_kaal_perc","Oeverzone_2a_emers_perc","Oeverzone_2a_ter_gras_kruid_perc", 
              "Oeverzone_2b_kaal_perc","Oeverzone_2b_emers_perc","Oeverzone_2b_ter_gras_kruid_perc", 
              "Terzone_3_kaal_perc", "Terzone_3_ter_gras_kruid_perc","Terzone_3_emers_perc"       
)
M <-cor(abio_proj[,cols_num,with=FALSE],use = "complete.obs")
corrplot(M, type="full", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"), tl.col = "black",tl.cex =0.8)




ggplot() +
  geom_jitter(data = abio_proj[!gebied %in% c('HW','ZG','SV','WL','MB'),], aes(x = max_slib, y = `perceel_(10,20]`, col = gebied), size = 2) + 
  geom_smooth(data = abio_proj[!gebied %in% c('HW','ZG','SV','WL'),], aes(x = max_slib, y = `perceel_(10,20]`)) +
  # coord_fixed(ratio=1, xlim=c(0,1), ylim=c(0,1))+
  # abline(1,1)+
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size=10), 
    strip.text.y = element_text(size = 10), 
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size= 12),
    axis.ticks =  element_line(colour = "black"),
    plot.title = element_text(size =12, face="bold", hjust = 0.5),
    panel.background = element_blank(),
    plot.background = element_blank(),
  )+
  ggtitle("") +
  labs(x= "waterdiepte",y="indringingsweerstand")

ggplot() +
  geom_jitter(data = abio_proj[gebied %in% c('HW','ZG','SV','WL','MB'),], aes(x = max_wtd, y = `perceel_(10,20]`, col = gebied), size = 2) + 
  geom_smooth(data = abio_proj[gebied %in% c('HW','ZG','SV','WL','MB'),], aes(x = max_wtd, y = `perceel_(10,20]`)) +
  # coord_fixed(ratio=1, xlim=c(0,1), ylim=c(0,1))+
  # abline(1,1)+
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size=10), 
    strip.text.y = element_text(size = 10), 
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size= 12),
    axis.ticks =  element_line(colour = "black"),
    plot.title = element_text(size =12, face="bold", hjust = 0.5),
    panel.background = element_blank(),
    plot.background = element_blank(),
  )+
  ggtitle("") +
  labs(x= "waterdiepte",y="indringingsweerstand")

ggplot() +
  geom_jitter(data = abio_proj, aes(x = drlg, y = drglg, col = waterschap), size = 3) +
  xlim(0,0.8)+ ylim(0,0.8)+
  geom_abline(intercept = 0, slope = 1)+
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    axis.title =  element_text(size=18), 
    axis.text = element_text(size = 15),
    legend.title = element_text(size= 15),
    legend.text = element_text(size= 15),
    axis.ticks =  element_line(colour = "black"),
    plot.title = element_text(size =18, face="bold", hjust = 0.5),
    panel.background = element_blank(),
    plot.background = element_blank(),
  )+
  ggtitle("Relatie veensloottypen en gemeten veensloottypen") +
  labs(x= "drooglegging obv peilgebieden",y="drooglegging obv meting")

ggplot() +
  geom_jitter(data = abio_proj[max_wtd < 0.8,], aes(x = max_wtd, y = drglg, col = clusters)) + 
  geom_smooth(data = abio_proj[max_wtd < 0.8,], aes(x = max_wtd, y = drglg)) +
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size=10), 
    strip.text.y = element_text(size = 10), 
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size= 12),
    axis.ticks =  element_line(colour = "black"),
    plot.title = element_text(size =12, face="bold", hjust = 0.5),
    panel.background = element_blank(),
    plot.background = element_blank(),
  )+
  ggtitle("Relatie waterdiepte - drooglegging") +
  labs(x= "waterdiepte (m)",y="drooglegging (m)")
ggsave(file=paste0('output/clusters/watdte_drglg.png'),width = 15,height = 15,units='cm',dpi=1000)


ggplot() +
  geom_jitter(data = abio_proj, aes(x = slib_redox_mgL, y = slib_conductiviteit_uS_cm)) + 
  geom_smooth(data = abio_proj, aes(x = slib_redox_mgL, y = slib_conductiviteit_uS_cm)) +
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size=10), 
    strip.text.y = element_text(size = 10), 
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size= 12),
    axis.ticks =  element_line(colour = "black"),
    plot.title = element_text(size =12, face="bold", hjust = 0.5),
    panel.background = element_blank(),
    plot.background = element_blank(),
  )+
  ggtitle("Relatie ") +
  labs(x= "redox",y="egv")
ggsave(file=paste0('output/clusters/watdte_drglg.png'),width = 15,height = 15,units='cm',dpi=1000)

ggplot() +
  geom_jitter(data = abio_proj, aes(x =slib_redox_mgL  ,y = max_wtd, col = as.factor(clusters))) + 
  # geom_smooth(data = abio_proj[max_wtd < 0.8,], aes(x = max_wtd, y = max_slib)) +
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size=10), 
    strip.text.y = element_text(size = 10), 
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size= 12),
    axis.ticks =  element_line(colour = "black"),
    plot.title = element_text(size =12, face="bold", hjust = 0.5),
    panel.background = element_blank(),
    plot.background = element_blank(),
  )+
  ggtitle("Relatie waterdiepte - vegetatiebedekking") +
  labs(x= "Redox ()",y="waterdiepte (m)")
ggsave(file=paste0('output/clusters/watdte_slib.png'),width = 15,height = 15,units='cm',dpi=1000)


### abio histogrammen -----------------------

abio_proj[,var1:= cut(max_slib, breaks = c('0','0.1','.50','1.00','2.00','3.00','5.00'), include.lowest = TRUE)]
abio_proj[,var:= as.numeric(slib_redox_mgL)] parname <- "Redox slib (mV)"#Breedte oeverzone (m)  "taludhoek (%)" 'waterdiepte (m)'#'drooglegging' #"slibdikte (m)" # onderholling(m)
abio_proj[,var:= Oeverzone_2a_emers_perc] 
parname <- "Bedekking emers aquatische oeverzone (%)"
abio_proj[,var:= Waterzone_1_subm_tot_perc] 
parname <- "Bedekking onderwaterplanten (%)"
abio_proj[,var:= Oeverzone_2b_kaal_perc] 
parname <- "Kale oever (%)"#  "taludhoek (%)" 
abio_proj[,var:= Oeverzone_2b_emers_perc] 
parname <- "Bedekking emers terrestrische oeverzone (%)"
abio_proj[,var:= Oeverzone_2a_breedte_cm] 
parname <- "Breedte aquatische oeverzone (cm)"
abio_proj[,var:= Oeverzone_2b_breedte_cm] 
parname <- "Breedte terrestrische oeverzone (cm)"
abio_proj[,var:= drglg] 
parname <- "drooglegging (m)"
abio_proj[,var:= holleoever] 
parname <- "onderholling (m)"
abio_proj[,var:= max_slib] 
parname <- "slibdikte (m)"
abio_proj[,var:= max_wtd] 
parname <- "waterdiepte (m)"
abio_proj[,var:= watbte] 
parname <- "waterbreedte (m)"
abio_proj[,var:= Oeverzone_2b_kaal_perc] 
abio_proj[,var:= drglg]

legendtitle <- "waterdiepte (m)"  

ggplot() +
  geom_histogram(data = abio_proj[!is.na(var)& gebied== 'RH',], aes(var),fill = 'lightgrey', col = 'black', binwidth = 10) + 
  # geom_histogram(data = abio_proj[!is.na(var1),], aes(var1),fill = 'darkgrey', col = 'black', binwidth = 20) + 
  # xlim(0,100)+
  # ylim(0,20)+
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    strip.text.y = element_text(size = 12), 
    axis.text = element_text(size = 14),
    axis.title = element_text(size= 15),
    axis.ticks =  element_line(colour = "black"),
    plot.title = element_text(size =15, face="bold", hjust = 0.5),
    panel.background = element_blank(),
    plot.background = element_blank(),
  )+
  guides(fill=guide_legend(title=legendtitle))+
  ggtitle(parname) +
  labs(x= parname, y=paste0("aantal monstertrajecten (n= " , uniqueN(abio_proj$SlootID),') '))
ggsave(file=paste0('output/hist_',parname,'.png'),width = 25,height = 15,units='cm',dpi=1000)

ggplot() +
  geom_boxplot(data = abio_proj[!is.na(var),], aes(x = gebied,  y = var), binwidth = 0.1) + 
  # facet_grid(~waterschap, scales = 'free_x' )+
  # ylim(0,2)+
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    strip.text.y = element_text(size = 10), 
    axis.text = element_text(size = 18, angle = 45),
    axis.title = element_text(size= 15),
    axis.ticks =  element_line(colour = "black"),
    plot.title = element_text(size =20, face="bold", hjust = 0.5),
    panel.background = element_blank(),
    plot.background = element_blank(),
  )+
  # guides(x= ggh4x::guide_axis_nested(delim = "&"))+
  ggtitle(parname) +
  labs(x= 'gebied', y=parname)
ggsave(file=paste0('output/clusters/',parname,'.png'),width = 40,height = 15,units='cm',dpi=1000)

## 14.4 plot penetrometer-------------------------
# calc draagkracht per diepte
# dist id 1 (perceel) en 2 (insteek) weg
ggplot()+
  geom_boxplot(data = penmerge[!is.na(dieptebin) & sectie == 'perceel',], aes(x= (gebied_locs), y=indringingsweerstand))+
  facet_grid(dieptebin~rev(sectie))+
  ylim(0,2)+
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size=10), 
    strip.text.y = element_text(size = 10), 
    axis.text.x = element_text(size = 10, angle = 90),
    axis.text.y = element_text(size= 12),
    axis.ticks =  element_line(colour = "black"),
    plot.title = element_text(size =12, face="bold", hjust = 0.5),
    panel.background = element_blank(),
    plot.background = element_blank(),
  )+
  ggtitle("Draagkracht oevers") +
  labs(x= "diepteinterval (cm)",y="indringingsweerstand")

ggplot()+
  geom_jitter(data = penmerge[!is.na(dieptebin) & gebied_locs %in% c('RH', 'ZG') ,], aes(col = (gebied_locs), x=indringingsweerstand, y = Diept*-1))+
  facet_grid(.~sectie)+
  # ylim(0,2)+
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size=10), 
    strip.text.y = element_text(size = 10), 
    axis.text.x = element_text(size = 10, angle = 90),
    axis.text.y = element_text(size= 12),
    axis.ticks =  element_line(colour = "black"),
    plot.title = element_text(size =12, face="bold", hjust = 0.5),
    panel.background = element_blank(),
    plot.background = element_blank(),
  )+
  ggtitle("Draagkracht") +
  labs(x= "indringingsweerstand",y="diepteinterval (cm)")

ggplot()+
  geom_jitter(data = abio_proj[], aes(col= gebied, x= `perceel_(20,30]`, y=`oever_(20,30]`))+
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size=10), 
    strip.text.y = element_text(size = 10), 
    axis.text.x = element_text(size = 10, angle = 90),
    axis.text.y = element_text(size= 12),
    axis.ticks =  element_line(colour = "black"),
    plot.title = element_text(size =12, face="bold", hjust = 0.5),
    panel.background = element_blank(),
    plot.background = element_blank(),
  )+
  ggtitle("Draagkracht oevers") +
  labs(x= "weerstand perceel",y="indringingsweerstand oever")

abio_proj[,onderholling := cut(holleoever, breaks = c('0','10','20','30','40','50','85'), include.lowest = TRUE)]
ggplot()+
  geom_boxplot(data = abio_proj, aes(x=  gebied, y=`oever_(20,30]`, fill = as.factor(onderholling)))+
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size=10), 
    strip.text.y = element_text(size = 10), 
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size= 12),
    axis.ticks =  element_line(colour = "black"),
    plot.title = element_text(size =12, face="bold", hjust = 0.5),
    panel.background = element_blank(),
    plot.background = element_blank(),
  )+
  ggtitle("Draagkracht oevers") +
  labs(x= "diepte",y="indringingsweerstand")

ggplot()+
  geom_jitter(data = abio_proj, aes(x=  holleoever, y=`perceel_[0,10]`, col = waterschap),  size = 3)+
  geom_jitter(data = abio_proj, aes(x=  holleoever, y=`perceel_(20,30]`, col = waterschap), size = 3)+
  # geom_jitter(data = abio_proj, aes(x=  holleoever, y=`perceel_(30,40]`, col = waterschap), size = 3)+
  # scale_color_manual(name = "waterschap")) +
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    axis.title = element_text(size= 14), 
    axis.text = element_text(size= 14),
    legend.title = element_text(size= 14),
    legend.text = element_text(size= 14),
    axis.ticks =  element_line(colour = "black"),
    plot.title = element_text(size =15, face="bold", hjust = 0.5),
    panel.background = element_blank(),
    plot.background = element_blank(),
  )+
  ggtitle("Draagkracht oevers") +
  labs(x= "onderholling",y="indringingsweerstand (Mpa)")
 
ggplot()+
  geom_jitter(data = abio_proj, aes(x=  holleoever, y=`oever_[0,10]`,col = as.factor(V2)),  size = 3)+
  # scale_color_manual(name = "Veensloottype") +
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size=10), 
    strip.text.y = element_text(size = 10), 
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size= 12),
    axis.ticks =  element_line(colour = "black"),
    plot.title = element_text(size =12, face="bold", hjust = 0.5),
    panel.background = element_blank(),
    plot.background = element_blank(),
  )+
  ggtitle("Draagkracht oevers") +
  labs(x= "onderholling",y="indringingsweerstand (Mpa)") 



# 15. Export the data ---------------------------------------------------------
## locaties---------------------------------------------------------
st_write(locaties, paste0(workspace,  "output/geo_locaties.gpkg"), append = FALSE)

## profielen, hoeken, onderholling-----------------------------------------
write.table(profiel, file = paste(workspace,"/output/profiel",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
profiel <- st_as_sf(profiel)
st_write(profiel, paste0(workspace,  "output/profiel.gpkg"), append = FALSE)
profiel_wide <- st_as_sf(profiel_wide)
st_write(profiel_wide, paste0(workspace,  "output/profiel_wide.gpkg"), append = FALSE)

prof <- unique(prof_abio[,c('name','max_wtd','max_slib','holleoever','tldk_bvwtr_perc_1','tldk_bvwtr_graden_1','tldk_bvwtr_perc_2','tldk_bvwtr_graden_2',
                    'tldk_ondwtr_perc_1','tldk_ondwtr_graden_1','tldk_ondwtr_perc_2','tldk_ondwtr_graden_2',
                    'tldk_wtr_perc_1','tldk_wtr_graden_1','tldk_wtr_perc_2 ','tldk_wtr_graden_2',
                    'tldk_vastbodem_perc_1','tldk_vastbodem_graden_1','tldk_vastbodem_perc_2','tldk_vastbodem_graden_2')])
write.table(prof, file = paste0('output/profielen_hoek.csv' ), sep = ';',dec = '.', row.names = F)

## abiotiek-----------------------------------------------------------------
write.table(abio, file = paste0('output/abio.csv' ), sep = ';',dec = '.', row.names = F)
write.table(locs_abio, file = paste0('output/locs_abio.csv' ), sep = ';',dec = '.', row.names = F)
write.table(loc.wq, file = paste(workspace,"/output/monst_codes",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)

## vegetatie-----------------------------------------------------------------
write.table(veg, file = paste0('output/veg.csv' ), sep = ';',dec = '.', row.names = F)

## locaties per cluster export----------------------------------------------
st_write(clusters_locs, paste0(workspace,  "output/clusters_locs_abio.gpkg"), append = TRUE)
setDT(clusters_locs)
write.table(clusters_locs, file = paste(workspace,"/output/clusters_locs_abio",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
st_as_sf(clust_abio)

## penetrometerdata -------------------------------------------------------
write.table(penmergecheck, file = paste(workspace,"/output/penetrometer_gps_check",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
write.table(locs_pen, file = paste(workspace,"/output/locs_penetrometer",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
gps <- st_as_sf(gps)
st_write(gps, paste0(workspace,  "output/gps_penetrometer.gpkg"), append = FALSE)

## locatieinfo Bware ------------------------------------------------------
write.table(abio_proj[,c('SlootID','Slibmonster','Waterkwaliteitmonster','drlg','max_wtd','max_slib','watbte','slib_conductiviteit_uS_cm','slib_O2_mgL','slib_redox_mgL','slib_pH','BODEMCODE','veentype','trofie','text',
                         "Start_traject_long.x","Start_traject_lat.x" ,"End_traject_long.x","End_traject_lat.x")], 
            file = paste(workspace,"/output/locs_info",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)

