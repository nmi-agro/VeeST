# Data import and processing

# 0. Load packages -----------------------------------------------------------
library(data.table)
library(sf)
library(R.utils)
library(dplyr)
library(ggplot2)
library(ggnewscale)
library(nngeo) #azimuth
library(readxl)
library(plotly)
library(RColorBrewer)
require(ggpubr)

# 0.1. Settings and functions ----------------------------------------------------------------
workspace <- paste0(Sys.getenv("NMI-SITE"), 'O 1900 - O 2000/1922.N.23 VeeST vwsloot vd toekomst/05. Data/')
workspace2 <- paste0("c:/Users/LauraMoria/Stichting Veenweiden Innovatiecentrum/VIPNL Themas - VIPNL Veenweidensloot/D. Data en analyse/")

## Load custom functions-----------------------------------------------------
source(paste0("scripts/functions/functions_veest.R"))

# 1. Load hulpdata -------------------------------------------------
## 1.1 clusters ---------------------------------------
cluster <- read_sf(paste0(workspace,"./GIS/clusters_versie20240318.gpkg"))
setDT(cluster)
cols_clus <- c('drlg','breedtewl','trofie',"A_SOM_LOI" ,"A_CLAY_MI" )
cluster.med <- cluster[,lapply(.SD,median,na.rm=TRUE),.SDcols=cols_clus, by = 'clusters']
setnames(cluster.med, c('clusters','drlg','breedtewl','trofie',"A_SOM_LOI" ,"A_CLAY_MI" ),
         c('clusters','drglg','watbte','trofie',"OS_perc_OR_25" ,"Z_CLAY_SA_OR_25"))
cluster <- st_as_sf(cluster)
aan <- read_sf(paste0(workspace,"./GIS/AAN_niveau3b.shp"))

## 1.2 load was/ wordt locaties-----------------------------------------
locaties <- readxl::read_excel(paste0(workspace2, 'analysePlan/wp_locaties_naam_correcties.xlsx'), sheet = 'locaties')
setDT(locaties)
locaties[,gebied := GebiedID]
locaties[is.na(gebied),gebied := sapply(strsplit(SlootID, '_'), `[`, 1)]
locaties[,sloot := sapply(strsplit(SlootID, '_'), `[`, 2)]
locaties[,oever := sapply(strsplit(SlootID, '_'), `[`, 4)]
locaties$geom <- sprintf("LINESTRING(%s %s, %s %s)", locaties$Start_traject_lat,locaties$Start_traject_long,locaties$End_traject_lat,locaties$End_traject_long)
locaties <- locaties[geom == "LINESTRING(NA NA, NA NA)", geom:= "LINESTRING(0 0, 0 0)" ]
locaties <- st_as_sf(locaties, wkt = "geom", crs = 4326)
locaties <- st_as_sf(locaties) %>% st_transform(crs = 28992)

## 1.3 create cluster per loc data---------------------------
clusters_locs <- st_join(locaties, cluster, st_nearest_feature, left = TRUE)
# sel verschillende indicatoren
# afwatopp: oppvl/ (omtrek_nat/ 2) brede percelen met weinig sloten is een hoog getal, smalle percelen met veel sloten is laag
clusters_locs <- unique(clusters_locs[,c('SlootID','SlootID_kort',"Sloot_nr","sloot","Behandeling","Oeverzijde","jaar",'Slibmonster_Bware','Oevermonster_AgroCares','WP','MeenemenDataAnalyse_totaal','clusters','trofie','afwatopp','drlg','breedtewl',"A_SOM_LOI" ,"A_CLAY_MI",'text')])
clusters_locs <- st_join(clusters_locs, aan, st_nearest_feature, left = TRUE)
setDT(clusters_locs)
# clusters_locs <- clusters_locs[WP %in% c('WP1','WP2','WP3'),]
duplicate_clust <- clusters_locs[duplicated(clusters_locs, incomparables=FALSE, fromLast=TRUE, by=c("SlootID","jaar"))|duplicated(clusters_locs,  by=c("SlootID","jaar")),]
## 1.4 load afvoergebieden------------------------------
afvoer <- st_read(paste0(workspace,'GIS/afvoergebiedaanvoergebied.gpkg'))
locaties <- st_as_sf(locaties)
afvoer_locs_intersect <- st_filter(afvoer, locaties)
setDT(afvoer_locs_intersect)
# deelstroomgebieden om te houden
afvoer_locs_intersect_extra <- afvoer_locs_intersect[code %in% c('GAF-30','GAF-118','3336','3324'),]
afvoer_locs_intersect <- afvoer_locs_intersect[!(typeafvoeraanvoergebied %in% c('Deelstroomgebied','Afwateringseenheid')),]
# haal geclusterde vrijafstromende gebieden eruit in Friesland
afvoer_locs_intersect <- afvoer_locs_intersect[!naam == 'Vrijafstromend',]
afvoer_locs_intersect <- rbind(afvoer_locs_intersect, afvoer_locs_intersect_extra)
afvoer_locs_intersect <- st_as_sf(afvoer_locs_intersect)
# 2do: idzegea moeten peilgebieden aan toegevoegd
# gebiedsnamen en codes toevoegen
gebiedkop <- st_join(afvoer_locs_intersect, locaties, st_nearest_feature, left = TRUE)
gebiedkop <- unique(gebiedkop[,c('code','naam','typeafvoeraanvoergebied','GebiedID','Gebiedsnaam','WP','MeenemenDataAnalyse_totaal','geometrie2d')])
# Find the row with "Stein" in the name
stein_row <- gebiedkop[grepl("Stein", gebiedkop$naam, ignore.case = TRUE), ]
stein_row$Gebiedsnaam <- "Stein Noord"
# Add the new row to gebiedkop
gebiedkop <- rbind(gebiedkop, stein_row)
# Find the row with "assendelft" in the name
stein_row <- gebiedkop[grepl("assendelft", gebiedkop$naam, ignore.case = TRUE), ]
stein_row$Gebiedsnaam <- "Zuiderveen"
# Add the new row to gebiedkop
gebiedkop <- rbind(gebiedkop, stein_row)
agv_demmerik <- st_read(paste0(workspace,'GIS/EAG.gpkg'))
agv_demmerik <- agv_demmerik[agv_demmerik$Code == '2500-EAG-2',]
# First prepare agv_demmerik to match the structure of gebiedkop
agv_demmerik_for_gebiedkop <- agv_demmerik[, c("Code", "Naam", "geom")]
setDT(agv_demmerik_for_gebiedkop)
# Add missing columns to match gebiedkop structure
agv_demmerik_for_gebiedkop[, `:=`(
  code = Code,
  naam = Naam,
  nen3610id = "onbekend",
  typeafvoeraanvoergebied = "Ecologisch deelgebied",
  GebiedID = "DE",
  Gebiedsnaam = "Reservaat Demmerik",
  geometrie2d = geom
)]
# Select only the columns that match gebiedkop
agv_demmerik_for_gebiedkop <- agv_demmerik_for_gebiedkop[, .(code, naam, nen3610id, typeafvoeraanvoergebied, GebiedID, Gebiedsnaam, geometrie2d)]
# Convert gebiedkop to data.table if it's still sf
gebiedkop <- st_cast(st_as_sf(gebiedkop), "MULTIPOLYGON")
agv_demmerik_for_gebiedkop <- st_cast(st_as_sf(agv_demmerik_for_gebiedkop), "MULTIPOLYGON")
# Set the CRS to match
setDT(gebiedkop)
# Add to gebiedkop
gebiedkop <- rbind(gebiedkop, agv_demmerik_for_gebiedkop, fill = TRUE)
unique(locaties$Gebiedsnaam[!locaties$Gebiedsnaam %in% unique(gebiedkop$Gebiedsnaam)])
gebiedkop[is.na(WP), WP := 'WP1']

gebiedkop <- st_as_sf(gebiedkop)
gebiedkop <- st_make_valid(gebiedkop)
st_write(gebiedkop, paste0(workspace,'GIS/afvoergebiedaanvoergebied_locs_f.gpkg'), append = FALSE)

## 1.5 load loc correctie oeverzijde------------------------------------
gpsoevers <- st_read(paste0(workspace,'GIS/gps_penetrometer_oevers.gpkg'))
## 1.6 load waterschapsgrenzen-------------------------------
waterschappen <- st_read(paste0(workspace,'GIS/2019_waterschappen_grenzen.gpkg'))

# 2. Penetrometer----------------------------------------------
## 2.1.1 import 2024----------------------------------------------
### import gps dxf files------------------------------------------------------
gps <-  importGPS(inputdir = paste0(workspace,"./GPS/2024"))
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
gps2 <-  importGPS2(inputdir = paste0(workspace,"./GPS/2024"), gpsid = max(gps$ID)) %>% st_transform(crs = 28992)
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

### import penetrometer data 2024----------------------------------------------
pen <- importPen(inputdir = paste0(workspace,"./Penetrometer/2024"))
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

## 2.2.1 proces 2024----------------------------------------------
### process gps files 24 --------------------------------------------------------
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
gps_24 <- gps
### postprocess penetrometer data 24---------------------------------------------
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
# add date/ jaar
penmerge[,jaar := "2024"]

## 2.1.2 Import 2025----------------------------------------------
### import gps csv data 2025--------------------------------------------------------
gps25 <-  importGPS2(inputdir = paste0(workspace,"./GPS/2025"), gpsid = max(gps$ID))
gps25 <-  st_transform(gps25, crs = 28992)
setDT(gps25)
gps25[,trajecten := 1]
# voeg volgnummer toe voor bepaling afstand tot sloot
# 1 = perceel, 2 = insteek, 3 tm 12 = oever en insteek en perceel andere oeverzijde
gps25 <- gps25[,dist_id := frank(Puntnummer, ties.method = 'dense'), by = 'ID']
# create id for filtering
coords <- data.frame(st_coordinates(st_zm(gps25$geometry)))
gps25 <- cbind(gps25,coords)
gps25[,ident:= paste0(X,'_',Y)]
gps25 <- gps25[,c('ID','name','Opmerking','trajecten','dist_id','geometry','X','Y','ident')]
## add location info for filtering and correcting
gps25[, gebied:= tstrsplit(name, "_")[1]]
gps25[, sloot:= tstrsplit(name, "_")[2]]
# correct dist id for multiple transects (2 oevers or 2 transects)
gps25[,max_dis_id := max(dist_id), by = 'name']
# make unique name 4 matching
gps25[,Opmerking := tolower(Opmerking)]
gps25[,Opmerking := lapply(.SD, function(x) gsub("\\s+|\\s+", "",x)),.SDcols = c('Opmerking')]
gps25[,plot := Opmerking]
gps25[,plot := gsub("^plor", "plot", plot)]
gps25[,plot := gsub("^polt", "plot", plot)]
gps25[,plot := gsub("plo4", "plot4", plot)]
gps25[,plot := gsub("plot8pen6rijspoor", "plot8pen6", plot)]
gps25[,plot := gsub("plot8pen5rijspoor", "plot8pen5", plot)]
gps25[,plot := gsub("plot3oen2", "plot3pen2", plot)]
gps25[,plot := gsub("pot3pen7", "plot3pen7", plot)]
gps25[,gebied:= tolower(gebied)]
gps25[grepl('WP1', name), extragebied:= 'wp1']
gps25[,oever := sapply(strsplit(name, '_'), `[`, 4)]
gps25[,oever:= toupper(oever)]
# correctie oever
gps25[name == 'IG_15_WP1_w' ,oever:= 'N']
gps25[name == 'SN_5_WP1_w' ,oever:= 'O']
gps25[name == 'SV_6_WP1_w',oever:= 'N']
gps25[name == 'SV_7_WP1_o',oever:= 'Z']
gps25[name == 'SV_8_WP1_o',oever:= 'Z']
gps25[name == 'WJV_5_WP1_n',oever:= 'W']
gps25[name == 'ZG_8_WP1_z',oever:= 'N']
gps25[name == 'ZVP_1_WP1_w',oever:= 'O']
gps25[name == 'UP_1_WP1_n',oever:= 'Z']
gps25[name == 'SW_4_M_o',oever:= 'ZO']
gps25[name == 'SW_4_M-AF_o',oever:= 'ZO']
gps25[name == 'SW_4_M-NVO_w',oever:= 'NW']
gps25[name == 'SW_4_M-AF-NVO_w',oever:= 'NW']
# extragebied toevoegen 
gps25[gebied %in% c('ad','bd','de','dp','dt','eem','ep','hw','ig','lg','lw','mb',
'ok','sn','sv','up','wjv','wl','wz','zvp','zw') , extragebied:= '']
gps25[,sloot := as.numeric(sloot)]
gps25[gebied == 'rh' & sloot %in% c(1,2,3,4,5), extragebied:= 'RH_1tm5'] #gps bevat alleen data van sloot 1 zuid, die hoort bij sloot6tm10
gps25[gebied == 'rh' & sloot %in% c(7,8,9,10,11), extragebied:= 'RH_7 tm RH_11']
gps25[gebied == 'sw' & sloot %in% c(1), extragebied:= 'SW_1_4']
gps25[gebied == 'sw' & sloot %in% c(2,3,5), extragebied:= 'SW_2_3_5']
gps25[gebied == 'sw' & sloot %in% c(4), extragebied:= 'SW_1_4']
gps25[gebied == 'zg' & sloot %in% c(1,2,3), extragebied:= 'ZG_1_3']
gps25[gebied == 'zg' & sloot %in% c(10,11,12,13), extragebied:= 'ZG_10_WP1 tm ZG_13_WP1']
gps25[gebied == 'zg' & sloot %in% c(6,7,8,9), extragebied:= 'ZG_6_WP1 tm ZG_9_WP1']
gps25[gebied == 'kw' & sloot %in% c(1), extragebied:= 'KW_1_2_M_R']
gps25[gebied == 'kw' & sloot %in% c(2) & ID %in% c(207,208), extragebied:= 'KW_1_2_M_R']
gps25[gebied == 'kw' & sloot %in% c(13,14,15,16), extragebied:= 'KW_13_WP1 tm KW_16_WP1']
gps25[gebied == 'kw' & sloot %in% c(3), extragebied:= 'KW_2_MAF_3']
gps25[gebied == 'kw' & sloot %in% c(2) & ID %in% c(206), extragebied:= 'KW_2_MAF_3']
gps25[gebied == 'sz' & sloot %in% c(1,2), extragebied:= 'SZ_1_2_WP1']
gps25[gebied == 'sz' & sloot %in% c(3,4,5,6), extragebied:= 'SZ_3_WP1 tm SZ_6_WP1']
### import penetrometer data 2025----------------------------------------------
pen <- importPen(inputdir = paste0(workspace,"./Penetrometer/2025"))
pen2 <- importPen2(inputdir = paste0(workspace,"./Penetrometer/2025"))
pen2[,Diept := as.numeric(Diept)]
pen2 <- pen2[,Diept := Diept -1]
# Selecteer alleen gemeenschappelijke kolommen
# Gemeenschappelijke kolommen
gemeenschappelijk <- intersect(colnames(pen), colnames(pen2))
pen <- pen[, ..gemeenschappelijk]
pen2 <- pen2[, ..gemeenschappelijk]
# Bind ze samen
pen <- rbind(pen, pen2)
pen <- pen[!is.na(indringingsweerstand),]
# make unique name 4 matching
pen[ ,plot:= as.numeric(gsub("^PLOTX*", "", Plotnaam))]
pen[ ,pen := as.numeric(gsub("^Pen*", "", as.character(Pen)))]
pen[ ,plot:= paste0('plot',plot,'pen',pen)]
pen[ ,gebied:= tstrsplit(name, "_")[1]]
pen[ ,gebied:=tolower(gebied)]
pen[ ,extragebied:= name]
pen[ ,extragebied:= gsub('_plot1','',extragebied)]
pen[ ,extragebied:= gsub('_plot2','',extragebied)]
pen[ ,extragebied:= gsub('_plot3','',extragebied)]
pen[ ,extragebied:= gsub('_plot4','',extragebied)]
pen[ ,extragebied:= gsub('_plot5','',extragebied)]
pen[ ,extragebied:= gsub('_plot6','',extragebied)]
pen[gebied %in% c('ad','bd','de','dp','dt','eem','ep','hw','ig','lg','lw','mb','ok','sn','sv','up','wjv','wl','wz','zvp','zw'), extragebied:= '']
## 2.2.2 proces 2025----------------------------------------------
### postprocess penetrometer data 25 ---------------------------------------------
# merge gps with pen
penmerge_25 <- merge(pen, gps25, by=c('gebied','plot','extragebied'), all = TRUE, allow.cartesian = FALSE, suffixes = c('_pen','_gps'))
# add date/ jaar
penmerge_25[,jaar := "2025"]
penmerge_25[,Diept := as.numeric(Diept)]
penmerge[,Diept := as.numeric(Diept)]
penmerge <- rbind(penmerge, penmerge_25, fill = TRUE)

## 4.3 merge with locationcorrection 24 en 25---------------------------------
setDT(locaties)
locaties[,jaar:= as.character(jaar)]
locaties_pen <- unique(locaties[,c('SlootID','SlootID_old_pengps','oever','jaar')])
locaties_pen <- locaties_pen[!is.na(SlootID_old_pengps),]
# Check voor duplicate combinaties van SlootID_old_pengps, oever en jaar
locaties_pen[, .N, by = .(SlootID_old_pengps, oever, jaar)][N > 1]

penmerge[,jaar:= as.character(jaar)]
penmerge <- merge(penmerge[!is.na(name_gps)], locaties_pen, 
                  by.x = c('name_gps','oever','jaar'), by.y = c('SlootID_old_pengps','oever','jaar'), all.x = TRUE, allow.cartesian = TRUE, suffixes = c('_pen','_locs'))
penmerge[name_gps == 'Rh_4_R' & oever == 'O', SlootID := SlootID_loc]
# 1 = perceel, 2 = insteek, 3 tm 12 = oever en insteek en perceel andere oeverzijde
penmerge[,sectie:= 'oever']
penmerge[dist_id==1,sectie:= 'perceel']
penmerge[dist_id==2,sectie:= 'insteek']
penmerge[,dieptebin := cut(as.numeric(Diept), breaks = c('0','10','20','30','40','50','85'), include.lowest = TRUE)]
penmerge[, dieptebin_num := {
  breaks <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 85)
  bin <- as.integer(cut(as.numeric(Diept), breaks = breaks, include.lowest = TRUE))
  midpoints <- (breaks[-length(breaks)] + breaks[-1]) / 2  # 5, 15, 25, 35, 45, 67.5
  midpoints[bin]
}]
penmerge_wide <- dcast(penmerge[!is.na(Diept),], SlootID+jaar~sectie+dieptebin, value.var = c('indringingsweerstand'), fun.aggregate = mean, na.rm = TRUE, fill = FALSE, drop = TRUE)

## 4.4 validatie regels penetrometer 24 en 25-------------------------
# check 4 double coordinates in pen_gps
gps <- st_as_sf(gps2)
gps2 <- st_zm(gps, crs = 28992)
coords <- as.data.table(st_coordinates(gps2))
dubbel_24 <- gps[which(duplicated(coords)),]
gps <- st_as_sf(gps25)
gps2 <- st_zm(gps, crs = 28992)
coords <- as.data.table(st_coordinates(gps2))
dubbel_25 <- gps[which(duplicated(coords)),]
# check double plots in penetrometerdata 
setDT(gps)
gps_tab <- dcast(gps25, gebied+plot+extragebied~., value.var = c('name'), fun.aggregate = uniqueN)
checkgps <- gps[, nunique := uniqueN(ident), by = c('gebied','plot','extragebied')]
checkgps <- checkgps[nunique>1,]
# check wel gps geen pen
checkgps <- merge(gps[,c('name','gebied','plot','extragebied')],pen[,c('name','gebied','plot','extragebied')], by = c('gebied','plot','extragebied'), all.x = TRUE)
checkgps <- checkgps[is.na(name.y),]
# oever present in al pen/gps data
checkgps <- unique(penmerge[is.na(oever)&!is.na(name_gps), name_gps])
# check if gps is present 4 penetrometer data
penmerge[,uni_plots := uniqueN(plot), by = c('name_pen','extragebied','trajecten')]
penmergecheck_misgps <- unique(penmerge[is.na(name_gps),c('name_pen','extragebied','trajecten','uni_plots')])
# check if all locs are present
penmergecheck <- unique(penmerge[,c('SlootID','name_pen','name_gps','extragebied','trajecten','oever','jaar')])
penmergecheck[,jaar:= as.integer(jaar)]
locaties[,jaar:= as.integer(jaar)]
locs_pen <- merge(unique(locaties[,c('SlootID','oever','jaar')]), penmergecheck, by = c('SlootID','oever','jaar'), all.x =TRUE, suffixes = c('_locs','_pen'))
# Controleer verschillen unieke combinaties in beide tabellen
setdiff(
  unique(locaties_pen[, .(SlootID, oever, jaar)]),
  unique(penmerge[, .(SlootID, oever, jaar)])
)
# koppeltabelSlootIDpenID (alle penetro zit hierin, koppellocs kunnen missen)
slootID_penetrometerID <- dcast(penmerge, SlootID+name_gps+oever+jaar~dist_id) # check unique loc combos, soms kan een pen 2 keer worden gekoppeld (RH_9_R) daarom allow cartesian

# 3. Abiotiek ---------------------------------------------------------
## 3.1 import ----------------------------------------------------------
inputdir <- paste0(workspace,"./ODK_abiotiek")
abio <- file.info(list.files(path= paste0(inputdir), pattern=".csv", full.names =  T))
abio <- rownames(abio)[which.max(abio$mtime)]
abio <- fread(abio, dec = ',', na.strings = c(999,9999,-999,-99,'999,0','999,00','999,000','NA','999','999,0000'))
abio[, datum := as.POSIXct(datemanual) ]
abio[,jaar:= year(datum)]
abio_cols <- fread(paste0(workspace,"./hulp_tabellen/veest_kolomnamen.csv"), dec = ',')
setnames(abio, abio_cols$nieuwe_kolomnamen, abio_cols$oude_kolomnamen, skip_absent = TRUE)
# remove columns without information
cols <- colnames(abio)[unlist(abio[,lapply(.SD,function(x) sum(is.na(x))==nrow(abio))])]
cols <- c(cols,"Date_start_auto","Date_end_auto","Device_ID","Datemanual","Waarnemer","Start_traject","End_traject",
          "Start_eindpunt_container","Afstand_startpunt_eindpunt_m","Start_traject_accur_m")
abio[,c(cols):= NULL]
# set data type
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
## 3.2 process abio-----------------------------------------------------
# correct values
abio[watertemp_C > 50, watertemp_C := NA]
# merge with unique/ koppelnames
setDT(locaties)
# check if instanceID locaties allemaal voorkomen in abio
# abio_loc_instanceidcheck <- unique(abio[!instanceID %in% unique(locaties$instanceID_abio),c("SlootID",'instanceID')])
# loc_abio_instanceidcheck <- unique(locaties[!instanceID_abio %in% unique(abio$instanceID),c("SlootID",'instanceID_abio')])
abio <- merge(abio, locaties, by.x ='instanceID', by.y ='instanceID_abio', all.x = TRUE, all.y = FALSE, suffixes = c('_abio',''))
#check if slootID_old_abio == SlootID_abio
check_abio <- unique(abio[!SlootID_old_abio == SlootID_abio, c('SlootID_old_abio','SlootID_abio')])
abio[, SlootID_old_abio := SlootID_abio]
abio[, instanceID_abio := instanceID]
abio[, instanceID := NULL]
# 1. Begin met de meest specifieke aggregatie
# Alleen numerieke kolommen + join keys meenemen in aggregatie
cols_num <- colnames(abio)[sapply(abio, is.numeric)]
# Verwijder join keys uit cols_num om duplicaten te voorkomen
cols_num <- setdiff(cols_num, c("SlootID", "jaar", "gebied", "sloot", "Behandeling"))
abio_slib_agg1 <- abio[, lapply(.SD, mean, na.rm = TRUE), .SDcols = cols_num, by = .(SlootID, jaar)]
abio_slib_agg2 <- abio[, lapply(.SD, mean, na.rm = TRUE), .SDcols = cols_num, by = .(gebied, sloot, Behandeling, jaar)]
abio_slib_agg3 <- abio[, lapply(.SD, mean, na.rm = TRUE), .SDcols = cols_num, by = .(gebied, sloot, jaar)]
# 2. Voeg de hiërarchische info toe aan abio
abio_hier <- unique(abio[, .(SlootID, SlootID_kort, gebied, Gebiedsnaam, sloot, Behandeling, jaar, instanceID_abio,instanceID_veg,datum,
uitraster_perc,uitraster_afstand_sloot_m,afscheur_veg_lengte_perc,afscheur_veg_breedte_cm,
landgebruik_traject,landgebruik_overkant,
beheersporen_water1_2a_hoeveel,beheersporen_water1_2a_welke,beheersporen_water1_2a_overig,
beheersporen_oever2b_3_hoeveel,beheersporen_oever2b_3_welke,beheersporen_oever2b_3_overig,
peilsporen_hoeveel,peilsporen_richting,peilsporen_uitleg,ondergrondse_drainage)])
# 3. Merge stap voor stap, vul aan waar NA
abio_hier <- merge(abio_hier, abio_slib_agg1[,], by = c("SlootID", "jaar"), all.x = TRUE, suffixes = c("", "_slootid"))
# Vul aan met gebied+sloot+behandeling waar NA
for (col in cols_num) {
  abio_hier[is.na(get(col)), (col) := abio_slib_agg2[.SD, on = .(gebied, sloot, Behandeling, jaar), get(col)]]
}
# Vul aan met gebied+sloot waar NA
for (col in cols_num) {
  abio_hier[is.na(get(col)), (col) := abio_slib_agg3[.SD, on = .(gebied, sloot, jaar), get(col)]]
}
# abio_hier bevat nu per SlootID de best beschikbare waarde per kolom
abio_hier[,behandeling_1 := sapply(strsplit(Behandeling, '-'), `[`, 1)]
abio_hier[,behandeling_2 := ifelse(grepl('AF', Behandeling),"AF",NA)]
## 3.3 validatie abiotiek -------------------------------
## check dubbele codes
abio_hier[, ndatum := uniqueN(datum), by = c('SlootID','instanceID_abio')]
checkabioloc <- unique(abio_hier[, c('SlootID','instanceID_abio','ndatum')])
### make matrix wq by ditch ID
loc.wq <- unique(abio[,c('SlootID','gebied','sloot',"Oevermonster_AgroCares","bodemmonster_oeverzone2b3","bodemmonster_oeverzone2b3_subsamples", "bodemmonster_oever2b3_boven",  
                         "bodemmonster_oever2b3_boven_subsamples"  ,"bodemmonster_oeverzone2b3_diep", "bodemmonster_oeverzone2b3_diep_subsamples", "slibmonster","slibmonster_subsamples","bodemmonster_water","porievochtmonster",                        
                         "porievochtmonster_subsamples","porievochtmonster_code","Waterkwaliteitmonster", "watermonster_subsamples","watermonster_code")])
# check if all locations are present in abio
check_db <- locaties[!SlootID %in% unique(abio$SlootID),c('SlootID','oever','jaar','instanceID_abio')]

# 4. Slootprofielen ---------------------------------------------------
## 6.1 Import ---------------------------------------
profiel_24 <-  importGPSprof(inputdir = paste0(workspace,"./GPS slootprofielen/2024"))
setDT(profiel_24)
profiel_24[,jaar := 2024]
profiel_24[,ID := paste0(ID,'_24')]
profiel_24[name == 'AD_3_WP1', name :='ZV_3_WP1'] # fout in naamgeving
profiel_25 <-  importGPSprof(inputdir = paste0(workspace,"./GPS slootprofielen/2025"))
setDT(profiel_25)
profiel_25[,jaar := 2025]
profiel_25[,ID := paste0(ID,'_25')]
profiel <- rbind(profiel_24, profiel_25)
## 6.2 Postprocess profielen-----------------------------------------------------
### 6.2.1 correct Mijnden -------------------------------------
setDT(profiel)
profiel[name == 'MD_1_NVO' & Puntnummer == 11 & jaar == 2024, Opmerking := 'waterlijn']

### 6.2.2 process data --------------
# extract label waterlijn
profiel[grepl('Waterlijn', Opmerking), Opmerking := 'waterlijn']
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
# sectie when insteek is reported x 2
profiel[grepl('insteek', Opmerking), numov := Puntnummer]
profiel[, numov_min := min(numov, na.rm = TRUE), by = "ID"]
profiel[, numov_max := max(numov, na.rm = TRUE), by = "ID"]
profiel[!(numov_min == numov_max) & Puntnummer <= numov_max & Puntnummer > numwl_max , sectie := 'oever']
profiel[!(numov_min == numov_max) & Puntnummer >= numov_min & Puntnummer < numwl_min, sectie := 'oever']
profiel[!(numov_min == numov_max) & Puntnummer > numov_max, sectie := 'perceel']
profiel[!(numov_min == numov_max) & Puntnummer < numov_min, sectie := 'perceel']
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
profiel[,slib := slib - wtd]
# max slibdikte
profiel[,max_slib := max(slib, na.rm = T), by = 'ID']
# breedte water
profiel[, watbte := dist[Puntnummer == numwl_max]-dist[Puntnummer == numwl_min], by ='ID']
# breedte oever
profiel[, oevbte := dist[Puntnummer == numwl_min]-dist[Puntnummer == min(Puntnummer[sectie == 'oever'])], by =c ('ID','sectie_2')]

### 6.2.2a correct Idzegeap -------------------------------------
profiel[name == 'IG_10_WP1' & Puntnummer > 31 & jaar == 2025, sectie := 'perceel']
profiel[name == 'WZ_1_WP1' & Puntnummer > 45 & jaar == 2025, sectie := 'perceel']

### 6.2.3 taludhoek berekenen ------------------------------
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
# filter coordinaten die niet bij dwarsprofiel horen
profiel <- profiel[!Opmerking %in% c('einde','eindpunt','begin','beginpunt','startpunt'),]

for(i in unique(profiel$ID)){
  profiel_nr <- profiel[profiel$ID == i,]
  talud_calc <- calc_taludhoek(profiel_nr)
  talud_calc_1 <- talud_calc[,c('tldk_bvwtr_perc_1','tldk_ondwtr_perc_1','tldk_wtrwtr_perc_1','tldk_oevrwtr_perc_1','tldk_vastbodem_perc_1')]
  profiel[profiel$ID == i & sectie_2 ==1, c('tldk_bvwtr_perc', 'tldk_ondwtr_perc','tldk_wtrwtr_perc','tldk_oevrwtr_perc','tldk_vastbodem_perc') := talud_calc_1]
  talud_calc_2 <- talud_calc[,c('tldk_bvwtr_perc_2','tldk_ondwtr_perc_2','tldk_wtrwtr_perc_2','tldk_oevrwtr_perc_2','tldk_vastbodem_perc_2')]
  profiel[profiel$ID == i & sectie_2 ==2, c('tldk_bvwtr_perc', 'tldk_ondwtr_perc','tldk_wtrwtr_perc','tldk_oevrwtr_perc','tldk_vastbodem_perc') := talud_calc_2]
}

### 6.2.4 drooglegging berekenen ------------------------------
setDT(profiel)
profiel[sectie == 'oever', max_hgt_or := max(z, na.rm =T), by = 'ID']
profiel[, drglg := max_hgt_or - mean(wl, na.rm = TRUE), by = 'ID']
profiel[, drglg_2 := max(z, na.rm =T) - mean(wl, na.rm = TRUE), by = 'ID']
# aggregate and transpose
# add begin and end coords 4 intersection with locations
profiel[, c('x_begin', 'y_begin') := list(x[Puntnummer == min(Puntnummer)],y[Puntnummer == min(Puntnummer)]), by = c('ID','sectie_2')]
profiel[, c('x_eind', 'y_eind') := list(x[Puntnummer == max(Puntnummer)],y[Puntnummer == max(Puntnummer)]), by = c('ID','sectie_2')]
profiel[, geom := sprintf("LINESTRING(%s %s, %s %s)", x_begin, y_begin, x_eind, y_eind)]

## 6.3 Aggregate profiel 2 data wide -------------------------
profiel_wide <- dcast(profiel,name+geom+sectie_2+jaar~., value.var=c('max_slib','max_wtd','watbte','drglg','drglg_2','oevbte',
                                                                'tldk_bvwtr_perc','tldk_ondwtr_perc','tldk_wtrwtr_perc','tldk_oevrwtr_perc',
                                                                'tldk_vastbodem_perc','max_hgt_or','wl'), 
                      fun.aggregate = mean, na.rm = TRUE, fill = FALSE, drop = TRUE)
profiel_wide_2 <- dcast(profiel,name+sectie_2+jaar~sectie, value.var=c('mean_talud','mean_talud_os'), 
                      fun.aggregate = mean, na.rm = TRUE, fill = FALSE, drop = TRUE)
profiel_wide <- merge(profiel_wide,profiel_wide_2, by=c('name','sectie_2','jaar'))
profiel_wide <- profiel_wide[,-c('mean_talud_os_oever','mean_talud_os_perceel')]
# select best talud
setDT(profiel_wide)
profiel_wide[is.na(tldk_wtrwtr_perc), tldk_wtrwtr_perc := tldk_ondwtr_perc]
profiel_wide[is.na(tldk_oevrwtr_perc), tldk_oevrwtr_perc := tldk_bvwtr_perc]
profiel_wide <- st_as_sf(profiel_wide, wkt = "geom", crs = 28992)
profiel_wide <- profiel_wide[,c('name','sectie_2','jaar','max_slib','max_wtd','watbte','oevbte','drglg','drglg_2',
                                'tldk_wtrwtr_perc','tldk_oevrwtr_perc',
                                'tldk_vastbodem_perc','max_hgt_or','wl','geom')]

### 6.3.1 intersect locations with profiel_wide ---------------------
locaties <- st_as_sf(locaties) %>% st_transform(crs = 28992)
locs_prof <- st_intersection(profiel_wide, locaties[!is.na(locaties$SlootID_old_profiel),c('SlootID','jaar','SlootID_old_profiel','oever')])
# Difference check: deze profielen missen in locs_prof dus doorkruizen geen geometrie van locaties
locs_prof_diff <- profiel_wide[!profiel_wide$name %in% locs_prof$name,]
# Check double values, when present an error in veseq is given when profiel en prof_locs are merged
setDT(locs_prof)
uniqueN(locs_prof[,c('name','sectie_2','jaar')])
locs_prof_double <- as.data.table(table(locs_prof[,c('name','sectie_2','jaar')])) # mag 0 en 1 zijn
# Check double slootIDs als twee profielen hetzelfde traject doorkruizen
locs_prof_double <- as.data.table(table(locs_prof[,c('SlootID')])) # mag 1 zijn
# DR_4_M-AUG wordt twee keer doorkruist door een profiel: mogelijk later een gemiddelde van talud etc. per slootID nemen
locs_prof <- locs_prof[!(SlootID == 'DR_4_M-AUG_Z' & name == '4_nak_reg_s2'),]
locs_prof <- locs_prof[!(SlootID == 'IG_15_WP1_N' & sectie_2 == 2),]
locs_prof <- locs_prof[!(SlootID == 'IG_14_WP1_N' & sectie_2 == 2),]
# add loc code
setDT(profiel)
setDT(locs_prof)
# select only shore based on intersect with locs
profiel <- merge(profiel, locs_prof, by.x =c('name','sectie_2','jaar'), by.y = c('name','sectie_2','jaar'), all.x = TRUE, all.y = FALSE, suffixes= c('','_locs'))
## 6.3 check voor dubbele locatiecodes in profielen ------------------
uniqueN(profiel$name) == uniqueN(profiel$ID)
checkloc <- dcast(profiel, name+jaar~., value.var = 'ID', fun.aggregate = uniqueN)
# check 4 floating secties 
check_floating_secties <- profiel[!sectie == shift(sectie,-1) & !sectie == shift(sectie,+1)]

# 5. Vegetatie --------------------------------------------------------
## 5.1 import odk ----------------------------------------------------------
inputdir <- paste0(workspace,"./ODK_vegetatie")
veg <- file.info(list.files(path= paste0(inputdir), pattern=".csv", full.names =  T))
veg <- rownames(veg)[which.max(veg$mtime)]
veg <- fread(veg, sep= ';', dec = '.', na.strings = c(999,9999,-999,-99,'999,0','NA','999'), fill = TRUE)
setnames(veg, abio_cols$nieuwe_kolomnamen, abio_cols$oude_kolomnamen, skip_absent = TRUE)
veg[, datum := as.Date(Date_start_auto) ]
veg[, jaar:= year(datum)]
# merge with unique/ koppelnames
setDT(locaties)
# veg_loc_instanceidcheck <- unique(veg[!instanceID %in% unique(locaties$instanceID_veg),c("SlootID",'jaar','instanceID')])
# loc_veg_instanceidcheck <- unique(locaties[!instanceID_veg %in% unique(veg$instanceID),c("SlootID",'jaar','instanceID_veg')])
veg <- merge(veg, locaties[,c('SlootID','SlootID_old_veg','instanceID_veg')] , by.x = 'instanceID', by.y = 'instanceID_veg', all.x = TRUE, all.y = FALSE, suffixes = c('_veg','_locs'))
# check if all abio locs are present in locaties
locs_veg <- unique(veg[,c('jaar','SlootID_locs','SlootID_veg','instanceID')])
veg[, SlootID := SlootID_locs];veg[,SlootID_veg := NULL];veg[,SlootID_locs := NULL]
veg <- veg[!is.na(SlootID),]
write.table(locs_veg, paste0(workspace,"/hulp_tabellen/locs_veg_instanceidcheck.csv"), sep = ';', dec = '.', row.names = FALSE)
## 5.2 import vegetatiesoortdata ------------------------------------------------
library(readxl)
veg_srt <- read_xlsx(paste0(inputdir,'/vegetatieopnames_vera_odk.xlsx'))
setDT(veg_srt)
veg_srt[SlootID == 'AD_3_WP1_Z' & jaar == 2025, SlootID := 'AD_3_WP1_Z'] # fout in naamgeving
veg_srt[SlootID == 'OK_4_WP1' & jaar == 2025, SlootID := 'OK_4_WP1_N'] # fout in naamgeving
check_db <- locaties[!SlootID %in% unique(veg_srt$SlootID),]
biotaxon <- read_xlsx(paste0(workspace,'/hulp_tabellen/veest_unieke_soorten_Groeivormen toegevoegd.xlsx'))
veg_srt<- merge(veg_srt, biotaxon, by = 'wetnaam', all.x = TRUE, suffixes = c('','_biotaxon'))
### 5.2.1 unieke soorten per monster ------------------------------------------------
veg_srt[, Submerse_groeivorm := as.numeric(Submerse_groeivorm)]
veg_sub_srt <- unique(veg_srt[Submerse_groeivorm > 20, c('wetnaam','nednaam')])
veg_srt[, Emerse_groeivorm := as.numeric(Emerse_groeivorm)]
veg_nsoorten_sub <- dcast(veg_srt[Submerse_groeivorm > 20,], SlootID+jaar~zone, value.var = 'wetnaam', fun.aggregate = uniqueN)
veg_nsoorten_oev <- dcast(veg_srt[Emerse_groeivorm > 20,], SlootID+jaar~zone, value.var = 'wetnaam', fun.aggregate = uniqueN)
veg_nsoorten_oev[`2` == 0, `2`:= `2a`+`2b`]
veg_nsoorten <- merge(veg_nsoorten_sub[,c('SlootID','jaar','1')], veg_nsoorten_oev[,c('SlootID','jaar','2','2a','2b')], by = c('SlootID','jaar'), all = TRUE)
setnames(veg_nsoorten, c('1','2','2a','2b'), c('n_soorten_sub_zone1','n_soorten_oev_zone2','n_soorten_oev_zone2a','n_soorten_oev_zone2b'))
# 6. Waterbodemdata en waterkwaliteit---------------------------------------------------
## Bware 2024
inputdir <- paste0(workspace,"./Bodemanalyses/data VeeST_data_2024_aangepast_27-02-2025.csv")
watbod <- fread(inputdir, dec = '.', na.strings = c(-999,'NA',''), encoding = "Latin-1")
watbod[, datum := as.POSIXct(datum_PW, format = "%d-%m-%Y") ]
watbod[,jaar:= year(datum)]
watbod<- watbod[!is.na(SlootID) & !is.na(`Fe_mmol/kg DW_SB`) & !is.na(`P_mmol/kg DW_SB`), ]
## Bware 2025
inputdir <- paste0(workspace,"./Bodemanalyses/Data VEEST 2025_2.csv")
watbod_25 <- fread(inputdir, dec = '.', na.strings = c(-999,'Niet gedaan','niet gedaan','foutmeting'), encoding = "Latin-1")
watbod_25[, datum := as.POSIXct(datum, format = "%d-%m-%Y")]
watbod_25 <- watbod_25[,jaar:= year(datum)]
watbod_25 <- watbod_25[!is.na(SlootID) & !is.na(`Fe_mmol/kg DW_SB`) & !is.na(`P_mmol/kg DW_SB`), ]
# merge 2024 and 2025 data
watbod <- rbind(watbod, watbod_25, fill = TRUE)
# watbod[is.na(Waterkwaliteitmonster), Waterkwaliteitmonster := waterkwaliteitsmonster]
# lege kolommen verwijderen
cols <- colnames(watbod)[unlist(watbod[,lapply(.SD,function(x) sum(is.na(x))==nrow(watbod))])]
watbod <- watbod[, !cols, with = FALSE]
watbod[, feP_DW_SB := `Fe_mmol/kg DW_SB`/`P_mmol/kg DW_SB`]
watbod[, feS_DW_SB := `Fe_mmol/kg DW_SB`/`S_mmol/kg DW_SB`]
watbod[, feP_PW := `Fe_µmol/l_PW`/`P_µmol/l_PW`]
watbod[, feS_PW := `Fe_µmol/l_PW`/`S_µmol/l_PW`]
watbod[, jaar:= year(datum)]
watbod_proj <- watbod[SlootID %in% c('RH_4_R_N','RH_4_M-AF_N'), ]
watbod_proj[, SlootID := gsub("_N$", "_Z", SlootID)]
watbod <- rbind(watbod, watbod_proj, fill = TRUE)
## ArgoCares 2024
inputdir <- paste0(workspace,"./Bodemanalyses/AgroCares_CustomPackage_Slib_1922.N.23_27-02-2025 COMPLETE.xlsx")
tabbladen <- excel_sheets(inputdir)
tab <- tabbladen[grepl("coding", tolower(tabbladen))]
watbod_coding <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab))
tab <- tabbladen[grepl("P-AL", tabbladen)]
watbod_pal <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab))
watbod_pal[,`P-AL mg p2o5/100g` := (`P-AL mg/kg`/10)*2.29]
tab <- tabbladen[grepl("ICP-MS_CC", tabbladen)]
cnames <- as.data.table(readxl::read_xlsx(path = inputdir, sheet = tab, n_max = 1))
cnames <- paste0(colnames(cnames),"_" ,as.character(cnames[1]))
cnames <- gsub("GSL_ID_NA","GSL_ID",cnames)
watbod_icpcc <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab, skip = 2, col_names = cnames))
tab <- tabbladen[grepl("DA_CC", tabbladen)]
cnames <- colnames(readxl::read_xlsx(path = inputdir, sheet = tab, n_max = 1))
watbod_dacc <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab, skip = 1, na ='below LOD', col_names = cnames))
tab <- tabbladen[grepl("pH_CC", tabbladen)]
cnames <- colnames(readxl::read_xlsx(path = inputdir, sheet = tab, n_max = 1))
watbod_phcc <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab, skip = 1, na = '',col_names = cnames))
watbod_phcc[,pH_CC := as.numeric(pH_CC)]
# format van dit xrf blad wijkt af
tab <- tabbladen[grepl("XRF", tabbladen)]
cnames <- as.data.table(readxl::read_xlsx(path = inputdir, sheet = tab, n_max = 3))
cnames <- paste0(as.character(cnames[1]),"_" ,as.character(cnames[2]))
cnames <- gsub("GSL_ID_NA","GSL_ID",cnames)
watbod_xrf <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab, skip = 3, col_names = cnames))
# De XRF wordt gemeten op monsters die tot 105 graden zijn gedroogd en de waardes worden uitgedrukt per kg grond die tot 105 graden is gedroogd. Alle andere parameters worden uitgedrukt per kg grond die tot 40 graden is gedroogd.
# In veengronden gaat een deel van de matrix verloren als je tot 105 graden droogt. Het heeft dus zin om de XRF data uit te drukken per kg grond (40 graden).
watbod_vocht <- setDT(readxl::read_xlsx(path = paste0(workspace,"./Bodemanalyses/Moisture 105 C on project 1922 (2024 samples).xlsx"), na = ''))
# calculate correction factor
watbod_xrf <- merge(watbod_xrf, watbod_vocht[,c('GSL CODE','Moist 105C (%)')], by.x = 'GSL_ID', by.y = 'GSL CODE',all.x = TRUE, all.y = FALSE, suffixes = c('','_vocht'))
watbod_xrf[, correction_factor := (100 + `Moist 105C (%)`)/100]
# correct xrf values to per kg dry at 40 degrees
xrf_cols <- colnames(watbod_xrf)[grepl('^C',colnames(watbod_xrf))]
watbod_xrf[, (xrf_cols) := lapply(.SD, function(x) x / correction_factor), .SDcols = xrf_cols]
#put all data frames into list
df_list <- list(watbod_coding,watbod_pal,watbod_icpcc,watbod_dacc,watbod_phcc,watbod_xrf)      
#remove duplicate columns
cols <- as.data.table(lapply(df_list, function(x) { colnames(x) }))
lapply(df_list, function(x) { x[,c("ID",'ID.x','ID.y','ID_NA','...1','NA_NA'):= NULL]})
#merge all data frames together
watbod_ac <- Reduce(function(x, y) merge(x, y, by = 'GSL_ID', all=TRUE), df_list) 
watbod_ac <- watbod_ac[!is.na(GSL_ID),]
# change colnames
colnames(watbod_ac) <- gsub("^C \\(", "", colnames(watbod_ac))
colnames(watbod_ac) <- gsub(")_", "_xrf_", colnames(watbod_ac))
colnames(watbod_ac) <- gsub("\r\n", "_", colnames(watbod_ac))
watbod_ac[, jaar := 2024]

## ArgoCares 2025
inputdir <- paste0(workspace,"./Bodemanalyses/NMI Custom Package 26AC0064 - 26AC0182 PARTIAL 03_03_2026.xlsx")
tabbladen <- excel_sheets(inputdir)
tab <- tabbladen[grepl("coding", tolower(tabbladen))]
watbod_coding <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab))
tab <- tabbladen[grepl("P-AL", tabbladen)]
watbod_pal <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab))
watbod_pal[,`P-AL mg p2o5/100g` := (`P-AL mg/kg`/10)*2.29]
tab <- tabbladen[grepl("ICP-MS_CC", tabbladen)]
cnames <- as.data.table(readxl::read_xlsx(path = inputdir, sheet = tab, n_max = 1,skip = 1))
cnames <- paste0(colnames(cnames),"_" ,as.character(cnames[1]))
cnames <- gsub("GSL_ID_NA","GSL_ID",cnames)
watbod_icpcc <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab, skip = 3, col_names = cnames))
tab <- tabbladen[grepl("DA_CC", tabbladen)]
cnames <- colnames(readxl::read_xlsx(path = inputdir, sheet = tab, n_max = 1, skip = 1))
watbod_dacc <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab, skip = 2, na ='below LOD', col_names = cnames))
tab <- tabbladen[grepl("pH_CC", tabbladen)]
cnames <- colnames(readxl::read_xlsx(path = inputdir, sheet = tab, n_max = 1))
watbod_phcc <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab, skip = 1, na = '',col_names = cnames))
watbod_phcc[,pH_CC := as.numeric(pH_CC)]
# dit xrf blad is er nog niet
# tab <- tabbladen[grepl("XRF", tabbladen)]
# cnames <- as.data.table(readxl::read_xlsx(path = inputdir, sheet = tab, n_max = 3))
# cnames <- paste0(as.character(cnames[1]),"_" ,as.character(cnames[2]))
# cnames <- gsub("GSL_ID_NA","GSL_ID",cnames)
# watbod_xrf <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab, skip = 3, col_names = cnames))
# # calculate correction factor
# watbod_xrf <- merge(watbod_xrf, watbod_vocht[,c('GSL CODE','Moist 105C (%)')], by.x = 'GSL_ID', by.y = 'GSL CODE',all.x = TRUE, all.y = FALSE, suffixes = c('','_vocht'))
# watbod_xrf[, correction_factor := (100 + `Moist 105C (%)`)/100]
# # correct xrf values to per kg dry at 40 degrees
# xrf_cols <- colnames(watbod_xrf)[grepl('^C',colnames(watbod_xrf))]
# watbod_xrf[, (xrf_cols) := lapply(.SD, function(x) x / correction_factor), .SDcols = xrf_cols]

#put all data frames into list
df_list <- list(watbod_coding,watbod_pal,watbod_icpcc,watbod_dacc,watbod_phcc)      
#remove duplicate columns
cols <- as.data.table(lapply(df_list, function(x) { colnames(x) }))
lapply(df_list, function(x) { x[,c("ID",'ID.x','ID.y','ID_NA','...1','NA_NA'):= NULL]})
#merge all data frames together
watbod_ac_25 <- Reduce(function(x, y) merge(x, y, by = 'GSL_ID', all=TRUE), df_list) 
watbod_ac_25 <- watbod_ac_25[!is.na(GSL_ID),]
# change colnames
colnames(watbod_ac_25) <- gsub("^C \\(", "", colnames(watbod_ac_25))
# colnames(watbod_ac_25) <- gsub(")_", "_xrf_", colnames(watbod_ac_25))
colnames(watbod_ac_25) <- gsub("\r\n", "_", colnames(watbod_ac_25))
watbod_ac_25[, jaar := 2025]

# merge 24 and 25 data
watbod_ac <- rbind(watbod_ac, watbod_ac_25, fill = TRUE)
## process data bodem
#calc ratios
watbod_ac[, feP_CC_SB := (`Fe_CC_mg/kg`/ 5584.5)/(`P_CC_mg/kg`/ 3097.3762)]
watbod_ac[, feP_XRF_SB := (`Fe2O3_xrf_g/kg`/ 55.845)/(`P2O5_xrf_g/kg`/ 30.973762)]
watbod_ac[, feS_CC_SB := (`Fe_CC_mg/kg`/ 5584.5)/(`S_CC_mg/kg`/ 3206.5)]
watbod_ac[, feS_XRF_SB := (`Fe2O3_xrf_g/kg`/ 55.845)/(`SO3_xrf_g/kg`/ 80.063)]
#calculate p-org
watbod_ac[, `P_CC_org_mg/kg` := `P_CC_mg/kg` - `P-PO4_CC_mg/kg`]
colnames(watbod_ac) <- paste0(colnames(watbod_ac),'_SB')
watbod_ac[, jaar := jaar_SB]

# 7. Bodemgegevens oever ----------------------------------------------
## ArgoCares 2024 --------------------------------------------------------
inputdir <- paste0(workspace,"./Bodemanalyses/AgroCares_FullPackageCustom_Oever_1922_12-11-2024.xlsx")
tabbladen <- excel_sheets(inputdir)
tab <- tabbladen[grepl("coding", tolower(tabbladen))]
watbod_coding <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab))
tab <- tabbladen[grepl("liab", tolower(tabbladen))]
cnames <- colnames(readxl::read_xlsx(path = inputdir, sheet = tab, n_max = 1))
watbod_liab <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab, skip = 1, na ='', col_names = cnames))
watbod_liab <- watbod_liab[ , lapply(.SD,as.numeric), .SDcols = (7:84), by = 'GSL_ID']
tab <- tabbladen[grepl("Mehlich-3", tabbladen)]
cnames <- as.data.table(readxl::read_xlsx(path = inputdir, sheet = tab, n_max = 1))
cnames <- paste0(colnames(cnames),"_" ,as.character(cnames[1]))
cnames <- gsub("GSL_ID_NA","GSL_ID",cnames)
watbod_m3 <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab, skip = 2, col_names = cnames))
tab <- tabbladen[grepl("TOC", tabbladen)]
cnames <- colnames(readxl::read_xlsx(path = inputdir, sheet = tab, n_max = 1))
watbod_toc <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab, skip = 1, na ='below LOD', col_names = cnames))
tab <- tabbladen[grepl("Cohex", tabbladen)]
cnames <- as.data.table(readxl::read_xlsx(path = inputdir, sheet = tab, n_max = 1))
cnames <- paste0(colnames(cnames),"_" ,as.character(cnames[1]))
cnames <- gsub("GSL_ID_NA","GSL_ID",cnames)
watbod_cohex <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab, skip = 2, na ='below LOD', col_names = cnames))
tab <- tabbladen[grepl("P-AL", tabbladen)]
watbod_pal <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab))
watbod_pal[,`P-AL mg p2o5/100g` := (`P-AL mg/kg`/10)*2.29]
tab <- tabbladen[grepl("ICP-MS_CC", tabbladen)]
cnames <- as.data.table(readxl::read_xlsx(path = inputdir, sheet = tab, n_max = 1))
cnames <- paste0(colnames(cnames),"_" ,as.character(cnames[1]))
cnames <- gsub("GSL_ID_NA","GSL_ID",cnames)
watbod_icpcc <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab, skip = 2, col_names = cnames))
tab <- tabbladen[grepl("DA_CC", tabbladen)]
cnames <- colnames(readxl::read_xlsx(path = inputdir, sheet = tab, n_max = 1))
watbod_dacc <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab, skip = 1, na ='below LOD', col_names = cnames))
tab <- tabbladen[grepl("pH_CC", tabbladen)]
cnames <- colnames(readxl::read_xlsx(path = inputdir, sheet = tab, n_max = 1))
watbod_phcc <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab, skip = 1, na = '',col_names = cnames))
watbod_phcc[,pH_CC := as.numeric(pH_CC)]
tab <- tabbladen[grepl("XRF", tabbladen)]
cnames <- as.data.table(readxl::read_xlsx(path = inputdir, sheet = tab, n_max = 1))
cnames <- paste0(colnames(cnames),"_" ,as.character(cnames[1]))
cnames <- gsub("GSL_ID_NA","GSL_ID",cnames)
watbod_xrf <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab, skip = 2, col_names = cnames))
# calculate correction factor op basis van bodemvocht
watbod_vocht <- setDT(readxl::read_xlsx(path = paste0(workspace,"./Bodemanalyses/Moisture 105 C on project 1922 (2024 samples).xlsx"), na = ''))
watbod_xrf <- merge(watbod_xrf, watbod_vocht[,c('GSL CODE','Moist 105C (%)')], by.x = 'GSL_ID', by.y = 'GSL CODE',all.x = TRUE, all.y = FALSE, suffixes = c('','_vocht'))
watbod_xrf[, correction_factor := (100 + `Moist 105C (%)`)/100]
# correct xrf values to per kg dry at 40 degrees
xrf_cols <- colnames(watbod_xrf)[grepl('^C',colnames(watbod_xrf))]
watbod_xrf[, (xrf_cols) := lapply(.SD, function(x) x / correction_factor), .SDcols = xrf_cols]
#put all data frames into list
df_list <- list(watbod_coding,watbod_cohex,watbod_toc,watbod_liab,watbod_m3,watbod_pal,watbod_icpcc,watbod_dacc,watbod_phcc,watbod_xrf)      
#remove duplicate columns
cols <- as.data.table(lapply(df_list, function(x) { colnames(x) }))
lapply(df_list, function(x) { x[,c("ID",'ID.x','ID.y','ID_NA','...1','NA_NA'):= NULL]})
#merge all data frames together
oever_ac <- Reduce(function(x, y) merge(x, y, by = 'GSL_ID', all=TRUE), df_list) 
oever_ac <- oever_ac[!is.na(GSL_ID),]
# change colnames
colnames(oever_ac) <- gsub("^C \\(", "", colnames(oever_ac))
colnames(oever_ac) <- gsub(")_", "_xrf_", colnames(oever_ac))
colnames(oever_ac) <- gsub("\r\n", "_", colnames(oever_ac))
oever_ac[,jaar:= 2024]

## ArgoCares 2025-------------------------------------------------------------------------
inputdir <- paste0(workspace,"./Bodemanalyses/NMI Full Package + XRF (232 samples) p.1922.N.24 23_03_2026.xlsx")
tabbladen <- excel_sheets(inputdir)
tab <- tabbladen[grepl("coding", tolower(tabbladen))]
watbod_coding <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab))
tab <- tabbladen[grepl("liab", tolower(tabbladen))]
cnames <- colnames(readxl::read_xlsx(path = inputdir, sheet = tab, n_max = 1, skip = 1))
watbod_liab <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab, skip = 2, na ='', col_names = cnames))
watbod_liab <- watbod_liab[ , lapply(.SD,as.numeric), .SDcols = (7:84), by = 'GSL_ID']
# tab <- tabbladen[grepl("Mehlich-3", tabbladen)]
# cnames <- as.data.table(readxl::read_xlsx(path = inputdir, sheet = tab, n_max = 1))
# cnames <- paste0(colnames(cnames),"_" ,as.character(cnames[1]))
# cnames <- gsub("GSL_ID_NA","GSL_ID",cnames)
# watbod_m3 <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab, skip = 2, col_names = cnames))
tab <- tabbladen[grepl("TOC", tabbladen)]
cnames <- colnames(readxl::read_xlsx(path = inputdir, sheet = tab, n_max = 1))
watbod_toc <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab, skip = 1, na ='below LOD', col_names = cnames))
tab <- tabbladen[grepl("Cohex", tabbladen)]
cnames <- as.data.table(readxl::read_xlsx(path = inputdir, sheet = tab, skip = 1,n_max = 1))
cnames <- paste0(colnames(cnames),"_" ,as.character(cnames[1]))
cnames <- gsub("GSL_ID_NA","GSL_ID",cnames)
watbod_cohex <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab, skip = 3, na ='below LOD', col_names = cnames))
tab <- tabbladen[grepl("P-AL", tabbladen)]
watbod_pal <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab))
watbod_pal[,`P-AL mg p2o5/100g` := (`P-AL mg/kg`/10)*2.29]
tab <- tabbladen[grepl("ICP-MS_CC", tabbladen)]
cnames <- as.data.table(readxl::read_xlsx(path = inputdir, sheet = tab, n_max = 1, skip = 1))
cnames <- paste0(colnames(cnames),"_" ,as.character(cnames[1]))
cnames <- gsub("GSL_ID_NA","GSL_ID",cnames)
watbod_icpcc <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab, skip = 3, col_names = cnames))
watbod_icpcc <- watbod_icpcc[!is.na(GSL_ID),]
tab <- tabbladen[grepl("DA_CC", tabbladen)]
cnames <- colnames(readxl::read_xlsx(path = inputdir, sheet = tab, n_max = 1, skip = 1))
watbod_dacc <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab, skip = 2, na ='below LOD', col_names = cnames))
watbod_dacc <- watbod_dacc[!is.na(GSL_ID),]
tab <- tabbladen[grepl("pH_CC", tabbladen)]
cnames <- colnames(readxl::read_xlsx(path = inputdir, sheet = tab, n_max = 1))
watbod_phcc <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab, skip = 1, na = '',col_names = cnames))
watbod_phcc[,pH_CC := as.numeric(pH_CC)]
watbod_phcc <- watbod_phcc[!is.na(GSL_ID),]
tab <- tabbladen[grepl("XRF", tabbladen)]
cnames <- as.data.table(readxl::read_xlsx(path = inputdir, sheet = tab, n_max = 1))
cnames <- paste0(colnames(cnames),"_" ,as.character(cnames[1]))
cnames <- gsub("GSL_ID_NA","GSL_ID",cnames)
watbod_xrf <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab, skip = 2, na =c('below LOD',' '), col_names = cnames))
tab <- tabbladen[grepl("Moisture", tabbladen)]
watbod_vocht <- setDT(readxl::read_xlsx(path = inputdir, sheet = tab))
watbod_vocht[,`Moist 105C (%)`:= `MOISTURE (%)`]
# calculate correction factor op basis van bodemvocht
watbod_xrf <- merge(watbod_xrf, watbod_vocht[,c('GSL_ID','Moist 105C (%)')], by = 'GSL_ID', all.x = TRUE, all.y = FALSE, suffixes = c('','_vocht'))
watbod_xrf <- watbod_xrf[!is.na(GSL_ID),]
watbod_xrf[, correction_factor := (100 + `Moist 105C (%)`)/100]
# watbod_xrf[!is.na(correction_factor),correction_factor:= mean(correction_factor, na.rm = TRUE)]
# correct xrf values to per kg dry at 40 degrees
xrf_cols <- colnames(watbod_xrf)[grepl('^C',colnames(watbod_xrf))]
# Filter xrf_cols to only numeric columns
watbod_xrf[, (xrf_cols) := lapply(.SD, function(x) x / correction_factor), .SDcols = xrf_cols]
#put all data frames into list
df_list <- list(watbod_coding,watbod_cohex,watbod_toc,watbod_liab,watbod_pal,watbod_icpcc,watbod_dacc,watbod_phcc,watbod_xrf)      
#remove duplicate columns
cols <- as.data.table(lapply(df_list, function(x) { colnames(x) }))
lapply(df_list, function(x) { x[,c("ID",'ID.x','ID.y','ID_NA','...1','...2','NA_NA','Gps','SeQ.'):= NULL]})
#merge all data frames together
oever_ac_25 <- Reduce(function(x, y) merge(x, y, by = 'GSL_ID', all=TRUE), df_list) 
oever_ac_25 <- oever_ac_25[!is.na(GSL_ID),]
# change colnames
colnames(oever_ac_25) <- gsub("^C \\(", "", colnames(oever_ac_25))
colnames(oever_ac_25) <- gsub(")_", "_xrf_", colnames(oever_ac_25))
colnames(oever_ac_25) <- gsub("\r\n", "_", colnames(oever_ac_25))
oever_ac_25[,jaar:= 2025]
## merge 2024 and 2025 data----------------------------------------------------
oever_ac <- rbind(oever_ac, oever_ac_25, fill = TRUE)
#calc ratios
oever_ac[, feP_CC_SB := (`Fe_CC_mg/kg`/ 5584.5)/(`P_CC_mg/kg`/ 3097.3762)]
oever_ac[, feP_XRF_SB := (`Fe2O3_xrf_g/kg`/ 55.845)/(`P2O5_xrf_g/kg`/ 30.973762)]
oever_ac[, feS_CC_SB := (`Fe_CC_mg/kg`/ 5584.5)/(`S_CC_mg/kg`/ 3206.5)]
oever_ac[, feS_XRF_SB := (`Fe2O3_xrf_g/kg`/ 55.845)/(`SO3_xrf_g/kg`/ 80.063)]
#calculate p-org
oever_ac[, `P_CC_org_mg/kg` := `P_CC_mg/kg` - `P-PO4_CC_mg/kg`]
oever_ac[, basen_bez := (`CA_CO_mmol+/kg` + `MG_CO_mmol+/kg` + `NA_CO_mmol+/kg` + `K_CO_mmol+/kg`) / `CEC_CO_mmol+/kg` * 100]
#calculate os from c
oever_ac[, OS_perc := 0.01 + 0.172 * `TOC [g/kg]`]
# reformat based on sample depth
oever_ac_25 <- oever_ac[monsterdiepte <= 25,]
colnames(oever_ac_25) <- paste0(colnames(oever_ac_25),'_OR_25')
oever_ac_50 <- oever_ac[monsterdiepte == 50,]
colnames(oever_ac_50) <- paste0(colnames(oever_ac_50),'_OR_50')

# 8. dikte veraarde laag -------------------------------------------------
veraardveen <- fread(paste0(workspace,"./Bodemanalyses/Dikteveraardeveenlagen.csv"), dec = '.', na.strings = c(-999,'NA',''), encoding = "Latin-1")
# 9. Beheergegevens ---------------------------------------------------
beheer <- fread(paste0(workspace2,"beheer_wp1_2024_2025.csv"), dec = '.', na.strings = c(-999,'NA',''), encoding = "Latin-1")
beheer2 <- fread(paste0(workspace2,"beheer_wp2_2024.csv"), dec = '.', na.strings = c(-999,'NA',''), encoding = "Latin-1")
beheer <- rbind(beheer, beheer2, fill= TRUE)
beheer[,Maaifrequentie_oever_per_jaar:= gsub('0,5','0.5', Maaifrequentie_oever_per_jaar)]
beheer[,Jaar := as.numeric(Jaar)]
beheer[Jaar == 2024,SlootID := gsub('GM_5_R_W','GM_5a_R_W', SlootID)]
beheer[Jaar == 2024,SlootID := gsub('GZ_2_R_Z','GZ_2a_R_Z', SlootID)]
beheer[Jaar == 2025,SlootID := gsub('AD_3_WP1_Z','AD_3_WP1_N', SlootID)]
beheer[Jaar == 2024,SlootID := gsub('AD_3_WP1_Z','ZV_3_WP1_N', SlootID)]
beheer[Jaar == 2024,SlootID := gsub('AD_4_WP1_Z','ZV_4_WP1_Z', SlootID)]
beheer[Jaar == 2024,SlootID := gsub('AD_5_WP1_N','ZV_5_WP1_N', SlootID)]
beheer[,c('SlootID_kort','Gebied') := NULL]
checkdata <- unique(beheer[is.na(Maaifrequentie_oever_per_jaar),c('SlootID','Jaar')])
unique(beheer$Maaifrequentie_oever_per_jaar)

# 9. Save workspace ------------------------------------------------------
save.image(file = paste0(workspace,"/Processed_data_workspace.RData"))
