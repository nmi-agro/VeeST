# 1. Load packages -----------------------------------------------------------
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
library(ggrepel)
library(patchwork)
library(stringr)
library(ggcorrplot)

# 2. Settings and functions ----------------------------------------------------------------
workspace <- paste0(Sys.getenv("NMI-SITE"), 'O 1900 - O 2000/1922.N.23 VeeST vwsloot vd toekomst/05. Data/')
## Load custom functions-----------------------------------------------------
source(paste0("scripts/functions/functions_veest.R"))
sys.load.image(paste0(workspace,"/Processed_data_workspace.RData"), quiet = FALSE)


# 3. Create database/ merge gegevens -------------------------------------------------------
setDT(locaties)
locaties[, jaar := as.integer(jaar)]
locaties[, c('instanceID_abio', 'instanceID_veg') := NULL]
## aggregated abio data ---------------
abio_proj <- abio_hier
# check welke locaties missen in data abiotiek
check_db <- locaties[!SlootID %in% unique(abio_proj$SlootID),]
## slootprofielen ---------------
# hier geen merge op jaar ivm ontbrekende jaren in profielen (WP2 gebruikt profielen uit 2024)
abio_proj <- merge(abio_proj, locs_prof[,-c('SlootID-kort','gebied','sloot','Sloot_nr','Gebiedsnaam','Behandeling','oever','instanceID_abio','instanceID_veg','datum','WP')], by = c('SlootID','jaar'), all.x = T, suffixes = c('','_prof'))
# 34 locaties missen in profielen en abiotiek omdat pre-nul en demmerik en Mijnden
check_db <- locaties[!SlootID %in% unique(locs_prof$SlootID),] # 349 unieke slootIDs in locaties en niet in data (pre-nul en demmerik)
## penetrometer data ---------------
#merge db en penetrometer
penmerge_wide[, jaar := as.integer(jaar)]
abio_proj <- merge(abio_proj, penmerge_wide, by = c('SlootID','jaar'), all.x = T, suffixes = c('','_pen'))
# 145 locaties missen in penetrometerdata omdat 2025 ontbreekt
check_db <- locaties[!SlootID %in% unique(penmerge_wide$SlootID),]
## vegetatie --------------
# slootid jaar niet uniek?
veg[, jaar := as.integer(jaar)]
abio_proj <- merge(abio_proj, veg, by.x = c('instanceID_veg'), by.y = c('instanceID'), all.x = T, suffixes = c('','_veg'))
dup_cols <- names(abio_proj)[duplicated(names(abio_proj))]
if (length(dup_cols) > 0) abio_proj[, (dup_cols) := NULL]
check_db <- locaties[!SlootID %in% unique(veg$SlootID),]
## vegetatie aantal soorten------------------
veg_nsoorten[, jaar := as.integer(jaar)]
abio_proj <- merge(abio_proj, veg_nsoorten, by = c('SlootID','jaar'), all.x = T, suffixes = c('','_vegsrt'))
## clusters en locatiedata ---------------
#!!! check slootID jaar combinatie uniek (is nu niet het geval in clusters_locs)
abio_proj <- merge(abio_proj, clusters_locs[,-c('geom')], by = c('SlootID','jaar'), all.x = T, suffixes = c('','_clust'))
abio_proj <- abio_proj[!is.na(SlootID),]
check_db <- locaties[!SlootID %in% unique(clusters_locs$SlootID),]
## waterbodemdata--------------- 
# hier geen jaar aan toegevoegd ivm ontbrekende jaren in waterbodem en oeverdata
watbod[, jaar := as.integer(jaar)]
abio_proj <- merge(abio_proj, watbod[,-c('SlootID_kort','Slibmonster_Bware','sloot','Gebied')], by = c('SlootID','jaar'), all.x = T, suffixes = c('','_watbod'))
# abio_proj <- merge(abio_proj, watbod_ac, by.x = c('Slibmonster_Bware'), by.y = c('Customer_ID_SB'), all.x = T)
abio_proj <- merge(abio_proj, watbod_ac[,-c('SlootID_kort')], by.x = c('Slibmonster_Bware','jaar'), by.y = c('Customer_ID_SB','jaar'), all.x = T, suffixes = c('','_watbod_ac'))
check_db <- locaties[!SlootID %in% unique(watbod$SlootID),]
check_db <- locaties[!Slibmonster_Bware %in% unique(watbod_ac$Customer_ID_SB),]
## oeverdata---------------
# 120 unieke uniqueN(abio_proj$Oevermonster_AgroCares) uniqueN(oever_ac$SlootID_kort) 
oever_ac_25[, jaar := as.integer(jaar_OR_25)]
oever_ac_50[, jaar := as.integer(jaar_OR_50)]
abio_proj <- merge(abio_proj, oever_ac_25, by.x = c('Oevermonster_AgroCares','jaar'), by.y = c('SlootID_kort_OR_25','jaar'), all.x = T, suffixes = c('_SB','_OR'))
abio_proj <- merge(abio_proj, oever_ac_50, by.x = c('Oevermonster_AgroCares','jaar'), by.y = c('SlootID_kort_OR_50','jaar'), all.x = T, suffixes = c('_25','_50'))
check_db <- locaties[!Oevermonster_AgroCares %in% unique(oever_ac_25$SlootID_kort),]
## veraard veen ---------------
abio_proj <- merge(abio_proj, veraardveen[,-c('Gebied','Sloot')], by.x = c('SlootID_kort','jaar'), by.y = c('Slootcode','jaar'), suffixes = c('','_vaveen'), all.x = T)
check_db <- locaties[!SlootID_kort %in% unique(veraardveen$Slootcode),]
## beheer data ---------------
beheer[, jaar := as.integer(Jaar)]
# Handle non-numeric columns
beheer[,Maaifrequentie_oever_per_jaar := as.numeric(Maaifrequentie_oever_per_jaar)]
beheer[,Baggerfrequentie_per_jaar := as.numeric(Baggerfrequentie_per_jaar)]
beheer[,Aantal_koeien_vee_perceel_dag := as.numeric(Aantal_koeien_vee_perceel_dag)]
beheer[,Aantal_Koedagen_per_jaar := as.numeric(Aantal_Koedagen_per_jaar)]
abio_proj <- merge(abio_proj, beheer[,-c('gebied','sloot','Sloot_nr','Gebiedsnaam','Behandeling','oever','instanceID_abio','instanceID_veg','datum','WP')], by = c('SlootID','jaar'), all.x = T, suffixes = c('','_beheer'))
# wel in locaties maar niet in beheer
check_db <- locaties[!SlootID %in% unique(beheer$SlootID),]

## add grouping vars -------------------------------
abio_proj[text == "Hoogheemraadschap De Stichtse Rijnlanden",waterschap := 'HDSR']
abio_proj[text == "Hoogheemraadschap Hollands Noorderkwartier" ,waterschap := 'HHNK']
abio_proj[text == "Waterschap Amstel, Gooi en Vecht",waterschap := 'AGV']
abio_proj[text == "Hoogheemraadschap van Rijnland",waterschap := 'Rijnland']
abio_proj[text == "Wetterskip Fryslân",waterschap := 'Fryslân']
abio_proj[text == "Waterschap Drents Overijsselse Delta",waterschap := 'WDOD']
abio_proj[text == "Hoogheemraadschap van Schieland en de Krimpenerwaard",waterschap := 'HHSK']
# veentype
abio_proj[grepl('b$',BODEMCODE), veentype:= 'broekveen']
abio_proj[grepl('k$',BODEMCODE), veentype:= 'kleiig veen']
abio_proj[grepl('s$',BODEMCODE), veentype:= 'veenmosveen']
abio_proj[grepl('c$',BODEMCODE), veentype:= 'zeggeveen_rietzeggeveen_broekveen']
abio_proj[grepl('r$',BODEMCODE), veentype:= 'zeggerietveen_rietveen']
abio_proj[grepl('d$',BODEMCODE), veentype:= 'bagger_verslagenveen_gyttja_anders']
abio_proj[is.na(veentype), veentype:= 'bagger_verslagenveen_gyttja_anders']
# factor op volgorde trofie/ doorlatendheid
abio_proj[, veentype := factor(veentype, levels = c('kleiig veen','veenmosveen','zeggerietveen_rietveen','zeggeveen_rietzeggeveen_broekveen','broekveen','bagger_verslagenveen_gyttja_anders'))]
abio_proj[,veentype_num:= as.numeric(factor(veentype))]  # Converteer veentype naar numeriek voor correlatieberekening
# beheer
abio_proj[,beheer := 'regulier']
abio_proj[grepl('M', Behandeling),beheer := 'minimaal']
abio_proj[grepl('M-AF', Behandeling),beheer := 'minimaal + afrastering']
abio_proj[grepl('R-AF', Behandeling),beheer := 'regulier + afrastering']
abio_proj[grepl('AF', Behandeling),beheer := 'afrastering']
abio_proj[grepl('NVO', Behandeling), beheer := 'NVO']
 ## adjust colnames----------------------------------
#  abio_proj[,Sloot_nr := Sloot_nr.x]


## adjust penetrometer data for analysis -------------------
penmerge1<-penmerge
penmerge[,Diept := as.numeric(Diept)]
penmerge[,dieptebin := cut(Diept, breaks = seq(from = 0, to = 80, by = 5), include.lowest = TRUE), by= .(SlootID, jaar)]
penmerge[,sectie_f := factor(sectie, levels=c('oever','insteek','perceel')), by= .(SlootID, jaar)]
veentype_unique <- abio_proj[, .SD[1], by = SlootID, .SDcols = c('veentype')]
penmerge <- merge(penmerge, veentype_unique, by = "SlootID", all.x = TRUE) #Bereken gemiddelde drooglegging per gebied voor de bars (hergebruik bestaande code)
penmerge[,jaar := as.integer(jaar)]
loc_pen <- unique(locaties[, c('SlootID','Gebiedsnaam', 'WP','jaar','Behandeling')])
loc_pen <- loc_pen[WP %in% c('WP1','WP2'),]
penmerge <- merge(penmerge, loc_pen, by = c('SlootID','jaar'), all.x = TRUE)
## adjust db -------------------
abio_proj[max_wtd>doorzicht2_mid_cm, doorzicht2_mid_cm := max_wtd]
# Corrigeren voor pH effect op redox (Nernst vergelijking)
# Redox daalt ~59 mV per pH eenheid stijging bij 25°C
abio_proj[, slib_redox_pH7 := slib_redox_mgL + (7 - slib_pH) * 59]
abio_proj[, water_redox_pH7 := water_redox + (7 - slib_pH) * 59]
abio_proj[water_redox_pH7 > 800, water_redox_pH7 := water_redox_pH7/10] # correctie foutieve waarden redox
# Gemiddelde draagkracht oever berekenen obv oever penetrometer metingen
abio_proj[, draagkracht_oever := rowMeans(.SD, na.rm = TRUE), 
          .SDcols = c("oever_(10,20]", "oever_(20,30]", "oever_(30,40]", "oever_(40,50]")]
# Gemiddelde draagkracht perceel berekenen obv perceel penetrometer metingen
abio_proj[, draagkracht_perceel := rowMeans(.SD, na.rm = TRUE), 
          .SDcols = c("perceel_(10,20]", "perceel_(20,30]", "perceel_(30,40]", "perceel_(40,50]")]
abio_proj[, slibdiepte := max_slib + max_wtd]
abio_proj[, doorzicht2_mid_m :=  doorzicht2_mid_cm/100]
abio_proj[, zichtdiepte :=   doorzicht2_mid_m/max_wtd]
# Taludhoeken omrekenen van percentage naar graden
perc_to_graden <- function(perc) {
  return(atan(perc / 100) * 180 / pi)
}
abio_proj[, tldk_oevrwtr_graden := perc_to_graden(tldk_oevrwtr_perc)]
abio_proj[, tldk_wtrwtr_graden := perc_to_graden(tldk_wtrwtr_perc)]
abio_proj[, tldk_vastbodem_graden := perc_to_graden(tldk_vastbodem_perc)]
# Dikte veraarde laag naar numeric en negatieve waarden naar NA
abio_proj[`Dikte veraarde laag (cm)` == '>100', `Dikte veraarde laag (cm)` := '100']
abio_proj[`Dikte veraarde laag (cm)` == '>120', `Dikte veraarde laag (cm)` := '120']
# Add additional cleaning for other potential non-numeric values
abio_proj[`Dikte veraarde laag (cm)` %in% c('', 'NA', 'n.v.t.', '-'), `Dikte veraarde laag (cm)` := NA]
# Ensure proper numeric conversion
abio_proj[, dkvalg := as.numeric(`Dikte veraarde laag (cm)`)]
# Set negative values to NA
abio_proj[dkvalg < 0, dkvalg := NA]
# correctie outliers breedte vegetatie
abio_proj[oeverzone_2b_breedte_cm > 200, oeverzone_2b_breedte_cm := oeverzone_2b_breedte_cm/10]
abio_proj[oevbte > 6, oevbte := oevbte/10]
# correctie outliers onderholling
abio_proj[holleoever > 150, holleoever := holleoever/10]
#remove foute waarde O2
abio_proj[water_O2_mgL > 100, water_O2_mgL := water_O2_mgL/100]
abio_proj[water_O2_mgL > 20, water_O2_mgL := water_O2_mgL/10]

## omrekenen eenheden ijzer, P, S naar mg/l----------------------------------------------
# Bereken moleculair gewichten (g/mol)
MW_Fe <- 55.845   # IJzer
MW_P <- 30.974    # Fosfor
MW_S <- 32.065    # Zwavel
# Omrekening van µmol/l naar mg/l voor Fe, P en S in poriewater
abio_proj[, `:=`(
  # Fe concentratie omrekening
  Fe_mg_l_PW = `Fe_µmol/l_PW` * MW_Fe / 1000,  # µmol/l naar mg/l
  # P concentratie omrekening  
  P_mg_l_PW = `P_µmol/l_PW` * MW_P / 1000,     # µmol/l naar mg/l
  P_mg_l_OW = `P_µmol/l_OW` * MW_P / 1000,     # µmol/l naar mg/l
  N_mg_l_OW = `TN_µmol/l_OW` * 14.007 / 1000,    # µmol/l naar mg/l
  # S concentratie omrekening
  S_mg_l_PW = `S_µmol/l_PW` * MW_S / 1000      # µmol/l naar mg/l
)]
abio_proj[, Cl_mg_l_OW := `Cl_µmol/l_OW` * 35.45 / 1000]
abio_proj[, Cl_mg_l_PW := `Cl_µmol/l_PW` * 35.45 / 1000]
## reformat data for plot loop-------------------------
cols_num <- colnames(abio_proj)[sapply(abio_proj, is.numeric)]

melt <- melt(setDT(abio_proj), id.vars = c("SlootID","Sloot_nr","WP","instanceID_abio","instanceID_veg","Gebiedsnaam","MeenemenDataAnalyse_totaal","gebied","sloot","Behandeling","beheer","jaar"), 
             measure.vars = cols_num, na.rm = TRUE)
pars <- as.data.table(unique(melt[, variable]))
pars <- fread(paste0(workspace,"./hulp_tabellen/parametersVeest_namen.csv"), dec = '.', na.strings = c('NA',''), encoding = "Latin-1")
melt[,variable :=tolower(variable)]
pars[,variable :=tolower(variable)]
melt <- merge(melt, pars, by = 'variable', all.x = TRUE)
melt[variable == 'doorzicht2_mid_cm', value :=  value/100]
check <- unique(melt[,c('variable','monsterdiepte','parameter','compartiment','eenheid','methode','varnames')])
melt <- melt[!is.na(melt$parameter),]
melt <- melt[!methode =='liab',]
setDT(melt)
melt[is.na(eenheid), eenheid := ""]
melt[,par_eenheid := paste0(parameter,"_", eenheid,"_", methode)]
melt[,compartiment_short := tolower(compartiment)]
melt[compartiment == 'OR', compartiment := 'oever']
melt[compartiment == 'SB', compartiment := 'slib']
melt[compartiment == 'OW', compartiment := 'water']
melt[compartiment == 'PW', compartiment := 'poriewater']
melt[,`gemiddelde VeeST` := mean(value, na.rm = TRUE), by = c('variable','monsterdiepte','parameter','compartiment','eenheid','methode','varnames')] 

## overzichtstabel met pargroups per gebied per jaar ---------------------------

overzicht_wide <- dcast(
  melt,
  Gebiedsnaam+jaar+WP ~ vargroup,
  value.var = "SlootID",
  ,
  fun.aggregate = uniqueN
)
write.table(overzicht_wide, file = paste(workspace2,"dataOverzicht/Overzichtstabel_pargroups_per_gebied_jaar",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), na = "", sep =';', dec = '.',row.names = FALSE)

overzicht_wide <- dcast(
  melt,
  SlootID+jaar+WP ~ .,
  value.var = c("instanceID_abio","instanceID_veg"),
  fun.aggregate = uniqueN
)
write.table(overzicht_wide, file = paste(workspace2,"dataOverzicht/Overzichtstabel_pargroups_per_SlootID_jaar",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), na = "", sep =';', dec = '.',row.names = FALSE)


# validate complete db-------------------------------------------------------------------
uniqueN(locaties$SlootID[locaties$`Complete data` == 1]) #238
uniqueN(abio_proj[!is.na(slib_pH)&!is.na(water_pH)&!is.na(max_slib)&!is.na(`insteek_[0,10]`)&!is.na(instanceID_abio),c('SlootID')])
# check if instanceID abiotiek voorkomt in abio
check_db <- abio_proj[!instanceID_abio %in% unique(abio$instanceID), c('SlootID','instanceID_abio')]
# Controleer per bestand of ZW_1_WP1_Z vaker voorkomt
SlootID_test <- "SW_2_M_O"
locaties[SlootID == SlootID_test, .N]
locs_prof[SlootID == SlootID_test, .N]
penmerge_wide[SlootID == SlootID_test, .N]
clusters_locs[SlootID == SlootID_test, .N]
veg[SlootID == SlootID_test, .N]
veg_nsoorten[SlootID == SlootID_test, .N]
beheer[SlootID == SlootID_test, .N]
watbod[SlootID == SlootID_test, .N]
watbod_ac[Customer_ID_SB == SlootID_test, .N]
oever_ac_25[SlootID_kort_OR_25 == SlootID_test, .N]
oever_ac_50[SlootID_kort_OR_50 == SlootID_test, .N]
veraardveen[Slootcode == SlootID_test, .N]
# check if loc info is filled in for all rows in abio_proj
melt[is.na(WP) | WP == "", .N, by = .(Gebiedsnaam, jaar)]
abio_proj[is.na(WP) | WP == "", .N, by = .(Gebiedsnaam, jaar)]

# Visualisaties ---------------------------------------------------------
## figs tussenrapportage plots -------------------
### Kleuren voor veentypen (Okabe-Ito palette)-------------------------------------------
okabe_ito_colors <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999",
  "#661100", "#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677",
  "#882255", "#AA4499", "#DDDDDD", "#000000", "#AA4466", "#4477AA", "#66CCEE", "#228833",
  "#CCBB44", "#EE6677", "#AA3377", "#BBBBBB", "#EE3377", "#11AA99", "#3366CC", "#CCAA44",
  "#EE7733", "#5588FF", "#99DDFF", "#44BB66", "#228844", "#AAAA00", "#FF5544", "#CC3399"
)
veentype_colors <- c(
  "kleiig veen" = okabe_ito_colors[1],                      # oranje
  "veenmosveen" = okabe_ito_colors[2],                     # lichtblauw 
  "zeggerietveen_rietveen" = okabe_ito_colors[3],          # groen
  "zeggeveen_rietzeggeveen_broekveen" = okabe_ito_colors[3], # groen (zelfde als zeggerietveen)
  "broekveen" = okabe_ito_colors[6],                       # oranje
  "bagger_verslagenveen_gyttja_anders" = okabe_ito_colors[8] # grijs
)

### Filter data 4 analyse omstandigheden, beheer en vegetatie-------------
melt <- melt[WP %in% c('WP1','WP2'),]
abio_proj <- abio_proj[WP %in% c('WP1','WP2'),]

### Filter data voor ronde hoep----------------------------------------------------
melt <- melt[Gebiedsnaam == "Ronde Hoep",]
abio_proj <- abio_proj[Gebiedsnaam == "Ronde Hoep",]
# add clusters
abio_proj[, sloot_cluster := fcase(
  Sloot_nr == 1,        "1",
  Sloot_nr %in% c(2,3), "2",
  Sloot_nr %in% c(4,5), "3",
  Sloot_nr %in% c(7,8), "4",
  Sloot_nr %in% c(9,11),"5",
  Sloot_nr == 10,       "6",
  Sloot_nr %in% c(14,13,12,9.1), "reservaat"
)]
melt[, sloot_cluster := fcase(
  sloot == 1,        "1",
  sloot %in% c(2,3), "2",
  sloot %in% c(4,5), "3",
  sloot %in% c(7,8), "4",
  sloot %in% c(9,11),"5",
  sloot == 10,       "6",
  sloot %in% c(14,13,12,9.1), "reservaat"
)]
penmerge[, sloot_cluster := fcase(
  sloot == 1,        "1",
  sloot %in% c(2,3), "2",
  sloot %in% c(4,5), "3",
  sloot %in% c(7,8), "4",
  sloot %in% c(9,11),"5",
  sloot == 10,       "6",
  sloot %in% c(14,13,12,9.1), "reservaat"
)]
### zet volgorde slootID----------------------------------------------------------------
library(gtools)
# Natural sort van SlootID in abio_proj
abio_proj[, SlootID := factor(SlootID, levels = mixedsort(unique(SlootID)))]
# Natural sort van SlootID in melt
melt[, SlootID := factor(SlootID, levels = mixedsort(unique(SlootID)))]

### Correlatie matrix abiotische parameters----------------
# Select the variables for correlation analysis
cols_corr <- c("drglg", "max_wtd", "doorzicht2_mid_cm", "max_slib", "watbte",
               "oeverzone_2a_breedte_cm", "oeverzone_2b_breedte_cm", 
               "holleoever", "tldk_wtrwtr_perc", "tldk_oevrwtr_perc",
              "Baggerfrequentie_per_jaar","Maaifrequentie_oever_per_jaar","Aantal_Koedagen_per_jaar","Aantal_koeien_vee_perceel_dag")  # voorbeeldparameters, pas aan op basis van beschikbaarheid
nederlandse_namen <- c(
  "Drooglegging (m)",
  "Maximale waterdiepte (m)", 
  "Doorzicht (cm)",
  "Maximale slibdikte (m)",
  "Waterbreedte (m)",
  "Breedte oeverzone 2a (cm)",
  "Breedte oeverzone 2b (cm)",
  "Onderholling (cm)",
  "Taludhoek waterlijn (%)",
  "Taludhoek oever (%)",
  "Baggerfrequentie per jaar",
  "Maaifrequentie oever per jaar",
  "Aantal koedagen per jaar",
  "Aantal koeien per perceel per dag"
)
# toevoeging oeverbreedte en veentype
cols_corr <- c("drglg", "watbte","oevbte",
               "oeverzone_2a_breedte_cm", "oeverzone_2b_breedte_cm", 
               "holleoever", "veentype_num", "Z_CLAY_SA_OR_50","draagkracht_oever","draagkracht_perceel",
               "Baggerfrequentie_per_jaar","Maaifrequentie_oever_per_jaar","Aantal_Koedagen_per_jaar","Aantal_koeien_vee_perceel_dag")  # voorbeeldparameters, pas aan op basis van beschikbaarheid)  # nieuwe parameters toegevoegd
nederlandse_namen <- c(
  "Drooglegging (m)",
  "Waterbreedte (m)",
  "Oeverbreedte (m)",
  "Breedte oevervegetatiezone 2a (cm)",
  "Breedte oevervegetatiezone 2b (cm)",
  "Onderholling (cm)",
  "Veentype",
  "Kleigehalte veen (%)",
  "Draagkracht oever (mPa)",
  "Draagkracht perceel (mPa)",
  "Baggerfrequentie per jaar",
  "Maaifrequentie oever per jaar",
  "Aantal koedagen per jaar",
  "Aantal koeien per perceel per dag"
)
# alle xgboost parameters
cols_corr <- c("waterzone_1_subm_tot_perc","2","draagkracht_oever", 
                 "slib_redox_pH7","max_slib",
                 "drglg", "max_wtd", "zichtdiepte", "max_slib", "watbte","oeverzone_2b_breedte_cm", "oeverzone_2b_kaal_perc",
                 "holleoever", "tldk_wtrwtr_perc", "tldk_oevrwtr_perc", "slib_redox_pH7","slib_pH",
                 "oevbte", "Z_CLAY_SA_OR_50",
                 "draagkracht_oever", "draagkracht_perceel", "water_pH", "NH4_µmol/l_PW","P-AL mg p2o5/100g_SB",
                "Baggerfrequentie_per_jaar","Maaifrequentie_oever_per_jaar","Aantal_Koedagen_per_jaar","Aantal_koeien_vee_perceel_dag")
# Create readable Dutch names mapping
nederlandse_namen <- c(
  "waterzone_1_subm_tot_perc" = "Bedekking submerse vegetatie in water (%)",
  "2" = "Aantal oeversoorten",
  "draagkracht_oever" = "Draagkracht oever (MPa)",
  "slib_redox_pH7" = "Redox slib bij pH7 (mV)",
  "max_slib" = "Slibdikte (m)",
  "drglg" = "Drooglegging (m)",
  "max_wtd" = "Maximale waterdiepte (m)", 
  "zichtdiepte" = "Doorzicht/waterdiepte",
  "max_slib" = "Maximale slibdikte (m)",
  "watbte" = "Waterbreedte (m)",
  "oeverzone_2b_breedte_cm" = "Breedte oevervegetatiezone 2b (cm)",
  "oeverzone_2b_kaal_perc" = "Bedekking kale oever zone 2b (%)",
  "holleoever" = "Onderholling (cm)",
  "tldk_wtrwtr_perc" = "Taludhoek waterlijn (%)",
  "tldk_oevrwtr_perc" = "Taludhoek oever (%)",
  "slib_redox_pH7" = "Redox slib bij pH7 (mV)",
  "slib_pH" = "pH slib",
  "oevbte" = "Oeverbreedte (m)",
  "veentype_num" = "Veentype (numeriek)",
  "Z_CLAY_SA_OR_50" = "Kleigehalte 50cm (%)",
  "draagkracht_oever" = "Draagkracht oever (MPa)",
  "draagkracht_perceel" = "Draagkracht perceel (MPa)",
  "water_pH" = "Water pH",
  "NH4_µmol/l_PW" = "Ammonium (µmol/l)",
  "P-AL mg p2o5/100g_SB" = "P-AL slib (mg P2O5/100g)",
   "Baggerfrequentie_per_jaar" = "Baggerfrequentie per jaar",
   "Maaifrequentie_oever_per_jaar"= "Maaifrequentie oever per jaar",
   "Aantal_Koedagen_per_jaar"= "Aantal koedagen per jaar",
   "Aantal_koeien_vee_perceel_dag"= "Aantal koeien per perceel per dag")  

# Check which variables actually exist in the dataset
available_cols <- cols_corr[cols_corr %in% colnames(abio_proj)]
print("Available variables:")
print(available_cols)

# Create correlation matrix and p-value matrix with the SAME variables
abio_proj[,trofie := as.numeric(trofie)]
abio_proj[,draagkracht_perceel := as.numeric(draagkracht_perceel)]

# Handle non-numeric columns
abio_proj[Maaifrequentie_oever_per_jaar %in% c('0,5'), Maaifrequentie_oever_per_jaar := 0.5]
abio_proj[is.na(Maaifrequentie_oever_per_jaar), Maaifrequentie_oever_per_jaar := 0]
abio_proj[,Maaifrequentie_oever_per_jaar := as.numeric(Maaifrequentie_oever_per_jaar)]
abio_proj[,Baggerfrequentie_per_jaar := as.numeric(Baggerfrequentie_per_jaar)]
abio_proj[is.na(Baggerfrequentie_per_jaar), Baggerfrequentie_per_jaar := 0]
abio_proj[, Aantal_koeien_vee_perceel_dag := as.numeric(Aantal_koeien_vee_perceel_dag)]

# Create correlation matrix with available columns only
cormatrix <- abio_proj[, available_cols, with = FALSE]
cormatrix <- na.omit(cormatrix)
# Zorg dat alle kolommen numeric zijn
cormatrix <- cormatrix[, lapply(.SD, as.numeric)]
is_bad <- function(x) all(is.na(x)) || all(is.infinite(x)) || sd(x, na.rm = TRUE) == 0
good_cols <- !apply(cormatrix, 2, is_bad)
cormatrix_clean <- cormatrix[, good_cols, with = FALSE]

M <- cor(cormatrix_clean, use = "complete.obs")
rownames(M) <- colnames(cormatrix_clean)
colnames(M) <- colnames(cormatrix_clean)
# Create both matrices with the same data
M <- cor(cormatrix)
p.mat <- cor_pmat(cormatrix, use = "complete.obs")

# Apply Dutch names to the same matrix
rownames(M) <- colnames(cormatrix)
colnames(M) <- colnames(cormatrix)
# Nu kunt u de namen vervangen
rownames(M) <- nederlandse_namen[rownames(M)]
colnames(M) <- nederlandse_namen[colnames(M)]

# Now use M (not M_clean) with matching p.mat
ggcorrplot(M, 
           type = "lower",
           hc.order = TRUE,
           outline.color = "white",
           colors = c("#D55E00", "#FFFFFF", "#0072B2"),
           lab = TRUE,
           lab_size = 4,
           digits = 1,
           tl.cex = 12,
           title = "Correlatiematrix: Slootprofiel en vegetatiezones",
           p.mat = p.mat,
           insig = "blank",
          show.diag = TRUE,
          legend.title = "") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12)
  )+
  labs(legend.title ='')


### Slootprofiel (drooglegging, waterdiepte midden sloot en langs de oever, slibdikte, doorzicht, breedte zones, talud oevers, onderholling)--------------------------
##### Plot relatie breedtes vegetatie en profiel ---------------------------
# geom_jitter(data = abio_proj, aes(x = watbte, max_wtd, drglg, oeverzone_2a_breedte_cm)) 
# NIEUWE OMREKENING NAAR METERS
abio_proj[, waterzone_1_breedte_m := waterzone_1_breedte_cm / 100]
abio_proj[, oeverzone_2a_breedte_m := oeverzone_2a_breedte_cm / 100]
abio_proj[, waterzone_1_2a_breedte_m := waterzone_1_breedte_m + oeverzone_2a_breedte_m]
abio_proj[, oeverzone_2b_breedte_m := oeverzone_2b_breedte_cm / 100]

# R² berekenen voor waterzone vs waterbreedte (in meters)
r2_waterzone_watbte <- get_r_squared(abio_proj[!is.na(waterzone_1_2a_breedte_m) & !is.na(watbte)], 
                                     "watbte", "waterzone_1_2a_breedte_m")

p1 <- ggplot()+
  geom_jitter(data=abio_proj[!is.na(oeverzone_2a_breedte_m) & !is.na(watbte),],
              aes(y=waterzone_1_2a_breedte_m, x=watbte), alpha=0.3, size=2)+
  # geom_smooth(data=abio_proj[!is.na(oeverzone_2a_breedte_m) & !is.na(watbte),],
  #             aes(y=waterzone_1_2a_breedte_m, x=watbte), method='lm', color='#1B9E77', size=0.9)+
  geom_abline(slope=1, intercept=0, linetype='dashed', color='black', size=1.1)+
  # R² annotatie
  annotate("text", x=Inf, y=Inf, 
           label=paste0("R² = ", round(r2_waterzone_watbte, 3)), 
           hjust=1.1, vjust=1.5, size=4, fontface="bold") +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.ticks = element_line(colour = "black"),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    plot.background = element_blank()
  ) +
  ggtitle('Relatie waterzone 1 en 2a en waterbreedte') +
  labs(y = 'breedte waterzone 1 en oever 2a', x = 'waterbreedte (m)')
# R² berekenen voor oeverzone vs oeverbreedte (in meters)  
r2_oeverzone_oevbte <- get_r_squared(abio_proj[!is.na(oeverzone_2b_breedte_m) & !is.na(oevbte)], 
                                     "oevbte", "oeverzone_2b_breedte_m")
p2 <- ggplot()+
  geom_jitter(data=abio_proj[!is.na(oeverzone_2b_breedte_cm) & !is.na(oevbte),],
              aes(y=oeverzone_2b_breedte_m, x=oevbte), alpha=0.3, size=2)+
  # geom_smooth(data=abio_proj[!is.na(oeverzone_2b_breedte_cm) & !is.na(oevbte),],
  #             aes(y=oeverzone_2b_breedte_m, x=oevbte), method='lm', color='#1B9E77', size=0.9)+
  geom_abline(slope=1, intercept=0, linetype='dashed', color='black', size=1.1)+ # R² annotatie
  annotate("text", x=Inf, y=Inf, 
           label=paste0("R² = ", round(r2_oeverzone_oevbte, 3)), 
           hjust=1.1, vjust=1.5, size=4, fontface="bold") +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.ticks = element_line(colour = "black"),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    plot.background = element_blank()
  ) +
  ggtitle('Relatie oeverzone 2b en oeverbreedte') +
  labs(y = 'breedte oeverzone 2b', x = 'breedte insteek tot waterlijn (m)')
# Combineer alle drie plots naast elkaar met gedeelde legenda
combined_plot <- p1 + p2 + 
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(
    title = "Relaties tussen breedte vegetatiezones en water- en oeverbreedte",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
    )
  ) &
  theme(legend.position = "bottom")

# Toon de gecombineerde plot
print(combined_plot)
ggsave(file=paste0('output/AlleGebieden/Tussenrapportage/breedteVegSloot_relatie.png'), width = 25,height = 15,units='cm',dpi=800)


##### plot met slib, waterdiepte en doorzicht ----------------
abio_proj[,slibdiepte := max_slib + max_wtd]
abio_proj[,doorzicht2_mid_m :=  doorzicht2_mid_cm/100]
# Calculate median values for the three variables
median_slibdiepte <- median(abio_proj$slibdiepte, na.rm = TRUE)
median_max_wtd <- median(abio_proj$max_wtd, na.rm = TRUE)  
median_doorzicht <- median(abio_proj$doorzicht2_mid_m, na.rm = TRUE)
median_slib <- median(abio_proj$max_slib, na.rm = TRUE)
abio_proj_view <- abio_proj[, c('SlootID','slibdiepte','max_slib','max_wtd','doorzicht2_mid_m','waterzone_1_subm_tot_perc')]
abio_proj_cast <- dcast(abio_proj, Gebiedsnaam ~ ., value.var = c('slibdiepte','max_slib','max_wtd','doorzicht2_mid_m','waterzone_1_subm_tot_perc'), fun.aggregate = mean, na.rm=TRUE)
abio_proj_cast[,zichtdiepte:= doorzicht2_mid_m/max_wtd]
pairs(abio_proj_cast[,c('slibdiepte','max_wtd','doorzicht2_mid_m','zichtdiepte')])
 
# Sorteer de data op slibdiepte en zet Gebiedsnaam om naar een factor met de juiste volgorde
plot_data <- abio_proj_cast[!is.na(Gebiedsnaam), ]
plot_data <- plot_data[order(slibdiepte)]
plot_data[, Gebiedsnaam := factor(Gebiedsnaam, levels = unique(Gebiedsnaam))]
 
ggplot(data = plot_data) +
    geom_col(aes(x= Gebiedsnaam, y = -1*slibdiepte, fill = 'slibdikte (m)'),alpha = 0.7) +
    geom_col(aes(x= Gebiedsnaam, y = -1*max_wtd, fill = 'maximale waterdiepte (m)'),alpha = 0.8) +
    geom_col(aes(x= Gebiedsnaam, y = -1*doorzicht2_mid_m, fill = 'doorzicht (m)'),alpha = 0.8) +
    # Add median lines
    geom_hline(yintercept = -1*median_slibdiepte, color = "brown", linetype = "dashed", size = 1) +
    geom_hline(yintercept = -1*median_max_wtd, color = "skyblue", linetype = "dashed", size = 1) +
    geom_hline(yintercept = -1*median_doorzicht, color = "darkblue", linetype = "dashed", size = 1) +
    scale_fill_manual(values = c("darkblue","skyblue","brown"), na.value = "#A6761D")+
    # facet_grid(.~ jaar, space = 'free_x', scales = 'free_x', switch = 'x')+
    theme_minimal(base_size = 15)+
    theme(
      strip.background = element_blank(),
      strip.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 15, vjust = 0.8, hjust =1, angle = 90),
      axis.text.y = element_text(size = 15),
      axis.title = element_text(size= 15),
      axis.ticks =  element_line(colour = "black"),
      axis.line = element_line(colour='black'),
      plot.title = element_text(size =18, face="bold", hjust = 0.5),
      panel.background = element_blank(),
      panel.border = element_rect(colour='black', fill = NA),
      plot.background = element_blank(),
      legend.position = "bottom",
      legend.box.just = "center"
    )+
    guides(fill = guide_legend(title = '', title.vjust = 1))+
    guides(color = guide_legend(title = ''))+
    ggtitle(paste0("Doorzicht, waterdiepte & slibdikte")) +
    labs(x= 'Gebied' , y= 'meter')

ggsave(file=paste0('output/AlleGebieden/Tussenrapportage/watdte_zicht_slib.png'), width = 25,height = 15,units='cm',dpi=800)

# voor één gebied
abio_proj_cast <- dcast(abio_proj, SlootID+sloot_cluster+jaar ~ ., value.var = c('slibdiepte','max_wtd','doorzicht2_mid_m','waterzone_1_subm_tot_perc'), fun.aggregate = mean, na.rm=TRUE)
abio_proj_cast[,zichtdiepte:= doorzicht2_mid_m/max_wtd]
ggplot(data = abio_proj_cast[!is.na(max_wtd),]) +
    geom_col(aes(x= SlootID, y = -1*slibdiepte, fill = 'slibdikte (m)'),alpha = 0.7) +
    geom_col(aes(x= SlootID, y = -1*max_wtd, fill = 'maximale waterdiepte (m)'),alpha = 0.8) +
    geom_col(aes(x= SlootID, y = -1*doorzicht2_mid_m, fill = 'doorzicht (m)'),alpha = 0.8) +
    # Add median lines
    geom_hline(yintercept = -1*median_slibdiepte, color = "brown", linetype = "dashed", size = 1) +
    geom_hline(yintercept = -1*median_max_wtd, color = "skyblue", linetype = "dashed", size = 1) +
    geom_hline(yintercept = -1*median_doorzicht, color = "darkblue", linetype = "dashed", size = 1) +
    scale_fill_manual(values = c("darkblue","skyblue","brown"), na.value = "#A6761D")+
    facet_grid(jaar~sloot_cluster, space = 'free_x', scales = 'free_x', switch = 'x')+
    theme_minimal(base_size = 15)+
    theme(
      strip.background = element_blank(),
      strip.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 15, vjust = 0.8, hjust =1, angle = 90),
      axis.text.y = element_text(size = 15),
      axis.title = element_text(size= 15),
      axis.ticks =  element_line(colour = "black"),
      axis.line = element_line(colour='black'),
      plot.title = element_text(size =18, face="bold", hjust = 0.5),
      panel.background = element_blank(),
      panel.border = element_rect(colour='black', fill = NA),
      plot.background = element_blank(),
      legend.position = "bottom",
      legend.box.just = "center"
    )+
    guides(fill = guide_legend(title = '', title.vjust = 1))+
    guides(color = guide_legend(title = ''))+
    ggtitle(paste0("Doorzicht, waterdiepte & slibdikte")) +
    labs(x= 'slootID en cluster' , y= 'meter')

##### Plot slibdikte tegen drooglegging ---------------------------

# R² berekenen voor waterzone vs waterbreedte (in meters)
r2_waterzone_watbte <- get_r_squared(abio_proj[!is.na(max_slib) & !is.na(drglg)], 
                                     "drglg", "max_slib")

p1 <- ggplot()+
  geom_jitter(data=abio_proj[!is.na(max_slib) & !is.na(drglg),],
              aes(y=max_slib, x=drglg), alpha=0.3, size=2)+
  geom_smooth(data=abio_proj[!is.na(max_slib) & !is.na(drglg),],
              aes(y=max_slib, x=drglg), method='lm', color='#1B9E77', size=1.5)+
  # R² annotatie
  annotate("text", x=Inf, y=Inf, 
           label=paste0("R² = ", round(r2_waterzone_watbte, 3)), 
           hjust=1.1, vjust=1.5, size=4, fontface="bold") +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.ticks = element_line(colour = "black"),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    plot.background = element_blank()
  ) +
  ggtitle('Relatie slibdikte en drooglegging') +
  labs(y = 'slibdikte (m)', x = 'drooglegging (m)')

# Toon de gecombineerde plot
print(p1)
ggsave(file=paste0('output/AlleGebieden/Tussenrapportage/slibdrooglegging_relatie.png'), width = 25,height = 15,units='cm',dpi=800)


### Bodemopbouw (type veen, bodemstructuur en diepte veraarding)-------------------
#### Plot van dikte veraarde toplaag-------------------------------
# Bereken gemiddelde drooglegging per gebied voor de bars
drglg_bars <- abio_proj[!is.na(Gebiedsnaam) & !is.na(dkvalg), 
                        .(avg_drglg = mean(drglg, na.rm = TRUE)), 
                        by = Gebiedsnaam]
# Schaal de drooglegging voor betere visualisatie
scale_factor <- 100  # Aanpassen naar gewenste schaal
# Sorteer de data en maak een geordende factor van Gebiedsnaam
drglg_bars <- drglg_bars[order(avg_drglg)]
drglg_bars[, Gebiedsnaam := factor(Gebiedsnaam, levels = unique(Gebiedsnaam))]

ggplot() +
  # Achtergrond bars voor drooglegging
  geom_col(data = drglg_bars, 
           aes(x = Gebiedsnaam, y = avg_drglg * scale_factor), 
           fill = "lightblue", alpha = 0.4, width = 0.8) +
  # Hoofdplot - veraarde laag
  geom_boxplot(data = abio_proj[!is.na(Gebiedsnaam) & !is.na(dkvalg),], 
               aes(x = Gebiedsnaam, y = dkvalg, fill = veentype), 
               alpha = 0.8) +
  scale_fill_manual(values = okabe_ito_colors, 
    labels = c('kleiig veen','veenmosveen','zeggeveen','broekveen','overig'), 
    na.value = "grey50") +
  geom_hline(yintercept = median(abio_proj$dkvalg, na.rm = TRUE), color = "black", linetype = "dashed", size = 1) +
  scale_y_continuous(
    name = "Dikte veraarde laag (cm)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Gemiddelde drooglegging (m)")
  ) +
  labs(
    title = "Dikte Veraarde Toplaag met Drooglegging per Gebied",
    x = "Gebiedsnaam",
    fill = "Veentype"
  ) +
  # coord_flip() +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 10)
  )
#### versie 2 dikte veraarde laag met drooglegging -------------------------------
scale_factor <- 100
# Eerst facet categorieën berekenen per gebied
facet_data <- abio_proj[!is.na(Gebiedsnaam) & !is.na(dkvalg) & !is.na(drglg), 
                        .(median_dkvalg = median(dkvalg, na.rm = TRUE),
                          p70_dkvalg = mean(dkvalg, na.rm = TRUE) + sd(dkvalg, na.rm = TRUE),
                          mean_drglg_cm = mean(drglg, na.rm = TRUE) * 100), 
                        by = Gebiedsnaam]
# Facet categorieën definiëren
facet_data[, facet_category := fifelse(
  median_dkvalg > mean_drglg_cm, 
  "Veraarde laag > Drooglegging",
  fifelse(
      p70_dkvalg <= mean_drglg_cm,
      "Veraarde laag ≤ Drooglegging",
      "Veraarde laag ≈ Drooglegging"
    )
  )]
# Merge data
abio_proj_facet <- merge(abio_proj, facet_data[, .(Gebiedsnaam, facet_category)], 
                         by = "Gebiedsnaam", all.x = TRUE)
# Data voorbereiden voor plots
dkvalg_summary_facet <- abio_proj_facet[!is.na(Gebiedsnaam) & !is.na(dkvalg) & !is.na(veentype), 
                                       .(mean_dkvalg = mean(dkvalg, na.rm = TRUE),
                                         se_dkvalg = sd(dkvalg, na.rm = TRUE) / sqrt(.N),
                                         n_obs = .N), 
                                       by = .(Gebiedsnaam, veentype, facet_category)]
drglg_bars_facet <- merge(drglg_bars, facet_data[, .(Gebiedsnaam, facet_category)], 
                          by = "Gebiedsnaam", all.x = TRUE)
sort_order <- drglg_bars_facet[order(avg_drglg)]$Gebiedsnaam
drglg_bars_facet[, Gebiedsnaam := factor(Gebiedsnaam, levels = sort_order)]
# Aantal gebieden per categorie tellen voor plot verhoudingen
n_plot1 <- length(unique(dkvalg_summary_facet[facet_category == "Veraarde laag > Drooglegging"]$Gebiedsnaam))
n_plot2 <- length(unique(dkvalg_summary_facet[facet_category == "Veraarde laag ≈ Drooglegging"]$Gebiedsnaam))
n_plot3 <- length(unique(dkvalg_summary_facet[facet_category == "Veraarde laag ≤ Drooglegging"]$Gebiedsnaam))
# Print aantal gebieden voor controle
cat("Aantal gebieden per plot:\n")
cat("Plot 1:", n_plot1, "\n")
cat("Plot 2:", n_plot2, "\n")
cat("Plot 3:", n_plot3, "\n")

# Plot 1: Veraarde laag > Drooglegging
plot1_data_dkvalg <- dkvalg_summary_facet[facet_category == "Veraarde laag > Drooglegging"]
plot1_data_drglg <- drglg_bars_facet[facet_category == "Veraarde laag > Drooglegging"]


p1 <- ggplot() +
  geom_col(data = plot1_data_drglg, 
           aes(x = Gebiedsnaam, y = avg_drglg * scale_factor), 
           fill = "steelblue", alpha = 0.4, width = 0.92) +  # Donkerder blauw (steelblue + alpha 0.8)
  geom_col(data = plot1_data_dkvalg, 
           aes(x = Gebiedsnaam, y = mean_dkvalg, fill = veentype), 
           position = position_dodge(width = 0.7), 
           alpha = 0.7, width = 0.6) +  # Minder donker (alpha 0.65)
  geom_errorbar(data = plot1_data_dkvalg,
                aes(x = Gebiedsnaam, 
                    ymin = mean_dkvalg - se_dkvalg, 
                    ymax = mean_dkvalg + se_dkvalg,
                    group = veentype), 
                position = position_dodge(width = 0.7),
                width = 0.1, size = 0.8, color = "black") +
  # geom_text(data = plot1_data_dkvalg,
  #           aes(x = Gebiedsnaam, 
  #               y = mean_dkvalg + se_dkvalg + 2, 
  #               label = paste0("n=", n_obs),
  #               group = veentype), 
  #           position = position_dodge(width = 0.7),
  #           size = 3.5, angle = 45, hjust = 0) +
# JUISTE KLEUREN MAPPING VOOR PENETROMETER DATA
  scale_fill_manual(
    values = c(
      "kleiig veen" = okabe_ito_colors[1],                                    # oranje
      "veenmosveen" = okabe_ito_colors[2],                                   # lichtblauw 
      "zeggerietveen_rietveen" = okabe_ito_colors[3],                       # groen
      "zeggeveen_rietzeggeveen_broekveen" = okabe_ito_colors[3],            # groen
      "broekveen" = okabe_ito_colors[4],                                     # geel
      "bagger_verslagenveen_gyttja_anders" = okabe_ito_colors[8]            # grijs
    ),
    name = "Veentype",
    labels = c(
      "kleiig veen" = "Kleiig veen",
      "veenmosveen" = "Veenmosveen", 
      "zeggerietveen_rietveen" = "Zegge-/rietveen",
      "zeggeveen_rietzeggeveen_broekveen" = "Zeggeveen/broekveen",
      "broekveen" = "Broekveen",
      "bagger_verslagenveen_gyttja_anders" = "Overig"
    ),
    na.value = "grey50"
  ) +
  geom_hline(yintercept = median(abio_proj$dkvalg, na.rm = TRUE), 
             color = "black", linetype = "dashed", size = 1) +
  scale_y_continuous(
    name = "Dikte veraarde laag (cm)",
    limits = c(0, 100),
    sec.axis = sec_axis(~ . / scale_factor, name = NULL)
  ) +
  labs(title = "Veraarde laag >\nDrooglegging", x = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(size = 12),
    legend.position = "none",
    plot.margin = margin(10, 5.5, 5.5, 5.5, "pt")
  )

# Plot 2: Veraarde laag ≈ Drooglegging
plot2_data_dkvalg <- dkvalg_summary_facet[facet_category == "Veraarde laag ≈ Drooglegging"]
plot2_data_drglg <- drglg_bars_facet[facet_category == "Veraarde laag ≈ Drooglegging"]

p2 <- ggplot() +
  geom_col(data = plot2_data_drglg, 
           aes(x = Gebiedsnaam, y = avg_drglg * scale_factor), 
           fill = "steelblue", alpha = 0.4, width = 0.92) +  # Donkerder blauw
  geom_col(data = plot2_data_dkvalg, 
           aes(x = Gebiedsnaam, y = mean_dkvalg, fill = veentype), 
           position = position_dodge(width = 0.7), 
           alpha = 0.7, width = 0.6) +  # Minder donker
  geom_errorbar(data = plot2_data_dkvalg,
                aes(x = Gebiedsnaam, 
                    ymin = mean_dkvalg - se_dkvalg, 
                    ymax = mean_dkvalg + se_dkvalg,
                    group = veentype), 
                position = position_dodge(width = 0.7),
                width = 0.2, size = 0.8, color = "black") +
  # geom_text(data = plot2_data_dkvalg,
  #           aes(x = Gebiedsnaam, 
  #               y = mean_dkvalg + se_dkvalg + 2, 
  #               label = paste0("n=", n_obs),
  #               group = veentype), 
  #           position = position_dodge(width = 0.7),
  #           size = 3.5, angle = 45, hjust = 0) +
  # # JUISTE KLEUREN MAPPING VOOR PENETROMETER DATA
  scale_fill_manual(
    values = c(
      "kleiig veen" = okabe_ito_colors[1],                                    # oranje
      "veenmosveen" = okabe_ito_colors[2],                                   # lichtblauw 
      "zeggerietveen_rietveen" = okabe_ito_colors[3],                       # groen
      "zeggeveen_rietzeggeveen_broekveen" = okabe_ito_colors[3],            # groen
      "broekveen" = okabe_ito_colors[4],                                     # geel
      "bagger_verslagenveen_gyttja_anders" = okabe_ito_colors[8]            # grijs
    ),
    name = "Veentype",
    labels = c(
      "kleiig veen" = "Kleiig veen",
      "veenmosveen" = "Veenmosveen", 
      "zeggerietveen_rietveen" = "Zegge-/rietveen",
      "zeggeveen_rietzeggeveen_broekveen" = "Zeggeveen/broekveen",
      "broekveen" = "Broekveen",
      "bagger_verslagenveen_gyttja_anders" = "Overig"
    ),
    na.value = "grey50"
  ) +
  geom_hline(yintercept = median(abio_proj$dkvalg, na.rm = TRUE), 
             color = "black", linetype = "dashed", size = 1) +
  scale_y_continuous(
    name = NULL,
    limits = c(0, 100),
    sec.axis = sec_axis(~ . / scale_factor, name = NULL)
  ) +
  labs(title = "Veraarde laag ≈\nDrooglegging", x = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(size = 12),
    legend.position = "none",
    plot.margin = margin(10, 5.5, 5.5, 5.5, "pt")
  )

# Plot 3: 70% percentiel ≤ Drooglegging
plot3_data_dkvalg <- dkvalg_summary_facet[facet_category == "Veraarde laag ≤ Drooglegging"]
plot3_data_drglg <- drglg_bars_facet[facet_category == "Veraarde laag ≤ Drooglegging"]

p3 <- ggplot() +
  geom_col(data = plot3_data_drglg, 
           aes(x = Gebiedsnaam, y = avg_drglg * scale_factor), 
           fill = "steelblue", alpha = 0.4, width = 0.92) +  # Donkerder blauw
  geom_col(data = plot3_data_dkvalg, 
           aes(x = Gebiedsnaam, y = mean_dkvalg, fill = veentype), 
           position = position_dodge(width = 0.7), 
           alpha = 0.7, width = 0.6) +  # Minder donker
  geom_errorbar(data = plot3_data_dkvalg,
                aes(x = Gebiedsnaam, 
                    ymin = mean_dkvalg - se_dkvalg, 
                    ymax = mean_dkvalg + se_dkvalg,
                    group = veentype), 
                position = position_dodge(width = 0.7),
                width = 0.2, size = 0.8, color = "black") +
  # geom_text(data = plot3_data_dkvalg,
  #           aes(x = Gebiedsnaam, 
  #               y = mean_dkvalg + se_dkvalg + 2, 
  #               label = paste0("n=", n_obs),
  #               group = veentype), 
  #           position = position_dodge(width = 0.7),
  #           size = 3.5, angle = 45, hjust = 0) +
  # # JUISTE KLEUREN MAPPING VOOR PENETROMETER DATA
  scale_fill_manual(
    values = c(
      "kleiig veen" = okabe_ito_colors[1],                                    # oranje
      "veenmosveen" = okabe_ito_colors[2],                                   # lichtblauw 
      "zeggerietveen_rietveen" = okabe_ito_colors[3],                       # groen
      "zeggeveen_rietzeggeveen_broekveen" = okabe_ito_colors[3],            # groen
      "broekveen" = okabe_ito_colors[4],                                     # geel
      "bagger_verslagenveen_gyttja_anders" = okabe_ito_colors[8]            # grijs
    ),
    name = "Veentype",
    labels = c(
      "kleiig veen" = "Kleiig veen",
      "veenmosveen" = "Veenmosveen", 
      "zeggerietveen_rietveen" = "Zegge-/rietveen",
      "zeggeveen_rietzeggeveen_broekveen" = "Zeggeveen/broekveen",
      "broekveen" = "Broekveen",
      "bagger_verslagenveen_gyttja_anders" = "Overig"
    ),
    na.value = "grey50"
  ) + geom_hline(yintercept = median(abio_proj$dkvalg, na.rm = TRUE), 
             color = "black", linetype = "dashed", size = 1) +
  scale_y_continuous(
    name = NULL,
    limits = c(0, 100),
    sec.axis = sec_axis(~ . / scale_factor, name = "Drooglegging (m)")
  ) +
  labs(title = "70% percentiel ≤\nDrooglegging", x = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(size = 12),
    legend.position = "none",
    plot.margin = margin(10, 5.5, 5.5, 5.5, "pt")
  )

# Voeg de legend apart toe
legend_plot <- ggplot() +
  geom_col(data = dkvalg_summary_facet[1:5], 
           aes(x = Gebiedsnaam, y = mean_dkvalg, fill = veentype)) +
  scale_fill_manual(values = okabe_ito_colors, 
                    name = "Veentype",
                    labels = c('Kleiig veen','Veenmosveen','Zeggeveen','Broekveen','Overig'), 
                    na.value = "grey50") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12),  # Vergroot legend titel
        legend.text = element_text(size = 11))  # Vergroot legend tekst

# Gecombineerde plot met patchwork - plot 1 iets breder maken
combined_plot <- p1 + p2 + p3 + 
  plot_layout(ncol = 3, widths = c(n_plot1 + 2, n_plot2, n_plot3)) +
  plot_annotation(
    title = "Dikte Veraarde Laag en Drooglegging per Gebied",
    subtitle = paste0("Gegroepeerd op basis van verhouding veraarde laag tot drooglegging\nStippellijn = mediaan dikte veraarde laag (", 
                     round(median(abio_proj$dkvalg, na.rm = TRUE), 1), " cm)"),
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # Vergroot titel
      plot.subtitle = element_text(size = 14, hjust = 0.5),  # Vergroot subtitle
      plot.caption = element_text(size = 12, hjust = 0.5, margin = margin(t = 10))
    )
  )

# Combineer alles
final_plot <- combined_plot + 
  get_legend(legend_plot) + 
  plot_layout(heights = c(10, 1))

# Toon de plot
print(final_plot)

#### plot voor dikte veraarde laag met drooglegging voor één gebied-------------------------------
scale_factor <- 100
ggplot() +
  geom_col(data = abio_proj, 
           aes(x = SlootID, y = drglg * scale_factor), 
           fill = "steelblue", alpha = 0.4, width = 0.92) +  # Donkerder blauw (steelblue + alpha 0.8)
  geom_col(data = abio_proj, 
           aes(x = SlootID, y = dkvalg, fill = "brown"), 
           position = position_dodge(width = 0.7), 
           alpha = 0.8, width = 0.6) +  # Minder donker (alpha 0.65)
  geom_hline(yintercept = median(abio_proj$dkvalg, na.rm = TRUE), 
             color = "black", linetype = "dashed", size = 1) +
  scale_y_continuous(
    name = "Dikte veraarde laag (cm)",
    limits = c(0, 100),
    sec.axis = sec_axis(~ . / scale_factor, name = "Drooglegging (m)")
  ) +
  # facet_wrap(sloot_cluster~. , ncol = 1, scales = "free_x") +
  labs(title = "Dikte veraarde laag en drooglegging", subtitle = "Blauwe balken: drooglegging en bruine balken: dikte veraarde laag\nStippellijn = mediaan dikte veraarde laag", x = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(size = 12),
    legend.position = "none",
    plot.margin = margin(10, 5.5, 5.5, 5.5, "pt")
  )

#### simpel plot dikte veraard laag ---------------------------------------------------
# Bereken whisker-range voor dikte veraarde laag uit ABIO_PROJ
veraarde_summary <- abio_proj[!is.na(Gebiedsnaam) & !is.na(dkvalg), 
                              .(mean_dkvalg = mean(dkvalg, na.rm = TRUE),
                                sd_dkvalg = sd(dkvalg, na.rm = TRUE),
                                q25_dkvalg = quantile(dkvalg, 0.25, na.rm = TRUE),
                                q75_dkvalg = quantile(dkvalg, 0.75, na.rm = TRUE),
                                min_dkvalg = min(dkvalg, na.rm = TRUE),
                                max_dkvalg = max(dkvalg, na.rm = TRUE),
                                n_obs = .N), 
                              by = c('Gebiedsnaam')]

# STAP 1: Bereken eerst alleen de IQR
veraarde_summary[, iqr_dkvalg := q75_dkvalg - q25_dkvalg]

# STAP 2: Bereken nu de whisker-range met de al bestaande iqr_dkvalg kolom
veraarde_summary[, `:=`(
  whisker_lower = pmax(min_dkvalg, q25_dkvalg - 1.5 * iqr_dkvalg),
  whisker_upper = pmin(max_dkvalg, q75_dkvalg + 1.5 * iqr_dkvalg)
)]

# Bereken gemiddelde drooglegging per gebied voor achtergrondbalken
drglg_bars <- abio_proj[!is.na(Gebiedsnaam) & !is.na(drglg), 
                        .(avg_drglg = mean(drglg, na.rm = TRUE)), 
                        by = Gebiedsnaam]

# Merge de datasets
veraarde_plot_data <- merge(veraarde_summary, drglg_bars, by = "Gebiedsnaam", all.x = TRUE)

# Schaalfactor voor drooglegging (zodat het zichtbaar is als achtergrond)
scale_factor <- 100

# Sorteer op gemiddelde DIKTE VERAARDE LAAG (mean_dkvalg)
veraarde_plot_data <- veraarde_plot_data[order(mean_dkvalg)]
veraarde_plot_data[, Gebiedsnaam := factor(Gebiedsnaam, levels = unique(Gebiedsnaam))]

# Bereken mediaan voor referentielijn
median_dkvalg <- median(abio_proj$dkvalg, na.rm = TRUE)

# Plot maken
ggplot(veraarde_plot_data) +
  # Achtergrondkolommen voor drooglegging (verbeterde zichtbaarheid)
  geom_col(aes(x = Gebiedsnaam, y = avg_drglg * scale_factor), 
           fill = "steelblue", alpha = 0.6, width = 1) +
  
  # Kolommen voor gemiddelde dikte veraarde laag
  geom_col(aes(x = Gebiedsnaam, y = mean_dkvalg), 
           fill = "#009E73", width = 0.7, alpha = 0.5) +
  
  # Whisker-range errorbars (zoals boxplot whiskers)
  geom_errorbar(aes(x = Gebiedsnaam, 
                    ymin = whisker_lower, 
                    ymax = whisker_upper),
                width = 0.5, color = "black", size = 0.8) +
  
  # Mediaan als referentielijn
  geom_hline(yintercept = median_dkvalg, 
             color = "black", linetype = "dashed", size = 1) +
  
  # Dubbele y-as
  scale_y_continuous(
    name = "Dikte veraarde laag (cm)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Gemiddelde drooglegging (m)")
  ) +
  coord_flip() +
  
  # Styling
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    panel.grid.major = element_line(color = "grey90", size = 0.5),
    panel.grid.minor = element_blank()
  ) +
  
  # Labels
  labs(
    title = "Dikte Veraarde Laag per Gebied met Drooglegging",
    subtitle = paste0("Donkerblauwe balken = gemiddelde drooglegging\nZwarte stippellijn = mediaan dikte veraarde laag (", 
                     round(median_dkvalg, 1), " cm)\nErrorbars = whisker-range (Q1-1.5×IQR tot Q3+1.5×IQR)"),
    x = "Gebied"
  )

# Plot opslaan
ggsave(file = 'output/AlleGebieden/Tussenrapportage/veraarde_laag_whisker_range_drooglegging.png', 
       width = 40, height = 25, units = 'cm', dpi = 800)

#### schema veraarde laag en drooglegging---------------------------------------------------
library(ggplot2)
library(grid)
library(gridExtra)

# Functie om een schematisch diagram te maken
create_peat_drainage_diagram <- function() {
  
  # Drie scenario's maken
  scenarios <- list(
    list(title = "Veraarde laag > Drooglegging", 
         peat_thickness = 3, drainage_level = 1.5, color = "#E69F00"),
    list(title = "Veraarde laag ≈ Drooglegging", 
         peat_thickness = 2, drainage_level = 2, color = "#56B4E9"),
    list(title = "Veraarde laag < Drooglegging", 
         peat_thickness = 1, drainage_level = 3, color = "#009E73")
  )
  
  # Functie om één scenario te plotten
  plot_scenario <- function(scenario, scenario_index) {
    
    # Grondwaterstand curves berekenen
    x_vals <- seq(0, 4, by = 0.1)
    gw_start <- 4 - scenario$drainage_level  # Start aan onderkant drooglegging
    
    # Bepaal amplitude zodat bolle curve tot bovenkant veraarde laag gaat
    peat_top <- 4  # Bovenkant veraarde laag = maaiveld
    amplitude <- (peat_top - gw_start)  # Dit wordt de amplitude voor beide curves
    
    # Bolle curve - hoogste in het midden tot bovenkant veraarde laag
    gw_vals_bol <- gw_start + amplitude * sin(pi * x_vals / 4)
    
    # Holle curve - even ver naar beneden als bolle curve naar boven
    gw_vals_hol <- gw_start - amplitude * sin(pi * x_vals / 4)
    
    # Basis plot setup
    p <- ggplot() +
      # Grond niveau (groen)
      geom_rect(aes(xmin = 0, xmax = 4, ymin = 4, ymax = 4.5), 
                fill = "#90EE90", color = "black", size = 0.5) +
      
      # Veraarde laag (bruin)
      geom_rect(aes(xmin = 0, xmax = 4, 
                    ymin = 4 - scenario$peat_thickness, ymax = 4), 
                fill = "#8B4513", color = "black", size = 0.5, alpha = 0.8) +
      
      # Ondergrond (donkerbruin)
      geom_rect(aes(xmin = 0, xmax = 4, ymin = 0, 
                    ymax = 4 - scenario$peat_thickness), 
                fill = "#654321", color = "black", size = 0.5) +
      
      # Water (blauw)
      geom_rect(aes(xmin = 3.5, xmax = 5, 
                    ymin = 4 - scenario$drainage_level, ymax = 4), 
                fill = "#4682B4", color = "black", size = 0.5, alpha = 0.7) +
      
      # Bolle grondwaterstand stippellijn (tot bovenkant veraarde laag)
      geom_line(data = data.frame(x = x_vals, y = gw_vals_bol),
                aes(x = x, y = y), 
                linetype = "dashed", color = "darkblue", size = 1.2) +
      
      # Holle grondwaterstand stippellijn (even ver naar beneden)
      geom_line(data = data.frame(x = x_vals, y = gw_vals_hol),
                aes(x = x, y = y), 
                linetype = "dashed", color = "navy", size = 1.2) +
      
      # Pijl voor veraarde laag dikte
      geom_segment(aes(x = -0.3, y = 4, 
                       xend = -0.3, yend = 4 - scenario$peat_thickness),
                   arrow = arrow(length = unit(0.2, "inches"), 
                                ends = "both", type = "closed"),
                   color = scenario$color, size = 2) +
      
      # Pijl voor drooglegging
      geom_segment(aes(x = 4.2, y = 4, 
                       xend = 4.2, yend = 4 - scenario$drainage_level),
                   arrow = arrow(length = unit(0.2, "inches"), 
                                ends = "both", type = "closed"),
                   color = "#4682B4", size = 2) +
      
      # LABELS IN DE VLAKKEN
      # Label voor veraarde laag (in de bruine laag)
      annotate("text", x = 2, y = 4 - scenario$peat_thickness/2, 
               label = "VERAARDE LAAG", hjust = 0.5, vjust = 0.5, size = 4, 
               fontface = "bold", color = "white") +
      
      # Label voor drooglegging (in het water)
      annotate("text", x = 4.25, y = 4 - scenario$drainage_level/2, 
               label = "DROOG-\nLEGGING", hjust = 0.5, vjust = 0.5, size = 4, 
               fontface = "bold", color = "white") +
      
      # Label bij de pijl voor dikte
      annotate("text", x = -0.8, y = 4 - scenario$peat_thickness/2, 
               label = "Dikte", hjust = 1, size = 5, fontface = "bold", 
               color = scenario$color) +
      
      # Waterpeil label
      annotate("text", x = 4.25, y = 4 - scenario$drainage_level - 0.2, 
               label = "Waterpeil", hjust = 0.5, size = 4, 
               color = "#4682B4", fontface = "bold") +
      
      # Grondwaterstand label
      annotate("text", x = 2, y = min(gw_vals_hol) - 0.3, 
               label = "Grondwaterstand", hjust = 0.5, size = 4, 
               color = "darkblue", fontface = "bold") +
      
      # Vegetatie (kleine groene lijntjes)
      geom_segment(aes(x = c(0.5, 1.5, 2.5, 3.5), y = 4.5, 
                       xend = c(0.5, 1.5, 2.5, 3.5), yend = 5),
                   color = "#228B22", size = 3) +
      
      # Titel
      labs(title = scenario$title) +
      
      # Thema aanpassingen
      coord_cartesian(xlim = c(-1.5, 5.5), ylim = c(-0.5, 5.5)) +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.margin = margin(10, 10, 10, 10)
      )
    
    return(p)
  }
  
  # Alle drie scenario's plotten met index
  plots <- list()
  for (i in 1:length(scenarios)) {
    plots[[i]] <- plot_scenario(scenarios[[i]], i)
  }
  
  # Combineer plots horizontaal
  combined_plot <- do.call(grid.arrange, c(plots, list(ncol = 3)))
  
  return(combined_plot)
}

# Diagram maken en opslaan
diagram <- create_peat_drainage_diagram()

# Opslaan als PNG
png("veraarde_laag_drooglegging_schema.png", 
    width = 15, height = 6, units = "in", res = 300)
print(diagram)
dev.off()

### Drooglegging -----------------------
#### Bereken mediaan en standard error per gebied voor drooglegging------------------------
drglg_summary <- abio_proj[!is.na(Gebiedsnaam) & !is.na(drglg_2), 
                          .(median_drglg = median(drglg_2, na.rm = TRUE),
                            mean_drglg = mean(drglg_2, na.rm = TRUE),
                            q25_drglg = quantile(drglg_2, 0.25, na.rm = TRUE),
                            q75_drglg = quantile(drglg_2, 0.75, na.rm = TRUE),
                            min_drglg = min(drglg_2, na.rm = TRUE),
                            max_drglg = max(drglg_2, na.rm = TRUE),
                            n_obs = .N), 
                          by = .(Gebiedsnaam, veentype)]
# Bereken IQR en whisker-range
drglg_summary[, iqr_drglg := q75_drglg - q25_drglg]
drglg_summary[, whisker_lower := pmax(min_drglg, q25_drglg - 1.5 * iqr_drglg)]
drglg_summary[, whisker_upper := pmin(max_drglg, q75_drglg + 1.5 * iqr_drglg)]

# Bereken ook overall mediaan voor referentielijn
overall_median <- median(abio_proj$drglg, na.rm = TRUE)
# Sorteer gebieden op mediaan drooglegging
sort_order <- drglg_summary[, .(median_gebied = median(median_drglg, na.rm = TRUE)), 
                           by = Gebiedsnaam][order(median_gebied)]
drglg_summary[, Gebiedsnaam := factor(Gebiedsnaam, levels = sort_order$Gebiedsnaam)]
# Plot met geom_point voor mediaan en errorbars
ggplot(drglg_summary, aes(x = Gebiedsnaam, y = median_drglg, color = veentype)) +
  # Mediaan punten per veentype
  geom_point(size = 4, alpha = 0.8, position = position_dodge(width = 0.5)) +
# Errorbars met whisker-range (zoals boxplot whiskers)
  geom_errorbar(aes(ymin = whisker_lower, 
                    ymax = whisker_upper),
                width = 0.2, 
                position = position_dodge(width = 0.5),
                alpha = 0.8, size = 0.8) +
  
  # Overall mediaan als referentielijn
  geom_hline(yintercept = overall_median, 
             color = "black", linetype = "dashed", size = 1) +
  
  # Okabe-Ito kleuren voor veentype
  scale_color_manual(
    values = c(
      "kleiig veen" = okabe_ito_colors[1],
      "veenmosveen" = okabe_ito_colors[2], 
      "zeggerietveen_rietveen" = okabe_ito_colors[3],
      "zeggeveen_rietzeggeveen_broekveen" = okabe_ito_colors[3],
      "broekveen" = okabe_ito_colors[4],
      "bagger_verslagenveen_gyttja_anders" = okabe_ito_colors[8]
    ),
    name = "Veentype",
    labels = c(
      "kleiig veen" = "Kleiig veen",
      "veenmosveen" = "Veenmosveen", 
      "zeggerietveen_rietveen" = "Zegge-/rietveen",
      "zeggeveen_rietzeggeveen_broekveen" = "Zeggeveen/broekveen",
      "broekveen" = "Broekveen",
      "bagger_verslagenveen_gyttja_anders" = "Overig"
    ),
    na.value = "grey50"
  ) +
  
  # Flip coordinates voor betere leesbaarheid
  coord_flip() +
  
  # Styling
  scale_y_continuous(
    name = "Drooglegging (m)",
    breaks = seq(0, 3, 0.5)
  ) +
  
  labs(
    title = "Mediaan Drooglegging per Gebied en Veentype",
    subtitle = paste0("Zwarte stippellijn = overall mediaan (", 
                     round(overall_median, 2), " m)\nErrorbars tonen spreiding data zonder outliers (whisker-range)"),
 
    x = "Gebied"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.position = "bottom",
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14, face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    panel.grid.major.y = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  ) +
  
  guides(color = guide_legend(nrow = 2, override.aes = list(size = 3)))

# Save de plot
ggsave(file = 'output/AlleGebieden/Tussenrapportage/drooglegging_mediaan_per_gebied.png', 
       width = 30, height = 25, units = 'cm', dpi = 800)

#### Drooglegging tov oever en perceel---------------------------------
# Bereken mediaan en standard error per gebied voor beide drooglegging variabelen
drglg_summary_combined <- abio_proj[!is.na(Gebiedsnaam) & (!is.na(drglg) | !is.na(drglg_2)), 
                                   .(median_drglg_oever = median(drglg, na.rm = TRUE),
                                     se_drglg_oever = sd(drglg, na.rm = TRUE) / sqrt(sum(!is.na(drglg))),
                                     n_obs_oever = sum(!is.na(drglg)),
                                     median_drglg_verschil = median(drglg_2, na.rm = TRUE),
                                     se_drglg_verschil = sd(drglg_2, na.rm = TRUE) / sqrt(sum(!is.na(drglg_2))),
                                     n_obs_verschil = sum(!is.na(drglg_2))), 
                                   by = .(Gebiedsnaam, veentype)]

# Filter gebieden die data hebben voor beide variabelen
drglg_summary_combined <- drglg_summary_combined[!is.na(median_drglg_oever) | !is.na(median_drglg_verschil)]

# Bereken overall medianen voor referentielijnen
overall_median_oever <- median(abio_proj$drglg, na.rm = TRUE)
overall_median_verschil <- median(abio_proj$drglg_2, na.rm = TRUE)

# Sorteer gebieden op mediaan drooglegging (oever-gebaseerd)
sort_order <- drglg_summary_combined[!is.na(median_drglg_oever), .(median_gebied = median(median_drglg_oever, na.rm = TRUE)), 
                                    by = Gebiedsnaam][order(median_gebied)]
drglg_summary_combined[, Gebiedsnaam := factor(Gebiedsnaam, levels = sort_order$Gebiedsnaam)]

# Plot 1: Drooglegging op basis van hoogste oeverpunt
p1 <- ggplot(drglg_summary_combined[!is.na(median_drglg_oever)], 
             aes(x = Gebiedsnaam, y = median_drglg_oever, color = veentype)) +
  # Mediaan punten per veentype
  geom_point(size = 4, alpha = 0.8, position = position_dodge(width = 0.5)) +
  
  # Error bars (standard error)
  geom_errorbar(aes(ymin = median_drglg_oever - se_drglg_oever, 
                    ymax = median_drglg_oever + se_drglg_oever),
                width = 0.2, 
                position = position_dodge(width = 0.5),
                alpha = 0.8, size = 0.8) +
  
  # Overall mediaan als referentielijn
  geom_hline(yintercept = overall_median_oever, 
             color = "black", linetype = "dashed", size = 1) +
  
  # Okabe-Ito kleuren voor veentype
  scale_color_manual(
    values = c(
      "kleiig veen" = okabe_ito_colors[1],
      "veenmosveen" = okabe_ito_colors[2], 
      "zeggerietveen_rietveen" = okabe_ito_colors[3],
      "zeggeveen_rietzeggeveen_broekveen" = okabe_ito_colors[3],
      "broekveen" = okabe_ito_colors[4],
      "bagger_verslagenveen_gyttja_anders" = okabe_ito_colors[8]
    ),
    name = "Veentype",
    na.value = "grey50"
  ) +
  
  # COORD_FLIP() TOEGEVOEGD
  coord_flip() +
  
  # Styling
  scale_y_continuous(
    name = "Drooglegging (m)",
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1)
  ) +
  
  labs(
    title = "Verschil waterlijn-insteek",
    x = "Gebied"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 13),
    legend.position = "none",
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    panel.grid.major.y = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 5.5, 5.5, 5.5, "pt")
  )

# Plot 2: Drooglegging op basis van verschil waterlijn en perceel
p2 <- ggplot(drglg_summary_combined[!is.na(median_drglg_verschil)], 
             aes(x = Gebiedsnaam, y = median_drglg_verschil, color = veentype)) +
  geom_point(size = 4, alpha = 0.8, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = median_drglg_verschil - se_drglg_verschil, 
                    ymax = median_drglg_verschil + se_drglg_verschil),
                width = 0.2, 
                position = position_dodge(width = 0.5),
                alpha = 0.8, size = 0.8) +
  geom_hline(yintercept = overall_median_verschil, 
             color = "black", linetype = "dashed", size = 1) +
  
  scale_color_manual(
    values = c(
      "kleiig veen" = okabe_ito_colors[1],
      "veenmosveen" = okabe_ito_colors[2], 
      "zeggerietveen_rietveen" = okabe_ito_colors[3],
      "zeggeveen_rietzeggeveen_broekveen" = okabe_ito_colors[3],
      "broekveen" = okabe_ito_colors[4],
      "bagger_verslagenveen_gyttja_anders" = okabe_ito_colors[8]
    ),
    name = "Veentype",
    na.value = "grey50"
  ) +
  
  # COORD_FLIP() TOEGEVOEGD
  coord_flip() +
  
  scale_y_continuous(
    name = "Drooglegging (m)",
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1)
  ) +
  
  # Y-AS LABELS WEG VOOR RECHTSE PLOT
  scale_x_discrete(labels = NULL) +
  
  labs(
    title = "Verschil waterlijn-perceel",
    x = NULL
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_blank(),  # Y-as labels weg
    axis.title = element_text(size = 13),
    legend.position = "none",
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    panel.grid.major.y = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 5.5, 5.5, 5.5, "pt")
  )

# Gecombineerde plot met patchwork
combined_plot <- p1 + p2 + 
  plot_layout(ncol = 2, widths = c(1, 1)) +
  plot_annotation(
    title = "Drooglegging per Gebied en Veentype",
    subtitle = paste0("Links: mediaan = ", round(overall_median_oever, 2), 
                     " m | Rechts: mediaan = ", round(overall_median_verschil, 2), 
                     " m | Errorbars = standard error"),
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    )
  )

# Voeg gedeelde legenda toe - MEER NAAR RECHTS
legend_plot <- ggplot() +
  geom_point(data = drglg_summary_combined[1:6], 
             aes(x = Gebiedsnaam, y = median_drglg_oever, color = veentype), size = 4) +
  scale_color_manual(values = okabe_ito_colors, 
                    name = "Veentype:",
                    labels = c('Kleiig veen','Veenmosveen','Zeggeveen','Broekveen','Overig'), 
                    na.value = "grey50") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11),
        legend.justification = "center",  # Centreer de legenda
        legend.margin = margin(t = 0, r = 50, b = 0, l = 50))  # Extra margin rechts

# Combineer alles
final_plot <- combined_plot + 
  get_legend(legend_plot) + 
  plot_layout(heights = c(10, 1))

# Toon de plot
print(final_plot)

# Save de plot
ggsave(file = 'output/AlleGebieden/Tussenrapportage/drooglegging_vergelijking_methoden.png', 
       plot = final_plot, width = 40, height = 25, units = 'cm', dpi = 800)

# Print statistieken voor controle
cat("Correlatie tussen beide drooglegging methoden:\n")
cor_result <- cor(abio_proj$drglg, abio_proj$drglg_2, use = "complete.obs")
print(paste0("Pearson correlatie: ", round(cor_result, 3)))

cat("\nVerschillen per gebied (medianen):\n")
verschillen <- drglg_summary_combined[!is.na(median_drglg_oever) & !is.na(median_drglg_verschil), 
                                     .(Gebiedsnaam, 
                                       oever_methode = round(median_drglg_oever, 3),
                                       verschil_methode = round(median_drglg_verschil, 3),
                                       verschil = round(median_drglg_verschil - median_drglg_oever, 3))]
print(verschillen[order(verschil)])


### Draagkracht (oever en perceel)-----------------------
#### barplot rapport per gebied draagkracht drooglegging ---------------------------------------------------
# Schaal de drooglegging voor betere visualisatie
# Data voorbereiden - filter alleen oever en perceel
pen_simple <- penmerge[sectie_f %in% c('oever', 'perceel') & !is.na(veentype) & !is.na(Gebiedsnaam),]
# Bereken gemiddelde indringingsweerstand per SlootID, sectie en diepte-interval
pen_summary <- pen_simple[ Diept < 40, .(
  mean_indringing = median(indringingsweerstand, na.rm = TRUE),
  veentype = unique(veentype),
  # Bereken quartiles en min/max voor whisker-range
  q25_indringing = quantile(indringingsweerstand, 0.25, na.rm = TRUE),
  q75_indringing = quantile(indringingsweerstand, 0.75, na.rm = TRUE),
  min_indringing = min(indringingsweerstand, na.rm = TRUE),
  max_indringing = max(indringingsweerstand, na.rm = TRUE)
), by = .(Gebiedsnaam, sectie_f)]

# STAP 1: Bereken eerst alleen de IQR
pen_summary[, iqr_indringing := q75_indringing - q25_indringing]

# STAP 2: Bereken nu de whisker-range met de al bestaande iqr_indringing kolom
pen_summary[, `:=`(
  whisker_lower = pmax(min_indringing, q25_indringing - 1.5 * iqr_indringing),
  whisker_upper = pmin(max_indringing, q75_indringing + 1.5 * iqr_indringing)
)]
# Filter drooglegging bars - alleen gebieden waar penetrometer data is
gebieden_met_pen <- unique(pen_summary$Gebiedsnaam)
veraard_bars <- abio_proj[!is.na(dkvalg) & Gebiedsnaam %in% gebieden_met_pen, 
                        .(avg_veraard = mean(dkvalg, na.rm = TRUE)), 
                        by = Gebiedsnaam]
pen_summary <- pen_summary[Gebiedsnaam %in% unique(veraard_bars$Gebiedsnaam),]
# Sorteer op drooglegging
veraard_bars <- veraard_bars[order(avg_veraard)]
gebied_levels <- veraard_bars$Gebiedsnaam
# Nog kleinere schaalfactor voor drooglegging
scale_factor <- 0.01
# Bereken mediaan indringingsweerstand van alle gebieden samen
median_indringing <- median(pen_simple$indringingsweerstand, na.rm = TRUE)
# Kritische draagkracht voor beweiding
kritieke_draagkracht <- 0.5

# Plot met aangepaste styling
ggplot() +
  # Achtergrondbalken voor drooglegging (horizontaal) - lichter
  geom_col(data = veraard_bars, 
           aes(y = factor(Gebiedsnaam, levels = gebied_levels), 
               x = avg_veraard * scale_factor), 
           fill = "brown", alpha = 0.25, width = 0.9) +
  # Hoofdplot - indringingsweerstand per veentype (horizontaal)
  geom_point(data = pen_summary, 
           aes(y = factor(Gebiedsnaam, levels = gebied_levels), 
               x = mean_indringing, col = veentype), 
           position = position_dodge(width = 0.8), 
           alpha = 0.8, size = 5) +
  # Error bars (horizontaal)
  geom_errorbar(data = pen_summary,
                aes(y = factor(Gebiedsnaam, levels = gebied_levels), 
                    xmin = whisker_lower, 
                    xmax = whisker_upper,
                    group = veentype), 
                position = position_dodge(width = 0.8),
                width = 0.2, size = 0.5, color = "black") +
  # Mediaan indringingsweerstand als zwarte stippellijn
  geom_vline(xintercept = median_indringing, 
             color = "black", linetype = "dashed", size = 1) +
  # Kritische draagkracht voor beweiding als rode stippellijn
  geom_vline(xintercept = kritieke_draagkracht, 
             color = "red", linetype = "dashed", size = 1) +
  # Facets voor oever en perceel - labels boven
  facet_wrap(~sectie_f, scales = "free_x", strip.position = "top") +
  # Kleuren en labels
  scale_color_manual(values = okabe_ito_colors, 
                    name = "Veentype",
                    labels = c('Kleiig veen','Veenmosveen','Zeggeveen','Broekveen','Overig'), 
                    na.value = "grey50") +
  # X-as met drooglegging op tweede as
  scale_x_continuous(
    name = "Indringingsweerstand (MPa)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Dikte veraarde laag (cm)")
  ) +
  # Y-as label
  scale_y_discrete(name = "Gebied") +
  # Thema en labels
  labs(title = "Indringingsweerstand per gebied",
       subtitle = paste0("Lichtbruine balken = gemiddelde dikte veraarde laag (gesorteerd)\nZwarte stippellijn = mediaan indringingsweerstand (", 
                        round(median_indringing, 2), " MPa)\nRode stippellijn = kritische draagkracht voor beweiding (", 
                        kritieke_draagkracht, " MPa)")) +
   # GROTER LETTERTYPE THEMA
  theme_minimal(base_size = 16) +  # Verhoogd van 12 naar 16
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),      # Verhoogd van 16 naar 20
    plot.subtitle = element_text(size = 15, hjust = 0.5),                  # Verhoogd van 12 naar 16
    axis.text = element_text(size = 14),                                   # Verhoogd van 11 naar 14
    axis.title = element_text(size = 16),                                  # Verhoogd van 13 naar 16
    strip.text = element_text(size = 14, face = "bold", lineheight = 0.9), # Verhoogd van 11 naar 14
    legend.position = "bottom",                                            # Legenda aan de onderkant
    legend.text = element_text(size = 14),                                 # Legenda tekst groter
    legend.title = element_text(size = 16, face = "bold"),                 # Legenda titel groter
    
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    strip.background = element_rect(colour = "black", fill = "white", linewidth = 0.8),
    panel.grid.major = element_line(color = "grey90", size = 0.5),
    panel.grid.minor = element_blank()
  ) 


#### boxplot van indringingsweerstand op de oever met drooglegging als achtergrond  -------------------------------
ggplot() +
  # Achtergrond bars voor drooglegging
  geom_col(data = drglg_bars[!is.na(Gebiedsnaam) & Gebiedsnaam %in% unique(penmerge$Gebiedsnaam),], 
           aes(x = Gebiedsnaam, y = avg_drglg * scale_factor), 
           fill = "lightblue", alpha = 0.4, width = 0.8) +
  # Hoofdplot - veraarde laag
  geom_boxplot(data = penmerge[!is.na(dieptebin) & !is.na(Gebiedsnaam) & sectie_f == 'oever' & Diept > 40,], 
               aes(x = Gebiedsnaam, y =indringingsweerstand), 
               alpha = 0.8) +
  scale_y_continuous(
    name = "Indringingsweerstand (MPa) - log schaal",
    trans = "log10",
    labels = scales::label_number(accuracy = 0.01),
    sec.axis = sec_axis(~ . / scale_factor, name = "Gemiddelde drooglegging (m)")
  ) +
  labs(
    title = "Indringingsweerstand in de bovenste 40 cm van oever",
    x = "Gebiedsnaam",
    fill = "Veentype"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 10)
  )


#### indingingsweerstand facet vs veentype-------------------------------
# Kleuren voor veentypen (Okabe-Ito palette)
veentype_colors <- c(
  "kleiig veen" = okabe_ito_colors[1],                      # oranje
  "veenmosveen" = okabe_ito_colors[2],                     # lichtblauw 
  "zeggerietveen_rietveen" = okabe_ito_colors[3],          # groen
  "zeggeveen_rietzeggeveen_broekveen" = okabe_ito_colors[3], # groen (zelfde als zeggerietveen)
  "broekveen" = okabe_ito_colors[6],                       # geel
  "bagger_verslagenveen_gyttja_anders" = okabe_ito_colors[8] # grijs
)
##### Plot met veentype als facet------------------------------------------
# Filter penetrometerdata voor alleen oever of perceel metingen voor duidelijkheid
penmerge_plot <- penmerge[!is.na(indringingsweerstand) & !is.na(Diept) & !is.na(veentype) & 
                         sectie_f == "perceel" & Diept <= 80 & !is.na(Gebiedsnaam)]
# VOEG DROOGLEGGING TOE vanuit abio_proj
drooglg_data <- unique(abio_proj[, .(SlootID, drglg)])
penmerge_plot <- merge(penmerge_plot, drooglg_data, by = "SlootID", all.x = TRUE)
# Voeg diepte interval toe voor gemiddelde berekening
penmerge_plot[, diepte_5cm := round(Diept/5)*5]
# Bereken gemiddelde draagkracht per diepte en veentype
penmerge_gemiddelde_veentype <- penmerge_plot[!is.na(indringingsweerstand), 
                                              .(gemiddelde_draagkracht = mean(indringingsweerstand, na.rm = TRUE),
                                                n_metingen = .N), 
                                              by = .(veentype, diepte_5cm)]
# Bereken drooglegging bandbreedte per veentype
drglg_bandbreedte_per_veentype <- penmerge_plot[!is.na(drglg) & !is.na(veentype), 
                                               .(min_drglg = min(drglg, na.rm = TRUE),
                                                 max_drglg = max(drglg, na.rm = TRUE)), 
                                               by = veentype]
# Kritieke draagkracht voor beweiding
kritieke_draagkracht <- 0.5

ggplot() +
  # DROOGLEGGING BANDBREEDTE ALS LICHTBLAUWE ACHTERGROND
  geom_rect(data = drglg_bandbreedte_per_veentype, 
            aes(xmin = -Inf, xmax = Inf, 
                ymin = min_drglg * 100, ymax = max_drglg * 100), 
            fill = "lightblue", alpha = 0.3, inherit.aes = FALSE) +
  
  # ALLE PUNTEN IN LICHTGRIJS ZONDER FILL
  geom_point(data = penmerge_plot, 
             aes(x = indringingsweerstand, y = Diept), 
             color = "lightgrey", alpha = 0.6, size = 1.2) +
  
  # Gemiddelde lijn per veentype
  geom_path(data = penmerge_gemiddelde_veentype[!is.na(gemiddelde_draagkracht)], 
            aes(x = gemiddelde_draagkracht, y = diepte_5cm), 
            linetype = "dashed", color = "black", size = 1.2) +
  
  # Gemiddelde punten per veentype
  geom_point(data = penmerge_gemiddelde_veentype[!is.na(gemiddelde_draagkracht)], 
             aes(x = gemiddelde_draagkracht, y = diepte_5cm), 
             color = "black", size = 2.5, shape = 17) +
  
  # Facet per veentype met BETERE LABELS
  facet_wrap(~factor(veentype, levels = c("kleiig veen", "veenmosveen", 
                                          "zeggerietveen_rietveen", "zeggeveen_rietzeggeveen_broekveen", 
                                          "broekveen", "bagger_verslagenveen_gyttja_anders")), 
             ncol = 3,
             labeller = labeller(.default = function(x) {
               case_when(
                 x == "kleiig veen" ~ "Kleiig veen",
                 x == "veenmosveen" ~ "Veenmosveen", 
                 x == "zeggerietveen_rietveen" ~ "Zegge- en rietveen",
                 x == "zeggeveen_rietzeggeveen_broekveen" ~ "Zegge-, rietzegge-\nen broekveen",
                 x == "broekveen" ~ "Broekveen",
                 x == "bagger_verslagenveen_gyttja_anders" ~ "Bagger, verslage veen,\ngyttja en overig",
                 TRUE ~ as.character(x)
               )
             })) +
  
  # Y-as omgekeerd (diepte)
  scale_y_reverse(
    name = "Diepte (cm)",
    breaks = seq(0, 80, 20),
    limits = c(80, 0)
  ) +
  
  # X-as indringingsweerstand
  scale_x_continuous(
    name = "Indringingsweerstand (MPa)",
    breaks = seq(0, 0.6, 0.1),
    labels = c("0.0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6"),
    limits = c(0, 1)
  ) +
  
  # Kritieke draagkracht lijn
  geom_vline(xintercept = kritieke_draagkracht, color = "red", linetype = "dotted", size = 1) +
  
  # Thema en styling - AANGEPASTE STRIP TEXT GROOTTE
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13),
    strip.text = element_text(size = 11, face = "bold", lineheight = 0.9),  # Kleinere tekst, betere lijnafstand
    legend.position = "none",
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    strip.background = element_rect(colour = "black", fill = "white", linewidth = 0.8),
    panel.grid.major = element_line(color = "grey90", size = 0.5),
    panel.grid.minor = element_blank()
  ) +
  
  # Kritieke waarde annotatie
  annotate("text", x = kritieke_draagkracht, y = 5, 
           label = "Kritiek", 
           hjust = 0, vjust = 1, size = 3, 
           color = "red", fontface = "bold") +
  
  # Titels en labels
  labs(
    title = "Draagkracht Perceel over Diepte per Veentype",
    subtitle = "Zwarte stippellijn = gemiddelde per veentype\nLichtblauwe zone = drooglegging bandbreedte\nRode stippellijn = kritische draagkracht voor beweiding",
    caption = paste0("Gebaseerd op ", nrow(penmerge_plot), " oevermetingen, diepte 0-80 cm")
  )

# Opslaan van de plot
ggsave(file = 'output/AlleGebieden/Tussenrapportage/draagkracht_diepte_veentype_facet.png', 
       width = 40, height = 25, units = 'cm', dpi = 800)

##### Plot met drooglegging als facet ------------------------------------------
# Filter penetrometerdata voor alleen oever metingen voor duidelijkheid
penmerge_plot <- penmerge[!is.na(indringingsweerstand) & !is.na(Diept) & !is.na(veentype) & 
                         sectie_f == "oever" & Diept <= 80 & !is.na(Gebiedsnaam)]
# VOEG DROOGLEGGING TOE vanuit abio_proj
drooglg_data <- unique(abio_proj[, .(SlootID, drglg)])
penmerge_plot <- merge(penmerge_plot, drooglg_data, by = "SlootID", all.x = TRUE)
# Voeg diepte interval toe voor gemiddelde berekening
penmerge_plot[, diepte_5cm := round(Diept/5)*5]
# Voeg drooglegging categorie toe aan penetrometerdata
penmerge_plot[, drglg_interval := cut(drglg, 
                                     breaks = c(0, 0.20, 0.40, 0.60, 0.80, Inf),
                                     labels = c("0.0-0.2m", "0.2-0.4m", "0.4-0.6m", "0.6-0.8m", ">0.8m"),
                                     include.lowest = TRUE)]
# Bereken gemiddelde draagkracht per diepte en drooglegging interval
penmerge_gemiddelde_drglg <- penmerge_plot[!is.na(indringingsweerstand) & !is.na(drglg_interval), 
                                          .(gemiddelde_draagkracht = mean(indringingsweerstand, na.rm = TRUE),
                                            n_metingen = .N), 
                                          by = .(drglg_interval, diepte_5cm)]
# Kleuren voor veentypen (Okabe-Ito palette)
veentype_colors <- c(
  "kleiig veen" = okabe_ito_colors[1],                      # oranje
  "veenmosveen" = okabe_ito_colors[2],                     # lichtblauw 
  "zeggerietveen_rietveen" = okabe_ito_colors[3],          # groen
  "zeggeveen_rietzeggeveen_broekveen" = okabe_ito_colors[3], # groen (zelfde als zeggerietveen)
  "broekveen" = okabe_ito_colors[6],                       # oranje
  "bagger_verslagenveen_gyttja_anders" = okabe_ito_colors[8] # grijs
)

# Kritieke draagkracht voor beweiding
kritieke_draagkracht <- 0.5

# Plot met drooglegging interval als facet
ggplot() +
  # ALLE PUNTEN IN LICHTGRIJS ZONDER FILL
  geom_point(data = penmerge_plot[!is.na(drglg_interval)], 
             aes(x = indringingsweerstand, y = Diept, color = veentype), 
             alpha = 0.6, size = 1.2) +
  
  # Gemiddelde lijn per drooglegging interval
  geom_path(data = penmerge_gemiddelde_drglg[!is.na(gemiddelde_draagkracht)], 
            aes(x = gemiddelde_draagkracht, y = diepte_5cm), 
            linetype = "dashed", color = "black", size = 1.2) +
  
  # Gemiddelde punten per drooglegging interval
  geom_point(data = penmerge_gemiddelde_drglg[!is.na(gemiddelde_draagkracht)], 
             aes(x = gemiddelde_draagkracht, y = diepte_5cm), 
             color = "black", size = 2.5, shape = 17) +
  
  # Facet per drooglegging interval
  facet_wrap(~drglg_interval, ncol = 3,
             labeller = labeller(drglg_interval = c(
               "0.0-0.2m" = "Drooglegging: 0.0-0.2m",
               "0.2-0.4m" = "Drooglegging: 0.2-0.4m", 
               "0.4-0.6m" = "Drooglegging: 0.4-0.6m",
               "0.6-0.8m" = "Drooglegging: 0.6-0.8m",
               ">0.8m" = "Drooglegging: >0.8m"
             ))) +
  
  # Kleuren voor veentypen
  scale_color_manual(
    values = veentype_colors,
    name = "Veentype",
    labels = c(
      "kleiig veen" = "Kleiig veen",
      "veenmosveen" = "Veenmosveen", 
      "zeggerietveen_rietveen" = "Zegge-/rietveen",
      "zeggeveen_rietzeggeveen_broekveen" = "Zeggeveen/broekveen",
      "broekveen" = "Broekveen",
      "bagger_verslagenveen_gyttja_anders" = "Overig"
    ),
    na.value = "grey50"
  ) +
  
  # Y-as omgekeerd (diepte)
  scale_y_reverse(
    name = "Diepte (cm)",
    breaks = seq(0, 80, 20),
    limits = c(80, 0)
  ) +
  
  # X-as indringingsweerstand
  scale_x_continuous(
    name = "Indringingsweerstand (MPa)",
    breaks = seq(0, 0.6, 0.1),
    labels = c("0.0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6"),
    limits = c(0, 0.6)
  ) +
  
  # Kritieke draagkracht lijn
  geom_vline(xintercept = kritieke_draagkracht, color = "red", linetype = "dotted", size = 1) +
  
  # Thema en styling
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13),
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    strip.background = element_rect(colour = "black", fill = "white", linewidth = 0.8),
    panel.grid.major = element_line(color = "grey90", size = 0.5),
    panel.grid.minor = element_blank()
  ) +
  
  # Kritieke waarde annotatie
  annotate("text", x = kritieke_draagkracht, y = 5, 
           label = "Kritiek", 
           hjust = 0, vjust = 1, size = 3, 
           color = "red", fontface = "bold") +
  
  # Titels en labels
  labs(
    title = "Draagkracht Oever over Diepte per Drooglegging",
    subtitle = "Zwarte stippellijn = gemiddelde per drooglegging interval\nRode stippellijn = kritische draagkracht voor beweiding",
    caption = paste0("Gebaseerd op ", nrow(penmerge_plot[!is.na(drglg_interval)]), " oevermetingen, diepte 0-80 cm")
  ) +
  
  # Legenda aanpassing
  guides(color = guide_legend(
    title = "Veentype:",
    nrow = 2,
    override.aes = list(size = 3, alpha = 1)
  ))

# Opslaan van de plot
ggsave(file = 'output/AlleGebieden/Tussenrapportage/draagkracht_diepte_drooglegging_veentype_facet.png', 
       width = 40, height = 25, units = 'cm', dpi = 800)


#### draagkracht over diepte per veentype -------------------------------
overpenmerge<- penmerge[, .(
  min_diept = min(Diept, na.rm = TRUE),
  max_diept = max(Diept, na.rm = TRUE),
  n = .N
), by = .(SlootID, sectie, jaar)][order(SlootID, sectie)]
overpenmerge<- penmerge[, .(
  diept = unique(dieptebin),
   n = .N
), by = .(SlootID, sectie, jaar)][order(SlootID, sectie, jaar)]

penmerge_plot <- penmerge[!is.na(indringingsweerstand) & !is.na(dieptebin_num) & !is.na(veentype) & !is.na(SlootID)]
penmerge_plot <- penmerge_plot[gebied %in% c("rh"),]
veraard_data <- unique(abio_proj[, .(SlootID, jaar, dkvalg)])
veraard_data[,jaar := as.integer(jaar)]
penmerge_plot <- merge(penmerge_plot, veraard_data, by = c("SlootID","jaar"), all.x = TRUE)

# Bereken gemiddelde draagkracht per diepte en drooglegging interval
penmerge_gemiddelde_veentype <- penmerge_plot[!is.na(indringingsweerstand) & sectie %in% c("oever", "perceel"), 
                                          .(mediaan_draagkracht = median(indringingsweerstand, na.rm = TRUE),
                                            n_metingen = .N), 
                                          by = .(sectie, jaar, dieptebin_num)]
# Kritieke draagkracht voor beweiding
kritieke_draagkracht <- 0.5

ggplot() +
  geom_point(data = penmerge_plot[indringingsweerstand >0 & sectie %in% c("oever", "perceel"),], 
             aes(x = indringingsweerstand, y = Diept), 
             color = "lightgrey", alpha = 0.6, size = 1.2) +
# Gemiddelde lijn per veentype
  geom_path(
    data = penmerge_gemiddelde_veentype[!is.na(mediaan_draagkracht)][order(dieptebin_num)],
    aes(x = mediaan_draagkracht, y = dieptebin_num, 
        color = as.character(jaar),
        group = interaction(jaar, sectie)),  # groepeer per jaar en sectie
    linewidth = 1.2
  )+
# Gemiddelde punten per veentype
  geom_point(data = penmerge_gemiddelde_veentype[!is.na(mediaan_draagkracht)], 
             aes(x = mediaan_draagkracht, y = dieptebin_num, color = as.character(jaar)), 
             size = 2.5, shape = 17) +
  # scale_color_manual(
  #   values = veentype_colors,
  #   name = "Veentype",
  #   labels = c(
  #     "kleiig veen" = "Kleiig veen",
  #     "veenmosveen" = "Veenmosveen", 
  #     "zeggerietveen_rietveen" = "Zegge-/rietveen",
  #     "zeggeveen_rietzeggeveen_broekveen" = "Zeggeveen/broekveen",
  #     "broekveen" = "Broekveen",
  #     "bagger_verslagenveen_gyttja_anders" = "Overig"
  #   ),
  #   na.value = "grey50"
  # ) +
# Facet per veentype met BETERE LABELS
  facet_wrap(~factor(sectie, levels = c("oever", "insteek", "perceel")), 
             ncol = 3, scales = "free_x") +
# Y-as omgekeerd (diepte)
  scale_y_reverse(
    name = "Diepte (cm)",
    breaks = seq(0, 80, 20),
    limits = c(80, 0)
  ) +
# X-as indringingsweerstand
   scale_x_continuous(
    name = "Indringingsweerstand (MPa)",
    limits = c(0, max(penmerge_gemiddelde_veentype$mediaan_draagkracht, na.rm = TRUE) * 1.1),
    breaks = pretty(c(0, max(penmerge_gemiddelde_veentype$mediaan_draagkracht, na.rm = TRUE)), n = 5)
  )+
# Kritieke draagkracht lijn
  geom_vline(xintercept = kritieke_draagkracht, color = "red", linetype = "dotted", size = 1) +
# GROTER LETTERTYPE THEMA
  theme_minimal(base_size = 16) +  # Verhoogd van 12 naar 16
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),      # Verhoogd van 16 naar 20
    plot.subtitle = element_text(size = 16, hjust = 0.5),                  # Verhoogd van 12 naar 16
    axis.text = element_text(size = 14),                                   # Verhoogd van 11 naar 14
    axis.title = element_text(size = 16),                                  # Verhoogd van 13 naar 16
    strip.text = element_text(size = 14, face = "bold", lineheight = 0.9), # Verhoogd van 11 naar 14
    legend.position = "bottom",                                            # Legenda aan de onderkant
    legend.text = element_text(size = 14),                                 # Legenda tekst groter
    legend.title = element_text(size = 16, face = "bold"),                 # Legenda titel groter
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    strip.background = element_rect(colour = "black", fill = "white", linewidth = 0.8),
    panel.grid.major = element_line(color = "grey90", size = 0.5),
    panel.grid.minor = element_blank()
  ) +
 # Kritieke waarde annotatie
  annotate("text", x = kritieke_draagkracht, y = 5, 
           label = "Kritiek", 
           hjust = 0, vjust = 1, size = 3, 
           color = "red", fontface = "bold") +
            # LEGENDA AANPASSINGEN
  guides(color = guide_legend(
    title = "Meetjaar:",
    nrow = 1,                                                              # 2 rijen voor de legenda
    override.aes = list(size = 4, alpha = 1)                             # Grotere symbolen in legenda
  ))+
 # Titels en labels
  labs(
    title = "Draagkracht Perceel over Diepte in de Ronde Hoep",
    caption = paste0("Gebaseerd op ", nrow(penmerge_plot), " oevermetingen, diepte 0-80 cm")
  )

#### draagkracht versus drooglegging oever en perceel -------------------------------

# Filter penetrometerdata voor alleen oever en perceel metingen
pen_drglg_data <- penmerge[!is.na(indringingsweerstand) & !is.na(Gebiedsnaam) & 
                          sectie_f %in% c("oever", "perceel"),]  # Focus op 10-80cm diepte
# Voeg drooglegging toe vanuit abio_proj
drooglg_data <- unique(abio_proj[, .(SlootID, drglg)])
pen_drglg_data <- merge(pen_drglg_data, drooglg_data, by = "SlootID", all.x = TRUE)
# Check hoeveel data er is
cat("Totaal aantal observaties na filtering:", nrow(pen_drglg_data), "\n")
# Voeg DIEPTE INTERVALLEN toe (10-40cm en 40-80cm)
pen_drglg_data[, diepte_interval := cut(Diept, 
                                      breaks = c(0, 40, 80.1),  # Aangepaste breaks
                                      labels = c("0-40cm", "40-80cm"),
                                      include.lowest = TRUE, right = FALSE)]
pen_drglg_data <- pen_drglg_data[!is.na(diepte_interval),]

# Bereken gemiddelde indringingsweerstand per SlootID, sectie en diepte-interval
pen_summary <- pen_drglg_data[!is.na(diepte_interval), .(
  mean_indringing = mean(indringingsweerstand, na.rm = TRUE),
  drglg = unique(drglg),
  Gebiedsnaam = unique(Gebiedsnaam),
  veentype = unique(veentype)
), by = .(SlootID, sectie_f, diepte_interval)]

# Filter alleen complete gevallen
pen_summary <- pen_summary[!is.na(mean_indringing) & !is.na(drglg) & !is.na(veentype)]
# Kritieke draagkracht lijn
kritieke_draagkracht <- 0.5
# Aangepaste functie om R² te berekenen met error handling
get_r_squared_safe <- function(dt, x_col, y_col) {
  # Check of er genoeg data is
  if(nrow(dt) < 3 || 
     sum(!is.na(dt[[x_col]])) < 3 || 
     sum(!is.na(dt[[y_col]])) < 3) {
    return(NA)
  }
  
  # Probeer model te fitten
  tryCatch({
    model <- lm(as.formula(paste(y_col, "~", x_col)), data = dt)
    return(summary(model)$r.squared)
  }, error = function(e) {
    return(NA)
  })
}
# Bereken R² per sectie en diepte - ALLEEN VOOR GROEPEN MET GENOEG DATA
r2_data <- pen_summary[, {
  n_obs = .N
  list(r2 = get_r_squared_safe(.SD, "drglg", "mean_indringing"),
      n = n_obs)}, by = .(sectie_f, diepte_interval)]
# Filter alleen groepen met resultaten
r2_data <- r2_data[!is.na(r2)]

# Scatterplot maken
ggplot() +
    # Punten per veentype
    geom_point(data = pen_drglg_data, aes(x = drglg, y = indringingsweerstand), size = 3, alpha = 0.8, color = 'lightgrey') +
    geom_point(data = pen_summary, aes(x = drglg, y = mean_indringing, color = veentype), size = 3, alpha = 0.8) +
    geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 1.2, alpha = 0.2)+
    geom_hline(yintercept = kritieke_draagkracht, 
               color = "red", linetype = "dashed", linewidth = 1) +
    geom_text(data = r2_data,
                aes(x = Inf, y = Inf, 
                    label = paste0("R² = ", round(r2, 3))),
                hjust = 1.1, vjust = 1.5, size = 4, fontface = "bold",
                color = "black", inherit.aes = FALSE)+
    
    # Facets voor sectie en diepte
    facet_grid(sectie_f ~ diepte_interval,
               labeller = labeller(
                 sectie_f = c("oever" = "Oever", "perceel" = "Perceel"),
                 diepte_interval = c("10-40cm" = "10-40cm diepte", "40-80cm" = "40-80cm diepte")
               )) +
    
    # Okabe-Ito kleuren voor veentype
    scale_color_manual(
      values = c(
        "kleiig veen" = okabe_ito_colors[1],                      # oranje
        "veenmosveen" = okabe_ito_colors[2],                     # lichtblauw 
        "zeggerietveen_rietveen" = okabe_ito_colors[3],          # groen
        "zeggeveen_rietzeggeveen_broekveen" = okabe_ito_colors[3], # groen
        "broekveen" = okabe_ito_colors[6],                       # donker oranje
        "bagger_verslagenveen_gyttja_anders" = okabe_ito_colors[8] # grijs
      ),
      name = "Veentype",
      labels = c(
        "kleiig veen" = "Kleiig veen",
        "veenmosveen" = "Veenmosveen", 
        "zeggerietveen_rietveen" = "Zegge-/rietveen",
        "zeggeveen_rietzeggeveen_broekveen" = "Zeggeveen/broekveen",
        "broekveen" = "Broekveen",
        "bagger_verslagenveen_gyttja_anders" = "Overig"
      ),
      na.value = "grey50"
    ) +
    
    # As-instellingen
    scale_x_continuous(
      name = "Drooglegging (m)",
      breaks = seq(0, 1, 0.2),
      limits = c(0, 1)
    ) +
    
    scale_y_continuous(
      name = "Gemiddelde indringingsweerstand (MPa)",
      breaks = seq(0, 1.5, 0.2),
      limits = c(0, 1.5)
    ) +
    
    # Thema en styling - consistent met tussenrapportage stijl
    theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 16, hjust = 0.5),
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 16),
      strip.text = element_text(size = 14, face = "bold"),
      legend.position = "bottom",
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16, face = "bold"),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
      strip.background = element_rect(colour = "black", fill = "white", linewidth = 0.8),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
      panel.grid.minor = element_blank()
    ) +
    
    # Kritieke waarde annotatie
    annotate("text", x = 0.05, y = kritieke_draagkracht + 0.05, 
             label = "Kritieke draagkracht", 
             hjust = 0, vjust = 0, size = 4, 
             color = "red", fontface = "bold") +
    
    # Titels en labels
    labs(
      title = "Relatie tussen Draagkracht en Drooglegging",
      subtitle = "Per veentype voor oever en perceel op verschillende dieptes\nRode lijn = kritieke draagkracht voor beweiding (0.5 MPa)",
      caption = paste0("Gebaseerd op ", nrow(pen_summary), " meettrajeceten")
    ) +
    
    # Legenda aanpassingen
    guides(color = guide_legend(
      title = "Veentype:",
      nrow = 1,
      override.aes = list(size = 4, alpha = 1)
    ))

# Opslaan van de plot
ggsave(file = 'output/AlleGebieden/Tussenrapportage/draagkracht_drooglegging_scatter_diepte_sectie.png', 
         width = 45, height = 25, units = 'cm', dpi = 800)


#### draagkracht versus dikte veraarde laag oever en perceel -------------------------------
# Filter penetrometerdata voor alleen oever en perceel metingen
pen_drglg_data <- penmerge[!is.na(indringingsweerstand) & !is.na(Gebiedsnaam) & gebied == 'rh' &
                          sectie_f %in% c("oever", "perceel"),]  # Focus op alle dieptes
# Voeg dikte veraarde laag toe vanuit abio_proj
veraard_data <- unique(abio_proj[, .(SlootID, jaar, dkvalg)])
veraard_data[,jaar:= as.integer(jaar)]
pen_drglg_data[,jaar:= as.integer(jaar)]
pen_drglg_data <- merge(pen_drglg_data, veraard_data, by = c("SlootID","jaar"), all.x = TRUE)
# Check hoeveel data er is
cat("Totaal aantal observaties na filtering:", nrow(pen_drglg_data), "\n")

# Voeg DIEPTE INTERVALLEN toe (10-40cm en 40-80cm)
pen_drglg_data[, diepte_interval := cut(Diept, 
                                      breaks = c(0, 40, 80.1),  # Aangepaste breaks
                                      labels = c("0-40cm", "40-80cm"),
                                      include.lowest = TRUE, right = FALSE)]
pen_drglg_data <- pen_drglg_data[!is.na(diepte_interval),]

# Bereken gemiddelde indringingsweerstand per SlootID, sectie en diepte-interval
pen_summary <- pen_drglg_data[!is.na(diepte_interval), .(
  mean_indringing = mean(indringingsweerstand, na.rm = TRUE),
  dkvalg = unique(dkvalg),
  Gebiedsnaam = unique(Gebiedsnaam),
  veentype = unique(veentype)
), by = .(SlootID, sectie_f, diepte_interval)]

# Filter alleen complete gevallen
pen_summary <- pen_summary[!is.na(mean_indringing) & !is.na(dkvalg) & !is.na(veentype)]

# Kritieke draagkracht lijn
kritieke_draagkracht <- 0.5

# Aangepaste functie om R² te berekenen met error handling
get_r_squared_safe <- function(dt, x_col, y_col) {
  # Check of er genoeg data is
  if(nrow(dt) < 3 || 
     sum(!is.na(dt[[x_col]])) < 3 || 
     sum(!is.na(dt[[y_col]])) < 3) {
    return(NA)
  }
  
  # Probeer model te fitten
  tryCatch({
    model <- lm(as.formula(paste(y_col, "~", x_col)), data = dt)
    return(summary(model)$r.squared)
  }, error = function(e) {
    return(NA)
  })
}

# Bereken R² per sectie en diepte - ALLEEN VOOR GROEPEN MET GENOEG DATA
r2_data <- pen_summary[, {
  n_obs = .N
  list(r2 = get_r_squared_safe(.SD, "dkvalg", "mean_indringing"),
      n = n_obs)}, by = .(sectie_f, diepte_interval)]

# Filter alleen groepen met resultaten
r2_data <- r2_data[!is.na(r2)]

# Scatterplot maken
ggplot() +
    # Punten per veentype - alle metingen lichtgrijs
    geom_point(data = pen_drglg_data, aes(x = dkvalg, y = indringingsweerstand), size = 3, alpha = 0.8, color = 'lightgrey') +
    
    # Gemiddelde punten per locatie gekleurd per veentype
    geom_point(data = pen_summary, aes(x = dkvalg, y = mean_indringing, color = veentype), size = 3, alpha = 0.8) +
    
    # Trendlijn per facet
    geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 1.2, alpha = 0.2) +
    
    # Kritieke draagkracht als horizontale lijn
    geom_hline(yintercept = kritieke_draagkracht, 
               color = "red", linetype = "dashed", linewidth = 1) +
    
    # R² annotaties per facet
    geom_text(data = r2_data,
                aes(x = Inf, y = Inf, 
                    label = paste0("R² = ", round(r2, 3))),
                hjust = 1.1, vjust = 1.5, size = 4, fontface = "bold",
                color = "black", inherit.aes = FALSE) +
    
    # Facets voor sectie en diepte
    facet_grid(sectie_f ~ diepte_interval,
               labeller = labeller(
                 sectie_f = c("oever" = "Oever", "perceel" = "Perceel"),
                 diepte_interval = c("0-40cm" = "0-40cm diepte", "40-80cm" = "40-80cm diepte")
               )) +
    
    # Okabe-Ito kleuren voor veentype
    scale_color_manual(
      values = c(
        "kleiig veen" = okabe_ito_colors[1],                      # oranje
        "veenmosveen" = okabe_ito_colors[2],                     # lichtblauw 
        "zeggerietveen_rietveen" = okabe_ito_colors[3],          # groen
        "zeggeveen_rietzeggeveen_broekveen" = okabe_ito_colors[3], # groen
        "broekveen" = okabe_ito_colors[6],                       # donker oranje
        "bagger_verslagenveen_gyttja_anders" = okabe_ito_colors[8] # grijs
      ),
      name = "Veentype",
      labels = c(
        "kleiig veen" = "Kleiig veen",
        "veenmosveen" = "Veenmosveen", 
        "zeggerietveen_rietveen" = "Zegge-/rietveen",
        "zeggeveen_rietzeggeveen_broekveen" = "Zeggeveen/broekveen",
        "broekveen" = "Broekveen",
        "bagger_verslagenveen_gyttja_anders" = "Overig"
      ),
      na.value = "grey50"
    ) +
    
    # As-instellingen
    scale_x_continuous(
      name = "Dikte veraarde laag (cm)",
      breaks = seq(0, 80, 20),
      limits = c(0, 80)
    ) +
    
    scale_y_continuous(
      name = "Gemiddelde indringingsweerstand (MPa)",
      breaks = seq(0, 1.5, 0.2),
      limits = c(0, 1.5)
    ) +
    
    # Thema en styling - consistent met tussenrapportage stijl
    theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 16, hjust = 0.5),
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 16),
      strip.text = element_text(size = 14, face = "bold"),
      legend.position = "bottom",
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16, face = "bold"),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
      strip.background = element_rect(colour = "black", fill = "white", linewidth = 0.8),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
      panel.grid.minor = element_blank()
    ) +
    
    # Kritieke waarde annotatie
    annotate("text", x = 5, y = kritieke_draagkracht + 0.05, 
             label = "Kritieke draagkracht", 
             hjust = 0, vjust = 0, size = 4, 
             color = "red", fontface = "bold") +
    
    # Titels en labels
    labs(
      title = "Relatie tussen Draagkracht en Dikte Veraarde Laag",
      subtitle = "Per veentype voor oever en perceel op verschillende dieptes\nRode lijn = kritieke draagkracht voor beweiding (0.5 MPa)",
      caption = paste0("Gebaseerd op ", nrow(pen_summary), " meettrajecten")
    ) +
    
    # Legenda aanpassingen
    guides(color = guide_legend(
      title = "Veentype:",
      nrow = 1,
      override.aes = list(size = 4, alpha = 1)
    ))

# Opslaan van de plot
ggsave(file = 'output/AlleGebieden/Tussenrapportage/draagkracht_veraarde_laag_scatter_diepte_sectie.png', 
         width = 45, height = 25, units = 'cm', dpi = 800)

# Print correlatie statistieken
cat("R² per sectie en diepte:\n")
print(r2_data)

#### draagkracht voor 1 gebied per slootid boxplots met facet jaar ------------------------------------
# Filter en bereid data voor
penmerge_sloot <- penmerge[
  !is.na(indringingsweerstand) & !is.na(Diept) & 
  Diept <= 50 &
  Gebiedsnaam == "Ronde Hoep"&
  WP %in% c("WP1", "WP2"),
]


ggplot(
  data = penmerge_sloot,
  aes(x = Behandeling, y = indringingsweerstand, fill = sectie_f)
) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed", linewidth = 0.8) +
  scale_fill_manual(
    values = c(
      "oever"   = okabe_ito_colors[3],
      "insteek" = okabe_ito_colors[2],
      "perceel" = okabe_ito_colors[6]
    ),
    name = "Sectie"
  ) +
  facet_grid(jaar ~ sloot, switch = "y", scales = "free") +
  scale_y_continuous(
    name = "Indringingsweerstand (MPa)",
    limits = c(0, 1.5)
  ) +
  labs(
    title = "Draagkracht per sloot en jaar - Ronde Hoep",
    x = "SlootID",
    caption = "Rode lijn = kritieke draagkracht (0.5 MPa)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(size = 11, face = "bold"),
    strip.background = element_rect(colour = "black", fill = "white"),
    panel.border = element_rect(colour = "black", fill = NA),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    legend.position = "bottom"
  )
### Samenstelling bodem op de oever-----------------------
#### Kleigehalte------------------------------
# Kleigehalte per slootcluster - uit abio_proj
# sort_order_clay <- abio_proj[!is.na(`Z_CLAY_SA_OR_25`), .(
#   mean_clay = mean(`Z_CLAY_SA_OR_25`, na.rm = TRUE)
# ), by = sloot_cluster][order(mean_clay)]

# Maak long format voor beide dieptes
clay_long <- melt(abio_proj[!is.na(sloot_cluster)],
                  id.vars = c("SlootID", "sloot_cluster", "jaar"),
                  measure.vars = c("Z_CLAY_SA_OR_25", "Z_CLAY_SA_OR_50"),
                  variable.name = "monsterdiepte",
                  value.name = "value")
clay_long[, monsterdiepte := fifelse(monsterdiepte == "Z_CLAY_SA_OR_25", "25 cm", "50 cm")]
clay_long[, sloot_cluster := factor(sloot_cluster, levels = sort_order_clay$sloot_cluster)]

ggplot(clay_long[!is.na(value)],
       aes(x = sloot_cluster, y = value, fill = monsterdiepte)) +
  facet_grid(. ~ sloot_cluster, scales = "free") +
  geom_boxplot(outliers = FALSE, width = 0.7) +
  theme_minimal() +
  theme(text = element_text(size = 14),
        axis.ticks = element_line(colour = "black"),
        axis.line = element_line(colour = 'black'),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        strip.text.y = element_text(size = 14),
        strip.text.x = element_blank(),
        panel.grid.major.x = element_line(color = "grey80", size = 0.5),
        panel.spacing.x = unit(0.2, "lines"),
        legend.position = "bottom"
  ) +
  scale_y_sqrt() +
  guides(fill = guide_legend(title = 'Monsterdiepte', title.vjust = 1)) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Slootcluster", y = "Kleigehalte (%)", fill = 'Monsterdiepte') +
  ggtitle('Kleigehalte oever per slootcluster - Ronde Hoep')

### Samenstelling water-----------------------
#### pH ------------------------------
# Data voorbereiden - filter alle pH data inclusief poriewater
ph_data <- abio_proj[!is.na(`pH_CC_OR_25`) | !is.na(`pH_CC_OR_50`) | !is.na(`pH_CC_SB`) | !is.na(`pH_PW`)]
# Herstructureer de data voor alle pH metingen met correctie voor pH CC
ph_melted <- rbindlist(list(
  data.table(
    Gebiedsnaam = ph_data$Gebiedsnaam,
    veentype = ph_data$veentype,
    compartiment_diepte = "Oever 25cm (CaCl2)",
    ph_value = ph_data$`pH_CC_OR_25` + 0.6  # Correctie naar water-equivalent
  ),
  data.table(
    Gebiedsnaam = ph_data$Gebiedsnaam,
    veentype = ph_data$veentype,
    compartiment_diepte = "Oever 50cm (CaCl2)",
    ph_value = ph_data$`pH_CC_OR_50` + 0.6  # Correctie naar water-equivalent
  ),
  data.table(
    Gebiedsnaam = ph_data$Gebiedsnaam,
    veentype = ph_data$veentype,
    compartiment_diepte = "Slib (CaCl2)",
    ph_value = ph_data$`pH_CC_SB` + 0.6  # Correctie naar water-equivalent
  ),
  # data.table(
  #   Gebiedsnaam = ph_data$Gebiedsnaam,
  #   veentype = ph_data$veentype,
  #   compartiment_diepte = "Slib in situ",
  #   ph_value = ph_data$slib_pH  # Geen correctie nodig voor poriewater pH
  # ),
  data.table(
    Gebiedsnaam = ph_data$Gebiedsnaam,
    veentype = ph_data$veentype,
    compartiment_diepte = "Water",
    ph_value = ph_data$`water_pH`  # Geen correctie nodig voor water pH
  )
))
ph_melted <- rbindlist(list(
  data.table(
    Gebiedsnaam = ph_data$sloot_cluster,
    veentype = ph_data$veentype,
    compartiment_diepte = "Oever 25cm (CaCl2)",
    ph_value = ph_data$`pH_CC_OR_25` + 0.6  # Correctie naar water-equivalent
  ),
  data.table(
    Gebiedsnaam = ph_data$sloot_cluster,
    veentype = ph_data$veentype,
    compartiment_diepte = "Oever 50cm (CaCl2)",
    ph_value = ph_data$`pH_CC_OR_50` + 0.6  # Correctie naar water-equivalent
  ),
  data.table(
    Gebiedsnaam = ph_data$sloot_cluster,
    veentype = ph_data$veentype,
    compartiment_diepte = "Slib (CaCl2)",
    ph_value = ph_data$`pH_CC_SB` + 0.6  # Correctie naar water-equivalent
  ),
  # data.table(
  #   Gebiedsnaam = ph_data$sloot_cluster,
  #   veentype = ph_data$veentype,
  #   compartiment_diepte = "Slib in situ",
  #   ph_value = ph_data$slib_pH  # Geen correctie nodig voor poriewater pH
  # ),
  data.table(
    Gebiedsnaam = ph_data$sloot_cluster,
    veentype = ph_data$veentype,
    compartiment_diepte = "Water",
    ph_value = ph_data$`water_pH`  # Geen correctie nodig voor water pH
  )
))

# Filter NA waarden
ph_melted <- ph_melted[!is.na(ph_value) & !is.na(Gebiedsnaam)]
# Bereken sorteervolgorde op basis van oever 25cm, dan poriewater, dan slib
sort_order <- ph_data[, .(
  oever_25_ph = mean(`pH_CC_OR_25` + 0.6, na.rm = TRUE),
  poriewater_ph = mean(pH_PW, na.rm = TRUE),
  slib_ph = mean(`slib_pH`, na.rm = TRUE),
  water_pH = mean(`water_pH`, na.rm = TRUE)
), by = Gebiedsnaam]
# Sorteer op oever 25cm, dan poriewater, dan slib
sort_order <- sort_order[order(oever_25_ph, poriewater_ph, slib_ph)]

# Bereken mediaan pH per facet (compartiment_diepte)
mediaan_per_facet <- ph_melted[, .(mediaan_ph = median(ph_value, na.rm = TRUE)), 
                               by = compartiment_diepte]

# Plot met boxplots zonder veentype fill
ggplot(ph_melted, aes(x = Gebiedsnaam, y = ph_value, fill = compartiment_diepte)) +
  # Boxplots zonder fill
  geom_boxplot(alpha = 0.8, outlier.size = 1) +
  # Mediaan pH per facet als horizontale stippellijn
  geom_hline(data = mediaan_per_facet,
             aes(yintercept = mediaan_ph), 
             color = "black", linetype = "dashed", size = 1) +
  # Neutrale pH (7.0) als groene referentielijn
  geom_hline(yintercept = 7.0, 
             color = "green", linetype = "dotted", size = 1) +
  facet_grid(. ~ compartiment_diepte, scales = "free_x")+
  scale_x_discrete(name = "Gebied") +
  coord_flip() +
  labs(title = "pH per gebied en compartiment",
       subtitle = "Zwarte stippellijn = mediaan pH\nGroene stippellijn = neutrale pH (7.0)\nCaCl2 metingen gecorrigeerd naar water-equivalent (+0.6)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = "none",
    strip.text = element_text(size = 14, face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    strip.placement = "outside"
  )

# Print mediaan waarden per facet voor controle
print(mediaan_per_facet)
#### Alle waarden naar water-equivalent pH ----------------------------
# Zorg dat pH CC waarden gecorrigeerd zijn
melt[parameter == "pH" & methode == "calciumchloride", value := value + 0.6]
melt[parameter == "pH" & methode == "calciumchloride", 
     methode := "CaCl2 extractie"]
melt[methode == "insitu", methode := "In situ meting"]
melt[methode == "Bware", methode := "labmeting"]
melt[methode == "icp Bware", methode := "labmeting"]
melt[methode == "lab meting", methode := "labmeting"]

melt[, compartiment := factor(compartiment, 
                                               levels = c("oever", "slib", "poriewater", "water"))]

ggplot(melt[parameter == "pH" & !is.na(Gebiedsnaam) & !is.na(compartiment),], 
       aes(x = methode , y = value, fill = compartiment)) +
  geom_boxplot(outlier.size = 1, alpha = 0.8) +
  # Neutrale pH (7.0) als groene referentielijn
  geom_hline(yintercept = 7.0, 
             color = "green", linetype = "dotted", size = 1) +
  # Facets per methode met zwarte lijnen
  facet_wrap(compartiment~., scales = "free_x", ncol = 4,
             strip.position = "bottom") +
  # Kleuren per compartiment
  scale_fill_manual(values = okabe_ito_colors[1:4],
                    name = "Compartiment: ") +
  # Y-as
  scale_y_continuous(
    name = "pH (water-equivalent)",
    limits = c(4, 8.5),
    breaks = seq(4, 8.5, 0.5)
  ) +
  # X-as label
  scale_x_discrete(name = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold"),
    strip.placement = "outside",
    # Zwarte lijnen om facets
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    strip.background = element_rect(colour = "black", fill = "white")
  ) +
  
  # Titel en ondertitel
  labs(title = "pH per Compartiment en Meetmethode",
       subtitle = "Groene stippellijn = neutrale pH (7.0)\nCaCl2 metingen gecorrigeerd naar water-equivalent (+0.6)")

# Print mediaan waarden per facet voor controle
print(mediaan_per_facet)


  

#### chlorofyl-A ---------------------------------
par <- unique(melt$parameter)[27]
ggplot(melt[parameter %in% par & !(methode %in% c('cohex'))& compartiment =="oppervlaktewater",], aes(x = Gebiedsnaam, y = value, fill = paste0(compartiment, ", ",methode))) +
    geom_boxplot(outliers = TRUE) +
    facet_wrap(~compartiment, scales = "free") +
    theme_minimal() +
    theme(text = element_text(size = 14),
          axis.ticks = element_line(colour = "black"),
          axis.line = element_line(colour = 'black'),
          axis.text.x = element_text(size = 14, angle = 45,hjust=1),
          axis.text.y = element_text(size = 14),
          strip.text = element_text(size = 14)  
    ) +
    scale_fill_manual(values = okabe_ito_colors) +
    labs(x = "gebied", y = paste0('-'), fill = 'compartiment') +
    ggtitle(paste0(unique(melt$parameter[melt$parameter %in% par] )))

#### P en N -----------------------------------

# Bereken de opbouw van N en P per gebied met juiste parameternamen
n_breakdown <- melt[parameter %in% c("NH4", "NO3", "TN") & 
                    compartiment == "water" & !is.na(Gebiedsnaam)]
n_breakdown <- n_breakdown[, .(mean_value = mean(value, na.rm = TRUE)), by = .(Gebiedsnaam, parameter)]
n_breakdown <- dcast(n_breakdown, Gebiedsnaam ~ parameter, value.var = "mean_value")
n_breakdown[, `:=`(
  N_overig = pmax(0, TN - NH4 - NO3, na.rm = TRUE),
  NH4 = fifelse(is.na(NH4), 0, NH4),
  NO3 = fifelse(is.na(NO3), 0, NO3)
)]
n_breakdown <- n_breakdown[, .(Gebiedsnaam, NH4, NO3, N_overig)]
n_breakdown <- melt(n_breakdown, id.vars = "Gebiedsnaam", measure.vars = c("NH4", "NO3", "N_overig"), 
                   variable.name = "fractie", value.name = "waarde")
n_breakdown[, type := "N"]

p_breakdown <- melt[parameter %in% c("P", "PO4") & 
                    compartiment == "water" & !is.na(Gebiedsnaam)]
p_breakdown <- p_breakdown[, .(mean_value = mean(value, na.rm = TRUE)), by = .(Gebiedsnaam, parameter)]
p_breakdown <- dcast(p_breakdown, Gebiedsnaam ~ parameter, value.var = "mean_value")
p_breakdown[, `:=`(
  P_overig = pmax(0, P - PO4, na.rm = TRUE),
  PO4 = fifelse(is.na(PO4), 0, PO4)
)]
p_breakdown <- p_breakdown[, .(Gebiedsnaam, PO4, P_overig)]
p_breakdown <- melt(p_breakdown, id.vars = "Gebiedsnaam", measure.vars = c("PO4", "P_overig"), 
                   variable.name = "fractie", value.name = "waarde")
p_breakdown[, type := "P"]
p_breakdown[, waarde := waarde * 16]  # Factor 16 voor P fracties

# Combineer de datasets
combined_breakdown <- rbind(n_breakdown, p_breakdown)

# Bereken totaal N en P met mean en sd voor errorbars
n_p_summary <- melt[parameter %in% c("TN", "P") & 
                    compartiment == "water" & !is.na(Gebiedsnaam)]
n_p_summary <- n_p_summary[, .(
  mean_value = mean(value, na.rm = TRUE),
  sd_value = sd(value, na.rm = TRUE)
), by = .(Gebiedsnaam, parameter)]

# Corrigeer P waarden met factor 16 voor vergelijking
n_p_summary[parameter == "P", `:=`(
  mean_value = mean_value * 16,
  sd_value = sd_value * 16
)]

# Sorteer gebieden op P totaal voor consistente volgorde

sort_order <- n_p_summary[parameter == "P"][order(mean_value), .(Gebiedsnaam)]
combined_breakdown[, Gebiedsnaam := factor(Gebiedsnaam, levels = sort_order$Gebiedsnaam)]
n_p_summary[, Gebiedsnaam := factor(Gebiedsnaam, levels = sort_order$Gebiedsnaam)]

# Maak de plot met gestapelde fracties
ggplot() +
  # N fracties gestapeld (rechts) - bredere balken
  geom_col(data = combined_breakdown[type == "N"], 
           aes(x = as.numeric(Gebiedsnaam) + 0.2, y = waarde, fill = fractie), 
           alpha = 0.8, width = 0.5, position = "stack") +
  
  # P fracties gestapeld (links) - bredere balken
  geom_col(data = combined_breakdown[type == "P"], 
           aes(x = as.numeric(Gebiedsnaam) - 0.2, y = waarde, fill = fractie), 
           alpha = 0.8, width = 0.5, position = "stack") +
  
  # Error bars voor N totaal
  geom_errorbar(data = n_p_summary[parameter == "TN"],
                aes(x = as.numeric(Gebiedsnaam) + 0.2, 
                    ymin = (mean_value - sd_value), 
                    ymax = (mean_value + sd_value)),
                width = 0.1, color = "black", size = 0.8) +
  
  # Error bars voor P totaal
  geom_errorbar(data = n_p_summary[parameter == "P"],
                aes(x = as.numeric(Gebiedsnaam) - 0.2, 
                    ymin = (mean_value - sd_value), 
                    ymax = (mean_value + sd_value)),
                width = 0.1, color = "black", size = 0.8) +
  
  # Groenige kleuren voor N en paarse kleuren voor P fracties
  scale_fill_manual(
    values = c(
      "NH4" = "#66C2A5",      # Licht groen voor NH4
      "NO3" = "#2CA02C",      # Middel groen voor NO3  
      "N_overig" = "#1B5E20", # Donker groen voor overige N
      "PO4" = "#9C88FF",      # Licht paars voor PO4
      "P_overig" = "#5E35B1"  # Donker paars voor overige P
    ),
    name = "Fractie",
    labels = c(
      "NH4" = "NH4-N",
      "NO3" = "NO3-N",
      "N_overig" = "Overig N",
      "PO4" = "PO4-P", 
      "P_overig" = "Overig P"
    )
  ) +
  
  # X-as met juiste gebiedsnaam labels
  scale_x_continuous(
    name = "Gebied",
    breaks = 1:length(sort_order$Gebiedsnaam),
    labels = sort_order$Gebiedsnaam
  ) +
  
  # Dubbele y-as met factor 16
  scale_y_continuous(
    name = "P concentratie (µmol/L)",
    sec.axis = sec_axis(~ . * 16, name = "N concentratie (µmol/l)")
  ) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    title = element_text(size = 16, face = "bold", hjust = 0.5),
    subtitle = element_text(size = 14, hjust = 0.5),
    panel.grid.major.x = element_blank()
  ) +
  labs(title = "N en P fracties in het oppervlaktewater per gebied",
       subtitle = "Links: P fracties, Rechts: N fracties (schaal ×16)\nError bars tonen standaarddeviatie") +
  guides(fill = guide_legend(nrow = 1))  # Legenda op één rij


#### N en P versie 2 met ratio subplot-------------------
# Bereken de opbouw van N en P per gebied met juiste parameternamen
n_breakdown <- melt[parameter %in% c("NH4", "NO3", "TN") & 
                    compartiment == "water" & !is.na(Gebiedsnaam)]
n_breakdown <- n_breakdown[, .(mean_value = mean(value, na.rm = TRUE)), by = .(Gebiedsnaam, parameter)]
n_breakdown <- dcast(n_breakdown, Gebiedsnaam ~ parameter, value.var = "mean_value")
n_breakdown[, `:=`(
  TN = TN,
  N_overig = pmax(0, TN - NH4 - NO3, na.rm = TRUE),
  NH4 = fifelse(is.na(NH4), 0, NH4),
  NO3 = fifelse(is.na(NO3), 0, NO3)
)]
n_breakdown <- n_breakdown[, .(Gebiedsnaam, NH4, NO3, N_overig, TN)]
n_breakdown_long <- melt(n_breakdown, id.vars = c("Gebiedsnaam"), measure.vars = c("NH4", "NO3", "N_overig", "TN"), 
                         variable.name = "fractie", value.name = "waarde")
n_breakdown_long[, type := "N"]

p_breakdown <- melt[parameter %in% c("P", "PO4") & 
                    compartiment == "water" & !is.na(Gebiedsnaam)]
p_breakdown <- p_breakdown[, .(mean_value = mean(value, na.rm = TRUE)), by = .(Gebiedsnaam, parameter)]
p_breakdown <- dcast(p_breakdown, Gebiedsnaam ~ parameter, value.var = "mean_value")
p_breakdown[, `:=`(
  TP = P,
  P_overig = pmax(0, P - PO4, na.rm = TRUE),
  PO4 = fifelse(is.na(PO4), 0, PO4)
)]
p_breakdown <- p_breakdown[, .(Gebiedsnaam, PO4, P_overig, TP)]
p_breakdown_long <- melt(p_breakdown, id.vars = c("Gebiedsnaam"), measure.vars = c("PO4", "P_overig", "TP"), 
                         variable.name = "fractie", value.name = "waarde")
p_breakdown_long[, type := "P"]
# Omrekenen van µmol/L naar mg/L
n_breakdown_long[, waarde_mg_l := waarde * 14.007 / 1000]  # µmol N/L naar mg N/L
p_breakdown_long[, waarde_mg_l := waarde * 30.974 / 1000]  # µmol P/L naar mg P/L
# Combineer de datasets
combined_breakdown <- rbind(
  n_breakdown_long[, .(Gebiedsnaam, fractie, waarde_mg_l, type)], 
  p_breakdown_long[, .(Gebiedsnaam, fractie, waarde_mg_l, type)]
)
# Haal de originele TN en TP metingen op
n_origineel <- melt[parameter == "TN" & compartiment == "water" & !is.na(Gebiedsnaam)]
n_origineel[, waarde_mg_l := value * 14.007 / 1000]  # Omrekenen naar mg/L

p_origineel <- melt[parameter == "P" & compartiment == "water" & !is.na(Gebiedsnaam)]  
p_origineel[, waarde_mg_l := value * 30.974 / 1000]  # Omrekenen naar mg/L

# Bereken whiskers voor N totaal op basis van alle individuele metingen
n_totaal_whiskers <- n_origineel[, .(
  mean_value = mean(waarde_mg_l, na.rm = TRUE),
  q25 = quantile(waarde_mg_l, 0.25, na.rm = TRUE),
  q75 = quantile(waarde_mg_l, 0.75, na.rm = TRUE),
  min_val = min(waarde_mg_l, na.rm = TRUE),
  max_val = max(waarde_mg_l, na.rm = TRUE),
  n_obs = .N  # Aantal metingen voor controle
), by = Gebiedsnaam]

n_totaal_whiskers[, `:=`(
  whisker_lower = pmax(min_val, q25 - 1.5 * (q75 - q25)),
  whisker_upper = pmin(max_val, q75 + 1.5 * (q75 - q25))
)]

# Hetzelfde voor P totaal
p_totaal_whiskers <- p_origineel[, .(
  mean_value_mg_l = mean(waarde_mg_l, na.rm = TRUE),
  q25 = quantile(waarde_mg_l, 0.25, na.rm = TRUE),
  q75 = quantile(waarde_mg_l, 0.75, na.rm = TRUE),
  min_val = min(waarde_mg_l, na.rm = TRUE),
  max_val = max(waarde_mg_l, na.rm = TRUE),
  n_obs = .N
), by = Gebiedsnaam]
p_totaal_whiskers[, `:=`(
  whisker_lower = pmax(min_val, q25 - 1.5 * (q75 - q25)),
  whisker_upper = pmin(max_val, q75 + 1.5 * (q75 - q25))
)]

np_totaal <- melt[parameter %in% c("TN", "P") & compartiment == "water" & !is.na(Gebiedsnaam)]
np_totaal <- dcast(np_totaal, Gebiedsnaam ~ parameter, value.var = "value", fun.aggregate = mean)
np_totaal[, NP_ratio_totaal := TN / P]  # Molaire ratio (µmol/µmol)

# Anorganisch N/P ratio (NH4 + NO3) / PO4
np_anorg <- melt[parameter %in% c("NH4", "NO3", "PO4") & compartiment == "water" & !is.na(Gebiedsnaam)]
np_anorg <- dcast(np_anorg, Gebiedsnaam ~ parameter, value.var = "value", fun.aggregate = mean)
np_anorg[, N_anorg := NH4 + NO3][, NP_ratio_anorg := N_anorg / PO4]

# Combineer ratio's
np_ratios <- merge(np_totaal[, .(Gebiedsnaam, NP_ratio_totaal)], 
                   np_anorg[, .(Gebiedsnaam, NP_ratio_anorg)], 
                   by = "Gebiedsnaam")

# Check resultaten
print("N/P Ratio's per gebied:")
print(np_ratios[order(Gebiedsnaam), .(Gebiedsnaam, 
                     `Totaal N/P` = round(NP_ratio_totaal, 1), 
                     `Anorganisch N/P` = round(NP_ratio_anorg, 1))])

# Voeg toe aan combined_breakdown voor plotting
combined_breakdown <- merge(combined_breakdown, np_ratios, by = "Gebiedsnaam", all.x = TRUE)
# Sorteer gebieden op P totaal voor consistente volgorde
sort_order <- p_totaal_whiskers[order(mean_value_mg_l), .(Gebiedsnaam)]
combined_breakdown[, Gebiedsnaam := factor(Gebiedsnaam, levels = sort_order$Gebiedsnaam)]
n_totaal_whiskers[, Gebiedsnaam := factor(Gebiedsnaam, levels = sort_order$Gebiedsnaam)]
p_totaal_whiskers[, Gebiedsnaam := factor(Gebiedsnaam, levels = sort_order$Gebiedsnaam)]

# Plot 1: N en P fracties - SQUARE ROOT TRANSFORMATIE
p1 <- ggplot() +
  # N fracties gestapeld (rechts) - groen
  geom_col(data = combined_breakdown[type == "N" & fractie != "TN",], 
           aes(x = as.numeric(Gebiedsnaam) + 0.2, y = waarde_mg_l/7, fill = fractie), 
           alpha = 0.8, width = 0.4, position = "stack") +
  
  # P fracties gestapeld (links) - paars - GESCHAALD MET FACTOR 7 
  geom_col(data = combined_breakdown[type == "P" & fractie != "TP",], 
           aes(x = as.numeric(Gebiedsnaam) - 0.2, y = waarde_mg_l , fill = fractie), 
           alpha = 0.8, width = 0.4, position = "stack") +
  
  # Whisker errorbars voor TOTAAL N (rechts)
  geom_errorbar(data = n_totaal_whiskers,
                aes(x = as.numeric(Gebiedsnaam) + 0.2, 
                    ymin = whisker_lower/7, 
                    ymax = whisker_upper/7),
                width = 0.2, color = "black", size = 0.8) +
  
  # Whisker errorbars voor TOTAAL P (links) (geschaald met factor 7)
  geom_errorbar(data = p_totaal_whiskers,
                aes(x = as.numeric(Gebiedsnaam) - 0.2, 
                    ymin = whisker_lower , 
                    ymax = whisker_upper ),
                width = 0.2, color = "black", size = 0.8) +
  
  # VOLLEDIGE KLEUREN MAPPING VOOR ALLE FRACTIES
  scale_fill_manual(
    values = c(
      "NH4" = "#66C2A5",      # Licht groen voor NH4
      "NO3" = "#2CA02C",      # Middel groen voor NO3  
      "N_overig" = "#1B5E20", # Donker groen voor overige N
      "PO4" = "#9C88FF",      # Licht paars voor PO4
      "P_overig" = "#5E35B1"  # Donker paars voor overige P
    ),
    name = "Fractie",
    labels = c(
      "NH4" = "NH4-N",
      "NO3" = "NO3-N",
      "N_overig" = "Overig N",
      "PO4" = "PO4-P", 
      "P_overig" = "Overig P"
    )
  ) +
  # X-as met juiste gebiedsnaam labels
  scale_x_continuous(
    name = "Gebied",
    breaks = 1:length(sort_order$Gebiedsnaam),
    labels = sort_order$Gebiedsnaam,
    expand = expansion(mult = c(0.02, 0.02))  # Meer ruimte aan de randen
  ) +
  # SQUARE ROOT TRANSFORMATIE voor Y-as
  scale_y_continuous(
    name = "mg P/l",
    sec.axis = sec_axis(~ .*7, name = "mgN/l")
  ) +
  # Flip coördinaten
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "none",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    title = element_text(size = 16, face = "bold", hjust = 0.5),
    subtitle = element_text(size = 14, hjust = 0.5),
    panel.grid.major.y = element_line(color = "grey90", size = 0.5),
    panel.grid.minor.y = element_line(color = "grey95", size = 0.3),
  ) +
  labs(title = "N en P fracties in oppervlaktewater",
       subtitle = "Boven: N fracties (groen), Onder: P fracties (paars, schaal ×7)\nErrorbars = whisker-range")

# Plot 2: N/P ratio's met SQUARE ROOT TRANSFORMATIE
p2 <- ggplot(combined_breakdown) +
  # Balken naast elkaar met position_dodge
  geom_col(aes(x = as.numeric(Gebiedsnaam) + 0.2, y = NP_ratio_totaal), fill ="#2C3E50", position = "dodge",width = 0.4, alpha = 0.8, width = 0.7) +
  geom_col(aes(x = as.numeric(Gebiedsnaam) - 0.2, y = NP_ratio_anorg), fill="#BDC3C7", position = "dodge", width = 0.4, alpha = 0.8, width = 0.7) +
  # Referentielijnen
  geom_hline(yintercept = 16, color = "purple", linetype = "dashed", size = 1.2) +
  # geom_hline(yintercept = 14, color = "darkgreen", linetype = "dashed", size = 1.2) +
  scale_y_continuous(
    name = "N/P Ratio (molbasis)") +
  scale_x_continuous(
     breaks = 1:length(sort_order$Gebiedsnaam),
    # MEER RUIMTE TUSSEN GEBIEDEN
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  # Flip coördinaten
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_blank(),
    axis.title = element_text(size = 14, color = "black"),
    axis.title.y = element_blank(),
    legend.position = "none",
    title = element_text(size = 16, face = "bold", hjust = 0.5, color = "black"),
    panel.grid.major.y = element_line(color = "grey90", size = 0.5),
    panel.grid.minor.y = element_line(color = "grey95", size = 0.3),
  ) +
  annotate("text", x = 19, y = 18, label = "16:1", 
           color = "purple", size = 4, fontface = "bold", hjust = 0) +
  labs(title = "N/P Ratio (molbasis)",
       x = NULL,
       y = "mol/mol") 

# Combineer plots
combined_np_plot_sqrt <- p1 + p2 + 
  plot_layout(ncol = 2, widths = c(2, 1)) +
  plot_annotation(
    title = "",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
    )
  )

# Voeg VOLLEDIGE gedeelde legenda toe
combined_breakdown[, fractie := factor(fractie, levels = c("NH4", "NO3", "N_overig", "PO4", "P_overig", "totaal-N/P", "anorg-N/P"))]
legend_plot <- ggplot() +
  # Voeg een dummy plot toe met ALLE fracties
  geom_col(data = data.frame(
    x = 1:7,
    y = 1:7,
    fractie = c("NH4", "NO3", "N_overig", "PO4", "P_overig", "totaal-N/P", "anorg-N/P")
  ), aes(x = x, y = y, fill = fractie)) +
  scale_fill_manual(
    values = c(
      "NH4" = "#66C2A5",      
      "NO3" = "#2CA02C",      
      "N_overig" = "#1B5E20", 
      "PO4" = "#9C88FF",      
      "P_overig" = "#5E35B1",
      "totaal-N/P" = "#2C3E50",
      "anorg-N/P" = "#BDC3C7"
    ),
    name = "Fractie:",
    breaks = c("NH4", "NO3", "N_overig", "PO4", "P_overig", "totaal-N/P", "anorg-N/P"), # Deze volgorde bepaalt legenda
    labels = c(
      "NH4" = "NH4-N",
      "NO3" = "NO3-N", 
      "N_overig" = "Overig N",
      "PO4" = "PO4-P",
      "P_overig" = "Overig P",
      "totaal-N/P" = "Totaal N/P",
      "anorg-N/P" = "Anorganisch N/P"
    )
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11))

# Combineer alles
final_plot_sqrt <- combined_np_plot_sqrt + 
  get_legend(legend_plot) + 
  plot_layout(heights = c(10, 1))

# Toon de plot
print(final_plot_sqrt)

# Print N/P ratio statistieken
cat("\nN/P Ratio's per gebied:\n")
np_summary <- np_ratios[order(Gebiedsnaam)]
print(np_summary[, .(Gebiedsnaam, 
                     `Totaal N/P` = round(NP_ratio_totaal, 1), 
                     `Anorganisch N/P` = round(NP_ratio_anorg, 1))])

# Opslaan
ggsave(file = 'output/AlleGebieden/Tussenrapportage/N_P_fracties_ratio_sqrt.png', 
       plot = final_plot_sqrt, width = 45, height = 25, units = 'cm', dpi = 800)


#### N en P versie 3 met ratio subplot - per gebied/ slootid-------------------
# Filter op Ronde Hoep
n_breakdown <- melt[parameter %in% c("NH4", "NO3", "TN") & 
                    compartiment == "water" & !is.na(sloot_cluster) &
                    Gebiedsnaam == "Ronde Hoep"]
n_breakdown <- n_breakdown[, .(mean_value = mean(value, na.rm = TRUE)), 
                            by = .(sloot_cluster, jaar, parameter)]
n_breakdown <- dcast(n_breakdown, sloot_cluster + jaar ~ parameter, value.var = "mean_value")
n_breakdown[, `:=`(
  N_overig = pmax(0, TN - NH4 - NO3, na.rm = TRUE),
  NH4 = fifelse(is.na(NH4), 0, NH4),
  NO3 = fifelse(is.na(NO3), 0, NO3)
)]
n_breakdown_long <- melt(n_breakdown[, .(sloot_cluster, jaar, NH4, NO3, N_overig, TN)],
                         id.vars = c("sloot_cluster", "jaar"),
                         measure.vars = c("NH4", "NO3", "N_overig", "TN"),
                         variable.name = "fractie", value.name = "waarde")
n_breakdown_long[, waarde_mg_l := waarde * 14.007 / 1000]
n_breakdown_long[, type := "N"]

p_breakdown <- melt[parameter %in% c("P", "PO4") & 
                    compartiment == "water" & !is.na(sloot_cluster) &
                    Gebiedsnaam == "Ronde Hoep"]
p_breakdown <- p_breakdown[, .(mean_value = mean(value, na.rm = TRUE)), 
                            by = .(sloot_cluster, jaar, parameter)]
p_breakdown <- dcast(p_breakdown, sloot_cluster + jaar ~ parameter, value.var = "mean_value")
p_breakdown[, `:=`(
  P_overig = pmax(0, P - PO4, na.rm = TRUE),
  PO4 = fifelse(is.na(PO4), 0, PO4)
)]
p_breakdown_long <- melt(p_breakdown[, .(sloot_cluster, jaar, PO4, P_overig, TP = P)],
                         id.vars = c("sloot_cluster", "jaar"),
                         measure.vars = c("PO4", "P_overig", "TP"),
                         variable.name = "fractie", value.name = "waarde")
p_breakdown_long[, waarde_mg_l := waarde * 30.974 / 1000]
p_breakdown_long[, type := "P"]

combined_breakdown <- rbind(
  n_breakdown_long[, .(sloot_cluster, jaar, fractie, waarde_mg_l, type)],
  p_breakdown_long[, .(sloot_cluster, jaar, fractie, waarde_mg_l, type)]
)

# Whiskers per sloot_cluster en jaar
n_origineel <- melt[parameter == "TN" & compartiment == "water" & 
                    !is.na(sloot_cluster) & Gebiedsnaam == "Ronde Hoep"]
n_origineel[, waarde_mg_l := value * 14.007 / 1000]
n_totaal_whiskers <- n_origineel[, .(
  mean_value = mean(waarde_mg_l, na.rm = TRUE),
  q25 = quantile(waarde_mg_l, 0.25, na.rm = TRUE),
  q75 = quantile(waarde_mg_l, 0.75, na.rm = TRUE),
  min_val = min(waarde_mg_l, na.rm = TRUE),
  max_val = max(waarde_mg_l, na.rm = TRUE)
), by = .(sloot_cluster, jaar)]
n_totaal_whiskers[, `:=`(
  whisker_lower = pmax(min_val, q25 - 1.5 * (q75 - q25)),
  whisker_upper = pmin(max_val, q75 + 1.5 * (q75 - q25))
)]

p_origineel <- melt[parameter == "P" & compartiment == "water" & 
                    !is.na(sloot_cluster) & Gebiedsnaam == "Ronde Hoep"]
p_origineel[, waarde_mg_l := value * 30.974 / 1000]
p_totaal_whiskers <- p_origineel[, .(
  mean_value_mg_l = mean(waarde_mg_l, na.rm = TRUE),
  q25 = quantile(waarde_mg_l, 0.25, na.rm = TRUE),
  q75 = quantile(waarde_mg_l, 0.75, na.rm = TRUE),
  min_val = min(waarde_mg_l, na.rm = TRUE),
  max_val = max(waarde_mg_l, na.rm = TRUE)
), by = .(sloot_cluster, jaar)]
p_totaal_whiskers[, `:=`(
  whisker_lower = pmax(min_val, q25 - 1.5 * (q75 - q25)),
  whisker_upper = pmin(max_val, q75 + 1.5 * (q75 - q25))
)]

# N/P ratios per sloot_cluster en jaar
np_totaal <- melt[parameter %in% c("TN", "P") & compartiment == "water" & 
                  !is.na(sloot_cluster) & Gebiedsnaam == "Ronde Hoep"]
np_totaal <- dcast(np_totaal, sloot_cluster + jaar ~ parameter, value.var = "value", fun.aggregate = mean)
np_totaal[, NP_ratio_totaal := TN / P]

np_anorg <- melt[parameter %in% c("NH4", "NO3", "PO4") & compartiment == "water" & 
                 !is.na(sloot_cluster) & Gebiedsnaam == "Ronde Hoep"]
np_anorg <- dcast(np_anorg, sloot_cluster + jaar ~ parameter, value.var = "value", fun.aggregate = mean)
np_anorg[, NP_ratio_anorg := (NH4 + NO3) / PO4]

np_ratios <- merge(np_totaal[, .(sloot_cluster, jaar, NP_ratio_totaal)],
                   np_anorg[, .(sloot_cluster, jaar, NP_ratio_anorg)],
                   by = c("sloot_cluster", "jaar"))

combined_breakdown <- merge(combined_breakdown, np_ratios, by = c("sloot_cluster", "jaar"), all.x = TRUE)

# Controleer
stopifnot(uniqueN(combined_breakdown$jaar) == 2)

# Natural sort op sloot_cluster
sloot_levels <- mixedsort(unique(as.character(combined_breakdown$sloot_cluster)))
combined_breakdown[, sloot_cluster := factor(sloot_cluster, levels = sloot_levels)]
n_totaal_whiskers[, sloot_cluster := factor(sloot_cluster, levels = sloot_levels)]
p_totaal_whiskers[, sloot_cluster := factor(sloot_cluster, levels = sloot_levels)]

# Plot 1: N en P fracties met jaar als facet
p1 <- ggplot() +
  geom_col(data = combined_breakdown[type == "N" & fractie != "TN"],
           aes(x = as.numeric(sloot_cluster) + 0.2, y = waarde_mg_l / 7, fill = fractie),
           alpha = 0.8, width = 0.4, position = "stack") +
  geom_col(data = combined_breakdown[type == "P" & fractie != "TP"],
           aes(x = as.numeric(sloot_cluster) - 0.2, y = waarde_mg_l, fill = fractie),
           alpha = 0.8, width = 0.4, position = "stack") +
  geom_errorbar(data = n_totaal_whiskers,
                aes(x = as.numeric(sloot_cluster) + 0.2,
                    ymin = whisker_lower / 7,
                    ymax = whisker_upper / 7),
                width = 0.2, color = "black", linewidth = 0.8) +
  geom_errorbar(data = p_totaal_whiskers,
                aes(x = as.numeric(sloot_cluster) - 0.2,
                    ymin = whisker_lower,
                    ymax = whisker_upper),
                width = 0.2, color = "black", linewidth = 0.8) +
  scale_fill_manual(
    values = c("NH4" = "#66C2A5", "NO3" = "#2CA02C", "N_overig" = "#1B5E20",
               "PO4" = "#9C88FF", "P_overig" = "#5E35B1"),
    name = "Fractie:",
    labels = c("NH4" = "NH4-N", "NO3" = "NO3-N", "N_overig" = "Overig N",
               "PO4" = "PO4-P", "P_overig" = "Overig P")
  ) +
  scale_x_continuous(
    name = "Slootcluster",
    breaks = seq_along(sloot_levels),
    labels = sloot_levels,
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  scale_y_continuous(
    name = "mg P/l",
    sec.axis = sec_axis(~ . * 7, name = "mg N/l")
  ) +
  facet_wrap(~ jaar, ncol = 1) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    legend.position = "none",  # legenda uit p1 halen
    strip.text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(colour = "black", fill = "white"),
    panel.border = element_rect(colour = "black", fill = NA)
  ) +
  labs(title = "N en P fracties per slootcluster - Ronde Hoep")

# Plot 2: N/P ratios met jaar als facet
np_plot_data <- unique(combined_breakdown[, .(sloot_cluster, jaar, NP_ratio_totaal, NP_ratio_anorg)])

p2 <- ggplot(np_plot_data) +
  geom_col(aes(x = as.numeric(sloot_cluster) + 0.2, y = NP_ratio_totaal,
               fill = "Totaal N/P"),
           width = 0.4, alpha = 0.8) +
  geom_col(aes(x = as.numeric(sloot_cluster) - 0.2, y = NP_ratio_anorg,
               fill = "Anorganisch N/P"),
           width = 0.4, alpha = 0.8) +
  scale_fill_manual(
    values = c("Totaal N/P" = "#2C3E50", "Anorganisch N/P" = "#BDC3C7"),
    name = "N/P ratio:"
  ) +
  geom_hline(yintercept = 16, color = "purple", linetype = "dashed", linewidth = 1.2) +
  annotate("text", x = length(sloot_levels) + 0.3, y = 18,
           label = "16:1", color = "purple", size = 3.5, fontface = "bold") +
  scale_x_continuous(
    breaks = seq_along(sloot_levels),
    labels = sloot_levels,
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  scale_y_continuous(name = "N/P Ratio (molbasis)") +
  facet_wrap(~ jaar, ncol = 1) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title = element_text(size = 13),
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(colour = "black", fill = "white"),
    panel.border = element_rect(colour = "black", fill = NA)
  ) +
  labs(title = "N/P Ratio")

# Legenda apart ophalen via dummy plot
legend_plot <- ggplot() +
  geom_col(data = data.frame(
    x = 1:7, y = 1,
    fractie = c("NH4", "NO3", "N_overig", "PO4", "P_overig", "Totaal N/P", "Anorganisch N/P")
  ), aes(x = x, y = y, fill = fractie)) +
  scale_fill_manual(
    values = c("NH4" = "#66C2A5", "NO3" = "#2CA02C", "N_overig" = "#1B5E20",
               "PO4" = "#9C88FF", "P_overig" = "#5E35B1",
               "Totaal N/P" = "#2C3E50", "Anorganisch N/P" = "#BDC3C7"),
    name = "Fractie / N/P ratio:",
    labels = c("NH4" = "NH4-N", "NO3" = "NO3-N", "N_overig" = "Overig N",
               "PO4" = "PO4-P", "P_overig" = "Overig P",
               "Totaal N/P" = "Totaal N/P", "Anorganisch N/P" = "Anorganisch N/P")
  ) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11))

# Combineer
final_np_plot <- (p1 + p2 + plot_layout(ncol = 2, widths = c(2, 1))) /
  get_legend(legend_plot) +
  plot_layout(heights = c(10, 1))

print(final_np_plot)


#### ir egv
# Laad LATframework en referencepoints
LATframework <- fread(paste0(workspace,"/hulp_tabellen/coordinates_LAT_framework.csv"))
referencepoints <- fread(paste0(workspace,"/hulp_tabellen/reference.points.csv"))

# Zet data in long format voor Ca en Cl
egv_long <- melt(
  abio_proj,
  id.vars = c("SlootID", "sloot_cluster"),
  measure.vars = list(
    Ca = c("Ca_µmol/l_OW", "Ca_µmol/l_PW"),
    Cl = c("Cl_µmol/l_OW", "Cl_µmol/l_PW"),
    EGV = c("EGV_µs/cm_OW", "EGV_µs/cm_PW")
  ),
  variable.name = "compartiment"
)

egv_long[, compartiment := fifelse(compartiment == 1, "OW", "PW")]
egv_long[, sloot_cluster := factor(sloot_cluster, levels = c("1", "2", "3", "4", "5", "6", "reservaat"))]
egv_long[, Ca_meq_l := Ca * 2 / 1000]
egv_long[, Cl_meq_l := Cl / 1000]
egv_long[, IR := Ca_meq_l / (Ca_meq_l + Cl_meq_l)]
egv_long <- egv_long[!is.na(IR) & !is.na(EGV) & !is.na(sloot_cluster)]

ggplot(egv_long, aes(x = EGV, y = IR, color = sloot_cluster, shape = compartiment)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_path(data = LATframework, aes(x = EC25 * 10, y = IR / 100), inherit.aes = FALSE, linetype = "dotdash", size = 0.7) +
  geom_text(data = referencepoints, aes(x = EC25 * 10, y = IR / 100, label = Name), inherit.aes = FALSE, size = 3) +
  scale_x_log10(name = "EGV (µS/cm)") +
  scale_y_continuous(name = "IR-ratio (Ca/(Ca+Cl))", limits = c(0, 1)) +
  scale_color_brewer(palette = "Dark2", name = "Slootcluster") +
  scale_shape_manual(values = c(16, 17), name = "Compartiment", labels = c("Oppervlaktewater", "Poriewater")) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 13),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13)
  ) +
  ggtitle("IR-EGV diagram per slootcluster (OW en PW)")

### PAL totaal en per gebied gesorteerd op gemiddelde waarde----------------------------------------------------------
melt_sel_fp <- melt[parameter%in%c("P2O5","P-AL","P-CC") & methode %in% c('xrf','pal','XRF','calciumchloride','CC'),]
melt_sel_fp <- melt_sel_fp[is.na(eenheid), eenheid := ""]
melt_sel_fp <- melt_sel_fp[!par_eenheid %in% 'P2O5_g/kg_xrf',]
# Voeg een kolom toe met de gewenste y-axis labels
melt_sel_fp[, y_axis_label := paste0(parameter, " (", eenheid, ")")]
ggplot(melt_sel_fp, aes(x = paste0(compartiment), y = value, fill = paste0(compartiment, ' ',monsterdiepte))) +
    facet_wrap(~y_axis_label, scales = "free",
               ncol = 3) +
    stat_boxplot(geom = 'errorbar') +
    geom_boxplot(outliers = FALSE) +
    theme_minimal() +
    theme(text = element_text(size = 14),
          axis.ticks =  element_line(colour = "black"),
          axis.line = element_line(colour='black'),
          axis.text.x = element_text(size = 14, angle = 45,hjust=1),
          strip.text = element_text(size = 12)  # Aanpassing voor langere labels
    ) +
    guides(fill = guide_legend(title = 'compartiment en monsterdiepte', title.vjust = 1))+
    scale_fill_brewer(palette = "Set2") +
    labs(x = "", y = "", fill = 'compartiment en monsterdiepte')
ggsave(file=paste0('output/fingerprints/P-AL_P-CC_P2O5','.png'), width = 25,height = 15,units='cm',dpi=800)

sort_order <- abio_proj[!is.na(`P-AL mg p2o5/100g_SB`), .(
  mean_pal = mean(`P-AL mg p2o5/100g_SB`, na.rm = TRUE)
), by = Gebiedsnaam][order(mean_pal)]
melt_sel_fp[, Gebiedsnaam := factor(Gebiedsnaam, levels = sort_order$Gebiedsnaam)]

ggplot(melt_sel_fp[parameter%in%c("P-AL") & eenheid =="mg P2O5/  100g" & !is.na(sloot_cluster),], aes(x = sloot_cluster, y = value, fill = paste0(compartiment, ' ',monsterdiepte))) +
    facet_grid(.~sloot_cluster, scales = "free") +
    # stat_boxplot(geom = 'errorbar', width = 1.2) +  # Bredere errorbars
    geom_boxplot(outliers = FALSE, width = 1.4) +   # Veel bredere boxplots
    theme_minimal() +
    theme(text = element_text(size = 14),
          axis.ticks =  element_line(colour = "black"),
          axis.line = element_line(colour='black'),
          axis.text.x = element_text(size = 14, angle = 45,hjust=1),
          strip.text = element_blank(),
          # VERTICALE HULPLIJNEN
          panel.grid.major.x = element_line(color = "grey80", size = 0.5),
          panel.grid.minor.x = element_line(color = "grey90", size = 0.3),
          panel.spacing.x = unit(0.2, "lines"),  # Minder ruimte tussen facets
          legend.position = "bottom"
    ) +
  scale_y_sqrt()+
    guides(fill = guide_legend(title = 'compartiment en monsterdiepte', title.vjust = 1))+
    scale_fill_brewer(palette = "Set2") +
    labs(x = "", y = "mg P2O5/  100g", fill = 'compartiment en monsterdiepte')+
    ggtitle(paste0('P-AL in verschillende gebieden'))

### NH4 per gebied gesorteerd op gemiddelde waarde-------------------------------------
melt_sel_fp <- melt[parameter%in%c("N-NH4","N-NO2","N-NO3") & methode %in% c('calciumchloride','CC'),]
melt_sel_fp <- melt_sel_fp[is.na(eenheid), eenheid := ""]

sort_order <- abio_proj[!is.na(abio_proj$`N-NH4_CC_mg/kg_SB`), .(
  mean_n = mean(`N-NH4_CC_mg/kg_SB`, na.rm = TRUE)
), by = Gebiedsnaam][order(mean_n)]
melt_sel_fp[, Gebiedsnaam := factor(Gebiedsnaam, levels = sort_order$Gebiedsnaam)]
ggplot(melt_sel_fp[parameter%in%c("N-NH4"),], aes(x = Gebiedsnaam, y = value, fill = paste0(compartiment, ' ',monsterdiepte))) +
    facet_grid(parameter~Gebiedsnaam, scales = "free") +
    # stat_boxplot(geom = 'errorbar', width = 1.2) +  # Bredere errorbars
    geom_boxplot(outliers = FALSE, width = 1.4) +   # Veel bredere boxplots
    theme_minimal() +
    theme(text = element_text(size = 14),
          axis.ticks =  element_line(colour = "black"),
          axis.line = element_line(colour='black'),
          axis.text.x = element_text(size = 14, angle = 45,hjust=1),
          strip.text.y = element_text(size = 14),
          strip.text.x = element_blank(),
          # VERTICALE HULPLIJNEN
          panel.grid.major.x = element_line(color = "grey80", size = 0.5),
          panel.grid.minor.x = element_line(color = "grey90", size = 0.3),
          panel.spacing.x = unit(0.2, "lines"),  # Minder ruimte tussen facets
          legend.position = "bottom"
    ) +
  scale_y_sqrt()+
    guides(fill = guide_legend(title = 'compartiment en monsterdiepte', title.vjust = 1))+
    scale_fill_brewer(palette = "Set2") +
    labs(x = "", y = "mg/kg", fill = 'compartiment en monsterdiepte')+
    ggtitle(paste0('Anorganisch stikstof in verschillende gebieden'))

sort_order <- abio_proj[!is.na(abio_proj$`N-NH4_CC_mg/kg_SB`), .(
  mean_n = mean(`N-NH4_CC_mg/kg_SB`, na.rm = TRUE)
), by = sloot_cluster][order(mean_n)]

melt_sel_fp[, sloot_cluster := factor(sloot_cluster, levels = sort_order$sloot_cluster)]

ggplot(melt_sel_fp[parameter %in% c("N-NH4") & !is.na(sloot_cluster),], 
       aes(x = sloot_cluster, y = value, fill = paste0(compartiment, ' ', monsterdiepte))) +
  facet_grid(parameter ~ sloot_cluster, scales = "free") +
  geom_boxplot(outliers = FALSE, width = 0.7) +
  theme_minimal() +
  theme(text = element_text(size = 14),
        axis.ticks = element_line(colour = "black"),
        axis.line = element_line(colour = 'black'),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        strip.text.y = element_text(size = 14),
        strip.text.x = element_blank(),
        panel.grid.major.x = element_line(color = "grey80", size = 0.5),
        panel.spacing.x = unit(0.2, "lines"),
        legend.position = "bottom"
  ) +
  scale_y_sqrt() +
  guides(fill = guide_legend(title = 'compartiment en monsterdiepte', title.vjust = 1)) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Slootcluster", y = "mg/kg", fill = 'compartiment en monsterdiepte') +
  ggtitle('Ammonium per slootcluster - Ronde Hoep')

# Selecteer de anorganische stikstof parameters voor CC methode
melt_sel_n <- melt[parameter %in% c("N-NH4", "N-NO2", "N-NO3") & 
                   methode %in% c('calciumchloride', 'CC') & 
                   !is.na(Gebiedsnaam)]
# Bereken de som van anorganische stikstof per SlootID, compartiment en monsterdiepte
n_anorg <- melt_sel_n[, .(
  sum_n_anorg = sum(value, na.rm = TRUE),
  n_parameters = .N
), by = .(SlootID, Gebiedsnaam, compartiment, monsterdiepte)]
# Sorteer gebieden op gemiddelde anorganische N waarde
sort_order <- n_anorg[, .(mean_n = mean(sum_n_anorg, na.rm = TRUE)), 
                      by = Gebiedsnaam][order(mean_n)]
n_anorg[, Gebiedsnaam := factor(Gebiedsnaam, levels = sort_order$Gebiedsnaam)]

ggplot(n_anorg, aes(x = Gebiedsnaam, y = sum_n_anorg, 
                    fill = paste0(compartiment, ' ', monsterdiepte))) +
  facet_grid(.~Gebiedsnaam, scales = "free") +
    # stat_boxplot(geom = 'errorbar', width = 1.2) +  # Bredere errorbars
    geom_boxplot(outliers = FALSE, width = 1.4) +   # Veel bredere boxplots
    theme_minimal() +
    theme(text = element_text(size = 14),
          axis.ticks =  element_line(colour = "black"),
          axis.line = element_line(colour='black'),
          axis.text.x = element_text(size = 14, angle = 45,hjust=1),
          strip.text.y = element_text(size = 14),
          strip.text.x = element_blank(),
          # VERTICALE HULPLIJNEN
          panel.grid.major.x = element_line(color = "grey80", size = 0.5),
          panel.grid.minor.x = element_line(color = "grey90", size = 0.3),
          panel.spacing.x = unit(0.2, "lines"),  # Minder ruimte tussen facets
          legend.position = "bottom"
    ) +
  scale_y_sqrt()+
    guides(fill = guide_legend(title = 'compartiment en monsterdiepte', title.vjust = 1))+
    scale_fill_brewer(palette = "Set2") +
  # Labels
  labs(
    title = "Som Anorganisch Stikstof (NH4 + NO2 + NO3) per gebied",
    subtitle = "CaCl2 extractie",
    x = "Gebied",
    y = "Anorganisch N (mg/kg)"
  ) +
  
  # Legenda aanpassingen
  guides(fill = guide_legend(
    nrow = 1,
    title = "Compartiment en diepte:"
  ))

# Opslaan
ggsave(file = 'output/AlleGebieden/Tussenrapportage/Anorganisch_N_som_boxplot.png', 
       width = 35, height = 25, units = 'cm', dpi = 800)

# Print statistieken
cat("Samenvatting anorganische N per gebied:\n")
print(n_anorg[, .(
  mediaan = median(sum_n_anorg, na.rm = TRUE),
  gemiddelde = round(mean(sum_n_anorg, na.rm = TRUE), 1),
  n_obs = .N
), by = .(Gebiedsnaam, compartiment)][order(Gebiedsnaam, compartiment)])

### chloride per gebied gesorteerd op gemiddelde waarde-------------------------------------

library(data.table)
library(ggplot2)

# Zorg dat sloot_cluster een factor is, maar zonder sortering op gemiddelde chloride
abio_proj[, sloot_cluster := factor(sloot_cluster, levels = unique(sloot_cluster))]
# Zet de gewenste volgorde als levels
sloot_cluster_levels <- c("1", "2", "3", "4", "5", "6", "reservaat")
# Pas toe op abio_proj en chloride_long (of je relevante data)
abio_proj[, sloot_cluster := factor(sloot_cluster, levels = sloot_cluster_levels)]
chloride_long[, sloot_cluster := factor(sloot_cluster, levels = sloot_cluster_levels)]
# Maak een long format voor beide chloride-variabelen
chloride_long <- melt(
  abio_proj,
  id.vars = c("sloot_cluster", "SlootID"),
  measure.vars = c("Cl_mg_l_OW", "Cl_mg_l_PW"),
  variable.name = "compartiment",
  value.name = "chloride"
)

# Optioneel: maak nette labels
chloride_long[, compartiment := fifelse(compartiment == "Cl_mg_l_OW", "Oppervlaktewater", "Poriewater")]
# Plot
ggplot(chloride_long[!is.na(chloride)], aes(x = sloot_cluster, y = chloride, fill = compartiment)) +
  geom_boxplot(outlier.shape = NA, width = 0.6, position = position_dodge(width = 0.7)) +
  scale_fill_manual(values = c("Oppervlaktewater" = "#E69F00", "Poriewater" = "#56B4E9")) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.ticks = element_line(colour = "black"),
    axis.line = element_line(colour = 'black'),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    strip.text.y = element_text(size = 14),
    strip.text.x = element_blank(),
    panel.grid.major.x = element_line(color = "grey80", size = 0.5),
    panel.spacing.x = unit(0.2, "lines"),
    legend.position = "bottom"
  ) +
  scale_y_sqrt() +
  labs(
    x = "Slootcluster",
    y = "Chloride (mg/l)",
    fill = "Compartiment"
  ) +
  ggtitle("Chloride per slootcluster (poriewater en oppervlaktewater)")

### Metalen en mineralen in slib en porievocht------------------------
sel <- c("SiO2","Al2O3","MgO","Zn","Pb","Cu","Ni","SO3", "CaO", "Fe2O3" )
melt_sel_fp <- melt[(parameter%in%sel & methode == 'xrf')|parameter%in%'organisch stof'|par_eenheid%in%'pH_CC_calciumchloride',]
  ggplot(melt_sel_fp, aes(x = sloot_cluster, y = value, fill = paste0(compartiment, ' ',monsterdiepte))) +
    facet_wrap(~parameter+eenheid, scales = "free",
               ncol = 3) +
    stat_boxplot(geom = 'errorbar') +
    geom_boxplot(outliers = FALSE) +
    theme_minimal() +
    theme(text = element_text(size = 15),
          axis.ticks =  element_line(colour = "black"),
          axis.line = element_line(colour='black')
    ) +
    scale_fill_brewer(palette = "Set2") +
    labs(x = "slootcluster", y = "", fill = 'compartiment en monsterdiepte')
  ggsave(file=paste0('output/fingerprints/metalen_mineralen_os','.png'), width = 25,height = 15,units='cm',dpi=800)
  
  melt_sel_fp <- melt[(parameter%in%sel5 & methode == 'xrf')|par_eenheid%in%"pH_CC_calciumchloride",]
  ggplot(melt_sel_fp, aes(x = compartiment, y = value, fill = paste0(compartiment, ' ',monsterdiepte))) +
    facet_wrap(~parameter+eenheid, scales = "free",
               ncol = 3) +
    stat_boxplot(geom = 'errorbar') +
    geom_boxplot(outliers = FALSE) +
    theme_minimal() +
    theme(text = element_text(size = 14),
          axis.ticks =  element_line(colour = "black"),
          axis.line = element_line(colour='black')
    ) +
    scale_fill_brewer(palette = "Set2") +
    labs(x = "compartiment", y = "", fill = 'compartiment en monsterdiepte')
  ggsave(file=paste0('output/fingerprints/metalen','.png'), width = 25,height = 15,units='cm',dpi=800)




### Redox------------------------
# Bereken whisker-range voor redox uit ABIO_PROJ
redox_summary <- abio_proj[!is.na(Gebiedsnaam) & !is.na(slib_redox_pH7), .(
  median_redox = median(slib_redox_pH7, na.rm = TRUE),
  sd_redox = sd(slib_redox_pH7, na.rm = TRUE),
  
  # Bereken quartiles en min/max voor whisker-range
  q25_redox = quantile(slib_redox_pH7, 0.25, na.rm = TRUE),
  q75_redox = quantile(slib_redox_pH7, 0.75, na.rm = TRUE),
  min_redox = min(slib_redox_pH7, na.rm = TRUE),
  max_redox = max(slib_redox_pH7, na.rm = TRUE)
), by = Gebiedsnaam]

# STAP 1: Bereken eerst alleen de IQR
redox_summary[, iqr_redox := q75_redox - q25_redox]

# STAP 2: Bereken nu de whisker-range met de al bestaande iqr_redox kolom
redox_summary[, `:=`(
  whisker_lower = pmax(min_redox, q25_redox - 1.5 * iqr_redox),
  whisker_upper = pmin(max_redox, q75_redox + 1.5 * iqr_redox)
)]

sort_order <- redox_summary[order(mean_redox)]
redox_summary[, Gebiedsnaam := factor(Gebiedsnaam, levels = sort_order$Gebiedsnaam)]

# Verbeterde rect kleuren met meer contrast en juiste volgorde
rects <- data.frame(xmin = -Inf, 
                    xmax = Inf,
                    ymin = c(-Inf,-250,-100,0,200,300),  
                    ymax = c(-250,-100,0,200,300, 800),
                    fill = c("#8B0000", "#FF4500", "#FFB347", "#d0ff00ff", "#4169E1","#062992ff"),
                    label = c("methonogenese", "sulfaatreductie","ijzeroxidereductie","mangaanreductie","denitrificatie","zuurstofreductie"))

# Legenda kleuren in OMGEKEERDE volgorde (van hoog naar laag redoxpotentiaal)
legend_colors <- setNames(c("#062992ff", "#4169E1", "#d0ff00ff", "#FFB347", "#FF4500", "#8B0000"), 
                         c("oxisch", "nitraatreductie", "ijzeroxidereductie", "sulfaatreductie", "methonogenese"))

ggplot() +
  geom_rect(data = rects, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), 
            inherit.aes = FALSE, alpha = 0.25) +
  scale_fill_identity('Redoxtoestand:', 
                      breaks = legend_colors, 
                      labels = c("zuurstofreductie", "denitrifictaie", "mangaanreductie","ijzeroxidereductie", "sulfaatreductie", "methonogenese"), 
                      guide = guide_legend(override.aes = list(alpha = 0.25))) +
  geom_col(data = redox_summary,
           aes(x = Gebiedsnaam, y = median_redox), 
           fill = "#7570B3", alpha = 0.7) +
  geom_errorbar(data = redox_summary,
                aes(x = Gebiedsnaam, 
                    ymin = whisker_lower, 
                    ymax = whisker_upper),
                width = 0.2, color = "black", size = 0.8) +
  geom_point(data = melt[variable %in% c("slib_redox_pH7") & !is.na(Gebiedsnaam),], 
             aes(x = Gebiedsnaam, y = `gemiddelde VeeST`, col = '* gemiddelde VeeST'),
             shape = 95, size = 10) +
  scale_colour_manual(values = c('grey2')) +
  coord_flip() +
  theme_minimal(base_size = 15) +
  theme(
    strip.background = element_blank(),
    strip.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 14, vjust = 0.5, hjust = 1, angle = 90),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.ticks = element_line(colour = "black"),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    plot.background = element_blank(),
    legend.position = "right",
    legend.box.just = "center"
  ) +
  guides(col = guide_legend(title = ''), fill = guide_legend(title = 'Redoxtoestand')) +
  ggtitle(paste0("Redoxpotentiaal in slib")) +
  labs(x = 'Gebied', y = 'mV (bij pH7)')

ggsave(file = paste0('output/AlleGebieden/Tussenrapportage/redox_summary.png'), 
       width = 25, height = 15, units = 'cm', dpi = 800)

### Redox slib en water ------------------------
# Bereken whisker-range voor redox in SLIB uit ABIO_PROJ
redox_slib_summary <- abio_proj[!is.na(Gebiedsnaam) & !is.na(slib_redox_pH7), .(
  median_redox = median(slib_redox_pH7, na.rm = TRUE),
  sd_redox = sd(slib_redox_pH7, na.rm = TRUE),
  
  # Bereken quartiles en min/max voor whisker-range
  q25_redox = quantile(slib_redox_pH7, 0.25, na.rm = TRUE),
  q75_redox = quantile(slib_redox_pH7, 0.75, na.rm = TRUE),
  min_redox = min(slib_redox_pH7, na.rm = TRUE),
  max_redox = max(slib_redox_pH7, na.rm = TRUE)
), by = Gebiedsnaam]
# STAP 1: Bereken eerst alleen de IQR voor slib
redox_slib_summary[, iqr_redox := q75_redox - q25_redox]
# STAP 2: Bereken nu de whisker-range met de al bestaande iqr_redox kolom voor slib
redox_slib_summary[, `:=`(
  whisker_lower = pmax(min_redox, q25_redox - 1.5 * iqr_redox),
  whisker_upper = pmin(max_redox, q75_redox + 1.5 * iqr_redox),
  compartiment = "Slib"
)]
# Bereken whisker-range voor redox in WATER uit ABIO_PROJ
redox_water_summary <- abio_proj[!is.na(Gebiedsnaam) & !is.na(water_redox_pH7), .(
  median_redox = median(water_redox_pH7, na.rm = TRUE),
  sd_redox = sd(water_redox_pH7, na.rm = TRUE),
  
  # Bereken quartiles en min/max voor whisker-range
  q25_redox = quantile(water_redox_pH7, 0.25, na.rm = TRUE),
  q75_redox = quantile(water_redox_pH7, 0.75, na.rm = TRUE),
  min_redox = min(water_redox_pH7, na.rm = TRUE),
  max_redox = max(water_redox_pH7, na.rm = TRUE)
), by = Gebiedsnaam]
# STAP 1: Bereken eerst alleen de IQR voor water
redox_water_summary[, iqr_redox := q75_redox - q25_redox]

# STAP 2: Bereken nu de whisker-range met de al bestaande iqr_redox kolom voor water
redox_water_summary[, `:=`(
  whisker_lower = pmax(min_redox, q25_redox - 1.5 * iqr_redox),
  whisker_upper = pmin(max_redox, q75_redox + 1.5 * iqr_redox),
  compartiment = "Water"
)]

# Combineer beide datasets
redox_combined <- rbind(redox_slib_summary, redox_water_summary)

# Sorteer op mediaan slib redox voor consistente volgorde
sort_order <- redox_slib_summary[order(median_redox)]
redox_combined[, Gebiedsnaam := factor(Gebiedsnaam, levels = sort_order$Gebiedsnaam)]

# Verbeterde rect kleuren met meer contrast en juiste volgorde
rects <- data.frame(xmin = -Inf, 
                    xmax = Inf,
                    ymin = c(-Inf,-250,-100,0,200,300),  
                    ymax = c(-250,-100,0,200,300, 800),
                    fill = c("#8B0000", "#FF4500", "#FFB347", "#d0ff00ff", "#4169E1","#062992ff"),
                    label = c("methonogenese", "sulfaatreductie","ijzeroxidereductie","mangaanreductie","denitrificatie","zuurstofreductie"))

# Legenda kleuren in OMGEKEERDE volgorde (van hoog naar laag redoxpotentiaal)
legend_colors <- setNames(c("#062992ff", "#4169E1", "#d0ff00ff", "#FFB347", "#FF4500", "#8B0000"), 
                         c("oxisch", "nitraatreductie", "mangaanreductie", "ijzeroxidereductie", "sulfaatreductie", "methonogenese"))

ggplot() +
  geom_rect(data = rects, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), 
            inherit.aes = FALSE, alpha = 0.25) +
  scale_fill_identity('Redoxtoestand:', 
                      breaks = legend_colors, 
                      labels = c("Zuurstofreductie", "Denitrificatie", "Mangaanreductie","IJzeroxide reductie", "Sulfaatreductie", "Methonogenese"), 
                      guide = guide_legend(override.aes = list(alpha = 0.25))) +
  
  # Mediaan balken (horizontaal omdat coord_flip)
  geom_col(data = redox_combined,
           aes(x = Gebiedsnaam, y = median_redox), 
           fill = "#7570B3", alpha = 0.7) +
  
  # Whisker-range errorbars (zoals boxplot whiskers)
  geom_errorbar(data = redox_combined,
                aes(x = Gebiedsnaam, 
                    ymin = whisker_lower, 
                    ymax = whisker_upper),
                width = 0.2, color = "black", size = 0.8) +
  
  # VeeST gemiddelde als punt - verschillende voor slib en water
  geom_point(data = melt[variable %in% c("slib_redox_pH7") & !is.na(Gebiedsnaam),], 
             aes(x = Gebiedsnaam, y = `gemiddelde VeeST`, col = '* gemiddelde VeeST'),
             shape = 95, size = 10) +
  
  geom_point(data = melt[variable %in% c("water_redox_pH7") & !is.na(Gebiedsnaam),], 
             aes(x = Gebiedsnaam, y = `gemiddelde VeeST`, col = '* gemiddelde VeeST'),
             shape = 95, size = 10) +
  
  scale_colour_manual(values = c('grey2')) +
  
  # Facet per compartiment
  facet_wrap(~ compartiment, scales = "free_x", ncol = 2) +
  
  # FLIP DE ASSEN
  coord_flip() +
  
  theme_minimal(base_size = 15) +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 14),  # Nu horizontale as
    axis.text.y = element_text(size = 14),  # Nu verticale as  
    axis.title = element_text(size = 14),
    axis.ticks = element_line(colour = "black"),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    plot.background = element_blank(),
    legend.position = "right",
    legend.box.just = "center",
    strip.background = element_rect(colour = "black", fill = "white", linewidth = 0.8)
  ) +
  guides(col = guide_legend(title = ''), fill = guide_legend(title = 'Redoxtoestand')) +
  
  labs(
    title = "Redoxpotentiaal in slib en water per gebied",
    subtitle = "Errorbars tonen whisker-range (Q1-1.5×IQR tot Q3+1.5×IQR)",
    x = 'Gebied', 
    y = 'mV (bij pH7)'
  )

ggsave(file = 'output/AlleGebieden/Tussenrapportage/redox_slib_water_whisker_range.png', 
       width = 35, height = 20, units = 'cm', dpi = 800)
### Redox en VeeST per slootID (1 gebied) ------------------------
# Bereken whisker-range voor redox in WATER uit ABIO_PROJ
redox_water_summary <- abio_proj[!is.na(SlootID_kort) & !is.na(water_redox_pH7), .(
  median_redox = median(water_redox_pH7, na.rm = TRUE),
  sd_redox = sd(water_redox_pH7, na.rm = TRUE),
  # Bereken quartiles en min/max voor whisker-range
  q25_redox = quantile(water_redox_pH7, 0.25, na.rm = TRUE),
  q75_redox = quantile(water_redox_pH7, 0.75, na.rm = TRUE),
  min_redox = min(water_redox_pH7, na.rm = TRUE),
  max_redox = max(water_redox_pH7, na.rm = TRUE)
), by = .(SlootID_kort, jaar)]
# STAP 1: Bereken eerst alleen de IQR voor water
redox_water_summary[, iqr_redox := q75_redox - q25_redox]
# STAP 2: Bereken nu de whisker-range met de al bestaande iqr_redox kolom voor water
redox_water_summary[, `:=`(
  whisker_lower = pmax(min_redox, q25_redox - 1.5 * iqr_redox),
  whisker_upper = pmin(max_redox, q75_redox + 1.5 * iqr_redox),
  compartiment = "Water"
)]

redox_slib_summary <- abio_proj[!is.na(slib_redox_pH7) & !is.na(SlootID_kort), .(
  median_redox = median(slib_redox_pH7, na.rm = TRUE),
  q25_redox = quantile(slib_redox_pH7, 0.25, na.rm = TRUE),
  q75_redox = quantile(slib_redox_pH7, 0.75, na.rm = TRUE),
  min_redox = min(slib_redox_pH7, na.rm = TRUE),
  max_redox = max(slib_redox_pH7, na.rm = TRUE)
), by = .(SlootID_kort, jaar)]
redox_slib_summary[, iqr_redox := q75_redox - q25_redox]
redox_slib_summary[, `:=`(
  whisker_lower = pmax(min_redox, q25_redox - 1.5 * iqr_redox),
  whisker_upper = pmin(max_redox, q75_redox + 1.5 * iqr_redox),
  compartiment = "Slib"
)]

redox_combined <- rbind(redox_slib_summary, redox_water_summary, fill = TRUE)

rects <- data.frame(
  xmin = -Inf, 
  xmax = Inf,
  ymin = c(-Inf, -250, -100, 0,   200, 300),
  ymax = c(-250, -100,    0, 200, 300, 800),
  fill = c("#8B0000", "#FF4500", "#FFB347", "#d0ff00ff", "#4169E1", "#062992ff")
)
legend_colors <- setNames(
  c("#062992ff", "#4169E1", "#d0ff00ff", "#FFB347", "#FF4500", "#8B0000"),
  c("zuurstofreductie", "denitrificatie", "mangaanreductie", "ijzeroxidereductie", "sulfaatreductie", "methanogenese")
)

# Zorg dat beide compartimenten voor elke SlootID aanwezig zijn (vul NA in waar ontbreekt)
alle_combos <- CJ(
  SlootID_kort = mixedsort(unique(redox_combined$SlootID_kort)),
  jaar = unique(redox_combined$jaar),
  compartiment = unique(redox_combined$compartiment)
)
redox_plot <- merge(alle_combos, redox_combined, by = c("SlootID_kort", "jaar", "compartiment"), all.x = TRUE)
redox_plot[, SlootID_kort := factor(SlootID_kort, levels = mixedsort(unique(as.character(SlootID_kort))))]

ggplot(redox_plot[!is.na(SlootID_kort),],
       aes(x = SlootID_kort, y = median_redox, fill = compartiment)) +
  geom_rect(data = rects, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
            inherit.aes = FALSE, alpha = 0.25) +
  scale_fill_identity('Redoxtoestand:',
                      breaks = legend_colors,
                      labels = c("Zuurstofreductie", "Denitrificatie", "Mangaanreductie",
                                 "IJzeroxide reductie", "Sulfaatreductie", "Methanogenese"),
                      guide = guide_legend(override.aes = list(alpha = 0.25))) +
  ggnewscale::new_scale_fill() +
  geom_col(aes(fill = compartiment),
           position = position_dodge(width = 0.9),
           alpha = 0.8, width = 0.9, na.rm = TRUE) +
  geom_errorbar(aes(ymin = whisker_lower, ymax = whisker_upper, group = compartiment),
                position = position_dodge(width = 0.9),
                width = 0.3, color = "black", linewidth = 0.8, na.rm = TRUE) +
  scale_fill_manual(values = c("Slib" = "#8B4513", "Water" = "#56B4E9"), name = "Compartiment") +
  facet_wrap(~ jaar, ncol = 1) +
  coord_flip() +
  theme_minimal(base_size = 15) +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    strip.background = element_rect(colour = "black", fill = "white", linewidth = 0.8),
    legend.position = "right"
  ) +
  labs(
    title = "Redoxpotentiaal in slib en water per sloot en jaar",
    subtitle = "Errorbars tonen whisker-range (Q1-1.5×IQR tot Q3+1.5×IQR)",
    x = "SlootID", y = "mV (bij pH7)"
  )


### Redox en VeeST per behandeling (1 gebied) ------------------------
# Bereken whisker-range voor redox in WATER uit ABIO_PROJ
redox_water_summary <- abio_proj[!is.na(Behandeling) & !is.na(water_redox_pH7), .(
  median_redox = median(water_redox_pH7, na.rm = TRUE),
  sd_redox = sd(water_redox_pH7, na.rm = TRUE),
  # Bereken quartiles en min/max voor whisker-range
  q25_redox = quantile(water_redox_pH7, 0.25, na.rm = TRUE),
  q75_redox = quantile(water_redox_pH7, 0.75, na.rm = TRUE),
  min_redox = min(water_redox_pH7, na.rm = TRUE),
  max_redox = max(water_redox_pH7, na.rm = TRUE)
), by = .(Behandeling, jaar)]
# STAP 1: Bereken eerst alleen de IQR voor water
redox_water_summary[, iqr_redox := q75_redox - q25_redox]
# STAP 2: Bereken nu de whisker-range met de al bestaande iqr_redox kolom voor water
redox_water_summary[, `:=`(
  whisker_lower = pmax(min_redox, q25_redox - 1.5 * iqr_redox),
  whisker_upper = pmin(max_redox, q75_redox + 1.5 * iqr_redox),
  compartiment = "Water"
)]

redox_slib_summary <- abio_proj[!is.na(slib_redox_pH7) & !is.na(Behandeling), .(
  median_redox = median(slib_redox_pH7, na.rm = TRUE),
  q25_redox = quantile(slib_redox_pH7, 0.25, na.rm = TRUE),
  q75_redox = quantile(slib_redox_pH7, 0.75, na.rm = TRUE),
  min_redox = min(slib_redox_pH7, na.rm = TRUE),
  max_redox = max(slib_redox_pH7, na.rm = TRUE)
), by = .(Behandeling, jaar)]
redox_slib_summary[, iqr_redox := q75_redox - q25_redox]
redox_slib_summary[, `:=`(
  whisker_lower = pmax(min_redox, q25_redox - 1.5 * iqr_redox),
  whisker_upper = pmin(max_redox, q75_redox + 1.5 * iqr_redox),
  compartiment = "Slib"
)]

redox_combined <- rbind(redox_slib_summary, redox_water_summary, fill = TRUE)

rects <- data.frame(
  xmin = -Inf, 
  xmax = Inf,
  ymin = c(-Inf, -250, -100, 0,   200, 300),
  ymax = c(-250, -100,    0, 200, 300, 800),
  fill = c("#8B0000", "#FF4500", "#FFB347", "#d0ff00ff", "#4169E1", "#062992ff")
)
legend_colors <- setNames(
  c("#062992ff", "#4169E1", "#d0ff00ff", "#FFB347", "#FF4500", "#8B0000"),
  c("zuurstofreductie", "denitrificatie", "mangaanreductie", "ijzeroxidereductie", "sulfaatreductie", "methanogenese")
)

# Zorg dat beide compartimenten voor elke SlootID aanwezig zijn (vul NA in waar ontbreekt)
alle_combos <- CJ(
  Behandeling = mixedsort(unique(redox_combined$Behandeling)),
  jaar = unique(redox_combined$jaar),
  compartiment = unique(redox_combined$compartiment)
)
redox_plot <- merge(alle_combos, redox_combined, by = c("Behandeling", "jaar", "compartiment"), all.x = TRUE)
redox_plot[, Behandeling := factor(Behandeling, levels = mixedsort(unique(as.character(Behandeling))))]

ggplot(redox_plot[!is.na(Behandeling),],
       aes(x = Behandeling, y = median_redox, fill = compartiment)) +
  geom_rect(data = rects, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
            inherit.aes = FALSE, alpha = 0.25) +
  scale_fill_identity('Redoxtoestand:',
                      breaks = legend_colors,
                      labels = c("Zuurstofreductie", "Denitrificatie", "Mangaanreductie",
                                 "IJzeroxide reductie", "Sulfaatreductie", "Methanogenese"),
                      guide = guide_legend(override.aes = list(alpha = 0.25))) +
  ggnewscale::new_scale_fill() +
  geom_col(aes(fill = compartiment),
           position = position_dodge(width = 0.9),
           alpha = 0.8, width = 0.9, na.rm = TRUE) +
  geom_errorbar(aes(ymin = whisker_lower, ymax = whisker_upper, group = compartiment),
                position = position_dodge(width = 0.9),
                width = 0.3, color = "black", linewidth = 0.8, na.rm = TRUE) +
  scale_fill_manual(values = c("Slib" = "#8B4513", "Water" = "#56B4E9"), name = "Compartiment") +
  facet_wrap(~ jaar, ncol = 1) +
  coord_flip() +
  theme_minimal(base_size = 15) +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    strip.background = element_rect(colour = "black", fill = "white", linewidth = 0.8),
    legend.position = "right"
  ) +
  labs(
    title = "Redoxpotentiaal in slib en water per sloot en jaar",
    subtitle = "Errorbars tonen whisker-range (Q1-1.5×IQR tot Q3+1.5×IQR)",
    x = "SlootID", y = "mV (bij pH7)"
  )



### Redox en VeeST per slootcluster (1 gebied) ------------------------
# Bereken whisker-range voor redox in WATER uit ABIO_PROJ
redox_water_summary <- abio_proj[!is.na(sloot_cluster) & !is.na(water_redox_pH7), .(
  median_redox = median(water_redox_pH7, na.rm = TRUE),
  sd_redox = sd(water_redox_pH7, na.rm = TRUE),
  # Bereken quartiles en min/max voor whisker-range
  q25_redox = quantile(water_redox_pH7, 0.25, na.rm = TRUE),
  q75_redox = quantile(water_redox_pH7, 0.75, na.rm = TRUE),
  min_redox = min(water_redox_pH7, na.rm = TRUE),
  max_redox = max(water_redox_pH7, na.rm = TRUE)
), by = .(sloot_cluster, jaar)]
# STAP 1: Bereken eerst alleen de IQR voor water
redox_water_summary[, iqr_redox := q75_redox - q25_redox]
# STAP 2: Bereken nu de whisker-range met de al bestaande iqr_redox kolom voor water
redox_water_summary[, `:=`(
  whisker_lower = pmax(min_redox, q25_redox - 1.5 * iqr_redox),
  whisker_upper = pmin(max_redox, q75_redox + 1.5 * iqr_redox),
  compartiment = "Water"
)]

redox_slib_summary <- abio_proj[!is.na(slib_redox_pH7) & !is.na(sloot_cluster), .(
  median_redox = median(slib_redox_pH7, na.rm = TRUE),
  q25_redox = quantile(slib_redox_pH7, 0.25, na.rm = TRUE),
  q75_redox = quantile(slib_redox_pH7, 0.75, na.rm = TRUE),
  min_redox = min(slib_redox_pH7, na.rm = TRUE),
  max_redox = max(slib_redox_pH7, na.rm = TRUE)
), by = .(sloot_cluster, jaar)]
redox_slib_summary[, iqr_redox := q75_redox - q25_redox]
redox_slib_summary[, `:=`(
  whisker_lower = pmax(min_redox, q25_redox - 1.5 * iqr_redox),
  whisker_upper = pmin(max_redox, q75_redox + 1.5 * iqr_redox),
  compartiment = "Slib"
)]

redox_combined <- rbind(redox_slib_summary, redox_water_summary, fill = TRUE)

rects <- data.frame(
  xmin = -Inf, 
  xmax = Inf,
  ymin = c(-Inf, -250, -100, 0,   200, 300),
  ymax = c(-250, -100,    0, 200, 300, 800),
  fill = c("#8B0000", "#FF4500", "#FFB347", "#d0ff00ff", "#4169E1", "#062992ff")
)
legend_colors <- setNames(
  c("#062992ff", "#4169E1", "#d0ff00ff", "#FFB347", "#FF4500", "#8B0000"),
  c("zuurstofreductie", "denitrificatie", "mangaanreductie", "ijzeroxidereductie", "sulfaatreductie", "methanogenese")
)

# Zorg dat beide compartimenten voor elke SlootID aanwezig zijn (vul NA in waar ontbreekt)
alle_combos <- CJ(
  sloot_cluster = mixedsort(unique(redox_combined$sloot_cluster)),
  jaar = unique(redox_combined$jaar),
  compartiment = unique(redox_combined$compartiment)
)
redox_plot <- merge(alle_combos, redox_combined, by = c("sloot_cluster", "jaar", "compartiment"), all.x = TRUE)
redox_plot[, sloot_cluster := factor(sloot_cluster, levels = mixedsort(unique(as.character(sloot_cluster))))]

ggplot(redox_plot[!is.na(sloot_cluster),],
       aes(x = sloot_cluster, y = median_redox, fill = compartiment)) +
  geom_rect(data = rects, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
            inherit.aes = FALSE, alpha = 0.25) +
  scale_fill_identity('Redoxtoestand:',
                      breaks = legend_colors,
                      labels = c("Zuurstofreductie", "Denitrificatie", "Mangaanreductie",
                                 "IJzeroxide reductie", "Sulfaatreductie", "Methanogenese"),
                      guide = guide_legend(override.aes = list(alpha = 0.25))) +
  ggnewscale::new_scale_fill() +
  geom_col(aes(fill = compartiment),
           position = position_dodge(width = 0.9),
           alpha = 0.8, width = 0.9, na.rm = TRUE) +
  geom_errorbar(aes(ymin = whisker_lower, ymax = whisker_upper, group = compartiment),
                position = position_dodge(width = 0.9),
                width = 0.3, color = "black", linewidth = 0.8, na.rm = TRUE) +
  scale_fill_manual(values = c("Slib" = "#8B4513", "Water" = "#56B4E9"), name = "Compartiment") +
  facet_wrap(~ jaar, ncol = 1) +
  coord_flip() +
  theme_minimal(base_size = 15) +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    strip.background = element_rect(colour = "black", fill = "white", linewidth = 0.8),
    legend.position = "right"
  ) +
  labs(
    title = "Redoxpotentiaal in slib en water per sloot en jaar",
    subtitle = "Errorbars tonen whisker-range (Q1-1.5×IQR tot Q3+1.5×IQR)",
    x = "SlootID", y = "mV (bij pH7)"
  )




### Ammonium toxicity ------------------------
# Maak dataframe van ammonium toxiciteitsklassen
ammonium <- data.frame(xmin = -Inf, 
                    xmax = Inf,
                    ymin = c(0,100,400,750,5000),  
                    ymax = c(100,400,750,5000,Inf),
                    fill = c("green","yellow", "orange","red","purple"),
                    label = c("niet", "voor gevoelige soorten","voor veel soorten","voor bijna alle soorten","voor alle soorten"))

legend_colors <- setNames(c("green","yellow", "orange","red","purple"), ammonium$label)
# Calculate summary statistics first
ammonium_summary <- abio_proj[!is.na(Gebiedsnaam) & !is.na(abio_proj$`NH4_µmol/l_PW`), .(
  mean_nh4 = median(`NH4_µmol/l_PW`, na.rm = TRUE),
  sd_nh4 = sd(`NH4_µmol/l_PW`, na.rm = TRUE)
), by = sloot_cluster]

# Sorteer 
sort_order <- ammonium_summary[order(mean_nh4)]
abio_proj[, Gebiedsnaam := factor(Gebiedsnaam, levels = unique(sort_order$Gebiedsnaam))]

ggplot() +
  geom_rect(data = ammonium, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), 
            inherit.aes = FALSE, alpha = 0.25) +
  scale_fill_identity('Giftig:', breaks = legend_colors, 
                     labels = c("niet", "voor gevoelige soorten", "voor veel soorten", 
                               "voor bijna alle soorten", "voor alle soorten"), 
                     guide = guide_legend(override.aes = list(alpha = 0.15))) +
  # geom_col(data = ammonium_summary,
  #          aes(x = Gebiedsnaam, y = mean_nh4), 
  #          fill = "#1B9E77", col = "#1B9E77",alpha = 0.2) +
  # geom_errorbar(data = ammonium_summary,
  #               aes(x = Gebiedsnaam, 
  #                   ymin = mean_nh4 - sd_nh4, 
  #                   ymax = mean_nh4 + sd_nh4),
  #               width = 0.2, color = "black") +
  geom_boxplot(data = abio_proj[!is.na(`NH4_µmol/l_PW`),],
               aes(x = sloot_cluster, y = `NH4_µmol/l_PW`),
               outlier.shape = NA, width=0.6, fill="#1B9E77", alpha=0.7) +
  coord_flip() +
  scale_y_log10(
    name = "Ammonium concentratie (µmol/l)",
    breaks = c(1,10,100,250,500,1000,5000,10000),
    labels = c(1,10,100,250,500,1000,5000,10000)
  ) +
  theme_minimal(base_size = 15) +
  theme(
    strip.background = element_blank(),
    strip.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.ticks = element_line(colour = "black"),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    plot.background = element_blank(),
    legend.position = "right",
    legend.box.just = "center"
  ) +
  ggtitle('Ammonium concentratie per gebied') +
  labs(x = 'Gebied', y = 'µmol/l')
ggsave(file=paste0('output/AlleGebieden/Tussenrapportage/nh4_tox.png'), width = 25,height = 15,units='cm',dpi=800)

### relatie ammonium en totaal anorganische koolstof --------------------------------------

ggplot()+
  geom_jitter(data=abio_proj[!is.na(`NH4_µmol/l_PW`) & !is.na(`TIC conc _µmol/l_PW`),],
              aes(y=`NH4_µmol/l_PW`, x=`TIC conc _µmol/l_PW`, col = Gebiedsnaam), alpha=0.3, size=2)+
  geom_smooth(data=abio_proj[!is.na(`NH4_µmol/l_PW`) & !is.na(`TIC conc _µmol/l_PW`),],
              aes(y=`NH4_µmol/l_PW`, x=`TIC conc _µmol/l_PW`), method='lm', color='#1B9E77', size=1.5)+
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.ticks = element_line(colour = "black"),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    plot.background = element_blank()
  ) +
  ggtitle('Relatie ammonium en totaal anorganische koolstof') +
  labs(y = 'Ammonium (µmol/l)', x = 'Totaal anorganische koolstof (µmol/l)')
ggsave(file=paste0('output/AlleGebieden/Tussenrapportage/nh4_tic_relatie.png'), width = 25,height = 15,units='cm',dpi=800)

# Plot 1: Ammonium vs TIC met kleuren per gebied
# R² berekenen voor ammonium vs TIC
filter <- abio_proj[!is.na(`NH4_µmol/l_PW`) & !is.na(`TIC conc _µmol/l_PW`) & !gebied %in% c('KW'),]
r2_nh4_tic <- get_r_squared(filter, 
                             "TIC conc _µmol/l_PW", "NH4_µmol/l_PW")

p1 <- ggplot()+
  geom_jitter(data=abio_proj[!is.na(`NH4_µmol/l_PW`) & !is.na(`TIC conc _µmol/l_PW`),],
              aes(y=`NH4_µmol/l_PW`, x=`TIC conc _µmol/l_PW`, color=Gebiedsnaam), 
              alpha=0.7, size=2)+
  geom_smooth(data=abio_proj[!is.na(`NH4_µmol/l_PW`) & !is.na(`TIC conc _µmol/l_PW`),],
              aes(y=`NH4_µmol/l_PW`, x=`TIC conc _µmol/l_PW`), method='lm', color='black', linewidth=1.5)+
   # R² annotatie
  annotate("text", x=Inf, y=Inf, 
           label=paste0("R² = ", round(r2_nh4_tic, 3)), 
           hjust=1.1, vjust=1.5, size=4, fontface="bold") +
  scale_color_manual(values = okabe_ito_colors, name = "Gebied") +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.ticks = element_line(colour = "black"),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    plot.background = element_blank(),
    legend.position = "bottom"
  ) +
  ggtitle('Relatie ammonium en totaal anorganische koolstof\n in poriewater') +
  labs(y = 'Ammonium (µmol/l)', x = 'Totaal anorganische koolstof (µmol/l)')

# Plot 2: P totaal vs TIC met kleuren per gebied
# R² berekenen voor P totaal vs TIC
r2_p_tic <- get_r_squared(abio_proj[!is.na(`P_µmol/l_PW`) & !is.na(`TIC conc _µmol/l_PW`)], 
                           "TIC conc _µmol/l_PW", "P_µmol/l_PW")

p2 <- ggplot()+
  geom_jitter(data=abio_proj[!is.na(`P_µmol/l_PW`) & !is.na(`TIC conc _µmol/l_PW`),],
              aes(y=`P_µmol/l_PW`, x=`TIC conc _µmol/l_PW`, color=Gebiedsnaam), 
              alpha=0.7, size=2)+
  geom_smooth(data=abio_proj[!is.na(`P_µmol/l_PW`) & !is.na(`TIC conc _µmol/l_PW`),],
              aes(y=`P_µmol/l_PW`, x=`TIC conc _µmol/l_PW`), method='lm', color='black', linewidth=1.5)+
   # R² annotatie
  annotate("text", x=Inf, y=Inf, 
           label=paste0("R² = ", round(r2_p_tic , 3)), 
           hjust=1.1, vjust=1.5, size=4, fontface="bold") +
  scale_color_manual(values = okabe_ito_colors, name = "Gebied") +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.ticks = element_line(colour = "black"),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    plot.background = element_blank(),
    legend.position = "bottom"
  ) +
  ggtitle('Relatie P-totaal en totaal anorganische koolstof\n in poriewater') +
  labs(y = 'Fosfor totaal (µmol/l)', x = 'Totaal anorganische koolstof (µmol/l)')

# Plot 3: P-AL in slib vs TIC met kleuren per gebied
r2_pal_tic <- get_r_squared(abio_proj[!is.na(`P-AL mg p2o5/100g_SB`) & !is.na(`TIC conc _µmol/l_PW`)], 
                             "TIC conc _µmol/l_PW", "P-AL mg p2o5/100g_SB")

p3 <- ggplot()+
  geom_jitter(data=abio_proj[!is.na(`P-AL mg p2o5/100g_SB`) & !is.na(`TIC conc _µmol/l_PW`),],
              aes(y=`P-AL mg p2o5/100g_SB`, x=`TIC conc _µmol/l_PW`, color=Gebiedsnaam), 
              alpha=0.7, size=2)+
  geom_smooth(data=abio_proj[!is.na(`P-AL mg p2o5/100g_SB`) & !is.na(`TIC conc _µmol/l_PW`),],
              aes(y=`P-AL mg p2o5/100g_SB`, x=`TIC conc _µmol/l_PW`), method='lm', color='black', linewidth=1.5)+
   # R² annotatie
  annotate("text", x=Inf, y=Inf, 
           label=paste0("R² = ", round(r2_pal_tic, 3)), 
           hjust=1.1, vjust=1.5, size=4, fontface="bold") +
  scale_color_manual(values = okabe_ito_colors, name = "Gebied") +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.ticks = element_line(colour = "black"),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    plot.background = element_blank(),
    legend.position = "bottom"
  ) +
  ggtitle('Relatie P-AL slib en totaal anorganische koolstof\n in poriewater') +
  labs(y = 'P-AL slib (µmol/g)', x = 'Totaal anorganische koolstof (µmol/l)')

# Combineer alle drie plots naast elkaar met gedeelde legenda
combined_plot <- p1 + p2 + p3 + 
  plot_layout(ncol = 3, guides = "collect") +
  plot_annotation(
    title = "Relaties tussen nutriënten en totaal anorganische koolstof",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
    )
  ) &
  theme(legend.position = "bottom")

# Toon de gecombineerde plot
print(combined_plot)

# Optioneel: opslaan
ggsave(file='output/AlleGebieden/Tussenrapportage/nh4_p_pal_tic_relaties.png', 
       plot = combined_plot, width = 45, height = 15, units='cm', dpi=800)



### relatie pal en p --------------------------------------
# Filter data voor P parameters
p_data <- abio_proj[!is.na(`P-AL mg p2o5/100g_SB`) & 
                    !is.na(`P-PO4_CC_mg/kg_SB`) & 
                    !is.na(`P_µmol/l_PW`)]

# Plot 1: P-AL slib vs P poriewater
p1 <- ggplot(p_data, aes(x = `P-AL mg p2o5/100g_SB`, y = `P_µmol/l_PW`)) +
  geom_jitter(aes(color = Gebiedsnaam), alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", color = "black", linewidth = 1.2) +
  stat_regline_equation(label.x = 0.7, label.y = 0.9, 
                       aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))) +
  scale_color_manual(values = okabe_ito_colors, name = "") +
  labs(x = "P-AL slib (mg P2O5/100g)", 
       y = "P poriewater (µmol/l)",
       title = "P-AL slib vs P poriewater") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "none",
    panel.border = element_rect(colour = "black", fill = NA),
    panel.grid.minor = element_blank()
  )

# Plot 2: P-CC slib vs P poriewater  
p2 <- ggplot(p_data, aes(x = `P-PO4_CC_mg/kg_SB`, y = `P_µmol/l_PW`)) +
  geom_jitter(aes(color = Gebiedsnaam), alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", color = "black", linewidth = 1.2) +
  stat_regline_equation(label.x = 0.7, label.y = 0.9,
                       aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))) +
  scale_color_manual(values = okabe_ito_colors, name = "") +
  labs(x = "P-CC slib (mg/kg)", 
       y = "P poriewater (µmol/l)",
       title = "P-CC slib vs P poriewater") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "none",
    panel.border = element_rect(colour = "black", fill = NA),
    panel.grid.minor = element_blank()
  )

# Plot 3: P-AL vs P-CC in slib
p3 <- ggplot(p_data, aes(x = `P-AL mg p2o5/100g_SB`, y = `P-PO4_CC_mg/kg_SB`)) +
  geom_jitter(aes(color = Gebiedsnaam), alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", color = "black", linewidth = 1.2) +
  stat_regline_equation(label.x = 0.7, label.y = 0.9,
                       aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))) +
  scale_color_manual(values = okabe_ito_colors, name = "") +
  labs(x = "P-AL slib (mg P2O5/100g)", 
       y = "P-CC slib (mg/kg)",
       title = "P-AL vs P-CC in slib") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "bottom",
    panel.border = element_rect(colour = "black", fill = NA),
    panel.grid.minor = element_blank()
  )

# Combineer alle plots met verbeterde legenda instellingen
combined_p_plot <- p1 + p2 + p3 + 
  plot_layout(ncol = 3, guides = "collect") +
  plot_annotation(
    title = "Relaties tussen verschillende P fracties in slib en poriewater",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
    )
  ) &
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),           # Grotere tekstgrootte
    legend.key.size = unit(1, "cm"),                 # Grotere legenda symbolen
    legend.margin = margin(t = 10),                  # Meer ruimte boven legenda
    legend.box.margin = margin(t = 10)               # Extra ruimte
  ) &
  guides(color = guide_legend(
    ncol = 6,                                        # Aantal kolommen in legenda
    override.aes = list(size = 3, alpha = 1)        # Grotere, minder transparante punten
  ))

# Toon de plot
print(combined_p_plot)

# Opslaan met aangepaste hoogte voor betere legenda
ggsave(file = 'output/AlleGebieden/Tussenrapportage/P_fracties_relaties.png', 
       plot = combined_p_plot, width = 45, height = 18, units = 'cm', dpi = 800)

### Relatie P-AL en P poriewater en P water------------------------------

p1 <- ggplot(abio_proj, aes(x = `P-AL mg p2o5/100g_SB`, y = `P-AL mg p2o5/100g_OR_50`)) +
  geom_jitter(aes(color = Gebiedsnaam), alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", color = "black", linewidth = 1.2) +
  stat_regline_equation(label.x = 0.7, label.y = 3, 
                       aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))) +
  scale_color_manual(values = okabe_ito_colors, name = "") +
  labs(x = "P-AL slib (mg P2O5/100g)", 
       y = "P oppervlaktewater (µmol/l)",
       title = "P-AL slib vs P oppervlaktewater") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "none",
    panel.border = element_rect(colour = "black", fill = NA),
    panel.grid.minor = element_blank()
  )
p2 <- ggplot(abio_proj, aes(x = `P_µmol/l_PW`, y = P_mg_l_OW)) +
  geom_jitter(aes(color = Gebiedsnaam), alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", color = "black", linewidth = 1.2) +
  stat_regline_equation(label.x = 0.7, label.y = 3, 
                       aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))) +
  scale_color_manual(values = okabe_ito_colors, name = "") +
  labs(x = "P poriewater (µmol/l)", 
       y = "P oppervlaktewater (µmol/l)",
       title = "P poriewater slib vs P oppervlaktewater") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "none",
    panel.border = element_rect(colour = "black", fill = NA),
    panel.grid.minor = element_blank()
  )
# Combineer alle plots met verbeterde legenda instellingen
combined_p_plot <- p1 + p2 + 
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(
    title = "Relaties tussen verschillende P in slib en oppervlaktewater",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
    )
  ) &
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),           # Grotere tekstgrootte
    legend.key.size = unit(1, "cm"),                 # Grotere legenda symbolen
    legend.margin = margin(t = 10),                  # Meer ruimte boven legenda
    legend.box.margin = margin(t = 10)               # Extra ruimte
  ) &
  guides(color = guide_legend(
    ncol = 6,                                        # Aantal kolommen in legenda
    override.aes = list(size = 3, alpha = 1)        # Grotere, minder transparante punten
  ))

# Toon de plot
print(combined_p_plot)

### P nalevering uit slib naar water ------------------------

# Bereken P-nalevering met beide formules
abio_proj[, `:=`(
  # Formule 1: y = 0.00004807x^2 + 0.03344949x (waar x = P-AL in mg P2O5/100g)
  P_nalevering_formule1 = 0.00004807 * (`P_µmol/l_PW`)^2 + 0.03344949 * (`P_µmol/l_PW`),
  # Formule 2: y = 0.00012907x^2 + 0.58280442(waar x = P-AL in mg P2O5/100g)  
  P_nalevering_formule2 = 0.00012907 * (`P_µmol/l_PW`)^2 + 0.00055877 * (`P_µmol/l_PW`),
  P_nalevering_baggernut = 0.80951 * `P_mg_l_PW` - 0.2905
)]
# Categoriseer zuurstofgehalte water
abio_proj[, O2_category := fifelse(
  water_O2_mgL > 2.5, 
  "Zuurstofrijk (>2.5 mg/l)", 
  "Zuurstofarm (≤2.5 mg/l)"
)]
# Bereken P-nalevering summary met whisker-range
p_nalevering_summary <- abio_proj[!is.na(P_nalevering_formule1) & !is.na(P_nalevering_formule2) & !is.na(P_nalevering_baggernut) & !is.na(Gebiedsnaam), .(
  mean_f1 = mean(P_nalevering_formule1, na.rm = TRUE),
  mean_f2 = mean(P_nalevering_formule2, na.rm = TRUE), 
  mean_baggernut = mean(P_nalevering_baggernut, na.rm = TRUE),
  
  # Bereken quartiles en whisker-range voor F1
  q25_f1 = quantile(P_nalevering_formule1, 0.25, na.rm = TRUE),
  q75_f1 = quantile(P_nalevering_formule1, 0.75, na.rm = TRUE),
  min_f1 = min(P_nalevering_formule1, na.rm = TRUE),
  max_f1 = max(P_nalevering_formule1, na.rm = TRUE),
  
  # Bereken quartiles en whisker-range voor F2
  q25_f2 = quantile(P_nalevering_formule2, 0.25, na.rm = TRUE),
  q75_f2 = quantile(P_nalevering_formule2, 0.75, na.rm = TRUE),
  min_f2 = min(P_nalevering_formule2, na.rm = TRUE),
  max_f2 = max(P_nalevering_formule2, na.rm = TRUE),
  
  # Bereken quartiles en whisker-range voor Baggernut
  q25_baggernut = quantile(P_nalevering_baggernut, 0.25, na.rm = TRUE),
  q75_baggernut = quantile(P_nalevering_baggernut, 0.75, na.rm = TRUE),
  min_baggernut = min(P_nalevering_baggernut, na.rm = TRUE),
  max_baggernut = max(P_nalevering_baggernut, na.rm = TRUE),
  
  # Bepaal kenmerken per gebied
  mean_Fe_P_ratio = mean(feP_PW, na.rm = TRUE),
  mean_O2 = mean(water_O2_mgL, na.rm = TRUE),
  n_obs = .N
), by = Gebiedsnaam]
# Bereken whisker-range (Q1-1.5×IQR tot Q3+1.5×IQR)
p_nalevering_summary[, `:=`(
  # Voor formule 1
  iqr_f1 = q75_f1 - q25_f1,
  whisker_lower_f1 = pmax(min_f1, q25_f1 - 1.5 * (q75_f1 - q25_f1)),
  whisker_upper_f1 = pmin(max_f1, q75_f1 + 1.5 * (q75_f1 - q25_f1)),
  
  # Voor formule 2
  iqr_f2 = q75_f2 - q25_f2,
  whisker_lower_f2 = pmax(min_f2, q25_f2 - 1.5 * (q75_f2 - q25_f2)),
  whisker_upper_f2 = pmin(max_f2, q75_f2 + 1.5 * (q75_f2 - q25_f2)),
  
  # Voor baggernut
  iqr_baggernut = q75_baggernut - q25_baggernut,
  whisker_lower_baggernut = pmax(min_baggernut, q25_baggernut - 1.5 * (q75_baggernut - q25_baggernut)),
  whisker_upper_baggernut = pmin(max_baggernut, q75_baggernut + 1.5 * (q75_baggernut - q25_baggernut))
)]
# Bereken Fe-ratio's met whisker-range
fe_ratio_summary <- abio_proj[!is.na(Gebiedsnaam), .(
  median_feP_PW = median(feP_PW, na.rm = TRUE),
  median_feS_SB = median(feS_DW_SB, na.rm = TRUE),
  median_feS_PW = median(feS_PW, na.rm = TRUE),
  
  # Bereken quartiles en whisker-range voor feP_PW
  q25_feP_PW = quantile(feP_PW, 0.25, na.rm = TRUE),
  q75_feP_PW = quantile(feP_PW, 0.75, na.rm = TRUE),
  min_feP_PW = min(feP_PW, na.rm = TRUE),
  max_feP_PW = max(feP_PW, na.rm = TRUE),
  
  # Bereken quartiles en whisker-range voor feS_SB
  q25_feS_SB = quantile(feS_DW_SB, 0.25, na.rm = TRUE),
  q75_feS_SB = quantile(feS_DW_SB, 0.75, na.rm = TRUE),
  min_feS_SB = min(feS_DW_SB, na.rm = TRUE),
  max_feS_SB = max(feS_DW_SB, na.rm = TRUE),
  
  # Bereken quartiles en whisker-range voor feS_PW
  q25_feS_PW = quantile(feS_PW, 0.25, na.rm = TRUE),
  q75_feS_PW = quantile(feS_PW, 0.75, na.rm = TRUE),
  min_feS_PW = min(feS_PW, na.rm = TRUE),
  max_feS_PW = max(feS_PW, na.rm = TRUE),
  
  mean_O2 = mean(water_O2_mgL, na.rm = TRUE),
  n_obs = .N
), by = Gebiedsnaam]
# Bereken whisker-range voor Fe-ratio's
fe_ratio_summary[, `:=`(
  # Voor feP_PW
  iqr_feP_PW = q75_feP_PW - q25_feP_PW,
  whisker_lower_feP_PW = pmax(min_feP_PW, q25_feP_PW - 1.5 * (q75_feP_PW - q25_feP_PW)),
  whisker_upper_feP_PW = pmin(max_feP_PW, q75_feP_PW + 1.5 * (q75_feP_PW - q25_feP_PW)),
  
  # Voor feS_SB
  iqr_feS_SB = q75_feS_SB - q25_feS_SB,
  whisker_lower_feS_SB = pmax(min_feS_SB, q25_feS_SB - 1.5 * (q75_feS_SB - q25_feS_SB)),
  whisker_upper_feS_SB = pmin(max_feS_SB, q75_feS_SB + 1.5 * (q75_feS_SB - q25_feS_SB)),
  
  # Voor feS_PW
  iqr_feS_PW = q75_feS_PW - q25_feS_PW,
  whisker_lower_feS_PW = pmax(min_feS_PW, q25_feS_PW - 1.5 * (q75_feS_PW - q25_feS_PW)),
  whisker_upper_feS_PW = pmin(max_feS_PW, q75_feS_PW + 1.5 * (q75_feS_PW - q25_feS_PW))
)]
# Filter NA gebiedsnamen weg
p_nalevering_summary <- p_nalevering_summary[!is.na(Gebiedsnaam) & Gebiedsnaam != ""]
fe_ratio_summary <- fe_ratio_summary[!is.na(Gebiedsnaam) & Gebiedsnaam != ""]
# Voeg markeringen toe aan gebiedsnamen voor P-nalevering
p_nalevering_summary[, Gebiedsnaam_marked := fifelse(
  mean_Fe_P_ratio >= 3, 
  paste0(Gebiedsnaam, " *"), 
  as.character(Gebiedsnaam)
)]
# Bepaal welke gebieden bold moeten (zuurstofarm) voor P-nalevering
p_nalevering_summary[, is_zuurstofarm := mean_O2 < 2.5]
# Voeg markeringen toe aan gebiedsnamen voor Fe-ratio's (zelfde markering als P-nalevering)
fe_ratio_summary <- merge(fe_ratio_summary, p_nalevering_summary[, .(Gebiedsnaam, Gebiedsnaam_marked, is_zuurstofarm)], 
                         by = "Gebiedsnaam", all.x = TRUE)
# Voor gebieden die alleen in fe_ratio_summary voorkomen
fe_ratio_summary[is.na(Gebiedsnaam_marked), Gebiedsnaam_marked := as.character(Gebiedsnaam)]
fe_ratio_summary[is.na(is_zuurstofarm), is_zuurstofarm := mean_O2 < 2.5]

# Sorteer gebieden op gemiddelde F1 waarde
sort_order <- p_nalevering_summary[order(mean_f1)]

# Herstructureer data voor plotting P-nalevering
p_nalevering_long <- melt(p_nalevering_summary, 
                         id.vars = c("Gebiedsnaam_marked", "is_zuurstofarm", "n_obs"),
                         measure.vars = list(
                           mean = c("mean_f1", "mean_f2", "mean_baggernut"),
                           whisker_lower = c("whisker_lower_f1", "whisker_lower_f2", "whisker_lower_baggernut"),
                           whisker_upper = c("whisker_upper_f1", "whisker_upper_f2", "whisker_upper_baggernut")
                         ),
                         variable.name = "formule")

p_nalevering_long[, formule_label := fifelse(formule == 1, "BWare - anaeroob", 
                                     fifelse(formule == 2, "BWare - aeroob", "BaggerNut - aeroob"))]
p_nalevering_long[, Gebiedsnaam_marked := factor(Gebiedsnaam_marked, levels = sort_order$Gebiedsnaam_marked)]
# Herstructureer data voor plotting Fe-ratio's
fe_ratio_long <- melt(fe_ratio_summary, 
                     id.vars = c("Gebiedsnaam_marked", "is_zuurstofarm", "n_obs"),
                     measure.vars = list(
                       median = c("median_feP_PW", "median_feS_SB", "median_feS_PW"),
                       whisker_lower = c("whisker_lower_feP_PW", "whisker_lower_feS_SB", "whisker_lower_feS_PW"),
                       whisker_upper = c("whisker_upper_feP_PW", "whisker_upper_feS_SB", "whisker_upper_feS_PW")
                     ),
                     variable.name = "ratio_type")

fe_ratio_long[, ratio_label := fifelse(ratio_type == 1, "Fe:P poriewater", 
                              fifelse(ratio_type == 2, "Fe:S sediment", "Fe:S poriewater"))]
# BELANGRIJK: Gebruik dezelfde factor levels als p_nalevering_long
fe_ratio_long[, Gebiedsnaam_marked := factor(Gebiedsnaam_marked, levels = sort_order$Gebiedsnaam_marked)]
# Filter fe_ratio_long om alleen gebieden te behouden die ook in p_nalevering_long staan
fe_ratio_long <- fe_ratio_long[Gebiedsnaam_marked %in% sort_order$Gebiedsnaam_marked]

# Plot 1: P-nalevering
p1 <- ggplot(p_nalevering_long, aes(x = Gebiedsnaam_marked, y = mean, fill = formule_label)) +
  # Bars met whisker-range errorbars
  geom_col(position = position_dodge(width = 0.8), alpha = 0.8, width = 0.7) +
  geom_errorbar(aes(ymin = whisker_lower, ymax = whisker_upper),
                position = position_dodge(width = 0.8),
                width = 0.2, color = "black", size = 0.5) +
  # Kleuren voor DRIE formules
  scale_fill_manual(
    values = c("BWare - anaeroob" = "#D55E00", "BWare - aeroob" = "#0072B2", "BaggerNut - aeroob" = "#56B4E9"),
    name = "Nalevering"
  ) +
  # Flip coordinates voor betere leesbaarheid
  coord_flip() +
  # Styling met vetgedrukte labels voor zuurstofarm water
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14, 
                               face = ifelse(sort_order$is_zuurstofarm, "bold", "plain")),
    axis.title = element_text(size = 14),
    legend.position = "bottom",
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 13),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    panel.grid.major.y = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 14, face = "bold")
  ) +
  
  # Labels
  labs(
    title = "Berekende P-nalevering naar oppervlaktewater",
    x = "Gebied",
    y = "P-nalevering (mg/m2/dag)"
  ) +
  
  # Legenda aanpassingen
  guides(fill = guide_legend(
    title = "",
    nrow = 1,
    override.aes = list(alpha = 1)
  ))

# Bepaal de maximale waarde voor de x-as
max_x_value <- 10

# Voeg gecappte waarden toe aan fe_ratio_long
fe_ratio_long[, `:=`(
  median_capped = pmin(median, max_x_value),
  is_capped = median > max_x_value,
  median_text = fifelse(median > max_x_value, as.character(round(median, 1)), "")
)]

# Plot 2: Fe-ratio's - legenda bottom met afgekapte waarden
p2 <- ggplot(fe_ratio_long, aes(x = Gebiedsnaam_marked, y = median_capped, fill = ratio_label)) +
  # Bars met whisker-range errorbars (ook afkappen)
  geom_col(position = position_dodge(width = 0.8), alpha = 0.8, width = 0.7) +
  geom_errorbar(aes(ymin = pmin(whisker_lower, max_x_value), 
                    ymax = pmin(whisker_upper, max_x_value)),
                position = position_dodge(width = 0.8),
                width = 0.2, color = "black", size = 0.5) +
  
  # Verticale referentielijnen
  geom_hline(yintercept = 5, color = "purple", linetype = "dashed", size = 1) +  # Fe/S = 5
  geom_hline(yintercept = 1, color = "red", linetype = "dashed", size = 1) +     # Fe/P = 1
  
  # Tekst voor afgekapte waarden
  geom_text(aes(x = Gebiedsnaam_marked, y = median_capped - 0.5, 
                label = median_text),
            position = position_dodge(width = 1.5),
            size = 4, color = "black", fontface = "bold") +
  
  # Kleuren die overeenkomen met de P-nalevering kleuren
  scale_fill_manual(
    values = c("Fe:P poriewater" = "#0072B2", "Fe:S sediment" = "#D55E00", "Fe:S poriewater" = "#56B4E9"),
    name = ""
  ) +
  
  # X-as limiet
  scale_y_continuous(limits = c(0, max_x_value), expand = c(0, 0)) +
  
  # Flip coordinates voor betere leesbaarheid
  coord_flip() +
  
  # Styling met vetgedrukte labels voor zuurstofarm water
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_blank(), # Y-as labels weg voor tweede plot
    axis.title = element_text(size = 14),
    axis.title.y = element_blank(), # Y-as titel weg voor tweede plot
    legend.position = "bottom",  # Legenda onderaan tweede plot
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 14, face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    panel.grid.major.y = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 14, face = "bold")
  ) +
  
  # Labels
  labs(
    title = "Fe-ratio's per gebied (mediaan)",
    x = NULL,
    y = "Fe-ratio (mol/mol)"
  ) +
  
  # Legenda aanpassingen
  guides(fill = guide_legend(
    title = "",
    nrow = 1,
    override.aes = list(alpha = 1)
  ))

# Combineer plots horizontaal
combined_plot <- p1 + p2 + plot_layout(ncol = 2, guides = 'keep')

combined_plot <- combined_plot + 
  plot_annotation(
    subtitle = "Markering gebieden:\n * = Fe/P ≥ 3\nvetgedrukt = zuurstofarm water (<2.5 mg/l)",
    theme = theme(plot.subtitle = element_text(size = 14, hjust = 0.5))
  )

# Toon plot
print(combined_plot)

# Opslaan
ggsave(file = 'output/AlleGebieden/Tussenrapportage/P_nalevering_Fe_ratios_whisker_combined.png', 
       plot = combined_plot,
       width = 35, height = 25, units = 'cm', dpi = 800)

### P nalevering uit slib naar water per slootcluster------------------------

# Bereken P-nalevering met beide formules
abio_proj[, `:=`(
  # Formule 1: y = 0.00004807x^2 + 0.03344949x (waar x = P-AL in mg P2O5/100g)
  P_nalevering_formule1 = 0.00004807 * (`P_µmol/l_PW`)^2 + 0.03344949 * (`P_µmol/l_PW`),
  # Formule 2: y = 0.00012907x^2 + 0.58280442(waar x = P-AL in mg P2O5/100g)  
  P_nalevering_formule2 = 0.00012907 * (`P_µmol/l_PW`)^2 + 0.00055877 * (`P_µmol/l_PW`),
  P_nalevering_baggernut = 0.80951 * `P_mg_l_PW` - 0.2905
)]
# Categoriseer zuurstofgehalte water
abio_proj[, O2_category := fifelse(
  water_O2_mgL > 2.5, 
  "Zuurstofrijk (>2.5 mg/l)", 
  "Zuurstofarm (≤2.5 mg/l)"
)]
# Bereken P-nalevering summary met whisker-range
p_nalevering_summary <- abio_proj[!is.na(P_nalevering_formule1) & !is.na(P_nalevering_formule2) & !is.na(P_nalevering_baggernut) & !is.na(Gebiedsnaam), .(
  mean_f1 = mean(P_nalevering_formule1, na.rm = TRUE),
  mean_f2 = mean(P_nalevering_formule2, na.rm = TRUE), 
  mean_baggernut = mean(P_nalevering_baggernut, na.rm = TRUE),
  
  # Bereken quartiles en whisker-range voor F1
  q25_f1 = quantile(P_nalevering_formule1, 0.25, na.rm = TRUE),
  q75_f1 = quantile(P_nalevering_formule1, 0.75, na.rm = TRUE),
  min_f1 = min(P_nalevering_formule1, na.rm = TRUE),
  max_f1 = max(P_nalevering_formule1, na.rm = TRUE),
  
  # Bereken quartiles en whisker-range voor F2
  q25_f2 = quantile(P_nalevering_formule2, 0.25, na.rm = TRUE),
  q75_f2 = quantile(P_nalevering_formule2, 0.75, na.rm = TRUE),
  min_f2 = min(P_nalevering_formule2, na.rm = TRUE),
  max_f2 = max(P_nalevering_formule2, na.rm = TRUE),
  
  # Bereken quartiles en whisker-range voor Baggernut
  q25_baggernut = quantile(P_nalevering_baggernut, 0.25, na.rm = TRUE),
  q75_baggernut = quantile(P_nalevering_baggernut, 0.75, na.rm = TRUE),
  min_baggernut = min(P_nalevering_baggernut, na.rm = TRUE),
  max_baggernut = max(P_nalevering_baggernut, na.rm = TRUE),
  
  # Bepaal kenmerken per gebied
  mean_Fe_P_ratio = mean(feP_PW, na.rm = TRUE),
  mean_O2 = mean(water_O2_mgL, na.rm = TRUE),
  n_obs = .N
), by = sloot_cluster]
# Bereken whisker-range (Q1-1.5×IQR tot Q3+1.5×IQR)
p_nalevering_summary[, `:=`(
  # Voor formule 1
  iqr_f1 = q75_f1 - q25_f1,
  whisker_lower_f1 = pmax(min_f1, q25_f1 - 1.5 * (q75_f1 - q25_f1)),
  whisker_upper_f1 = pmin(max_f1, q75_f1 + 1.5 * (q75_f1 - q25_f1)),
  
  # Voor formule 2
  iqr_f2 = q75_f2 - q25_f2,
  whisker_lower_f2 = pmax(min_f2, q25_f2 - 1.5 * (q75_f2 - q25_f2)),
  whisker_upper_f2 = pmin(max_f2, q75_f2 + 1.5 * (q75_f2 - q25_f2)),
  
  # Voor baggernut
  iqr_baggernut = q75_baggernut - q25_baggernut,
  whisker_lower_baggernut = pmax(min_baggernut, q25_baggernut - 1.5 * (q75_baggernut - q25_baggernut)),
  whisker_upper_baggernut = pmin(max_baggernut, q75_baggernut + 1.5 * (q75_baggernut - q25_baggernut))
)]
# Bereken Fe-ratio's met whisker-range
fe_ratio_summary <- abio_proj[!is.na(sloot_cluster), .(
  median_feP_PW = median(feP_PW, na.rm = TRUE),
  median_feS_SB = median(feS_DW_SB, na.rm = TRUE),
  median_feS_PW = median(feS_PW, na.rm = TRUE),
  
  # Bereken quartiles en whisker-range voor feP_PW
  q25_feP_PW = quantile(feP_PW, 0.25, na.rm = TRUE),
  q75_feP_PW = quantile(feP_PW, 0.75, na.rm = TRUE),
  min_feP_PW = min(feP_PW, na.rm = TRUE),
  max_feP_PW = max(feP_PW, na.rm = TRUE),
  
  # Bereken quartiles en whisker-range voor feS_SB
  q25_feS_SB = quantile(feS_DW_SB, 0.25, na.rm = TRUE),
  q75_feS_SB = quantile(feS_DW_SB, 0.75, na.rm = TRUE),
  min_feS_SB = min(feS_DW_SB, na.rm = TRUE),
  max_feS_SB = max(feS_DW_SB, na.rm = TRUE),
  
  # Bereken quartiles en whisker-range voor feS_PW
  q25_feS_PW = quantile(feS_PW, 0.25, na.rm = TRUE),
  q75_feS_PW = quantile(feS_PW, 0.75, na.rm = TRUE),
  min_feS_PW = min(feS_PW, na.rm = TRUE),
  max_feS_PW = max(feS_PW, na.rm = TRUE),
  
  mean_O2 = mean(water_O2_mgL, na.rm = TRUE),
  n_obs = .N
), by = sloot_cluster]
# Bereken whisker-range voor Fe-ratio's
fe_ratio_summary[, `:=`(
  # Voor feP_PW
  iqr_feP_PW = q75_feP_PW - q25_feP_PW,
  whisker_lower_feP_PW = pmax(min_feP_PW, q25_feP_PW - 1.5 * (q75_feP_PW - q25_feP_PW)),
  whisker_upper_feP_PW = pmin(max_feP_PW, q75_feP_PW + 1.5 * (q75_feP_PW - q25_feP_PW)),
  
  # Voor feS_SB
  iqr_feS_SB = q75_feS_SB - q25_feS_SB,
  whisker_lower_feS_SB = pmax(min_feS_SB, q25_feS_SB - 1.5 * (q75_feS_SB - q25_feS_SB)),
  whisker_upper_feS_SB = pmin(max_feS_SB, q75_feS_SB + 1.5 * (q75_feS_SB - q25_feS_SB)),
  
  # Voor feS_PW
  iqr_feS_PW = q75_feS_PW - q25_feS_PW,
  whisker_lower_feS_PW = pmax(min_feS_PW, q25_feS_PW - 1.5 * (q75_feS_PW - q25_feS_PW)),
  whisker_upper_feS_PW = pmin(max_feS_PW, q75_feS_PW + 1.5 * (q75_feS_PW - q25_feS_PW))
)]
# Filter NA gebiedsnamen weg
p_nalevering_summary <- p_nalevering_summary[!is.na(sloot_cluster) & sloot_cluster != ""]
fe_ratio_summary <- fe_ratio_summary[!is.na(sloot_cluster) & sloot_cluster != ""]
# Voeg markeringen toe aan gebiedsnamen voor P-nalevering
p_nalevering_summary[, sloot_cluster_marked := fifelse(
  mean_Fe_P_ratio >= 3, 
  paste0(sloot_cluster, " *"), 
  as.character(sloot_cluster)
)]
# Bepaal welke gebieden bold moeten (zuurstofarm) voor P-nalevering
p_nalevering_summary[, is_zuurstofarm := mean_O2 < 2.5]
# Voeg markeringen toe aan gebiedsnamen voor Fe-ratio's (zelfde markering als P-nalevering)
fe_ratio_summary <- merge(fe_ratio_summary, p_nalevering_summary[, .(sloot_cluster, sloot_cluster_marked, is_zuurstofarm)], 
                         by = "sloot_cluster", all.x = TRUE)
# Voor gebieden die alleen in fe_ratio_summary voorkomen
fe_ratio_summary[is.na(sloot_cluster_marked), sloot_cluster_marked := as.character(sloot_cluster)]
fe_ratio_summary[is.na(is_zuurstofarm), is_zuurstofarm := mean_O2 < 2.5]

# Sorteer gebieden op gemiddelde F1 waarde
sort_order <- p_nalevering_summary[order(mean_f1)]

# Herstructureer data voor plotting P-nalevering
p_nalevering_long <- melt(p_nalevering_summary, 
                         id.vars = c("sloot_cluster_marked", "is_zuurstofarm", "n_obs"),
                         measure.vars = list(
                           mean = c("mean_f1", "mean_f2", "mean_baggernut"),
                           whisker_lower = c("whisker_lower_f1", "whisker_lower_f2", "whisker_lower_baggernut"),
                           whisker_upper = c("whisker_upper_f1", "whisker_upper_f2", "whisker_upper_baggernut")
                         ),
                         variable.name = "formule")

p_nalevering_long[, formule_label := fifelse(formule == 1, "BWare - anaeroob", 
                                     fifelse(formule == 2, "BWare - aeroob", "BaggerNut - aeroob"))]
p_nalevering_long[, sloot_cluster_marked := factor(sloot_cluster_marked, levels = sort_order$sloot_cluster_marked)]
# Herstructureer data voor plotting Fe-ratio's
fe_ratio_long <- melt(fe_ratio_summary, 
                     id.vars = c("sloot_cluster_marked", "is_zuurstofarm", "n_obs"),
                     measure.vars = list(
                       median = c("median_feP_PW", "median_feS_SB", "median_feS_PW"),
                       whisker_lower = c("whisker_lower_feP_PW", "whisker_lower_feS_SB", "whisker_lower_feS_PW"),
                       whisker_upper = c("whisker_upper_feP_PW", "whisker_upper_feS_SB", "whisker_upper_feS_PW")
                     ),
                     variable.name = "ratio_type")

fe_ratio_long[, ratio_label := fifelse(ratio_type == 1, "Fe:P poriewater", 
                              fifelse(ratio_type == 2, "Fe:S sediment", "Fe:S poriewater"))]
# BELANGRIJK: Gebruik dezelfde factor levels als p_nalevering_long
fe_ratio_long[, sloot_cluster_marked := factor(sloot_cluster_marked, levels = sort_order$sloot_cluster_marked)]
# Filter fe_ratio_long om alleen gebieden te behouden die ook in p_nalevering_long staan
fe_ratio_long <- fe_ratio_long[sloot_cluster_marked %in% sort_order$sloot_cluster_marked]

# Plot 1: P-nalevering
p1 <- ggplot(p_nalevering_long, aes(x = sloot_cluster_marked, y = mean, fill = formule_label)) +
  # Bars met whisker-range errorbars
  geom_col(position = position_dodge(width = 0.8), alpha = 0.8, width = 0.7) +
  geom_errorbar(aes(ymin = whisker_lower, ymax = whisker_upper),
                position = position_dodge(width = 0.8),
                width = 0.2, color = "black", size = 0.5) +
  # Kleuren voor DRIE formules
  scale_fill_manual(
    values = c("BWare - anaeroob" = "#D55E00", "BWare - aeroob" = "#0072B2", "BaggerNut - aeroob" = "#56B4E9"),
    name = "Nalevering"
  ) +
  # Flip coordinates voor betere leesbaarheid
  coord_flip() +
  # Styling met vetgedrukte labels voor zuurstofarm water
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14, 
                               face = ifelse(sort_order$is_zuurstofarm, "bold", "plain")),
    axis.title = element_text(size = 14),
    legend.position = "bottom",
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 13),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    panel.grid.major.y = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 14, face = "bold")
  ) +
  
  # Labels
  labs(
    title = "Berekende P-nalevering naar oppervlaktewater",
    x = "Gebied",
    y = "P-nalevering (mg/m2/dag)"
  ) +
  
  # Legenda aanpassingen
  guides(fill = guide_legend(
    title = "",
    nrow = 1,
    override.aes = list(alpha = 1)
  ))

# Bepaal de maximale waarde voor de x-as
max_x_value <- 10

# Voeg gecappte waarden toe aan fe_ratio_long
fe_ratio_long[, `:=`(
  median_capped = pmin(median, max_x_value),
  is_capped = median > max_x_value,
  median_text = fifelse(median > max_x_value, as.character(round(median, 1)), "")
)]

# Plot 2: Fe-ratio's - legenda bottom met afgekapte waarden
p2 <- ggplot(fe_ratio_long, aes(x = sloot_cluster_marked, y = median_capped, fill = ratio_label)) +
  # Bars met whisker-range errorbars (ook afkappen)
  geom_col(position = position_dodge(width = 0.8), alpha = 0.8, width = 0.7) +
  geom_errorbar(aes(ymin = pmin(whisker_lower, max_x_value), 
                    ymax = pmin(whisker_upper, max_x_value)),
                position = position_dodge(width = 0.8),
                width = 0.2, color = "black", size = 0.5) +
  
  # Verticale referentielijnen
  geom_hline(yintercept = 5, color = "purple", linetype = "dashed", size = 1) +  # Fe/S = 5
  geom_hline(yintercept = 1, color = "red", linetype = "dashed", size = 1) +     # Fe/P = 1
  
  # Tekst voor afgekapte waarden
  geom_text(aes(x = sloot_cluster_marked, y = median_capped - 0.5, 
                label = median_text),
            position = position_dodge(width = 1.5),
            size = 4, color = "black", fontface = "bold") +
  
  # Kleuren die overeenkomen met de P-nalevering kleuren
  scale_fill_manual(
    values = c("Fe:P poriewater" = "#0072B2", "Fe:S sediment" = "#D55E00", "Fe:S poriewater" = "#56B4E9"),
    name = ""
  ) +
  
  # X-as limiet
  scale_y_continuous(limits = c(0, max_x_value), expand = c(0, 0)) +
  
  # Flip coordinates voor betere leesbaarheid
  coord_flip() +
  
  # Styling met vetgedrukte labels voor zuurstofarm water
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_blank(), # Y-as labels weg voor tweede plot
    axis.title = element_text(size = 14),
    axis.title.y = element_blank(), # Y-as titel weg voor tweede plot
    legend.position = "bottom",  # Legenda onderaan tweede plot
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 14, face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    panel.grid.major.y = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 14, face = "bold")
  ) +
  
  # Labels
  labs(
    title = "Fe-ratio's per gebied (mediaan)",
    x = NULL,
    y = "Fe-ratio (mol/mol)"
  ) +
  
  # Legenda aanpassingen
  guides(fill = guide_legend(
    title = "",
    nrow = 1,
    override.aes = list(alpha = 1)
  ))

# Combineer plots horizontaal
combined_plot <- p1 + p2 + plot_layout(ncol = 2, guides = 'keep')

combined_plot <- combined_plot + 
  plot_annotation(
    subtitle = "Markering gebieden:\n * = Fe/P ≥ 3\nvetgedrukt = zuurstofarm water (<2.5 mg/l)",
    theme = theme(plot.subtitle = element_text(size = 14, hjust = 0.5))
  )

# Toon plot
print(combined_plot)

# Opslaan
ggsave(file = 'output/AlleGebieden/Tussenrapportage/P_nalevering_Fe_ratios_whisker_combined.png', 
       plot = combined_plot,
       width = 35, height = 25, units = 'cm', dpi = 800)


### Onderholling------------------------
# Bereken whisker-range voor onderholling uit ABIO_PROJ
onderholling_summary <- abio_proj[!is.na(Gebiedsnaam) & !is.na(holleoever), .(
  mean_onderholling = median(holleoever, na.rm = TRUE),
  sd_onderholling = sd(holleoever, na.rm = TRUE),
  
  # Bereken quartiles en min/max voor whisker-range
  q25_onderholling = quantile(holleoever, 0.25, na.rm = TRUE),
  q75_onderholling = quantile(holleoever, 0.75, na.rm = TRUE),
  min_onderholling = min(holleoever, na.rm = TRUE),
  max_onderholling = max(holleoever, na.rm = TRUE)
), by = Gebiedsnaam]

# Bereken whisker-range (Q1-1.5×IQR tot Q3+1.5×IQR)
onderholling_summary[, `:=`(
  iqr_onderholling = q75_onderholling - q25_onderholling,
  whisker_lower = pmax(min_onderholling, q25_onderholling - 1.5 * (q75_onderholling - q25_onderholling)),
  whisker_upper = pmin(max_onderholling, q75_onderholling + 1.5 * (q75_onderholling - q25_onderholling))
)]

# Sorteer de data en maak een geordende factor van Gebiedsnaam
onderholling_summary <- onderholling_summary[order(mean_onderholling)]
onderholling_summary[, Gebiedsnaam := factor(Gebiedsnaam, levels = unique(Gebiedsnaam))]

# Bereken mediaan voor referentielijn
median_onderholling <- median(abio_proj$holleoever, na.rm = TRUE)

ggplot() +
  geom_col(data = onderholling_summary,
           aes(x = Gebiedsnaam, y = mean_onderholling), 
           fill = "#009E73", alpha = 0.7, width = 0.8) +
  
  # Whisker-range errorbars (zoals boxplot whiskers)
  geom_errorbar(data = onderholling_summary,
                aes(x = Gebiedsnaam, 
                    ymin = whisker_lower, 
                    ymax = whisker_upper),
                width = 0.2, color = "black", size = 0.8) +
  
  # Mediaan als referentielijn
  geom_hline(yintercept = median_onderholling, 
             color = "black", linetype = "dashed", size = 1) +
  
  scale_y_continuous(breaks = c(10,20,30,40,50,60,70,80,100,200)) +
  coord_flip() +
  
  theme_minimal(base_size = 15) +
  theme(
    strip.background = element_blank(),
    strip.text.y = element_text(size = 12),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.ticks = element_line(colour = "black"),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    plot.background = element_blank(),
    legend.position = "right",
    legend.box.just = "center"
  ) +
  
  labs(
    title = 'Onderholling per gebied',
    subtitle = paste0("Zwarte stippellijn = mediaan onderholling (", 
                     round(median_onderholling, 1), " cm)\nErrorbars tonen whisker-range (Q1-1.5×IQR tot Q3+1.5×IQR)"),
    x = 'Gebied', 
    y = 'Onderholling (cm)'
  )

# Opslaan van de plot
ggsave(file = 'output/AlleGebieden/Tussenrapportage/onderholling_whisker_range.png', 
       width = 25, height = 15, units = 'cm', dpi = 800)
### Onderholling per gebied------------------------
# Bereken whisker-range voor onderholling uit ABIO_PROJ
onderholling_summary <- abio_proj[!is.na(sloot_cluster) & !is.na(holleoever) & jaar == 2025, .(
  mean_onderholling = median(holleoever, na.rm = TRUE),
  sd_onderholling = sd(holleoever, na.rm = TRUE),
  
  # Bereken quartiles en min/max voor whisker-range
  q25_onderholling = quantile(holleoever, 0.25, na.rm = TRUE),
  q75_onderholling = quantile(holleoever, 0.75, na.rm = TRUE),
  min_onderholling = min(holleoever, na.rm = TRUE),
  max_onderholling = max(holleoever, na.rm = TRUE)
), by = c("sloot_cluster","Behandeling")]

# Bereken whisker-range (Q1-1.5×IQR tot Q3+1.5×IQR)
onderholling_summary[, `:=`(
  iqr_onderholling = q75_onderholling - q25_onderholling,
  whisker_lower = pmax(min_onderholling, q25_onderholling - 1.5 * (q75_onderholling - q25_onderholling)),
  whisker_upper = pmin(max_onderholling, q75_onderholling + 1.5 * (q75_onderholling - q25_onderholling))
)]

# Sorteer de data en maak een geordende factor van sloot_cluster
# onderholling_summary <- onderholling_summary[order(mean_onderholling)]
# onderholling_summary[, sloot_cluster := factor(sloot_cluster, levels = unique(sloot_cluster))]

# Bereken mediaan voor referentielijn
median_onderholling <- median(abio_proj$holleoever, na.rm = TRUE)

ggplot() +
  geom_col(data = onderholling_summary,
           aes(x = Behandeling, y = mean_onderholling), 
           fill = "#009E73", alpha = 0.7, width = 0.8) +
  # Whisker-range errorbars (zoals boxplot whiskers)
  geom_errorbar(data = onderholling_summary,
                aes(x = Behandeling, 
                    ymin = whisker_lower, 
                    ymax = whisker_upper),
                width = 0.2, color = "black", size = 0.8) +
  # Mediaan als referentielijn
  geom_hline(yintercept = median_onderholling, 
             color = "black", linetype = "dashed", size = 1) +
  facet_wrap(.~sloot_cluster, nrow = 1, scales = "free_x") +
  scale_y_continuous(breaks = c(10,20,30,40,50,60,70,80,100,200)) +
  # coord_flip() +
  
  theme_minimal(base_size = 15) +
  theme(
    strip.background = element_blank(),
    strip.text.y = element_text(size = 12),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.title = element_text(size = 14),
    axis.ticks = element_line(colour = "black"),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    plot.background = element_blank(),
    legend.position = "right",
    legend.box.just = "center"
  ) +
  
  labs(
    title = 'Onderholling per gebied',
    subtitle = paste0("Zwarte stippellijn = mediaan onderholling (", 
                     round(median_onderholling, 1), " cm)\nErrorbars tonen whisker-range (Q1-1.5×IQR tot Q3+1.5×IQR)"),
    x = 'Slootnummer en behandeling', 
    y = 'Onderholling (cm)'
  )

# Opslaan van de plot
ggsave(file = 'output/AlleGebieden/Tussenrapportage/onderholling_whisker_range.png', 
       width = 25, height = 15, units = 'cm', dpi = 800)

### Taludhoek ------------------------
#### Maak datatable van abio_proj met gemiddelden per gebied--------------------------------------
setDT(abio_proj)
tldk_summary <- abio_proj[!is.na(Gebiedsnaam) & (!is.na(tldk_oevrwtr_graden) | !is.na(tldk_wtrwtr_graden) | !is.na(tldk_vastbodem_graden)), .(
  mean_tldk_oevrwtr_perc = mean(tldk_oevrwtr_perc, na.rm = TRUE),
  sd_tldk_oevrwtr_perc = sd(tldk_oevrwtr_perc, na.rm = TRUE),
  mean_tldk_wtrwtr_perc = mean(tldk_wtrwtr_perc, na.rm = TRUE),
  sd_tldk_wtrwtr_perc = sd(tldk_wtrwtr_perc, na.rm = TRUE),
  mean_tldk_vastbodem_perc = mean(tldk_vastbodem_perc, na.rm = TRUE),
  sd_tldk_vastbodem_perc = sd(tldk_vastbodem_perc, na.rm = TRUE),
  mean_tldk_oevrwtr = mean(tldk_oevrwtr_graden, na.rm = TRUE),
  sd_tldk_oevrwtr = sd(tldk_oevrwtr_graden, na.rm = TRUE),
  mean_tldk_wtrwtr = mean(tldk_wtrwtr_graden, na.rm = TRUE),
  sd_tldk_wtrwtr = sd(tldk_wtrwtr_graden, na.rm = TRUE),
  mean_tldk_vastbodem = mean(tldk_vastbodem_graden, na.rm = TRUE),
  sd_tldk_vastbodem = sd(tldk_vastbodem_graden, na.rm = TRUE)), by = Gebiedsnaam]

# Sorteer de data en maak een geordende factor van Gebiedsnaam
tldk_summary <- tldk_summary[order(mean_tldk_oevrwtr)]
tldk_summary[, Gebiedsnaam := factor(Gebiedsnaam, levels = unique(Gebiedsnaam))]

ggplot() + # Taludhoek oever boven water in graden
  geom_col(data = tldk_summary,
           aes(x = Gebiedsnaam, y = mean_tldk_oevrwtr), 
           fill = "#009E73", alpha = 0.7) +
  geom_errorbar(data = tldk_summary,
                aes(x = Gebiedsnaam, 
                    ymin = ifelse((mean_tldk_oevrwtr - sd_tldk_oevrwtr)> 0, mean_tldk_oevrwtr - sd_tldk_oevrwtr, 0),
                    ymax = mean_tldk_oevrwtr + sd_tldk_oevrwtr),
                width = 0.2, color = "black") +
  # Add median lines
  geom_hline(yintercept = median(abio_proj$tldk_oevrwtr_graden, na.rm = TRUE), color = "black", linetype = "dashed", size = 1) +
  scale_y_continuous(breaks = seq(0, 45, 5))+
  coord_flip()+
  theme_minimal(base_size = 15) +
  theme(
    strip.background = element_blank(),
    strip.text.y = element_text(size = 12),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.ticks = element_line(colour = "black"),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    plot.background = element_blank(),
    legend.position = "right",
    legend.box.just = "center")+
  ggtitle('Taludhoek oever boven de waterlijn') +
  labs(x = 'Gebied', y = 'Graden (°)')
ggsave(file=paste0('output/AlleGebieden/Tussenrapportage/taludhoek_oever_boven_water.png'), width = 25,height = 15,units='cm',dpi=800)

ggplot() +
  geom_col(data = tldk_summary,
           aes(x = Gebiedsnaam, y = mean_tldk_wtrwtr),
           fill = "#D55E00", alpha = 0.7) +
  geom_errorbar(data = tldk_summary,
                aes(x = Gebiedsnaam, 
                    ymin = ifelse((mean_tldk_wtrwtr - sd_tldk_wtrwtr)> 0, mean_tldk_wtrwtr - sd_tldk_wtrwtr, 0),
                    ymax = mean_tldk_wtrwtr + sd_tldk_wtrwtr),
                width = 0.2, color = "black") +
  # Add median lines
  geom_hline(yintercept = median(abio_proj$tldk_wtrwtr_graden, na.rm = TRUE), color = "black", linetype = "dashed", size = 1) +
  scale_y_continuous(breaks = seq(0, 45, 5))+
  coord_flip()+
  theme_minimal(base_size = 15) +
  theme(
    strip.background = element_blank(),
    strip.text.y = element_text(size = 12),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.ticks = element_line(colour = "black"),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    plot.background = element_blank(),
    legend.position = "right",
    legend.box.just = "center"
  ) +
  ggtitle('Taludhoek onder de waterlijn') +
  labs(x = 'Gebied', y = 'Graden (°)')
ggsave(file=paste0('output/waterbodem/taludhoek_onder_water.png'), width = 25,height = 15,units='cm',dpi=800)

ggplot() +
  geom_col(data = tldk_summary,
           aes(x = Gebiedsnaam, y = mean_tldk_vastbodem),
           fill = "#E6AB02", alpha = 0.7) +
  geom_errorbar(data = tldk_summary,
                aes(x = Gebiedsnaam, 
                    ymin = ifelse((mean_tldk_vastbodem - sd_tldk_vastbodem)> 0, mean_tldk_vastbodem - sd_tldk_vastbodem, 0),
                    ymax = mean_tldk_vastbodem + sd_tldk_vastbodem),
                width = 0.2, color = "black") +
  # Add median lines
  geom_hline(yintercept = median(abio_proj$tldk_vastbodem_graden, na.rm = TRUE), color = "black", linetype = "dashed", size = 1) +
  scale_y_continuous(breaks = seq(0, 45, 5))+
  coord_flip()+
  theme_minimal(base_size = 15) +
  theme(
    strip.background = element_blank(),
    strip.text.y = element_text(size = 12),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.ticks = element_line(colour = "black"),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    plot.background = element_blank(),
    legend.position = "right",
    legend.box.just = "center"
  ) +
  ggtitle('Taludhoek vaste bodem') +
  labs(x = 'Gebied', y = 'Graden (°)')
ggsave(file=paste0('output/waterbodem/taludhoek_vaste_bodem.png'), width = 25,height = 15,units='cm',dpi=800)
#### Taludhoeken waterlijn, boven wl en vast bodem ------------------------------------------------------------------
# Bereken whisker-range voor alle taludhoek metingen uit ABIO_PROJ
tldk_summary <- abio_proj[!is.na(Gebiedsnaam) & (!is.na(tldk_oevrwtr_graden) | !is.na(tldk_wtrwtr_graden) | !is.na(tldk_vastbodem_graden)), .(
  mean_tldk_oevrwtr = median(tldk_oevrwtr_graden, na.rm = TRUE),
  sd_tldk_oevrwtr = sd(tldk_oevrwtr_graden, na.rm = TRUE),
  mean_tldk_wtrwtr = mean(tldk_wtrwtr_graden, na.rm = TRUE),
  sd_tldk_wtrwtr = sd(tldk_wtrwtr_graden, na.rm = TRUE),
  mean_tldk_vastbodem = mean(tldk_vastbodem_graden, na.rm = TRUE),
  sd_tldk_vastbodem = sd(tldk_vastbodem_graden, na.rm = TRUE),
  
  # Bereken quartiles en min/max voor whisker-range
  q25_oevrwtr = quantile(tldk_oevrwtr_graden, 0.25, na.rm = TRUE),
  q75_oevrwtr = quantile(tldk_oevrwtr_graden, 0.75, na.rm = TRUE),
  min_oevrwtr = min(tldk_oevrwtr_graden, na.rm = TRUE),
  max_oevrwtr = max(tldk_oevrwtr_graden, na.rm = TRUE),
  
  q25_wtrwtr = quantile(tldk_wtrwtr_graden, 0.25, na.rm = TRUE),
  q75_wtrwtr = quantile(tldk_wtrwtr_graden, 0.75, na.rm = TRUE),
  min_wtrwtr = min(tldk_wtrwtr_graden, na.rm = TRUE),
  max_wtrwtr = max(tldk_wtrwtr_graden, na.rm = TRUE),
  
  q25_vastbodem = quantile(tldk_vastbodem_graden, 0.25, na.rm = TRUE),
  q75_vastbodem = quantile(tldk_vastbodem_graden, 0.75, na.rm = TRUE),
  min_vastbodem = min(tldk_vastbodem_graden, na.rm = TRUE),
  max_vastbodem = max(tldk_vastbodem_graden, na.rm = TRUE)
), by = Gebiedsnaam]
# Bereken whisker-range (Q1-1.5×IQR tot Q3+1.5×IQR)
tldk_summary[, `:=`(
  # Voor oever boven water
  iqr_oevrwtr = q75_oevrwtr - q25_oevrwtr,
  whisker_lower_oevrwtr = pmax(min_oevrwtr, q25_oevrwtr - 1.5 * (q75_oevrwtr - q25_oevrwtr)),
  whisker_upper_oevrwtr = pmin(max_oevrwtr, q75_oevrwtr + 1.5 * (q75_oevrwtr - q25_oevrwtr)),
  
  # Voor onder waterlijn
  iqr_wtrwtr = q75_wtrwtr - q25_wtrwtr,
  whisker_lower_wtrwtr = pmax(min_wtrwtr, q25_wtrwtr - 1.5 * (q75_wtrwtr - q25_wtrwtr)),
  whisker_upper_wtrwtr = pmin(max_wtrwtr, q75_wtrwtr + 1.5 * (q75_wtrwtr - q25_wtrwtr)),
  
  # Voor vaste bodem
  iqr_vastbodem = q75_vastbodem - q25_vastbodem,
  whisker_lower_vastbodem = pmax(min_vastbodem, q25_vastbodem - 1.5 * (q75_vastbodem - q25_vastbodem)),
  whisker_upper_vastbodem = pmin(max_vastbodem, q75_vastbodem + 1.5 * (q75_vastbodem - q25_vastbodem))
)]
# Sorteer de data en maak een geordende factor van Gebiedsnaam
tldk_summary <- tldk_summary[order(mean_tldk_oevrwtr)]
tldk_summary[, Gebiedsnaam := factor(Gebiedsnaam, levels = unique(Gebiedsnaam))]
# Maak tldk_long met whisker-range data
tldk_long <- melt(tldk_summary, 
                  id.vars = "Gebiedsnaam", 
                  measure.vars = list(
                    mean = c("mean_tldk_oevrwtr", "mean_tldk_wtrwtr", "mean_tldk_vastbodem"),
                    sd = c("sd_tldk_oevrwtr", "sd_tldk_wtrwtr", "sd_tldk_vastbodem"),
                    whisker_lower = c("whisker_lower_oevrwtr", "whisker_lower_wtrwtr", "whisker_lower_vastbodem"),
                    whisker_upper = c("whisker_upper_oevrwtr", "whisker_upper_wtrwtr", "whisker_upper_vastbodem")
                  ),
                  variable.name = "talud_type", 
                  value.name = c("mean", "sd", "whisker_lower", "whisker_upper"))

# Voeg labels toe
tldk_long[talud_type == 1, talud_label := "Boven waterlijn"]
tldk_long[talud_type == 2, talud_label := "Onder waterlijn"]
tldk_long[talud_type == 3, talud_label := "Vaste bodem"]

# Plot met whisker-range errorbars
ggplot(tldk_long, aes(x = Gebiedsnaam, y = mean, fill = talud_label)) +
  geom_col(position = "dodge", alpha = 0.7) +
  
  # Whisker-range errorbars (zoals boxplot whiskers)
  geom_errorbar(aes(ymin = whisker_lower, 
                    ymax = whisker_upper), 
                position = position_dodge(0.9), 
                width = 0.25, 
                color = "black", 
                alpha = 0.8, 
                size = 0.8) +
  
  # Okabe-Ito kleuren
  scale_fill_manual(values = okabe_ito_colors[1:3]) +
  
  coord_flip() +
  
  facet_wrap(~talud_label, scales = "free_x") +
  
  labs(
    title = "Taludhoeken oever per gebied", 
    subtitle = "Errorbars tonen whisker-range (Q1-1.5×IQR tot Q3+1.5×IQR)", 
    x = "Gebied", 
    y = "Taludhoek (°)"
  ) +
  
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "none", 
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    strip.text = element_text(size = 14, face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    strip.background = element_rect(colour = "black", fill = "white", linewidth = 0.8)
  )
# Opslaan van de plot
ggsave(file = 'output/AlleGebieden/Tussenrapportage/taludhoeken_whisker_range.png', 
       width = 45, height = 25, units = 'cm', dpi = 800)

### Taludhoek per sloot_id------------------------
#### taludhoeken waterlijn, boven wl en vast bodem ------------------------------------------------------------------
# Bereken whisker-range voor alle taludhoek metingen uit ABIO_PROJ
tldk_summary <- abio_proj[!is.na(sloot_cluster) & (!is.na(tldk_oevrwtr_graden) | !is.na(tldk_wtrwtr_graden) | !is.na(tldk_vastbodem_graden)), .(
  mean_tldk_oevrwtr = median(tldk_oevrwtr_graden, na.rm = TRUE),
  sd_tldk_oevrwtr = sd(tldk_oevrwtr_graden, na.rm = TRUE),
  mean_tldk_wtrwtr = mean(tldk_wtrwtr_graden, na.rm = TRUE),
  sd_tldk_wtrwtr = sd(tldk_wtrwtr_graden, na.rm = TRUE),
  mean_tldk_vastbodem = mean(tldk_vastbodem_graden, na.rm = TRUE),
  sd_tldk_vastbodem = sd(tldk_vastbodem_graden, na.rm = TRUE),
  
  # Bereken quartiles en min/max voor whisker-range
  q25_oevrwtr = quantile(tldk_oevrwtr_graden, 0.25, na.rm = TRUE),
  q75_oevrwtr = quantile(tldk_oevrwtr_graden, 0.75, na.rm = TRUE),
  min_oevrwtr = min(tldk_oevrwtr_graden, na.rm = TRUE),
  max_oevrwtr = max(tldk_oevrwtr_graden, na.rm = TRUE),
  
  q25_wtrwtr = quantile(tldk_wtrwtr_graden, 0.25, na.rm = TRUE),
  q75_wtrwtr = quantile(tldk_wtrwtr_graden, 0.75, na.rm = TRUE),
  min_wtrwtr = min(tldk_wtrwtr_graden, na.rm = TRUE),
  max_wtrwtr = max(tldk_wtrwtr_graden, na.rm = TRUE),
  
  q25_vastbodem = quantile(tldk_vastbodem_graden, 0.25, na.rm = TRUE),
  q75_vastbodem = quantile(tldk_vastbodem_graden, 0.75, na.rm = TRUE),
  min_vastbodem = min(tldk_vastbodem_graden, na.rm = TRUE),
  max_vastbodem = max(tldk_vastbodem_graden, na.rm = TRUE)
), by = sloot_cluster]
# Bereken whisker-range (Q1-1.5×IQR tot Q3+1.5×IQR)
tldk_summary[, `:=`(
  # Voor oever boven water
  iqr_oevrwtr = q75_oevrwtr - q25_oevrwtr,
  whisker_lower_oevrwtr = pmax(min_oevrwtr, q25_oevrwtr - 1.5 * (q75_oevrwtr - q25_oevrwtr)),
  whisker_upper_oevrwtr = pmin(max_oevrwtr, q75_oevrwtr + 1.5 * (q75_oevrwtr - q25_oevrwtr)),
  
  # Voor onder waterlijn
  iqr_wtrwtr = q75_wtrwtr - q25_wtrwtr,
  whisker_lower_wtrwtr = pmax(min_wtrwtr, q25_wtrwtr - 1.5 * (q75_wtrwtr - q25_wtrwtr)),
  whisker_upper_wtrwtr = pmin(max_wtrwtr, q75_wtrwtr + 1.5 * (q75_wtrwtr - q25_wtrwtr)),
  
  # Voor vaste bodem
  iqr_vastbodem = q75_vastbodem - q25_vastbodem,
  whisker_lower_vastbodem = pmax(min_vastbodem, q25_vastbodem - 1.5 * (q75_vastbodem - q25_vastbodem)),
  whisker_upper_vastbodem = pmin(max_vastbodem, q75_vastbodem + 1.5 * (q75_vastbodem - q25_vastbodem))
)]
# Sorteer de data en maak een geordende factor van sloot_cluster
tldk_summary <- tldk_summary[order(mean_tldk_oevrwtr)]
tldk_summary[, sloot_cluster := factor(sloot_cluster, levels = unique(sloot_cluster))]
# Maak tldk_long met whisker-range data
tldk_long <- melt(tldk_summary, 
                  id.vars = "sloot_cluster", 
                  measure.vars = list(
                    mean = c("mean_tldk_oevrwtr", "mean_tldk_wtrwtr", "mean_tldk_vastbodem"),
                    sd = c("sd_tldk_oevrwtr", "sd_tldk_wtrwtr", "sd_tldk_vastbodem"),
                    whisker_lower = c("whisker_lower_oevrwtr", "whisker_lower_wtrwtr", "whisker_lower_vastbodem"),
                    whisker_upper = c("whisker_upper_oevrwtr", "whisker_upper_wtrwtr", "whisker_upper_vastbodem")
                  ),
                  variable.name = "talud_type", 
                  value.name = c("mean", "sd", "whisker_lower", "whisker_upper"))

# Voeg labels toe
tldk_long[talud_type == 1, talud_label := "Boven waterlijn"]
tldk_long[talud_type == 2, talud_label := "Onder waterlijn"]
tldk_long[talud_type == 3, talud_label := "Vaste bodem"]

# Plot met whisker-range errorbars
ggplot(tldk_long, aes(x = sloot_cluster, y = mean, fill = talud_label)) +
  geom_col(position = "dodge", alpha = 0.7) +
  
  # Whisker-range errorbars (zoals boxplot whiskers)
  geom_errorbar(aes(ymin = whisker_lower, 
                    ymax = whisker_upper), 
                position = position_dodge(0.9), 
                width = 0.25, 
                color = "black", 
                alpha = 0.8, 
                size = 0.8) +
  
  # Okabe-Ito kleuren
  scale_fill_manual(values = okabe_ito_colors[1:3]) +
  
  coord_flip() +
  
  facet_wrap(~talud_label, scales = "free_x") +
  
  labs(
    title = "Taludhoeken oever per gebied", 
    subtitle = "Errorbars tonen whisker-range (Q1-1.5×IQR tot Q3+1.5×IQR)", 
    x = "Gebied", 
    y = "Taludhoek (°)"
  ) +
  
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "none", 
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    strip.text = element_text(size = 14, face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    strip.background = element_rect(colour = "black", fill = "white", linewidth = 0.8)
  )
# Opslaan van de plot
ggsave(file = 'output/AlleGebieden/Tussenrapportage/taludhoeken_whisker_range.png', 
       width = 45, height = 25, units = 'cm', dpi = 800)
### Vegetatie breedte zone-----------------------
ggplot(melt[par_eenheid == "breedte_cm_vegetatieopname" & !is.na(Gebiedsnaam) & zone %in% c('2a','2b') & value < 10000,], aes(x = zone, y = value, fill = paste0(zone))) +
    stat_summary(fun = mean, geom = "col", na.rm = TRUE) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, na.rm = TRUE) +
  
    facet_wrap(~str_wrap(Gebiedsnaam, width = 15), scales = "free", ncol = 8) +
    theme_minimal() +
    theme(text = element_text(size = 14),
          axis.ticks = element_line(colour = "black"),
          axis.line = element_line(colour = 'black'),
          axis.text.x = element_text(size = 12),
          strip.text = element_text(size = 12)  
    ) +
    scale_fill_brewer(palette = "Set2") +
    labs(x = "zone", y = paste0('-'), fill = 'zone') +
    ggtitle(paste0(unique('Breedte vegetatiezone')))
### relatie vegetatie waterbodemkwaliteit -----------------------
# Bereken aantal submerse soorten per locatie
submerse_soorten <- veg_srt[zone == "1" & Submerse_groeivorm > 25, .(
  n_submerse_soorten = uniqueN(wetnaam)
), by = .(SlootID, jaar)]

# Merge met abio data voor de relaties
relatie_data <- merge(abio_proj, submerse_soorten, by = c("SlootID","jaar"), all.x = TRUE)
relatie_data[is.na(n_submerse_soorten), n_submerse_soorten := 0]

# Filter data voor P-AL en P poriewater
pal_data <- relatie_data[!is.na(`P-AL mg p2o5/100g_SB`) & !is.na(n_submerse_soorten)]
p_poriewater_data <- relatie_data[!is.na(`P_µmol/l_PW`) & !is.na(n_submerse_soorten)]

# Functie voor verschillende statistische modellen (hergebruik van eerder)
fit_multiple_models <- function(data, x_var, y_var) {
  x <- data[[x_var]]
  y <- data[[y_var]]
  
  models <- list()
  r_squared <- list()
  
  # 1. Negatieve exponentiële fit (L-curve): y = a * exp(-b * x) + c
  tryCatch({
    neg_exp <- nls(y ~ a * exp(-b * x) + c, 
                   start = list(a = max(y), b = 0.01, c = min(y)),
                   control = nls.control(maxiter = 100, warnOnly = TRUE))
    models$neg_exp <- neg_exp
    pred <- predict(neg_exp)
    r_squared$neg_exp <- 1 - sum((y - pred)^2) / sum((y - mean(y))^2)
  }, error = function(e) {
    models$neg_exp <<- NULL
    r_squared$neg_exp <<- NA
  })
  
  # 2. Inverse relatie: y = a / (x + b) + c
  tryCatch({
    inverse <- nls(y ~ a / (x + b) + c,
                   start = list(a = max(y) * 10, b = 1, c = min(y)),
                   control = nls.control(maxiter = 100, warnOnly = TRUE))
    models$inverse <- inverse
    pred <- predict(inverse)
    r_squared$inverse <- 1 - sum((y - pred)^2) / sum((y - mean(y))^2)
  }, error = function(e) {
    models$inverse <<- NULL
    r_squared$inverse <<- NA
  })
  
  # 3. Power law: y = a * x^(-b) + c
  tryCatch({
    x_adj <- pmax(x, 0.1)  # Voorkom x=0
    power <- nls(y ~ a * x_adj^(-b) + c,
                 start = list(a = max(y), b = 0.5, c = min(y)),
                 control = nls.control(maxiter = 100, warnOnly = TRUE))
    models$power <- power
    pred <- predict(power)
    r_squared$power <- 1 - sum((y - pred)^2) / sum((y - mean(y))^2)
  }, error = function(e) {
    models$power <<- NULL
    r_squared$power <<- NA
  })
  
  # 4. Logistische curve: y = a / (1 + exp(b * (x - c))) + d
  tryCatch({
    logistic <- nls(y ~ a / (1 + exp(b * (x - c))) + d,
                    start = list(a = max(y) - min(y), b = 0.1, 
                                c = median(x), d = min(y)),
                    control = nls.control(maxiter = 100, warnOnly = TRUE))
    models$logistic <- logistic
    pred <- predict(logistic)
    r_squared$logistic <- 1 - sum((y - pred)^2) / sum((y - mean(y))^2)
  }, error = function(e) {
    models$logistic <<- NULL
    r_squared$logistic <<- NA
  })
  
  # 5. GAM met smooth spline (voor L-curve)
  library(mgcv)
  gam_model <- gam(y ~ s(x, k = 6), data = data.frame(x = x, y = y))
  models$gam <- gam_model
  r_squared$gam <- summary(gam_model)$r.sq
  
  # Selecteer beste model op basis van R²
  best_r2 <- which.max(unlist(r_squared))
  best_model_name <- names(r_squared)[best_r2]
  
  return(list(
    models = models,
    r_squared = r_squared,
    best_model = models[[best_model_name]],
    best_model_name = best_model_name,
    best_r2 = r_squared[[best_model_name]]
  ))
}

# Fit modellen voor P-AL en P poriewater
pal_models <- fit_multiple_models(pal_data, "P-AL mg p2o5/100g_SB", "n_submerse_soorten")
ammonium_models <- fit_multiple_models(pal_data, "NH4_µmol/l_PW", "n_submerse_soorten")
p_poriewater_models <- fit_multiple_models(p_poriewater_data, "P_µmol/l_PW", "n_submerse_soorten")
P_nalevering_formule2_models <- fit_multiple_models(pal_data, "P_nalevering_formule2", "n_submerse_soorten")

# Plot 1: ammonium slib met beste model
p1 <- ggplot(pal_data, aes(x = `NH4_µmol/l_PW`, y = n_submerse_soorten, col = Gebiedsnaam)) +
  geom_point(alpha = 0.6, size = 2) + # , color = "#0072B2"
  # Voeg beste model toe
  geom_smooth(method = "gam", formula = y ~ s(x, k = 6), 
              se = TRUE, color = "#D55E00", fill = "#D55E00", alpha = 0.2) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Ammonium slib",
    x = "NH4 slib (µmol/l)",
    y = "Aantal submerse soorten"
  ) +
  
  # Model informatie
  annotate("text", x = Inf, y = Inf, 
           label = paste0(ammonium_models$best_model_name, "\nR² = ", 
                         round(ammonium_models$best_r2, 3)), 
           hjust = 1.1, vjust = 1.5, size = 4, fontface = "bold")

# Plot 2: P-nalvering slib met beste model
p2 <- ggplot(pal_data, aes(x = P_nalevering_formule2, y = n_submerse_soorten, col = Gebiedsnaam)) +
  geom_point(alpha = 0.6, size = 2) +
  
  # Voeg beste model toe
  geom_smooth(method = "gam", formula = y ~ s(x, k = 6), 
              se = TRUE, color = "#D55E00", fill = "#D55E00", alpha = 0.2) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    panel.grid.minor = element_blank()
  ) +
  
  labs(
    title = "P-nalvering slib",
    x = "P-nalevering slib (mg/m2/l)",
    y = "Aantal submerse soorten"
  ) +
  
  # Model informatie
  annotate("text", x = Inf, y = Inf, 
           label = paste0(P_nalevering_formule2_models$best_model_name, "\nR² = ", 
                         round(P_nalevering_formule2_models$best_r2, 3)), 
           hjust = 1.1, vjust = 1.5, size = 4, fontface = "bold")

# Plot 3: P-AL slib met beste model
p3 <- ggplot(pal_data, aes(x = `P-AL mg p2o5/100g_SB`, y = n_submerse_soorten, col = Gebiedsnaam)) +
  geom_point(alpha = 0.6, size = 2) +
  
  # Voeg beste model toe
  geom_smooth(method = "gam", formula = y ~ s(x, k = 6), 
              se = TRUE, color = "#D55E00", fill = "#D55E00", alpha = 0.2) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    panel.grid.minor = element_blank()
  ) +
  
  labs(
    title = "P-AL slib",
    x = "P-AL slib (mg P2O5/100g)",
    y = "Aantal submerse soorten"
  ) +
  
  # Model informatie
  annotate("text", x = Inf, y = Inf, 
           label = paste0(pal_models$best_model_name, "\nR² = ", 
                         round(pal_models$best_r2, 3)), 
           hjust = 1.1, vjust = 1.5, size = 4, fontface = "bold")

# Plot 4: P poriewater met beste model  
p4 <- ggplot(p_poriewater_data, aes(x = `P_µmol/l_PW`, y = n_submerse_soorten, col = Gebiedsnaam)) +
  geom_point(alpha = 0.6, size = 2) +
  
  # Voeg beste model toe
  geom_smooth(method = "gam", formula = y ~ s(x, k = 6),
              se = TRUE, color = "#D55E00", fill = "#D55E00", alpha = 0.2) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    panel.grid.minor = element_blank()
  ) +
  
  labs(
    title = "P poriewater",
    x = "P poriewater (µmol/l)",
    y = "Aantal submerse soorten"
  ) +
  
  # Model informatie
  annotate("text", x = Inf, y = Inf, 
           label = paste0(p_poriewater_models$best_model_name, "\nR² = ", 
                         round(p_poriewater_models$best_r2, 3)), 
           hjust = 1.1, vjust = 1.5, size = 4, fontface = "bold")

# Combineer alle vier plots in een 2x2 grid
combined_relatie_plot_extended <- (p1 + p2) / (p3 + p4)

combined_relatie_plot_extended <- (p1 + p2) / (p3 + p4) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Relaties slibchemie en submerse plantendiversiteit",
    subtitle = "GAM smoothing met optimale model selectie",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    )
  ) &
  theme(legend.position = "bottom")

print(combined_relatie_plot_extended)

# Toon plot
print(combined_relatie_plot_extended)

# Print alle model resultaten
cat("P-AL slib modellen:\n")
for(i in names(pal_models$r_squared)) {
  if(!is.na(pal_models$r_squared[[i]])) {
    cat(paste0(i, ": R² = ", round(pal_models$r_squared[[i]], 3), "\n"))
  }
}

cat("\nP poriewater modellen:\n")
for(i in names(p_poriewater_models$r_squared)) {
  if(!is.na(p_poriewater_models$r_squared[[i]])) {
    cat(paste0(i, ": R² = ", round(p_poriewater_models$r_squared[[i]], 3), "\n"))
  }
}

# Sla uitgebreide plot op
ggsave(file = 'output/AlleGebieden/Tussenrapportage/Relatie_waterchemie_submerse_soorten_extended.png', 
       plot = combined_relatie_plot_extended,
       width = 30, height = 24, units = 'cm', dpi = 800)
### relatie oeversoorten en chemie oever/bodem -----------------------
# Data voorbereiden - filter voor beschikbare parameters
p_n_data <- abio_proj[!is.na(`P-AL mg p2o5/100g_OR_25`) & 
                      (!is.na(`N-NH4_CC_mg/kg_OR_25`) | 
                       !is.na(`N-NO3_CC_mg/kg_OR_25`) | 
                       !is.na(`A_N_RT_OR_25`) | 
                       !is.na(`P-PO4_CC_mg/kg_OR_25`))]

# Functie voor optimale modelselectie
fit_optimal_model <- function(data, x_var, y_var) {
  x <- data[[x_var]]
  y <- data[[y_var]]
  
  # Test verschillende modellen
  models <- list()
  aic_scores <- list()
  
  # 1. Lineair model
  tryCatch({
    lm_model <- lm(y ~ x, data = data.frame(x = x, y = y))
    models$linear <- lm_model
    aic_scores$linear <- AIC(lm_model)
  }, error = function(e) {
    models$linear <<- NULL
    aic_scores$linear <<- Inf
  })
  
  # 2. GAM met verschillende smoothing parameters
  library(mgcv)
  for(k in c(3, 4, 5, 6, 8)) {
    tryCatch({
      gam_model <- gam(y ~ s(x, k = k), data = data.frame(x = x, y = y))
      model_name <- paste0("gam_k", k)
      models[[model_name]] <- gam_model
      aic_scores[[model_name]] <- AIC(gam_model)
    }, error = function(e) {
      models[[model_name]] <<- NULL
      aic_scores[[model_name]] <<- Inf
    })
  }
  
  # 3. Polynomial modellen (2de en 3de graad)
  for(degree in 2:3) {
    tryCatch({
      poly_model <- lm(y ~ poly(x, degree), data = data.frame(x = x, y = y))
      model_name <- paste0("poly_", degree)
      models[[model_name]] <- poly_model
      aic_scores[[model_name]] <- AIC(poly_model)
    }, error = function(e) {
      models[[model_name]] <<- NULL
      aic_scores[[model_name]] <<- Inf
    })
  }
  
  # Selecteer beste model op basis van AIC
  best_model_name <- names(which.min(unlist(aic_scores)))
  best_model <- models[[best_model_name]]
  
  # Bereken R²
  if(grepl("gam", best_model_name)) {
    r_squared <- summary(best_model)$r.sq
  } else {
    r_squared <- summary(best_model)$r.squared
  }
  
  return(list(
    model = best_model,
    model_name = best_model_name,
    r_squared = r_squared,
    aic = aic_scores[[best_model_name]],
    all_aic = aic_scores
  ))
}

# Plot 1: P-AL

optimal_model_1 <- fit_optimal_model(p_n_data[!is.na(`P-AL mg p2o5/100g_OR_25`) & !is.na(n_soorten_oev_zone2)], "n_soorten_oev_zone2", "P-AL mg p2o5/100g_SB")

p1 <- ggplot(p_n_data, aes(x = n_soorten_oev_zone2, y = `P-AL mg p2o5/100g_OR_25`, col = Gebiedsnaam)) +
  geom_point(alpha = 0.6, size = 2) +
  
  # Voeg beste model toe met oranje kleur
  geom_smooth(method = "gam", formula = y ~ s(x, k = 6), 
              se = TRUE, color = "#D55E00", fill = "#D55E00", alpha = 0.2) +
  
  # Model informatie
  annotate("text", x = Inf, y = Inf, 
           label = paste0(optimal_model_1$model_name, "\nR² = ", 
                         round(optimal_model_1$r_squared, 3)), 
           hjust = 1.1, vjust = 1.5, size = 4, fontface = "bold") +
  
  labs(x = "Aantal soorten zone 2b", 
       y = "P-AL oever (mg P2O5/100g)",
       title = "P-AL") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    panel.grid.minor = element_blank()
  )

# Plot 2: P-CC
optimal_model_2 <- fit_optimal_model(p_n_data[!is.na(`P-PO4_CC_mg/kg_OR_25`) & !is.na(n_soorten_oev_zone2)], "n_soorten_oev_zone2", "P-PO4_CC_mg/kg_SB")

p2 <- ggplot(p_n_data, aes(x = n_soorten_oev_zone2, y = `P-PO4_CC_mg/kg_OR_25`, col = Gebiedsnaam)) +
  geom_point(alpha = 0.6, size = 2) +
  
  geom_smooth(method = "gam", formula = y ~ s(x, k = 6), 
              se = TRUE, color = "#D55E00", fill = "#D55E00", alpha = 0.2) +
  
  annotate("text", x = Inf, y = Inf, 
           label = paste0(optimal_model_2$model_name, "\nR² = ", 
                         round(optimal_model_2$r_squared, 3)), 
           hjust = 1.1, vjust = 1.5, size = 4, fontface = "bold") +
  
  labs(x = "Aantal soorten zone 2b", 
       y = "P-CC oever (mg/kg)",
       title = "P-CC") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    panel.grid.minor = element_blank()
  )

# Plot 3: N totaal
optimal_model_3 <- fit_optimal_model(p_n_data[!is.na(`A_N_RT_OR_25`) & !is.na(n_soorten_oev_zone2)], "n_soorten_oev_zone2", "A_N_RT_OR_25")

p3 <- ggplot(p_n_data, aes(x = n_soorten_oev_zone2, y = `A_N_RT_OR_25`, col = Gebiedsnaam)) +
  geom_point(alpha = 0.6, size = 2) +
  
  geom_smooth(method = "gam", formula = y ~ s(x, k = 6), 
              se = TRUE, color = "#D55E00", fill = "#D55E00", alpha = 0.2) +
  
  annotate("text", x = Inf, y = Inf, 
           label = paste0(optimal_model_3$model_name, "\nR² = ", 
                         round(optimal_model_3$r_squared, 3)), 
           hjust = 1.1, vjust = 1.5, size = 4, fontface = "bold") +
  
  labs(x = "Aantal soorten zone 2b", 
       y = "N totaal oever 25cm (g/kg)",
       title = "N totaal") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    panel.grid.minor = element_blank()
  )

# Plot 4: NH4-N
optimal_model_4 <- fit_optimal_model(p_n_data[!is.na(`N-NH4_CC_mg/kg_OR_25`) & !is.na(n_soorten_oev_zone2)], "n_soorten_oev_zone2", "N-NH4_CC_mg/kg_OR_25")

p4 <- ggplot(p_n_data, aes(x = n_soorten_oev_zone2, y = `N-NH4_CC_mg/kg_OR_25`, col = Gebiedsnaam)) +
  geom_point(alpha = 0.6, size = 2) +
  
  geom_smooth(method = "gam", formula = y ~ s(x, k = 6), 
              se = TRUE, color = "#D55E00", fill = "#D55E00", alpha = 0.2) +
  
  annotate("text", x = Inf, y = Inf, 
           label = paste0(optimal_model_4$model_name, "\nR² = ", 
                         round(optimal_model_4$r_squared, 3)), 
           hjust = 1.1, vjust = 1.5, size = 4, fontface = "bold") +
  
  labs(x = "Aantal soorten zone 2b", 
       y = "NH4-N oever 25cm (mg/kg)",
       title = "NH4-N") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    panel.grid.minor = element_blank()
  )

# Combineer alle vier plots in een 2x2 grid
combined_p_n_plot <- (p1 + p2) / (p3 + p4)

combined_p_n_plot <- combined_p_n_plot + 
 plot_layout(guides = "collect") +
  
  plot_annotation(
    title = "Relaties tussen soortenaantal zone 2b en bodemchemie",
    subtitle = "GAM smoothing met optimale model selectie",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    )
  )

# Toon de plot
print(combined_p_n_plot)

# Opslaan
ggsave(file = 'output/AlleGebieden/Tussenrapportage/Soortenaantal_bodemchemie_optimum.png', 
       plot = combined_p_n_plot, width = 30, height = 24, units = 'cm', dpi = 800)

### Kaart gebieden-----------------------
bbox <- st_bbox(gebiedkop)
nederland_background <- st_as_sfc(bbox)
gebiedkop <- st_as_sf(gebiedkop) %>% st_transform(crs = 28992)
gebiedkop_labels <- gebiedkop %>%
  distinct(Gebiedsnaam, WP, .keep_all = TRUE)

# Okabe-Ito kleurenpalet
okabe_ito_colors <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999",
  "#661100", "#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677",
  "#882255", "#AA4499", "#DDDDDD", "#000000", "#AA4466", "#4477AA", "#66CCEE", "#228833",
  "#CCBB44", "#EE6677", "#AA3377", "#BBBBBB", "#EE3377", "#11AA99", "#3366CC", "#CCAA44",
  "#EE7733", "#5588FF"
)

# Pas het aantal kleuren aan op basis van het aantal gebieden
n_areas <- length(unique(gebiedkop$Gebiedsnaam[!is.na(gebiedkop$Gebiedsnaam)]))
okabe_colors_subset <- rep(okabe_ito_colors, length.out = n_areas)

# deelkaart met waterschappen
deelkaart <- c("09", "14", "15", "39", "13", "31", "12", "59", "02")
# deelkaart <- c("14", "15", "39", "13", "31")
# deelkaart <- c("12","31") #HHNK #AGV
# deelkaart <- c("31") #AGV
# deelkaart <- c("59", "02")
bbox <- st_bbox(waterschappen[waterschappen$wbh_code_objectidentificatie %in% deelkaart,])

ggplot() +
   ggspatial::annotation_map_tile(type= "osm", cachedir =  '/osm_cache',
                                   zoomin = 1, progress = c("none"),
                                   quiet = TRUE) +
  # Gebiedkop areas with labels
  geom_sf(data = st_crop(gebiedkop[!is.na(gebiedkop$Gebiedsnaam),],bbox), aes(fill = Gebiedsnaam, label = Gebiedsnaam), alpha = 0.6, color = "black", size = 0.2, show.legend = TRUE) +
  # Waterschapsgrenzen (waterboard boundaries)
  geom_sf(data = waterschappen[waterschappen$wbh_code_objectidentificatie %in% deelkaart,], fill = NA, color = "blue", alpha = 0.3, size = 0.2) +
  facet_wrap(~WP, labeller = labeller(WP = c(
    "WP1" = "Inventarisatiesloten",
    "WP2" = "Inventarisatiesloten en beheerexperimenten"
  ))) +
  # Add labels for gebiedsnaam
  geom_label_repel(
    data = st_crop(gebiedkop_labels,bbox),
    aes(label = Gebiedsnaam, geometry = geometrie2d),
    stat = "sf_coordinates",
    size = 4,                    # Grotere tekst
    max.overlaps = 5,           # Meer labels tonen
    force = 2,                   # Krachtiger uit elkaar duwen
    force_pull = 1,              # Aantrekkingskracht naar punten
    box.padding = 0.4,           # Meer ruimte rond labels
    point.padding = 0.5,
    min.segment.length = 0,
    segment.size = 1,          # Dunnere verbindingslijnen
    segment.alpha = 0.8          # Transparantere lijnen
  )+
  # Okabe-Ito kleuren toevoegen
  scale_fill_manual(values = okabe_colors_subset) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.labels = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  ) +
  labs(title = "Bemonsterde gebieden met inventarisatie- en proefslootlocaties") +
  coord_sf(
    crs = 28992,
    expand = FALSE
  )


### Kaart meetlocaties--------------------------------------------
# 1. Filter locaties op RH gebied
locaties_rh <- locaties |>
  filter(gebied == "RH") |>
  filter(WP %in% c("WP1", "WP2")) |>
  mutate(
    afrastering = grepl("AF", Behandeling),
    kreeft = grepl("KR", Behandeling)
  )

# 2. Kleuren op basis van M of R in Behandeling
okabe_ito <- c(
  "#E69F00", "#56B4E9", "#009E73", 
  "#F0E442", "#0072B2", "#D55E00", 
  "#CC79A7", "#000000"
)
behandelingen_rh <- unique(na.omit(locaties_rh$Behandeling))
behandeling_fill <- setNames(
  okabe_ito[seq_along(behandelingen_rh)],
  behandelingen_rh
)
# 3. Labels: centroïd per uniek slootnummer
locaties_labels_rh <- locaties_rh |>
  group_by(Sloot_nr) |>
  slice(1) |>
  ungroup() |>
  st_centroid()
coords_rh <- st_coordinates(locaties_labels_rh)
locaties_labels_rh <- locaties_labels_rh |>
  st_drop_geometry() |>
  mutate(
    x = coords_rh[, 1],
    y = coords_rh[, 2],
    nudge_x = ifelse(Sloot_nr %in% c(5, 8), 400, -400)
  )
# 4. Bbox op basis van locaties_rh
bbox <- st_bbox(locaties_rh)

ggplot() +
  ggspatial::annotation_map_tile(
    type = "osm", cachedir = '/osm_cache',
    zoomin = 1, progress = "none", quiet = TRUE
  ) +
  geom_sf(
    data = locaties_rh,
    aes(color = Behandeling),
    linewidth = 2
  ) +
  scale_color_manual(
    name = "Behandeling",
    values = behandeling_fill,
    na.value = "grey80"
  ) +
  geom_label_repel(
    data = locaties_labels_rh,
    aes(x = x, y = y, label = Sloot_nr),
    nudge_x = locaties_labels_rh$nudge_x,
    size = 5,
    fontface = "bold",
    direction = "y",
    segment.color = "black",
    segment.size = 0.4,
    box.padding = 0.3,
    min.segment.length = 0,
    max.overlaps = 30
  ) +
  coord_sf(
    xlim = c(bbox["xmin"] - 200, bbox["xmax"] + 200),
    ylim = c(bbox["ymin"] - 200, bbox["ymax"] + 200),
    crs = 28992
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  labs(title = "Proeflocaties met behandeling en slootnummer")

### Kaart kleipercentage-----------------------
# Voeg kleigehalte toe aan locaties vanuit abio_proj
locaties_clay <- st_as_sf(abio_proj) 
locaties_clay <- locaties_clay[!st_is_empty(locaties_clay), ]
locaties_clay <- locaties_clay[,c("SlootID", "Sloot_nr", "Behandeling", "Z_CLAY_SA_OR_50", "geom")]
st_bbox(st_as_sf(locaties_clay))
locaties_labels <- locaties_clay |>
  group_by(Sloot_nr) |>
  slice(1) |>
  ungroup()
# Kleuren op basis van M of R in Behandeling
behandeling_kleuren <- setNames(
  ifelse(grepl("M", unique(locaties_clay$Behandeling)), "darkgreen", "red"),
  unique(locaties_clay$Behandeling)
)
ggplot() +
  ggspatial::annotation_map_tile(type = "osm", cachedir = '/osm_cache',
                                  zoomin = 1, progress = "none",
                                  quiet = TRUE) +
  geom_sf(
    data = locaties_clay,
    aes(color = Behandeling, fill = Z_CLAY_SA_OR_50),
    shape = 21, size = 5, stroke = 1.0
  ) +
  scale_fill_viridis_c(option = "D", name = "Kleigehalte 50cm (%)",,
    limits = c(0, 30),
    breaks = seq(0, 30, by = 5)
  ) +
  scale_color_manual(
    name = "Behandeling",
    values = behandeling_kleuren
  ) +
  geom_label_repel(
    data = locaties_labels,
    aes(label = Sloot_nr, geometry = geom),
    stat = "sf_coordinates",
    size = 5,
    fontface = "bold",
    nudge_x = -400,
    direction = "y",
    segment.color = "black",
    segment.size = 0.4,
    box.padding = 0.3,
    min.segment.length = 0
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  labs(title = "Locaties met Behandeling, slootnummer en kleigehalte 50cm")



## 5.2 plot profiel--------------
### pairs ------------------
setDT(profiel_wide)
pairs(profiel_wide[,3:9])

# "tldk_bvwtr_perc" = 3 meter van de oeverlijn/ waterlijn de oever op (niet verder dan insteek)
# "tldk_ondwtr_perc" = 1 meter het water in bovenkant slib
# "tldk_vastbodem_perc" = 1 meter water in onderkant slib
# "tldk_wtrwtr_perc"  = hoek tot bodem 25 cm lager dan waterlijn, bij missend = onderwater
# "tldk_oevrwtr_perc" = hoek tot oever 25 hoger dan waterlijn, bij missend = oever
# "mean_talud_oever" = gemiddelde hoek insteek waterlijn
# "mean_talud_water" = gemiddelde hoek onder water
# "mean_talud_os_water" = gemiddelde hoek onderkant slib

ggplot(data = profiel_wide, aes(x= tldk_vastbodem_perc, y=drglg ))+
  geom_jitter()+
  geom_smooth(method="loess") +
  # ylim(-10,200)+
  # stat_regline_equation(label.x=200, label.y=470)+
  stat_cor(aes(label=..rr.label..), label.x=0.5, label.y=0.5)+
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
  ggtitle("Profiel") +
  labs(x= "drooglegging (cm)",y="taludhoek onderkant slib (%)")

### loop profielplaatjes ------------------
profiel <- merge(profiel, locaties, by = c("SlootID","jaar"), all.x = TRUE)
for(i in unique(profiel$ID)){
  setDT(profiel)
  visualise_profiel(profiel[ID == i,])
  print(i)
}
# als water niet aansluit dan is Z op oever lager dan de waterlijn


## 5.3 plot clusters------------
abio_proj[!behandeling_1 =='WP1', behandeling_1 := 'WP2']
ggplot() +
  # geom_boxplot(data = abio_proj, aes(x = gebied, y = clusters)) + 
  geom_histogram(data = abio_proj, aes(cluster_def, fill = behandeling_1), col = 'black', binwidth = 1)+
  # geom_histogram(data = abio_proj, aes(cluster, fill = behandeling_1), col = 'black', binwidth = 1)+
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size=15), 
    strip.text.y = element_text(size = 10), 
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size= 15),
    axis.ticks =  element_line(colour = "black"),
    plot.title = element_text(size =15, face="bold", hjust = 0.5),
    axis.title = element_text(size=15),
    panel.background = element_blank(),
    plot.background = element_blank(),
  )+
  ggtitle("") +
  labs(x= "veensloottype",y="aantal monsters")
ggsave(file=paste0('output/clusters/clusters.png'),width = 25, height = 15,units='cm',dpi=800)

### boxplots all cluster all parcels NL--------------------
melt <- melt(setDT(cluster), id.vars = c("clusters"), 
             measure.vars = c( "drlg", "breedtewl", "frac_water" ,"trofie" , "A_SOM_LOI","A_CLAY_MI" ))
melt$clusters <- as.factor(melt$clusters)
melt <- melt[!(melt$variable == "breedtewl" & melt$value > 20),]
namenind <- c("drooglegging", "slootbreedte", 
              "fractie oppervlak waterlopen", "trofiegraad veen","organische stof %","klei %")
melt[, level := factor(variable, labels = namenind)]
setDT(melt)
d1 <-  melt[,.(percentiel25 = quantile(value, probs =c(0.25), na.rm = TRUE, digits = 2),mediaan = quantile(value, probs =c(0.5), na.rm = TRUE, digits = 2),percentiel75 = quantile(value, probs =c(0.75), na.rm =TRUE, digits = 2)), by = c('clusters','variable','level')]
d1<- dcast(d1,clusters ~ variable, value.var = c('percentiel25','mediaan','percentiel75'), fun.aggregate = mean)

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

### clust by data -------------------------
# melt data table voor boxplots
melt <- melt(setDT(abio_proj), id.vars = c("clusters_clustber"), 
             measure.vars = c('drglg','watbte','trofie',"TOC [%]_25" ,"Z_CLAY_SA_25" ))
melt$clusters <- as.factor(melt$clusters)
namenind <- c("drooglegging", "slootbreedte","trofiegraad veen","organisch stof","kleigehalte")
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
    strip.text.x = element_text(size = 12), 
    strip.text.y = element_text(size = 12), 
    axis.text.x = element_text(size = 12,angle=90,hjust=1),
    axis.text.y = element_text(size = 12),
    axis.ticks =  element_line(colour = "black"),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    plot.background = element_blank(),
    legend.position="none"
  )+
  guides(fill="none")+
  ggtitle('') +
  #guides(col = '') +
  labs(x= '' , y= '')
ggplotly(p)

### relatie clustervar kaart en gemeten---------------
setDT(abio_proj)
abio_proj[,cluster_def := as.numeric(cluster_def)]
abio_proj[,clusters := as.numeric(clusters)]
ggplot() +
  geom_jitter(data = abio_proj, aes(x = clusters, y = cluster_def, col = waterschap), size = 3) +
  # xlim(0,0.8)+ ylim(0,0.8)+
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
  labs(x= "cluster obv veensloottypen",y="cluster obv meting")

#drooglegging
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

#os
ggplot() +
  geom_jitter(data = abio_proj, aes(x = A_SOM_LOI, y = OS_perc_OR_25, col = waterschap), size = 3) +
  # xlim(0,0.8)+ ylim(0,0.8)+
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
  labs(x= "organisch stof obv BodemSchat",y="organisch stof obv meting")

## 5.4 plot relaties database --------------------
### pairs ---------------------------
pairs(abio_proj[,c("holleoever","water_conductiviteit_uS_cm","water_redox","water_pH","max_slib","max_wtd","slib_pH","oever_[0,10]" ,"oever_(30,40]" ) ,with=FALSE])
pairs(abio_proj[,c("holleoever","water_pH","max_slib","max_wtd","oever_(30,40]", "slib_redox_mgL"),with=FALSE], col = abio_proj$clusters_clustber)
pairs(abio_proj[,c("holleoever","oever_(30,40]","Oeverzone_2b_emers_perc","max_slib","max_wtd","Waterzone_1_subm_tot_perc"),with=FALSE], col = abio_proj$clusters_clustber)
pairs(abio_proj[,c("drglg","Oeverzone_2b_breedte_cm","Oeverzone_2a_breedte_cm"),with=FALSE], col = abio_proj$clusters_clustber)
pairs(abio_proj[,c("holleoever","drglg","slib_redox_mgL","water_pH","max_wtd") ,with=FALSE])

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

### specifieke relaties-------------------------------
# poriewater P en kroos
p <- ggplot(data = abio_proj, aes(x= `P_µmol/l_PW` , y= Waterzone_1_kroos_perc))+
  geom_jitter(aes(col= gebied, text = SlootID))+
  geom_smooth(method="gam") +
  # stat_regline_equation(label.x=200, label.y=470)+
  stat_cor(aes(label=after_stat(rr.label)), label.x=200, label.y=20)+
  # ylim(0,150)+
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
  ggtitle("profiel") +
  labs(x= "P poriewater (µmol/l)",y="kroos (%)")
ggplotly(p, tooltip = c('text'))

# poriewater P en algen
p <- ggplot(data = abio_proj, aes(x= `P_µmol/l_PW` , y= `Chl-a totaal_ug/l_OW`))+
  geom_jitter(aes(col= gebied, text = SlootID))+
  geom_smooth(method="gam") +
  # stat_regline_equation(label.x=200, label.y=470)+
  stat_cor(aes(label=after_stat(rr.label)), label.x=200, label.y=100)+
  ylim(0,150)+
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
  ggtitle("profiel") +
  labs(x= "P poriewater (µmol/l)",y="algen (µmol/l)")
ggplotly(p, tooltip = c('text'))

# poriewater P en redox
p <- ggplot(data = abio_proj, aes(x= `P_µmol/l_PW` , y= slib_redox_mgL))+
  geom_jitter(aes(col= gebied, text = SlootID))+
  geom_smooth(method="gam") +
  # stat_regline_equation(label.x=200, label.y=470)+
  stat_cor(aes(label=after_stat(rr.label)), label.x=200, label.y=20)+
  # ylim(0,150)+
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
  ggtitle("profiel") +
  labs(x= "P poriewater (µmol/l)",y="redox")
ggplotly(p, tooltip = c('text'))

#waterdiepte drooglegging
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

# drooglegging 
# breedte oever watbte, max diepte max_wtd
# breedte aquatische oever (cm) Oeverzone_2a_breedte_cm
# tldk_vastbodem_perc tldk_oevrwtr_perc
p <- ggplot(data = abio_proj, aes(x= drglg , y= max_wtd))+
  geom_jitter(aes(col= gebied, text = SlootID))+
  geom_smooth(method="gam") +
  # stat_regline_equation(label.x=200, label.y=470)+
  stat_cor(aes(label=after_stat(rr.label)), label.x=0.5, label.y=0.5)+
  # ylim(0,150)+
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
  ggtitle("profiel") +
  labs(x= "drooglegging (m)",y="waterdiepte (m)")
ggplotly(p, tooltip = c('text'))

p <- ggplot(data = abio_proj, aes(x= drglg , y= holleoever))+
  geom_jitter(aes(col= gebied, text = SlootID))+
  geom_smooth(method="gam") +
  # stat_regline_equation(label.x=200, label.y=470)+
  stat_cor(aes(label=after_stat(rr.label)), label.x=0.5, label.y=0.5)+
  # ylim(0,150)+
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
  ggtitle("profiel") +
  labs(x= "drooglegging (m)",y="onderholling (m)")
ggplotly(p, tooltip = c('text'))

## 5.4a abio per gebied en per parameter -----------------------
# fingerprint per par, eenheid en methode alle gebieden
for(j in unique(melt$par_eenheid)){
  # j <- unique(melt$par_eenheid)[158]
  melt_sel <- melt[par_eenheid %in% j,]
  # sel only pars with or & two depths
  if(uniqueN(melt_sel$monsterdiepte)>=2& uniqueN(melt_sel$compartiment)>=2){
  var_folder <- gsub('/','_',j)
  var_folder <- gsub('%','perc',var_folder)
  
  
  ggplot(melt_sel, aes(x = compartiment, y = value, fill = paste0(compartiment, ' ',monsterdiepte))) +
    facet_wrap(gebied~., scales = "free",
               nrow = 3) +
    stat_boxplot(geom = 'errorbar') +
    geom_boxplot(outliers = FALSE) +
    theme_minimal() +
    theme(text = element_text(size = 14),
          axis.ticks =  element_line(colour = "black"),
          axis.line = element_line(colour='black'),
          axis.text.x = element_text(size = 14, angle = 45,hjust=1)
          
    ) +
    scale_fill_manual(values = okabe_ito_colors) +
    labs(x = "compartiment", y = paste0(unique(melt_sel$eenheid)), fill = 'compartiment en monsterdiepte')+
    ggtitle(paste0(unique(melt_sel$parameter)," ",unique(melt_sel$methode))) 
  ggsave(file=paste0('output/AlleGebieden/fingerprints/',var_folder ,'.png'), width = 35,height = 15,units='cm',dpi=800)
  } 
}
### loop per parameter, compartiment, monsterdiepte alle gebieden
for(i in unique(melt$variable)){
  # i <- unique(melt$variable)[306]
  melt_sel <- melt[variable %in% i,]
  var_folder <- gsub('/','_',unique(melt_sel$variable))
  var_folder <- gsub('%','perc',var_folder)
  
  ggplot() +
    geom_histogram(data = melt_sel, aes(value), col = 'black') +
    facet_wrap(jaar~.) +
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
    guides(fill=guide_legend(title=unique(melt_sel$varnames)))+
    ggtitle(unique(melt_sel$varnames)) +
    labs(x= paste0(unique(melt_sel$eenheid)), y=paste0("aantal monstertrajecten (n= " , uniqueN(melt_sel$SlootID),') '))
  ggsave(file=paste0('output/AlleGebieden/histogram/',var_folder,unique(melt_sel$methode),'.png'), width = 15,height = 10,units='cm',dpi=800)

  ggplot() +
   geom_boxplot(data = melt_sel, aes(x = gebied,  y = value), binwidth = 0.1, outliers = FALSE) + 
   facet_wrap(jaar~., nrow = 2) +
   theme_minimal()+
   theme(
    strip.background = element_blank(),
    strip.text.y = element_text(size = 12), 
    axis.text = element_text(size = 14, angle = 45),
    axis.title = element_text(size= 15),
    axis.ticks =  element_line(colour = "black"),
    plot.title = element_text(size =15, face="bold", hjust = 0.5),
    panel.background = element_blank(),
    plot.background = element_blank(),
  )+
  # guides(x= ggh4x::guide_axis_nested(delim = "&"))+
  ggtitle(melt_sel$varnames) +
  labs(x= 'gebied', y=paste0(unique(melt_sel$eenheid)))
  ggsave(file=paste0('output/AlleGebieden/boxplot/',var_folder,unique(melt_sel$methode),'.png'),width = 35,height = 15,units='cm',dpi=800)


}
### loop per parameter, compartiment, monsterdiepte alle gebieden - uitgelicht per gebied
for(i in unique(melt$variable)){
  # i <- unique(melt$variable)[148]
  melt_sel <- melt[variable %in% i,]
  var_folder <- gsub('/','_',unique(melt_sel$variable))
  var_folder <- gsub('%','perc',var_folder)
  
  for( j in unique(melt_sel$gebied)){
    # j <- unique(melt_sel$gebied)[22] # ronde hoep
    melt_sel[, fillcol := "alles VeeST"]
    melt_sel[gebied == j, fillcol := Gebiedsnaam]
      
  ggplot() +
    geom_histogram(data = melt_sel, aes(value, fill = fillcol)) +
    scale_fill_manual(values = c("black", "grey")) +
    facet_wrap(jaar~.) +
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
    guides(fill=guide_legend(title=unique(melt_sel$varnames)))+
    ggtitle(unique(melt_sel$varnames)) +
    labs(x= paste0(unique(melt_sel$eenheid)), y=paste0("aantal monstertrajecten (n= " , uniqueN(melt_sel$SlootID),') '))
  
    # Create directory if it doesn't exist and save in one step
  output_path <- paste0('output/', j, '/histogram/', var_folder, '_',unique(melt_sel$methode), '.png')
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  ggsave(file = output_path, width = 25, height = 15, units = 'cm', dpi = 800)
  
    
  ggplot() +
   geom_boxplot(data = melt_sel, aes(x = fillcol,  y = value), binwidth = 0.1, outliers = FALSE) + 
   facet_wrap(jaar~.) +
   theme_minimal()+
   theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 16), 
    axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA),
    axis.text = element_text(size = 14),
    axis.title = element_text(size= 15),
    axis.ticks =  element_line(colour = "black"),
    plot.title = element_text(size =15, face="bold", hjust = 0.5),
    panel.background = element_blank(),
    plot.background = element_blank(),
  )+
  # guides(x= ggh4x::guide_axis_nested(delim = "&"))+
  ggtitle(melt_sel$varnames) +
  labs(x= 'gebied', y=paste0(unique(melt_sel$eenheid)))
  
  output_path <- paste0('output/', j, '/boxplot/', var_folder, '_',unique(melt_sel$methode), '.png')
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  ggsave(file = output_path, width = 25, height = 15, units = 'cm', dpi = 800)
  }

}

## 5.4b loop per parameter en gebied en waterbodemdata---------------
### loop per gebied voor specifieke parameters in water en bodem die relevant zijn voor veensloten
for(i in unique(melt$gebied)){
# i <- unique(melt$gebied)[22] # ronde hoep
sel1 <- c("PO4-CC_mg P/kg_calciumchloride", "Fe_mg/kg_calciumchloride","Fe/P_CC_calciumchloride",
          "Fe2O3_g/kg_xrf","SO3_g/kg_xrf", "Fe/S__xrf",
          "P_µmol/l_PW", "Fe_µmol/l_PW", "Fe/P_PW_PW",
          "Fe/S_DW_PW", "Fe_mmol/kg DW_PW", "S_mmol/kg DW_PW",
          "NH4_µmol/l_PW","S_µmol/l_PW")

melt_sel <- melt[par_eenheid %in% sel1,]
melt_sel[,Sloot_nr:= as.character(Sloot_nr)]

#### ammonium tox-----------------
#nh4 range van 0.9-907 umol/l (mediaan= 330, q90 = 341), max 28 mgN/l = 200 umol
ammonium <- data.frame(xmin = -Inf, 
                    xmax = Inf,
                    ymin = c(0,100,1000,2000),  
                    ymax = c(100,1000,2000,Inf),
                    fill = c("blue","yellow", "orange","red"),
                    label = c("niet", "voor gevoelige soorten","voor veel soorten","voor bijna alle soorten"))

legend_colors <- setNames(c("blue","yellow", "orange","red"), ammonium$label)

ggplot() +
  geom_rect(data= ammonium, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), 
            inherit.aes = FALSE, alpha = 0.15) +
  scale_fill_identity('Giftig:',breaks = legend_colors, labels = c("niet", "voor gevoelige soorten","voor veel soorten","voor bijna alle soorten"), guide = guide_legend(override.aes = list(alpha = 0.15)))+
  geom_col(data = melt_sel[variable %in% c('NH4_µmol/l_PW') & gebied %in% i],
           aes(x= SlootID, y = value), fill = "#1B9E77",
           position = 'dodge', alpha = 0.7)+
  facet_grid(jaar ~ Gebiedsnaam, space = 'free_x', scales = 'free_x', switch = 'x')+
  theme_minimal(base_size = 15)+
  theme(
    strip.background = element_blank(),
    strip.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 14, vjust = 0.5, hjust = 1, angle = 90),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.ticks = element_line(colour = "black"),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    plot.background = element_blank(),
    legend.position = "right",
    legend.box.just = "center"
  )+
  ggtitle('') +
  labs(x= 'gebied & slootnummer' , y= 'µmol/l')
ggsave(file=paste0('output/',i,'/waterbodem/',i,'_nh4_tox.png'), width = 25,height = 15,units='cm',dpi=800)

#### S tox--------------
#S range van 0.5-358 umol/l (mediaan=18.4, q90 = 53.2). 

sulfide <- data.frame(xmin = -Inf, 
                       xmax = Inf,
                       ymin = c(0,10,40,200),  
                       ymax = c(10,40,200,Inf),
                       fill = c("blue","yellow", "orange","red"),
                       label = c("niet", "voor gevoelige soorten","voor veel soorten","voor bijna alle soorten"))

legend_colors <- setNames(c("blue","yellow", "orange","red"), sulfide$label)

ggplot() +
  geom_rect(data= sulfide, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), 
            inherit.aes = FALSE, alpha = 0.15) +
  scale_fill_identity('Mogelijk giftig:',breaks = legend_colors, labels = c("niet", "voor gevoelige soorten","voor veel soorten","voor bijna alle soorten"), guide = guide_legend(override.aes = list(alpha = 0.15)))+
  geom_col(data = melt_sel[variable %in% c('S_µmol/l_PW') & gebied %in% i],
           aes(x= Sloot_nr, y = value), fill = "#1B9E77",
           position = 'dodge', alpha = 0.7)+
  facet_grid(.~ Gebiedsnaam, space = 'free_x', scales = 'free_x', switch = 'x')+
  theme_minimal(base_size = 15)+
  theme(
    strip.background = element_blank(),
    strip.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 14, vjust = 0.5, hjust = 1, angle = 90),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.ticks = element_line(colour = "black"),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    plot.background = element_blank(),
    legend.position = "right",
    legend.box.just = "center"
  )+
  ggtitle('') +
  labs(x= 'gebied & slootnummer' , y= 'µmol/l')
ggsave(file=paste0('output/',i,'/waterbodem/',i,'_S_tox.png'), width = 25,height = 15,units='cm',dpi=800)

melt_sel <- dcast(melt_sel, Sloot_nr+Gebiedsnaam+gebied~variable, value.var = 'value', fun.aggregate = mean)

#### P cc fe/s xrf -------------
ggplot() +
  geom_point(data = melt_sel, aes(x= feS_XRF_SB_SB, y = `P-PO4_CC_mg/kg`), col = 'lightgrey', size = 5) +
  geom_point(data = melt_sel[gebied %in% i,], aes(x= feS_XRF_SB_SB, y = `P-PO4_CC_mg/kg`, col = Sloot_nr), size = 5) +
  # ylim(0,max(melt_sel[gebied %in% i,`P-PO4_CC_mg/kg`]*1.01))+
  xlim(0,max(melt_sel[gebied %in% i,feS_XRF_SB_SB]*1.01))+
  scale_colour_manual(values =c("#1B9E77","#7570B3","#E6AB02","#D95F02","#E7298A",
                                "#000000", "#E69F00","#56B4E9","#009E73" ,"#F0E442" ,
                                "#0072B2", "#D55E00", "#CC79A7","#999999",  
                                "#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                                "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888"))+
  theme_minimal(base_size = 15)+
  theme(
    strip.background = element_blank(),
    strip.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 14, vjust = 0.5, hjust = 1, angle = 90),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.ticks = element_line(colour = "black"),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    plot.background = element_blank(),
    legend.position = "right",
    legend.box.just = "center"
  )+
  ggtitle('') +
  labs(x= 'Fe/S ratio slib (molbasis)' , y= 'P-PO4 CC (mg/kg)', col = 'Sloot nummer' )
ggsave(file=paste0('output/',i,'/waterbodem/',i,'_PversusFeS_CC_XRF.png'), width = 25,height = 15,units='cm',dpi=800)

#### p versus feP - ccP cc fe/s xrf -----------
ggplot() +
  geom_point(data = melt_sel, aes(x= feP_CC_SB_SB, y = `P-PO4_CC_mg/kg`), col = 'lightgrey', size = 5) +
  geom_point(data = melt_sel[gebied %in% i,], aes(x= feP_CC_SB_SB, y = `P-PO4_CC_mg/kg`, col = Sloot_nr), size = 5) +
  # ylim(0,max(melt_sel[gebied %in% i,`P-PO4_CC_mg/kg`]*1.01))+
  xlim(0,max(melt_sel[gebied %in% i,feP_CC_SB_SB]*1.01))+
  
  scale_colour_manual(values =c("#1B9E77","#7570B3","#E6AB02","#D95F02","#E7298A",
                                "#000000", "#E69F00","#56B4E9","#009E73" ,"#F0E442" ,
                                "#0072B2", "#D55E00", "#CC79A7","#999999",  
                                "#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                                "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888"))+
  theme_minimal(base_size = 15)+
  theme(
    strip.background = element_blank(),
    strip.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 14, vjust = 0.5, hjust = 1, angle = 90),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.ticks = element_line(colour = "black"),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    plot.background = element_blank(),
    legend.position = "right",
    legend.box.just = "center"
  )+
  ggtitle('') +
  labs(x= 'Fe/P ratio (molbasis)' , y= 'P-PO4 CC (mg/kg)', col = 'Sloot nummer' )
ggsave(file=paste0('output/',i,'/waterbodem/',i,'_PversusFeP_CC.png'), width = 25,height = 15,units='cm',dpi=800)

#### p versus feS - bware -------------
ggplot() +
  geom_point(data = melt_sel, aes(x= `Fe/P_PW`, y = `P_µmol/l_PW`), col = 'lightgrey', size = 5) +
  geom_point(data = melt_sel[gebied %in% i,], aes(x= `Fe/P_PW`, y = `P_µmol/l_PW`, col = Sloot_nr), size = 5) +
  # ylim(0,max(melt_sel[gebied %in% i,`P_µmol/l_PW`]*1.01))+
  xlim(0,max(melt_sel[gebied %in% i,`Fe/P_PW`]*1.01))+
  scale_colour_manual(values =c("#1B9E77","#7570B3","#E6AB02","#D95F02","#E7298A",
                                "#000000", "#E69F00","#56B4E9","#009E73" ,"#F0E442" ,
                                "#0072B2", "#D55E00", "#CC79A7","#999999",  
                                "#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                                "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888"))+
  
  theme_minimal(base_size = 15)+
  theme(
    strip.background = element_blank(),
    strip.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 14, vjust = 0.5, hjust = 1, angle = 90),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.ticks = element_line(colour = "black"),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    plot.background = element_blank(),
    legend.position = "right",
    legend.box.just = "center"
  )+
  ggtitle('') +
  labs(x= 'Fe/P ratio poriewater (molbasis)' , y= 'P poriewater (µmol/l)', col = 'Sloot nummer' )
ggsave(file=paste0('output/',i,'/waterbodem/',i,'_PversusFeP_Bware.png'), width = 25,height = 15,units='cm',dpi=800)

#### p versus feS - bware -------------
ggplot() +
  geom_point(data = melt_sel, aes(x= feS_DW_SB, y = `P_µmol/l_PW`), col = 'lightgrey', size = 5) +
  geom_point(data = melt_sel[gebied %in% i,], aes(x= feS_DW_SB, y = `P_µmol/l_PW`, col = Sloot_nr), size = 5) +
  # ylim(0,max(melt_sel[gebied %in% i,`P_µmol/l_PW`]*1.01))+
  xlim(0,max(melt_sel[gebied %in% i,feS_DW_SB]*1.01))+
  scale_colour_manual(values =c("#1B9E77","#7570B3","#E6AB02","#D95F02","#E7298A",
                                "#000000", "#E69F00","#56B4E9","#009E73" ,"#F0E442" ,
                                "#0072B2", "#D55E00", "#CC79A7","#999999",  
                                "#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                                "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888"))+
  
  theme_minimal(base_size = 15)+
  theme(
    strip.background = element_blank(),
    strip.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 14, vjust = 0.5, hjust = 1, angle = 90),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.ticks = element_line(colour = "black"),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    plot.background = element_blank(),
    legend.position = "right",
    legend.box.just = "center"
  )+
  ggtitle('') +
  labs(x= 'Fe/S ratio slib (molbasis)' , y= 'P poriewater (µmol/l)', col = 'Sloot nummer' )
ggsave(file=paste0('output/',i,'/waterbodem/',i,'_PversusFeS_Bware.png'), width = 25,height = 15,units='cm',dpi=800)

}
### loop abiotiek per gebied----------------
for(i in unique(melt$gebied)){
  # i <- unique(melt$gebied)[8]
  melt_sel <- unique(melt[gebied %in% i,])

  ## diepte doorzicht -------------------
  melt_sel_agg <- dcast(melt_sel, Sloot_nr+variable+Gebiedsnaam+gebied~., value.var = 'value', fun.aggregate = mean)
  melt_sel_agg[,Sloot_nr := as.factor(Sloot_nr)]
  melt_sel_agg <- melt_sel_agg[variable %in% c('max_slib','max_wtd','doorzicht2_mid_cm'),]
  melt_sel_agg[,slibdiepte := `.`[variable == 'max_wtd']+`.`[variable == 'max_slib'], by = c('Sloot_nr')]
 
  ggplot() +
    geom_col(data = melt_sel_agg[variable %in% c('max_slib')], aes(x= Sloot_nr, y = -1*slibdiepte, fill = 'slibdikte (m)'),alpha = 0.7) +
    geom_col(data = melt_sel_agg[variable %in% c('max_wtd')], aes(x= Sloot_nr, y = -1*`.`, fill = 'maximale waterdiepte (m)'),alpha = 0.8) +
    geom_col(data = melt_sel_agg[variable %in% c('doorzicht2_mid_cm')], aes(x= Sloot_nr, y = -1*`.`, fill = 'doorzicht (m)'),alpha = 0.8) +
     scale_fill_manual(values = c("darkblue","skyblue","brown"), na.value = "#A6761D")+
    facet_grid(.~ Gebiedsnaam, space = 'free_x', scales = 'free_x', switch = 'x')+
    theme_minimal(base_size = 15)+
    theme(
      strip.background = element_blank(),
      strip.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 15, vjust = 0.8, hjust =1, angle = 90),
      axis.text.y = element_text(size = 15),
      axis.title = element_text(size = 15),
      axis.ticks =  element_line(colour = "black"),
      axis.line = element_line(colour='black'),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      panel.background = element_blank(),
      panel.border = element_rect(colour = 'black', fill = NA),
      plot.background = element_blank(),
      legend.position = "bottom",
      legend.box.just = "center"
    )+
    guides(fill = guide_legend(title = '', title.vjust = 1))+
    guides(color = guide_legend(title = ''))+
    ggtitle(paste0("Doorzicht, waterdiepte & slibdikte")) +
    labs(x= 'Slootnummer' , y= 'meter')
  
  ggsave(file=paste0('output/',i,'/abiotiek/',i, '_watdte_zicht','.png'), width = 25,height = 15,units='cm',dpi=800)
  
  ## redox------------------------
  melt_sel[,Sloot_nr:= as.character(Sloot_nr)]
  melt_sel[is.na(monsterdiepte),monsterdiepte := '-']
  
  # define background goals (GEP_2022 is goal which is set in 2022)
  rects <- data.frame(xmin = -Inf, 
                      xmax = Inf,
                      ymin = c(-Inf,-240,-230,0,430),  
                      ymax = c(-240,-230,0,430, Inf),
                      fill = c("red", "darkorange","orange", "yellow","blue"),
                      label = c("methonogenese", "sulfaatreductie","ijzeroxidereductie","nitraatreductie","oxisch"))
  
  legend_colors <- setNames(c("red", "darkorange","orange", "yellow","blue"), rects$label)
 
  ggplot() +
    geom_col(data = melt_sel[variable %in% c("slib_redox_mgL"),], aes(x= Sloot_nr, y = value, fill = beheer), position = "dodge") +
    geom_point(data = melt_sel[variable %in% c("slib_redox_mgL"),], aes(x= Sloot_nr, y = `gemiddelde VeeST`, col = '* gemiddelde VeeST'),
               shape = 95, size = 10) +
    scale_fill_manual(values =c("#1B9E77","#7570B3","#E6AB02","#D95F02","#E7298A"))+
    scale_colour_manual(values =c('grey2'))+
    new_scale_fill()+
    geom_rect(data= rects, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), 
              inherit.aes = FALSE, alpha = 0.15) +
    scale_fill_identity('Redoxtoestand:',breaks = legend_colors, labels = c("methonogenese", "sulfaatreductie","ijzeroxidereductie","nitraatreductie","oxisch"), guide = guide_legend(override.aes = list(alpha = 0.15)))+
    facet_grid(. ~ Gebiedsnaam, space = 'free_x', scales = 'free_x', switch = 'x')+
    theme_minimal(base_size = 15)+
    theme(
      strip.background = element_blank(),
      strip.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 14, vjust = 0.5, hjust = 1, angle = 90),
      axis.text.y = element_text(size = 14),
      axis.title = element_text(size = 14),
      axis.ticks = element_line(colour = "black"),
      axis.line = element_line(colour = 'black'),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      panel.background = element_blank(),
      panel.border = element_rect(colour = 'black', fill = NA),
      plot.background = element_blank(),
      legend.position = "right",
      legend.box.just = "center"
    )+
    guides(col = guide_legend(title = ''), fill = guide_legend(title = 'Redoxtoestand'))+
    ggtitle(paste0("Redoxpotentiaal in slib in ",unique(melt_sel$Gebiedsnaam))) +
    labs(x= 'Slootnummer' , y='mV', fill = 'Type beheer' )
  ggsave(file=paste0('output/',i,'/abiotiek/',i, '_redox','.png'), width = 25,height = 15,units='cm',dpi=800)
  
  
  
  ## fingerprint-------------
  sel <- c("SiO2","Al2O3","MgO" )
  sel5 <- c("Zn","Pb","Cu","Ni","PH_CC" ) # kan ook opgelost uitspoelen door pyrietoxidatie en zink spoelt makkelijk uit zure veenbodems
  sel2 <- c("SO3", "CaO", "Fe2O3")
  sel3 <- c("N-NH4", "N-NO3")
  sel4 <- c("P2O5","P-AL","P-CC")
  sel6 <- c("Fe/P_DW_PW","Fe/P_PW_PW",
            "Fe/P_CC_calciumchloride","Fe/P__xrf")
  sel7 <- c("Fe/S__xrf","Fe/S_CC_calciumchloride","Fe/S__PW","Fe/S_DW_PW")
  
  melt_sel_fp <- melt_sel[(par_eenheid%in%sel6),]
  if(nrow(melt_sel_fp)>0){
  ggplot(melt_sel_fp, aes(x = compartiment, y = value, fill = paste0(compartiment, ' ',monsterdiepte))) +
    facet_wrap(methode~parameter+eenheid, scales = "free",
               ncol = 3) +
    stat_boxplot(geom = 'errorbar') +
    geom_boxplot(outliers = FALSE) +
    theme_minimal() +
    theme(text = element_text(size = 14),
          axis.ticks =  element_line(colour = "black"),
          axis.line = element_line(colour='black'),
          axis.text.x = element_text(size = 14, angle = 45,hjust=1)
          
    ) +
    scale_fill_brewer(palette = "Set2") +
    labs(x = "compartiment", y = "", fill = 'compartiment en monsterdiepte')+
    ggtitle(paste0(unique(melt_sel$parameter)," ",unique(melt_sel$methode))) 
  ggsave(file=paste0('output/AlleGebieden/fingerprints/',var_folder ,'.png'), width = 35,height = 15,units='cm',dpi=800)
  
  melt_sel_fp <- melt_sel[(par_eenheid%in%sel7),]
  ggplot(melt_sel_fp, aes(x = compartiment, y = value, fill = paste0(compartiment, ' ',monsterdiepte))) +
    facet_wrap(methode~parameter+eenheid, scales = "free",
               ncol = 3) +
    stat_boxplot(geom = 'errorbar') +
    geom_boxplot(outliers = FALSE) +
    theme_minimal() +
    theme(text = element_text(size = 14),
          axis.ticks =  element_line(colour = "black"),
          axis.line = element_line(colour='black'),
          axis.text.x = element_text(size = 14, angle = 45,hjust=1)
    ) +
    scale_fill_brewer(palette = "Set2") +
    labs(x = "compartiment", y = "", fill = 'compartiment en monsterdiepte')+
    ggtitle(paste0(unique(melt_sel$parameter)," ",unique(melt_sel$methode))) 
  ggsave(file=paste0('output/AlleGebieden/fingerprints/',var_folder ,'.png'), width = 35,height = 15,units='cm',dpi=800)
  
  melt_sel_fp <- melt_sel[(parameter%in%sel & methode == 'xrf')|parameter%in%'organisch stof',]
  ggplot(melt_sel_fp, aes(x = compartiment, y = value, fill = paste0(compartiment, ' ',monsterdiepte))) +
    facet_wrap(~parameter+eenheid, scales = "free",
               ncol = 3) +
    stat_boxplot(geom = 'errorbar') +
    geom_boxplot(outliers = FALSE) +
    theme_minimal() +
    theme(text = element_text(size = 14),
          axis.ticks =  element_line(colour = "black"),
          axis.line = element_line(colour='black')
    ) +
    scale_fill_brewer(palette = "Set2") +
    labs(x = "compartiment", y = "", fill = 'compartiment en monsterdiepte')+
    ggtitle(paste0(unique(melt_sel$parameter)," ",unique(melt_sel$methode))) 
  ggsave(file=paste0('output/AlleGebieden/fingerprints/',var_folder ,'.png'), width = 35,height = 15,units='cm',dpi=800)
  
  melt_sel_fp <- melt_sel[(parameter%in%sel5 & methode == 'xrf')|par_eenheid%in%"pH_CC_calciumchloride",]
  ggplot(melt_sel_fp, aes(x = compartiment, y = value, fill = paste0(compartiment, ' ',monsterdiepte))) +
    facet_wrap(~parameter+eenheid, scales = "free",
               ncol = 3) +
    stat_boxplot(geom = 'errorbar') +
    geom_boxplot(outliers = FALSE) +
    theme_minimal() +
    theme(text = element_text(size = 14),
          axis.ticks =  element_line(colour = "black"),
          axis.line = element_line(colour='black')
    ) +
    scale_fill_brewer(palette = "Set2") +
    labs(x = "compartiment", y = "", fill = 'compartiment en monsterdiepte')+
    ggtitle(paste0(unique(melt_sel$parameter)," ",unique(melt_sel$methode))) 
  ggsave(file=paste0('output/AlleGebieden/fingerprints/',var_folder ,'.png'), width = 35,height = 15,units='cm',dpi=800)
  
  melt_sel_fp <- melt_sel[(parameter%in%sel2 & methode == 'xrf'),]
  ggplot(melt_sel_fp, aes(x = compartiment, y = value, fill = paste0(compartiment, ' ',monsterdiepte))) +
    facet_wrap(~parameter+eenheid, scales = "free",
               ncol = 3) +
    stat_boxplot(geom = 'errorbar') +
    geom_boxplot(outliers = FALSE) +
    theme_minimal() +
    theme(text = element_text(size = 14),
          axis.ticks =  element_line(colour = "black"),
          axis.line = element_line(colour='black')
    ) +
    scale_fill_brewer(palette = "Set2") +
    labs(x = "compartiment", y = "", fill = 'compartiment en monsterdiepte')+
    ggtitle(paste0(unique(melt_sel$parameter)," ",unique(melt_sel$methode))) 
  ggsave(file=paste0('output/AlleGebieden/fingerprints/',var_folder ,'.png'), width = 35,height = 15,units='cm',dpi=800)
  
  melt_sel_fp <- melt_sel[parameter%in%sel4 & methode %in% c('xrf','pal','XRF','calciumchloride','CC'),]
  melt_sel_fp <- melt_sel_fp[is.na(eenheid), eenheid := ""]
  melt_sel_fp <- melt_sel_fp[!par_eenheid %in% 'P-AL_mg/kg',]
  ggplot(melt_sel_fp, aes(x = paste0(compartiment), y = value, fill = paste0(compartiment, ' ',monsterdiepte))) +
    facet_wrap(~parameter+methode, scales = "free",
               ncol = 3) +
    stat_boxplot(geom = 'errorbar') +
    geom_boxplot(outliers = FALSE) +
    theme_minimal() +
    theme(text = element_text(size = 14),
          axis.ticks =  element_line(colour = "black"),
          axis.line = element_line(colour='black'),
          axis.text.x = element_text(size = 14, angle = 45,hjust=1)
    ) +
    guides(fill = guide_legend(title = 'compartiment en monsterdiepte', title.vjust = 1))+
    scale_fill_brewer(palette = "Set2") +
    labs(x = "", y = "", fill = 'compartiment en monsterdiepte')
  ggsave(file=paste0('output/AlleGebieden/fingerprints/',var_folder ,'.png'), width = 35,height = 15,units='cm',dpi=800)
  
  melt_sel_fp <- melt_sel[parameter%in% "Fe/P" & methode %in% c('xrf','pal','XRF','calciumchloride','CC','PW'),]
  melt_sel_fp <- melt_sel_fp[is.na(eenheid), eenheid := ""]
  ggplot(melt_sel_fp, aes(x = paste0(compartiment), y = value, fill = paste0(compartiment, ' ',monsterdiepte))) +
    facet_wrap(~parameter+methode, scales = "fixed",
               ncol = 3) +
    stat_boxplot(geom = 'errorbar') +
    geom_boxplot(outliers = FALSE) +
    theme_minimal() +
    theme(text = element_text(size = 14),
          axis.ticks =  element_line(colour = "black"),
          axis.line = element_line(colour='black'),
          axis.text.x = element_text(size = 14, angle = 45,hjust=1)
    ) +
    guides(fill = guide_legend(title = 'compartiment en monsterdiepte', title.vjust = 1))+
    scale_fill_brewer(palette = "Set2") +
    labs(x = "", y = "", fill = 'compartiment en monsterdiepte')
  ggsave(file=paste0('output/AlleGebieden/fingerprints/',var_folder ,'.png'), width = 35,height = 15,units='cm',dpi=800)
  
  melt_sel_fp <- melt_sel[parameter%in% "Fe/S" & methode %in% c('xrf','pal','XRF','calciumchloride','CC','icp Bware'),]
  melt_sel_fp <- melt_sel_fp[is.na(eenheid), eenheid := ""]
  ggplot(melt_sel_fp, aes(x = paste0(compartiment), y = value, fill = paste0(compartiment, ' ',monsterdiepte))) +
    facet_wrap(~parameter+methode, scales = "fixed",
               ncol = 3) +
    stat_boxplot(geom = 'errorbar') +
    geom_boxplot(outliers = FALSE) +
    theme_minimal() +
    theme(text = element_text(size = 14),
          axis.ticks =  element_line(colour = "black"),
          axis.line = element_line(colour='black'),
          axis.text.x = element_text(size = 14, angle = 45,hjust=1)
    ) +
    guides(fill = guide_legend(title = 'compartiment en monsterdiepte', title.vjust = 1))+
    scale_fill_brewer(palette = "Set2") +
    labs(x = "", y = "", fill = 'compartiment en monsterdiepte')
  ggsave(file=paste0('output/AlleGebieden/fingerprints/',var_folder ,'.png'), width = 35,height = 15,units='cm',dpi=800)
  }
  
  ## abiotiek-----------------
  # per sloot nr en behandeling (compartiment en mons diepte facet)
  for(j in unique(melt_sel$par_eenheid)){
    # j <- unique(melt_sel$par_eenheid)[164]
    melt_sel_v <- melt_sel[par_eenheid %in% j,]
    
      melt_sel_v[,Sloot_nr:= as.character(Sloot_nr)]
      melt_sel_v[is.na(monsterdiepte),monsterdiepte := '-']
      # melt_sel_v[,Sloot_nr:= factor(Sloot_nr, levels = c('1','2','3','4','5','6','7','8','9','10','11','12','13','14','15'))]
      var_folder <- gsub('/','_',j)
      var_folder <- gsub('%','perc',var_folder)
      
      ggplot() +
        geom_col(data = melt_sel_v, aes(x= Behandeling, y = value, fill = beheer), position = "dodge") +
        geom_point(data = melt_sel_v, aes(x= Behandeling, y = `gemiddelde VeeST`, col = '* gemiddelde VeeST'),
                   shape = 95, size = 10) +
        scale_fill_manual(values =c("#1B9E77","#7570B3","#E6AB02","#D95F02","#E7298A"))+
        scale_colour_manual(values =c('grey2'))+
        facet_grid(compartiment+monsterdiepte ~ Sloot_nr, space = 'free_x', scales = 'free_x', switch = 'x')+
        theme_minimal(base_size = 15)+
        theme(
          strip.background = element_blank(),
          strip.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 14, vjust = 0.5, hjust = 1, angle = 90),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 14),
          axis.ticks =  element_line(colour = "black"),
          axis.line = element_line(colour='black'),
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          panel.background = element_blank(),
          panel.border = element_rect(colour='black', fill = NA),
          plot.background = element_blank(),
          legend.position = "right",
          legend.box.just = "center"
        )+
        guides(col = guide_legend(title = ''), fill = guide_legend(title = 'Type beheer'))+
        ggtitle(paste0(unique(melt_sel_v$parameter), " ",unique(melt_sel_v$Gebiedsnaam))) +
        labs(x= 'Slootnummer en behandeling' , y=paste0(unique(melt_sel_v$parameter),' (',unique(melt_sel_v$eenheid),') '), fill = 'Type beheer' )
      ggsave(file=paste0('output/',i,'/abiotiek/',i,'_monsdte_comp_',var_folder,unique(melt_sel_v$methode),'.png'), width = 35,height = 15,units='cm',dpi=800)
      
       
  }
  # per sloot nr en behandeling unieke par, methode en compartiment
  for(j in unique(melt_sel$variable)){
    # j <- unique(melt_sel$variable)[306]
    melt_sel_v <- melt_sel[variable %in% j,]
    melt_sel_v[,Sloot_nr:= as.character(Sloot_nr)]
    # melt_sel_v[,Sloot_nr:= factor(Sloot_nr, levels = c('1','2','3','4','5','6','7','8','9','10','11','12','13','14','15'))]  
    melt_sel_v[,value_ref :=  value -`gemiddelde VeeST`]
    gemVeest <- round(unique(melt_sel_v$`gemiddelde VeeST`), digits = 2)
    var_folder <- gsub('/','_',j)
    var_folder <- gsub('%','perc',var_folder)
    
    ggplot() +
      geom_col(data = melt_sel_v, aes(x= Behandeling, y = value_ref, fill = beheer)) +
      scale_fill_manual(values =c("#D95F02", "#A6761D", "#7570B3", "#1B9E77","#E6AB02","#E7298A","#66A61E"))+
      scale_y_continuous(labels = function(x)x+gemVeest,
                         sec.axis = sec_axis(function(x)x, name = 'tov gemiddelde VeeST'))+
      facet_grid(.~ Sloot_nr, space = 'free_x', scales = 'free_x', switch = 'x')+
      theme_minimal(base_size = 15)+
      theme(
        strip.background = element_blank(),
        strip.text.y = element_text(size = 12), 
        axis.text = element_text(size = 14, angle = 45),
        axis.title = element_text(size= 15),
        axis.ticks =  element_line(colour = "black"),
        plot.title = element_text(size =15, face="bold"),
        panel.background = element_blank(),
        panel.border = element_rect(colour='black', fill = NA),
        plot.background = element_blank(),
        legend.position = "right",
        legend.box.just = "center"
      )+
      guides(col = guide_legend(title = ''), fill = guide_legend(title = 'Type beheer'))+
      labs(title = (paste0(capitalize(unique(melt_sel_v$varnames))," ten opzichte van het gemiddelde VeeST (",round(gemVeest, digits = 2), " ",unique(melt_sel_v$eenheid),")" )),
           subtitle = paste0('in ', unique(melt_sel_v$Gebiedsnaam)),
           x= 'Slootnummer en behandeling' ,
           y= fifelse(unique(melt_sel_v$eenheid) == "",
                        unique(melt_sel_v$parameter),
                             paste0(unique(melt_sel_v$parameter),' (',unique(melt_sel_v$eenheid),') ')),
           fill = 'Type beheer' )
    ggsave(file=paste0('output/',i,'/abiotiek/',i, '_hist_',var_folder,unique(melt_sel_v$methode),'.png'), width = 35,height = 15,units='cm',dpi=800)
  }
  } 

## 5.5 plot penetrometer-------------------------
# calc draagkracht per diepte
# dist id 1 (perceel) en 2 (insteek) weg
penmerge[,sectie_f := factor(sectie, levels=c('oever','insteek','perceel'))]
ggplot()+
  geom_boxplot(data = penmerge[!is.na(dieptebin) ,], aes(x= (gebied_locs), y=indringingsweerstand))+
  facet_grid(dieptebin~sectie_f)+
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
  ggtitle("Indringingsweerstand 0 - 80 cm diepte") +
  labs(x= "gebied",y="indringingsweerstand")
ggsave(file=paste0('output/AlleGebieden','/penetrometer/','box_penetrometer_alledieptenapart.png'),width = 20,height = 10,units='in',dpi=800)

### loop per gebied -------------------------------

# beheer
penmerge[,beheer := 'regulier']
penmerge[grepl('M', Behandeling),beheer := 'minimaal']
penmerge[grepl('M-AF', Behandeling),beheer := 'minimaal + afrastering']
penmerge[grepl('R-AF', Behandeling),beheer := 'regulier + afrastering']
penmerge[grepl('AF', Behandeling),beheer := 'afrastering']
penmerge[grepl('NVO', Behandeling), beheer := 'NVO']
penmerge[grepl('NVO-AF', Behandeling), beheer := 'NVO + afrastering']
penmerge <- penmerge[!is.na(gebied_locs),]

# dist id 1 (perceel) en 2 (insteek) en 3 tm x oever, waarbij hoogste getal waterlijn
for(gb in unique(penmerge$gebied_locs)){
  # gb <- unique(penmerge$gebied_locs)[9]
  penmerge_gb <- penmerge[!is.na(dieptebin) & gebied_locs %in% gb,]
  penmerge_gb[,dieptebin := cut(Diept, breaks = seq(from = 0, to = 80, by = 5), include.lowest = TRUE)]
  penmerge_gb[,sectie_f := factor(sectie, levels=c('oever','insteek','perceel'))]
    geom_rect(data = rects, aes(xmin = xmin, xmax = xmax, 
                               ymin = ymin, ymax = ymax, fill = fill),inherit.aes = FALSE, alpha = 0.15)+
    scale_fill_identity('Indringingsweerstand',breaks = "red", labels = c("te laag voor beweiding"), guide = guide_legend(override.aes = list(alpha = 0.2)))+
    scale_y_reverse(breaks = seq(0,80,10), limits = c(80,0)) + 
    xlim(0,1)+
    geom_point(col = 'lightgrey')+
    stat_smooth(se = FALSE, orientation = 'y', col = 'black')+
    facet_grid(.~Sloot_nr, scales = 'fixed')+
    theme_minimal(base_size = 14)+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 14),
      strip.text.y = element_text(size = 14), 
      axis.text.x = element_text(size = 14, angle = 90),
      axis.text.y = element_text(size= 14),
      axis.ticks =  element_line(colour = "black"),
      plot.title = element_text(size =15, face="bold"),
      panel.border = element_rect(color = 'black', fill = NA),
      plot.background = element_blank(),
      )+
    labs(title= paste0("Indringsweerstand per sloot"), 
         subtitle = "gemeten tussen waterlijn en insteek" , x= "indringingsweerstand (mPa)",y="diepteinterval (cm)", col ='Zone')
  ggsave(file=paste0('output/',unique(penmerge_gb$gebied_locs),'/penetrometer/', unique(penmerge_gb$gebied_locs),'persloot_ydiepte.png'),width = 15,height = 7.0,units='in',dpi=800)
   
  #### per behandeling alleen oever--------------
  penmerge_gb <- penmerge_gb[,lapply(.SD,mean,na.rm=TRUE),.SDcols=c('Diept','indringingsweerstand'),by=c('Gebiedsnaam','gebied_locs','sloot_locs','SlootID','Sloot_nr','sectie','Behandeling','beheer','sectie_f','dieptebin')]
  ggplot(data = penmerge_gb, aes(x= indringingsweerstand, y = Diept, col = sectie_f), size = 2)+
    geom_rect(data = rects, aes(xmin = xmin, xmax = xmax, 
                                ymin = ymin, ymax = ymax, fill = fill),inherit.aes = FALSE, alpha = 0.15)+
    scale_fill_identity('Indringingsweerstand',breaks = "red", labels = c("te laag voor beweiding"), guide = guide_legend(override.aes = list(alpha = 0.1)))+
    scale_y_reverse(breaks = seq(0,80,10), limits = c(80,0)) + 
    geom_point()+
    stat_smooth(se = FALSE, orientation = 'y')+
    scale_color_manual(values =c("#1B9E77","#D95F02","#7570B3", "#E6AB02","#E7298A"))+
    facet_grid(.~Sloot_nr+Behandeling, scales = 'fixed')+
    theme_minimal(base_size = 14)+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 14),
      strip.text.y = element_text(size = 14), 
      axis.text.x = element_text(size = 14, angle = 90),
      axis.text.y = element_text(size= 14),
      axis.ticks =  element_line(colour = "black"),
      plot.title = element_text(size =15, face="bold"),
      panel.border = element_rect(color = 'black', fill = NA),
      plot.background = element_blank(),
    )+
    labs(title= paste0("Indringsweerstand per sloot en behandeling in ",unique(penmerge_gb$Gebiedsnaam)), 
         subtitle = "gemeten tussen waterlijn en insteek" , x= "indringingsweerstand (mPa)",y="diepteinterval (cm)", col ='Zone')
  ggsave(file=paste0('output/',unique(penmerge_gb$gebied_locs),'/penetrometer/', unique(penmerge_gb$gebied_locs),'sloot_behandeling_diepte.png'),width = 15,height = 7.0,units='in',dpi=800)
  
  #### plot boxplot---------------
  rects <- data.frame(xmin = -Inf, 
                      xmax = Inf,
                      ymin = 0,  
                      ymax = 0.5,
                      fill = c("red"),
                      label = c("te laag voor beweiding"))
  penmerge_gb[,dieptebin := cut(Diept, breaks = c(0,25,50,80), include.lowest = TRUE)]
  
  ggplot(data = penmerge_gb, aes(y=indringingsweerstand, x = sectie_f, col = paste(beheer, sep = '_', recycle0 = TRUE)), size = 2)+
    geom_rect(data = rects, aes(xmin = xmin, xmax = xmax, 
                                ymin = ymin, ymax = ymax, fill = fill),inherit.aes = FALSE, alpha = 0.15)+
    scale_fill_identity('Indringingsweerstand',breaks = "red", labels = c("te laag voor beweiding"), guide = guide_legend(override.aes = list(alpha = 0.1)))+
    geom_boxplot()+
    facet_grid(dieptebin~paste0(sloot_locs,"_",Behandeling))+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size=14), 
      strip.text.y = element_text(size = 14), 
      axis.text.x = element_text(size = 14, angle = 90, vjust = 0.5, hjust = 1),
      axis.text.y = element_text(size= 14),
      axis.ticks =  element_line(colour = "black"),
      plot.title = element_text(size =15, face="bold", hjust = 0.5),
      panel.background = element_blank(),
      plot.background = element_blank(),
    )+
    ggtitle(paste0("Indringingsweerstand 0 - 80 cm diepte in ",unique(penmerge_gb$Gebiedsnaam))) +
    labs(x= "locatie",y="indringingsweerstand", col ='Behandeling')
  ggsave(file=paste0('output/',unique(penmerge_gb$gebied_locs),'/penetrometer/', unique(penmerge_gb$Gebiedsnaam),'_boxplot_zones.png'),width = 15,height = 7.0,units='in',dpi=800)
  
  print(gb)  
  
  
  ### loop per locatie oever, perceel en insteek ------------------------------------------------------
  for(loc in unique(penmerge_gb$Sloot_nr)){
    # loc <- unique(penmerge_gb$Sloot_nr)[1]
    penmerge_gb_sloot <- penmerge_gb[!is.na(dieptebin) & Sloot_nr %in% loc,]
    rects <- data.frame(xmin = 0, 
                        xmax = 0.5,
                        ymin = -Inf,  
                        ymax = Inf,
                        fill = c("red"),
                        label = c("te laag voor beweiding"))
    
    ggplot(data = penmerge_gb_sloot, aes(x=indringingsweerstand, y = Diept, col = sectie_f), size = 2)+
      geom_rect(data = rects, aes(xmin = xmin, xmax = xmax, 
                                  ymin = ymin, ymax = ymax, fill = fill),inherit.aes = FALSE, alpha = 0.15)+
      scale_fill_identity('Indringingsweerstand',breaks = "red", labels = c("te laag voor beweiding"), guide = guide_legend(override.aes = list(alpha = 0.1)))+
      scale_y_reverse(breaks = seq(0,80,10), limits = c(80,0)) + 
      geom_point()+
      stat_smooth(se = FALSE, orientation = 'y')+
      scale_color_manual(values =c("#1B9E77","#D95F02","#7570B3","#E6AB02","#E7298A"))+
      facet_grid(.~Behandeling+beheer, scales = 'free')+
      # ylim(0,2)+
      theme_minimal()+
      theme(
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 12, angle = 90),
        axis.text.y = element_text(size= 12),
        axis.ticks =  element_line(colour = "black"),
        plot.title = element_text(size =12, face="bold", hjust = 0.5),
        panel.border = element_rect(color = 'black', fill = NA),
        panel.background = element_blank(),
        plot.background = element_blank(),
      )+
      ggtitle(paste0("Indringsweerstand op locatie ",unique(penmerge_gb_sloot$Sloot_nr))) +
      labs(x= "indringingsweerstand (mPa)",y="diepteinterval (cm)", col ='Afstand waterlijn:')
    ggsave(file=paste0('output/',unique(penmerge_gb_sloot$gebied_locs),'/penetrometer/',unique(penmerge_gb_sloot$gebied_locs),'_', unique(penmerge_gb_sloot$Sloot_nr),'.png'),width = 10,height = 7.0,units='in',dpi=800)
    
    print(loc)  
  }
}

## 5.6 overig----------------------
### waterbodem ----------------

p<- ggplot(data = abio_proj[], aes(x= `P_µmol/l_PW`, y=`P-AL mg/kg_SB`))+
  geom_jitter(aes(col= gebied))+
  geom_smooth(method="lm") +
  # stat_regline_equation(label.x=200, label.y=220)+
  stat_cor(aes(label=..rr.label..), label.x=200, label.y=200)+
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
  ggtitle("waterbodem") +
  labs(x= "P poriewater (µmol/l)",y="P-AL slib (mg/kg)")
ggplotly(p, tooltip = c('SlootID','gebied'))

p<- ggplot(data = abio_proj, aes(x= `P_µmol/l_PW`, y=`P-PO4_CC_mg/kg_SB`))+
  geom_jitter(aes(col= gebied))+
  geom_smooth(method="lm") +
  # stat_regline_equation(label.x=200, label.y=220)+
  stat_cor(aes(label=..rr.label..), label.x=200, label.y=40)+
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
  ggtitle("waterbodem") +
  labs(x= "P poriewater (µmol/l)",y="P-CC slib (mg/kg)")
ggplotly(p, tooltip = c('SlootID','gebied'))

p<- ggplot(data = abio_proj, aes(x= `P2O5_xrf_g/kg_SB`, y=`P_mmol/kg DW_SB`))+
  geom_jitter(aes(col= gebied))+
  geom_smooth(method="lm") +
  stat_regline_equation(label.x=2, label.y=110)+
  stat_cor(aes(label=..rr.label..), label.x=2, label.y=100)+
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
  ggtitle("waterbodem") +
  labs(x= "P tot XRF",y="P tot ICP")
ggplotly(p, tooltip = c('SlootID','gebied'))

p<- ggplot(data = abio_proj, aes(x= `Fe2O3_xrf_g/kg_SB`, y=`Fe_mmol/kg DW_SB`))+
  geom_jitter(aes(col= gebied))+
  geom_smooth(method="lm") +
  stat_regline_equation(label.x=25, label.y=1100)+
  stat_cor(aes(label=..rr.label..), label.x=25, label.y=1000)+
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
  ggtitle("waterbodem") +
  labs(x= "Fe tot XRF",y="Fe tot ICP")
ggplotly(p, tooltip = c('SlootID','gebied'))

p<- ggplot(data = abio_proj, aes(x= `Zn_xrf_mg/kg_SB`, y=`Zn_mmol/kg DW_SB`))+
  geom_jitter(aes(col= gebied))+
  geom_smooth(method="lm") +
  stat_regline_equation(label.x=500, label.y=12)+
  stat_cor(aes(label=..rr.label..), label.x=500, label.y=10)+
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
  ggtitle("waterbodem") +
  labs(x= "zn tot XRF",y="zn tot ICP")
ggplotly(p, tooltip = c('SlootID','gebied'))

abio_proj[,feP_factor:= cut(abio_proj$feP_PW, breaks = c(0,0.5,1,2,10), include_lowest = T)]
abio_proj[,feS_factor:= cut(abio_proj$feS_SB_DW, breaks = c(0,0.5,1,2,10), include_lowest = T)]
abio_proj[,feS_factor_xrf:= cut(abio_proj$feS_SB_XRF, breaks = c(0,0.5,1,2,10), include_lowest = T)]
abio_proj[,feP_factor_xrf:= cut(abio_proj$feP_SB_XRF, breaks = c(0,0.5,1,2,10), include_lowest = T)]
abio_proj[,feS_factor_cc:= cut(abio_proj$feS_SB_CC, breaks = c(0,0.5,1,2,10), include_lowest = T)]
abio_proj[,feP_factor_cc:= cut(abio_proj$feP_SB_CC, breaks = c(0,0.5,1,2,10), include_lowest = T)]
# p-org `P_CC_org_mg/kg` `P-PO4_CC_mg/kg`

p<- ggplot(data = abio_proj, aes(x= `P_µmol/l_PW`, y=`P-AL mg/kg_SB`, text = SlootID))+
  geom_jitter(aes(col = feP_factor))+
  xlim(0,410)+
  geom_smooth(method="lm") +
  stat_regline_equation(label.x=200, label.y=220)+
  stat_cor(aes(label=..rr.label..), label.x=200, label.y=200)+
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
  ggtitle("waterbodem") +
  labs(x= "P poriewater (µmol/l)",y="P-AL slib (mg/kg)")
ggplotly(p, tooltip = c('text','gebied'))

## waterbodem 
abio_proj[,gebied :=as.factor(gebied)]
pairs(abio_proj[feS_SB_DW<6,c('P_µmol/l_PW','feP_PW','feS_SB_DW','feS_PW')], col = abio_proj$gebied)
pairs(abio_proj[feS_SB_DW<6,c('P_µmol/l_PW','feP_PW','feS_SB_DW','Fe_µmol/l_PW')], col = abio_proj$gebied)

## fingerprint


### draagkracht --------------
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

### relatie onderholling draagkracht ---------------------------------------------------
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
  geom_jitter(data = abio_proj, aes(x=  holleoever, y=`oever_(30,40]`, col = waterschap),  size = 3)+
  geom_jitter(data = abio_proj, aes(x=  holleoever, y=`oever_(20,30]`, col = waterschap), size = 3)+
  geom_jitter(data = abio_proj, aes(x=  holleoever, y=`oever_(10,20]`, col = waterschap), size = 3)+
  # scale_color_manual(name = "waterschap")) +
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    axis.title = element_text(size= 14), 
    axis.text = element_text(size= 14),
    legend.title = element_text(size= 15),
    legend.text = element_text(size= 14),
    axis.ticks =  element_line(colour = "black"),
    plot.title = element_text(size =15, face="bold", hjust = 0.5),
    panel.background = element_blank(),
    plot.background = element_blank(),
  )+
  ggtitle("Draagkracht oevers") +
  labs(x= "onderholling",y="indringingsweerstand (Mpa)")

abio_proj$oeverzone_2a_breedte_cm
ggplot()+
  geom_jitter(data = abio_proj, aes(x= holleoever, y=(oeverzone_2a_breedte_cm * oeverzone_2a_emers_perc)/100, col = waterschap),  size = 3)+
  # scale_color_manual(name = "waterschap")) +
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    axis.title = element_text(size= 14), 
    axis.text = element_text(size= 14),
    legend.title = element_text(size= 15),
    legend.text = element_text(size= 14),
    axis.ticks =  element_line(colour = "black"),
    plot.title = element_text(size =15, face="bold", hjust = 0.5),
    panel.background = element_blank(),
    plot.background = element_blank(),
  )+
  ggtitle("Aanwezigheid emerse vegetatie en onderholling") +
  labs(x= "onderholling",y="oppervlak emerse vegetatie (m2/ meter oever)")

ggplot()+
  geom_jitter(data = abio_proj, aes(x=  holleoever, y=`oever_(30,40]`, col = waterschap),  size = 3)+
  geom_jitter(data = abio_proj, aes(x=  holleoever, y=`oever_(20,30]`, col = waterschap), size = 3)+
  geom_jitter(data = abio_proj, aes(x=  holleoever, y=`oever_(10,20]`, col = waterschap), size = 3)+
  # scale_color_manual(name = "waterschap")) +
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    axis.title = element_text(size= 14), 
    axis.text = element_text(size= 14),
    legend.title = element_text(size= 15),
    legend.text = element_text(size= 14),
    axis.ticks =  element_line(colour = "black"),
    plot.title = element_text(size =15, face="bold", hjust = 0.5),
    panel.background = element_blank(),
    plot.background = element_blank(),
  )+
  ggtitle("Draagkracht oevers") +
  labs(x= "onderholling (cm)",y="indringingsweerstand (Mpa)")
 
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
### relatie onderholling en redox slib ---------------------------------------------------

ggplot()+
  geom_jitter(data = abio_proj, aes(x= holleoever, y=slib_redox_pH7, col = waterschap),  size = 3)+
  # scale_color_manual(name = "waterschap")) +
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    axis.title = element_text(size= 14), 
    axis.text = element_text(size= 14),
    legend.title = element_text(size= 15),
    legend.text = element_text(size= 14),
    axis.ticks =  element_line(colour = "black"),
    plot.title = element_text(size =15, face="bold", hjust = 0.5),
    panel.background = element_blank(),
    plot.background = element_blank(),
  )+
  ggtitle("Redoxpotentiaal slib en onderholling") +
  labs(x= "onderholling",y="redox potentiaal slib (mV)")


### relatie maaifrequentie en draagkracht------------------------------------------------
checkdata <- unique(abio_proj[is.na(Maaifrequentie_oever_per_jaar),c('SlootID','Gebiedsnaam','jaar','MeenemenDataAnalyse_totaal')])
ggplot()+  
  geom_jitter(data = abio_proj, aes(x=  Maaifrequentie_oever_per_jaar, y=`oever_(30,40]`, col = waterschap),  size = 3)+
  geom_jitter(data = abio_proj, aes(x=  Maaifrequentie_oever_per_jaar, y=`oever_(20,30]`, col = waterschap), size = 3)+
  geom_jitter(data = abio_proj, aes(x=  Maaifrequentie_oever_per_jaar, y=`oever_(10,20]`, col = waterschap), size = 3)+
  # scale_color_manual(name = "waterschap")) +
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    axis.title = element_text(size= 14), 
    axis.text = element_text(size= 14),
    legend.title = element_text(size= 15),
    legend.text = element_text(size= 14),
    axis.ticks =  element_line(colour = "black"),
    plot.title = element_text(size =15, face="bold", hjust = 0.5),
    panel.background = element_blank(),
    plot.background = element_blank(),
  )+
  ggtitle("Draagkracht oevers") +
  labs(x= "maaifrequentie (a)",y="indringingsweerstand (Mpa)")

### maaifrequentie ----------------------------------------------------------------------
ggplot() +
  # Achtergrond bars voor drooglegging
  geom_boxplot(data = abio_proj, 
           aes(x = Gebiedsnaam, y = Maaifrequentie_oever_per_jaar), 
           fill = "lightblue", alpha = 0.4, width = 0.8) +
  labs(
    title = "Maaifreq oever per gebied",
    x = "Gebiedsnaam",
    fill = "Veentype"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 10)
  )
### vertrappingschade -------------------------------------------------------
abio_proj[, oeverzone_2b_vertrapping_schade := as.double(oeverzone_2b_vertrapping_schade)]
median_vertrapping <- median(abio_proj$oeverzone_2b_vertrapping_schade, na.rm = TRUE)
vertrapping_summary <- abio_proj[!is.na(sloot_cluster) & !is.na(oeverzone_2b_vertrapping_schade) & jaar == 2025, .(
  mean_vertrapping = median(oeverzone_2b_vertrapping_schade, na.rm = TRUE),
  q25_vertrapping  = quantile(oeverzone_2b_vertrapping_schade, 0.25, na.rm = TRUE),
  q75_vertrapping  = quantile(oeverzone_2b_vertrapping_schade, 0.75, na.rm = TRUE),
  min_vertrapping  = min(oeverzone_2b_vertrapping_schade, na.rm = TRUE),
  max_vertrapping  = max(oeverzone_2b_vertrapping_schade, na.rm = TRUE)
), by = .(sloot_cluster, Behandeling)]
vertrapping_summary[, `:=`(
  iqr_vertrapping   = q75_vertrapping - q25_vertrapping,
  whisker_lower     = pmax(min_vertrapping, q25_vertrapping - 1.5 * (q75_vertrapping - q25_vertrapping)),
  whisker_upper     = pmin(max_vertrapping, q75_vertrapping + 1.5 * (q75_vertrapping - q25_vertrapping))
)]

ggplot() +
  geom_col(data = vertrapping_summary,
           aes(x = Behandeling, y = mean_vertrapping),
           fill = "#009E73", alpha = 0.7, width = 0.8) +
  geom_errorbar(data = vertrapping_summary,
                aes(x = Behandeling,
                    ymin = whisker_lower,
                    ymax = whisker_upper),
                width = 0.2, color = "black", size = 0.8) +
  geom_hline(yintercept = median_vertrapping,
             color = "black", linetype = "dashed", size = 1) +
  facet_wrap(.~sloot_cluster, nrow = 1, scales = "free_x") +
  theme_minimal(base_size = 15) +
  theme(
    strip.background   = element_blank(),
    strip.text.y       = element_text(size = 12),
    axis.text.y        = element_text(size = 14),
    axis.text.x        = element_text(size = 14, angle = 45, hjust = 1),
    axis.title         = element_text(size = 14),
    axis.ticks         = element_line(colour = "black"),
    axis.line          = element_line(colour = "black"),
    plot.title         = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle      = element_text(size = 14, hjust = 0.5),
    panel.background   = element_blank(),
    panel.border       = element_rect(colour = "black", fill = NA),
    plot.background    = element_blank(),
    legend.position    = "right",
    legend.box.just    = "center"
  ) +
  labs(
    title    = "Vertrappingsschade oeverzone per gebied",
    subtitle = paste0("Zwarte stippellijn = mediaan vertrapping (",
                     round(median_vertrapping, 1), ")\nErrorbars tonen whisker-range (Q1-1.5×IQR tot Q3+1.5×IQR)"),
    x        = "Slootnummer en behandeling",
    y        = "Vertrappingsschade oeverzone"
  )


### tabel waterdiepte, slib en drooglegging ---------------------------------------------------
tabel_watdiep_slib <- abio_proj[WP %in% c("WP1", "WP2"), .(
  waterdiepte  = round(median(max_wtd,  na.rm = TRUE), 2),
  slibdikte    = round(median(max_slib, na.rm = TRUE), 2),
  drooglegging = round(median(drglg,    na.rm = TRUE), 2),
  n            = .N
), by = .(Sloot_nr, Behandeling)] |>
  setorder(Sloot_nr)
library(gt)
tabel_watdiep_slib |>
  gt() |>
  tab_header(
    title = "Waterdiepte, slibdikte en drooglegging per sloot",
    subtitle = "Mediaan per slootnummer en behandeling"
  ) |>
  cols_label(
    Sloot_nr     = "Slootnr",
    Behandeling  = "Behandeling",
    waterdiepte  = "Waterdiepte (m)",
    slibdikte    = "Slibdikte (m)",
    drooglegging = "Drooglegging (m)",
    n            = "N"
  )
# 6. Export the data ---------------------------------------------------------
## locaties---------------------------------------------------------
locaties <- st_as_sf(locaties) %>% st_transform(crs = 28992)
st_write(locaties, paste0(workspace,  "output/GIS/geo_locaties.gpkg"), append = FALSE)

## profielen, hoeken, onderholling-----------------------------------------
write.table(profiel, file = paste(workspace,"/output/Database/profiel",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
profiel[, geom_locs := NULL]
st_geometry(profiel) <- "geometry"
profiel <- st_as_sf(profiel)
st_write(profiel, paste0(workspace,  "output/GIS/profiel.gpkg"), append = FALSE)

profiel_wide <- st_as_sf(profiel_wide)
st_write(profiel_wide, paste0(workspace,  "output/GIS/profiel_wide.gpkg"), append = FALSE)
write.table(profiel_wide, file = paste0('output/Database/profielen_hoek.csv' ), sep = ';',dec = '.', row.names = F)

## abiotiek-----------------------------------------------------------------
write.table(abio, file = paste0('output/Database/abio.csv' ), sep = ';',dec = '.', row.names = F)
write.table(locs_abio, file = paste0('output/Database/locs_abio.csv' ), sep = ';',dec = '.', row.names = F)
write.table(loc.wq, file = paste(workspace,"/output/Database/monst_codes",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)

## vegetatie-----------------------------------------------------------------
write.table(veg, file = paste0('output/Database/veg.csv' ), sep = ';',dec = '.', row.names = F)

## locaties per cluster export----------------------------------------------
st_write(clusters_locs, paste0(workspace,  "output/GIS/clusters_locs_abio_2025.gpkg"), append = TRUE)
setDT(clusters_locs)
write.table(clusters_locs, file = paste(workspace,"/output/Database/clusters_locs_abio",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)

## penetrometerdata -------------------------------------------------------
# write.table(penmergecheck, file = paste(workspace,"/output/penetrometer_gps_check",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
write.table(locs_pen, file = paste(workspace,"/output/Database/locs_penetrometer",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
gps <- st_as_sf(gps_24)
st_write(gps, paste0(workspace,  "output/GIS/gps_penetrometer.gpkg"), append = FALSE)
setDT(gps)
write.table(gps, file = paste(workspace,"/output/Database/gps_penetrometer",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
gps25 <- st_as_sf(gps25)
st_write(gps25, paste0(workspace,  "output/GIS/gps_penetrometer_25.gpkg"), append = FALSE)
setDT(gps25)
write.table(gps25, file = paste(workspace,"/output/Database/gps_penetrometer_25",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)

## locatieinfo Bware ------------------------------------------------------
write.table(abio_proj[jaar == '2025' & WP == 'WP1',c('SlootID','Gebiedsnaam','jaar','Slibmonster_Bware','Waterkwaliteitmonster_Bware','drlg','max_wtd','max_slib','watbte','slib_conductiviteit_uS_cm','slib_O2_mgL','slib_redox_mgL','slib_pH','BODEMCODE','veentype','trofie','text',
                         "Start_traject_long","Start_traject_lat" ,"End_traject_long","End_traject_lat")], 
            file = paste(workspace,"/output/Database/locs_info_Bware",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
## pars voor indices---------------------------------
write.table(abio_proj[,c('SlootID','oever','Waterkwaliteitmonster_Bware','drlg','max_wtd','max_slib','watbte','slib_conductiviteit_uS_cm','slib_O2_mgL','slib_redox_mgL','slib_pH','BODEMCODE','veentype','trofie','text',
                         "Start_traject_long","Start_traject_lat" ,"End_traject_long","End_traject_lat")], 
            file = paste(workspace,"/output/Database/locs_info_Floronindexfiguur",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)

## alles db ---------------------------------------------------------
# Find duplicate SlootID-jaar combinations
abio_proj <- abio_proj[WP %in% c('WP1','WP2'),]
dubbelen <- abio_proj[, .N, by = c('SlootID','jaar','instanceID_veg','instanceID_abio')][N > 1]
dubbelen <- abio_proj[, .N, by = c('SlootID','jaar')][N > 1]
# wegschrijven tabel
write.table(abio_proj, file = paste(workspace,"output/Database/db_veest",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), na = "", sep =';', dec = '.',row.names = FALSE)
write.table(abio_proj, file = paste(workspace2,"db_veest",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), na = "", sep =';', dec = '.',row.names = FALSE)

## gebiedsanalyses ------------------------------------------------------
gebiedsanalyse <- abio_proj[,c('Gebiedsnaam','gebied','SlootID','Sloot_nr','clusters','drglg','wl','max_hgt_or','watbte','max_wtd','max_slib','veentype', 'trofie','Z_CLAY_SA_OR_25','OS_perc_OR_25','P-AL mg p2o5/100g_OR_25','P-PO4_CC_mg/kg_OR_25','pH_CC_OR_25','holleoever','oever_(20,30]')]
write.table(gebiedsanalyse, file = paste(workspace,"/output/Database/gebiedsanalyse",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), na = "", sep =';', dec = '.',row.names = FALSE)
setDT(locaties)
abio_proj_loc <- merge(locaties[,c('SlootID','geom')], gebiedsanalyse, by = 'SlootID', all.x= TRUE, suffixes =  c('_locs','_db'))
abio_proj_loc <- st_as_sf(abio_proj_loc) %>% st_transform(crs = 28992)
st_write(abio_proj_loc, paste0(workspace,  "output/GIS/gebiedsanalyse.gpkg"), append = FALSE)
