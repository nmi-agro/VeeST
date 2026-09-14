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
locaties[!WP == 'WP2-prenul',]
## aggregated abio data ---------------
abio_proj <- abio_hier
abio_proj[, jaar := as.integer(jaar)]
# check welke locaties missen in data abiotiek
check_db <- locaties[!SlootID %in% unique(abio_proj$SlootID),]
## slootprofielen ---------------
# 1) Neem alleen kolommen die géén list zijn (dus geom/sfc valt weg)
cmp_cols <- names(locs_prof)[!vapply(locs_prof, is.list, logical(1))]
# 2) Dedupe via base::duplicated op data.frame (om data.table list-fout te vermijden)
dup <- duplicated(as.data.frame(locs_prof[, ..cmp_cols]))
# 3) Unieke rijen
locs_prof <- locs_prof[!dup]
# 4) Controle
list(
  verwijderd = sum(dup),
  over = nrow(locs_prof),
  geom_in_cmp = "geom" %in% cmp_cols
)
locs_prof[, jaar := as.integer(jaar)]
abio_proj <- merge(abio_proj, locs_prof[,-c('SlootID-kort','gebied','sloot','Sloot_nr','Gebiedsnaam','Behandeling','oever','instanceID_abio','instanceID_veg','datum','WP')], by = c('SlootID','jaar'), all.x = T, suffixes = c('','_prof'))
# 34 locaties missen in profielen en abiotiek omdat pre-nul en demmerik en Mijnden
check_db <- locaties[!SlootID %in% unique(locs_prof$SlootID),] # 349 unieke slootIDs in locaties en niet in data (pre-nul en demmerik)
## penetrometer data ---------------
#merge db en penetrometer
penmerge_wide[, jaar := as.integer(jaar)]
penmerge_wide <- unique(penmerge_wide)
abio_proj <- merge(abio_proj, penmerge_wide, by = c('SlootID','jaar'), all.x = T, suffixes = c('','_pen'))
# 145 locaties missen in penetrometerdata omdat 2025 ontbreekt
check_db <- locaties[!SlootID %in% unique(penmerge_wide$SlootID),]
## vegetatie --------------
# slootid jaar niet uniek?
veg[, jaar := as.integer(jaar)]
abio_proj <- merge(abio_proj, veg, by.x = c('instanceID_veg'), by.y = c('instanceID'), all.x = T, suffixes = c('','_veg'))
abio_proj[, jaar := as.integer(jaar)]
check_db <- locaties[!SlootID %in% unique(veg$SlootID),]
## vegetatie aantal soorten------------------
veg_nsoorten[, jaar := as.integer(jaar)]
abio_proj <- merge(abio_proj, veg_nsoorten, by = c('SlootID','jaar'), all.x = T, suffixes = c('','_vegsrt'))
## vegetatie ekr en oeverindex------------------
abio_proj <- merge(abio_proj, veg_ekr_oev, by.x = c('SlootID','jaar'), by.y = c('SlootID','jaar'), all.x = T, suffixes = c('','_veg_ekr'))
## clusters en locatiedata --------------------------------------------------------
#!!! check slootID jaar combinatie uniek (is nu niet het geval in clusters_locs)
clusters_locs[, jaar := as.integer(jaar)]
abio_proj <- merge(abio_proj, clusters_locs[,-c('geom')], by = c('SlootID','jaar'), all.x = T, suffixes = c('','_clust'))
abio_proj <- abio_proj[!is.na(SlootID),]
check_db <- locaties[!SlootID %in% unique(clusters_locs$SlootID),]
# Unieke sleutel op SlootID + jaar
loc_key <- unique(
  locaties[!is.na(SlootID) & !is.na(jaar),
           .(SlootID, jaar, Gebiedsnaam_loc = Gebiedsnaam, gebied_loc = gebied, vernat_loc = `Vernat?`)]
)
# Als er toch dubbelen zijn: kies eerste niet-NA
loc_key <- loc_key[
  order(SlootID, jaar, is.na(Gebiedsnaam_loc))
][, .SD[1], by = .(SlootID, jaar)]
# Koppel en vul
abio_proj <- merge(abio_proj, loc_key, by = c("SlootID", "jaar"), all.x = TRUE)
abio_proj[is.na(Gebiedsnaam), Gebiedsnaam := Gebiedsnaam_loc]
abio_proj[is.na(gebied), gebied := gebied_loc]
abio_proj[, c("Gebiedsnaam_loc", "gebied_loc") := NULL]
abio_proj[is.na(vernat_loc), vernat := "nee"]
# Fallback op alleen SlootID (als jaar mismatcht)
loc_key_sid <- unique(
  locaties[!is.na(SlootID) & !is.na(Gebiedsnaam),
           .(SlootID, Gebiedsnaam_sid = Gebiedsnaam)]
)
abio_proj <- merge(abio_proj, loc_key_sid, by = "SlootID", all.x = TRUE)
abio_proj[is.na(Gebiedsnaam), Gebiedsnaam := Gebiedsnaam_sid]
abio_proj[, Gebiedsnaam_sid := NULL]

abio_proj[, .(
  n = .N,
  n_na_Gebiedsnaam = sum(is.na(Gebiedsnaam)),
  n_na_gebied = sum(is.na(gebied))
), by = jaar][order(jaar)]
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
abio_proj[SlootID == "MD_8_NVO_N"  & is.na(Oevermonster_AgroCares) & jaar == 2024,
          Oevermonster_AgroCares := "MD_8_NVO"]
abio_proj[SlootID == "MD_8a_NVO_N" & is.na(Oevermonster_AgroCares) & jaar == 2024,
          Oevermonster_AgroCares := "MD_8_NVO1"]
abio_proj <- merge(abio_proj, oever_ac_25, by.x = c('Oevermonster_AgroCares','jaar'), by.y = c('SlootID_kort_OR_25','jaar'), all.x = T, suffixes = c('_SB','_OR'))
abio_proj <- merge(abio_proj, oever_ac_50, by.x = c('Oevermonster_AgroCares','jaar'), by.y = c('SlootID_kort_OR_50','jaar'), all.x = T, suffixes = c('_25','_50'))
check_db <- locaties[!Oevermonster_AgroCares %in% unique(oever_ac_25$SlootID_kort),]

## veraard veen ---------------
veraardveen <- veraardveen[!(Slootcode == "SW_2_M" & Behandeling == "M_O"),]
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
## afgeleide koeien variabelen bepalen----------------------------------------------
to_flag <- function(x) {
  y <- tolower(trimws(as.character(x)))
  y[y %in% c("", "na", "nan", "n.v.t./onbekend", "nvt", "onbekend")] <- NA_character_
  fcase(
    y %in% c("ja", "yes", "y", "1", "true", "wel", "koeien drinken uit sloot"), TRUE,
    y %in% c("nee", "no", "n", "0", "false", "niet", "koeien drinken niet uit sloot"), FALSE,
    default = NA
  )
}
abio_proj[, Aantal_koeien_vee_perceel_dag := as.numeric(Aantal_koeien_vee_perceel_dag)]
abio_proj[, Aantal_Koedagen_per_jaar := as.numeric(Aantal_Koedagen_per_jaar)]
abio_proj[, afr_raw := fcoalesce(
  suppressWarnings(as.numeric(uitraster_perc_slootid))
)]

abio_proj[, drinken_flag := to_flag(Koeien_drinken_sloot)]
abio_proj[, afrastering_flag := fifelse(is.na(afr_raw), FALSE, afr_raw > 0)]
abio_proj[, drinkbak_flag := to_flag(Drinkbakken_aanwezig)]

abio_proj[, koebelasting_drinkende_koeien := fcase(
  afrastering_flag %in% TRUE, 0,
  drinken_flag %in% FALSE, 0,
  is.na(Aantal_koeien_vee_perceel_dag) | is.na(Aantal_Koedagen_per_jaar), NA_real_,
  default = pmax(Aantal_koeien_vee_perceel_dag, 0) * (pmax(Aantal_Koedagen_per_jaar, 0)/365) *  pmax(omtrek_nat, 0)
)]
abio_proj[, koeien_drinken_correctie := fifelse(
  koebelasting_drinkende_koeien > 0,
  "Wel drinken uit sloot",
  "Geen drinken uit sloot"
)]
# controle welke is 
abio_proj[SlootID %in% unique(abio_proj[afrastering_flag %in% TRUE & drinken_flag %in% TRUE, SlootID]), .(
  SlootID, jaar, uitraster_perc_slootid, uitraster_perc, afr_raw, afrastering_flag, drinken_flag, koebelasting_drinkende_koeien
)]
abio_proj[SlootID %in% unique(abio_proj[afrastering_flag %in% TRUE & drinkbak_flag %in% TRUE, SlootID]), .(
  SlootID, jaar, uitraster_perc_slootid, uitraster_perc, afr_raw, afrastering_flag, drinken_flag, drinkbak_flag, koebelasting_drinkende_koeien
)]
abio_proj[, .(
  n = .N,
  median_koebelasting = median(koebelasting_drinkende_koeien, na.rm = TRUE)
), by = .(afrastering_flag, drinken_flag)][order(afrastering_flag, drinken_flag)]

## add grouping vars -------------------------------
abio_proj[text == "Hoogheemraadschap De Stichtse Rijnlanden",waterschap := 'HDSR']
abio_proj[text == "Hoogheemraadschap Hollands Noorderkwartier" ,waterschap := 'HHNK']
abio_proj[text == "Waterschap Amstel, Gooi en Vecht",waterschap := 'AGV']
abio_proj[text == "Hoogheemraadschap van Rijnland",waterschap := 'Rijnland']
abio_proj[text == "Wetterskip Fryslân",waterschap := 'Fryslân']
abio_proj[text == "Waterschap Drents Overijsselse Delta",waterschap := 'WDOD']
abio_proj[text == "Hoogheemraadschap van Schieland en de Krimpenerwaard",waterschap := 'HHSK']
abio_proj[text == "Waterschap Vallei en Veluwe",waterschap := 'WVV']
abio_proj[text == "Waterschap Zuiderzeeland",waterschap := 'ZZL']
abio_proj[text == "Hoogheemraadschap van Delfland",waterschap := 'HDL']
abio_proj[gebied == 'EEM',waterschap := 'WVV']
abio_proj[gebied == 'BD',waterschap := 'Fryslân']
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
## adjust penetrometer data for analysis -------------------
#122160 rijen
penmerge[,Diept := as.numeric(Diept)]
penmerge[,dieptebin := cut(Diept, breaks = seq(from = 0, to = 80, by = 5), include.lowest = TRUE), by= .(SlootID, jaar)]
penmerge[,sectie_f := factor(sectie, levels=c('oever','insteek','perceel')), by= .(SlootID, jaar)]
veentype_unique <- abio_proj[, .SD[1], by = SlootID, .SDcols = c('veentype')]
penmerge <- merge(penmerge, veentype_unique, by = "SlootID", all.x = TRUE) #Bereken gemiddelde drooglegging per gebied voor de bars (hergebruik bestaande code)
penmerge[,jaar := as.integer(jaar)]
loc_pen <- unique(locaties[, c('SlootID','Sloot_nr','Gebiedsnaam','WP','jaar','Behandeling')])
loc_pen <- loc_pen[!WP == 'WP2-prenul',]
loc_pen <- loc_pen[!duplicated(loc_pen[,c('SlootID','jaar')]),]
dups <- loc_pen[, .N, by = .(SlootID, jaar)][N > 1, .(SlootID, jaar)]
loc_pen[dups, on = .(SlootID, jaar)][order(SlootID, jaar)] |> head(20)
# merge
penmerge <- merge(penmerge, loc_pen, by = c("SlootID", "jaar"), all.x = TRUE)

# fix: maak 1 standaardkolom Behandeling
if (!"Behandeling" %in% names(penmerge)) {
  penmerge[, Behandeling := fcoalesce(
    if ("Behandeling.x" %in% names(penmerge)) Behandeling.x else NA_character_,
    if ("Behandeling.y" %in% names(penmerge)) Behandeling.y else NA_character_
  )]
  drop_cols <- intersect(c("Behandeling.x", "Behandeling.y"), names(penmerge))
  if (length(drop_cols) > 0L) penmerge[, (drop_cols) := NULL]
}

# beheer
penmerge[, beheer := "regulier"]
penmerge[grepl("M", Behandeling), beheer := "minimaal"]
penmerge[grepl("M-AF", Behandeling), beheer := "minimaal + afrastering"]
penmerge[grepl("R-AF", Behandeling), beheer := "regulier + afrastering"]
penmerge[grepl("AF", Behandeling), beheer := "afrastering"]
penmerge[grepl("NVO", Behandeling), beheer := "NVO"]
penmerge[grepl("NVO-AF", Behandeling), beheer := "NVO + afrastering"]
penmerge <- penmerge[!is.na(gebied),]
penmerge[, jaar := as.integer(jaar)]
unique(penmerge[is.na(Gebiedsnaam),c('SlootID','jaar','Gebiedsnaam','gebied','name_gps','name_pen','oever')])

## adjust abiotic data for analysis -------------------
abio_proj[, slib_redox_pH7 := slib_redox_mgL + (7 - slib_pH) * 59]
abio_proj[, water_redox_pH7 := water_redox + (7 - slib_pH) * 59]
abio_proj[water_redox_pH7 > 800, water_redox_pH7 := water_redox_pH7/10] # correctie foutieve waarden redox
# draagkracht_oever, draagkracht_perceel, draagkracht_perceel_diep en draagkracht_insteek
# worden nu in data_import_ppr.R rechtstreeks uit de ruwe penetrometermetingen berekend
# (niet meer als gemiddelde van gemiddelde dieptebins) en komen al mee in penmerge_wide.

abio_proj[, slibdiepte := max_slib + max_wtd]
abio_proj[, doorzicht2_mid_m :=  doorzicht2_mid_cm/100]
# Zichtdiepte (doorzicht/waterdiepte): eerst de ratio berekenen, daarna
# normaliseren op het maximum van die ratio over de HELE dataset (i.p.v. een
# harde cap op 1 per punt). Zonder deze stap bepaalden gebieden met erg
# ondiep water (kleine max_wtd) een ratio >> 1 voor bijna alle punten, die
# vervolgens allemaal naar dezelfde cap-waarde afgekapt werden.
.zichtdiepte_ratio <- fifelse(
  is.na(abio_proj$doorzicht2_mid_m) | is.na(abio_proj$max_wtd) | abio_proj$max_wtd == 0,
  NA_real_, abio_proj$doorzicht2_mid_m / abio_proj$max_wtd
)
.zichtdiepte_max <- max(.zichtdiepte_ratio, na.rm = TRUE)
abio_proj[, zichtdiepte := fifelse(
  is.na(.zichtdiepte_ratio), NA_real_,
  pmin(.zichtdiepte_ratio / .zichtdiepte_max, 1)
)]
rm(.zichtdiepte_ratio, .zichtdiepte_max)
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
abio_proj[,holleoever1 := holleoever]
abio_proj[,holleoever := rowMeans(.SD, na.rm = TRUE),
          .SDcols = c("holleoever1", "holleoever2", "holleoever3", "holleoever4", "holleoever5")]
abio_proj[holleoever > 150, holleoever := holleoever/10]
#remove foute waarde O2
abio_proj[water_O2_mgL > 100, water_O2_mgL := water_O2_mgL/100]
abio_proj[water_O2_mgL > 20, water_O2_mgL := water_O2_mgL/10]
# berekenen N mineraal in oever en slib
abio_proj[, N_mineraal_OR_25 := {
  s <- rowSums(.SD, na.rm = TRUE)
  ifelse(rowSums(!is.na(.SD)) == 0, NA_real_, s)
}, .SDcols = c("N-NH4_CC_mg/kg_OR_25", "N-NO3_CC_mg/kg_OR_25", "N-NO2_CC_mg/kg_OR_25")]
abio_proj[,N_mineraal_SB := {
  s <- rowSums(.SD, na.rm = TRUE)
  ifelse(rowSums(!is.na(.SD)) == 0, NA_real_, s)
}, .SDcols = c("N-NH4_CC_mg/kg_SB","N-NO3_CC_mg/kg_SB","N-NO2_CC_mg/kg_SB")]
abio_proj[,N_mineraal_OR_50 := {
  s <- rowSums(.SD, na.rm = TRUE)
  ifelse(rowSums(!is.na(.SD)) == 0, NA_real_, s)
}, .SDcols = c("N-NH4_CC_mg/kg_OR_50","N-NO3_CC_mg/kg_OR_50","N-NO2_CC_mg/kg_OR_50")]
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

## correct values that contain commas and are read as characters instead of numeric
abio_proj[, names(abio_proj) := lapply(.SD, function(x) {
  if (is.character(x)) gsub(";", ":", iconv(x, to = "UTF-8", sub = "byte")) else x
})]

## indices berekenen -------------------------------
# Hulpfunctie: rowMeans maar NA als alle waarden in een rij NA zijn
rowMeans_na <- function(...) {
  m <- cbind(...)
  ifelse(rowSums(!is.na(m)) == 0, NA_real_, rowMeans(m, na.rm = TRUE))
}

### Kraggevorming vlag (herzien): alleen kragge% zone 2b, drempel > 50% -------
# als TRUE dan kraggevorming (kragge-oever), gebruikt als kleurcodering in de
# erosieindex-plot (kragge oevers remmen erosie)
abio_proj[, kraggevorming_flag := fifelse(is.na(oeverzone_2b_kraggen_perc), NA,
                                           oeverzone_2b_kraggen_perc > 25)]

### Oevervormindex: geometrie oever ----------------------------------------------
# Flauwer talud (lage taludhoek rond waterlijn) = hogere index
# Grilliger oeverlijn (oeverzone_2b_grillig) = hogere index
# tldk_oevrwtr_perc: hoek rand waterlijn als % (lager = flauwer = beter)
# Componenten worden ook als losse kolommen bewaard t.b.v. visualisatie/diagnose
abio_proj[, oevervorm_talud_norm := fifelse(is.na(tldk_oevrwtr_perc), NA_real_, 1 - pmin(tldk_oevrwtr_perc / 100, 1))]
# oeverzone_2b_grillig is character ("matig"/"uniform"/"zeer"); via factor() naar ordinale code
abio_proj[, oevervorm_grillig_norm := fifelse(is.na(oeverzone_2b_grillig), NA_real_,
                                               pmin((as.numeric(factor(oeverzone_2b_grillig, levels = c("uniform","matig","zeer"))) - 1) / 2, 1))]
abio_proj[, oevervormindex := rowMeans_na(oevervorm_talud_norm, oevervorm_grillig_norm)]

# Draagkracht oever genormaliseerd op dataset-max: puur diagnostisch, geen
# onderdeel van de erosieindex- of vernattingsrisico-formule. Geïnverteerd,
# net als draagkracht perceel: lagere draagkracht = instabieler = meer erosie.
.dk_max_diag <- max(abio_proj$draagkracht_oever, na.rm = TRUE)
abio_proj[, stabiliteit_draagkracht_oever_norm := fifelse(is.na(draagkracht_oever), NA_real_, 1 - (draagkracht_oever / .dk_max_diag))]
rm(.dk_max_diag)

# Export voor rapport (draagkracht-diagnostiek: spreiding in tijd vs. tussen gebieden)
saveRDS(
  abio_proj[, .(SlootID, jaar, Gebiedsnaam, draagkracht_perceel_diep, draagkracht_oever)],
  paste0(workspace, "output/rapport/draagkracht_diagnostiek.rds")
)

### Erosieindex: hoog = meer erosie -----------------------------------------------
# Componenten van de indexformule: afscheur (hoog = meer erosie), onderholling
#   (hoog = meer erosie), waterdiepte/slibdikte (veel erosie leidt tot veel
#   bagger; hoog = meer erosie), draagkracht perceel 50-80 cm diepte
#   (geïnverteerd: lagere draagkracht = meer erosie).
# Kraggevorming wordt NIET in de berekening meegenomen, alleen als
# kleurcodering getoond in de componentenplot (kragge-oevers remmen erosie).
# Draagkracht oever is verwijderd als indexcomponent (dit is de gemeten
# draagkracht oever zelf, apart getoond als diagnostisch facet).
# Kale oever en oevervormindex zijn verwijderd als component (kale oever
# hoort bij de vernattingsrisico-index; oevervormindex is een aparte index
# en wordt niet meer meegewogen in de erosieindex).
# Alle componenten genormaliseerd naar 0-1 schaal
# Componenten worden ook als losse kolommen bewaard t.b.v. visualisatie/diagnose
# Afscheur-oppervlak: % lengte x breedte (cm) is een oppervlak-achtige maat,
# geen percentage meer; normaliseer op dataset-max en cap op 1 zodat de
# component weer op de 0-1 schaal valt (hoog = meer erosie)
abio_proj[, afscheur_opp := afscheur_veg_lengte_perc * afscheur_veg_breedte_cm]
.afscheur_opp_max <- max(abio_proj$afscheur_opp, na.rm = TRUE)
abio_proj[, erosie_afscheur_norm := fifelse(is.na(afscheur_opp), NA_real_, pmin(afscheur_opp / .afscheur_opp_max, 1))]
rm(.afscheur_opp_max)
# Onderholling: cm, normaliseer op max 150 cm (gecorrigeerde waarde) en cap op 1
abio_proj[, erosie_onderholling_norm := fifelse(is.na(holleoever), NA_real_, pmin(holleoever / 150, 1))]
# Waterdiepte/slibdikte: max_wtd (m) / max_slib (m), genormaliseerd op het
# 95e-percentiel van de ratio (i.p.v. het absolute dataset-max), vervolgens
# geïnverteerd (lage ratio = veel slib t.o.v. water = meer erosie).
# Normaliseren op het absolute max bleek gevoelig voor een handjevol punten
# met een bijna-nul max_slib (< 2 cm), die een extreme ratio (tot ~118)
# veroorzaakten; die ene uitschieter trok dan alle overige waarden richting 1,
# zodat er in de boxplots per gebied geen verschil meer zichtbaar was.
.wtd_slib_ratio <- fifelse(
  is.na(abio_proj$max_wtd) | is.na(abio_proj$max_slib) | abio_proj$max_slib == 0,
  NA_real_, abio_proj$max_wtd / abio_proj$max_slib
)
.wtd_slib_p95 <- quantile(.wtd_slib_ratio, probs = 0.95, na.rm = TRUE)
abio_proj[, erosie_wtd_slib_norm := fifelse(
  is.na(.wtd_slib_ratio), NA_real_,
  1 - pmin(.wtd_slib_ratio / .wtd_slib_p95, 1)
)]
rm(.wtd_slib_p95, .wtd_slib_ratio)
# Draagkracht perceel 50-80 cm diepte: laag = instabieler = meer erosie, dus geïnverteerd op dataset-max
.dk_perceel_diep_max_erosie <- max(abio_proj$draagkracht_perceel_diep, na.rm = TRUE)
abio_proj[, erosie_draagkracht_perceel_diep_norm := fifelse(
  is.na(draagkracht_perceel_diep), NA_real_,
  1 - pmin(draagkracht_perceel_diep / .dk_perceel_diep_max_erosie, 1)
)]
rm(.dk_perceel_diep_max_erosie)
# Kraggevorming: alleen voor visualisatie (kleurcodering), geen onderdeel van de berekening
abio_proj[, erosie_kragg_factor      := fifelse(is.na(kraggevorming_flag), 1, fifelse(kraggevorming_flag, 0.5, 1))]
# Oppervlak emers zone 2a (perc x breedte, in m2): hoog = meer bescherming = minder erosie, dus geïnverteerd
# Normaliseren op het 95e-percentiel i.p.v. het absolute dataset-max: het max
# (~4,8 m2) is een sterke uitschieter t.o.v. het 99e-percentiel (~1,07 m2) en
# de mediaan (~0,12 m2); normaliseren op het max drukte daardoor vrijwel alle
# metingen na inversie richting 1, zodat er geen spreiding meer zichtbaar was
# (zelfde probleem als destijds bij de waterdiepte/slibdikte-component).
abio_proj[, oeverzone_2a_emers_m2 := (oeverzone_2a_emers_perc / 100) * (oeverzone_2a_breedte_cm / 100)]
.emers2a_opp_p95_erosie <- quantile(abio_proj$oeverzone_2a_emers_m2, probs = 0.95, na.rm = TRUE)
abio_proj[, erosie_emers2a_norm := fifelse(is.na(oeverzone_2a_emers_m2), NA_real_,
                                            1 - pmin(oeverzone_2a_emers_m2 / .emers2a_opp_p95_erosie, 1))]
rm(.emers2a_opp_p95_erosie)

abio_proj[, erosieindex := rowMeans_na(
  erosie_afscheur_norm, erosie_onderholling_norm,
  erosie_wtd_slib_norm, erosie_draagkracht_perceel_diep_norm,
  erosie_emers2a_norm
)]

### Vernattingsrisico-index: hoog = veel risico bij vernatten -------------------------
# Een kale, (productie)grasoever die steil is geeft een groot risico; een hoog
# oppervlak emers 2b verlaagt het risico.
# Componenten: kale/productiegras-oever % (hoog = risico), taludhoek/steilheid
# (hoog = risico), oppervlak emers 2b (hoog = verlaagt risico, dus geïnverteerd)
# Oppervlak emers zone 2b (perc x breedte, in m2)
abio_proj[, oeverzone_2b_emers_m2 := (oeverzone_2b_emers_perc / 100) * (oeverzone_2b_breedte_cm / 100)]
# Normalisaties op max van dataset worden vooraf berekend (buiten := blok)
.emers2b_opp_max <- max(abio_proj$oeverzone_2b_emers_m2, na.rm = TRUE)
# Componenten worden ook als losse kolommen bewaard t.b.v. visualisatie/diagnose
# Kale/productiegras-oever: som van kale oever % en productiegras % (zone 2b), gecapt op 100
abio_proj[, vernatting_kaal_gras_perc := pmin(
  fifelse(is.na(oeverzone_2b_kaal_perc), 0, oeverzone_2b_kaal_perc) +
    fifelse(is.na(oeverzone_2b_ter_prodgras_perc), 0, oeverzone_2b_ter_prodgras_perc),
  100
)]
abio_proj[, vernatting_kaal_gras_norm := fifelse(
  is.na(oeverzone_2b_kaal_perc) & is.na(oeverzone_2b_ter_prodgras_perc), NA_real_,
  vernatting_kaal_gras_perc / 100
)]
# Steilheid oever: taludhoek rand waterlijn (hoog % = steil = risico)
abio_proj[, vernatting_steilte_norm := fifelse(is.na(tldk_oevrwtr_perc), NA_real_, pmin(tldk_oevrwtr_perc / 100, 1))]
# Oppervlak emers 2b (perc x breedte, in m2): hoog = lager risico, dus geïnverteerd
abio_proj[, vernatting_emers2b_norm := fifelse(is.na(oeverzone_2b_emers_m2), NA_real_,
                                                1 - pmin(oeverzone_2b_emers_m2 / .emers2b_opp_max, 1))]
rm(.emers2b_opp_max)

abio_proj[, vernattingsrisico_index := rowMeans_na(
  vernatting_kaal_gras_norm, vernatting_steilte_norm,
  vernatting_emers2b_norm
)]

### indices visualiseren ------------------------------------

idx_long <- melt(
  abio_proj[, .(erosieindex, oevervormindex, vernattingsrisico_index)],
  measure.vars = c("erosieindex","oevervormindex","vernattingsrisico_index"),
  variable.name = "index", value.name = "waarde"
)
idx_long[, index_label := factor(index,
  levels = c("erosieindex","oevervormindex","vernattingsrisico_index"),
  labels = c(
    "Erosieindex\n← weinig erosie    veel erosie →",
    "Oevervormindex\n← steil, recht    flauw, grillig →",
    "Vernattingsrisico-index\n← laag risico    hoog risico →"
  )
)]

p_hist <- ggplot(idx_long, aes(x = waarde)) +
  geom_histogram(bins = 30, na.rm = TRUE) +
  facet_wrap(~index_label, scales = "free_x") +
  labs(x = NULL, y = "Aantal", title = "Verdeling indices") +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14),
        strip.text = element_text(size = 14))

p_hist

idx_gebied <- melt(
  abio_proj[, .(Gebiedsnaam, erosieindex, oevervormindex, vernattingsrisico_index)],
  id.vars = "Gebiedsnaam",
  measure.vars = c("erosieindex","oevervormindex","vernattingsrisico_index"),
  variable.name = "index", value.name = "waarde"
)
idx_gebied[, index_label := factor(index,
  levels = c("erosieindex","oevervormindex","vernattingsrisico_index"),
  labels = c("Erosieindex", "Oevervormindex", "Vernattingsrisico-index")
)]

# Sorteer gebieden op mediaan erosieindex (gebruikt door de componentenplots verderop)
gebied_order <- abio_proj[, .(med = median(erosieindex, na.rm = TRUE)), by = Gebiedsnaam][order(med), Gebiedsnaam]
# Sorteer gebieden op mediaan onderholling (holleoever) voor de hoofdindexplot,
# zelfde volgorde als het losse componentenplot verderop in het script
gebied_order_onderholling <- abio_proj[, .(med = median(holleoever, na.rm = TRUE)), by = Gebiedsnaam][order(med), Gebiedsnaam]
idx_gebied[, Gebiedsnaam := factor(Gebiedsnaam, levels = gebied_order)]

ggplot(idx_gebied[!is.na(Gebiedsnaam),], aes(x = waarde, y = Gebiedsnaam)) +
  geom_boxplot(outlier.size = 1) +
  facet_wrap(~index_label, scales = "free_x") +
  labs(x = "Indexwaarde", y = NULL, title = "Indices per gebied") +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(size = 14),
        strip.text = element_text(size = 14), axis.title.x = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"))

## erosieindex: componenten uitgesplitst -----------------------------------
# Laat per gebied zien welke component (afscheur, onderholling,
# waterdiepte/slibdikte, draagkracht perceel 50-80 cm [geïnverteerd]) de
# erosieindex omhoog of omlaag drukt.
# Kraggevorming wordt niet als apart facet getoond, maar als kleur van de
# individuele meetpunten (jitter) bovenop de boxplots van alle panelen: groen =
# wel kraggevorming (kragge% zone 2b > 50, remt erosie), rood = geen kraggevorming.
# Kraggevorming is GEEN onderdeel van de indexformule, alleen visualisatie.
# Draagkracht oever wordt als extra diagnostisch facet getoond (geen
# onderdeel van de erosieindex-formule). Oevervormindex en kale oever zijn
# geen onderdeel meer van de erosieindex en worden hier niet getoond
# (oevervormindex is een aparte index, kale oever hoort bij vernattingsrisico).
# Hoogteverschil waterlijn-oever (hgt_wl_oever) is uit dit plot verwijderd.
erosie_comp_long <- melt(
  abio_proj[, .(Gebiedsnaam, erosieindex,
                afscheur                 = erosie_afscheur_norm,
                onderholling             = erosie_onderholling_norm,
                wtd_slib                 = erosie_wtd_slib_norm,
                draagkracht_perceel_diep = erosie_draagkracht_perceel_diep_norm,
                opp_emers_2a             = erosie_emers2a_norm,
                draagkracht_oever        = stabiliteit_draagkracht_oever_norm,
                erosieindex_comp         = erosieindex,
                kleigehalte              = Z_CLAY_SA_OR_25)],
  id.vars = c("Gebiedsnaam", "erosieindex", "kleigehalte"),
  measure.vars = c("erosieindex_comp", "afscheur", "onderholling", "wtd_slib",
                   "draagkracht_perceel_diep", "opp_emers_2a", "draagkracht_oever"),
  variable.name = "component", value.name = "waarde_norm"
)
erosie_comp_long[, component := factor(component,
  levels = c("erosieindex_comp", "afscheur", "onderholling", "wtd_slib",
             "draagkracht_perceel_diep", "opp_emers_2a", "draagkracht_oever"),
  labels = c("Erosieindex",
             "Afscheur \n(veg. lengte %)\nhoog = meer erosie",
             "Onderholling \n(holleoever)\nhoog = meer erosie",
             "Waterdiepte/slibdikte \nhoog = meer erosie \n(relatief veel slib t.o.v. water)",
             "Draagkracht perceel \n50-80cm \nhoog = meer erosie \n(lagere draagkracht)",
             "Oppervlak emers 2a\n (geïnverteerd)\nhoog = meer erosie\n(weinig emerse vegetatie)",
             "Draagkracht oever\n (geïnverteerd)\nhoog = meer erosie\n(GEEN deel index)")
)]

# Sorteer gebieden op mediaan erosieindex (zelfde volgorde als eerdere plot)
erosie_comp_long[, Gebiedsnaam := factor(Gebiedsnaam, levels = gebied_order)]

# Boxplot per component en gebied, met kleigehalte-meetpunten op alle panelen
ggplot(erosie_comp_long[!is.na(Gebiedsnaam),], aes(x = waarde_norm, y = Gebiedsnaam)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(
    data = erosie_comp_long[!is.na(Gebiedsnaam) & !is.na(kleigehalte)],
    aes(color = kleigehalte),
    height = 0.2, width = 0, size = 4, alpha = 0.6
  ) +
  facet_wrap(~component, scales = "free_x", nrow = 1) +
  scale_color_viridis_c(name = "Kleigehalte\n25cm (%)") +
  labs(
    x = "Genormaliseerde waarde (0-1)", y = NULL,
    title = "Erosieindex componenten per gebied",
    subtitle = paste0(
      "Erosieindex = gemiddelde(afscheur, onderholling, waterdiepte/slibdikte [geïnverteerd], draagkracht perceel 50-80cm [geïnverteerd], oppervlak emers 2a [geïnverteerd])\n",
      "Kleigehalte is GEEN onderdeel van de formule, alleen getoond als kleur; draagkracht oever is een extra diagnostisch paneel, geen onderdeel van de formule\n",
      "Hogere waarde = grotere bijdrage aan erosie; kleur van de punten geeft het kleigehalte (%) op 25cm diepte weer"
    )
  ) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14),
        strip.text = element_text(size = 14), legend.position = "bottom",
        legend.text = element_text(size = 14), legend.title = element_text(size = 14), 
        legend.subtitle = element_text(size = 14))

## erosieindex: totale bedekking zone 2a (extra, niet in index) ------------
# Puur visueel/diagnostisch: verdeling van de bedekkingscomponenten in
# oeverzone 2a (kaal, submers totaal, natans, emers, kragge, gras/kruiden,
# hout). Geen van deze kolommen maakt onderdeel uit van de erosieindex-formule.
bedekking_2a_long <- melt(
  abio_proj[, .(Gebiedsnaam,
                kaal        = oeverzone_2a_kaal_perc,
                submers     = oeverzone_2a_subm_tot_perc,
                natans      = oeverzone_2a_natans_perc,
                emers       = oeverzone_2a_emers_perc,
                kragge      = oeverzone_2a_kraggen_perc,
                gras_kruid  = oeverzone_2a_ter_gras_kruid_perc,
                hout        = oeverzone_2a_hout_perc)],
  id.vars = "Gebiedsnaam",
  measure.vars = c("kaal", "submers", "natans", "emers", "kragge", "gras_kruid", "hout"),
  variable.name = "bedekkingstype", value.name = "waarde_perc"
)
bedekking_2a_long[, bedekkingstype := factor(bedekkingstype,
  levels = c("kaal", "submers", "natans", "emers", "kragge", "gras_kruid", "hout"),
  labels = c("Kaal", "Submers", "Natans", "Emers", "Kragge", "Gras/kruiden", "Hout")
)]
bedekking_2a_long[, Gebiedsnaam := factor(Gebiedsnaam, levels = gebied_order)]

ggplot(bedekking_2a_long[!is.na(Gebiedsnaam),], aes(x = waarde_perc, y = Gebiedsnaam)) +
  geom_boxplot(outlier.size = 1) +
  facet_wrap(~bedekkingstype, nrow = 1, scales = "free_x") +
  labs(
    x = "Bedekking (%)", y = NULL,
    title = "Totale bedekking oeverzone 2a per gebied (extra, niet in erosieindex)",
    subtitle = "Verdeling van bedekkingstypen in zone 2a: kaal, submers, natans, emers, kragge, gras/kruiden, hout.\nGeen van deze componenten maakt onderdeel uit van de erosieindex-formule, alleen visueel."
  ) +
  theme(axis.text.y = element_text(size = 13), axis.text.x = element_text(size = 11),
        strip.text = element_text(size = 11))

## oevervormindex: componenten uitgesplitst --------------------------------
# Laat per gebied zien welke component (taludhoek, grilligheid) de
# oevervormindex omhoog of omlaag drukt.
# Sorteer gebieden op mediaan oevervormindex (eigen sortering, los van erosieindex)
gebied_order_oevervorm <- abio_proj[, .(med = median(oevervormindex, na.rm = TRUE)), by = Gebiedsnaam][order(med), Gebiedsnaam]
oevervorm_comp_long <- melt(
  abio_proj[, .(Gebiedsnaam,
                talud    = oevervorm_talud_norm,
                grillig  = oevervorm_grillig_norm,
                oevervormindex_comp = oevervormindex)],
  id.vars = "Gebiedsnaam",
  measure.vars = c("oevervormindex_comp", "talud", "grillig"),
  variable.name = "component", value.name = "waarde_norm"
)
oevervorm_comp_long[, component := factor(component,
  levels = c("oevervormindex_comp", "talud", "grillig"),
  labels = c("Oevervormindex", "Talud waterlijn (geïnverteerd)", "Grilligheid oeverlijn")
)]
oevervorm_comp_long[, Gebiedsnaam := factor(Gebiedsnaam, levels = gebied_order_oevervorm)]

ggplot(oevervorm_comp_long[!is.na(Gebiedsnaam),], aes(x = waarde_norm, y = Gebiedsnaam)) +
  geom_boxplot(outlier.size = 1) +
  facet_wrap(~component, nrow = 1) +
  labs(
    x = "Genormaliseerde waarde (0-1)", y = NULL,
    title = "Oevervormindex componenten per gebied",
    subtitle = paste0(
      "Oevervormindex = gemiddelde(talud waterlijn geïnverteerd, grilligheid oeverlijn)\n",
      "Hogere waarde = flauwer talud / grilliger oeverlijn"
    )
  ) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14),
        strip.text = element_text(size = 14))

## vernattingsrisico-index: componenten uitgesplitst ------------------------------
# Laat per gebied zien welke component (kale/productiegras-oever, steilheid,
# oppervlak emers 2a/2b) het vernattingsrisico omhoog of omlaag drukt.
# Kragge% zone 2b wordt getoond als individuele meetpunten (jitter, kleur =
# kraggevorming-flag) bovenop alle panelen. Draagkracht oever wordt als extra
# diagnostisch facet getoond; deze maakt GEEN onderdeel uit van de
# indexformule. Hoogteverschil waterlijn-oever (hgt_wl_oever) is uit dit plot
# verwijderd.
vernatting_comp_long <- melt(
  abio_proj[, .(Gebiedsnaam,
                opp_emers_2b         = vernatting_emers2b_norm,
                kaal_gras            = vernatting_kaal_gras_norm,
                steilte              = vernatting_steilte_norm,
                draagkracht_oever    = stabiliteit_draagkracht_oever_norm,
                vernattingsrisico_index_comp = vernattingsrisico_index,
                kraggevorming_flag)],
  id.vars = c("Gebiedsnaam", "kraggevorming_flag"),
  measure.vars = c("vernattingsrisico_index_comp", "opp_emers_2b", "kaal_gras",
                   "steilte", "draagkracht_oever"),
  variable.name = "component", value.name = "waarde_norm"
)
vernatting_comp_long[, component := factor(component,
  levels = c("vernattingsrisico_index_comp", "opp_emers_2b", "kaal_gras",
             "steilte", "draagkracht_oever"),
  labels = c("Vernattingsrisico-index",
             "Oppervlak emers 2b (geïnverteerd)\nhoog = meer risico (kleiner oppervlak)",
             "Kale/productiegras-oever %\nhoog = meer risico",
             "Steilheid oever (talud)\nhoog = meer risico",
             "Draagkracht oever (geïnverteerd)\nhoog = meer risico (lagere draagkracht)\n(GEEN onderdeel van index)")
)]
vernatting_comp_long[, kraggevorming_label := factor(
  fifelse(is.na(kraggevorming_flag), NA_character_,
          fifelse(kraggevorming_flag, "Wel kraggevorming (2b kragge% > 50)", "Geen kraggevorming")),
  levels = c("Wel kraggevorming (2b kragge% > 50)", "Geen kraggevorming")
)]
# Sorteer gebieden op mediaan vernattingsrisico_index (ipv mediaan erosieindex)
gebied_order_vernatting <- abio_proj[, .(med = median(vernattingsrisico_index, na.rm = TRUE)), by = Gebiedsnaam][order(med), Gebiedsnaam]
vernatting_comp_long[, Gebiedsnaam := factor(Gebiedsnaam, levels = gebied_order_vernatting)]

ggplot(vernatting_comp_long[!is.na(Gebiedsnaam),], aes(x = waarde_norm, y = Gebiedsnaam)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(
    data = vernatting_comp_long[!is.na(Gebiedsnaam) & !is.na(kraggevorming_label)],
    aes(color = kraggevorming_label),
    height = 0.2, width = 0, size = 4, alpha = 0.6
  ) +
  facet_wrap(~component, nrow = 1, scales = "free_x") +
  scale_color_manual(
    values = c("Wel kraggevorming (2b kragge% > 50)" = "#009E73", "Geen kraggevorming" = "#D55E00"),
    name = "Kraggevorming"
  ) +
  labs(
    x = "Genormaliseerde waarde (0-1)", y = NULL,
    title = "Vernattingsrisico-index componenten per gebied",
    subtitle = paste0(
      "Vernattingsrisico-index = gemiddelde(kale/productiegras-oever %, steilheid oever, oppervlak emers 2b [geïnverteerd])\n",
      "Hogere waarde = groter risico bij vernatten; draagkracht oever is een extra diagnostisch paneel, geen onderdeel van de formule;\n",
      "groene punten = wel kraggevorming, rode punten = geen kraggevorming"
    )
  ) +
  theme(
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 11),
    axis.title.x = element_text(size = 13),
    strip.text = element_text(size = 10, lineheight = 0.85),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13)
  )

## vernattingsrisico-index: totale bedekking zone 2b (extra, niet in index) ----
# Puur visueel/diagnostisch: verdeling van de bedekkingscomponenten in
# oeverzone 2b (kaal, emers, kragge, gras/kruiden, productiegras, hout). Geen
# van deze kolommen maakt onderdeel uit van de vernattingsrisico-index-formule.
bedekking_2b_long <- melt(
  abio_proj[, .(Gebiedsnaam,
                kaal        = oeverzone_2b_kaal_perc,
                emers       = oeverzone_2b_emers_perc,
                kragge      = oeverzone_2b_kraggen_perc,
                gras_kruid  = oeverzone_2b_ter_gras_kruid_perc,
                prodgras    = oeverzone_2b_ter_prodgras_perc,
                hout        = oeverzone_2b_hout_perc)],
  id.vars = "Gebiedsnaam",
  measure.vars = c("kaal", "emers", "kragge", "gras_kruid", "prodgras", "hout"),
  variable.name = "bedekkingstype", value.name = "waarde_perc"
)
bedekking_2b_long[, bedekkingstype := factor(bedekkingstype,
  levels = c("kaal", "emers", "kragge", "gras_kruid", "prodgras", "hout"),
  labels = c("Kaal", "Emers", "Kragge", "Gras/kruiden", "Productiegras", "Hout")
)]
bedekking_2b_long[, Gebiedsnaam := factor(Gebiedsnaam, levels = gebied_order_vernatting)]

ggplot(bedekking_2b_long[!is.na(Gebiedsnaam),], aes(x = waarde_perc, y = Gebiedsnaam)) +
  geom_boxplot(outlier.size = 1) +
  facet_wrap(~bedekkingstype, nrow = 1, scales = "free_x") +
  labs(
    x = "Bedekking (%)", y = NULL,
    title = "Totale bedekking oeverzone 2b per gebied (extra, niet in vernattingsrisico-index)",
    subtitle = "Verdeling van bedekkingstypen in zone 2b: kaal, emers, kragge, gras/kruiden, productiegras, hout.\nGeen van deze componenten maakt onderdeel uit van de vernattingsrisico-index-formule, alleen visueel."
  ) +
  theme(axis.text.y = element_text(size = 13), axis.text.x = element_text(size = 11),
        strip.text = element_text(size = 11))

## los componentenplot: kragge%, oppervlak emers, afscheur%, kale oever%, gras%, grilligheid ----
# Toont de losse ruwe (niet-genormaliseerde) componenten die samen de
# oever-erosie/stabiliteit-diagnostiek vormen: kragge% zone 2b, oppervlak
# emers 2a/2b (perc x breedte in m), afscheurpercentage, kale-oever%,
# productiegras% zone 2b, en grilligheid oeverlijn (ordinaal).
componenten_los_long <- melt(
  abio_proj[, .(Gebiedsnaam,
                
                onderholling       = holleoever,
                kragge_perc        = oeverzone_2a_kraggen_perc,
                opp_emers_2a       = oeverzone_2a_emers_m,
                opp_emers_2b       = oeverzone_2b_emers_m,
                afscheur_perc      = afscheur_veg_lengte_perc,
                kale_oever_perc    = oeverzone_2b_kaal_perc,
                gras_perc_2b       = oeverzone_2b_ter_prodgras_perc,
                grilligheid        = as.numeric(factor(oeverzone_2b_grillig, levels = c("uniform","matig","zeer"))) - 1)],
  id.vars = "Gebiedsnaam",
  measure.vars = c("kragge_perc","onderholling", "opp_emers_2a", "opp_emers_2b", "afscheur_perc",
                   "kale_oever_perc", "gras_perc_2b", "grilligheid"),
  variable.name = "component", value.name = "waarde"
)
componenten_los_long[, component := factor(component,
  levels = c("onderholling","kragge_perc", "opp_emers_2a", "opp_emers_2b", "afscheur_perc",
             "kale_oever_perc", "gras_perc_2b", "grilligheid"),
  labels = c("Onderholling (m)", "Kragge % (zone 2a)", "Oppervlak emers 2a (m)", "Oppervlak emers 2b (m)",
             "Afscheur (% lengte)", "Kale oever % (zone 2b)", "Productiegras % (zone 2b)",
             "Grilligheid oeverlijn\n(0=uniform, 1=matig, 2=zeer)")
)]
# Sorteer gebieden op mediaan onderholling (ipv mediaan erosieindex)
gebied_order_onderholling <- abio_proj[, .(med = median(holleoever, na.rm = TRUE)), by = Gebiedsnaam][order(med), Gebiedsnaam]
componenten_los_long[, Gebiedsnaam := factor(Gebiedsnaam, levels = gebied_order_onderholling)]

ggplot(componenten_los_long[!is.na(Gebiedsnaam),], aes(x = waarde, y = Gebiedsnaam)) +
  geom_boxplot(outlier.size = 1) +
  facet_wrap(~component, nrow = 1, scales = "free_x") +
  labs(
    x = "Waarde (ruwe eenheid)", y = NULL,
    title = "Losse componenten per gebied: kragge, onderholling, oppervlak emers, afscheur, kale oever, gras, grilligheid"
  ) +
  theme(axis.text.y = element_text(size = 10))




## reformat data for plot loop------------------------------------------------------------------
cols_num <- colnames(abio_proj)[sapply(abio_proj, is.numeric)]
dup_cols <- names(abio_proj)[duplicated(names(abio_proj))]
if (length(dup_cols) > 0) abio_proj[, (dup_cols) := NULL]
melt <- melt(setDT(abio_proj), id.vars = c("SlootID","Sloot_nr","WP","instanceID_abio","instanceID_veg","Gebiedsnaam","MeenemenDataAnalyse_totaal","gebied","sloot","Behandeling","beheer","jaar"), 
             measure.vars = cols_num, na.rm = TRUE)
# pars <- as.data.table(unique(melt[, variable]))
pars <- fread(paste0(workspace,"./hulp_tabellen/parametersVeest_namen.csv"), dec = '.', na.strings = c('NA',''), encoding = "Latin-1")
melt[,variable :=tolower(variable)]
pars[,variable_lower :=tolower(variable)]
melt <- merge(melt, pars, by.x = 'variable', by.y = 'variable_lower', all.x = TRUE)
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


# validate db-------------------------------------------------------------------
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



