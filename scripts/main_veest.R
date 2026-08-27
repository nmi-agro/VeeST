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
  over = nrow(locs_prof_unique),
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

## indices berekenen -------------------------------

# Kraggevorming vlag: groeiende oever als kraggen*breedte > 10 OF sluiting zone 2a én 2b > 90%
abio_proj[, kraggevorming_flag := (
  (fifelse(is.na(oeverzone_2b_kraggen_perc), 0, oeverzone_2b_kraggen_perc) * fifelse(is.na(oeverzone_2b_breedte_cm), 0, oeverzone_2b_breedte_cm) +
   fifelse(is.na(oeverzone_2a_kraggen_perc), 0, oeverzone_2a_kraggen_perc) * fifelse(is.na(oeverzone_2a_breedte_cm), 0, oeverzone_2a_breedte_cm)) > 10 |
  (fifelse(is.na(oeverzone_2a_emers_perc), 0, oeverzone_2a_emers_perc) > 90 &
   fifelse(is.na(oeverzone_2b_emers_perc), 0, oeverzone_2b_emers_perc) > 90)
)]

# Erosieindex: hoog = meer erosie
# Componenten: afscheur (hoog = meer erosie), onderholling (hoog = meer erosie),
#   kraggevorming (groeiende oever = minder erosie, verlaagt index),
#   kale oever (hoog = meer erosie)
# Alle componenten genormaliseerd naar 0-1 schaal (percentages gedeeld door 100)
abio_proj[, erosieindex := {
  afscheur_norm   <- fifelse(is.na(afscheur_veg_lengte_perc), NA_real_, afscheur_veg_lengte_perc / 100)
  # Onderholling: cm, normaliseer op max 150 cm (gecorrigeerde waarde) en cap op 1
  onderholling_norm <- fifelse(is.na(holleoever), NA_real_, pmin(holleoever / 150, 1))
  # Kraggevorming verlaagt index: 1 als geen kraggevorming, 0 als kraggevorming
  kragg_factor    <- fifelse(is.na(kraggevorming_flag), 1, fifelse(kraggevorming_flag, 0, 1))
  kaal_norm       <- fifelse(is.na(oeverzone_2b_kaal_perc), NA_real_, oeverzone_2b_kaal_perc / 100)
  rowMeans(cbind(afscheur_norm, onderholling_norm, kaal_norm), na.rm = TRUE) * kragg_factor
}]

# Oevervormindex: geometrie oever
# Flauwer talud (lage taludhoek rond waterlijn) = hogere index
# Grilliger oeverlijn (oeverzone_2b_grillig) = hogere index
# tldk_oevrwtr_perc: hoek rand waterlijn als % (lager = flauwer = beter)
abio_proj[, oevervormindex := {
  # Inverteer taludhoek: flauwe helling = hoge score
  talud_boven_norm <- fifelse(is.na(tldk_oevrwtr_perc), NA_real_, 1 - pmin(tldk_oevrwtr_perc / 100, 1))
  # Grilligheid: ordinale score (1-3 verwacht); normaliseer naar 0-1
  grillig_norm     <- fifelse(is.na(oeverzone_2b_grillig), NA_real_,
                              pmin((as.numeric(oeverzone_2b_grillig) - 1) / 2, 1))
  rowMeans(cbind(talud_boven_norm, grillig_norm), na.rm = TRUE)
}]

# Stabiliteitsindex: hoog = stabielere oever
# Componenten: sluiting emers zone 2a (hoog = stabiel), sluiting emers zone 2b,
#   draagkracht oever (hoog = stabiel), breedte oeverzone (breder = stabieler),
#   oevervormindex (flauwer/grilliger = stabieler)
# Normalisaties op max van dataset worden vooraf berekend (buiten := blok)
.dk_max      <- max(abio_proj$draagkracht_oever, na.rm = TRUE)
.breedte_max <- max(
  fifelse(is.na(abio_proj$oevbte), 0, abio_proj$oevbte * 100) +
  fifelse(is.na(abio_proj$oeverzone_2b_breedte_cm), 0, abio_proj$oeverzone_2b_breedte_cm),
  na.rm = TRUE
)
abio_proj[, stabiliteitsindex := {
  emers_2a_norm <- fifelse(
    is.na(oeverzone_2a_emers_perc) | is.na(oeverzone_2a_breedte_cm), NA_real_,
    fifelse(oeverzone_2a_emers_perc > 90 & (oeverzone_2a_breedte_cm / 100) > 0.5, 1,
            oeverzone_2a_emers_perc / 100)
  )
  emers_2b_norm <- fifelse(
    is.na(oeverzone_2b_emers_perc) | is.na(oeverzone_2b_breedte_cm), NA_real_,
    fifelse(oeverzone_2b_emers_perc > 90 & (oeverzone_2b_breedte_cm / 100) > 0.5, 1,
            oeverzone_2b_emers_perc / 100)
  )
  dk_norm      <- fifelse(is.na(draagkracht_oever), NA_real_, draagkracht_oever / .dk_max)
  breedte_norm <- (fifelse(is.na(oevbte), 0, oevbte * 100) +
                   fifelse(is.na(oeverzone_2b_breedte_cm), 0, oeverzone_2b_breedte_cm)) / .breedte_max
  rowMeans(cbind(emers_2a_norm, emers_2b_norm, dk_norm, breedte_norm, oevervormindex), na.rm = TRUE)
}]
rm(.dk_max, .breedte_max)

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
# Gemiddelde draagkracht oever berekenen obv oever penetrometer metingen

abio_proj[, draagkracht_oever := rowMeans(.SD, na.rm = TRUE), 
          .SDcols = c("oever_(10,20]", "oever_(20,30]", "oever_(30,40]", "oever_(40,50]")]
# Gemiddelde draagkracht perceel berekenen obv perceel penetrometer metingen
abio_proj[, draagkracht_perceel := rowMeans(.SD, na.rm = TRUE), 
          .SDcols = c("perceel_(10,20]", "perceel_(20,30]", "perceel_(30,40]", "perceel_(40,50]")]
abio_proj[, draagkracht_insteek := rowMeans(.SD, na.rm = TRUE), 
          .SDcols = c("insteek_(10,20]", "insteek_(20,30]", "insteek_(30,40]", "insteek_(40,50]")]

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



