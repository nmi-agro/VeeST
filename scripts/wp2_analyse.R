# =============================================================================
# WP2 analyse: verschil tussen behandelingen (beheer) langs eenzelfde sloot
# =============================================================================
# Doel:
#  1. Data filteren op WP2
#  2. Checken of alle locaties data hebben voor zowel 2024 als 2025
#  3. Visualiseren van targets en belangrijkste predictoren per Behandeling,
#     over de jaren (mediaan per Behandeling + spreiding van metingen)
#  4. Toetsen of verschillen tussen Behandelingen significant zijn:
#     - gemengd model (SlootID als blok, want behandelingen liggen langs
#       dezelfde sloot -> geblokte opzet) i.p.v. gewone ANOVA
#     - ANCOVA: Behandeling x covariaat, om te checken of het effect van
#       Behandeling afhangt van het niveau van een verklarende variabele
#       (bv. alleen zichtbaar bij lage/hoge P-AL, of alleen in een bepaald
#       bereik van een belangrijke predictor)
#  5. Variatie tussen behandelingen op drie niveaus (alle sloten, gebied,
#     sloot), met en zonder sloten waar geen effect is waargenomen (sectie 9-12)
#  6. Gerichte contrasten (sectie 13-18): full factorial in Hegewarren,
#     minimaal/regulier en wel/geen afrastering per gebied, afkreeften in
#     Ronde Hoep en Oukoop, maaimaand in Oukoop, NVO en oevernutriënten in
#     Spaarnwoude, peilverhoging binnen een gebied, maai-/baggermaand als
#     beheerparameter en oeverbreedte (zone 2a/2b)
#
# LET OP - multiple testing: dit script doorloopt meerdere targets en
# meerdere covariaten voor de ANCOVA-interacties. Dat is een groot aantal
# toetsen; p-waarden van de ANCOVA-interacties zijn ter exploratie en worden
# met p.adjust (BH/FDR) gecorrigeerd. Behandel losse significante resultaten
# vooral als hypothesevormend, niet als bevestigend bewijs.
# =============================================================================

library(tidyverse)
library(data.table)
library(lme4)
library(lmerTest)   # voor p-waarden bij lmer via Satterthwaite
library(broom)
library(broom.mixed)
library(car)        # Anova() type III, leveneTest
library(colorspace) # lighten()/darken() voor sloot-kleurgradiënt per gebied

# 0. Data + hulpbestanden inladen ----------------------------------------------
# Verwacht: abio_proj is al beschikbaar in de sessie (na main_veest.R /
# Analyses_dev_db.R), of wordt hier ingelezen vanaf de rapport-RDS.

if (!exists("workspace")) {
  workspace <- paste0(Sys.getenv("NMI-SITE"), "/O 1900 - O 2000/1922.N.23 VeeST vwsloot vd toekomst/05. Data/")
}
rds_dir <- paste0(workspace, "output/rapport/")

# Map om alle wp2-analyse figuren weg te schrijven. workspace2 wordt elders
# (data_import_ppr.R) ingesteld op de VIPNL-schijf; als dat script niet is
# gedraaid in deze sessie vallen we terug op een map naast de huidige workspace.
if (!exists("workspace2")) {
  workspace2 <- paste0("c:/Users/LauraMoria/Stichting Veenweiden Innovatiecentrum/VIPNL Themas - VIPNL Veenweidensloot/D. Data en analyse/")
}
figuren_dir <- paste0(workspace2, "figuren_wp2_analyse_LM/")
if (!dir.exists(figuren_dir)) dir.create(figuren_dir, recursive = TRUE)

# Hulpfunctie om een ggplot weg te schrijven naar figuren_dir met consistente
# instellingen (breedte schaalt mee met het aantal facet-panelen).
#' Sla een figuur op in de submap van figuren_dir die figuur_indeling aangeeft
#'
#' @param plot ggplot- of patchwork-object.
#' @param naam Bestandsnaam zonder extensie; bepaalt via figuur_indeling de
#'   submap ("rapport", "bijlage" of "overig").
#' @param width,height Afmetingen in inch.
#' @param dpi Resolutie.
sla_figuur_op <- function(plot, naam, width = 10, height = 7, dpi = 300) {
  map <- coalesce(figuur_indeling$map[str_detect(naam, figuur_indeling$patroon)][1], "overig")
  dir.create(paste0(figuren_dir, map), showWarnings = FALSE)
  # oude versie in een andere map weghalen (bv. na een gewijzigde indeling),
  # anders blijft die in het rapport of de bijlage staan
  oud <- paste0(figuren_dir, setdiff(c("rapport", "bijlage", "overig", "profielen_sloot"), map), "/", naam, ".png")
  unlink(oud[file.exists(oud)])
  ggsave(
    filename = paste0(figuren_dir, map, "/", naam, ".png"),
    plot = plot, width = width, height = height, dpi = dpi
  )
}

# Indeling van de figuren voor het WP2-rapport: "rapport" = hoofdtekst,
# "bijlage" = bijlagen, alles wat hier niet staat komt in "overig". Het eerste
# patroon dat past wint, dus specifieke patronen staan boven
# algemene. Tabellen (csv) blijven in figuren_dir zelf.
figuur_indeling <- tribble(
  ~patroon,                                            ~map,
  # hoofdtekst
  "^niveaus_variantiedecompositie$",                   "rapport",
  "^niveau_alles_",                                    "rapport",
  "^significante_contrasten_jaar$",                    "rapport",
  "^contrast_hegewarren_factorial$",                   "rapport",
  "^afkreeften_",                                      "rapport",
  "^spaarnwoude_nvo_oevernutrienten$",                 "rapport",
  "^profiel_draagkracht_diepte$",                      "rapport",
  "^profiel_draagkracht_diepte_per_gebied$",           "rapport",
  "^verschil_draagkracht_vs_verschil_vegetatiehoogte$", "rapport",
  "^oeverbreedte_zone2a_2b$",                          "rapport",
  "^respons_abiotiek_overzicht$",                      "overig",
  "^respons_structuur_2a_abiotiek$",                   "rapport",
  "^sloot_covariaten_variantie$",                      "rapport",
  "^redox_temperatuur_binnen_locatie$",                "rapport",
  "^uitgangssituatie_waargenomen_effect$",             "rapport",
  "^gebied_diepte_doorzicht$",                         "rapport",
  "^effect_beginwaarde_",                              "rapport",
  # per sloot (wp2_profielen_sloot.R), in bijlage D van het rapport
  "^sloot_(profiel|diepte)_",                          "profielen_sloot",
  # bijlagen
  "^thema_verandering_",                               "overig",
  "^thema_jaar_",                                      "bijlage",
  # niet in het rapport (dubbel met de hoofdtekst of te gedetailleerd)
  "^niveau_gebied_",                                   "overig",
  "^niveau_sloot_",                                    "overig",
  "^contrasten_per_gebied_heatmap$",                   "bijlage",
  "^peilverhoging_",                                   "bijlage",
  "^beheer_",                                          "overig",
  "^respons_abiotiek_",                                "overig",
  "^draagkracht_oever_vs_vegetatiehoogte$",            "overig"
)

if (!exists("abio_proj")) {
  abio_proj <- readRDS(paste0(rds_dir, "abio_proj.rds")) |> as.data.table()
}
abio_proj <- as.data.table(abio_proj)

# Predictorenset uit de XGBoost-analyse (variable importance per target)
xgb_importance <- readRDS(paste0(rds_dir, "all_xgb_importance.rds")) |> as.data.table()

# 1. Filter op WP2 -------------------------------------------------------------
wp2 <- abio_proj[WP == "WP2"]
# Geometriekolommen (sfc) zijn hier niet nodig en breken dplyr/tibble-
# bewerkingen als sf niet geladen is.
wp2 <- wp2[, !vapply(wp2, inherits, logical(1), what = "sfc"), with = FALSE]

# Nullen die ontbrekende metingen zijn (geen echte 0) -> NA. Anders ontstaan
# schijnbaar grote veranderingen t.o.v. het eerste meetjaar.
#  - doorzicht 0 m is fysiek onmogelijk (2024: SW_3 alle vakken en ZG_2_M_O);
#    zichtdiepte (doorzicht/waterdiepte) is daarvan afgeleid
#  - onderholling 0 cm in ZG in 2024 (alle gemeten vakken; in 2025/2026 ~20-27 cm)
n_nul <- c(
  doorzicht    = wp2[doorzicht2_mid_m == 0, .N],
  zichtdiepte  = wp2[zichtdiepte == 0, .N],
  onderholling = wp2[gebied == "ZG" & jaar == 2024 & holleoever == 0, .N]
)
wp2[doorzicht2_mid_m == 0, doorzicht2_mid_m := NA]
wp2[zichtdiepte == 0, zichtdiepte := NA]
wp2[gebied == "ZG" & jaar == 2024 & holleoever == 0, holleoever := NA]
cat("Nullen als ontbrekend behandeld:", paste(names(n_nul), n_nul, sep = " = ", collapse = ", "), "\n")

# Slibtemperatuur staat in twee kolommen (slib_temp_C en slib_temp_c, per jaar
# verschillend gevuld): samenvoegen. Waarden boven 40 °C zijn invoerfouten
# (9994 in SW_5 in 2026, 677 in ZG_2_M_O in 2025) -> NA.
wp2[, slib_temp := fcoalesce(
  suppressWarnings(as.numeric(slib_temp_C)), suppressWarnings(as.numeric(slib_temp_c))
)]
cat("Slibtemperatuur > 40 °C als ontbrekend behandeld:", wp2[slib_temp > 40, .N], "\n")
wp2[slib_temp > 40, slib_temp := NA]

# Redox: op 6 juni 2025 was de redoxmeter kapot (KW_1, KW_2_M en KW_2_R). Voor
# KW_2_M_W en KW_2_R_W staan toch waarden in de data, gelijk aan die van
# KW_2_M-AF_W op 3 juni (slibredox -16,2 mV, waterredox 232 mV). Ook die
# waarde van KW_2_M-AF_W is onjuist (andere jaren -239 tot -300 mV). Alle drie
# -> NA. KW_1 is in 2025 al leeg.
redox_kolommen <- intersect(c("slib_redox_mgL", "slib_redox_pH7", "water_redox", "water_redox_pH7"), names(wp2))
redox_kapot <- wp2[, SlootID %in% c("KW_2_M-AF_W", "KW_2_M_W", "KW_2_R_W") & jaar == 2025]
cat("Redox als ontbrekend behandeld (KW_2, 2025):", sum(redox_kapot), "locaties\n")
wp2[redox_kapot, (redox_kolommen) := NA]

# Oppervlak vegetatie in de oeverzone (perc x zonebreedte, in m2 per m
# slootlengte), net als oeverzone_2a/2b_emers_m2 in main_veest.R. Terrestrische
# grassen en kruiden in zone 2a en 2b samen; productiegras alleen in zone 2b.
# Bij een zonebreedte van 0 is het oppervlak 0, ook als het percentage leeg is.
opp_zone <- function(perc, breedte_cm) fifelse(breedte_cm %in% 0, 0, (perc / 100) * (breedte_cm / 100))
wp2[, `:=`(
  oeverzone_2_gras_kruid_m2 = opp_zone(oeverzone_2a_ter_gras_kruid_perc, oeverzone_2a_breedte_cm) +
    opp_zone(oeverzone_2b_ter_gras_kruid_perc, oeverzone_2b_breedte_cm),
  oeverzone_2b_prodgras_m2 = opp_zone(oeverzone_2b_ter_prodgras_perc, oeverzone_2b_breedte_cm)
)]

# Selectie van meetlocaties: kolom MeenemenDataAnalyse_WP2 in
# wp_locaties_naam_correcties.xlsx (per SlootID en jaar). Alleen "ja" doet mee
# in de behandelingsanalyses (zie sectie 2e). Eerst kopiëren, zodat het
# bestand ook in te lezen is als het in Excel openstaat.

#' Kopieer een bestand, ook als het in Excel openstaat
#'
#' file.copy() faalt als Excel het bestand open heeft; PowerShell Copy-Item
#' kan het dan wel lezen.
#'
#' @param van Pad naar het bronbestand.
#' @param naar Pad naar de kopie.
#' @return `naar` (onzichtbaar); stopt als kopiëren niet lukt.
kopieer_gedeeld <- function(van, naar) {
  if (!isTRUE(suppressWarnings(file.copy(van, naar, overwrite = TRUE)))) {
    ps <- sprintf(
      "Copy-Item -LiteralPath '%s' -Destination '%s' -Force",
      normalizePath(van, winslash = "\\"), normalizePath(naar, winslash = "\\", mustWork = FALSE)
    )
    system2("powershell", c("-NoProfile", "-Command", shQuote(ps)))
  }
  if (!file.exists(naar) || file.size(naar) == 0) stop("Kan ", van, " niet kopiëren")
  invisible(naar)
}
locaties_kopie <- tempfile(fileext = ".xlsx")
kopieer_gedeeld(paste0(workspace2, "analysePlan/wp_locaties_naam_correcties.xlsx"), locaties_kopie)
meenemen_wp2 <- as.data.table(readxl::read_excel(locaties_kopie, sheet = "locaties"))[
  WP == "WP2", .(SlootID, jaar = as.integer(jaar), MeenemenDataAnalyse_WP2)
] |> unique()
stopifnot(!anyDuplicated(meenemen_wp2[, .(SlootID, jaar)]))
wp2 <- merge(wp2, meenemen_wp2, by = c("SlootID", "jaar"), all.x = TRUE)
cat("WP2-rijen zonder MeenemenDataAnalyse_WP2 in de locatietabel:", wp2[is.na(MeenemenDataAnalyse_WP2), .N], "\n")

cat("Aantal rijen WP2:", nrow(wp2), "\n")
cat("Aantal unieke sloten (SlootID) WP2:", uniqueN(wp2$SlootID), "\n")

# Oukoop (afkreeften en maaimaand juli/augustus) is een apart WP. Het zit wel
# in de database-export van main_veest.R, maar Analyses_dev_db.R filtert op
# WP1/WP2, waardoor Oukoop niet in abio_proj.rds staat. Daarom lezen we
# Oukoop uit de meest recente db_veest-export (tijdstempel in de bestandsnaam).
db_bestanden <- list.files(
  paste0(workspace, "output/Database/"),
  pattern = "^db_veest\\d{12}\\.csv$", full.names = TRUE
)
oukoop <- NULL
if (length(db_bestanden) > 0) {
  db_laatste <- sort(db_bestanden)[length(db_bestanden)]
  # De regulier beheerde Oukoop-vakken zijn in 2024 als WP1 geregistreerd (bv.
  # GM_5_R_W, DR_1_R_N). Daarom alle rijen van de Oukoop-SlootID's meenemen,
  # ook die met WP1; bij dubbele SlootID x jaar gaat de Oukoop-rij voor.
  db_oukoop <- fread(db_laatste, sep = ";", dec = ".", encoding = "UTF-8")
  oukoop <- db_oukoop[SlootID %in% db_oukoop[WP == "Oukoop", unique(SlootID)] & WP %in% c("Oukoop", "WP1")]
  oukoop <- oukoop[order(SlootID, jaar, WP != "Oukoop")][, .SD[1], by = .(SlootID, jaar)]
  rm(db_oukoop)
  oukoop[zichtdiepte > 1, zichtdiepte := 1]
  cat("Oukoop ingelezen uit", basename(db_laatste), ":", nrow(oukoop), "rijen,",
      uniqueN(oukoop$SlootID), "locaties\n")
} else {
  message("Geen db_veest-export gevonden: Oukoop-analyses (sectie 15) worden overgeslagen.")
}

# 2. Check dekking 2024 / 2025 per locatie -------------------------------------
dekking <- wp2 |>
  distinct(SlootID, jaar) |>
  mutate(aanwezig = TRUE) |>
  pivot_wider(names_from = jaar, values_from = aanwezig, values_fill = FALSE)

print(dekking)

ontbrekend_2024 <- dekking |> filter(`2024` == FALSE) |> pull(SlootID)
ontbrekend_2025 <- dekking |> filter(`2025` == FALSE) |> pull(SlootID)

if (length(ontbrekend_2024) > 0) {
  message("Sloten ZONDER data in 2024: ", paste(ontbrekend_2024, collapse = ", "))
}
if (length(ontbrekend_2025) > 0) {
  message("Sloten ZONDER data in 2025: ", paste(ontbrekend_2025, collapse = ", "))
}
if (length(ontbrekend_2024) == 0 && length(ontbrekend_2025) == 0) {
  message("Alle WP2-sloten hebben data in zowel 2024 als 2025.")
}

# Check ook: hoeveel Behandelingen per SlootID (verwacht >1, want vergelijking
# langs dezelfde sloot vereist minstens 2 behandelingen per sloot/jaar)
behandelingen_per_sloot <- wp2 |>
  distinct(SlootID, jaar, Behandeling) |>
  count(SlootID, jaar, name = "n_behandelingen")

print(behandelingen_per_sloot |> arrange(n_behandelingen))

# 2b. Behandeling groeperen voor kleur in de figuren ---------------------------
# 9 losse Behandeling-niveaus zijn met okabe-kleuren + dodge niet goed te
# onderscheiden. We groeperen daarom naar 4 kleurgroepen (Okabe-Ito):
#  - "AF"    : bevat afrastering (AF), ongeacht overige toevoegingen
#  - "NVO"   : bevat NVO, geen AF
#  - "M"     : overige (regulier-)minimaal beheer, geen AF/NVO
#  - "R"     : overig regulier beheer, geen AF/NVO
# De losse Behandeling-niveaus blijven zichtbaar als aparte lijnen/dodge-
# posities binnen een kleurgroep.
okabe_ito <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442",
  "#0072B2", "#D55E00", "#CC79A7", "#999999"
)
kleurgroep_kleuren <- c(
  "AF"  = okabe_ito[6],  # D55E00 (oranjerood)
  "NVO" = okabe_ito[5],  # 0072B2 (blauw)
  "M"   = okabe_ito[3],  # 009E73 (groen)
  "R"   = okabe_ito[1]   # E69F00 (oranje/geel)
)

wp2 <- wp2 |>
  mutate(
    behandeling_groep = case_when(
      str_detect(Behandeling, "AF")  ~ "AF",
      str_detect(Behandeling, "NVO") ~ "NVO",
      str_detect(Behandeling, "^M")  ~ "M",
      str_detect(Behandeling, "^R")  ~ "R",
      TRUE ~ "overig"
    ),
    behandeling_groep = factor(behandeling_groep, levels = c("M", "R", "AF", "NVO", "overig"))
  )

print(wp2 |> count(behandeling_groep, Behandeling))

# 2c. Behandeling opsplitsen in x-as-categorie (basisbeheer) en vorm (AF/NVO) ----
# x-as: minimaal / regulier / afrastering. Sloten met AF (met of zonder KR of
# NVO erbij) vallen op de x-as onder "afrastering", ongeacht het M/R-prefix.
# vorm (shape): alleen KR (kreeftwering) en NVO krijgen een eigen symbool.
# Afrastering (AF) zelf krijgt geen apart symbool, want dat onderscheid zit al
# op de x-as.
#  - "NVO"         : driehoekje (wint van KR bij combinaties, al komt M-AF-KR-NVO niet voor)
#  - "kreeftwering": open bolletje (KR, zonder NVO)
#  - "regulier/minimaal": gevuld bolletje (geen KR, geen NVO; dus ook AF zonder KR/NVO)
wp2 <- wp2 |>
  mutate(
    behandeling_x = case_when(
      str_detect(Behandeling, "AF") ~ "afrastering",
      str_detect(Behandeling, "^M") ~ "minimaal",
      str_detect(Behandeling, "^R") ~ "regulier",
      TRUE ~ NA_character_
    ),
    behandeling_x = factor(behandeling_x, levels = c("regulier", "minimaal", "afrastering")),
    behandeling_vorm = case_when(
      str_detect(Behandeling, "NVO") ~ "NVO",
      str_detect(Behandeling, "KR")  ~ "kreeftwering",
      TRUE ~ "regulier/minimaal"
    ),
    behandeling_vorm = factor(behandeling_vorm, levels = c("regulier/minimaal", "kreeftwering", "NVO"))
  )

print(wp2 |> count(behandeling_x, behandeling_vorm, Behandeling))

vorm_shapes <- c("regulier/minimaal" = 16, "kreeftwering" = 1, "NVO" = 17)

# 2d. Sloot_nummer (gebied + Sloot_nr) als kleur voor individuele punten -------
# Elke sloot (combinatie van gebiedID en sloot_nr, onafhankelijk van
# behandeling/zijde) krijgt een eigen kleur, zodat je in de spreiding kunt
# zien welke punten bij dezelfde sloot horen. In plaats van één doorlopend
# Okabe-Ito-gebaseerd palet (dat voor 25 sloten op sommige plekken op elkaar
# ging lijken) krijgt elk gebied een eigen kleurfamilie/hoofdkleur, met
# sloten binnen dat gebied als verschillende tinten (licht -> donker) van
# die kleur. Dat maakt het meteen duidelijk tot welk gebied een punt hoort,
# en blijven sloten binnen een gebied toch onderscheidbaar.
#  - HW: bruin, KW: groen, RH: rood, SW: blauw, WL: paars, ZG: geel
gebied_hoofdkleur <- c(
  HW = "#7B4B1A",  # bruin
  KW = "#1B7B3A",  # groen
  RH = "#C0392B",  # rood
  SW = "#1F4E9C",  # blauw
  WL = "#7A3B9C",  # paars
  ZG = "#C9A400"   # geel
)

wp2 <- wp2 |>
  mutate(sloot_nummer = paste(gebied, Sloot_nr, sep = "_"))

sloot_niveaus <- sort(unique(wp2$sloot_nummer))

# Per gebied een gradiënt (licht -> donker) van de hoofdkleur, één tint per
# sloot_nr binnen dat gebied.
sloot_kleuren <- wp2 |>
  distinct(gebied, sloot_nummer) |>
  arrange(gebied, sloot_nummer) |>
  summarise(
    .by = gebied,
    kleur = list(
      colorRampPalette(c(
        colorspace::lighten(gebied_hoofdkleur[gebied[1]], 0.5),
        colorspace::darken(gebied_hoofdkleur[gebied[1]], 0.3)
      ))(n())
    ),
    sloot_nummer = list(sort(unique(sloot_nummer)))
  ) |>
  { \(d) setNames(unlist(d$kleur), unlist(d$sloot_nummer)) }()
sloot_kleuren <- sloot_kleuren[sloot_niveaus]

# 2e. Selectie van meetlocaties ---------------------------------------------------
# Alleen vakken met MeenemenDataAnalyse_WP2 == "ja" (locatietabel, per SlootID
# en jaar) doen mee in de behandelingsanalyses. Daarmee vallen o.a. de sloten
# zonder behandelingscontrast weg (SW_3 en SW_5: volgens de kolom Behandeling
# overal R of R-NVO; RH_1: in 2026 draad overal weg en niets gemaaid), en ook
# alle R-AF- en NVO-vakken. wp2_compleet bewaart alle vakken; die wordt
# gebruikt voor de proefopzet, Hegewarren (basisbeheer x afrastering, sectie
# 13a) en de NVO in Spaarnwoude (sectie 15). In Spaarnwoude telt SW_5 daar wel
# mee (contrast NVO / geen NVO binnen de sloot), SW_3 niet (geen NVO).
sloten_zonder_contrast <- c("SW_3", "SW_5", "RH_1")
sloten_nvo_contrast    <- c("SW_5")
wp2_compleet <- wp2

# Proefopzet per gebied (alle sloten, ook die zonder behandelingscontrast):
# aantal sloten en locaties, locaties per meetjaar en uitgevoerde behandelingen.
proefopzet_gebied <- wp2_compleet |>
  as_tibble() |>
  summarise(
    .by = gebied,
    n_sloten = n_distinct(sloot_nummer),
    n_locaties = n_distinct(SlootID),
    n_locaties_selectie = n_distinct(SlootID[MeenemenDataAnalyse_WP2 %in% "ja"]),
    behandelingen = paste(sort(unique(Behandeling)), collapse = ", ")
  ) |>
  left_join(
    wp2_compleet |>
      as_tibble() |>
      distinct(gebied, jaar, SlootID) |>
      count(gebied, jaar) |>
      arrange(jaar) |>
      pivot_wider(names_from = jaar, values_from = n, names_prefix = "n_", values_fill = 0),
    by = "gebied"
  ) |>
  arrange(gebied)
print(proefopzet_gebied)

wp2 <- wp2 |> filter(MeenemenDataAnalyse_WP2 %in% "ja")
vakken_selectie <- wp2[, paste(SlootID, jaar)]
cat("Vakken buiten de selectie (MeenemenDataAnalyse_WP2 != 'ja'):", nrow(wp2_compleet) - nrow(wp2), "rijen\n")
# Afrastering komt in de selectie alleen voor bij minimaal beheer (M-AF), zodat
# behandeling_basis R, M en M-AF heeft; R-KR telt bij R en M-AF-KR bij M-AF.
stopifnot(!any(str_detect(wp2$Behandeling, "^R.*AF|NVO")))

# 3. Targets definiëren + predictoren selecteren via XGBoost variable importance ----
targets <- c(
  draagkracht_oever   = "draagkracht_oever",
  slibdikte           = "max_slib",
  ekr_helofyten       = "Soortensamenstelling Helofyten",
  ekr_hydrofyten      = "Soortensamenstelling Hydrofyten",
  oeverindex          = "oeverindex",
  opp_emers           = "oeverzone_2a_emers_m2",
  erosie_index        = "erosieindex",
  vernattingsindex    = "vernattingsrisico_index",
  pal                 = "P-AL mg p2o5/100g_SB",
  redox_slib          = "slib_redox_pH7",
  taludhoek_oever     = "tldk_oevrwtr_perc"
)

check_cols <- function(named_vec, data) {
  named_vec <- named_vec[!is.na(named_vec)]
  ontbreekt <- named_vec[!named_vec %in% names(data)]
  if (length(ontbreekt) > 0) {
    warning("Ontbrekende kolommen: ", paste(ontbreekt, collapse = ", "))
  }
  named_vec[named_vec %in% names(data)]
}
targets <- check_cols(targets, wp2)

# Top-N predictoren per target op basis van Gain-importance uit de
# XGBoost-analyse (Analyses_dev_db.R), aangevuld met doorzicht /
# doorzicht-waterdiepte-ratio / slibdikte die we altijd willen zien.
n_top_predictoren <- 5

# Predictoren die we expliciet uitsluiten als ANCOVA/plot-covariaat, ongeacht
# hun XGBoost Gain-importance. water_pH staat hier niet om een datakwaliteits-
# reden (die ligt apart, zie de 5 verdachte ZG-2024-records met water_pH==0),
# maar omdat water_pH inhoudelijk niet relevant wordt geacht als verklarende
# variabele voor deze behandelingsvergelijking.
predictoren_uitgesloten <- c("water_pH")

vaste_predictoren <- c(
  doorzicht           = "doorzicht2_mid_m",
  doorzicht_wtd_ratio = "zichtdiepte",
  slibdikte           = "max_slib"
)
vaste_predictoren <- check_cols(vaste_predictoren, wp2)

top_predictoren_per_target <- xgb_importance[
  imp_type == "Gain" & target_var %in% targets & !(Feature %in% predictoren_uitgesloten),
][order(target_var, -Importance)][
  , .SD[seq_len(min(n_top_predictoren, .N))], by = target_var
][, .(target_var, Feature, Nederlandse_naam, Importance)]

print(top_predictoren_per_target)

# Unie van alle geselecteerde predictoren (voor de predictor-overzichtsplot)
predictoren_selectie <- unique(top_predictoren_per_target$Feature)
predictoren_selectie <- check_cols(setNames(predictoren_selectie, predictoren_selectie), wp2)
predictoren <- c(vaste_predictoren, predictoren_selectie[!predictoren_selectie %in% vaste_predictoren])

# 4. Visualisatie per thema: verandering tussen eerste en laatste meetjaar -----
# x-as: basisbeheer (minimaal / regulier / afrastering; behandeling_x).
# vorm: gevuld bolletje = regulier/minimaal, open bolletje = kreeftwering (KR),
#       driehoekje = NVO (behandeling_vorm). Afrastering zelf heeft geen eigen
#       symbool, dat onderscheid zit al op de x-as.
# y-as: verandering per SlootID = waarde(laatste jaar) - waarde(eerste jaar)
# die voor die sloot beschikbaar zijn (meestal 2024 -> 2026, maar als een
# sloot bv. pas in 2025 start, dan 2025 -> 2026 - zie jaar_eerste/jaar_laatste
# in de onderliggende tabel). Een POSITIEVE waarde betekent dus een TOENAME
# t.o.v. het eerste meetjaar, een NEGATIEVE waarde een AFNAME. De horizontale
# lijn op 0 markeert "geen verandering".
# Let op: een SlootID kan van behandeling_x/vorm wisselen tussen jaren als het
# beheer is aangepast; hier gebruiken we de behandeling_x/vorm van het laatste
# jaar (het jaar waar de verandering "naartoe" gaat).

theme_wp2 <- theme_minimal(base_size = 12) +
  theme(
    panel.border = element_rect(colour = "grey70", fill = NA),
    strip.background = element_rect(fill = "grey95", colour = "grey70"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

# Thema-indeling (kolomnamen zoals in abio_proj), met eenheden voor de plots
themas <- list(
  profiel = c(
    talud            = "tldk_oevrwtr_perc",
    waterdiepte      = "max_wtd",
    doorzicht        = "doorzicht2_mid_m",
    zichtdiepte      = "zichtdiepte",
    slibdikte        = "max_slib"
  ),
  waterbodem = c(
    pal              = "P-AL mg p2o5/100g_SB",
    NH4              = "NH4_µmol/l_PW",
    P_poriewater     = "P_µmol/l_PW",
    redox_slib       = "slib_redox_pH7"
  ),
  oever = c(
    pal              = "P-AL mg p2o5/100g_OR_25",
    nmin             = "N_mineraal_OR_25",
    erosie_index     = "erosieindex",
    vernattingsindex = "vernattingsrisico_index",
    draagkracht_oever = "draagkracht_oever",
    onderholling     = "holleoever"
  ),
  vegetatie = c(
    opp_emers        = "oeverzone_2a_emers_m2",
    opp_emers_2b     = "oeverzone_2b_emers_m2",
    oeverindex       = "oeverindex",
    structuur        = "structuur",
    n_emers          = "aantal_oeverplanten_zone_2",
    helofyten        = "Soortensamenstelling Helofyten",
    hydrofyten       = "Soortensamenstelling Hydrofyten",
    gras_kruid       = "oeverzone_2_gras_kruid_m2",
    productiegras    = "oeverzone_2b_prodgras_m2"
  )
)

# Eenheden per variabele (gebruikt in de facet-labels van de thema-plots).
# "-" betekent dimensieloos/index/percentage-achtig zonder vaste eenheid.
eenheden_var <- c(
  talud              = "%",
  waterdiepte        = "m",
  doorzicht          = "m",
  zichtdiepte        = "doorzicht/waterdiepte, -",
  slibdikte          = "m",
  pal                = "mg P2O5/100g",
  NH4                = "\u00b5mol/l",
  P_poriewater       = "\u00b5mol/l",
  redox_slib         = "mV",
  nmin               = "mg/kg",
  erosie_index       = "index, -",
  vernattingsindex   = "index, -",
  draagkracht_oever  = "MPa",
  onderholling       = "cm",
  helofyten          = "EKR, -",
  hydrofyten         = "EKR, -",
  oeverindex         = "index, -",
  opp_emers          = "m²",
  opp_emers_2b       = "m²",
  structuur          = "-",
  n_emers            = "aantal",
  gras_kruid         = "m²",
  productiegras      = "m²"
)

# Betekenis van een positieve/negatieve verandering per variabele, als
# toelichting in de facet-labels van de veranderingsplots.
richting_var <- c(
  talud              = "+ = steiler talud, − = flauwer",
  waterdiepte        = "+ = dieper, − = ondieper",
  doorzicht          = "+ = meer doorzicht (helderder), − = minder",
  zichtdiepte        = "+ = doorzicht groter t.o.v. waterdiepte, − = kleiner",
  slibdikte          = "+ = dikkere sliblaag, − = dunner",
  pal                = "+ = meer fosfaat (P-AL), − = minder",
  NH4                = "+ = meer ammonium, − = minder",
  P_poriewater       = "+ = meer fosfaat in het poriewater, − = minder",
  redox_slib         = "+ = minder reducerend (meer zuurstof), − = sterker reducerend",
  nmin               = "+ = meer minerale N, − = minder",
  erosie_index       = "+ = hoger erosierisico, − = lager",
  vernattingsindex   = "+ = hoger risico bij vernatten, − = lager",
  draagkracht_oever  = "+ = stevigere oever, − = slapper",
  onderholling       = "+ = dieper uitgehold, − = minder uitgehold",
  helofyten          = "+ = hogere EKR (beter), − = lager",
  hydrofyten         = "+ = hogere EKR (beter), − = lager",
  oeverindex         = "+ = hogere oeverindex, − = lager",
  opp_emers          = "+ = meer emerse vegetatie, − = minder",
  opp_emers_2b       = "+ = meer emerse vegetatie, − = minder",
  structuur          = "+ = meer structuur in de oevervegetatie, − = minder",
  n_emers            = "+ = meer emerse soorten, − = minder",
  gras_kruid         = "+ = meer terrestrische grassen en kruiden, − = minder",
  productiegras      = "+ = meer productiegras, − = minder",
  oeverbreedte       = "+ = bredere oever, − = smaller",
  breedte_2a         = "+ = bredere zone 2a, − = smaller",
  breedte_2b         = "+ = bredere zone 2b, − = smaller"
)

#' Toelichting op de richting van de verandering voor een variabele
#'
#' @param naam Character vector met variabelenamen; een thema-achtervoegsel
#'   (bv. "pal_oever") wordt genegeerd als de volledige naam niet bestaat.
#' @return Character vector met de toelichting ("" als die ontbreekt).
richting_label <- function(naam) {
  basis <- str_remove(naam, "_(profiel|waterbodem|oever|vegetatie|oevermorfologie)$")
  unname(coalesce(richting_var[naam], richting_var[basis], ""))
}

# Leesbare namen voor figuren en tabellen. De interne naam (naam) blijft de
# sleutel in tabellen en in het rapport; alleen het label verandert.
weergavenamen <- c(
  talud              = "Taludhoek oever",
  waterdiepte        = "Waterdiepte",
  doorzicht          = "Doorzicht",
  zichtdiepte        = "Zichtdiepte",
  slibdikte          = "Slibdikte",
  pal                = "P-AL",
  pal_waterbodem     = "P-AL slib",
  pal_oever          = "P-AL oever",
  NH4                = "NH4 poriewater",
  P_poriewater       = "P poriewater",
  redox_slib         = "Redox slib",
  nmin               = "N-mineraal oever",
  erosie_index       = "Erosie-index",
  vernattingsindex   = "Vernattingsindex",
  draagkracht_oever  = "Draagkracht oever",
  onderholling       = "Onderholling",
  helofyten          = "Soortensamenstelling helofyten",
  hydrofyten         = "Soortensamenstelling hydrofyten",
  oeverindex         = "Oeverindex",
  opp_emers          = "Emerse vegetatie zone 2a",
  opp_emers_2b       = "Emerse vegetatie zone 2b",
  structuur          = "Structuur oevervegetatie (deelindicator oeverindex)",
  n_emers            = "Aantal emerse soorten zone 2",
  gras_kruid         = "Terrestrische grassen en kruiden zone 2a + 2b",
  productiegras      = "Productiegras zone 2b",
  oeverbreedte       = "Oeverbreedte",
  breedte_2a         = "Breedte zone 2a",
  breedte_2b         = "Breedte zone 2b"
)

#' Leesbare naam van een variabele
#'
#' @param naam Character vector met interne variabelenamen.
#' @return Character vector met de weergavenaam (de interne naam als er geen
#'   weergavenaam is).
weergave <- function(naam) unname(coalesce(weergavenamen[naam], naam))

# Check of alle kolommen bestaan; verwijder ontbrekende met waarschuwing
themas <- map(themas, check_cols, data = wp2)

# Verandering per SlootID (laatste jaar - eerste jaar), met behandeling_x /
# behandeling_vorm van het laatste jaar.
bereken_verandering <- function(data, var_kolommen) {
  d_lang <- data |>
    select(SlootID, gebied, sloot_nummer, jaar, behandeling_x, behandeling_vorm, all_of(unname(var_kolommen))) |>
    pivot_longer(all_of(unname(var_kolommen)), names_to = "variabele", values_to = "waarde") |>
    mutate(variabele = factor(variabele, levels = unname(var_kolommen), labels = names(var_kolommen))) |>
    filter(!is.na(waarde))

  d_lang |>
    summarise(
      .by = c(SlootID, variabele),
      gebied = gebied[1],
      sloot_nummer = sloot_nummer[1],
      jaar_eerste = min(jaar), jaar_laatste = max(jaar),
      waarde_eerste = waarde[jaar == min(jaar)][1],
      waarde_laatste = waarde[jaar == max(jaar)][1],
      behandeling_x = behandeling_x[jaar == max(jaar)][1],
      behandeling_vorm = behandeling_vorm[jaar == max(jaar)][1]
    ) |>
    filter(jaar_eerste != jaar_laatste) |>
    mutate(verandering = waarde_laatste - waarde_eerste)
}

#' Toets het verschil in verandering tussen regulier, minimaal en afrastering
#'
#' Gemengd model verandering ~ behandeling_x + (1 | gebied) + (1 | sloot_nummer),
#' net als in sectie 12: locaties in dezelfde sloot of hetzelfde gebied tellen
#' zo niet als onafhankelijke metingen (gebied en sloot zijn random effects;
#' er wordt voor gecorrigeerd, niet op getoetst). Bij minder dan 3 gebieden
#' alleen sloot als random effect. Geeft de algemene F-toets (Satterthwaite)
#' en de paarsgewijze verschillen tussen de niveaus (t-toets op het contrast,
#' Holm-gecorrigeerd over de paren).
#'
#' @param d_var Data voor één variabele met verandering, behandeling_x,
#'   gebied en sloot_nummer.
#' @return Tibble met één rij per toets: contrast ("totaal" voor de F-toets,
#'   anders bv. "minimaal − regulier"), schatting (verschil in gemiddelde
#'   verandering), p_waarde en p_holm (alleen voor de paren).
toets_verandering <- function(d_var) {
  leeg <- tibble(contrast = character(), schatting = numeric(), p_waarde = numeric(), p_holm = numeric())
  d <- d_var |> filter(!is.na(verandering), !is.na(behandeling_x)) |> droplevels()
  if (n_distinct(d$behandeling_x) < 2 || nrow(d) < 6) return(leeg)
  random <- if (n_distinct(d$gebied) >= 3) "(1 | gebied) + (1 | sloot_nummer)" else "(1 | sloot_nummer)"
  fit <- tryCatch(
    suppressWarnings(suppressMessages(
      lmerTest::lmer(as.formula(paste("verandering ~ behandeling_x +", random)), data = d)
    )),
    error = \(e) NULL
  )
  if (is.null(fit)) return(leeg)

  p_totaal <- tryCatch(anova(fit)["behandeling_x", "Pr(>F)"], error = \(e) NA_real_)
  coefs <- names(lme4::fixef(fit))
  paarsgewijs <- map_dfr(combn(levels(d$behandeling_x), 2, simplify = FALSE), \(paar) {
    # Contrast paar[2] - paar[1]; het referentieniveau heeft geen eigen coëfficiënt
    L <- setNames(numeric(length(coefs)), coefs)
    for (i in 1:2) {
      term <- paste0("behandeling_x", paar[i])
      if (term %in% coefs) L[term] <- c(-1, 1)[i]
    }
    res <- tryCatch(lmerTest::contest1D(fit, L), error = \(e) NULL)
    tibble(
      contrast = paste(paar[2], "−", paar[1]),
      schatting = if (is.null(res)) NA_real_ else res$Estimate,
      p_waarde = if (is.null(res)) NA_real_ else res$`Pr(>|t|)`
    )
  }) |>
    mutate(p_holm = p.adjust(p_waarde, method = "holm"))

  bind_rows(tibble(contrast = "totaal", schatting = NA_real_, p_waarde = p_totaal, p_holm = NA_real_), paarsgewijs)
}

# Label voor p-waarden in figuren (ook gebruikt in sectie 12 en verder)
p_label <- function(p) if_else(is.na(p), "n.v.t.", trimws(formatC(p, digits = 2, format = "g")))

# "niet significant" linksboven in facetten waar de getoonde p >= 0,05 is
# (zonder correctie voor meervoudig toetsen: zelfs ongecorrigeerd geen
# aanwijzing). Facetten zonder dit label zijn de gevallen die een tweede blik
# waard zijn; let daarbij op de gecorrigeerde p in de tabellen.
caption_niet_sign <- "\"niet significant\" = p ≥ 0,05 (zonder correctie voor meervoudig toetsen)."

#' Laag met "niet significant" linksboven in facetten zonder aanwijzing voor een effect
#'
#' @param data Data met de facetkolom(men) van de plot en een logische kolom
#'   `niet_sign` (TRUE als p >= 0,05).
#' @return Een ggplot2-laag.
laag_niet_significant <- function(data) {
  geom_text(
    data = filter(data, niet_sign),
    aes(x = -Inf, y = Inf, label = "niet significant"),
    inherit.aes = FALSE, hjust = -0.06, vjust = 1.5, size = 3, colour = "grey35", fontface = "italic"
  )
}

# Toetsen per thema en variabele; ook weggeschreven als tabel (einde script).
toetsen_thema <- imap_dfr(themas, \(var_kolommen, thema_naam) {
  bereken_verandering(wp2, var_kolommen) |>
    group_by(variabele) |>
    group_modify(\(x, sleutel) toets_verandering(x)) |>
    ungroup() |>
    mutate(thema = thema_naam, variabele = as.character(variabele), .before = 1)
})
print(toetsen_thema, n = 100)

# Korte namen voor de paren in de facet-labels
afkorting_x <- c(regulier = "reg", minimaal = "min", afrastering = "afr")

# Plot per thema: verandering (y) tegen behandeling_x (x), vorm = behandeling_vorm.
# Facet-labels tonen de eenheid, de algemene p en de paarsgewijze p (Holm).
plot_thema_verandering <- function(thema_naam, var_kolommen, data) {
  d_var <- bereken_verandering(data, var_kolommen)

  labels_p <- toetsen_thema |>
    filter(thema == thema_naam) |>
    summarise(
      .by = variabele,
      p_totaal = p_waarde[contrast == "totaal"][1],
      label_totaal = paste0("p = ", p_label(p_totaal)),
      label_paren = paste0(
        str_replace_all(contrast[contrast != "totaal"], afkorting_x), " ",
        p_label(p_holm[contrast != "totaal"]),
        collapse = " · "
      )
    )

  d_var <- d_var |>
    mutate(variabele = as.character(variabele)) |>
    left_join(labels_p, by = "variabele") |>
    mutate(
      eenheid = eenheden_var[variabele],
      eenheid = ifelse(is.na(eenheid), "", paste0(", ", eenheid)),
      variabele_label = paste0(
        weergave(variabele), eenheid, "\n", richting_label(variabele), "\n",
        coalesce(label_totaal, "p = n.v.t."), "; ", coalesce(label_paren, "")
      ),
      niet_sign = !is.na(p_totaal) & p_totaal >= 0.05
    )

  ggplot(d_var, aes(x = behandeling_x, y = verandering)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.4) +
    geom_jitter(aes(shape = behandeling_vorm, colour = sloot_nummer), width = 0.15, size = 2, alpha = 0.7) +
    stat_summary(fun = median, geom = "point", size = 3, colour = "black", shape = 18) +
    laag_niet_significant(distinct(d_var, variabele_label, niet_sign)) +
    scale_shape_manual(values = vorm_shapes) +
    scale_colour_manual(values = sloot_kleuren) +
    facet_wrap(~variabele_label, scales = "free_y") +
    labs(
      title = paste0("WP2 - ", thema_naam, ": verandering per SlootID (waarde laatste jaar − waarde eerste jaar)"),
      subtitle = paste0(
        "Positief = toename t.o.v. eerste meetjaar, negatief = afname. Zwarte ruit = mediaan over sloten; kleur = sloot (gebied + sloot_nr).\n",
        "p = algemene toets basisbeheer in gemengd model met gebied en sloot als random effect (gecorrigeerd voor gebied en sloot).\n",
        "Facetlabel: betekenis van + en −; daaronder p en paarsgewijze verschillen (reg = regulier, min = minimaal, afr = afrastering; p Holm-gecorrigeerd)."
      ),
      caption = caption_niet_sign,
      x = "Basisbeheer", y = "Verandering (laatste jaar − eerste jaar; eenheid per paneel)",
      shape = "Toevoeging", colour = "Sloot"
    ) +
    theme_wp2
}

plots_thema <- imap(themas, function(var_kolommen, thema_naam) {
  plot_thema_verandering(thema_naam, var_kolommen, data = wp2)
})

walk(plots_thema, print)
iwalk(plots_thema, function(p, thema_naam) {
  sla_figuur_op(p, paste0("thema_verandering_", thema_naam), width = 12, height = 8)
})

# 4b. Visualisatie per thema: mediane waarde per jaar, met/zonder afrastering
# en minimaal/regulier beheer ---------------------------------------------
# Twee aanvullende vergelijkingen per thema (zelfde variabelen/thema's als
# hierboven), met jaar op de x-as en de mediane waarde per groep op de y-as:
#  - facet 1: wel vs. geen afrastering (AF), ongeacht basisbeheer/overig
#  - facet 2: minimaal vs. regulier beheer (alleen sloten zonder afrastering,
#    want "minimaal/regulier" is per definitie het beheer zonder AF)
# Dunne lijnen/punten tonen individuele SlootID's (gekleurd per sloot), de
# vette lijn met punt is de mediaan per groep en jaar.
wp2 <- wp2 |>
  mutate(
    heeft_afrastering = factor(
      ifelse(str_detect(Behandeling, "AF"), "met afrastering", "zonder afrastering"),
      levels = c("zonder afrastering", "met afrastering")
    ),
    beheer_mr = case_when(
      str_detect(Behandeling, "AF") ~ NA_character_,
      str_detect(Behandeling, "^M") ~ "minimaal",
      str_detect(Behandeling, "^R") ~ "regulier",
      TRUE ~ NA_character_
    ),
    beheer_mr = factor(beheer_mr, levels = c("regulier", "minimaal"))
  )

# Mediane waarde per groep (en per sloot, voor de dunne lijnen) x jaar,
# voor een gegeven groeperingskolom (heeft_afrastering of beheer_mr).
bereken_mediaan_jaar <- function(data, var_kolommen, groep_col) {
  data |>
    select(SlootID, sloot_nummer, jaar, groep = all_of(groep_col), all_of(unname(var_kolommen))) |>
    pivot_longer(all_of(unname(var_kolommen)), names_to = "variabele", values_to = "waarde") |>
    mutate(variabele = factor(variabele, levels = unname(var_kolommen), labels = names(var_kolommen))) |>
    filter(!is.na(waarde), !is.na(groep))
}

# Plot: jaar (x) tegen mediane waarde (y), gefacet per variabele, met kleur
# voor de groep (afrastering wel/geen, of minimaal/regulier). Dunne lijnen
# per sloot (grijs, gekleurd zou met 2 groepsklueren + 25 slootkleuren te
# druk worden) op de achtergrond, vette lijn + punt = mediaan per groep/jaar.
plot_thema_jaar_mediaan <- function(thema_naam, var_kolommen, data, groep_col, groep_label,
                                     groep_kleuren) {
  d_lang <- bereken_mediaan_jaar(data, var_kolommen, groep_col)

  d_sloot <- d_lang |>
    summarise(.by = c(SlootID, sloot_nummer, groep, jaar, variabele), waarde = median(waarde, na.rm = TRUE))

  d_mediaan <- d_lang |>
    summarise(.by = c(groep, jaar, variabele), waarde = median(waarde, na.rm = TRUE), n = n())

  d_sloot <- d_sloot |>
    mutate(eenheid = eenheden_var[as.character(variabele)],
           eenheid = ifelse(is.na(eenheid), "", paste0(", ", eenheid)),
           variabele_label = paste0(weergave(as.character(variabele)), eenheid))
  d_mediaan <- d_mediaan |>
    mutate(eenheid = eenheden_var[as.character(variabele)],
           eenheid = ifelse(is.na(eenheid), "", paste0(", ", eenheid)),
           variabele_label = paste0(weergave(as.character(variabele)), eenheid))

  ggplot(d_mediaan, aes(x = factor(jaar), y = waarde, colour = groep, group = groep)) +
    geom_line(
      data = d_sloot, aes(x = factor(jaar), y = waarde, group = interaction(SlootID, groep)),
      colour = "grey70", linewidth = 0.3, alpha = 0.6
    ) +
    geom_point(
      data = d_sloot, aes(x = factor(jaar), y = waarde, group = interaction(SlootID, groep)),
      colour = "grey50", size = 1.2, alpha = 0.5
    ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    scale_colour_manual(values = groep_kleuren) +
    facet_wrap(~variabele_label, scales = "free_y") +
    labs(
      title = paste0("WP2 - ", thema_naam, ": mediane waarde per jaar, ", groep_label),
      subtitle = "Grijze lijnen/punten = individuele SlootID's (mediaan per sloot/jaar); kleur = mediaan per groep en jaar.",
      x = "Jaar", y = "Mediane waarde (eenheid per paneel)", colour = groep_label
    ) +
    theme_wp2
}

kleuren_afrastering <- c("zonder afrastering" = okabe_ito[1], "met afrastering" = okabe_ito[6])
kleuren_beheer_mr   <- c("regulier" = okabe_ito[1], "minimaal" = okabe_ito[3])

plots_thema_afrastering <- imap(themas, function(var_kolommen, thema_naam) {
  plot_thema_jaar_mediaan(
    thema_naam, var_kolommen, data = wp2,
    groep_col = "heeft_afrastering", groep_label = "wel/geen afrastering",
    groep_kleuren = kleuren_afrastering
  )
})

plots_thema_beheer_mr <- imap(themas, function(var_kolommen, thema_naam) {
  plot_thema_jaar_mediaan(
    thema_naam, var_kolommen, data = wp2,
    groep_col = "beheer_mr", groep_label = "minimaal/regulier beheer",
    groep_kleuren = kleuren_beheer_mr
  )
})

walk(plots_thema_afrastering, print)
walk(plots_thema_beheer_mr, print)

iwalk(plots_thema_afrastering, function(p, thema_naam) {
  sla_figuur_op(p, paste0("thema_jaar_afrastering_", thema_naam))
})
iwalk(plots_thema_beheer_mr, function(p, thema_naam) {
  sla_figuur_op(p, paste0("thema_jaar_minimaal_regulier_", thema_naam))
})

# Richting van de verandering expliciet in tabelvorm (bv. om te zien of NH4
# overal daalt of stijgt, los van de plot): mediaan en IQR van de verandering
# per variabele en per basisbeheer, plus hoeveel sloten toe- vs. afnamen.
samenvatting_verandering <- map2_dfr(themas, names(themas), function(var_kolommen, thema_naam) {
  bereken_verandering(wp2, var_kolommen) |>
    mutate(thema = thema_naam) |>
    summarise(
      .by = c(thema, variabele, behandeling_x),
      n = n(),
      n_toename = sum(verandering > 0, na.rm = TRUE),
      n_afname = sum(verandering < 0, na.rm = TRUE),
      mediaan_verandering = median(verandering, na.rm = TRUE),
      q25 = quantile(verandering, 0.25, na.rm = TRUE),
      q75 = quantile(verandering, 0.75, na.rm = TRUE)
    )
})

print(samenvatting_verandering, n = 100)

# 5. Toetsen: verschil tussen Behandelingen, met SlootID als blok/random effect ----
# Hier en in de ANCOVA (sectie 6) gebruiken we behandeling_x (minimaal /
# regulier / afrastering) in plaats van de 9 losse Behandeling-codes: KR en
# NVO worden alleen in de visualisaties (vorm/symbool) getoond, niet als
# aparte behandelingsniveaus getoetst. Dat houdt de toetsen in lijn met de
# gevraagde indeling en voorkomt dat KR/NVO (elk maar 6 waarnemingen) de
# modellen onnodig verzwaren met bijna-lege niveaus.
toets_behandeling <- function(target_col, data) {
  d <- data |>
    select(SlootID, jaar, behandeling_x, waarde = all_of(target_col)) |>
    filter(!is.na(waarde), !is.na(behandeling_x))

  if (n_distinct(d$behandeling_x) < 2 || nrow(d) < 6) {
    return(tibble(target = target_col, methode = NA, p_waarde = NA,
                   opmerking = "te weinig data / te weinig behandelingsniveaus"))
  }

  d$jaar <- factor(d$jaar)
  d$behandeling_x <- factor(d$behandeling_x)

  fit <- tryCatch(
    lmerTest::lmer(waarde ~ behandeling_x + jaar + (1 | SlootID), data = d),
    error = function(e) NULL, warning = function(w) NULL
  )

  if (!is.null(fit)) {
    aov_tab <- tryCatch(anova(fit), error = function(e) NULL)
    if (!is.null(aov_tab) && "behandeling_x" %in% rownames(aov_tab)) {
      return(tibble(
        target = target_col, methode = "lmer (SlootID random)",
        p_waarde = aov_tab["behandeling_x", "Pr(>F)"],
        opmerking = NA
      ))
    }
  }

  # Terugval: gewone ANOVA met SlootID als fixed blok
  fit2 <- tryCatch(
    aov(waarde ~ behandeling_x + jaar + SlootID, data = d),
    error = function(e) NULL
  )
  if (is.null(fit2)) {
    return(tibble(target = target_col, methode = NA, p_waarde = NA,
                   opmerking = "model kon niet gefit worden"))
  }
  tab <- summary(fit2)[[1]]
  tibble(
    target = target_col, methode = "aov (SlootID fixed blok)",
    p_waarde = tab["behandeling_x", "Pr(>F)"],
    opmerking = NA
  )
}

resultaten_behandeling <- map_dfr(unname(targets), toets_behandeling, data = wp2) |>
  mutate(p_adj_BH = p.adjust(p_waarde, method = "BH"))

print(resultaten_behandeling)

# 6. ANCOVA: hangt het Behandelingseffect af van het niveau van een predictor? ----
# Model: waarde ~ behandeling_x * covariaat + jaar + (SlootID als blok)
# Net als in sectie 5 gebruiken we behandeling_x (minimaal/regulier/
# afrastering) als behandelingsfactor, niet de 9 losse Behandeling-codes:
# KR en NVO blijven voorbehouden aan de visualisatie (vorm/symbool), niet
# als aparte niveaus in het getoetste model.
# Een significante behandeling_x:covariaat-interactie betekent dat het
# verschil tussen minimaal/regulier/afrastering niet constant is, maar
# afhangt van het niveau van de covariaat (bv. alleen bij lage P-AL, of
# alleen bij ondiepe waterdiepte).
# Per target gebruiken we alleen diens eigen top-predictoren (uit sectie 3),
# zodat het aantal getoetste combinaties beperkt blijft.

ancova_interactie <- function(target_col, predictor_col, data) {
  d <- data |>
    select(SlootID, jaar, behandeling_x,
           waarde = all_of(target_col), covariaat = all_of(predictor_col)) |>
    filter(!is.na(waarde), !is.na(covariaat), !is.na(behandeling_x))

  if (n_distinct(d$behandeling_x) < 2 || nrow(d) < 10 ||
      n_distinct(d$SlootID) < 3) {
    return(tibble(target = target_col, predictor = predictor_col,
                   p_interactie = NA, opmerking = "te weinig data"))
  }
  if (!is.numeric(d$covariaat)) {
    return(tibble(target = target_col, predictor = predictor_col,
                   p_interactie = NA, opmerking = "covariaat niet numeriek"))
  }

  d$jaar <- factor(d$jaar)
  d$behandeling_x <- factor(d$behandeling_x)
  d$covariaat_z <- as.numeric(scale(d$covariaat))

  fit <- tryCatch(
    lmerTest::lmer(waarde ~ behandeling_x * covariaat_z + jaar + (1 | SlootID), data = d),
    error = function(e) NULL, warning = function(w) NULL
  )

  if (is.null(fit)) {
    return(tibble(target = target_col, predictor = predictor_col,
                   p_interactie = NA, opmerking = "model kon niet gefit worden"))
  }

  aov_tab <- tryCatch(anova(fit), error = function(e) NULL)
  interactie_term <- grep(":", rownames(aov_tab), value = TRUE)
  if (is.null(aov_tab) || length(interactie_term) == 0) {
    return(tibble(target = target_col, predictor = predictor_col,
                   p_interactie = NA, opmerking = "geen interactieterm in model"))
  }

  tibble(
    target = target_col, predictor = predictor_col,
    p_interactie = aov_tab[interactie_term[1], "Pr(>F)"],
    opmerking = NA
  )
}

# Combinaties: elke target met zijn eigen top-predictoren (max n_top_predictoren),
# exclusief de covariaat gelijk aan de target zelf.
combinaties <- top_predictoren_per_target |>
  rename(target_col = target_var, predictor_col = Feature) |>
  filter(target_col != predictor_col) |>
  select(target_col, predictor_col)

resultaten_ancova <- map2_dfr(
  combinaties$target_col, combinaties$predictor_col,
  ancova_interactie, data = wp2
) |>
  mutate(p_adj_BH = p.adjust(p_interactie, method = "BH")) |>
  arrange(p_adj_BH)

print(resultaten_ancova, n = 30)

significante_interacties <- resultaten_ancova |>
  filter(!is.na(p_adj_BH), p_adj_BH < 0.05)

if (nrow(significante_interacties) > 0) {
  message(nrow(significante_interacties), " significante Behandeling x covariaat interacties (BH < 0.05):")
  print(significante_interacties)
} else {
  message("Geen significante Behandeling x covariaat interacties na BH-correctie.")
}

# 6b. Modeloutput (coëfficiënten) van de ANCOVA-modellen -----------------------
# resultaten_ancova bevat alleen de p-waarde van de Behandeling:covariaat
# interactieterm (uit anova()). Voor de volledige modeloutput - geschatte
# hellingen per Behandeling, standaardfouten, t- en p-waarden per coëfficiënt -
# fitten we de modellen hieronder opnieuw en bewaren we de tidy-samenvatting.
# Dit is de output die je kunt rapporteren als "het ANCOVA-model".
fit_ancova_model <- function(target_col, predictor_col, data) {
  d <- data |>
    select(SlootID, jaar, behandeling_x,
           waarde = all_of(target_col), covariaat = all_of(predictor_col)) |>
    filter(!is.na(waarde), !is.na(covariaat), !is.na(behandeling_x))

  if (n_distinct(d$behandeling_x) < 2 || nrow(d) < 10 || n_distinct(d$SlootID) < 3) return(NULL)
  if (!is.numeric(d$covariaat)) return(NULL)

  d$jaar <- factor(d$jaar)
  d$behandeling_x <- factor(d$behandeling_x)
  d$covariaat_z <- as.numeric(scale(d$covariaat))

  tryCatch(
    lmerTest::lmer(waarde ~ behandeling_x * covariaat_z + jaar + (1 | SlootID), data = d),
    error = function(e) NULL, warning = function(w) NULL
  )
}

# Modeloutput (coëfficiënten) + plots: voor elk target tonen we altijd
# minstens de sterkste (laagste p) covariaat-interactie, ook als die niet
# significant is - zo blijft elk getest target in beeld. Combinaties die al
# significant zijn (BH < 0.05) worden sowieso toegevoegd, ook als een target
# daardoor met meer dan één predictor in beeld komt.
sterkste_per_target <- resultaten_ancova |>
  filter(!is.na(p_interactie)) |>
  group_by(target) |>
  slice_min(p_interactie, n = 1, with_ties = FALSE) |>
  ungroup()

interacties_voor_modeloutput <- bind_rows(significante_interacties, sterkste_per_target) |>
  distinct(target, predictor, .keep_all = TRUE) |>
  arrange(p_adj_BH)

print(interacties_voor_modeloutput, n = 30)

modellen_ancova <- map2(
  interacties_voor_modeloutput$target, interacties_voor_modeloutput$predictor,
  fit_ancova_model, data = wp2
)
names(modellen_ancova) <- paste(interacties_voor_modeloutput$target,
                                  interacties_voor_modeloutput$predictor, sep = " ~ behandeling_x x ")

coefficienten_ancova <- map2_dfr(
  modellen_ancova, names(modellen_ancova),
  function(fit, naam) {
    if (is.null(fit)) return(tibble())
    broom.mixed::tidy(fit, effects = "fixed") |> mutate(model = naam, .before = 1)
  }
)

# coefficienten_ancova: per model (target ~ Behandeling x predictor) de
# geschatte effecten, standaardfout, t- en p-waarde - dit is de volledige
# ANCOVA-modeloutput.
print(coefficienten_ancova, n = 100)

# 7. Visualisatie van de ANCOVA-resultaten -------------------------------------
# Regressielijnen per Behandeling over het bereik van de covariaat, voor elke
# significante interactie (of, als er geen zijn, de sterkste kandidaten).
# Deze plots horen bij de modellen in coefficienten_ancova/modellen_ancova:
# als de lijnen per Behandeling een duidelijk verschillende richting/helling
# hebben, betekent dat dat het effect van Behandeling op de target afhangt
# van het niveau van de covariaat (= de interactie). Punten zijn individuele
# metingen (gevormd per meetjaar); de band rond elke lijn is het 95%-
# betrouwbaarheidsinterval van een gewone (ongeblokte) lineaire regressie per
# Behandeling, ter illustratie - de getoetste p-waarde komt uit het gemengde
# model in coefficienten_ancova, niet uit deze geom_smooth-lijnen.
plot_ancova <- function(target_col, predictor_col, data, p_adj = NA_real_) {
  d <- data |>
    select(SlootID, jaar, behandeling_x, behandeling_vorm,
           waarde = all_of(target_col), covariaat = all_of(predictor_col)) |>
    filter(!is.na(waarde), !is.na(covariaat), !is.na(behandeling_x))

  label_p <- if (is.na(p_adj)) "n.v.t." else paste0(
    "p(BH) = ", formatC(p_adj, digits = 3, format = "g"),
    ifelse(p_adj < 0.05, " (significant)", " (n.s.)")
  )

  ggplot(d, aes(x = covariaat, y = waarde, color = behandeling_x)) +
    geom_point(aes(shape = behandeling_vorm), size = 2, alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.15) +
    scale_shape_manual(values = vorm_shapes) +
    labs(
      title = paste0(target_col, " ~ Basisbeheer \u00d7 ", predictor_col),
      subtitle = paste0("Interactie basisbeheer:covariaat, ", label_p),
      x = predictor_col, y = target_col, color = "Basisbeheer", shape = "Toevoeging"
    ) +
    theme_wp2
}

plots_ancova <- pmap(
  list(
    interacties_voor_modeloutput$target,
    interacties_voor_modeloutput$predictor,
    interacties_voor_modeloutput$p_adj_BH
  ),
  function(target_col, predictor_col, p_adj) plot_ancova(target_col, predictor_col, data = wp2, p_adj = p_adj)
)

# Toon in groepjes van 4 voor leesbaarheid. De uitleg over wat de hellingen
# betekenen staat één keer als gezamenlijke titel boven elke groep (i.p.v. als
# subtitle in elk los paneel, wat bij meerdere panelen naast elkaar overlapt).
if (length(plots_ancova) > 0) {
  groepen_ancova <- split(seq_along(plots_ancova), ceiling(seq_along(plots_ancova) / 4))
  walk(groepen_ancova, function(idx) {
    print(
      patchwork::wrap_plots(plots_ancova[idx], ncol = 2) +
        patchwork::plot_annotation(
          title = "ANCOVA: Basisbeheer \u00d7 covariaat",
          subtitle = paste0(
            "Verschillende hellingen per basisbeheer (minimaal/regulier/afrastering) = het effect hangt af van het niveau van deze covariaat.\n",
            "De getoetste p-waarde (zie resultaten_ancova/coefficienten_ancova) hoort bij de interactieterm basisbeheer:covariaat: p < 0.05 betekent dat de hellingen aantoonbaar van elkaar verschillen."
          )
        )
    )
  })
}

# 8. Draagkracht/indringingsweerstand: verandering per SlootID en dieptebin ----
# Long-format penetrometerdata (penmerge) heeft één rij per meting, met
# diepte (cm) en indringingsweerstand (MPa). We filteren op WP2 en op
# oever-metingen (sectie == "oever"), binnen per 5 cm diepte, en berekenen per
# SlootID x dieptebin eerst de mediane indringingsweerstand per jaar, en
# daarvan de verandering (laatste jaar - eerste jaar beschikbaar voor die
# sloot/dieptebin) - analoog aan de thema-verandering-plots in sectie 4.
# NVO laten we hier weg als categorie (te weinig sloten/diepte-combinaties om
# betrouwbaar te splitsen); basisbeheer is minimaal/regulier/afrastering
# (behandeling_x), waarbij M en R zowel met als zonder NVO/KR meetellen zolang
# ze geen AF hebben. Per dieptebin toetsen we (ANOVA) of de mediane
# verandering verschilt tussen minimaal/regulier/afrastering: dicht bolletje
# = significant verschil tussen behandelingen op die diepte, open bolletje =
# niet significant.

if (!exists("penmerge")) {
  # penmerge zit in de Processed_data_workspace.RData die main_veest.R inleest.
  # Als main_veest.R (t/m regel ~258) al gedraaid is in deze sessie is het
  # object al aanwezig; anders hier alsnog laden.
  sys.load.image(paste0(workspace, "Processed_data_workspace.RData"), quiet = TRUE)
}

penmerge <- as.data.table(penmerge)

# De penmerge uit de RData heeft nog geen WP en Behandeling; die voegt
# main_veest.R pas toe. Ontbreken ze, dan hier aanvullen uit abio_proj (per
# SlootID en jaar), zodat het script ook zonder main_veest.R draait.
ontbrekend_pen <- setdiff(c("WP", "Behandeling"), names(penmerge))
if (length(ontbrekend_pen) > 0) {
  penmerge[, jaar := as.integer(jaar)]
  loc_pen <- unique(abio_proj[, c("SlootID", "jaar", ontbrekend_pen), with = FALSE], by = c("SlootID", "jaar"))
  loc_pen[, jaar := as.integer(jaar)]
  penmerge <- merge(penmerge, loc_pen, by = c("SlootID", "jaar"), all.x = TRUE)
}

pm_oever <- penmerge[
  WP == "WP2" & sectie == "oever" &
    !is.na(Diept) & !is.na(indringingsweerstand) & !is.na(Behandeling) &
    !str_detect(Behandeling, "NVO") &
    # alleen vakken uit de selectie (sectie 2e)
    paste(SlootID, jaar) %in% vakken_selectie
]

# Basisbeheer: minimaal / regulier / afrastering (afrastering wint bij
# combinaties), consistent met behandeling_x in de rest van het script. KR
# telt hier niet als aparte categorie, alleen AF/M/R bepalen de x-as.
pm_oever[, behandeling_x := fcase(
  str_detect(Behandeling, "AF"), "afrastering",
  str_detect(Behandeling, "^M"), "minimaal",
  str_detect(Behandeling, "^R"), "regulier",
  default = NA_character_
)]
pm_oever[, behandeling_x := factor(behandeling_x, levels = c("regulier", "minimaal", "afrastering"))]

# 5 cm dieptebins, met het bin-midden als numerieke diepte voor de y-as
diepte_breaks <- seq(0, 85, by = 5)
pm_oever[, dieptebin_5cm := cut(Diept, breaks = diepte_breaks, include.lowest = TRUE)]
pm_oever[, diepte_mid := diepte_breaks[as.integer(dieptebin_5cm)] + 2.5]

# Mediane indringingsweerstand per SlootID x dieptebin x jaar
mediaan_sloot_diepte_jaar <- pm_oever[
  , .(mediaan_weerstand = median(indringingsweerstand, na.rm = TRUE)),
  by = .(SlootID, behandeling_x, diepte_mid, jaar)
]

# Verandering per SlootID x dieptebin: laatste jaar - eerste jaar beschikbaar
# voor die combinatie (net als bereken_verandering() in sectie 4, maar dan
# toegepast op dieptebins in plaats van op een enkele kolom per sloot).
verandering_draagkracht <- mediaan_sloot_diepte_jaar[
  , .(
    jaar_eerste = min(jaar), jaar_laatste = max(jaar),
    waarde_eerste = mediaan_weerstand[jaar == min(jaar)][1],
    waarde_laatste = mediaan_weerstand[jaar == max(jaar)][1],
    behandeling_x = behandeling_x[1]
  ),
  by = .(SlootID, diepte_mid)
][jaar_eerste != jaar_laatste][, verandering := waarde_laatste - waarde_eerste]

# Per dieptebin: ANOVA op verandering ~ behandeling_x (minimaal/regulier/
# afrastering) - toetst of de mediane verandering in indringingsweerstand op
# die diepte verschilt tussen de drie basisbeheer-groepen.
toets_dieptebin <- function(d) {
  if (n_distinct(d$behandeling_x) < 2 || nrow(d) < 6) return(NA_real_)
  fit <- tryCatch(aov(verandering ~ behandeling_x, data = d), error = function(e) NULL)
  if (is.null(fit)) return(NA_real_)
  tab <- summary(fit)[[1]]
  if (!"behandeling_x" %in% trimws(rownames(tab))) return(NA_real_)
  tab[grep("behandeling_x", rownames(tab)), "Pr(>F)"][1]
}

significantie_diepte <- verandering_draagkracht[
  !is.na(behandeling_x),
  .(p_waarde = toets_dieptebin(.SD)),
  by = .(diepte_mid)
]
significantie_diepte[, significant := !is.na(p_waarde) & p_waarde < 0.05]

print(significantie_diepte[order(diepte_mid)])

# Mediane verandering per dieptebin x behandeling_x, voor de lijn/punten in
# de plot (mediaan over sloten van de per-sloot-verandering).
mediaan_verandering_diepte <- verandering_draagkracht[
  !is.na(behandeling_x),
  .(mediaan_verandering = median(verandering, na.rm = TRUE), n = .N),
  by = .(behandeling_x, diepte_mid)
][order(behandeling_x, diepte_mid)] |>
  left_join(significantie_diepte, by = "diepte_mid")

plot_profiel_draagkracht <- ggplot(
  verandering_draagkracht |> filter(!is.na(behandeling_x)),
  aes(x = verandering, y = -diepte_mid, color = behandeling_x)
) +
  geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.4) +
  geom_point(
    position = position_jitter(height = 1, width = 0),
    alpha = 0.15, size = 1.1
  ) +
  geom_path(
    data = mediaan_verandering_diepte, aes(x = mediaan_verandering, y = -diepte_mid, color = behandeling_x),
    linewidth = 0.9
  ) +
  geom_point(
    data = mediaan_verandering_diepte, aes(x = mediaan_verandering, y = -diepte_mid, color = behandeling_x,
                                             shape = significant),
    size = 2.3
  ) +
  scale_shape_manual(
    values = c(`TRUE` = 16, `FALSE` = 1), na.value = 1,
    labels = c(`TRUE` = "significant (p < 0.05)", `FALSE` = "niet significant"),
    name = "Verschil tussen basisbeheer"
  ) +
  lims(x = c(-0.5, 0.5), y = c(-85, 0)) +
  labs(
    title = "WP2 - Verandering indringingsweerstand oever per dieptebin (5 cm), per basisbeheer",
    subtitle = paste0(
      "Mediane verandering per SlootID (laatste jaar \u2212 eerste jaar) per dieptebin. Positief = toename, negatief = afname.\n",
      "Dicht bolletje = significant verschil tussen minimaal/regulier/afrastering op die diepte (ANOVA, p < 0.05)."
    ),
    x = "Verandering indringingsweerstand (MPa, laatste jaar \u2212 eerste jaar)",
    y = "Diepte (cm, negatief = dieper)",
    color = "Basisbeheer"
  ) +
  theme_wp2

plot_profiel_draagkracht
sla_figuur_op(plot_profiel_draagkracht, "profiel_draagkracht_diepte")

# 8b. Zelfde profiel per gebied. Gebied uit het SlootID-voorvoegsel (bv. "RH").
# De toets per dieptebin gebeurt hier binnen het gebied; met weinig sloten per
# gebied is die zwak (geen toets bij < 6 locaties of < 2 behandelingen).

#' Profiel van de verandering in indringingsweerstand per gebied en basisbeheer
#'
#' @param verandering data.table met per SlootID x dieptebin de verandering
#'   (kolommen SlootID, behandeling_x, diepte_mid, verandering).
#' @return Een ggplot-object.
maak_profiel_per_gebied <- function(verandering) {
  v <- copy(verandering)[!is.na(behandeling_x)]
  v[, gebied := str_extract(SlootID, "^[A-Z]+")]

  significantie <- v[, .(p_waarde = toets_dieptebin(.SD)), by = .(gebied, diepte_mid)]
  significantie[, significant := !is.na(p_waarde) & p_waarde < 0.05]

  n_locaties <- v[, .(n_locaties = uniqueN(SlootID)), by = gebied]
  n_locaties[, gebied_label := paste0(gebied, " (", n_locaties, " locaties)")]

  mediaan <- v[
    , .(mediaan_verandering = median(verandering, na.rm = TRUE), n = .N),
    by = .(gebied, behandeling_x, diepte_mid)
  ][order(gebied, behandeling_x, diepte_mid)] |>
    left_join(significantie, by = c("gebied", "diepte_mid")) |>
    left_join(n_locaties, by = "gebied")

  ggplot(v[n_locaties, on = "gebied"], aes(x = verandering, y = -diepte_mid, color = behandeling_x)) +
    geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.4) +
    geom_point(position = position_jitter(height = 1, width = 0), alpha = 0.15, size = 1.1) +
    geom_path(data = mediaan, aes(x = mediaan_verandering, y = -diepte_mid, color = behandeling_x), linewidth = 0.9) +
    geom_point(
      data = mediaan, aes(x = mediaan_verandering, y = -diepte_mid, color = behandeling_x, shape = significant),
      size = 2
    ) +
    scale_shape_manual(
      values = c(`TRUE` = 16, `FALSE` = 1), na.value = 1,
      labels = c(`TRUE` = "significant (p < 0.05)", `FALSE` = "niet significant"),
      name = "Verschil tussen basisbeheer"
    ) +
    coord_cartesian(xlim = c(-0.5, 0.5), ylim = c(-85, 0)) +
    facet_wrap(~gebied_label, nrow = 2) +
    labs(
      title = paste0("WP2 - Verandering indringingsweerstand oever per dieptebin (5 cm), per gebied en basisbeheer"),
      subtitle = paste0(
        "Lijn = mediane verandering per SlootID (laatste jaar − eerste jaar) per dieptebin. Positief = stevigere oever, negatief = slapper.\n",
        "Dicht bolletje = significant verschil tussen de behandelingen binnen het gebied op die diepte (ANOVA, p < 0.05); indicatief, weinig sloten per gebied."
      ),
      x = "Verandering indringingsweerstand (MPa, laatste jaar − eerste jaar)",
      y = "Diepte (cm, negatief = dieper)",
      color = "Basisbeheer"
    ) +
    theme_wp2
}

plot_profiel_draagkracht_gebied <- maak_profiel_per_gebied(verandering_draagkracht)
print(plot_profiel_draagkracht_gebied)
sla_figuur_op(plot_profiel_draagkracht_gebied, "profiel_draagkracht_diepte_per_gebied", width = 14, height = 10)

# 8b. Verandering in indringingsweerstand per gebied, laag en behandeling -------
# Voor de duiding in het rapport: gemiddelde verandering (laatste - eerste
# jaar) van de mediane indringingsweerstand per vak, in drie lagen, per
# behandeling (R, M, R-AF, M-AF; KR telt niet mee als aparte categorie).
draagkracht_laag_gebied <- pm_oever[
  , behandeling_4 := fcase(
    str_detect(Behandeling, "^M-AF"), "M-AF", str_detect(Behandeling, "^R-AF"), "R-AF",
    str_detect(Behandeling, "^M"), "M", str_detect(Behandeling, "^R"), "R"
  )
][
  , laag := fcase(diepte_mid <= 20, "0-20 cm", diepte_mid <= 40, "20-40 cm", default = "40-85 cm")
][
  , .(w = median(indringingsweerstand, na.rm = TRUE)), by = .(SlootID, behandeling_4, laag, jaar)
][
  , .(jaar_eerste = min(jaar), jaar_laatste = max(jaar),
      verandering = w[jaar == max(jaar)][1] - w[jaar == min(jaar)][1]),
  by = .(SlootID, behandeling_4, laag)
][jaar_eerste != jaar_laatste][
  , gebied := str_extract(SlootID, "^[A-Z]+")
][
  , .(gem_verandering = mean(verandering), n_vakken = .N, n_sloten = uniqueN(str_extract(SlootID, "^[A-Z]+_\\d+")),
      jaar_laatste_min = min(jaar_laatste)),
  by = .(gebied, laag, behandeling = behandeling_4)
][order(gebied, laag, behandeling)]
print(draagkracht_laag_gebied)

# 9. Effectcategorie per sloot en behandelingsfactoren --------------------------
# Op basis van de veldbeoordeling is per sloot vastgesteld wat er sinds de
# start van WP2 zichtbaar is veranderd. Deze veldbeoordeling is alleen voor de
# duiding: sloten zonder waargenomen effect zijn in de figuren herkenbaar, maar
# blijven in alle toetsen. De selectie van vakken voor de analyse volgt alleen
# MeenemenDataAnalyse_WP2 (sectie 2e).
sloten_geen_effect        <- c("RH_1", "RH_8", "RH_10", "RH_11")
sloten_alleen_morfologie  <- c("RH_2", "RH_3")
sloten_alleen_draagkracht <- c("WL_1")
effect_niveaus <- c(
  "vegetatie (+ draagkracht/morfologie)", "alleen morfologie",
  "alleen draagkracht", "geen effect"
)

# Peilverhoging komt uit de kolom `Vernat?` van wp_locaties_naam_correcties.xlsx
# (vernat_loc in abio_proj; 'ja', 'tijdelijk' en 'beperkt' tellen als
# peilverhoging, net als in Analyses_dev_db.R). Sloten die hieronder
# handmatig worden opgegeven, krijgen ook "met peilverhoging" (bv. zolang de
# locatietabel nog niet is bijgewerkt).
peil_ja <- c("ja", "tijdelijk", "beperkt", "1")
sloten_peilverhoging_handmatig <- character(0)

#' Voeg behandelingsfactoren, oeverbreedte, peilverhoging en effectcategorie toe
#'
#' Splitst de Behandeling-code op in losse factoren (basisbeheer minimaal/
#' regulier, afrastering, NVO, afkreeften) zodat per gebied de contrasten
#' getoetst kunnen worden die daar in de proefopzet zitten.
#'
#' @param data Data met de kolommen Behandeling, sloot_nummer, vernat_loc,
#'   oeverzone_2a_breedte_cm en oeverzone_2b_breedte_cm.
#' @return `data` met de extra kolommen basis_mr, afrastering, nvo,
#'   afkreeften, behandeling_basis, oeverbreedte_2a2b_cm, peilverhoging en
#'   effect_categorie.
voeg_behandelingsfactoren_toe <- function(data) {
  data |>
    mutate(
      basis_mr = factor(
        if_else(str_detect(Behandeling, "^M"), "minimaal", "regulier"),
        levels = c("regulier", "minimaal")
      ),
      afrastering = factor(
        if_else(str_detect(Behandeling, "AF"), "met AF", "zonder AF"),
        levels = c("zonder AF", "met AF")
      ),
      nvo = factor(
        if_else(str_detect(Behandeling, "NVO"), "met NVO", "zonder NVO"),
        levels = c("zonder NVO", "met NVO")
      ),
      afkreeften = factor(
        if_else(str_detect(Behandeling, "KR"), "met afkreeften", "zonder afkreeften"),
        levels = c("zonder afkreeften", "met afkreeften")
      ),
      behandeling_basis = factor(
        paste0(if_else(basis_mr == "minimaal", "M", "R"), if_else(afrastering == "met AF", "-AF", "")),
        levels = c("R", "M", "R-AF", "M-AF")
      ),
      oeverbreedte_2a2b_cm = oeverzone_2a_breedte_cm + oeverzone_2b_breedte_cm,
      peilverhoging = factor(
        if_else(
          as.character(vernat_loc) %in% peil_ja | sloot_nummer %in% sloten_peilverhoging_handmatig,
          "met peilverhoging", "zonder peilverhoging"
        ),
        levels = c("zonder peilverhoging", "met peilverhoging")
      ),
      effect_categorie = factor(
        case_when(
          sloot_nummer %in% sloten_geen_effect ~ "geen effect",
          sloot_nummer %in% sloten_alleen_morfologie ~ "alleen morfologie",
          sloot_nummer %in% sloten_alleen_draagkracht ~ "alleen draagkracht",
          TRUE ~ effect_niveaus[1]
        ),
        levels = effect_niveaus
      )
    )
}

wp2 <- voeg_behandelingsfactoren_toe(wp2)
print(wp2 |> distinct(gebied, sloot_nummer, effect_categorie, peilverhoging) |> arrange(gebied, sloot_nummer))

# Oukoop: SlootID is bv. DR_1_M-JUL_Z (sloot zonder afkreeften) en
# DR_1a_M-JUL-KR_Z (de bijbehorende sloot met afkreeften). Sloten met en
# zonder "a" vormen een paar (sloot_paar). Maaibeheer: regulier, of minimaal
# met maaien in juli of in augustus.
if (!is.null(oukoop) && nrow(oukoop) > 0) {
  oukoop <- oukoop |>
    mutate(
      sloot_nummer = str_extract(SlootID, "^[A-Z]+_\\d+a?"),
      sloot_paar = str_remove(sloot_nummer, "a$"),
      maaibeheer = factor(
        case_when(
          str_detect(Behandeling, "JUL") ~ "minimaal, maaien juli",
          str_detect(Behandeling, "AUG") ~ "minimaal, maaien augustus",
          str_detect(Behandeling, "^R") ~ "regulier"
        ),
        levels = c("regulier", "minimaal, maaien juli", "minimaal, maaien augustus")
      )
    ) |>
    voeg_behandelingsfactoren_toe()
  print(oukoop |> count(sloot_paar, sloot_nummer, afkreeften, maaibeheer))
}

# 10. Variabelen en verandering per locatie -------------------------------------
# Zelfde thema's als in sectie 4, aangevuld met oevermorfologie: totale
# oeverbreedte (zone 2a + 2b) en de breedte van zone 2a en 2b afzonderlijk.
themas_uitgebreid <- c(themas, list(oevermorfologie = c(
  oeverbreedte = "oeverbreedte_2a2b_cm",
  breedte_2a   = "oeverzone_2a_breedte_cm",
  breedte_2b   = "oeverzone_2b_breedte_cm"
)))
eenheden_var <- c(eenheden_var, oeverbreedte = "cm", breedte_2a = "cm", breedte_2b = "cm")

# Platte variabelenlijst; een kolom die in meerdere thema's staat telt één
# keer, en dubbele namen (bv. 'pal' voor slib en oever) krijgen het thema erbij.
var_lijst <- imap_dfr(themas_uitgebreid, \(kol, thema) tibble(thema = thema, naam = names(kol), kolom = unname(kol))) |>
  filter(kolom %in% names(wp2)) |>
  distinct(kolom, .keep_all = TRUE) |>
  mutate(
    eenheid = coalesce(unname(eenheden_var[naam]), "-"),
    naam = if_else(naam %in% naam[duplicated(naam)], paste0(naam, "_", thema), naam),
    label = paste0(weergave(naam), " (", eenheid, ")"),
    thema = factor(thema, levels = names(themas_uitgebreid))
  )

#' Voeg de richtingstoelichting toe aan een variabelelabel (voor facet-labels)
#'
#' @param label Character vector met labels zoals in var_lijst$label.
#' @param breedte Maximale regelbreedte van de toelichting (str_wrap).
#' @return Label met daaronder wat + en − betekenen (ongewijzigd als het label
#'   niet in var_lijst staat of er geen toelichting is).
met_richting <- function(label, breedte = 80) {
  richting <- unname(setNames(richting_label(var_lijst$naam), var_lijst$label)[label])
  if_else(is.na(richting) | richting == "", label, paste0(label, "\n", str_wrap(richting, breedte)))
}

groep_cols <- c(
  "gebied", "sloot_nummer", "Behandeling", "behandeling_basis", "behandeling_vorm",
  "basis_mr", "afrastering", "nvo", "afkreeften", "peilverhoging", "effect_categorie"
)

#' Verandering per locatie tussen eerste en laatste meetjaar, in lang formaat
#'
#' Net als bereken_verandering() in sectie 4, maar voor meerdere variabelen
#' tegelijk en met alle opgegeven groepeerkolommen (waarde van het laatste
#' meetjaar van de locatie).
#'
#' @param data Data met SlootID, jaar, `groep_cols` en de kolommen uit `var_lijst`.
#' @param var_lijst Tibble met minstens de kolommen kolom, thema, naam en label.
#' @param groep_cols Character vector met groepeerkolommen die mee moeten.
#' @return Tibble met één rij per SlootID x variabele, met verandering =
#'   waarde laatste jaar - waarde eerste jaar.
bereken_verandering_lang <- function(data, var_lijst, groep_cols) {
  data |>
    as_tibble() |>
    select(SlootID, jaar, all_of(groep_cols), all_of(var_lijst$kolom)) |>
    mutate(across(all_of(var_lijst$kolom), as.numeric)) |>
    pivot_longer(all_of(var_lijst$kolom), names_to = "kolom", values_to = "waarde") |>
    filter(!is.na(waarde)) |>
    arrange(jaar) |>
    summarise(
      .by = c(SlootID, kolom),
      across(all_of(groep_cols), last),
      jaar_eerste = first(jaar), jaar_laatste = last(jaar),
      waarde_eerste = first(waarde), waarde_laatste = last(waarde)
    ) |>
    filter(jaar_eerste != jaar_laatste) |>
    mutate(verandering = waarde_laatste - waarde_eerste) |>
    left_join(var_lijst, by = "kolom")
}

wp2_verandering <- bereken_verandering_lang(wp2, var_lijst, groep_cols)

# 11. Toetsfunctie voor alle contrasten -----------------------------------------
#' Toets een (set) behandelingsterm(en) met een lineair of gemengd model
#'
#' Kiest de opzet op basis van de data: varieert de eerste behandelingsterm
#' binnen een blok (bv. verschillende behandelingen langs dezelfde sloot), dan
#' wordt het blok als vaste factor meegenomen (geblokte vergelijking binnen
#' sloten). Zo niet (bv. afkreeften geldt voor de hele sloot), dan wordt het
#' blok een random effect, zodat het behandelingseffect tussen sloten wordt
#' geschat. Covariaten met maar één niveau in de subset worden weggelaten.
#'
#' @param d Data voor één variabele, met de kolommen uit de formule plus
#'   SlootID en sloot_nummer.
#' @param model_termen Behandelingsdeel van de formule, bv. "basis_mr * afrastering".
#' @param toets_termen Termen waarvoor p-waarde en schatting worden gerapporteerd.
#' @param covariaten Optionele extra termen (character vector).
#' @param blok Optionele blokvariabele, bv. "sloot_nummer".
#' @param random Optionele random-effectterm(en), bv. "(1 | gebied)".
#' @param respons Naam van de responskolom (standaard "verandering").
#' @return Tibble met per toets_term de schatting (alleen bij één coëfficiënt,
#'   bv. een 2-niveau-factor), p-waarde (F-toets; lm: type II, lmer: type III
#'   met Satterthwaite), aantal locaties en sloten, opzet en opmerking.
toets_model <- function(d, model_termen, toets_termen, covariaten = character(0),
                        blok = NULL, random = NULL, respons = "verandering") {
  d <- droplevels(filter(d, !is.na(.data[[respons]])))
  uitkomst <- tibble(
    term = toets_termen, schatting = NA_real_, p_waarde = NA_real_,
    n_locaties = n_distinct(d$SlootID), n_sloten = n_distinct(d$sloot_nummer),
    opzet = NA_character_, opmerking = NA_character_
  )
  hoofdtermen <- str_trim(unlist(str_split(model_termen, "[*+:]")))
  # Minimaal 4 waarnemingen (bv. 2 sloten x 2 behandelingen); heeft het model
  # daarna geen vrijheidsgraden over, dan geeft de F-toets zelf NA.
  if (nrow(d) < 4 || any(map_lgl(hoofdtermen, \(t) n_distinct(d[[t]]) < 2))) {
    return(mutate(uitkomst, opmerking = "te weinig data of maar één niveau"))
  }
  covariaten <- covariaten[map_lgl(covariaten, \(t) n_distinct(d[[t]]) > 1)]

  opzet_tekst <- "zonder blok"
  if (!is.null(blok) && n_distinct(d[[blok]]) > 1) {
    varieert_binnen_blok <- d |>
      summarise(.by = all_of(blok), n = n_distinct(.data[[hoofdtermen[1]]])) |>
      pull(n) |>
      max() > 1
    if (varieert_binnen_blok) {
      covariaten <- c(covariaten, blok)
      opzet_tekst <- paste0("binnen ", blok, " (vast blok)")
    } else if (is.null(random) && n_distinct(d[[blok]]) >= 3) {
      random <- paste0("(1 | ", blok, ")")
    }
  }
  if (!is.null(random)) opzet_tekst <- paste0(opzet_tekst, "; random ", random)
  rhs <- paste(c(model_termen, covariaten), collapse = " + ")

  # Waarschuwingen (singuliere fit, convergentie) onderdrukt: bij deze kleine
  # aantallen komen ze vaak voor; de p-waarden zijn sowieso indicatief.
  if (is.null(random)) {
    fit <- tryCatch(lm(as.formula(paste(respons, "~", rhs)), data = d), error = \(e) NULL)
    tab <- if (!is.null(fit)) tryCatch(car::Anova(fit, type = 2), error = \(e) NULL)
    coefs <- if (!is.null(fit)) coef(fit)
  } else {
    fit <- tryCatch(
      suppressWarnings(suppressMessages(
        lmerTest::lmer(as.formula(paste(respons, "~", rhs, "+", random)), data = d)
      )),
      error = \(e) NULL
    )
    tab <- if (!is.null(fit)) tryCatch(anova(fit), error = \(e) NULL)
    coefs <- if (!is.null(fit)) lme4::fixef(fit)
  }
  if (is.null(tab)) {
    return(mutate(uitkomst, opzet = opzet_tekst, opmerking = "model kon niet gefit worden"))
  }

  uitkomst |>
    mutate(
      opzet = opzet_tekst,
      schatting = map_dbl(term, \(t) {
        patroon <- paste0("^", paste(str_split(t, ":")[[1]], collapse = "[^:]*:"), "[^:]*$")
        treffers <- coefs[str_detect(names(coefs), patroon)]
        if (length(treffers) == 1) unname(treffers) else NA_real_
      }),
      p_waarde = map_dbl(term, \(t) if (t %in% rownames(tab)) tab[t, "Pr(>F)"] else NA_real_)
    )
}

#' Pas toets_model() toe op elke variabele in een lang data-object
#'
#' @param d_lang Data in lang formaat met de kolommen thema, naam, label en kolom.
#' @param ... Argumenten voor toets_model().
#' @return Tibble met één rij per variabele x toets_term.
toets_per_variabele <- function(d_lang, ...) {
  d_lang |>
    group_by(thema, naam, label, kolom) |>
    group_modify(\(d, sleutel) toets_model(d, ...)) |>
    ungroup()
}


# Sla een tabel op naast de figuren (puntkomma + decimale komma voor Excel NL)
sla_tabel_op <- function(tabel, naam) {
  # Ook een lege tabel wegschrijven (alleen kolomnamen): anders blijft een
  # oude versie staan en leest het rapport verouderde resultaten in (bv.
  # tabel_significante_contrasten als er geen significante contrasten meer zijn).
  fwrite(tabel, paste0(figuren_dir, naam, ".csv"), sep = ";", dec = ",")
}

# 12. Variatie tussen behandelingen op drie niveaus -----------------------------
# 12a. Variantiedecompositie: hoeveel van de variatie in de verandering
# (laatste - eerste meetjaar) verklaart het basisbeheer (R / M / R-AF / M-AF),
# hoeveel zit in een behandelingseffect dat per gebied verschilt, hoeveel
# tussen gebieden, tussen sloten binnen een gebied en tussen locaties binnen
# een sloot? Model per variabele:
#   verandering ~ behandeling_basis + (1 | gebied) + (1 | gebied:behandeling_basis)
#                 + (1 | sloot_nummer)
# Het aandeel "behandeling" is de variantie van de vaste effecten (zoals in de
# marginale R² van Nakagawa & Schielzeth). "behandeling × gebied" is de
# variatie in het behandelingseffect tussen gebieden: per gebied wijkt het
# effect binnen dat gebied af van het gemiddelde effect (behandeling genest in
# gebied). "sloot" is de variatie tussen sloten binnen hetzelfde gebied
# (sloot_nummer bevat de gebiedscode, dus sloot is genest in gebied; gebied +
# sloot samen is de variatie tussen alle sloten). "locatie binnen sloot
# (rest)" is wat overblijft: een behandelingseffect dat per sloot in grootte of
# richting verschilt (behandeling × sloot), omstandigheden die per vak
# verschillen en los van de behandeling de verandering beïnvloeden (bv.
# ligging, een drinkplek of kreeft) en meetruis. Die zijn niet te scheiden: in
# de selectie heeft elke sloot één vak per basisbeheer en is er één meting per
# vak per jaar.

niveaus_variantie <- c(
  "behandeling (basisbeheer)", "behandeling × gebied", "gebied", "sloot",
  "locatie binnen sloot (rest)"
)
# Weergavenamen in de figuur; de niveaunamen hierboven blijven de sleutel in
# de tabellen en in het rapport.
niveaus_variantie_label <- setNames(
  c(
    "behandeling (gemiddeld effect, alle gebieden)",
    "behandeling × gebied (afwijking van het effect binnen een gebied)", "gebied",
    "sloot (binnen gebied)", "rest: binnen sloot (behandeling × sloot, vak en meetruis)"
  ),
  niveaus_variantie
)
kleuren_niveau <- setNames(
  c(okabe_ito[6], colorspace::lighten(okabe_ito[6], 0.5), okabe_ito[1], okabe_ito[3], "grey75"),
  niveaus_variantie
)

#' Variantiedecompositie van de verandering over behandeling, gebied, sloot en locatie
#'
#' @param d Data voor één variabele met verandering, behandeling_basis,
#'   gebied en sloot_nummer.
#' @return Tibble met variantie en aandeel per niveau (leeg bij te weinig data).
variantie_niveaus <- function(d) {
  d <- droplevels(filter(d, !is.na(verandering)))
  if (nrow(d) < 12 || n_distinct(d$gebied) < 3) return(tibble())
  fit <- tryCatch(
    suppressWarnings(suppressMessages(lme4::lmer(
      verandering ~ behandeling_basis + (1 | gebied) + (1 | gebied:behandeling_basis) + (1 | sloot_nummer),
      data = d
    ))),
    error = \(e) NULL
  )
  if (is.null(fit)) return(tibble())
  vc <- as.data.frame(lme4::VarCorr(fit))
  vc_groep <- \(g) sum(vc$vcov[vc$grp == g])
  tibble(
    niveau = niveaus_variantie,
    variantie = c(
      var(as.vector(lme4::getME(fit, "X") %*% lme4::fixef(fit))),
      vc_groep("gebied:behandeling_basis"), vc_groep("gebied"),
      vc_groep("sloot_nummer"), vc_groep("Residual")
    )
  ) |>
    mutate(
      aandeel = variantie / sum(variantie), n_locaties = n_distinct(d$SlootID),
      n_sloten = n_distinct(d$sloot_nummer), n_gebieden = n_distinct(d$gebied)
    )
}

variantie_decompositie <- wp2_verandering |>
  group_by(thema, naam, label, kolom) |>
  group_modify(\(x, sleutel) variantie_niveaus(x)) |>
  ungroup() |>
  mutate(niveau = factor(niveau, levels = niveaus_variantie))

print(variantie_decompositie |> select(naam, niveau, aandeel) |>
        pivot_wider(names_from = niveau, values_from = aandeel), n = 60)

# Toelichtingstabel onder de figuur: wat elk niveau meet en hoe je een groot
# aandeel leest. De eerste kolom krijgt de kleur van het niveau in de balken.
toelichting_niveaus <- tibble(
  Niveau = str_wrap(unname(niveaus_variantie_label), 28),
  `Wat het meet` = c(
    "Gemiddeld verschil tussen R, M en M-AF over alle sloten en gebieden",
    "Hoeveel het behandelingseffect binnen een gebied afwijkt van het gemiddelde effect",
    "Verschil in gemiddelde verandering tussen gebieden (alle behandelingen samen)",
    "Verschil in gemiddelde verandering tussen sloten in hetzelfde gebied (gebied + sloot = verschil tussen alle sloten)",
    "Wat overblijft: verschil tussen de vakken van één sloot dat niet door het gemiddelde behandelingseffect of het gebied wordt verklaard"
  ),
  `Groot aandeel betekent` = c(
    "Beheer heeft een consistent effect: overal dezelfde kant op",
    "Effect van beheer hangt af van het gebied (bv. in het ene gebied toename, in het andere afname)",
    "Alle locaties in een gebied veranderen tussen de meetronden dezelfde kant op (werkelijke verandering of meetomstandigheden zoals meetdag en peil)",
    "De hele sloot verandert samen (bv. baggeren, of slootkenmerken die de snelheid van verandering bepalen)",
    "Behandelingseffect verschilt per sloot in grootte of richting, of een vak verandert anders door eigen omstandigheden (bv. drinkplek, ligging) of meetruis; met één vak per behandeling per sloot niet te scheiden"
  )
) |>
  mutate(across(-Niveau, \(x) str_wrap(x, 62)))

tabel_toelichting <- gridExtra::tableGrob(
  toelichting_niveaus,
  rows = NULL,
  theme = gridExtra::ttheme_minimal(
    base_size = 9,
    core = list(
      fg_params = list(hjust = 0, x = 0.02),
      bg_params = list(
        fill = c(colorspace::lighten(kleuren_niveau, 0.4), rep("white", 2 * length(kleuren_niveau))),
        col = "grey85"
      )
    ),
    colhead = list(fg_params = list(hjust = 0, x = 0.02, fontface = "bold"))
  )
)
# Kolommen over de volle figuurbreedte, zodat de tekst op 1 regel past; de
# tabel krijgt onder de grafiek precies zijn eigen (natuurlijke) hoogte.
tabel_toelichting$widths <- unit(c(0.2, 0.37, 0.42), "npc")

# Behandeling links in de balk (position_stack(reverse = TRUE)); per thema
# gesorteerd op het aandeel van de behandeling, van groot (boven) naar klein
# (onder), bij gelijke stand op behandeling × gebied.
# Het aantal sloten en gebieden met data staat achter de parameternaam.
variantie_fig <- variantie_decompositie |>
  mutate(label_n = paste0(label, "\n", n_locaties, " vakken, ", n_sloten, " sloten, ", n_gebieden, " gebieden"))

volgorde_variantie <- variantie_fig |>
  select(thema, label_n, niveau, aandeel) |>
  pivot_wider(names_from = niveau, values_from = aandeel) |>
  arrange(thema, `behandeling (basisbeheer)`, `behandeling × gebied`) |>
  pull(label_n)

plot_variantie_balken <- variantie_fig |>
  mutate(label_n = factor(label_n, levels = unique(volgorde_variantie))) |>
  ggplot(aes(x = aandeel, y = label_n, fill = niveau)) +
  geom_col(width = 0.75, position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = kleuren_niveau, labels = niveaus_variantie_label) +
  scale_x_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.01))) +
  facet_grid(
    thema ~ ., scales = "free_y", space = "free_y",
    labeller = as_labeller(\(x) str_replace(x, "oevermorfologie", "oever-\nmorfologie"))
  ) +
  guides(fill = guide_legend(ncol = 1)) +
  labs(
    title = "WP2 - Waar zit de variatie in de verandering (laatste − eerste meetjaar)?",
    subtitle = paste0(
      "Gemengd model per variabele: verandering ~ basisbeheer + (1 | gebied) + (1 | gebied:basisbeheer)\n",
      "+ (1 | sloot). Alle vakken uit de selectie voor de vergelijking van behandelingen;\n",
      "per thema gesorteerd op het aandeel van de behandeling (groot boven)."
    ),
    x = "Aandeel van de totale variantie", y = NULL, fill = "Niveau"
  ) +
  theme_wp2

plot_variantie <- patchwork::wrap_plots(
  plot_variantie_balken, patchwork::wrap_elements(full = tabel_toelichting),
  ncol = 1, heights = grid::unit.c(unit(1, "null"), sum(tabel_toelichting$heights) + unit(0.2, "in"))
)
print(plot_variantie)
sla_figuur_op(plot_variantie, "niveaus_variantiedecompositie", width = 11, height = 16)

# 12b. Niveau 'alles': verandering per basisbeheer over alle gebieden, met de
# p-waarde van het basisbeheer uit het gemengde model (gebied en sloot
# random).
toets_alles <- toets_per_variabele(
  wp2_verandering, model_termen = "behandeling_basis", toets_termen = "behandeling_basis",
  random = "(1 | gebied) + (1 | sloot_nummer)"
) |>
  mutate(p_bh = p.adjust(p_waarde, method = "BH"))
print(toets_alles |> select(naam, p_waarde, p_bh, n_locaties), n = 60)

# Sloten zonder waargenomen effect krijgen een zwart kruisje over het punt
# (vervagen was slecht leesbaar). De kruisjeslaag tekent alle rijen met
# dezelfde jitter-seed als de puntenlaag, zodat het kruisje precies op het
# punt valt; rijen met effect krijgen shape NA en worden niet getekend.
label_geen_effect <- "geen effect gezien bij veldbeoordeling 2026"
shapes_geen_effect <- c(vorm_shapes, setNames(4, label_geen_effect))

#' Laag met een kruisje over punten van sloten zonder waargenomen effect
#'
#' @param geen_effect Logische expressie (data-masking) die TRUE is voor
#'   sloten zonder waargenomen effect.
#' @param width Jitterbreedte; gelijk aan die van de puntenlaag.
#' @return Een ggplot2-laag.
laag_geen_effect <- function(geen_effect, width) {
  geom_point(
    aes(shape = if_else({{ geen_effect }}, label_geen_effect, NA_character_)),
    position = position_jitter(width = width, height = 0, seed = 1),
    colour = "black", size = 2.8, stroke = 0.9, na.rm = TRUE
  )
}

# Figuur per basisbeheer (R, M, M-AF) voor de hoofdtekst. Per variabele een
# gemengd model verandering ~ 0 + basisbeheer + (1 | gebied) + (1 | sloot),
# zoals toets_verandering() (sectie 4). Daaruit:
#  - links: de gemiddelde verandering per basisbeheer met 95%-BI, met de
#    beginwaarde op 0 (eerste meetjaar) en de verandering bij het laatste
#    meetjaar; het label geeft de gemiddelde absolute waarden (eerste ->
#    laatste meetjaar, over de locaties met beide jaren);
#  - rechts: het verschil in verandering tussen M en R, M-AF en M, en M-AF en
#    R met 95%-BI en p-waarde (t-toets op het contrast, Satterthwaite).
groepen_basis <- c("R", "M", "M-AF")
contrasten_basis <- list(c("M", "R"), c("M-AF", "M"), c("M-AF", "R"))

#' Gemiddelde verandering per basisbeheer en de verschillen daartussen
#'
#' @param d Data voor één variabele met verandering, waarde_eerste,
#'   waarde_laatste, behandeling_basis, gebied en sloot_nummer.
#' @return Tibble met per groep (type "groep") en per contrast (type
#'   "contrast") de schatting, 95%-BI (ondergrens, bovengrens) en p-waarde;
#'   voor de groepen ook n en de gemiddelde waarde in het eerste en laatste
#'   meetjaar. Leeg als het model niet te fitten is.
schat_basisbeheer <- function(d) {
  d <- d |>
    filter(!is.na(verandering), behandeling_basis %in% groepen_basis) |>
    mutate(groep = factor(behandeling_basis, levels = groepen_basis)) |>
    droplevels()
  if (n_distinct(d$groep) < 2 || nrow(d) < 6) return(tibble())
  random <- if (n_distinct(d$gebied) >= 3) "(1 | gebied) + (1 | sloot_nummer)" else "(1 | sloot_nummer)"
  fit <- tryCatch(
    suppressWarnings(suppressMessages(
      lmerTest::lmer(as.formula(paste("verandering ~ 0 + groep +", random)), data = d)
    )),
    error = \(e) NULL
  )
  if (is.null(fit)) return(tibble())
  coefs <- names(lme4::fixef(fit))
  toets <- function(L) {
    res <- tryCatch(lmerTest::contest1D(fit, L, confint = TRUE), error = \(e) NULL)
    if (is.null(res)) return(tibble(schatting = NA_real_, ondergrens = NA_real_, bovengrens = NA_real_, p_waarde = NA_real_))
    tibble(schatting = res$Estimate, ondergrens = res$lower, bovengrens = res$upper, p_waarde = res$`Pr(>|t|)`)
  }
  groepen <- map_dfr(levels(d$groep), \(g) {
    L <- setNames(as.numeric(coefs == paste0("groep", g)), coefs)
    bind_cols(tibble(type = "groep", naam_schatting = g), toets(L))
  }) |>
    left_join(
      d |>
        summarise(.by = groep, n = n(), eerste = mean(waarde_eerste), laatste = mean(waarde_laatste)) |>
        transmute(naam_schatting = as.character(groep), n, eerste, laatste),
      by = "naam_schatting"
    )
  contrasten <- map_dfr(keep(contrasten_basis, \(paar) all(paar %in% levels(d$groep))), \(paar) {
    L <- setNames(numeric(length(coefs)), coefs)
    L[paste0("groep", paar)] <- c(1, -1)
    bind_cols(tibble(type = "contrast", naam_schatting = paste(paar[1], "−", paar[2])), toets(L))
  })
  bind_rows(groepen, contrasten)
}

basisbeheer_schatting <- wp2_verandering |>
  group_by(thema, naam, label, kolom) |>
  group_modify(\(d, sleutel) schat_basisbeheer(d)) |>
  ungroup() |>
  mutate(p_holm = if_else(type == "contrast", p.adjust(p_waarde, method = "holm"), NA_real_), .by = kolom)
print(
  basisbeheer_schatting |> filter(type == "contrast") |> select(thema, naam, naam_schatting, schatting, p_waarde, p_holm),
  n = 100
)

# Meetjaren voor de x-as: meestal 2024 -> 2026
jaar_eerste_lab <- names(which.max(table(wp2_verandering$jaar_eerste)))
jaar_laatste_lab <- names(which.max(table(wp2_verandering$jaar_laatste)))

#' Getal met 3 significante cijfers in Nederlandse notatie (voor labels)
#'
#' @param x Numerieke vector.
#' @return Character vector.
nl_kort <- function(x) trimws(formatC(signif(x, 3), format = "fg", digits = 3, decimal.mark = ",", big.mark = "."))

#' Figuurpaar voor één variabele: verandering per basisbeheer en verschillen
#'
#' @param s Rijen uit basisbeheer_schatting voor één variabele.
#' @return patchwork-object met links de verandering t.o.v. het eerste
#'   meetjaar per basisbeheer en rechts de verschillen tussen basisbeheer.
plot_basisbeheer_variabele <- function(s) {
  g <- s |>
    filter(type == "groep") |>
    mutate(
      groep = factor(naam_schatting, levels = groepen_basis),
      x = 2 + (as.integer(groep) - 2) * 0.12,
      label = paste0(groep, " (n = ", n, "): ", nl_kort(eerste), " → ", nl_kort(laatste))
    )
  # Labels in de volgorde van de schatting, gelijkmatig verdeeld over de
  # hoogte van het paneel, zodat ze niet overlappen
  bereik <- range(c(0, g$ondergrens, g$bovengrens), na.rm = TRUE)
  g <- g |>
    mutate(y_label = seq(bereik[1], bereik[2], length.out = n() + 2)[rank(schatting, ties.method = "first") + 1])
  lijnen <- bind_rows(mutate(g, xx = x - 1, yy = 0), mutate(g, xx = x, yy = schatting))
  titel <- paste0(s$label[1], "\n", richting_label(s$naam[1]))
  links <- ggplot(g, aes(colour = groep)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.4) +
    geom_line(data = lijnen, aes(x = xx, y = yy, group = groep), linewidth = 0.6) +
    geom_point(aes(x = x - 1, y = 0), size = 2) +
    geom_errorbar(aes(x = x, ymin = ondergrens, ymax = bovengrens), width = 0.08, linewidth = 0.6) +
    geom_point(aes(x = x, y = schatting), size = 2.6) +
    geom_text(aes(x = 2.25, y = y_label, label = label), hjust = 0, size = 3, show.legend = FALSE) +
    scale_colour_manual(values = kleuren_basis[groepen_basis], drop = FALSE) +
    scale_x_continuous(breaks = 1:2, labels = c(jaar_eerste_lab, jaar_laatste_lab), limits = c(0.8, 3.6)) +
    labs(title = titel, x = NULL, y = "Verandering t.o.v. eerste meetjaar") +
    theme_wp2 +
    theme(
      plot.title = element_text(size = 10, face = "bold", hjust = 0), legend.position = "none",
      panel.grid.minor = element_blank()
    )
  k <- s |>
    filter(type == "contrast") |>
    mutate(
      contrast = factor(naam_schatting, levels = rev(map_chr(contrasten_basis, \(p) paste(p[1], "−", p[2])))),
      p_tekst = paste0("p = ", p_label(p_waarde)),
      sign = !is.na(p_waarde) & p_waarde < 0.05
    )
  rechts <- ggplot(k, aes(y = contrast)) +
    geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.4) +
    geom_errorbarh(aes(xmin = ondergrens, xmax = bovengrens), height = 0.2, linewidth = 0.6) +
    geom_point(aes(x = schatting, shape = sign), size = 2.6, fill = "white") +
    geom_text(aes(x = Inf, label = p_tekst, fontface = if_else(sign, "bold", "plain")), hjust = 1.05, size = 3) +
    scale_shape_manual(values = c(`FALSE` = 21, `TRUE` = 16), guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.35))) +
    labs(title = " \n ", x = "Verschil in verandering", y = NULL) +
    theme_wp2 +
    theme(plot.title = element_text(size = 10), panel.grid.minor = element_blank())
  links + rechts + patchwork::plot_layout(widths = c(1.5, 1))
}

#' Figuur per thema: verandering per basisbeheer, alle variabelen
#'
#' @param thema_sel Naam van het thema (niveau van var_lijst$thema).
#' @return patchwork-object.
plot_niveau_alles <- function(thema_sel) {
  s_thema <- basisbeheer_schatting |> filter(thema == thema_sel, !is.na(schatting))
  kolommen <- intersect(var_lijst$kolom[var_lijst$thema == thema_sel], s_thema$kolom)
  rijen <- map(kolommen, \(k) plot_basisbeheer_variabele(filter(s_thema, kolom == k)))
  patchwork::wrap_plots(rijen, ncol = if (length(rijen) > 3) 2 else 1) +
    patchwork::plot_annotation(
      title = paste0("WP2 - ", thema_sel, ": verandering per basisbeheer, alle gebieden"),
      subtitle = paste0(
        "Links: gemiddelde verandering t.o.v. ", jaar_eerste_lab, " (op 0) met 95%-BI per basisbeheer; label = gemiddelde waarde ",
        jaar_eerste_lab, " → ", jaar_laatste_lab, ".\n",
        "Rechts: verschil in verandering tussen basisbeheer met 95%-BI; dicht punt en vet = p < 0,05 (zonder correctie). ",
        "Gemengd model met gebied en sloot als random effect."
      ),
      caption = "R = regulier (incl. R-KR), M = minimaal, M-AF = minimaal met afrastering (incl. M-AF-KR).",
      theme = theme(plot.title = element_text(face = "bold", hjust = 0.5))
    )
}

# 12c. Niveau 'gebied': dezelfde vergelijking per gebied (kolommen).
plot_niveau_gebied <- function(thema_sel) {
  d <- wp2_verandering |> filter(thema == thema_sel)
  ggplot(d, aes(x = behandeling_basis, y = verandering)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.4) +
    geom_point(
      aes(colour = sloot_nummer, shape = behandeling_vorm),
      position = position_jitter(width = 0.12, height = 0, seed = 1), size = 2
    ) +
    laag_geen_effect(effect_categorie == "geen effect", width = 0.12) +
    stat_summary(fun = median, geom = "point", shape = 18, size = 3.5, colour = "black") +
    scale_colour_manual(values = sloot_kleuren) +
    scale_shape_manual(values = shapes_geen_effect, na.translate = FALSE) +
    facet_grid(label_facet_smal ~ gebied, scales = "free", space = "free_x") +
    labs(
      title = paste0("WP2 - ", thema_sel, ": verandering per basisbeheer, per gebied"),
      subtitle = paste0(
        "Zwarte ruit = mediaan per gebied en basisbeheer. × = sloot waar bij de veldbeoordeling in 2026 geen effect is gezien\n",
        "(alleen ter duiding; deze sloten tellen mee in de mediaan en de toetsen)."
      ),
      x = "Basisbeheer", y = "Verandering (laatste − eerste meetjaar)",
      colour = "Sloot", shape = "Toevoeging"
    ) +
    theme_wp2 +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.text.y = element_text(angle = 0))
}

# 12d. Niveau 'sloot': alle behandelingen langs dezelfde sloot naast elkaar;
# de grijze lijn geeft per sloot het bereik tussen de behandelingen. Achter
# de slootnaam staat de effectcategorie: (-) geen effect, (M) alleen
# morfologie, (D) alleen draagkracht.
sloot_volgorde <- wp2_verandering |>
  distinct(gebied, sloot_nummer, effect_categorie) |>
  mutate(
    nr = as.numeric(str_extract(sloot_nummer, "[0-9.]+$")),
    sloot_label = paste0(sloot_nummer, case_when(
      effect_categorie == "geen effect" ~ " (-)",
      effect_categorie == "alleen morfologie" ~ " (M)",
      effect_categorie == "alleen draagkracht" ~ " (D)",
      TRUE ~ ""
    ))
  ) |>
  arrange(gebied, nr)
wp2_verandering <- wp2_verandering |>
  left_join(select(sloot_volgorde, sloot_nummer, sloot_label), by = "sloot_nummer") |>
  mutate(
    sloot_label = factor(sloot_label, levels = sloot_volgorde$sloot_label),
    # Facet-labels met de betekenis van + en −; smal voor facetten aan de zijkant
    label_facet = met_richting(label),
    label_facet_smal = met_richting(label, 30)
  )

kleuren_basis <- c("R" = okabe_ito[1], "M" = okabe_ito[3], "R-AF" = okabe_ito[6], "M-AF" = okabe_ito[5])

plot_niveau_sloot <- function(thema_sel) {
  d <- wp2_verandering |> filter(thema == thema_sel)
  bereik <- d |> summarise(.by = c(label_facet, sloot_label), ymin = min(verandering), ymax = max(verandering))
  ggplot(d, aes(x = sloot_label, y = verandering)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.4) +
    geom_linerange(data = bereik, aes(x = sloot_label, ymin = ymin, ymax = ymax),
                   inherit.aes = FALSE, colour = "grey60", linewidth = 0.8) +
    geom_point(aes(colour = behandeling_basis, shape = behandeling_vorm), size = 2.3) +
    scale_colour_manual(values = kleuren_basis) +
    scale_shape_manual(values = vorm_shapes) +
    facet_wrap(~label_facet, ncol = 1, scales = "free_y") +
    labs(
      title = paste0("WP2 - ", thema_sel, ": verschil tussen behandelingen binnen dezelfde sloot"),
      subtitle = paste0(
        "Grijze lijn = bereik tussen de locaties van één sloot. Achter de slootnaam de veldbeoordeling in 2026 (alleen ter duiding):\n",
        "(-) geen effect gezien, (M) alleen effect op morfologie, (D) alleen effect op draagkracht."
      ),
      x = "Sloot", y = "Verandering (laatste − eerste meetjaar)",
      colour = "Basisbeheer", shape = "Toevoeging"
    ) +
    theme_wp2 +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}

for (thema_sel in levels(var_lijst$thema)) {
  n_var <- sum(var_lijst$thema == thema_sel)
  p_alles <- plot_niveau_alles(thema_sel)
  p_gebied <- plot_niveau_gebied(thema_sel)
  p_sloot <- plot_niveau_sloot(thema_sel)
  print(p_alles); print(p_gebied); print(p_sloot)
  # zelfde indeling als in plot_niveau_alles(): twee kolommen bij meer dan 3 variabelen
  n_kol <- if (n_var > 3) 2 else 1
  sla_figuur_op(p_alles, paste0("niveau_alles_", thema_sel), width = 9.5 * n_kol, height = 2.8 * ceiling(n_var / n_kol) + 1.6)
  sla_figuur_op(p_gebied, paste0("niveau_gebied_", thema_sel), width = 14, height = 2.6 * n_var + 2)
  sla_figuur_op(p_sloot, paste0("niveau_sloot_", thema_sel), width = 14, height = 2.6 * n_var + 2)
}

# Variatie binnen een sloot in tabelvorm: per sloot en variabele het bereik
# en de standaarddeviatie van de verandering tussen de behandelingen.
variatie_binnen_sloot <- wp2_verandering |>
  summarise(
    .by = c(thema, naam, gebied, sloot_nummer, effect_categorie),
    n_behandelingen = n_distinct(Behandeling),
    behandelingen = paste(sort(unique(Behandeling)), collapse = "/"),
    bereik = max(verandering) - min(verandering),
    sd = sd(verandering)
  ) |>
  arrange(thema, naam, gebied, sloot_nummer)

# 12e. Verklaren bodem, beheer of meetomstandigheden het slootaandeel? ----------
# In 12a valt een groot deel van de variatie in de verandering van de
# nutriënten in oever en slib op het niveau sloot. Hier is nagegaan of
# bodemeigenschappen (beginwaarde in de oever), beheer (beheerenquête 2024,
# één waarde per sloot/perceel) of meetomstandigheden (verandering in water-
# en slibtemperatuur en waterdiepte tussen de meetdagen) dat aandeel
# verkleinen. Per respons en covariaat: het model uit 12a met en zonder de
# gestandaardiseerde covariaat, op dezelfde locaties. Een covariaat die
# verschillen tussen sloten verklaart, verkleint de slootvariantie.
# Let op: met ~55 locaties in ~24 sloten is de slootvariantie onzeker; lees de
# afname als indicatie. Beheer varieert alleen in RH en SW tussen sloten.
covariaten_sloot <- tribble(
  ~covariaat,                       ~kolom,                                  ~type,         ~groep,
  "organische stof oever (begin)",  "OS_perc_OR_25",                         "begin",       "bodem",
  "Fe:P oever, XRF (begin)",        "feP_XRF_OR_25",                         "begin",       "bodem",
  "Fe-CaCl2 oever (begin)",         "Fe_CC_mg/kg_OR_25",                     "begin",       "bodem",
  "klei oever (begin)",             "Z_CLAY_SA_OR_25",                       "begin",       "bodem",
  "koeien per perceel per dag",     "Aantal_koeien_vee_perceel_dag",         "begin",       "beheer",
  "breedte bemestingsvrije zone",   "Breedte_bemestingsvrije_zone_huidig_m", "begin",       "beheer",
  "koebelasting drinkende koeien",  "koebelasting_drinkende_koeien",         "begin",       "beheer",
  "verandering watertemperatuur",   "watertemp_C",                           "verandering", "meetomstandigheden",
  "verandering slibtemperatuur",    "slib_temp",                             "verandering", "meetomstandigheden",
  "verandering waterdiepte",        "max_wtd",                               "verandering", "meetomstandigheden"
) |>
  filter(kolom %in% names(wp2))

responsen_sloot <- c(
  "P-AL mg p2o5/100g_OR_25", "N_mineraal_OR_25",
  "P-AL mg p2o5/100g_SB", "NH4_µmol/l_PW", "slib_redox_pH7"
)

# Covariaat per locatie: beginwaarde (eerste meetjaar met een waarde) of
# verandering (laatste - eerste meetjaar).
covariaat_waarden <- wp2 |>
  as_tibble() |>
  select(SlootID, jaar, all_of(covariaten_sloot$kolom)) |>
  mutate(across(-c(SlootID, jaar), \(x) suppressWarnings(as.numeric(x)))) |>
  pivot_longer(-c(SlootID, jaar), names_to = "kolom", values_to = "waarde") |>
  filter(!is.na(waarde)) |>
  arrange(jaar) |>
  summarise(
    .by = c(SlootID, kolom),
    begin = first(waarde),
    verandering = if (n_distinct(jaar) > 1) last(waarde) - first(waarde) else NA_real_
  ) |>
  inner_join(covariaten_sloot, by = "kolom") |>
  mutate(cov_waarde = if_else(type == "begin", begin, verandering)) |>
  select(SlootID, covariaat, groep, cov_waarde)

#' Slootvariantie in de verandering met en zonder covariaat
#'
#' Past het model uit 12a (verandering ~ basisbeheer + (1 | gebied) +
#' (1 | gebied:basisbeheer) + (1 | sloot)) met en zonder een
#' gestandaardiseerde covariaat, op dezelfde locaties.
#'
#' @param d Data voor één respons en één covariaat met verandering,
#'   cov_waarde, behandeling_basis, gebied en sloot_nummer.
#' @return Tibble met de variantie van sloot, gebied en rest zonder en met
#'   covariaat, de relatieve afname van de slootvariantie en de schatting en
#'   p-waarde van de covariaat (per standaarddeviatie); leeg bij te weinig data.
sloot_variantie_covariaat <- function(d) {
  d <- droplevels(filter(d, !is.na(verandering), !is.na(cov_waarde)))
  if (nrow(d) < 12 || n_distinct(d$gebied) < 3 || sd(d$cov_waarde) == 0) return(tibble())
  d <- mutate(d, cov_z = as.numeric(scale(cov_waarde)))
  random <- "(1 | gebied) + (1 | gebied:behandeling_basis) + (1 | sloot_nummer)"
  pas_toe <- \(rhs) tryCatch(
    suppressWarnings(suppressMessages(lmerTest::lmer(as.formula(paste("verandering ~", rhs, "+", random)), data = d))),
    error = \(e) NULL
  )
  fit_zonder <- pas_toe("behandeling_basis")
  fit_met <- pas_toe("behandeling_basis + cov_z")
  if (is.null(fit_zonder) || is.null(fit_met)) return(tibble())
  vc <- \(fit, g) {
    v <- as.data.frame(lme4::VarCorr(fit))
    sum(v$vcov[v$grp == g])
  }
  coefs <- summary(fit_met)$coefficients
  tibble(
    n_locaties = nrow(d), n_sloten = n_distinct(d$sloot_nummer),
    var_sloot_zonder = vc(fit_zonder, "sloot_nummer"), var_sloot_met = vc(fit_met, "sloot_nummer"),
    var_gebied_zonder = vc(fit_zonder, "gebied"), var_gebied_met = vc(fit_met, "gebied"),
    var_rest_zonder = vc(fit_zonder, "Residual"), var_rest_met = vc(fit_met, "Residual"),
    schatting = coefs["cov_z", "Estimate"], p_waarde = coefs["cov_z", "Pr(>|t|)"]
  ) |>
    mutate(afname_sloot = if_else(var_sloot_zonder > 0, 1 - var_sloot_met / var_sloot_zonder, NA_real_))
}

resultaten_sloot_covariaat <- wp2_verandering |>
  filter(kolom %in% responsen_sloot) |>
  select(SlootID, thema, naam, label, gebied, sloot_nummer, behandeling_basis, verandering) |>
  inner_join(covariaat_waarden, by = "SlootID", relationship = "many-to-many") |>
  group_by(thema, naam, label, groep, covariaat) |>
  group_modify(\(x, sleutel) sloot_variantie_covariaat(x)) |>
  ungroup() |>
  mutate(p_bh = p.adjust(p_waarde, method = "BH"))

print(
  resultaten_sloot_covariaat |>
    select(naam, covariaat, n_locaties, afname_sloot, schatting, p_waarde, p_bh) |>
    mutate(across(c(afname_sloot, p_waarde, p_bh), \(x) round(x, 3))),
  n = 60
)

plot_sloot_covariaat <- resultaten_sloot_covariaat |>
  filter(!is.na(afname_sloot)) |>
  mutate(
    covariaat = factor(covariaat, levels = rev(covariaten_sloot$covariaat)),
    groep = factor(groep, levels = c("bodem", "beheer", "meetomstandigheden")),
    significantie = if_else(p_waarde < 0.05, "p < 0,05 (zonder correctie)", "p ≥ 0,05")
  ) |>
  ggplot(aes(x = afname_sloot, y = covariaat, colour = groep, shape = significantie)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_point(size = 2.8, stroke = 1) +
  scale_colour_manual(values = c(bodem = okabe_ito[1], beheer = okabe_ito[3], meetomstandigheden = okabe_ito[5])) +
  scale_shape_manual(values = c("p < 0,05 (zonder correctie)" = 16, "p ≥ 0,05" = 1)) +
  scale_x_continuous(labels = scales::percent) +
  facet_wrap(~label, ncol = 3) +
  labs(
    title = "WP2 - Verklaren bodem, beheer of meetomstandigheden de verschillen tussen sloten?",
    subtitle = paste0(
      "Hoeveel kleiner de variatie tussen sloten in de verandering (2024 -> 2026) wordt als de variabele aan het model van de variantiefiguur wordt toegevoegd.\n",
      "Bodem: beginwaarde oever (0-25 cm); beheer: beheerenquête 2024; meetomstandigheden: verandering tussen de meetdagen."
    ),
    x = "Afname variatie tussen sloten (negatief = geen afname)", y = NULL, colour = "Soort variabele", shape = NULL
  ) +
  theme_wp2
print(plot_sloot_covariaat)
sla_figuur_op(plot_sloot_covariaat, "sloot_covariaten_variantie", width = 14, height = 8)

# 12f. Meetomstandigheden: temperatuur, waterdiepte en redox ----------------------
# Elk gebied wordt per jaar in 1-4 dagen bemeten (een meetronde). Verschillen
# tussen gebieden en jaren kunnen dus ook door het weer op de meetdag komen.
#  - Variantiedecompositie van de niveaus van water- en slibtemperatuur,
#    waterdiepte en redox over jaar, gebied, meetronde (gebied x jaar), sloot
#    en locatie.
#  - Samenhang tussen temperatuur en redox van het slib op drie schalen:
#    binnen een locatie tussen jaren, binnen een meetronde tussen locaties en
#    tussen de gemiddelden per meetronde. Bij hogere temperatuur is meer
#    afbraak en zuurstofverbruik te verwachten, dus een lagere redox.
#  - Samenhang tussen redox en P in het slib (P-AL) en in het poriewater: bij
#    gereduceerde omstandigheden kan fosfaat vrijkomen.
meet_vars <- tribble(
  ~naam,               ~kolom,
  "watertemperatuur",  "watertemp_C",
  "slibtemperatuur",   "slib_temp",
  "waterdiepte",       "max_wtd",
  "redox slib (pH 7)", "slib_redox_pH7"
) |>
  filter(kolom %in% names(wp2))

meet_data <- wp2 |>
  as_tibble() |>
  select(SlootID, gebied, sloot_nummer, jaar, datum, all_of(meet_vars$kolom),
         any_of(c("P-AL mg p2o5/100g_SB", "P_µmol/l_PW"))) |>
  mutate(
    across(-c(SlootID, gebied, sloot_nummer, jaar, datum), \(x) suppressWarnings(as.numeric(x))),
    meetronde = paste(gebied, jaar)
  )

#' Variantiedecompositie van een niveau over jaar, gebied, meetronde, sloot en locatie
#'
#' @param d Data met y (gemeten waarde), jaar, gebied, meetronde
#'   (gebied x jaar), sloot_nummer en SlootID.
#' @return Tibble met variantie en aandeel per niveau (leeg als het model niet
#'   past).
variantie_niveau_meetronde <- function(d) {
  d <- mutate(filter(d, !is.na(y)), jaar = factor(jaar))
  fit <- tryCatch(
    suppressWarnings(suppressMessages(lme4::lmer(
      y ~ (1 | jaar) + (1 | gebied) + (1 | meetronde) + (1 | sloot_nummer) + (1 | SlootID),
      data = d
    ))),
    error = \(e) NULL
  )
  if (is.null(fit)) return(tibble())
  as.data.frame(lme4::VarCorr(fit)) |>
    transmute(
      niveau = dplyr::recode(grp, meetronde = "gebied × jaar (meetronde)", sloot_nummer = "sloot", SlootID = "locatie", Residual = "rest"),
      variantie = vcov
    ) |>
    mutate(n = nrow(d), aandeel = variantie / sum(variantie))
}

meet_variantie <- map_dfr(seq_len(nrow(meet_vars)), \(i) {
  meet_data |>
    transmute(y = .data[[meet_vars$kolom[i]]], jaar, gebied, meetronde, sloot_nummer, SlootID) |>
    variantie_niveau_meetronde() |>
    mutate(naam = meet_vars$naam[i], .before = 1)
})
print(meet_variantie |> select(naam, niveau, aandeel) |> pivot_wider(names_from = niveau, values_from = aandeel))

# Het gebiedsaandeel in de verandering (12a) is per definitie variatie tussen
# meetronden: vaste verschillen tussen gebieden vallen in een verandering weg.
# Ter controle: de niveaus van alle parameters uit 12a opgedeeld in een vast
# gebiedsverschil en een verschil per meetronde, naast het gebiedsaandeel in
# de verandering.
gebied_verandering_niveau <- map_dfr(seq_len(nrow(var_lijst)), \(i) {
  wp2 |>
    as_tibble() |>
    transmute(
      y = suppressWarnings(as.numeric(.data[[var_lijst$kolom[i]]])),
      jaar, gebied, meetronde = paste(gebied, jaar), sloot_nummer, SlootID
    ) |>
    variantie_niveau_meetronde() |>
    mutate(naam = var_lijst$naam[i], .before = 1)
}) |>
  select(naam, niveau, aandeel) |>
  pivot_wider(names_from = niveau, values_from = aandeel, names_prefix = "niveau_") |>
  left_join(
    variantie_decompositie |>
      filter(niveau == "gebied") |>
      select(naam, label, verandering_gebied = aandeel),
    by = "naam"
  ) |>
  arrange(desc(verandering_gebied))
print(gebied_verandering_niveau, n = 30)

#' Samenhang tussen twee variabelen op drie schalen
#'
#' @param d Data met de kolommen y (respons), x (verklarende variabele),
#'   SlootID en meetronde.
#' @return Tibble met per schaal de helling (eenheid y per eenheid x) en
#'   p-waarde, of voor de schaal tussen meetronden de Spearman-correlatie en
#'   p-waarde, plus het aantal waarnemingen.
helling_op_schalen <- function(d) {
  d <- filter(d, !is.na(y), !is.na(x))
  binnen <- \(f, schaal) {
    fit <- lm(f, data = d)
    co <- summary(fit)$coefficients
    tibble(schaal = schaal, helling = co["x", "Estimate"], rho = NA_real_, p_waarde = co["x", "Pr(>|t|)"], n = nobs(fit))
  }
  tussen <- summarise(d, .by = meetronde, y = mean(y), x = mean(x))
  ct <- suppressWarnings(cor.test(tussen$x, tussen$y, method = "spearman", exact = FALSE))
  bind_rows(
    binnen(y ~ x + factor(SlootID), "binnen locatie, tussen jaren"),
    binnen(y ~ x + factor(meetronde), "binnen meetronde, tussen locaties"),
    tibble(schaal = "tussen meetronden", helling = NA_real_, rho = unname(ct$estimate), p_waarde = ct$p.value, n = nrow(tussen))
  )
}

paren_meet <- tribble(
  ~respons,            ~verklarend,         ~respons_kolom,         ~verklarend_kolom,
  "redox slib (pH 7)", "watertemperatuur",  "slib_redox_pH7",       "watertemp_C",
  "redox slib (pH 7)", "slibtemperatuur",   "slib_redox_pH7",       "slib_temp",
  "P-AL slib",         "redox slib (pH 7)", "P-AL mg p2o5/100g_SB", "slib_redox_pH7",
  "P poriewater",      "redox slib (pH 7)", "P_µmol/l_PW",          "slib_redox_pH7"
) |>
  filter(respons_kolom %in% names(meet_data), verklarend_kolom %in% names(meet_data))

redox_temperatuur <- pmap_dfr(paren_meet, \(respons, verklarend, respons_kolom, verklarend_kolom) {
  d <- transmute(meet_data, y = .data[[respons_kolom]], x = .data[[verklarend_kolom]], SlootID, meetronde)
  helling_op_schalen(d) |>
    mutate(
      respons = respons, verklarend = verklarend,
      rho_alle = cor(d$x, d$y, method = "spearman", use = "pairwise.complete.obs"),
      .before = 1
    )
})
print(redox_temperatuur)

# Gemiddelden per meetronde, met het aantal meetdagen
meetronden <- meet_data |>
  summarise(
    .by = c(gebied, jaar),
    n_locaties = n_distinct(SlootID),
    n_meetdagen = n_distinct(datum, na.rm = TRUE),
    across(all_of(meet_vars$kolom), \(x) mean(x, na.rm = TRUE))
  ) |>
  arrange(gebied, jaar)
print(meetronden)

# Sloten waarvan de vakken in een jaar op verschillende dagen zijn bemeten.
# Binnen een sloot vallen meetdageffecten in de vergelijking van behandelingen
# weg, behalve als de vakken op verschillende dagen zijn bemeten.
meetdagen_sloot <- meet_data |>
  filter(!is.na(datum)) |>
  summarise(
    .by = c(sloot_nummer, jaar),
    n_dagen = n_distinct(as.Date(datum)),
    spreiding_dagen = as.numeric(max(as.Date(datum)) - min(as.Date(datum)))
  ) |>
  arrange(desc(spreiding_dagen))
print(filter(meetdagen_sloot, n_dagen > 1))

# Opvallende redoxwaarden: afwijkend ten opzichte van de andere vakken in
# dezelfde sloot en hetzelfde jaar (zelfde meetdag), of een grote sprong ten
# opzichte van hetzelfde vak in het vorige meetjaar. Drempel: de 5% grootste
# afwijkingen resp. sprongen. In de veldnotities is voor deze waarden geen
# verklaring gevonden; de tabel dient voor de interpretatie.
redox_vak <- meet_data |>
  filter(!is.na(slib_redox_pH7)) |>
  select(SlootID, gebied, sloot_nummer, jaar, redox = slib_redox_pH7, watertemp_C) |>
  mutate(
    .by = c(sloot_nummer, jaar),
    n_vakken = n(),
    mediaan_andere = vapply(seq_len(n()), \(i) if (n() > 1) median(redox[-i]) else NA_real_, numeric(1))
  ) |>
  arrange(SlootID, jaar) |>
  mutate(.by = SlootID, vorig_jaar = lag(jaar), vorige_redox = lag(redox)) |>
  mutate(afwijking_sloot = redox - mediaan_andere, sprong = redox - vorige_redox)
drempel_afwijking <- quantile(abs(redox_vak$afwijking_sloot), 0.95, na.rm = TRUE)
drempel_sprong <- quantile(abs(redox_vak$sprong), 0.95, na.rm = TRUE)
redox_opvallend <- redox_vak |>
  mutate(
    afwijkend_sloot = !is.na(afwijking_sloot) & abs(afwijking_sloot) > drempel_afwijking,
    grote_sprong = !is.na(sprong) & abs(sprong) > drempel_sprong,
    drempel_afwijking = drempel_afwijking, drempel_sprong = drempel_sprong
  ) |>
  filter(afwijkend_sloot | grote_sprong) |>
  arrange(desc(pmax(abs(afwijking_sloot), abs(sprong), na.rm = TRUE)))
print(redox_opvallend)

# Figuur: redox tegen watertemperatuur binnen een locatie (afwijking van het
# locatiegemiddelde over de meetjaren). De helling van de lijn is gelijk aan
# de helling 'binnen locatie, tussen jaren'.
helling_redox_wt <- redox_temperatuur |>
  filter(respons == "redox slib (pH 7)", verklarend == "watertemperatuur", schaal == "binnen locatie, tussen jaren")
redox_temp_afw <- meet_data |>
  filter(!is.na(slib_redox_pH7), !is.na(watertemp_C)) |>
  mutate(
    .by = SlootID,
    n_jaar = n(),
    temp_afw = watertemp_C - mean(watertemp_C),
    redox_afw = slib_redox_pH7 - mean(slib_redox_pH7)
  ) |>
  filter(n_jaar > 1) |>
  mutate(jaar_f = factor(jaar))

# Helling per meetjaar (zelfde afwijkingen, apart per jaar), voor de figuur
helling_redox_jaar <- redox_temp_afw |>
  summarise(
    .by = jaar_f,
    n = n(),
    helling = coef(lm(redox_afw ~ temp_afw))[2],
    p_waarde = summary(lm(redox_afw ~ temp_afw))$coefficients["temp_afw", "Pr(>|t|)"]
  ) |>
  arrange(jaar_f)
nl_getal <- \(x, d) formatC(round(x, d), format = "f", digits = d, decimal.mark = ",")
nl_p <- \(p) ifelse(p < 0.01, "< 0,01", paste("=", nl_getal(p, 2)))

plot_redox_temp <- ggplot(redox_temp_afw, aes(x = temp_afw, y = redox_afw)) +
  geom_hline(yintercept = 0, colour = "grey70") +
  geom_vline(xintercept = 0, colour = "grey70") +
  geom_point(aes(colour = gebied, shape = jaar_f), size = 2.3, alpha = 0.8) +
  # alle jaren samen (grijs) en per jaar (zwart, lijnsoort per jaar)
  geom_smooth(method = "lm", formula = y ~ x, colour = "grey50", fill = "grey85", linewidth = 1.2) +
  geom_smooth(aes(linetype = jaar_f, group = jaar_f), method = "lm", formula = y ~ x,
              se = FALSE, colour = "black", linewidth = 0.7) +
  scale_colour_manual(values = gebied_hoofdkleur) +
  labs(
    title = "WP2 - Redox van het slib en watertemperatuur: afwijking van het locatiegemiddelde",
    subtitle = paste0(
      "Per meting het verschil met het gemiddelde van dezelfde locatie over de meetjaren.\n",
      "Dit zijn geen meetwaarden: alle gemeten waarden zijn negatief.\n",
      "Grijze lijn = alle jaren: ",
      nl_getal(helling_redox_wt$helling, 1), " mV per °C (p = ", nl_getal(helling_redox_wt$p_waarde, 3), ").\n",
      "Zwarte lijnen per jaar: ",
      paste0(helling_redox_jaar$jaar_f, " ", nl_getal(helling_redox_jaar$helling, 1),
             " mV per °C (p ", nl_p(helling_redox_jaar$p_waarde), ")", collapse = "; "), "."
    ),
    x = "Afwijking watertemperatuur van locatiegemiddelde (°C)",
    y = "Afwijking redox slib (pH 7) van locatiegemiddelde (mV)\n+ = minder reducerend dan gemiddeld op die locatie",
    colour = "Gebied", shape = "Jaar", linetype = "Jaar"
  ) +
  theme_wp2
print(plot_redox_temp)
sla_figuur_op(plot_redox_temp, "redox_temperatuur_binnen_locatie", width = 10, height = 7)

# 12g. Slibdikte en P-AL: verandering of meetvariatie? ---------------------------
# Slibdikte (max_slib) en waterdiepte (max_wtd) komen uit de profielmeting
# (data_import_ppr.R): slibdikte = diepte tot de vaste bodem - waterdiepte per
# profielpunt, daarvan het maximum. Bij echte aangroei neemt de slibdikte toe
# en de waterdiepte af, terwijl de diepte van de vaste bodem gelijk blijft.
# Volgt de verandering in slibdikte vooral de gevonden diepte van de vaste
# bodem, dan gaat het om verschillen in peiling. Benadering: diepte vaste
# bodem = max_slib + max_wtd (beide maxima over het profiel, niet per se van
# hetzelfde punt). Daarnaast de herhaalbaarheid tussen het eerste en laatste
# meetjaar en de verandering per gebied, ook voor P-AL in slib (bovenste 25 cm
# van de sliblaag) en oever.
stabiliteit_vars <- c(
  slibdikte = "max_slib", waterdiepte = "max_wtd", vaste_bodem = "vaste_bodem",
  pal_slib = "P-AL mg p2o5/100g_SB", ptot_slib = "P2O5_xrf_g/kg_SB", pal_oever = "P-AL mg p2o5/100g_OR_25"
)

stabiliteit_lang <- wp2 |>
  as_tibble() |>
  mutate(vaste_bodem = max_slib + max_wtd) |>
  select(SlootID, gebied, jaar, all_of(stabiliteit_vars)) |>
  mutate(across(all_of(names(stabiliteit_vars)), \(x) suppressWarnings(as.numeric(x)))) |>
  pivot_longer(all_of(names(stabiliteit_vars)), names_to = "variabele", values_to = "waarde") |>
  filter(!is.na(waarde)) |>
  arrange(jaar) |>
  summarise(
    .by = c(SlootID, gebied, variabele),
    n_jaar = n_distinct(jaar), eerste = first(waarde), laatste = last(waarde)
  ) |>
  filter(n_jaar > 1) |>
  mutate(verandering = laatste - eerste, rel_verandering = if_else(eerste != 0, verandering / eerste, NA_real_))

# Herhaalbaarheid: rangcorrelatie tussen eerste en laatste meetjaar en de
# mediane absolute relatieve verandering per locatie
stabiliteit_samenvatting <- stabiliteit_lang |>
  summarise(
    .by = variabele,
    n_locaties = n(),
    rho_jaren = cor(eerste, laatste, method = "spearman"),
    mediaan_rel_verandering = median(abs(rel_verandering), na.rm = TRUE)
  )

stabiliteit_gebied <- stabiliteit_lang |>
  summarise(
    .by = c(variabele, gebied),
    n_locaties = n(), gem_eerste = mean(eerste), gem_laatste = mean(laatste),
    gem_verandering = mean(verandering), gem_rel_verandering = mean(rel_verandering, na.rm = TRUE),
    aandeel_toename = mean(verandering > 0)
  ) |>
  arrange(variabele, gebied)

# Samenhang tussen de veranderingen per locatie. P-AL en totaal-P (XRF) op
# basis van de relatieve verandering, omdat de eenheden verschillen.
verandering_breed <- stabiliteit_lang |>
  select(SlootID, variabele, verandering) |>
  pivot_wider(names_from = variabele, values_from = verandering)
rel_verandering_breed <- stabiliteit_lang |>
  select(SlootID, variabele, rel_verandering) |>
  pivot_wider(names_from = variabele, values_from = rel_verandering)
rho_verandering <- \(x, y, d = verandering_breed) cor(d[[x]], d[[y]], method = "spearman", use = "pairwise.complete.obs")
slibdikte_samenhang <- tibble(
  paar = c("slibdikte ~ vaste_bodem", "slibdikte ~ waterdiepte", "slibdikte ~ pal_slib", "pal_slib ~ ptot_slib (relatief)"),
  rho = c(
    rho_verandering("slibdikte", "vaste_bodem"),
    rho_verandering("slibdikte", "waterdiepte"),
    rho_verandering("slibdikte", "pal_slib"),
    rho_verandering("pal_slib", "ptot_slib", rel_verandering_breed)
  )
)
print(stabiliteit_samenvatting)
print(stabiliteit_gebied, n = 40)
print(slibdikte_samenhang)

# 12h. Slibniveau en vaste bodem in NAP per profielpunt ------------------------
# Controle op 12g met de profielpunten (object profiel uit
# Processed_data_workspace.RData, zie data_import_ppr.R). Per waterpunt:
# slibniveau = z (GPS-hoogte bovenkant slib, m NAP), vaste bodem = wl -
# gepeilde diepte (Opmerking, cm vanaf het wateroppervlak). Per locatie en jaar
# de mediaan over de waterpunten. De perceelhoogte hoort tussen de jaren gelijk
# te blijven en dient als controle op de GPS-hoogtes. De workspace wordt in een
# eigen omgeving geladen, zodat objecten in deze sessie niet overschreven worden.
if (!exists("profiel")) {
  ws_omgeving <- new.env()
  load(paste0(workspace, "Processed_data_workspace.RData"), envir = ws_omgeving)
  profiel <- ws_omgeving$profiel
  rm(ws_omgeving)
}
profiel_nap <- as.data.table(profiel)[
  SlootID %in% wp2$SlootID & sectie %in% c("water", "perceel"),
  .(SlootID, jaar, sectie, z, wl, peiling = suppressWarnings(as.numeric(Opmerking)))
]
profiel_locatie <- merge(
  profiel_nap[
    sectie == "water" & !is.na(peiling) & !is.na(wl),
    .(peil = wl[1], slibniveau = median(z), vaste_bodem = median(wl - peiling / 100),
      slibdikte = median(z - (wl - peiling / 100))),
    by = .(SlootID, jaar)
  ],
  profiel_nap[sectie == "perceel", .(perceel = median(z, na.rm = TRUE)), by = .(SlootID, jaar)],
  by = c("SlootID", "jaar"), all.x = TRUE
)
nap_kolommen <- c("peil", "slibniveau", "vaste_bodem", "slibdikte", "perceel")
profiel_verandering <- profiel_locatie[
  order(jaar),
  if (.N > 1) lapply(.SD, \(x) last(x) - first(x)),
  by = SlootID, .SDcols = nap_kolommen
][, gebied := str_extract(SlootID, "^[A-Z]+")]

profiel_nap_gebied <- profiel_verandering[
  , c(list(n_locaties = .N), lapply(.SD, \(x) mean(x, na.rm = TRUE))),
  by = gebied, .SDcols = nap_kolommen
][order(gebied)]
rho_nap <- \(x, y) profiel_verandering[, cor(get(x), get(y), method = "spearman", use = "pairwise.complete.obs")]
profiel_nap_samenhang <- data.table(
  maat = c("rho_slibdikte_vaste_bodem", "rho_slibdikte_slibniveau", "perceel_mediaan_abs_verandering_m",
           "vaste_bodem_max_abs_gebied_m", "n_locaties"),
  waarde = c(
    rho_nap("slibdikte", "vaste_bodem"), rho_nap("slibdikte", "slibniveau"),
    median(abs(profiel_verandering$perceel), na.rm = TRUE),
    max(abs(profiel_nap_gebied$vaste_bodem)), nrow(profiel_verandering)
  )
)
print(profiel_nap_gebied)
print(profiel_nap_samenhang)

# 12i. Taludhoek en vernattingsindex -------------------------------------------
# Bij de taludhoek rond de waterlijn (tldk_oevrwtr_perc, + = steiler) valt een
# groot deel van de variatie in de verandering op behandeling x gebied (12a).
# Hier per sloot het verschil in verandering tussen behandelingen in dezelfde
# sloot: afrastering bij gelijk basisbeheer (M-AF t.o.v. M, R-AF t.o.v. R),
# of, als dat niet kan, M-AF t.o.v. R; en minimaal t.o.v. regulier zonder
# afrastering. Daarnaast de samenhang tussen beginwaarde en verandering
# (terugkeer naar het gemiddelde) en tussen de verandering in de
# vernattingsindex en die in zijn drie componenten (main_veest.R).
talud_sloot <- wp2_verandering |>
  filter(kolom == "tldk_oevrwtr_perc") |>
  summarise(.by = c(gebied, sloot_nummer, behandeling_basis), verandering = mean(verandering)) |>
  pivot_wider(names_from = behandeling_basis, values_from = verandering)
for (b in c("R", "M", "R-AF", "M-AF")) if (!b %in% names(talud_sloot)) talud_sloot[[b]] <- NA_real_

talud_contrast_sloot <- bind_rows(
  transmute(talud_sloot, gebied, sloot_nummer, contrast = "M-AF t.o.v. M", afrastering = TRUE, verschil = `M-AF` - M),
  transmute(talud_sloot, gebied, sloot_nummer, contrast = "R-AF t.o.v. R", afrastering = TRUE, verschil = `R-AF` - R),
  transmute(talud_sloot, gebied, sloot_nummer, contrast = "M-AF t.o.v. R", afrastering = TRUE,
            verschil = if_else(is.na(M) & is.na(`R-AF`), `M-AF` - R, NA_real_)),
  transmute(talud_sloot, gebied, sloot_nummer, contrast = "M t.o.v. R", afrastering = FALSE, verschil = M - R)
) |>
  filter(!is.na(verschil)) |>
  arrange(gebied, sloot_nummer, contrast)

talud_contrast_gebied <- talud_contrast_sloot |>
  summarise(.by = c(gebied, afrastering), n_contrasten = n(), gem_verschil = mean(verschil))

talud_locatie <- filter(wp2_verandering, kolom == "tldk_oevrwtr_perc")

# Een negatieve samenhang tussen beginwaarde en verandering ontstaat ook zonder
# echte verandering, door toevallige meetfouten (terugkeer naar het
# gemiddelde). Echte convergentie naar een evenwichtshelling zou de spreiding
# tussen vakken verkleinen; bij alleen meetfout blijft die gelijk. Toets:
# Pitman-Morgan (cor(som, verschil) = 0 <=> gelijke varianties) en, robuuster,
# Oldham (Spearman tussen het gemiddelde van beide jaren en de verandering).
# Daarnaast de samenhang met de verandering in waterpeil op de meetdag, omdat
# de taludhoek rond de waterlijn wordt gemeten.
talud_gemiddeld <- (talud_locatie$waarde_eerste + talud_locatie$waarde_laatste) / 2
talud_oldham <- cor.test(talud_gemiddeld, talud_locatie$verandering, method = "spearman", exact = FALSE)
talud_pitman <- cor.test(talud_gemiddeld, talud_locatie$verandering)
peil_jaar <- as_tibble(profiel_locatie)[, c("SlootID", "jaar", "peil")] |>
  mutate(jaar = as.numeric(as.character(jaar)))
talud_peil <- talud_locatie |>
  mutate(across(c(jaar_eerste, jaar_laatste), \(x) as.numeric(as.character(x)))) |>
  left_join(rename(peil_jaar, jaar_eerste = jaar, peil_eerste = peil), by = c("SlootID", "jaar_eerste")) |>
  left_join(rename(peil_jaar, jaar_laatste = jaar, peil_laatste = peil), by = c("SlootID", "jaar_laatste")) |>
  mutate(peil_verandering = peil_laatste - peil_eerste) |>
  filter(!is.na(peil_verandering))
talud_peil_toets <- cor.test(talud_peil$peil_verandering, talud_peil$verandering, method = "spearman", exact = FALSE)
talud_peil_abs_toets <- cor.test(abs(talud_peil$peil_verandering), abs(talud_peil$verandering),
                                 method = "spearman", exact = FALSE)

vernatting_verandering <- wp2 |>
  as_tibble() |>
  select(SlootID, jaar, vernattingsindex = vernattingsrisico_index, taludhoek = tldk_oevrwtr_perc,
         kaal_productiegras = vernatting_kaal_gras_norm, emerse_vegetatie = vernatting_emers2b_norm) |>
  arrange(jaar) |>
  summarise(.by = SlootID, across(-jaar, \(x) {
    x <- x[!is.na(x)]
    if (length(x) > 1) last(x) - first(x) else NA_real_
  }))
rho_vernatting <- \(k) cor(vernatting_verandering$vernattingsindex, vernatting_verandering[[k]],
                           method = "spearman", use = "pairwise.complete.obs")
talud_samenhang <- tibble(
  maat = c("rho_talud_begin_verandering", "p_talud_begin_verandering", "n_talud",
           "sd_talud_eerste", "sd_talud_laatste", "p_talud_pitman_morgan",
           "rho_talud_oldham", "p_talud_oldham",
           "rho_talud_peil", "p_talud_peil", "rho_talud_peil_abs", "p_talud_peil_abs", "n_talud_peil",
           "rho_vernatting_taludhoek", "rho_vernatting_kaal_productiegras", "rho_vernatting_emerse_vegetatie",
           "n_vernatting"),
  waarde = c(
    cor(talud_locatie$waarde_eerste, talud_locatie$verandering, method = "spearman"),
    cor.test(talud_locatie$waarde_eerste, talud_locatie$verandering, method = "spearman", exact = FALSE)$p.value,
    nrow(talud_locatie),
    sd(talud_locatie$waarde_eerste), sd(talud_locatie$waarde_laatste), talud_pitman$p.value,
    unname(talud_oldham$estimate), talud_oldham$p.value,
    unname(talud_peil_toets$estimate), talud_peil_toets$p.value,
    unname(talud_peil_abs_toets$estimate), talud_peil_abs_toets$p.value, nrow(talud_peil),
    rho_vernatting("taludhoek"), rho_vernatting("kaal_productiegras"), rho_vernatting("emerse_vegetatie"),
    sum(!is.na(vernatting_verandering$vernattingsindex))
  )
)
print(talud_contrast_sloot, n = 50)
print(talud_contrast_gebied)
print(talud_samenhang)

# 12j. Oevervegetatie zone 2b in sloten met waargenomen verruiging ------------
# In het veld is verruiging van de oevervegetatie waargenomen in onderstaande
# sloten. Per vak de verandering (laatste - eerste meetjaar) in de bedekking met
# emerse vegetatie en met productiegras in zone 2b, als illustratie bij de
# duiding van het (gemiddeld niet aantoonbare) effect van basisbeheer. Let op:
# de sloten zijn geselecteerd op een veldwaarneming, dit is geen toets.
sloten_verruiging <- c("WL_2", "KW_1", "SW_1", "SW_2", "SW_4", "ZG_2", "ZG_3")
verruiging_2b <- wp2 |>
  as_tibble() |>
  filter(sloot_nummer %in% sloten_verruiging) |>
  select(SlootID, sloot_nummer, jaar, Behandeling,
         emers = oeverzone_2b_emers_perc, productiegras = oeverzone_2b_ter_prodgras_perc) |>
  mutate(across(c(emers, productiegras), \(x) suppressWarnings(as.numeric(x)))) |>
  arrange(jaar) |>
  summarise(
    .by = c(SlootID, sloot_nummer),
    behandeling = last(Behandeling),
    verandering_emers = if (sum(!is.na(emers)) > 1) last(na.omit(emers)) - first(na.omit(emers)) else NA_real_,
    verandering_productiegras = if (sum(!is.na(productiegras)) > 1) last(na.omit(productiegras)) - first(na.omit(productiegras)) else NA_real_
  ) |>
  mutate(afrastering = str_detect(behandeling, "AF")) |>
  arrange(sloot_nummer, behandeling)
print(verruiging_2b)

# 12k. Breedte zone 2a bij wel en geen afrastering -------------------------------
# Verandering in de breedte van zone 2a in vakken met en zonder afrastering,
# over alle sloten en binnen dezelfde sloot (verschil afgerasterde vakken -
# vakken zonder afrastering). Toets: gemengd model met alleen wel/geen
# afrastering (gebied en sloot random); dit is een aanvullende, achteraf
# gekozen toets naast die op basisbeheer (sectie 12b).
z2a <- wp2_verandering |>
  filter(kolom == "oeverzone_2a_breedte_cm") |>
  mutate(afrastering = str_detect(Behandeling, "AF"))
z2a_binnen <- z2a |>
  summarise(.by = c(gebied, sloot_nummer, afrastering), v = mean(verandering)) |>
  pivot_wider(names_from = afrastering, values_from = v, names_prefix = "af_") |>
  filter(!is.na(af_TRUE), !is.na(af_FALSE)) |>
  mutate(verschil = af_TRUE - af_FALSE)
z2a_fit <- tryCatch(
  suppressWarnings(suppressMessages(lmerTest::lmer(verandering ~ afrastering + (1 | gebied) + (1 | sloot_nummer), data = z2a))),
  error = \(e) NULL
)
z2a_co <- if (is.null(z2a_fit)) NULL else summary(z2a_fit)$coefficients
zone2a_af <- tibble(
  gem_met_af = mean(z2a$verandering[z2a$afrastering]), gem_zonder_af = mean(z2a$verandering[!z2a$afrastering]),
  n_sloten_vergelijk = nrow(z2a_binnen), n_sloten_af_breder = sum(z2a_binnen$verschil > 0),
  mediaan_verschil_binnen_sloot = median(z2a_binnen$verschil),
  schatting = if (is.null(z2a_co)) NA_real_ else z2a_co["afrasteringTRUE", "Estimate"],
  p_waarde = if (is.null(z2a_co)) NA_real_ else z2a_co["afrasteringTRUE", "Pr(>|t|)"],
  grootste_verschil = paste(head(arrange(z2a_binnen, desc(verschil))$sloot_nummer, 3), collapse = ", ")
)
print(zone2a_af)

# Behandeling per vak en jaar, met de veldnotities over afwijkend beheer
# (WP2_beheer_anders uit het veldformulier abiotiek). Alle sloten, ook die
# zonder behandelingscontrast.
behandeling_per_vak <- wp2_compleet |>
  as_tibble() |>
  select(gebied, sloot_nummer, SlootID, jaar, Behandeling, WP2_beheer_anders) |>
  summarise(
    .by = c(gebied, sloot_nummer, SlootID, jaar),
    Behandeling = paste(unique(na.omit(Behandeling)), collapse = "/"),
    notitie = paste(unique(na.omit(WP2_beheer_anders[WP2_beheer_anders != ""])), collapse = "; ")
  ) |>
  arrange(jaar) |>
  summarise(
    .by = c(gebied, sloot_nummer, SlootID),
    b_2024 = coalesce(Behandeling[jaar == 2024][1], ""),
    b_2025 = coalesce(Behandeling[jaar == 2025][1], ""),
    b_2026 = coalesce(Behandeling[jaar == 2026][1], ""),
    veldnotities = paste(unique(paste0(jaar[notitie != ""], ": ", notitie[notitie != ""])), collapse = "; ")
  ) |>
  mutate(veldnotities = if_else(veldnotities == ": ", "", veldnotities)) |>
  arrange(gebied, sloot_nummer, SlootID)

# 12l. Uitgangssituatie van sloten met en zonder waargenomen effect --------------
# Uitgangssituatie (eerste meetjaar met een waarde, gemiddeld over de vakken van
# een sloot) van de sloten waar in het veld de verwachte verruiging van de
# oevervegetatie is gezien (sloten_verruiging, 12j), de sloten waar geen enkel
# effect is gezien (sloten_geen_effect) en de overige sloten. Verkennend: de
# groepen zijn gekozen op een veldwaarneming en klein; Wilcoxon-toets
# 'verwacht effect' tegen de overige sloten, met BH-correctie.
uitgang_vars <- tribble(
  ~kolom,                            ~kenmerk,                         ~groep_kenmerk,
  "oeverzone_2b_kaal_perc",          "Kale oever zone 2b (%)",         "vegetatie",
  "oeverzone_2b_emers_perc",         "Emerse vegetatie zone 2b (%)",   "vegetatie",
  "oeverzone_2b_ter_prodgras_perc",  "Productiegras zone 2b (%)",      "vegetatie",
  "n_soorten_oev_zone2b",            "Aantal soorten zone 2b",         "vegetatie",
  "Soortensamenstelling Helofyten",  "EKR helofyten",                  "vegetatie",
  "max_wtd",                         "Waterdiepte (m)",                "hydrologie",
  "drglg",                           "Drooglegging (m)",               "hydrologie",
  "slib_redox_pH7",                  "Redox slib (mV)",                "slib",
  "P-AL mg p2o5/100g_SB",            "P-AL slib (mg P2O5/100 g)",      "slib",
  "P_µmol/l_PW",                     "P poriewater (µmol/l)",          "slib",
  "NH4_µmol/l_PW",                   "NH4 poriewater (µmol/l)",        "slib",
  "P-AL mg p2o5/100g_OR_25",         "P-AL oever (mg P2O5/100 g)",     "oever",
  "N_mineraal_OR_25",                "N-mineraal oever (mg/kg)",       "oever",
  "OS_perc_OR_25",                   "Organische stof oever (%)",      "oever",
  "Z_CLAY_SA_OR_25",                 "Klei oever (%)",                 "oever"
) |>
  filter(kolom %in% names(wp2))

groep_effect <- \(s) case_when(
  s %in% sloten_verruiging ~ "verwacht effect gezien",
  s %in% sloten_geen_effect ~ "geen effect gezien",
  TRUE ~ "overige sloten"
)
uitgang_sloot <- wp2 |>
  as_tibble() |>
  select(gebied, sloot_nummer, SlootID, jaar, all_of(uitgang_vars$kolom)) |>
  mutate(across(all_of(uitgang_vars$kolom), \(x) suppressWarnings(as.numeric(x)))) |>
  pivot_longer(all_of(uitgang_vars$kolom), names_to = "kolom", values_to = "waarde") |>
  filter(!is.na(waarde)) |>
  arrange(jaar) |>
  summarise(.by = c(gebied, sloot_nummer, SlootID, kolom), begin = first(waarde)) |>
  summarise(.by = c(gebied, sloot_nummer, kolom), begin = mean(begin)) |>
  left_join(uitgang_vars, by = "kolom") |>
  mutate(groep = factor(groep_effect(sloot_nummer), levels = c("verwacht effect gezien", "overige sloten", "geen effect gezien")))

uitgang_vergelijking <- uitgang_sloot |>
  summarise(
    .by = c(groep_kenmerk, kenmerk),
    mediaan_verwacht = median(begin[groep == "verwacht effect gezien"]),
    mediaan_overig = median(begin[groep == "overige sloten"]),
    mediaan_geen = median(begin[groep == "geen effect gezien"]),
    p_waarde = tryCatch(
      wilcox.test(begin[groep == "verwacht effect gezien"], begin[groep != "verwacht effect gezien"], exact = FALSE)$p.value,
      error = \(e) NA_real_
    )
  ) |>
  mutate(p_bh = p.adjust(p_waarde, method = "BH"))
print(uitgang_vergelijking, n = 30)

# Veentype per sloot (meest voorkomende waarde)
uitgang_veentype <- wp2 |>
  as_tibble() |>
  filter(!is.na(veentype)) |>
  summarise(.by = c(gebied, sloot_nummer), veentype = names(which.max(table(veentype)))) |>
  mutate(groep = groep_effect(sloot_nummer)) |>
  count(groep, veentype)
print(uitgang_veentype)

plot_uitgang <- uitgang_sloot |>
  mutate(kenmerk = factor(kenmerk, levels = uitgang_vars$kenmerk)) |>
  ggplot(aes(x = groep, y = begin)) +
  geom_boxplot(outlier.shape = NA, colour = "grey50", width = 0.5) +
  geom_point(aes(colour = gebied), position = position_jitter(width = 0.12, height = 0, seed = 1), size = 2, alpha = 0.85) +
  scale_colour_manual(values = gebied_hoofdkleur) +
  scale_x_discrete(labels = \(x) str_wrap(x, 12)) +
  facet_wrap(~kenmerk, scales = "free_y", ncol = 5) +
  labs(
    title = "WP2 - Uitgangssituatie van sloten met en zonder effect bij de veldbeoordeling in 2026",
    subtitle = paste0(
      "Eerste meetjaar, gemiddeld per sloot. Indeling volgens de veldbeoordeling in 2026. Verwacht effect gezien: ", paste(sloten_verruiging, collapse = ", "),
      "; geen effect gezien: ", paste(sloten_geen_effect, collapse = ", "), "."
    ),
    x = NULL, y = NULL, colour = "Gebied"
  ) +
  theme_wp2
print(plot_uitgang)
sla_figuur_op(plot_uitgang, "uitgangssituatie_waargenomen_effect", width = 16, height = 11)

# 13. Contrasten per gebied ------------------------------------------------------
# 13a. Hegewarren: het enige gebied met R, M, R-AF en M-AF. Full factorial
# (basisbeheer x afrastering) met sloot als vast blok. Let op: 2 sloten x 4
# behandelingen = 8 locaties, dus weinig vrijheidsgraden (3 voor de rest);
# p-waarden zijn indicatief.
# R-AF valt buiten de selectie (sectie 2e); daarom op alle vakken van HW.
verandering_hw <- voeg_behandelingsfactoren_toe(filter(wp2_compleet, gebied == "HW")) |>
  bereken_verandering_lang(var_lijst, groep_cols)
resultaten_hw <- toets_per_variabele(
  verandering_hw,
  model_termen = "basis_mr * afrastering",
  toets_termen = c("basis_mr", "afrastering", "basis_mr:afrastering"),
  blok = "sloot_nummer"
) |>
  mutate(p_bh = p.adjust(p_waarde, method = "BH"), .by = term)
print(resultaten_hw |> select(naam, term, schatting, p_waarde, p_bh, opzet), n = 80)

labels_hw <- resultaten_hw |>
  mutate(term = case_when(
    term == "basis_mr" ~ "M/R",
    term == "afrastering" ~ "AF",
    TRUE ~ "M/R×AF"
  )) |>
  summarise(
    .by = c(kolom, label),
    facet = paste0(met_richting(label[1], 35), "\n", paste0(term, " p = ", p_label(p_waarde), collapse = "; ")),
    niet_sign = all(!is.na(p_waarde)) & all(p_waarde >= 0.05)
  )

plot_hw <- verandering_hw |>
  left_join(select(labels_hw, kolom, facet), by = "kolom") |>
  ggplot(aes(x = basis_mr, y = verandering, colour = afrastering, group = afrastering)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.4) +
  geom_point(aes(shape = sloot_nummer), size = 2.3, position = position_dodge(width = 0.3)) +
  stat_summary(fun = mean, geom = "line", linewidth = 0.9, position = position_dodge(width = 0.3)) +
  laag_niet_significant(select(labels_hw, facet, niet_sign)) +
  scale_colour_manual(values = c("zonder AF" = okabe_ito[1], "met AF" = okabe_ito[6])) +
  facet_wrap(~facet, scales = "free_y", ncol = 5) +
  labs(
    title = "Hegewarren - full factorial: basisbeheer × afrastering",
    subtitle = paste0(
      "Lijn = gemiddelde verandering per combinatie; niet-parallelle lijnen wijzen op een interactie.\n",
      "p uit lm(verandering ~ basisbeheer × afrastering + sloot); 8 locaties, dus indicatief."
    ),
    caption = paste(caption_niet_sign, "Hier: alle drie de termen p ≥ 0,05."),
    x = "Basisbeheer", y = "Verandering (laatste − eerste meetjaar)",
    colour = "Afrastering", shape = "Sloot"
  ) +
  theme_wp2
print(plot_hw)
sla_figuur_op(plot_hw, "contrast_hegewarren_factorial", width = 18, height = 16)

# 13b. Overige gebieden: minimaal vs. regulier (alleen locaties zonder AF en
# zonder NVO) en wel vs. geen afrastering (locaties zonder NVO). Waar beide
# niveaus langs dezelfde sloot liggen, is de sloot een vast blok; anders is
# het een vergelijking tussen sloten (sloot random). Welke behandelingen
# feitelijk vergeleken worden, staat in 'vergeleken_behandelingen' (bv. in
# Ronde Hoep is wel/geen afrastering vooral M-AF vs. R).
contrasten_gebied <- list(
  `minimaal vs regulier` = list(
    selectie = \(d) filter(d, afrastering == "zonder AF", nvo == "zonder NVO"),
    term = "basis_mr"
  ),
  `wel vs geen afrastering` = list(
    selectie = \(d) filter(d, nvo == "zonder NVO"),
    term = "afrastering"
  )
)

#' Beschrijf welke Behandeling-codes per niveau van een contrastterm vallen
#'
#' @param d Data met de kolom Behandeling en de kolom `term`.
#' @param term Naam van de contrastkolom.
#' @return Eén tekst, bv. "zonder AF: M/R vs met AF: M-AF".
beschrijf_vergelijking <- function(d, term) {
  d |>
    distinct(niveau = .data[[term]], Behandeling) |>
    summarise(.by = niveau, b = paste(sort(unique(Behandeling)), collapse = "/")) |>
    arrange(niveau) |>
    mutate(tekst = paste0(niveau, ": ", b)) |>
    pull(tekst) |>
    paste(collapse = " vs ")
}

resultaten_contrasten_gebied <- expand_grid(
  gebied_sel = sort(unique(wp2$gebied)),
  contrast = names(contrasten_gebied)
) |>
  pmap_dfr(\(gebied_sel, contrast) {
    def <- contrasten_gebied[[contrast]]
    d <- def$selectie(filter(wp2_verandering, gebied == gebied_sel))
    if (n_distinct(d[[def$term]]) < 2) return(tibble())
    toets_per_variabele(d, model_termen = def$term, toets_termen = def$term, blok = "sloot_nummer") |>
      mutate(
        gebied = gebied_sel, contrast = contrast,
        vergeleken_behandelingen = beschrijf_vergelijking(d, def$term)
      )
  }) |>
  mutate(p_bh = p.adjust(p_waarde, method = "BH"), .by = c(gebied, contrast))

print(resultaten_contrasten_gebied |>
        distinct(gebied, contrast, vergeleken_behandelingen, opzet))

# 13c. Overzicht (heatmap) van alle gebiedscontrasten. De
# kleur is het gestandaardiseerde effect (schatting / SD van de verandering
# van die variabele over alle WP2-locaties): rood = grotere toename bij het
# tweede niveau (minimaal resp. met AF), blauw = kleinere toename / afname.
sd_per_variabele <- wp2_verandering |> summarise(.by = kolom, sd_var = sd(verandering, na.rm = TRUE))

heatmap_data <- resultaten_contrasten_gebied |>
  left_join(sd_per_variabele, by = "kolom") |>
  mutate(
    effect_std = schatting / sd_var,
    kolomlabel = paste0(gebied, "\n", contrast),
    ster = case_when(p_bh < 0.05 ~ "**", p_waarde < 0.05 ~ "*", TRUE ~ "")
  )

plot_heatmap_gebied <- ggplot(heatmap_data, aes(x = kolomlabel, y = label, fill = effect_std)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = ster), size = 5, vjust = 0.75) +
  scale_fill_gradient2(
    low = okabe_ito[5], mid = "white", high = okabe_ito[6], midpoint = 0,
    limits = c(-1.5, 1.5), breaks = c(-1.5, -0.75, 0, 0.75, 1.5),
    oob = scales::squish, na.value = "grey90"
  ) +
  guides(fill = guide_colourbar(barwidth = unit(6, "cm"))) +
  facet_grid(thema ~ ., scales = "free_y", space = "free_y") +
  labs(
    title = "WP2 - Contrasten per gebied (verandering laatste − eerste meetjaar)",
    subtitle = paste0(
      "Kleur = effect van minimaal (t.o.v. regulier) of met AF (t.o.v. zonder AF), in SD van de verandering.\n",
      "* p < 0.05; ** p(BH) < 0.05 binnen gebied en contrast. Grijs = niet te schatten."
    ),
    x = NULL, y = NULL, fill = "Effect (SD)"
  ) +
  theme_wp2 +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.text.y = element_text(angle = 0))
print(plot_heatmap_gebied)
sla_figuur_op(plot_heatmap_gebied, "contrasten_per_gebied_heatmap", width = 12, height = 12)

# 13d. Significante gebiedscontrasten (p (BH) < 0,05) apart in beeld --------
# De jaarfiguren 'wel/geen afrastering' en 'minimaal/regulier' (sectie 4b)
# gaan naar de bijlage. Alleen contrasten die na BH-correctie significant zijn
# krijgen hier een eigen figuur voor de hoofdtekst: mediaan per jaar en groep
# (dikke lijn) en de locaties (dunne lijnen), voor het gebied waarin het
# verschil significant is. Zijn er geen, dan wordt dat gemeld en kan in de
# rapporttekst staan dat er geen significante verschillen zijn.
sign_contrasten <- resultaten_contrasten_gebied |>
  filter(!is.na(p_bh), p_bh < 0.05) |>
  select(gebied, contrast, kolom, label, schatting, p_waarde, p_bh, n_sloten)
sla_tabel_op(sign_contrasten, "tabel_significante_contrasten")

if (nrow(sign_contrasten) == 0) {
  message("Geen gebiedscontrasten met p (BH) < 0,05: in de rapporttekst benoemen dat er geen significante verschillen zijn.")
} else {
  print(sign_contrasten)
  sign_jaar <- pmap_dfr(sign_contrasten, \(gebied, contrast, kolom, label, schatting, p_waarde, p_bh, n_sloten) {
    def <- contrasten_gebied[[contrast]]
    d <- wp2 |> as_tibble() |> filter(.data$gebied == !!gebied)
    def$selectie(d) |>
      transmute(SlootID, jaar, groep = as.character(.data[[def$term]]), waarde = as.numeric(.data[[kolom]])) |>
      filter(!is.na(waarde)) |>
      mutate(paneel = paste0(
        gebied, " – ", met_richting(label, 45), "\n",
        contrast, ": p = ", p_label(p_waarde), ", p (BH) = ", p_label(p_bh), ", ", n_sloten, " sloten"
      ))
  })

  plot_sign_contrasten <- ggplot(sign_jaar, aes(x = factor(jaar), y = waarde, colour = groep)) +
    geom_line(aes(group = SlootID), alpha = 0.35) +
    stat_summary(aes(group = groep), fun = median, geom = "line", linewidth = 1.2) +
    stat_summary(aes(group = groep), fun = median, geom = "point", size = 2.3) +
    scale_colour_manual(values = c(
      "regulier" = okabe_ito[1], "minimaal" = okabe_ito[3],
      "zonder AF" = okabe_ito[1], "met AF" = okabe_ito[6]
    )) +
    facet_wrap(~paneel, scales = "free_y") +
    labs(
      title = "WP2 - Significante verschillen binnen een gebied (p na BH-correctie < 0,05)",
      subtitle = "Dunne lijn = locatie, dikke lijn = mediaan per groep en jaar.\nToets op de verandering (laatste − eerste meetjaar), sloot als blok.",
      x = "Jaar", y = "Waarde (eenheid per paneel)", colour = "Groep"
    ) +
    theme_wp2
  print(plot_sign_contrasten)
  sla_figuur_op(plot_sign_contrasten, "significante_contrasten_jaar",
                width = 6 * min(nrow(sign_contrasten), 3) + 2, height = 5 * ceiling(nrow(sign_contrasten) / 3) + 2)
}

# 14. Afkreeften: Ronde Hoep en Oukoop -------------------------------------------
# Ronde Hoep: afkreeften (KR) geldt voor de hele sloot (RH_3, RH_5, RH_7), dus
# het effect is een vergelijking tussen sloten (sloot random), gecorrigeerd
# voor afrastering.
resultaten_afkreeften <- toets_per_variabele(
  filter(wp2_verandering, gebied == "RH"),
  model_termen = "afkreeften", toets_termen = "afkreeften",
  covariaten = "afrastering", random = "(1 | sloot_nummer)"
) |>
  mutate(gebied = "Ronde Hoep")

# Oukoop: sloten met en zonder afkreeften liggen in paren (bv. DR_1 en DR_1a),
# elk met regulier beheer en minimaal beheer met maaien in juli of augustus.
# Afkreeften: paar als vast blok, sloot random, gecorrigeerd voor maaibeheer.
# Maaimaand: juli vs. augustus binnen dezelfde sloot (alleen minimaal beheer).
oukoop_verandering <- NULL
if (!is.null(oukoop) && nrow(oukoop) > 0) {
  var_lijst_oukoop <- filter(var_lijst, kolom %in% names(oukoop))
  oukoop_verandering <- bereken_verandering_lang(
    oukoop, var_lijst_oukoop,
    c(setdiff(groep_cols, "behandeling_vorm"), "maaibeheer", "sloot_paar")
  )

  resultaten_afkreeften <- bind_rows(
    resultaten_afkreeften,
    toets_per_variabele(
      oukoop_verandering,
      model_termen = "afkreeften", toets_termen = "afkreeften",
      covariaten = "maaibeheer", blok = "sloot_paar", random = "(1 | sloot_nummer)"
    ) |>
      mutate(gebied = "Oukoop")
  )

  resultaten_maaimaand_oukoop <- oukoop_verandering |>
    filter(maaibeheer != "regulier") |>
    toets_per_variabele(
      model_termen = "maaibeheer", toets_termen = "maaibeheer",
      covariaten = "afkreeften", blok = "sloot_nummer"
    ) |>
    mutate(p_bh = p.adjust(p_waarde, method = "BH"), contrast = "maaien augustus vs juli")

  resultaten_mr_oukoop <- oukoop_verandering |>
    mutate(basis_mr = factor(basis_mr, levels = c("regulier", "minimaal"))) |>
    toets_per_variabele(
      model_termen = "basis_mr", toets_termen = "basis_mr",
      covariaten = "afkreeften", blok = "sloot_nummer"
    ) |>
    mutate(p_bh = p.adjust(p_waarde, method = "BH"), contrast = "minimaal vs regulier")

  print(bind_rows(resultaten_maaimaand_oukoop, resultaten_mr_oukoop) |>
          select(contrast, naam, schatting, p_waarde, p_bh, n_locaties, opzet), n = 60)

  # 14b. Maaibeheer in Oukoop met alle behandelingen en de uitgangssituatie ----
  # Aanvullend op hierboven, in lijn met een aanvullende analyse binnen het
  # project: alle Oukoop-vakken (regulier, maaien juli, maaien augustus), het
  # oppervlak emerse vegetatie in zone 2a en 2b en de draagkracht van de oever
  # in de bovenste 10 cm, met de beginwaarde als verklarende variabele.
  # Model per respons: verandering ~ maaibeheer + beginwaarde + sloot (vast
  # blok); afkreeften valt binnen het slootblok weg. Contrasten: augustus en
  # regulier t.o.v. juli (referentie).
  maai_vars <- tribble(
    ~kolom,                    ~kenmerk,
    "oeverzone_2a_emers_m2",   "Emerse vegetatie zone 2a (m²)",
    "oeverzone_2b_emers_m2",   "Emerse vegetatie zone 2b (m²)",
    "oever_[0,10]",            "Draagkracht oever 0-10 cm (MPa)",
    "draagkracht_oever",       "Draagkracht oever 0-40 cm (MPa)"
  ) |>
    filter(kolom %in% names(oukoop))

  maaibeheer_oukoop <- map_dfr(seq_len(nrow(maai_vars)), \(i) {
    d <- oukoop |>
      as_tibble() |>
      transmute(SlootID, sloot_nummer, jaar, maaibeheer, waarde = suppressWarnings(as.numeric(.data[[maai_vars$kolom[i]]]))) |>
      filter(!is.na(waarde), !is.na(maaibeheer)) |>
      arrange(jaar) |>
      summarise(.by = c(SlootID, sloot_nummer, maaibeheer), n_jaar = n_distinct(jaar),
                begin = first(waarde), verandering = last(waarde) - first(waarde)) |>
      filter(n_jaar > 1) |>
      mutate(maaibeheer = relevel(droplevels(maaibeheer), ref = "minimaal, maaien juli"),
             begin_z = as.numeric(scale(begin)))
    fit <- tryCatch(lm(verandering ~ maaibeheer + begin_z + sloot_nummer, data = d), error = \(e) NULL)
    if (is.null(fit)) return(tibble())
    co <- summary(fit)$coefficients
    rij <- \(coef_naam, label) {
      if (!coef_naam %in% rownames(co)) return(NULL)
      tibble(term = label, schatting = co[coef_naam, 1], p_waarde = co[coef_naam, 4])
    }
    bind_rows(
      rij("maaibeheerminimaal, maaien augustus", "augustus t.o.v. juli"),
      rij("maaibeheerregulier", "regulier t.o.v. juli"),
      rij("begin_z", "beginwaarde (per SD)")
    ) |>
      mutate(kenmerk = maai_vars$kenmerk[i], n_vakken = nrow(d), n_sloten = n_distinct(d$sloot_nummer),
             gem_juli = mean(d$verandering[d$maaibeheer == "minimaal, maaien juli"]),
             gem_augustus = mean(d$verandering[d$maaibeheer == "minimaal, maaien augustus"]),
             gem_regulier = mean(d$verandering[d$maaibeheer == "regulier"]), .before = 1)
  })
  print(maaibeheer_oukoop, n = 30)

  # 14c. Kreeftvangst per afgekreefte sloot (Oukoop) ----------------------------
  # Vangst per maand (juli 2024 - december 2025) per "Locatie 1-5". Locatie n
  # hoort bij de sloot met KR in de behandeling en slootnummer n (DR_1a, GZ_2a,
  # DR_3a, DR_4a, GM_5a). Per sloot-paar wordt het effect van afkreeften
  # berekend als de verandering in de afgekreefte sloot min die in de sloot
  # zonder afkreeften, per maaibeheer en daarna gemiddeld. Dat verschil wordt
  # vergeleken met de totale vangst (Spearman; met vijf paren beschrijvend).
  kreeft_bestand <- paste0(
    Sys.getenv("NMI-SITE"),
    "/O 2000 - O 2001/2056.N.24 Oukoop proefsloten/04. Data en resultaten/Afkreeften/afkreeften.xlsx"
  )
  kreeftvangst_oukoop <- NULL
  kreeftvangst_effect <- NULL
  if (file.exists(kreeft_bestand)) {
    kr_sloten <- oukoop |>
      as_tibble() |>
      filter(str_detect(Behandeling, "KR")) |>
      distinct(sloot_nummer, sloot_paar) |>
      mutate(locatie_nr = as.integer(str_extract(sloot_nummer, "\\d+")))
    stopifnot(!anyDuplicated(kr_sloten$locatie_nr))

    kreeftvangst_oukoop <- readxl::read_excel(kreeft_bestand) |>
      mutate(locatie_nr = as.integer(str_extract(Locatie, "\\d+"))) |>
      summarise(
        .by = locatie_nr,
        vangst_kg = sum(Gewicht, na.rm = TRUE),
        vangst_kg_2024 = sum(Gewicht[Jaar == 2024], na.rm = TRUE),
        vangst_kg_2025 = sum(Gewicht[Jaar == 2025], na.rm = TRUE),
        vangst_aantal = sum(Aantal, na.rm = TRUE),
        n_maanden = n_distinct(paste(Jaar, Maand))
      ) |>
      left_join(kr_sloten, by = "locatie_nr") |>
      arrange(locatie_nr)
    stopifnot(!anyNA(kreeftvangst_oukoop$sloot_nummer))
    print(kreeftvangst_oukoop)

    # verschil in verandering met afkreeften t.o.v. zonder, per paar
    kreeft_paar <- oukoop_verandering |>
      summarise(.by = c(kolom, naam, label, sloot_paar, maaibeheer, afkreeften),
                verandering = mean(verandering, na.rm = TRUE)) |>
      pivot_wider(names_from = afkreeften, values_from = verandering) |>
      filter(!is.na(`met afkreeften`), !is.na(`zonder afkreeften`)) |>
      summarise(.by = c(kolom, naam, label, sloot_paar),
                verschil = mean(`met afkreeften` - `zonder afkreeften`),
                n_maaibeheer = n()) |>
      inner_join(select(kreeftvangst_oukoop, sloot_paar, sloot_nummer, vangst_kg), by = "sloot_paar")

    kreeftvangst_effect <- kreeft_paar |>
      filter(n_distinct(sloot_paar) >= 4, .by = kolom) |>
      summarise(
        .by = c(kolom, naam, label),
        n_paren = n(),
        rho = suppressWarnings(cor(verschil, vangst_kg, method = "spearman")),
        p_waarde = suppressWarnings(cor.test(verschil, vangst_kg, method = "spearman", exact = TRUE)$p.value),
        gem_verschil = mean(verschil)
      ) |>
      arrange(p_waarde)
    print(kreeftvangst_effect, n = 40)

    kreeft_vars <- c("doorzicht2_mid_m", "holleoever", "zichtdiepte")
    p_kreeft <- kreeft_paar |>
      filter(kolom %in% kreeft_vars) |>
      left_join(select(kreeftvangst_effect, kolom, rho, p_waarde), by = "kolom") |>
      mutate(facet = paste0(met_richting(label, 35), "\nρ = ", nl_getal(rho, 2), ", p = ", p_label(p_waarde))) |>
      ggplot(aes(x = vangst_kg, y = verschil)) +
      geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.4) +
      geom_point(size = 2.5, colour = okabe_ito[6]) +
      ggrepel::geom_text_repel(aes(label = sloot_paar), size = 3, seed = 1) +
      facet_wrap(~facet, scales = "free_y", nrow = 1) +
      labs(
        title = "Oukoop: kreeftvangst en effect van afkreeften",
        subtitle = paste0("Effect = verandering in de afgekreefte sloot min die in de sloot zonder afkreeften (zelfde paar, per maaibeheer gemiddeld).\n",
                          "Positief = grotere toename in de afgekreefte sloot; negatief = grotere toename in de sloot waar de kreeft bleef."),
        x = "Kreeftvangst juli 2024 - december 2025 (kg)", y = "Verschil in verandering (met − zonder afkreeften)"
      ) +
      theme_wp2
    print(p_kreeft)
    sla_figuur_op(p_kreeft, "afkreeften_vangst_oukoop", width = 12, height = 5)
  } else {
    message("Kreeftvangst niet gevonden: ", kreeft_bestand)
  }
}

resultaten_afkreeften <- resultaten_afkreeften |>
  mutate(p_bh = p.adjust(p_waarde, method = "BH"), .by = gebied)
print(resultaten_afkreeften |> select(gebied, naam, schatting, p_waarde, p_bh, n_sloten, opzet), n = 80)

afkreeften_plotdata <- bind_rows(
  wp2_verandering |>
    filter(gebied == "RH") |>
    mutate(gebied_label = "Ronde Hoep", behandeling_label = as.character(behandeling_basis)),
  if (!is.null(oukoop_verandering)) {
    oukoop_verandering |>
      mutate(gebied_label = "Oukoop", behandeling_label = as.character(maaibeheer))
  }
) |>
  mutate(
    geen_effect = effect_categorie == "geen effect",
    gebied_label = factor(gebied_label, levels = c("Ronde Hoep", "Oukoop"))
  )

kleuren_afkreeften <- c(
  kleuren_basis,
  "regulier" = okabe_ito[1], "minimaal, maaien juli" = okabe_ito[2],
  "minimaal, maaien augustus" = okabe_ito[7]
)

labels_afkreeften <- resultaten_afkreeften |>
  summarise(.by = c(kolom, label), facet = paste0(met_richting(label[1], 35), "\n", paste0(gebied, " p = ", p_label(p_waarde), collapse = "; ")))

# "niet significant" per paneel (variabele x gebied)
niet_sign_afkreeften <- resultaten_afkreeften |>
  summarise(.by = c(kolom, gebied), niet_sign = all(!is.na(p_waarde)) & all(p_waarde >= 0.05)) |>
  rename(gebied_label = gebied)

for (thema_sel in levels(var_lijst$thema)) {
  d <- afkreeften_plotdata |>
    filter(thema == thema_sel) |>
    left_join(select(labels_afkreeften, kolom, facet), by = "kolom")
  if (nrow(d) == 0) next
  tekst_niet_sign <- d |>
    distinct(kolom, facet, gebied_label) |>
    left_join(niet_sign_afkreeften |> mutate(gebied_label = factor(gebied_label, levels = levels(d$gebied_label))),
              by = c("kolom", "gebied_label")) |>
    mutate(niet_sign = coalesce(niet_sign, FALSE))
  p <- ggplot(d, aes(x = afkreeften, y = verandering)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.4) +
    geom_boxplot(outlier.shape = NA, fill = "grey95", colour = "grey55", width = 0.5) +
    geom_point(
      aes(colour = behandeling_label),
      position = position_jitter(width = 0.12, height = 0, seed = 1), size = 2
    ) +
    laag_geen_effect(geen_effect, width = 0.12) +
    laag_niet_significant(tekst_niet_sign) +
    scale_colour_manual(values = kleuren_afkreeften) +
    scale_shape_manual(values = shapes_geen_effect, na.translate = FALSE) +
    facet_grid(facet ~ gebied_label, scales = "free_y") +
    labs(
      title = paste0("Afkreeften - ", thema_sel, ": Ronde Hoep en Oukoop"),
      subtitle = paste0(
        "Ronde Hoep: tussen sloten (sloot random), gecorrigeerd voor afrastering.\n",
        "Oukoop: binnen sloot-paren (paar vast, sloot random), gecorrigeerd voor maaibeheer.\n",
        "× = sloot waar bij de veldbeoordeling in 2026 geen effect is gezien (alleen ter duiding; telt mee in de toets)."
      ),
      caption = caption_niet_sign,
      x = NULL, y = "Verandering (laatste − eerste meetjaar)", colour = "Behandeling", shape = NULL
    ) +
    theme_wp2 +
    theme(strip.text.y = element_text(angle = 0))
  print(p)
  sla_figuur_op(p, paste0("afkreeften_", thema_sel), width = 12, height = 2.6 * n_distinct(d$kolom) + 2)
}

# 15. Spaarnwoude: nutriënten in de oever bij wel/geen NVO ------------------------
# Oevermonsters (OR_25 en OR_50) zijn in 2024 en 2026 genomen. Twee toetsen:
#  - verandering 2024 -> 2026 ~ NVO + afrastering, met sloot als blok
#  - niveau per jaar ~ NVO x jaar + afrastering, met sloot en locatie random
# NVO volgt de kolom Behandeling: in SW_3 hebben de locaties met NVO in de
# SlootID volgens Behandeling geen NVO (R), dus NVO zit alleen in SW_4 en SW_5.
oever_nutrienten <- tribble(
  ~naam,                  ~kolom,                       ~eenheid,
  "P-AL oever (OR_25)",   "P-AL mg p2o5/100g_OR_25",    "mg P2O5/100g",
  "N-mineraal (OR_25)",   "N_mineraal_OR_25",           "mg/kg",
  "P-CaCl2 (OR_25)",      "P_CC_mg/kg_OR_25",           "mg/kg",
  "NH4-N CaCl2 (OR_25)",  "N-NH4_CC_mg/kg_OR_25",       "mg/kg",
  "NO3-N CaCl2 (OR_25)",  "N-NO3_CC_mg/kg_OR_25",       "mg/kg",
  "K-CaCl2 (OR_25)",      "K_CC_mg/kg_OR_25",           "mg/kg",
  "organische stof (OR_25)", "OS_perc_OR_25",           "%",
  "P-AL oever (OR_50)",   "P-AL mg p2o5/100g_OR_50",    "mg P2O5/100g",
  "N-mineraal (OR_50)",   "N_mineraal_OR_50",           "mg/kg",
  "P-CaCl2 (OR_50)",      "P_CC_mg/kg_OR_50",           "mg/kg"
) |>
  filter(kolom %in% names(wp2)) |>
  mutate(thema = "oevernutriënten", label = paste0(naam, " (", eenheid, ")"))

# SW_5 (alleen contrast NVO / geen NVO) doet hier wel mee, zie sectie 2e; SW_3
# niet, want daar heeft geen enkel vak NVO.
wp2_sw <- voeg_behandelingsfactoren_toe(wp2_compleet) |>
  filter(gebied == "SW", !sloot_nummer %in% setdiff(sloten_zonder_contrast, sloten_nvo_contrast))
sw_verandering <- bereken_verandering_lang(wp2_sw, oever_nutrienten, groep_cols)

sw_niveau <- wp2_sw |>
  as_tibble() |>
  select(SlootID, sloot_nummer, jaar, Behandeling, nvo, afrastering, all_of(oever_nutrienten$kolom)) |>
  mutate(across(all_of(oever_nutrienten$kolom), as.numeric)) |>
  pivot_longer(all_of(oever_nutrienten$kolom), names_to = "kolom", values_to = "waarde") |>
  filter(!is.na(waarde)) |>
  left_join(oever_nutrienten, by = "kolom") |>
  mutate(jaar_f = factor(jaar))

resultaten_nvo_sw <- bind_rows(
  toets_per_variabele(
    sw_verandering, model_termen = "nvo", toets_termen = "nvo",
    covariaten = "afrastering", blok = "sloot_nummer"
  ) |>
    mutate(toets = "verandering 2024 -> 2026"),
  toets_per_variabele(
    sw_niveau, model_termen = "nvo * jaar_f", toets_termen = c("nvo", "nvo:jaar_f"),
    covariaten = "afrastering", random = "(1 | sloot_nummer) + (1 | SlootID)", respons = "waarde"
  ) |>
    mutate(toets = "niveau per jaar")
) |>
  mutate(p_bh = p.adjust(p_waarde, method = "BH"), .by = c(toets, term))
print(resultaten_nvo_sw |> select(toets, naam, term, schatting, p_waarde, p_bh, opzet), n = 60)

labels_nvo <- resultaten_nvo_sw |>
  filter(toets == "verandering 2024 -> 2026") |>
  transmute(
    kolom,
    facet = paste0(label, "\n+ = hoger gehalte, − = lager", "\np verandering = ", p_label(p_waarde)),
    niet_sign = !is.na(p_waarde) & p_waarde >= 0.05
  )

plot_nvo_sw <- sw_niveau |>
  left_join(labels_nvo, by = "kolom") |>
  ggplot(aes(x = jaar_f, y = waarde, colour = nvo)) +
  geom_line(aes(group = SlootID), alpha = 0.35) +
  geom_point(aes(shape = afrastering), size = 2, alpha = 0.8) +
  stat_summary(aes(group = nvo), fun = median, geom = "line", linewidth = 1.2) +
  laag_niet_significant(select(labels_nvo, facet, niet_sign)) +
  scale_colour_manual(values = c("zonder NVO" = okabe_ito[1], "met NVO" = okabe_ito[5])) +
  facet_wrap(~facet, scales = "free_y", ncol = 5) +
  labs(
    title = "Spaarnwoude - nutriënten in de oever bij wel/geen NVO",
    subtitle = paste0(
      "Dunne lijn = locatie, dikke lijn = mediaan per groep. ",
      "p = NVO-effect op de verandering 2024 → 2026 (lm met sloot als blok, gecorrigeerd voor afrastering)."
    ),
    caption = caption_niet_sign,
    x = "Jaar", y = "Waarde (eenheid per paneel)", colour = "NVO", shape = "Afrastering"
  ) +
  theme_wp2
print(plot_nvo_sw)
sla_figuur_op(plot_nvo_sw, "spaarnwoude_nvo_oevernutrienten", width = 16, height = 8)

# 16. Peilverhoging binnen een gebied ---------------------------------------------
# Vergelijking van sloten met en zonder peilverhoging binnen hetzelfde gebied.
# Peilverhoging geldt per sloot, dus de vergelijking is tussen sloten (sloot
# random), gecorrigeerd voor basisbeheer. Alleen gebieden met beide niveaus.
gebieden_peil <- wp2_verandering |>
  distinct(gebied, sloot_nummer, peilverhoging) |>
  summarise(.by = gebied, n_niveaus = n_distinct(peilverhoging)) |>
  filter(n_niveaus == 2) |>
  pull(gebied)

resultaten_peil <- tibble()
if (length(gebieden_peil) == 0) {
  message(
    "Peilverhoging: in geen enkel gebied komen sloten met én zonder peilverhoging voor. ",
    "Vul de kolom `Vernat?` in wp_locaties_naam_correcties.xlsx, of sloten_peilverhoging_handmatig (sectie 9)."
  )
} else {
  resultaten_peil <- map_dfr(gebieden_peil, \(g) {
    toets_per_variabele(
      filter(wp2_verandering, gebied == g),
      model_termen = "peilverhoging", toets_termen = "peilverhoging",
      covariaten = "behandeling_basis", random = "(1 | sloot_nummer)"
    ) |>
      mutate(gebied = g)
  }) |>
    mutate(p_bh = p.adjust(p_waarde, method = "BH"), .by = gebied)
  print(resultaten_peil |> select(gebied, naam, schatting, p_waarde, p_bh, n_sloten), n = 60)

  niet_sign_peil <- resultaten_peil |>
    summarise(.by = c(kolom, gebied), niet_sign = all(!is.na(p_waarde)) & all(p_waarde >= 0.05))

  for (thema_sel in levels(var_lijst$thema)) {
    d_peil <- wp2_verandering |> filter(thema == thema_sel, gebied %in% gebieden_peil)
    tekst_peil <- d_peil |>
      distinct(kolom, label_facet_smal, gebied) |>
      left_join(niet_sign_peil, by = c("kolom", "gebied")) |>
      mutate(niet_sign = coalesce(niet_sign, FALSE))
    p <- d_peil |>
      ggplot(aes(x = peilverhoging, y = verandering)) +
      geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.4) +
      geom_boxplot(outlier.shape = NA, fill = "grey95", colour = "grey55", width = 0.5) +
      geom_point(aes(colour = sloot_nummer, shape = behandeling_basis),
                 position = position_jitter(width = 0.12, height = 0, seed = 1), size = 2) +
      laag_niet_significant(tekst_peil) +
      scale_colour_manual(values = sloot_kleuren) +
      facet_grid(label_facet_smal ~ gebied, scales = "free_y") +
      labs(
        title = paste0("Peilverhoging - ", thema_sel, ": sloten met en zonder peilverhoging binnen een gebied"),
        caption = paste(caption_niet_sign, "p voor alle sloten."),
        x = NULL, y = "Verandering (laatste − eerste meetjaar)", colour = "Sloot", shape = "Basisbeheer"
      ) +
      theme_wp2 +
      theme(strip.text.y = element_text(angle = 0))
    print(p)
    sla_figuur_op(p, paste0("peilverhoging_", thema_sel), width = 12, height = 8)
  }
}

# 17. Beheerparameters: maaimaand en baggermaand -----------------------------------
# Uit de beheerenquête: Maaimaanden_oever / Maaimaanden_sloot (bv. "6,8,9") en
# Baggermoment_maand (bv. 7.5 = eind juli). We gebruiken de eerste genoemde
# maand per locatie (eerste jaar waarin ingevuld). Aanname: 0 en
# "n.v.t./onbekend" betekenen niet gemaaid/gebaggerd of onbekend -> NA.
#' Eerste maand uit een beheerveld
#'
#' @param x Vector met maandcodes als tekst of getal, bv. "6,8,9" of 7.5.
#' @return Numerieke vector met de eerste maand; 0 en tekst zonder getal worden NA.
eerste_maand <- function(x) {
  m <- suppressWarnings(as.numeric(str_extract(as.character(x), "\\d+(\\.\\d+)?")))
  replace(m, m == 0, NA)
}

# Eerste niet-lege waarde (NA als er geen is)
eerste_niet_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) > 0) x[1] else NA_real_
}

beheer_per_locatie <- wp2 |>
  as_tibble() |>
  arrange(jaar) |>
  summarise(
    .by = SlootID,
    maaimaand_oever = eerste_niet_na(eerste_maand(Maaimaanden_oever)),
    maaimaand_sloot = eerste_niet_na(eerste_maand(Maaimaanden_sloot)),
    baggermaand     = eerste_niet_na(eerste_maand(Baggermoment_maand))
  )

# Overzicht voor de bijlage: per gebied en behandeling de maaimaanden, de
# baggermaand en de frequenties uit de beheerenquête (alleen 2024 ingevuld).
# Per kolom de voorkomende waarden, gescheiden door "/"; "n.v.t./onbekend"
# telt als niet ingevuld. Bij maanden betekent 0 "niet van toepassing", bij
# frequenties is 0 een echte waarde (niet gemaaid of gebaggerd).
waarden_tekst <- function(x, nul_is_leeg = FALSE) {
  x <- trimws(as.character(x))
  x <- x[!is.na(x) & x != "" & !str_detect(tolower(x), "n.v.t|onbekend")]
  if (nul_is_leeg) x <- x[x != "0"]
  x <- str_replace_all(x, "[.]", ",")
  if (length(x) == 0) "–" else paste(sort(unique(x)), collapse = " / ")
}
overzicht_beheer <- wp2 |>
  as_tibble() |>
  filter(!is.na(Maaifrequentie_oever_per_jaar) | !is.na(Maaimaanden_oever) | !is.na(Baggermoment_maand)) |>
  summarise(
    .by = c(gebied, Behandeling),
    n_vakken = n_distinct(SlootID),
    maaimaanden_oever = waarden_tekst(Maaimaanden_oever, nul_is_leeg = TRUE),
    maaifrequentie_oever = waarden_tekst(Maaifrequentie_oever_per_jaar),
    maaimaanden_sloot = waarden_tekst(Maaimaanden_sloot, nul_is_leeg = TRUE),
    maaifrequentie_bvz = waarden_tekst(Maaifrequentie_bemestingsvrije_zone_per_jaar),
    # vraag: wordt het perceel even vaak gemaaid als de bemestingsvrije zone/oever?
    perceel_even_vaak = waarden_tekst(Maaifrequentie_perceel_per_jaar),
    baggermaand = waarden_tekst(Baggermoment_maand, nul_is_leeg = TRUE),
    baggerfrequentie = waarden_tekst(Baggerfrequentie_per_jaar)
  ) |>
  arrange(gebied, Behandeling)
print(overzicht_beheer, n = 60)

beheerparameters <- c("maaimaand_oever", "maaimaand_sloot", "baggermaand")
wp2_verandering_beheer <- wp2_verandering |> left_join(beheer_per_locatie, by = "SlootID")

# Per beheerparameter en variabele: verandering ~ maand + basisbeheer, met
# gebied en sloot random. Alleen locaties waar de maand bekend is.
resultaten_beheer <- map_dfr(beheerparameters, \(parameter) {
  d <- wp2_verandering_beheer |> filter(!is.na(.data[[parameter]]))
  toets_per_variabele(
    d, model_termen = parameter, toets_termen = parameter,
    covariaten = "behandeling_basis", random = "(1 | gebied) + (1 | sloot_nummer)"
  ) |>
    mutate(parameter = parameter)
}) |>
  mutate(p_bh = p.adjust(p_waarde, method = "BH"), .by = parameter)
print(resultaten_beheer |> select(parameter, naam, schatting, p_waarde, p_bh, n_locaties), n = 80)

for (parameter in beheerparameters) {
  labels_par <- resultaten_beheer |>
    filter(.data$parameter == !!parameter) |>
    transmute(
      kolom,
      facet = paste0(met_richting(label, 35), "\np = ", p_label(p_waarde), " (helling ", signif(schatting, 2), "/maand)"),
      niet_sign = !is.na(p_waarde) & p_waarde >= 0.05
    )
  p <- wp2_verandering_beheer |>
    filter(!is.na(.data[[parameter]])) |>
    left_join(labels_par, by = "kolom") |>
    ggplot(aes(x = .data[[parameter]], y = verandering)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.4) +
    geom_smooth(method = "lm", formula = y ~ x, colour = "grey40", linewidth = 0.6, se = FALSE) +
    geom_point(aes(colour = behandeling_basis, shape = gebied),
               position = position_jitter(width = 0.15, height = 0, seed = 1), size = 2) +
    laag_niet_significant(select(labels_par, facet, niet_sign)) +
    scale_colour_manual(values = kleuren_basis) +
    facet_wrap(~facet, scales = "free_y", ncol = 5) +
    labs(
      title = paste0("WP2 - verandering tegen ", str_replace_all(parameter, "_", " ")),
      subtitle = "p en helling uit gemengd model: verandering ~ maand + basisbeheer + (1 | gebied) + (1 | sloot).",
      caption = caption_niet_sign,
      x = "Maand", y = "Verandering (laatste − eerste meetjaar)", colour = "Basisbeheer", shape = "Gebied"
    ) +
    theme_wp2
  print(p)
  sla_figuur_op(p, paste0("beheer_", parameter), width = 18, height = 16)
}

# 18. Oeverbreedte en breedte zone 2a/2b: niveau per jaar ---------------------------
# De verandering in oeverbreedte zit al in de thema-figuren (oevermorfologie).
# Hier ook het niveau per jaar, omdat verschillen tussen behandelingen in de
# breedte zelf (niet alleen in de verandering) relevant zijn. Toets:
# breedte ~ basisbeheer x jaar + (1 | gebied) + (1 | sloot) + (1 | locatie).
breedte_lang <- wp2 |>
  as_tibble() |>
  select(SlootID, sloot_nummer, gebied, jaar, behandeling_basis, effect_categorie,
         `oever totaal (2a + 2b)` = oeverbreedte_2a2b_cm,
         `zone 2a` = oeverzone_2a_breedte_cm, `zone 2b` = oeverzone_2b_breedte_cm) |>
  pivot_longer(c(`oever totaal (2a + 2b)`, `zone 2a`, `zone 2b`), names_to = "naam", values_to = "waarde") |>
  filter(!is.na(waarde)) |>
  mutate(thema = "oeverbreedte", label = paste0(naam, " (cm)"), kolom = naam, jaar_f = factor(jaar))

resultaten_breedte <- toets_per_variabele(
  breedte_lang, model_termen = "behandeling_basis * jaar_f",
  toets_termen = c("behandeling_basis", "behandeling_basis:jaar_f"),
  random = "(1 | gebied) + (1 | sloot_nummer) + (1 | SlootID)", respons = "waarde"
)
print(resultaten_breedte |> select(naam, term, p_waarde, n_locaties))

labels_breedte <- resultaten_breedte |>
  mutate(term = if_else(term == "behandeling_basis", "basisbeheer", "basisbeheer×jaar")) |>
  summarise(
    .by = naam,
    facet = paste0(naam[1], " (cm)\n", paste0(term, " p = ", p_label(p_waarde), collapse = "; ")),
    niet_sign = all(!is.na(p_waarde)) & all(p_waarde >= 0.05)
  )

# "niet significant" alleen in de eerste gebiedskolom (de toets geldt voor alle gebieden samen)
tekst_breedte <- labels_breedte |>
  mutate(gebied = sort(unique(breedte_lang$gebied))[1])

plot_breedte <- breedte_lang |>
  left_join(select(labels_breedte, naam, facet), by = "naam") |>
  ggplot(aes(x = jaar_f, y = waarde, colour = behandeling_basis)) +
  geom_line(aes(group = SlootID, alpha = effect_categorie == "geen effect"), linewidth = 0.4) +
  laag_niet_significant(tekst_breedte) +
  stat_summary(aes(group = behandeling_basis), fun = median, geom = "line", linewidth = 1.2) +
  stat_summary(aes(group = behandeling_basis), fun = median, geom = "point", size = 2.3) +
  scale_colour_manual(values = kleuren_basis) +
  scale_alpha_manual(values = c(`FALSE` = 0.45, `TRUE` = 0.15), guide = "none") +
  facet_grid(facet ~ gebied, scales = "free_y") +
  labs(
    title = "WP2 - Oeverbreedte en breedte zone 2a/2b per jaar en basisbeheer",
    subtitle = paste0(
      "Dunne lijn = locatie, dikke lijn = mediaan per gebied en basisbeheer. p uit gemengd model met gebied, sloot en locatie random.\n",
      "Vage dunne lijn = sloot waar bij de veldbeoordeling in 2026 geen effect is gezien (alleen ter duiding; telt mee in mediaan en toets)."
    ),
    caption = paste(caption_niet_sign, "Toets voor alle gebieden samen; label in de eerste kolom."),
    x = "Jaar", y = "Breedte (cm)", colour = "Basisbeheer"
  ) +
  theme_wp2 +
  theme(strip.text.y = element_text(angle = 0))
print(plot_breedte)
sla_figuur_op(plot_breedte, "oeverbreedte_zone2a_2b", width = 16, height = 10)

# Tabellen wegschrijven naast de figuren
sla_tabel_op(toetsen_thema, "tabel_thema_verandering_toets_paarsgewijs")
sla_tabel_op(proefopzet_gebied, "tabel_proefopzet_gebied")
sla_tabel_op(variantie_decompositie, "tabel_niveaus_variantiedecompositie")
sla_tabel_op(resultaten_sloot_covariaat, "tabel_sloot_covariaten_variantie")
sla_tabel_op(meet_variantie, "tabel_meetomstandigheden_variantie")
sla_tabel_op(redox_temperatuur, "tabel_redox_temperatuur")
sla_tabel_op(meetronden, "tabel_meetronden")
sla_tabel_op(meetdagen_sloot, "tabel_meetdagen_sloot")
sla_tabel_op(redox_opvallend, "tabel_redox_opvallend")
sla_tabel_op(gebied_verandering_niveau, "tabel_gebied_verandering_niveau")
sla_tabel_op(stabiliteit_samenvatting, "tabel_stabiliteit_samenvatting")
sla_tabel_op(stabiliteit_gebied, "tabel_stabiliteit_gebied")
sla_tabel_op(slibdikte_samenhang, "tabel_slibdikte_samenhang")
sla_tabel_op(profiel_nap_gebied, "tabel_profiel_nap_gebied")
sla_tabel_op(profiel_nap_samenhang, "tabel_profiel_nap_samenhang")
sla_tabel_op(talud_contrast_sloot, "tabel_talud_contrast_sloot")
sla_tabel_op(talud_contrast_gebied, "tabel_talud_contrast_gebied")
sla_tabel_op(talud_samenhang, "tabel_talud_vernatting_samenhang")
sla_tabel_op(verruiging_2b, "tabel_verruiging_zone2b")
sla_tabel_op(zone2a_af, "tabel_zone2a_afrastering")
sla_tabel_op(behandeling_per_vak, "tabel_behandeling_per_vak")
sla_tabel_op(uitgang_vergelijking, "tabel_uitgangssituatie_waargenomen_effect")
sla_tabel_op(uitgang_veentype, "tabel_uitgangssituatie_veentype")
sla_tabel_op(toets_alles, "tabel_niveau_alles_toets_basisbeheer")
sla_tabel_op(basisbeheer_schatting, "tabel_basisbeheer_verandering")
sla_tabel_op(variatie_binnen_sloot, "tabel_niveau_sloot_variatie_binnen_sloot")
sla_tabel_op(resultaten_hw, "tabel_contrast_hegewarren_factorial")
sla_tabel_op(draagkracht_laag_gebied, "tabel_draagkracht_laag_gebied")
sla_tabel_op(resultaten_contrasten_gebied, "tabel_contrasten_per_gebied")
sla_tabel_op(resultaten_afkreeften, "tabel_afkreeften_RH_Oukoop")
if (exists("resultaten_maaimaand_oukoop")) {
  sla_tabel_op(bind_rows(resultaten_maaimaand_oukoop, resultaten_mr_oukoop), "tabel_oukoop_maaimaand_minimaal_regulier")
  if (exists("maaibeheer_oukoop")) sla_tabel_op(maaibeheer_oukoop, "tabel_oukoop_maaibeheer_beginwaarde")
  if (!is.null(kreeftvangst_oukoop)) {
    sla_tabel_op(kreeftvangst_oukoop, "tabel_oukoop_kreeftvangst")
    sla_tabel_op(kreeftvangst_effect, "tabel_oukoop_kreeftvangst_effect")
    sla_tabel_op(kreeft_paar, "tabel_oukoop_kreeftvangst_paren")
  }
}
sla_tabel_op(resultaten_nvo_sw, "tabel_spaarnwoude_nvo_oevernutrienten")
sla_tabel_op(resultaten_peil, "tabel_peilverhoging")
sla_tabel_op(overzicht_beheer, "tabel_beheer_maai_baggermaand")
sla_tabel_op(resultaten_beheer, "tabel_beheer_toets_maai_baggermaand")
sla_tabel_op(resultaten_breedte, "tabel_oeverbreedte_niveau")

# =============================================================================
# Samenvatting van wat dit script oplevert:
# - dekking: overzicht 2024/2025 dekking per SlootID
# - behandelingen_per_sloot: aantal behandelingen per sloot/jaar
# - top_predictoren_per_target: XGBoost-predictorenselectie per target
# - plots_thema: per thema (profiel, waterbodem, oever, vegetatie) de
#   verandering per SlootID tussen eerste en laatste meetjaar, tegen
#   basisbeheer (x-as) met vorm voor afrastering/NVO
# - toetsen_thema: algemene en paarsgewijze toets regulier/minimaal/afrastering
#   (gemengd model met gebied en sloot random, paren Holm-gecorrigeerd)
# - resultaten_behandeling: hoofdtoets Behandeling-effect per target
#   (gemengd model met SlootID als blok, jaar als covariaat)
# - resultaten_ancova: Behandeling x top-predictor interacties (BH-gecorrigeerd)
# - plots_ancova: regressielijnen per Behandeling voor de (meest) significante
#   interacties, om te zien in welk bereik van de covariaat de behandelingen
#   uiteenlopen
# - plot_profiel_draagkracht: indringingsweerstand-profiel over diepte in de
#   oever, gefacet per jaar, met significantie-indicator per dieptebin
# - variantie_decompositie / plot_variantie: aandeel van de variatie in de
#   verandering dat het basisbeheer verklaart, behandeling x gebied, gebied,
#   sloot en locatie binnen sloot, met een
#   toelichtingstabel onder de figuur
# - resultaten_sloot_covariaat / plot_sloot_covariaat: afname van het
#   slootaandeel in de verandering van de nutriënten in oever en slib als
#   bodem, beheer of meetomstandigheden (temperatuur, waterdiepte) als
#   covariaat worden toegevoegd
# - meet_variantie / redox_temperatuur / meetronden / plot_redox_temp:
#   variatie in temperatuur, waterdiepte en redox per meetronde en de
#   samenhang tussen temperatuur, redox en P in het slib
# - niveau_alles/gebied/sloot-figuren per thema (incl. oevermorfologie) en
#   variatie_binnen_sloot (bereik en SD tussen behandelingen per sloot)
# - resultaten_hw: full factorial basisbeheer x afrastering in Hegewarren
# - resultaten_contrasten_gebied + heatmap: minimaal vs regulier en wel/geen
#   afrastering per gebied
# - resultaten_afkreeften: afkreeften in Ronde Hoep en Oukoop;
#   resultaten_maaimaand_oukoop: maaien in juli vs augustus in Oukoop
# - resultaten_nvo_sw: oevernutriënten bij wel/geen NVO in Spaarnwoude
# - resultaten_peil: peilverhoging binnen een gebied (als `Vernat?` gevuld is)
# - resultaten_beheer: maaimaand oever/sloot en baggermaand als covariaat
# - resultaten_breedte / plot_breedte: oeverbreedte en zone 2a/2b per jaar
# Alle tabellen staan ook als csv in figuren_dir.
# =============================================================================
