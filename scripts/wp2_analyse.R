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
  workspace <- paste0(Sys.getenv("NMI-SITE"), "O 1900 - O 2000/1922.N.23 VeeST vwsloot vd toekomst/05. Data/")
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
sla_figuur_op <- function(plot, naam, width = 10, height = 7, dpi = 300) {
  ggsave(
    filename = paste0(figuren_dir, naam, ".png"),
    plot = plot, width = width, height = height, dpi = dpi
  )
}

if (!exists("abio_proj")) {
  abio_proj <- readRDS(paste0(rds_dir, "abio_proj.rds")) |> as.data.table()
}
abio_proj <- as.data.table(abio_proj)

# Predictorenset uit de XGBoost-analyse (variable importance per target)
xgb_importance <- readRDS(paste0(rds_dir, "all_xgb_importance.rds")) |> as.data.table()

# 1. Filter op WP2 -------------------------------------------------------------
wp2 <- abio_proj[WP == "WP2"]

cat("Aantal rijen WP2:", nrow(wp2), "\n")
cat("Aantal unieke sloten (SlootID) WP2:", uniqueN(wp2$SlootID), "\n")

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
    nh4              = "NH4_µmol/l_PW",
    redox_slib       = "slib_redox_pH7"
  ),
  oever = c(
    pal              = "P-AL mg p2o5/100g_OR_25",
    nmin             = "N_mineraal_OR_25",
    erosie_index     = "erosieindex",
    vernattingsindex = "vernattingsrisico_index",
    draagkracht_oever = "draagkracht_oever"
  ),
  vegetatie = c(
    helofyten        = "Soortensamenstelling Helofyten",
    hydrofyten       = "Soortensamenstelling Hydrofyten",
    oeverindex       = "oeverindex",
    opp_emers        = "oeverzone_2a_emers_m2",
    erosie_index     = "erosieindex"
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
  nh4                = "\u00b5mol/l",
  redox_slib         = "mV",
  nmin               = "mg/kg",
  erosie_index       = "index, -",
  vernattingsindex   = "index, -",
  draagkracht_oever  = "MPa",
  helofyten          = "EKR, -",
  hydrofyten         = "EKR, -",
  oeverindex         = "index, -",
  opp_emers          = "m²"
)

# Check of alle kolommen bestaan; verwijder ontbrekende met waarschuwing
themas <- map(themas, check_cols, data = wp2)

# Verandering per SlootID (laatste jaar - eerste jaar), met behandeling_x /
# behandeling_vorm van het laatste jaar.
bereken_verandering <- function(data, var_kolommen) {
  d_lang <- data |>
    select(SlootID, sloot_nummer, jaar, behandeling_x, behandeling_vorm, all_of(unname(var_kolommen))) |>
    pivot_longer(all_of(unname(var_kolommen)), names_to = "variabele", values_to = "waarde") |>
    mutate(variabele = factor(variabele, levels = unname(var_kolommen), labels = names(var_kolommen))) |>
    filter(!is.na(waarde))

  d_lang |>
    summarise(
      .by = c(SlootID, variabele),
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

# Toets: significant verschil tussen behandeling_x-niveaus in de verandering?
# (ANOVA op verandering ~ behandeling_x; bij <2 niveaus of te weinig data geen toets)
toets_verandering <- function(d_var) {
  d <- d_var |> filter(!is.na(verandering), !is.na(behandeling_x))
  if (n_distinct(d$behandeling_x) < 2 || nrow(d) < 6) return(NA_real_)
  fit <- tryCatch(aov(verandering ~ behandeling_x, data = d), error = function(e) NULL)
  if (is.null(fit)) return(NA_real_)
  tab <- summary(fit)[[1]]
  if (!"behandeling_x" %in% trimws(rownames(tab))) return(NA_real_)
  tab[grep("behandeling_x", rownames(tab)), "Pr(>F)"][1]
}

# Plot per thema: verandering (y) tegen behandeling_x (x), vorm = behandeling_vorm,
# significantie van het thema-brede verschil in titel/subtitel per variabele.
# Facet-labels tonen de eenheid van elke variabele (uit eenheden_var).
plot_thema_verandering <- function(thema_naam, var_kolommen, data) {
  d_var <- bereken_verandering(data, var_kolommen)

  p_waarden <- d_var |>
    group_by(variabele) |>
    group_map(~ tibble(variabele = .y$variabele, p = toets_verandering(.x))) |>
    bind_rows()

  d_var <- d_var |> left_join(p_waarden, by = "variabele") |>
    mutate(
      label_p = ifelse(is.na(p), "n.v.t.",
                        paste0("p = ", formatC(p, digits = 3, format = "g"))),
      eenheid = eenheden_var[as.character(variabele)],
      eenheid = ifelse(is.na(eenheid), "", paste0(", ", eenheid)),
      variabele_label = paste0(variabele, " (", label_p, ")", eenheid)
    )

  ggplot(d_var, aes(x = behandeling_x, y = verandering)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.4) +
    geom_jitter(aes(shape = behandeling_vorm, colour = sloot_nummer), width = 0.15, size = 2, alpha = 0.7) +
    stat_summary(fun = median, geom = "point", size = 3, colour = "black", shape = 18) +
    scale_shape_manual(values = vorm_shapes) +
    scale_colour_manual(values = sloot_kleuren) +
    facet_wrap(~variabele_label, scales = "free_y") +
    labs(
      title = paste0("WP2 - ", thema_naam, ": verandering per SlootID (waarde laatste jaar \u2212 waarde eerste jaar)"),
      subtitle = paste0(
        "Positief = toename t.o.v. eerste meetjaar van de sloot, negatief = afname. Zwarte ruit = mediaan over sloten; kleur = sloot (gebied + sloot_nr).\n",
        "p = p-waarde van een ANOVA (verandering ~ basisbeheer): kans op dit verschil tussen minimaal/regulier/afrastering als er in werkelijkheid geen verschil is; p < 0.05 wijst op een aantoonbaar verschil."
      ),
      x = "Basisbeheer", y = "Verandering (laatste jaar \u2212 eerste jaar; eenheid per paneel)",
      shape = "Toevoeging", colour = "Sloot"
    ) +
    theme_wp2
}

plots_thema <- imap(themas, function(var_kolommen, thema_naam) {
  plot_thema_verandering(thema_naam, var_kolommen, data = wp2)
})

walk(plots_thema, print)
iwalk(plots_thema, function(p, thema_naam) {
  sla_figuur_op(p, paste0("thema_verandering_", thema_naam))
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
           variabele_label = paste0(variabele, eenheid))
  d_mediaan <- d_mediaan |>
    mutate(eenheid = eenheden_var[as.character(variabele)],
           eenheid = ifelse(is.na(eenheid), "", paste0(", ", eenheid)),
           variabele_label = paste0(variabele, eenheid))

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
  sys.load.image(paste0(workspace, "Processed_data_workspace.RData"))
}

penmerge <- as.data.table(penmerge)

pm_oever <- penmerge[
  WP == "WP2" & sectie == "oever" &
    !is.na(Diept) & !is.na(indringingsweerstand) & !is.na(Behandeling) &
    !str_detect(Behandeling, "NVO")
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

# =============================================================================
# Samenvatting van wat dit script oplevert:
# - dekking: overzicht 2024/2025 dekking per SlootID
# - behandelingen_per_sloot: aantal behandelingen per sloot/jaar
# - top_predictoren_per_target: XGBoost-predictorenselectie per target
# - plots_thema: per thema (profiel, waterbodem, oever, vegetatie) de
#   verandering per SlootID tussen eerste en laatste meetjaar, tegen
#   basisbeheer (x-as) met vorm voor afrastering/NVO
# - resultaten_behandeling: hoofdtoets Behandeling-effect per target
#   (gemengd model met SlootID als blok, jaar als covariaat)
# - resultaten_ancova: Behandeling x top-predictor interacties (BH-gecorrigeerd)
# - plots_ancova: regressielijnen per Behandeling voor de (meest) significante
#   interacties, om te zien in welk bereik van de covariaat de behandelingen
#   uiteenlopen
# - plot_profiel_draagkracht: indringingsweerstand-profiel over diepte in de
#   oever, gefacet per jaar, met significantie-indicator per dieptebin
# =============================================================================
