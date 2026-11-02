# =============================================================================
# WP2: dwarsprofiel en diepte per sloot
# =============================================================================
# Per sloot twee figuren, met per behandelingsvak een rij en per meetjaar een
# kolom:
#  1. sloot_profiel_<sloot>: de gemeten dwarsdoorsnede (oever en water) in m
#     NAP, met water (lichtblauw), slib (bruin) en vaste bodem (lichtgroen),
#     in dezelfde stijl als visualise_profiel() in functions/functions_veest.R.
#     De x-as is de afstand tot de eerste waterlijn, zodat de jaren goed te
#     vergelijken zijn. Het hele profiel wordt getoond, van oever tot oever;
#     de SlootID hoort bij de behandelde oever (sectie_2), maar geldt hier voor
#     het hele profiel. Waterpunten waar de GPS-stok vermoedelijk op de vaste
#     bodem stond (markeer_stok_op_vaste_bodem), zijn rood gemarkeerd.
#  2. sloot_diepte_<sloot>: overzicht van maximale waterdiepte, maximale
#     slibdikte en doorzicht per vak en jaar, in dezelfde stijl als de figuur
#     "Doorzicht, waterdiepte & slibdikte" in visualisatiesEnExport.R, met
#     dezelfde maten als in de analyse (max_wtd, max_slib). De maxima kunnen
#     van verschillende profielpunten komen.
# Beide komen in bijlage D van het rapport. Daarnaast één figuur voor de
# hoofdtekst:
#  3. gebied_diepte_doorzicht: dezelfde maten gemiddeld per gebied en
#     basisbeheer (regulier, minimaal, afrastering), voor 2024 en 2026, met de
#     bandbreedte (minimum-maximum) over de vakken.
#
# Gebruik: eerst wp2_analyse.R draaien (wp2_compleet, figuren_dir,
# sla_figuur_op, sla_tabel_op en workspace moeten bestaan). Het object profiel komt uit
# Processed_data_workspace.RData; als het niet in de sessie staat, wordt het
# in een eigen omgeving geladen, zodat objecten in de sessie niet
# overschreven worden.
# =============================================================================

benodigd <- c("wp2_compleet", "figuren_dir", "sla_figuur_op", "sla_tabel_op", "workspace")
ontbreekt <- benodigd[!vapply(benodigd, exists, logical(1))]
if (length(ontbreekt) > 0) {
  stop("Draai eerst wp2_analyse.R; ontbrekende objecten: ", paste(ontbreekt, collapse = ", "))
}

if (!exists("profiel")) {
  ws_omgeving <- new.env()
  load(paste0(workspace, "Processed_data_workspace.RData"), envir = ws_omgeving)
  profiel <- ws_omgeving$profiel
  rm(ws_omgeving)
}

kleuren_profiel <- c(water = "lightblue", slib = "brown", `vaste bodem` = "lightgreen")
kleuren_diepte <- c(
  `diepte tot vaste bodem (m)` = "brown",
  `maximale waterdiepte (m)` = "skyblue",
  `doorzicht (m)` = "darkblue"
)

# Behandeling per vak (laatste meetjaar) voor de rijlabels
vak_label <- as.data.table(wp2_compleet)[order(jaar), .(
  sloot_nummer = sloot_nummer[1],
  gebied = gebied[1],
  label = paste0(SlootID[1], "\n(", last(Behandeling), ")")
), by = SlootID]

# Profielpunten van oever en water, met de afstand tot de waterlijn. De
# SlootID staat alleen op de punten van de behandelde oever; die geldt hier
# voor het hele profiel (ID).
profiel_wp2 <- copy(as.data.table(profiel))
if (!"stok_op_vaste_bodem" %in% names(profiel_wp2)) profiel_wp2[, stok_op_vaste_bodem := FALSE]
profiel_wp2[, SlootID := SlootID[!is.na(SlootID)][1], by = ID]
profiel_wp2 <- profiel_wp2[
  SlootID %in% vak_label$SlootID & sectie %in% c("oever", "water"),
  .(SlootID, jaar, ID, Puntnummer, sectie, dist, z, wl, slib, Opmerking, stok_op_vaste_bodem)
]
profiel_wp2[, dist_waterlijn := dist - min(dist[!is.na(wl)]), by = ID]
profiel_wp2[, vaste_bodem := z - slib]
profiel_wp2 <- merge(profiel_wp2, vak_label, by = "SlootID")
setorder(profiel_wp2, ID, dist)

#' Dwarsprofiel van alle vakken van één sloot
#'
#' @param d data.table met de profielpunten van één sloot (kolommen
#'   dist_waterlijn, z, wl, vaste_bodem, label, jaar, ID).
#' @param sloot Slootnummer voor de titel.
#' @return ggplot-object met per vak een rij en per jaar een kolom.
plot_profiel_sloot <- function(d, sloot) {
  zmin <- min(d$vaste_bodem, na.rm = TRUE) - 0.1
  ggplot(d, aes(x = dist_waterlijn, group = ID)) +
    geom_ribbon(aes(ymin = zmin, ymax = vaste_bodem, fill = "vaste bodem"), alpha = 0.5) +
    geom_ribbon(aes(ymin = vaste_bodem, ymax = z, fill = "slib"), alpha = 0.5) +
    geom_ribbon(data = d[!is.na(wl)], aes(ymin = z, ymax = wl, fill = "water"), alpha = 0.5) +
    geom_line(aes(y = z)) +
    geom_line(aes(y = vaste_bodem)) +
    geom_line(data = d[!is.na(wl)], aes(y = wl)) +
    geom_point(data = d[stok_op_vaste_bodem %in% TRUE], aes(y = z), colour = "red", size = 1.5) +
    scale_fill_manual(values = kleuren_profiel, breaks = names(kleuren_profiel), name = NULL) +
    facet_grid(label ~ jaar) +
    coord_fixed(ratio = 2) +
    labs(
      title = paste0("Dwarsprofiel oever en sloot ", sloot),
      subtitle = "Per behandelingsvak (rij) en meetjaar (kolom). Verticaal 2× overdreven.",
      x = "Afstand tot de waterlijn (m; negatief = oever)", y = "Hoogte (m NAP)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      strip.text.y = element_text(angle = 0, hjust = 0),
      strip.text = element_text(face = "bold"),
      panel.border = element_rect(colour = "grey80", fill = NA),
      legend.position = "bottom"
    )
}

#' Overzicht van maximale waterdiepte, slibdikte en doorzicht per vak
#'
#' @param d data.table met per vak en jaar max_wtd, max_slib,
#'   doorzicht2_mid_m, label en jaar voor één sloot.
#' @param sloot Slootnummer voor de titel.
#' @return ggplot-object met staven naar beneden per vak, per jaar een kolom.
plot_diepte_sloot <- function(d, sloot) {
  lang <- rbind(
    d[, .(label, jaar, maat = "diepte tot vaste bodem (m)", waarde = max_wtd + max_slib)],
    d[, .(label, jaar, maat = "maximale waterdiepte (m)", waarde = max_wtd)],
    d[, .(label, jaar, maat = "doorzicht (m)", waarde = doorzicht2_mid_m)]
  )[!is.na(waarde)]
  lang[, maat := factor(maat, levels = names(kleuren_diepte))]
  # Staven over elkaar heen, de grootste diepte achteraan; doorzicht smaller,
  # zodat de waterdiepte zichtbaar blijft als het doorzicht tot de bodem reikt
  ggplot(lang, aes(x = label, y = -waarde, fill = maat)) +
    geom_col(data = lang[maat != "doorzicht (m)"], position = "identity", width = 0.7, alpha = 0.85) +
    geom_col(data = lang[maat == "doorzicht (m)"], width = 0.3, alpha = 0.9) +
    scale_fill_manual(values = kleuren_diepte, name = NULL, drop = FALSE) +
    facet_grid(. ~ jaar) +
    labs(
      title = paste0("Doorzicht, waterdiepte en slibdikte ", sloot),
      subtitle = "Maxima over het profiel (max_wtd, max_slib); slibdikte = verschil tussen diepte tot vaste bodem en waterdiepte.",
      x = NULL, y = "Diepte onder het wateroppervlak (m)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      strip.text = element_text(face = "bold"),
      panel.border = element_rect(colour = "grey80", fill = NA),
      legend.position = "bottom"
    )
}

diepte_vak <- merge(
  as.data.table(wp2_compleet)[, .(SlootID, jaar, max_wtd, max_slib, doorzicht2_mid_m)],
  vak_label, by = "SlootID"
)

for (s in sort(unique(vak_label$sloot_nummer))) {
  d_prof <- profiel_wp2[sloot_nummer == s]
  n_vak <- uniqueN(vak_label[sloot_nummer == s, SlootID])
  if (nrow(d_prof) > 0) {
    sla_figuur_op(plot_profiel_sloot(d_prof, s), paste0("sloot_profiel_", s),
                  width = 4 + 4 * uniqueN(d_prof$jaar), height = 2 + 2.2 * n_vak)
  }
  d_diepte <- diepte_vak[sloot_nummer == s]
  if (nrow(d_diepte) > 0) {
    sla_figuur_op(plot_diepte_sloot(d_diepte, s), paste0("sloot_diepte_", s),
                  width = 3 + 1.2 * n_vak * uniqueN(d_diepte$jaar), height = 6)
  }
}
cat("Figuren per sloot opgeslagen in", paste0(figuren_dir, "profielen_sloot/"), "\n")

# Overzicht per gebied en basisbeheer (hoofdtekst) -----------------------------
# Gemiddelde over de vakken per gebied, basisbeheer en jaar, met de
# bandbreedte (minimum-maximum over de vakken). Alleen het eerste en laatste
# meetjaar. Basisbeheer volgt de behandeling in dat jaar, zoals behandeling_x
# in wp2_analyse.R (afrastering wint bij combinaties). Alleen vakken uit de
# selectie (MeenemenDataAnalyse_WP2 == "ja"), net als in de rest van de
# vergelijking per basisbeheer.
gebied_namen <- c(
  HW = "Hegewarren", KW = "Krimpenerwaard", RH = "Ronde Hoep",
  SW = "Spaarnwoude", WL = "Bloemerdalergouw", ZG = "Zegveld"
)
basisbeheer_niveaus <- c("regulier", "minimaal", "afrastering")
diepte_vak_beheer <- merge(
  diepte_vak,
  as.data.table(wp2_compleet)[, .(SlootID, jaar, Behandeling, MeenemenDataAnalyse_WP2)],
  by = c("SlootID", "jaar")
)[MeenemenDataAnalyse_WP2 %in% "ja"]
diepte_vak_beheer[, basisbeheer := fcase(
  str_detect(Behandeling, "AF"), "afrastering",
  str_detect(Behandeling, "^M"), "minimaal",
  str_detect(Behandeling, "^R"), "regulier"
)]
diepte_gebied <- rbind(
  diepte_vak_beheer[, .(gebied, basisbeheer, jaar, maat = "diepte tot vaste bodem (m)", waarde = max_wtd + max_slib)],
  diepte_vak_beheer[, .(gebied, basisbeheer, jaar, maat = "maximale waterdiepte (m)", waarde = max_wtd)],
  diepte_vak_beheer[, .(gebied, basisbeheer, jaar, maat = "doorzicht (m)", waarde = doorzicht2_mid_m)]
)[jaar %in% c(2024, 2026) & !is.na(waarde) & !is.na(basisbeheer), .(
  gemiddeld = mean(waarde), minimum = min(waarde), maximum = max(waarde), n_vakken = .N
), by = .(gebied, basisbeheer, jaar, maat)]
diepte_gebied[, `:=`(
  maat = factor(maat, levels = names(kleuren_diepte)),
  basisbeheer = factor(basisbeheer, levels = basisbeheer_niveaus)
)]
setorder(diepte_gebied, gebied, basisbeheer, jaar, maat)
sla_tabel_op(diepte_gebied, "tabel_diepte_doorzicht_gebied")

#' Doorzicht, waterdiepte en diepte tot de vaste bodem per gebied en basisbeheer
#'
#' @param d data.table met per gebied, basisbeheer, jaar en maat het
#'   gemiddelde, minimum en maximum over de vakken (kolommen gebied,
#'   basisbeheer, jaar, maat, gemiddeld, minimum, maximum).
#' @return ggplot-object met per gebied een paneel en per basisbeheer en jaar
#'   een staaf naar beneden; de bandbreedte staat links (vaste bodem), in het
#'   midden (doorzicht) en rechts (waterdiepte) in de staaf.
plot_diepte_gebied <- function(d) {
  d <- copy(d)
  jaren <- sort(unique(d$jaar))
  # Vaste x-posities: per basisbeheer een paar staven (een per jaar) met een
  # tussenruimte, zodat de x-as in elk paneel dezelfde betekenis heeft
  d[, x_staaf := (as.integer(basisbeheer) - 1) * (length(jaren) + 0.6) + match(jaar, jaren)]
  d[, x := x_staaf +
    c(`diepte tot vaste bodem (m)` = -0.25, `maximale waterdiepte (m)` = 0.25, `doorzicht (m)` = 0)[as.character(maat)]]
  posities <- unique(d[, .(x_staaf, jaar)])
  beheer_label <- d[, .(x = mean(range(x_staaf))), by = .(gebied, basisbeheer)]
  kleuren_band <- c(
    `diepte tot vaste bodem (m)` = "#5c1a0b",
    `maximale waterdiepte (m)` = "#1f5f8b",
    `doorzicht (m)` = "#08306b"
  )
  # Staven als in plot_diepte_sloot(); de bandbreedte met een witte rand,
  # zodat die ook binnen een staaf van dezelfde kleur zichtbaar is
  ggplot(d, aes(x = x_staaf, y = -gemiddeld)) +
    geom_col(data = d[maat != "doorzicht (m)"], aes(fill = maat), position = "identity", width = 0.8, alpha = 0.85) +
    geom_col(data = d[maat == "doorzicht (m)"], aes(fill = maat), width = 0.35, alpha = 0.9) +
    geom_errorbar(aes(x = x, ymin = -maximum, ymax = -minimum), colour = "white", width = 0.14, linewidth = 1.4) +
    geom_errorbar(aes(x = x, ymin = -maximum, ymax = -minimum, colour = maat), width = 0.12, linewidth = 0.5) +
    geom_text(data = beheer_label, aes(x = x, y = 0.08, label = basisbeheer), size = 3.3, vjust = 0) +
    scale_fill_manual(values = kleuren_diepte, name = NULL, drop = FALSE) +
    scale_colour_manual(values = kleuren_band, guide = "none") +
    scale_x_continuous(
      breaks = posities$x_staaf, labels = posities$jaar,
      expand = expansion(add = 0.6)
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.03, 0.08))) +
    facet_grid(. ~ gebied, labeller = as_labeller(gebied_namen), scales = "free_x", space = "free_x") +
    labs(
      title = "Doorzicht, waterdiepte en slibdikte per gebied en basisbeheer",
      subtitle = paste(
        "Gemiddelde over de vakken (staaf) en minimum-maximum (lijn: links vaste bodem, midden doorzicht, rechts waterdiepte).",
        "Slibdikte = verschil tussen diepte tot vaste bodem en waterdiepte.",
        sep = "\n"
      ),
      x = NULL, y = "Diepte onder het wateroppervlak (m)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      strip.text = element_text(face = "bold"),
      panel.border = element_rect(colour = "grey80", fill = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_text(size = 9),
      legend.position = "bottom"
    )
}

sla_figuur_op(plot_diepte_gebied(diepte_gebied), "gebied_diepte_doorzicht", width = 16, height = 7)
