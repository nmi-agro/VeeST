# =============================================================================
# WP2: waarom reageert de oever wel of niet op minimaal beheer / afrastering?
# =============================================================================
# Doel: nagaan bij welke abiotische uitgangssituatie een vak met minimaal beheer
# en/of afrastering beter (of minder slecht) verandert dan het vak met regulier
# beheer in dezelfde sloot. Respons per vak = verandering (laatste - eerste
# meetjaar) in het vak min die in het R-vak van dezelfde sloot; in sloten
# zonder R-vak (bv. Zegveld) M-AF t.o.v. M (dan alleen het effect van
# afrastering). Zo valt een trend die voor de hele sloot geldt weg.
#
# Responsvariabelen: soortensamenstelling helofyten (EKR), structuur van de
# oevervegetatie (deelindicator oeverindex) en breedte zone 2a.
#
# Daarnaast (sectie 6): de vakken met minimaal beheer (M en M-AF) ingedeeld
# naar of structuur en zone 2a toenemen, met per groep de abiotische
# uitgangssituatie (Kruskal-Wallis, ook binnen gebieden).
#
# Toets: Spearman-rangcorrelatie tussen uitgangswaarde (eerste meetjaar) en de
# relatieve respons, over alle vakken en binnen gebieden (beide na centreren op
# het gebiedsgemiddelde), met BH-correctie over de abiotische variabelen.
# Verkennend.
#
# Gebruik: eerst wp2_analyse.R draaien (wp2, theme_wp2, sla_figuur_op,
# sla_tabel_op, gebied_hoofdkleur, okabe_ito en p_label moeten bestaan).
# SW_3/SW_5 en de als ontbrekend gemarkeerde nullen zijn daar al verwerkt.
# =============================================================================

benodigd <- c("wp2", "theme_wp2", "sla_figuur_op", "sla_tabel_op", "gebied_hoofdkleur", "okabe_ito", "p_label", "nl_getal")
ontbreekt <- benodigd[!vapply(benodigd, exists, logical(1))]
if (length(ontbreekt) > 0) {
  stop("Draai eerst wp2_analyse.R; ontbrekende objecten: ", paste(ontbreekt, collapse = ", "))
}

# 1. Respons- en abiotische variabelen ----------------------------------------
# Breedte emers zone 2a: oeverzone_2a_emers_m2 uit main_veest.R (bedekking %
# x breedte zone 2a, in m² per meter oever = breedte in m). Niet de kolom
# oeverzone_2a_emers_m uit de import: die is in 2026 niet gevuld en staat in cm.
respons_vars <- tribble(
  ~naam,             ~kolom,                           ~eenheid,
  "helofyten",       "Soortensamenstelling Helofyten", "EKR, -",
  "structuur",       "structuur",                      "-",
  "breedte zone 2a", "oeverzone_2a_breedte_cm",        "cm"
)

# Veentype ontbreekt bewust: veentype_num is een alfabetische codering (geen
# rangorde) en valt in WP2 vrijwel samen met het gebied.
abiotiek_vars <- tribble(
  ~naam,                 ~kolom,                       ~eenheid,
  "drooglegging",        "drglg",                      "m",
  "waterdiepte",         "max_wtd",                    "m",
  "slibdikte",           "max_slib",                   "m",
  "doorzicht",           "doorzicht2_mid_m",           "m",
  "talud oever",         "tldk_oevrwtr_perc",          "%",
  "onderholling",        "holleoever",                 "cm",
  "draagkracht oever",   "draagkracht_oever",          "MPa",
  "redox slib",          "slib_redox_pH7",             "mV",
  "pH slib",             "slib_pH",                    "-",
  "P-AL waterbodem",     "P-AL mg p2o5/100g_SB",       "mg P2O5/100g",
  "NH4 poriewater",      "NH4_µmol/l_PW",         "µmol/l",
  "Fe/P poriewater",     "feP_PW",                     "-",
  "Cl poriewater",       "Cl_µmol/l_PW",          "µmol/l",
  "EC oppervlaktewater", "water_conductiviteit_uS_cm", "µS/cm",
  "P-AL oever",          "P-AL mg p2o5/100g_OR_25",    "mg P2O5/100g",
  "N-mineraal oever",    "N_mineraal_OR_25",           "mg/kg",
  "organische stof oever", "OS_perc_OR_25",            "%",
  "klei oever",          "Z_CLAY_SA_OR_25",            "%"
) |>
  filter(kolom %in% names(wp2)) |>
  mutate(label = paste0(naam, " (", eenheid, ")"))

# 2. Respons per vak ten opzichte van het referentievak in dezelfde sloot -----
#' Verandering van een responsvariabele per vak (laatste - eerste meetjaar)
#'
#' @param data Data met SlootID, gebied, sloot_nummer, jaar, behandeling_x en `kolom`.
#' @param kolom Kolomnaam van de responsvariabele.
#' @return Tibble met één rij per SlootID met de verandering.
verandering_per_vak <- function(data, kolom) {
  data |>
    select(SlootID, gebied, sloot_nummer, jaar, behandeling_x, Behandeling, waarde = all_of(kolom)) |>
    mutate(waarde = as.numeric(waarde)) |>
    filter(!is.na(waarde), !is.na(behandeling_x)) |>
    arrange(jaar) |>
    summarise(
      .by = SlootID,
      gebied = first(gebied), sloot_nummer = first(sloot_nummer),
      behandeling_x = last(behandeling_x), Behandeling = last(Behandeling),
      jaar_eerste = first(jaar), jaar_laatste = last(jaar),
      verandering = last(waarde) - first(waarde)
    ) |>
    filter(jaar_eerste != jaar_laatste)
}

#' Relatieve respons: verandering in een M- of AF-vak min die in het
#' referentievak van dezelfde sloot
#'
#' Referentie is het gemiddelde van de regulier beheerde vakken (R) in de
#' sloot. Heeft een sloot geen R-vak (bv. Zegveld), dan is de referentie het
#' vak met minimaal beheer zonder afrastering (M) en worden alleen de
#' afgerasterde vakken vergeleken; het verschil meet dan alleen het effect van
#' afrastering.
#'
#' @param v Uitvoer van verandering_per_vak().
#' @return Tibble met de vergeleken vakken, het verschil en de referentie.
relatieve_respons <- function(v) {
  v |>
    mutate(heeft_r = any(behandeling_x == "regulier"), .by = sloot_nummer) |>
    mutate(rol = case_when(
      heeft_r & behandeling_x == "regulier" ~ "referentie",
      heeft_r ~ "vergelijk",
      !heeft_r & behandeling_x == "minimaal" ~ "referentie",
      !heeft_r & behandeling_x == "afrastering" ~ "vergelijk"
    )) |>
    mutate(verandering_ref = mean(verandering[rol %in% "referentie"]), .by = sloot_nummer) |>
    filter(rol %in% "vergelijk", is.finite(verandering_ref)) |>
    mutate(
      verschil = verandering - verandering_ref,
      referentie = if_else(heeft_r, "t.o.v. R-vak", "t.o.v. M-vak (geen R-vak)")
    ) |>
    select(-heeft_r, -rol)
}

respons <- pmap_dfr(respons_vars, \(naam, kolom, eenheid) {
  verandering_per_vak(as_tibble(wp2), kolom) |>
    relatieve_respons() |>
    mutate(respons_naam = naam, .before = 1)
})
print(count(respons, respons_naam, gebied, referentie))

# 3. Abiotische uitgangssituatie per vak ---------------------------------------
# Waarde in het eerste jaar waarin de variabele voor dat vak gemeten is.
abiotiek_start <- as_tibble(wp2) |>
  select(SlootID, jaar, all_of(abiotiek_vars$kolom)) |>
  mutate(across(all_of(abiotiek_vars$kolom), as.numeric)) |>
  pivot_longer(all_of(abiotiek_vars$kolom), names_to = "kolom", values_to = "waarde") |>
  filter(!is.na(waarde)) |>
  arrange(jaar) |>
  summarise(.by = c(SlootID, kolom), waarde_start = first(waarde)) |>
  left_join(select(abiotiek_vars, kolom, abiotiek = naam, abiotiek_label = label), by = "kolom")

respons_abiotiek <- respons |>
  inner_join(abiotiek_start, by = "SlootID", relationship = "many-to-many")

# 4. Samenhang tussen uitgangswaarde en relatieve respons ----------------------
# Spearman over alle vakken, en binnen gebieden: uitgangswaarde en verschil
# eerst gecentreerd op het gebiedsgemiddelde, zodat verschillen tussen
# gebieden niet meetellen. Verkennend; BH over de abiotische variabelen.
#' Spearman-correlatie met p-waarde (NA bij minder dan 6 vakken)
#'
#' @param x,y Numerieke vectoren.
#' @return Lijst met rho en p.
spearman <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 6 || sd(x[ok]) == 0 || sd(y[ok]) == 0) return(list(rho = NA_real_, p = NA_real_))
  ct <- suppressWarnings(cor.test(x[ok], y[ok], method = "spearman", exact = FALSE))
  list(rho = unname(ct$estimate), p = ct$p.value)
}

resultaten_respons <- respons_abiotiek |>
  mutate(
    .by = c(respons_naam, abiotiek, gebied),
    x_binnen = waarde_start - mean(waarde_start), y_binnen = verschil - mean(verschil)
  ) |>
  summarise(
    .by = c(respons_naam, abiotiek, abiotiek_label),
    n_vakken = n(), n_sloten = n_distinct(sloot_nummer), n_gebieden = n_distinct(gebied),
    rho = spearman(waarde_start, verschil)$rho, p_waarde = spearman(waarde_start, verschil)$p,
    rho_binnen = spearman(x_binnen, y_binnen)$rho, p_binnen = spearman(x_binnen, y_binnen)$p
  ) |>
  mutate(
    .by = respons_naam,
    p_bh = p.adjust(p_waarde, method = "BH"), p_bh_binnen = p.adjust(p_binnen, method = "BH")
  ) |>
  arrange(respons_naam, p_waarde)
print(resultaten_respons, n = 80)

# 5. Figuren ------------------------------------------------------------------
# 5a. Per responsvariabele: relatieve respons tegen de uitgangswaarde
plot_respons_abiotiek <- function(respons_sel) {
  d <- respons_abiotiek |> filter(respons_naam == respons_sel)
  labels <- resultaten_respons |>
    filter(respons_naam == respons_sel) |>
    transmute(abiotiek, facet = paste0(
      abiotiek_label, "\nρ = ", nl_getal(rho, 2), " (p = ", p_label(p_waarde), "); binnen gebied ",
      nl_getal(rho_binnen, 2), " (p = ", p_label(p_binnen), ")"
    ))
  eenheid <- respons_vars$eenheid[respons_vars$naam == respons_sel]
  d |>
    left_join(labels, by = "abiotiek") |>
    ggplot(aes(x = waarde_start, y = verschil)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.4) +
    geom_point(aes(colour = gebied, shape = referentie), size = 2) +
    scale_colour_manual(values = gebied_hoofdkleur) +
    scale_shape_manual(values = c("t.o.v. R-vak" = 16, "t.o.v. M-vak (geen R-vak)" = 1)) +
    facet_wrap(~facet, scales = "free", ncol = 5) +
    labs(
      title = paste0("WP2 - Bij welke uitgangssituatie verandert ", respons_sel, " beter dan bij regulier beheer?"),
      subtitle = paste0(
        "Vakken met minimaal beheer en/of afrastering. y = verandering (", eenheid, ", laatste − eerste meetjaar) in het vak min die in het R-vak van dezelfde sloot\n",
        "(sloten zonder R-vak: M-AF t.o.v. M). Positief = beter of minder slecht dan de referentie. x = abiotische uitgangswaarde (eerste meetjaar).\n",
        "ρ = Spearman over alle vakken en binnen gebieden (verkennend)."
      ),
      x = "Uitgangswaarde (eenheid per paneel)", y = paste0("Verschil in verandering (", eenheid, ")"),
      colour = "Gebied", shape = "Referentie"
    ) +
    theme_wp2
}

plots_respons <- set_names(respons_vars$naam) |> map(plot_respons_abiotiek)
walk(plots_respons, print)
iwalk(plots_respons, \(p, naam) {
  sla_figuur_op(p, paste0("respons_abiotiek_", str_replace_all(naam, " ", "_")), width = 18, height = 12)
})

# 5b. Overzicht: rho over alle vakken en binnen gebieden
plot_respons_overzicht <- resultaten_respons |>
  pivot_longer(c(rho, rho_binnen), names_to = "schaal", values_to = "rho_w") |>
  mutate(
    p_w = if_else(schaal == "rho", p_waarde, p_binnen),
    p_bh_w = if_else(schaal == "rho", p_bh, p_bh_binnen),
    schaal = factor(if_else(schaal == "rho", "alle vakken", "binnen gebied"), levels = c("alle vakken", "binnen gebied")),
    ster = case_when(p_bh_w < 0.05 ~ "**", p_w < 0.05 ~ "*", TRUE ~ ""),
    abiotiek_label = fct_rev(factor(abiotiek_label, levels = abiotiek_vars$label))
  ) |>
  ggplot(aes(x = schaal, y = abiotiek_label, fill = rho_w)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = paste0(nl_getal(rho_w, 2), ster)), size = 3.2) +
  facet_wrap(~respons_naam) +
  scale_fill_gradient2(low = okabe_ito[5], mid = "white", high = okabe_ito[6], midpoint = 0,
                       limits = c(-1, 1), na.value = "grey90") +
  labs(
    title = "WP2 - Abiotische uitgangssituatie en verandering t.o.v. regulier beheer",
    subtitle = paste0(
      "Vakken met minimaal beheer en/of afrastering; respons = verandering in het vak min die in het R-vak van dezelfde sloot\n",
      "(sloten zonder R-vak: M-AF t.o.v. M). ρ = Spearman tussen uitgangswaarde en die relatieve respons.\n",
      "Oranje (ρ > 0) = bij een hogere uitgangswaarde verandert het vak beter dan de referentie; blauw (ρ < 0) = slechter.\n",
      "Binnen gebied = na aftrek van het gebiedsgemiddelde. * p < 0,05; ** p (BH) < 0,05."
    ),
    x = NULL, y = NULL, fill = "ρ"
  ) +
  theme_wp2 +
  theme(panel.grid = element_blank())
print(plot_respons_overzicht)
sla_figuur_op(plot_respons_overzicht, "respons_abiotiek_overzicht", width = 9, height = 9)

sla_tabel_op(resultaten_respons, "tabel_respons_abiotiek")
sla_tabel_op(respons, "tabel_respons_per_locatie")

# 6. Waar nemen structuur en zone 2a toe? -------------------------------------
# Vakken met minimaal beheer (M en M-AF) ingedeeld naar de verandering
# (laatste - eerste meetjaar) van de structuur en de breedte van zone 2a. Een
# toename telt bij structuur >= drempel_structuur (ongeveer de helft van de
# gemiddelde toename bij M-AF) en zone 2a >= drempel_2a cm breder (twee
# breedteklassen van 5 cm). Per groep de abiotische uitgangssituatie;
# Kruskal-Wallis over de drie groepen, over alle vakken en binnen gebieden
# (uitgangswaarde gecentreerd op het gebiedsgemiddelde). Verkennend.
drempel_structuur <- 0.5
drempel_2a <- 10
respons_groepen <- c("geen van beide", "één van beide", "structuur én zone 2a")

respons_cat <- inner_join(
  verandering_per_vak(as_tibble(wp2), "structuur") |>
    select(SlootID, gebied, sloot_nummer, behandeling_x, d_structuur = verandering),
  verandering_per_vak(as_tibble(wp2), "oeverzone_2a_breedte_cm") |>
    select(SlootID, d_2a = verandering),
  by = "SlootID"
) |>
  filter(behandeling_x %in% c("minimaal", "afrastering")) |>
  mutate(
    n_toename = (d_structuur >= drempel_structuur) + (d_2a >= drempel_2a),
    respons_groep = factor(respons_groepen[n_toename + 1], levels = respons_groepen),
    basisbeheer = if_else(behandeling_x == "afrastering", "M-AF", "M")
  )
print(count(respons_cat, respons_groep, basisbeheer))
print(count(respons_cat, respons_groep, gebied))

respons_cat_abiotiek <- respons_cat |>
  inner_join(abiotiek_start, by = "SlootID", relationship = "one-to-many") |>
  mutate(.by = c(abiotiek, gebied), x_binnen = waarde_start - mean(waarde_start))

#' Kruskal-Wallis p-waarde (NA bij minder dan 2 groepen of 6 waarnemingen)
#'
#' @param x Numerieke vector.
#' @param g Groepsfactor.
#' @return p-waarde.
kruskal_p <- function(x, g) {
  ok <- is.finite(x) & !is.na(g)
  if (sum(ok) < 6 || n_distinct(g[ok]) < 2) return(NA_real_)
  kruskal.test(x[ok], droplevels(g[ok]))$p.value
}

resultaten_cat <- respons_cat_abiotiek |>
  summarise(
    .by = c(abiotiek, abiotiek_label),
    n_vakken = n(),
    mediaan_geen = median(waarde_start[respons_groep == respons_groepen[1]]),
    mediaan_een = median(waarde_start[respons_groep == respons_groepen[2]]),
    mediaan_beide = median(waarde_start[respons_groep == respons_groepen[3]]),
    p_waarde = kruskal_p(waarde_start, respons_groep),
    p_binnen = kruskal_p(x_binnen, respons_groep)
  ) |>
  mutate(p_bh = p.adjust(p_waarde, method = "BH"), p_bh_binnen = p.adjust(p_binnen, method = "BH")) |>
  arrange(p_waarde)
print(resultaten_cat, n = 40)

plot_respons_cat <- respons_cat_abiotiek |>
  left_join(
    transmute(resultaten_cat, abiotiek, facet = paste0(
      abiotiek_label, "\np = ", p_label(p_waarde), "; binnen gebied p = ", p_label(p_binnen)
    )),
    by = "abiotiek"
  ) |>
  mutate(facet = factor(facet, levels = unique(facet[order(match(abiotiek, abiotiek_vars$naam))]))) |>
  ggplot(aes(x = respons_groep, y = waarde_start)) +
  geom_boxplot(outlier.shape = NA, fill = "grey95", colour = "grey55", width = 0.6) +
  geom_point(
    aes(colour = gebied, shape = basisbeheer),
    position = position_jitter(width = 0.15, height = 0, seed = 1), size = 2
  ) +
  scale_colour_manual(values = gebied_hoofdkleur) +
  scale_shape_manual(values = c(M = 1, `M-AF` = 16)) +
  facet_wrap(~facet, scales = "free_y", ncol = 5) +
  labs(
    title = "WP2 - Abiotische uitgangssituatie van vakken waar structuur en zone 2a wel of niet toenemen",
    subtitle = paste0(
      "Vakken met minimaal beheer (M en M-AF). Toename: structuur ≥ ", nl_getal(drempel_structuur, 1),
      " en/of zone 2a ≥ ", drempel_2a, " cm breder (laatste − eerste meetjaar). y = waarde in het eerste meetjaar.\n",
      "p = Kruskal-Wallis over de drie groepen; binnen gebied = na aftrek van het gebiedsgemiddelde (verkennend, zonder correctie)."
    ),
    x = NULL, y = "Uitgangswaarde (eenheid per paneel)", colour = "Gebied", shape = "Basisbeheer"
  ) +
  theme_wp2 +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
print(plot_respons_cat)
sla_figuur_op(plot_respons_cat, "respons_structuur_2a_abiotiek", width = 18, height = 14)
sla_tabel_op(resultaten_cat, "tabel_respons_structuur_2a_abiotiek")
sla_tabel_op(respons_cat, "tabel_respons_structuur_2a_per_vak")

# 7. Draagkracht oever versus vegetatiehoogte ---------------------------------
# Alle WP2-locaties en meetjaren (één punt = locatie x jaar), per basisbeheer
# (regulier / minimaal / afrastering). Lijn = lineaire trend per paneel;
# rho = Spearman-rangcorrelatie (verkennend: metingen van dezelfde locatie in
# verschillende jaren zijn niet onafhankelijk).
draagkracht_veg <- wp2 |>
  as_tibble() |>
  select(SlootID, gebied, jaar, behandeling_x,
         draagkracht = draagkracht_oever, veg_hoogte = oeverzone_2b_gem_veg_hoogte_cm) |>
  mutate(across(c(draagkracht, veg_hoogte), as.numeric)) |>
  filter(!is.na(draagkracht), !is.na(veg_hoogte), !is.na(behandeling_x))

labels_dv <- draagkracht_veg |>
  summarise(
    .by = behandeling_x,
    n = n(),
    rho = suppressWarnings(cor.test(veg_hoogte, draagkracht, method = "spearman", exact = FALSE))$estimate,
    p = suppressWarnings(cor.test(veg_hoogte, draagkracht, method = "spearman", exact = FALSE))$p.value
  ) |>
  mutate(facet = paste0(behandeling_x, "\nrho = ", formatC(rho, digits = 2, format = "f"), ", p = ", p_label(p), ", n = ", n))
rho_alle <- suppressWarnings(cor.test(draagkracht_veg$veg_hoogte, draagkracht_veg$draagkracht, method = "spearman", exact = FALSE))

plot_draagkracht_veg <- draagkracht_veg |>
  left_join(select(labels_dv, behandeling_x, facet), by = "behandeling_x") |>
  mutate(facet = factor(facet, levels = labels_dv$facet[order(labels_dv$behandeling_x)])) |>
  ggplot(aes(x = veg_hoogte, y = draagkracht)) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "black", fill = "grey85", linewidth = 0.8) +
  geom_point(aes(colour = gebied, shape = factor(jaar)), size = 2, alpha = 0.8) +
  scale_colour_manual(values = gebied_hoofdkleur) +
  facet_wrap(~facet, nrow = 1) +
  labs(
    title = "WP2 - Draagkracht oever versus vegetatiehoogte zone 2b",
    subtitle = paste0(
      "Punt = locatie × meetjaar; lijn = lineaire trend met 95%-betrouwbaarheidsband. ",
      "rho = Spearman-rangcorrelatie (verkennend, herhaalde metingen per locatie).\n",
      "Alle basisbeheer samen: rho = ", formatC(unname(rho_alle$estimate), digits = 2, format = "f"),
      ", p = ", p_label(rho_alle$p.value), ", n = ", nrow(draagkracht_veg), "."
    ),
    x = "Gemiddelde vegetatiehoogte zone 2b (cm)", y = "Draagkracht oever (MPa)",
    colour = "Gebied", shape = "Jaar"
  ) +
  theme_wp2
print(plot_draagkracht_veg)
sla_figuur_op(plot_draagkracht_veg, "draagkracht_oever_vs_vegetatiehoogte", width = 14, height = 6)

# 7b. Verandering draagkracht versus verandering vegetatiehoogte per locatie
# (laatste - eerste jaar waarin beide gemeten zijn). Eén punt per locatie, dus
# geen herhaalde metingen.
verschil_dv <- draagkracht_veg |>
  arrange(jaar) |>
  summarise(
    .by = SlootID,
    gebied = first(gebied), behandeling_x = last(behandeling_x),
    jaar_eerste = first(jaar), jaar_laatste = last(jaar),
    d_draagkracht = last(draagkracht) - first(draagkracht),
    d_veg_hoogte = last(veg_hoogte) - first(veg_hoogte)
  ) |>
  filter(jaar_eerste != jaar_laatste)

labels_verschil <- verschil_dv |>
  summarise(
    .by = behandeling_x,
    n = n(),
    rho = suppressWarnings(cor.test(d_veg_hoogte, d_draagkracht, method = "spearman", exact = FALSE))$estimate,
    p = suppressWarnings(cor.test(d_veg_hoogte, d_draagkracht, method = "spearman", exact = FALSE))$p.value
  ) |>
  mutate(facet = paste0(behandeling_x, "\nrho = ", formatC(rho, digits = 2, format = "f"), ", p = ", p_label(p), ", n = ", n))
rho_verschil <- suppressWarnings(cor.test(verschil_dv$d_veg_hoogte, verschil_dv$d_draagkracht, method = "spearman", exact = FALSE))

plot_verschil_dv <- verschil_dv |>
  left_join(select(labels_verschil, behandeling_x, facet), by = "behandeling_x") |>
  mutate(facet = factor(facet, levels = labels_verschil$facet[order(labels_verschil$behandeling_x)])) |>
  ggplot(aes(x = d_veg_hoogte, y = d_draagkracht)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.4) +
  geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.4) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "black", fill = "grey85", linewidth = 0.8) +
  geom_point(aes(colour = gebied), size = 2.2, alpha = 0.85) +
  scale_colour_manual(values = gebied_hoofdkleur) +
  facet_wrap(~facet, nrow = 1) +
  labs(
    title = "WP2 - Verandering draagkracht oever versus verandering vegetatiehoogte zone 2b",
    subtitle = paste0(
      "Punt = locatie; verandering = laatste − eerste meetjaar. x: + = hogere vegetatie, − = lager; ",
      "y: + = stevigere oever, − = slapper.\n",
      "Alle basisbeheer samen: rho = ", formatC(unname(rho_verschil$estimate), digits = 2, format = "f"),
      ", p = ", p_label(rho_verschil$p.value), ", n = ", nrow(verschil_dv), " (Spearman)."
    ),
    x = "Verandering vegetatiehoogte zone 2b (cm)", y = "Verandering draagkracht oever (MPa)", colour = "Gebied"
  ) +
  theme_wp2
print(plot_verschil_dv)
sla_figuur_op(plot_verschil_dv, "verschil_draagkracht_vs_verschil_vegetatiehoogte", width = 14, height = 6)
