# =============================================================================
# WP2: treedt het effect van beheer vooral op in voedselrijke of soortenrijke
# sloten? Effect per sloot tegen de uitgangssituatie
# =============================================================================
# Effect per sloot = verandering (laatste - eerste meetjaar) in het ene vak min
# die in het andere vak van dezelfde sloot, voor drie contrasten:
#   M - R     effect van minimaal beheer (sloten met een R- en een M-vak)
#   M-AF - M  effect van afrastering bij minimaal beheer
#   M-AF - R  minimaal beheer + afrastering samen (vooral Ronde Hoep, waar
#             geen M-vak is)
# In de selectie heeft elke sloot één vak per basisbeheer, dus het effect is
# het verschil tussen twee vakken. Een trend die voor de hele sloot geldt
# (meetdag, peil) valt zo weg.
#
# Uitgangssituatie per sloot = gemiddelde van de twee vergeleken vakken in het
# eerste meetjaar, voor de doelvariabelen, de top-10 voorspellers uit de
# XGBoost-modellen van WP1 (gemiddelde Gain over de doelvariabelen) en P-AL en
# N-mineraal in de oever (voedselrijkdom). Het gemiddelde van beide vakken
# voorkomt dat regressie naar het gemiddelde bij dezelfde variabele een
# schijnverband geeft: een toevallig hoge beginwaarde in één vak zit in het
# effect met een min- en in de beginwaarde met een plusteken, maar via het
# gemiddelde van beide vakken heffen die elkaar (bij gelijke meetfout) op.
#
# Toets: Spearman-rangcorrelatie tussen beginwaarde en effect over de sloten,
# BH-correctie per contrast over alle combinaties. Verkennend: per contrast
# 5 tot 14 sloten, en de sloten van een gebied lijken op elkaar, zodat een
# verband ook een verschil tussen gebieden kan zijn.
#
# Gebruik: eerst wp2_analyse.R draaien (wp2, wp2_verandering, var_lijst,
# targets, xgb_importance, predictoren_uitgesloten, theme_wp2, sla_figuur_op,
# sla_tabel_op, gebied_hoofdkleur, okabe_ito, p_label en nl_getal moeten
# bestaan).
# =============================================================================

benodigd <- c(
  "wp2", "wp2_verandering", "var_lijst", "targets", "xgb_importance", "predictoren_uitgesloten",
  "theme_wp2", "sla_figuur_op", "sla_tabel_op", "gebied_hoofdkleur", "okabe_ito", "p_label", "nl_getal"
)
ontbreekt <- benodigd[!vapply(benodigd, exists, logical(1))]
if (length(ontbreekt) > 0) {
  stop("Draai eerst wp2_analyse.R; ontbrekende objecten: ", paste(ontbreekt, collapse = ", "))
}

# 1. Variabelen ------------------------------------------------------------------
n_top_wp1 <- 10

# Doelvariabelen: effect (y) én beginwaarde (x)
doel_vars <- var_lijst |>
  filter(kolom %in% targets) |>
  transmute(kolom, label = weergave(naam), groep = "doelvariabele")

# Top-10 voorspellers uit WP1: gemiddelde Gain over de doelvariabelen (een
# variabele die bij een doelvariabele niet in het model zit telt als 0).
# Variabelen die zelf doelvariabele zijn en water_pH (predictoren_uitgesloten)
# vallen af.
n_targets_wp1 <- uniqueN(xgb_importance$target_var)
top_wp1 <- xgb_importance[
  imp_type == "Gain" & !(Feature %in% c(predictoren_uitgesloten, doel_vars$kolom)) & Feature %in% names(wp2),
  .(gain = sum(Importance) / n_targets_wp1, label = first(Nederlandse_naam)),
  by = .(kolom = Feature)
][order(-gain)][seq_len(n_top_wp1)]
print(top_wp1)

# Voedselrijkdom van de oever, als ze niet al in de top-10 zitten
voedsel_oever <- tibble(
  kolom = c("P-AL mg p2o5/100g_OR_25", "N_mineraal_OR_25"),
  label = c("P-AL oever (mg P2O5/100g)", "N-mineraal oever (mg/kg)")
) |>
  filter(kolom %in% names(wp2), !kolom %in% top_wp1$kolom)

begin_vars <- bind_rows(
  doel_vars,
  as_tibble(top_wp1) |> transmute(kolom, label, groep = "top-10 WP1"),
  voedsel_oever |> mutate(groep = "voedselrijkdom oever")
) |>
  distinct(kolom, .keep_all = TRUE) |>
  mutate(groep = factor(groep, levels = c("doelvariabele", "top-10 WP1", "voedselrijkdom oever")))

# 2. Effect per sloot -------------------------------------------------------------
contrasten <- tribble(
  ~contrast,   ~behandeling, ~referentie,
  "M − R",     "M",          "R",
  "M-AF − M",  "M-AF",       "M",
  "M-AF − R",  "M-AF",       "R"
) |>
  mutate(contrast = factor(contrast, levels = contrast))

verandering_doel <- wp2_verandering |>
  filter(kolom %in% doel_vars$kolom) |>
  select(SlootID, gebied, sloot_nummer, behandeling_basis, kolom, verandering)

#' Effect per sloot: verandering in het behandelde vak min die in het referentievak
#'
#' @param behandeling,referentie Basisbeheer (behandeling_basis) van de twee vakken.
#' @return Tibble met per sloot en doelvariabele het effect en de SlootID's van
#'   beide vakken (gemiddeld als een sloot meer vakken met hetzelfde basisbeheer heeft).
effect_per_sloot <- function(behandeling, referentie) {
  verandering_doel |>
    filter(behandeling_basis %in% c(behandeling, referentie)) |>
    mutate(rol = if_else(behandeling_basis == behandeling, "beh", "ref")) |>
    summarise(
      .by = c(gebied, sloot_nummer, kolom, rol),
      verandering = mean(verandering), vakken = list(SlootID)
    ) |>
    pivot_wider(names_from = rol, values_from = c(verandering, vakken)) |>
    filter(!is.na(verandering_beh), !is.na(verandering_ref)) |>
    transmute(
      gebied, sloot_nummer, kolom_effect = kolom,
      effect = verandering_beh - verandering_ref,
      vakken = map2(vakken_beh, vakken_ref, c)
    )
}

effect_sloot <- contrasten |>
  mutate(data = map2(behandeling, referentie, effect_per_sloot)) |>
  select(contrast, data) |>
  unnest(data) |>
  left_join(select(doel_vars, kolom_effect = kolom, effect_label = label), by = "kolom_effect")
print(count(effect_sloot, contrast, effect_label) |> pivot_wider(names_from = contrast, values_from = n))

# 3. Beginwaarde per vak en per sloot ---------------------------------------------
# Waarde in het eerste jaar waarin de variabele voor dat vak gemeten is.
begin_vak <- as_tibble(wp2) |>
  select(SlootID, jaar, all_of(begin_vars$kolom)) |>
  mutate(across(all_of(begin_vars$kolom), \(x) suppressWarnings(as.numeric(x)))) |>
  pivot_longer(all_of(begin_vars$kolom), names_to = "kolom_begin", values_to = "waarde") |>
  filter(!is.na(waarde)) |>
  arrange(jaar) |>
  summarise(.by = c(SlootID, kolom_begin), waarde_begin = first(waarde))

# Gemiddelde beginwaarde van de twee vergeleken vakken. Verschilt per
# doelvariabele, omdat niet elk vak voor elke doelvariabele een effect heeft.
effect_begin <- effect_sloot |>
  select(contrast, gebied, sloot_nummer, kolom_effect, effect_label, effect, vakken) |>
  unnest(vakken, keep_empty = FALSE) |>
  rename(SlootID = vakken) |>
  inner_join(begin_vak, by = "SlootID", relationship = "many-to-many") |>
  summarise(
    .by = c(contrast, gebied, sloot_nummer, kolom_effect, effect_label, effect, kolom_begin),
    beginwaarde = mean(waarde_begin)
  ) |>
  left_join(select(begin_vars, kolom_begin = kolom, begin_label = label, begin_groep = groep), by = "kolom_begin")

# 4. Samenhang --------------------------------------------------------------------
#' Alle permutaties van 1..n (als matrix, één permutatie per rij)
#'
#' @param n Aantal elementen.
#' @return Matrix met factorial(n) rijen en n kolommen.
permutaties <- function(n) {
  if (n == 1) return(matrix(1L))
  p <- permutaties(n - 1)
  do.call(rbind, lapply(seq_len(n), \(i) cbind(i, matrix(setdiff(seq_len(n), i)[p], ncol = n - 1))))
}
perm_cache <- map(1:8, permutaties)

#' Spearman-correlatie met p-waarde (NA bij minder dan 5 sloten of geen spreiding)
#'
#' Tot en met 8 sloten een exacte permutatietoets (ook bij gelijke waarden);
#' de t-benadering van cor.test() geeft bij zo weinig waarnemingen en ρ = ±1
#' een p-waarde van vrijwel 0, terwijl de kleinste mogelijke p bij 5 sloten
#' 2/120 = 0,017 is. Vanaf 9 sloten de benadering.
#'
#' @param x,y Numerieke vectoren.
#' @return Lijst met rho en p.
spearman_sloot <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  n <- sum(ok)
  if (n < 5 || sd(x[ok]) == 0 || sd(y[ok]) == 0) return(list(rho = NA_real_, p = NA_real_))
  rx <- rank(x[ok])
  ry <- rank(y[ok])
  rho <- cor(rx, ry)
  if (n <= 8) {
    rho_perm <- apply(perm_cache[[n]], 1, \(i) cor(rx, ry[i]))
    p <- mean(abs(rho_perm) >= abs(rho) - 1e-10)
  } else {
    p <- suppressWarnings(cor.test(x[ok], y[ok], method = "spearman", exact = FALSE))$p.value
  }
  list(rho = rho, p = p)
}

resultaten_effect_begin <- effect_begin |>
  summarise(
    .by = c(contrast, kolom_effect, effect_label, kolom_begin, begin_label, begin_groep),
    n_sloten = n(), n_gebieden = n_distinct(gebied),
    rho = spearman_sloot(beginwaarde, effect)$rho,
    p_waarde = spearman_sloot(beginwaarde, effect)$p
  ) |>
  mutate(.by = contrast, p_bh = p.adjust(p_waarde, method = "BH")) |>
  arrange(contrast, p_waarde)
print(resultaten_effect_begin |> filter(p_waarde < 0.05), n = 60)

# 4b. Beginwaarde op vakniveau ------------------------------------------------------
# Bij bv. vegetatie kan de eigen uitgangssituatie van het vak bepalen hoe het
# reageert (een vak met al veel emerse vegetatie kan minder toenemen). De
# beginwaarde van alleen het behandelde vak geeft echter een schijnverband
# door regressie naar het gemiddelde. Daarom per contrast een model op
# vakniveau met de sloot als blok:
#   verandering ~ sloot + behandeling * beginwaarde (van het vak zelf)
# Regressie naar het gemiddelde werkt in beide vakken even sterk en komt in
# het hoofdeffect van de beginwaarde; de interactie meet of het
# behandelingseffect afhangt van de beginwaarde van het vak (onvertekend bij
# gelijke meetfout in beide behandelingen). Verandering en beginwaarde worden
# eerst in rangen omgezet (rang-ANCOVA), zodat één afwijkende sloot het
# resultaat niet bepaalt, net als bij de Spearman-correlatie hierboven. Een
# beginwaarde die voor beide vakken gelijk is (slootkenmerk) valt als
# hoofdeffect weg tegen de sloot; de interactie is dan nog wel te schatten.
# Maat: partiële correlatie van de interactie, r = t / sqrt(t² + df).

#' Interactie behandeling × beginwaarde (vakniveau) voor één combinatie
#'
#' @param d Data met sloot_nummer, beh (0/1), verandering en waarde_begin.
#' @return Tibble met n_sloten, df, r_partieel en p_waarde (NA bij te weinig data).
interactie_vak <- function(d) {
  d <- d |>
    filter(is.finite(verandering), is.finite(waarde_begin)) |>
    filter(.by = sloot_nummer, n_distinct(beh) == 2)
  leeg <- tibble(n_sloten = n_distinct(d$sloot_nummer), df = NA_real_, r_partieel = NA_real_, p_waarde = NA_real_)
  if (n_distinct(d$sloot_nummer) < 5 || sd(d$waarde_begin) == 0 || sd(d$verandering) == 0) return(leeg)
  d <- mutate(d, y = rank(verandering), x = rank(waarde_begin), sloot_nummer = factor(sloot_nummer))
  fit <- lm(y ~ sloot_nummer + beh * x, data = d)
  co <- summary(fit)$coefficients
  # Minder dan 4 vrijheidsgraden (M − R: 5 sloten) geeft vrijwel altijd
  # r = ±1 en is niet te interpreteren.
  if (!"beh:x" %in% rownames(co) || fit$df.residual < 4) return(leeg)
  t <- co["beh:x", "t value"]
  mutate(leeg, df = fit$df.residual, r_partieel = t / sqrt(t^2 + fit$df.residual), p_waarde = co["beh:x", "Pr(>|t|)"])
}

vak_contrast <- function(behandeling, referentie) {
  verandering_doel |>
    filter(behandeling_basis %in% c(behandeling, referentie)) |>
    transmute(SlootID, gebied, sloot_nummer, kolom_effect = kolom, verandering,
              beh = as.integer(behandeling_basis == behandeling))
}

resultaten_effect_begin_vak <- contrasten |>
  mutate(data = map2(behandeling, referentie, vak_contrast)) |>
  select(contrast, data) |>
  unnest(data) |>
  inner_join(begin_vak, by = "SlootID", relationship = "many-to-many") |>
  group_by(contrast, kolom_effect, kolom_begin) |>
  group_modify(\(d, sleutel) interactie_vak(d)) |>
  ungroup() |>
  left_join(select(doel_vars, kolom_effect = kolom, effect_label = label), by = "kolom_effect") |>
  left_join(select(begin_vars, kolom_begin = kolom, begin_label = label, begin_groep = groep), by = "kolom_begin") |>
  mutate(.by = contrast, p_bh = p.adjust(p_waarde, method = "BH")) |>
  arrange(contrast, p_waarde)
print(resultaten_effect_begin_vak |> filter(p_waarde < 0.05), n = 60)

# 5. Figuren ----------------------------------------------------------------------
# 5a. Overzicht: rho per beginwaarde (rij) en effect op doelvariabele (kolom)
n_sloten_contrast <- effect_sloot |>
  summarise(
    .by = contrast, n = n_distinct(sloot_nummer), n_gebieden = n_distinct(gebied),
    gebieden = paste(sort(unique(gebied)), collapse = ", ")
  )
facet_contrast <- with(n_sloten_contrast, setNames(
  paste0(contrast, "\n", n, " sloten in ", n_gebieden, " gebieden (", gebieden, ")"),
  contrast
))

#' Heatmap van de samenhang tussen effect (kolom) en beginwaarde (rij) per contrast
#'
#' @param res Resultatentabel met contrast, effect_label, begin_label,
#'   begin_groep, p_waarde, p_bh en de kolom `maat`.
#' @param maat Naam van de kolom met de samenhang (tussen -1 en 1).
#' @param titel,ondertitel Titel en ondertitel.
#' @param legenda Legendatitel.
#' @return ggplot-object.
plot_heatmap_effect <- function(res, maat, titel, ondertitel, legenda) {
  res |>
    mutate(
      waarde = .data[[maat]],
      ster = case_when(p_bh < 0.05 ~ "**", p_waarde < 0.05 ~ "*", TRUE ~ ""),
      begin_label = factor(begin_label, levels = rev(begin_vars$label)),
      effect_label = factor(effect_label, levels = doel_vars$label),
      contrast = factor(facet_contrast[as.character(contrast)], levels = facet_contrast)
    ) |>
    ggplot(aes(x = effect_label, y = begin_label, fill = waarde)) +
    geom_tile(colour = "white") +
    geom_text(aes(label = if_else(is.na(waarde), "", paste0(nl_getal(waarde, 1), ster))), size = 2.6) +
    facet_grid(
      begin_groep ~ contrast, scales = "free_y", space = "free_y",
      labeller = labeller(begin_groep = \(x) str_replace(str_wrap(x, 12), "voedselrijkdom", "voedsel-\nrijkdom"))
    ) +
    scale_fill_gradient2(
      low = okabe_ito[5], mid = "white", high = okabe_ito[6], midpoint = 0,
      limits = c(-1, 1), na.value = "grey90"
    ) +
    labs(title = titel, subtitle = ondertitel, x = "Effect op doelvariabele", y = "Beginwaarde", fill = legenda) +
    theme_wp2 +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 8),
      strip.text.x = element_text(size = 8),
      strip.text.y = element_text(size = 8, angle = 0, hjust = 0)
    )
}

plot_effect_begin <- plot_heatmap_effect(
  resultaten_effect_begin, "rho",
  titel = "WP2 - Hangt het effect van beheer samen met de uitgangssituatie van de sloot?",
  ondertitel = paste0(
    "Effect per sloot = verandering (laatste − eerste meetjaar) in het ene vak min die in het andere vak van dezelfde sloot (kolom).\n",
    "Beginwaarde = gemiddelde van beide vakken in het eerste meetjaar (rij). ρ = Spearman over de sloten.\n",
    "Oranje (ρ > 0) = bij een hogere beginwaarde een groter (positiever) effect; blauw (ρ < 0) = kleiner of negatiever.\n",
    "* p < 0,05; ** p (BH, per contrast) < 0,05. Verkennend: weinig sloten, en sloten uit hetzelfde gebied lijken op elkaar."
  ),
  legenda = "ρ"
)
print(plot_effect_begin)
sla_figuur_op(plot_effect_begin, "effect_beginwaarde_overzicht", width = 16, height = 11)

# 5a2. Zelfde overzicht met de beginwaarde van het vak zelf (sectie 4b)
plot_effect_begin_vak <- plot_heatmap_effect(
  resultaten_effect_begin_vak, "r_partieel",
  titel = "WP2 - Hangt het effect van beheer samen met de uitgangssituatie van het vak?",
  ondertitel = paste0(
    "Rang-ANCOVA per contrast: verandering ~ sloot + behandeling × beginwaarde van het vak zelf (eerste meetjaar).\n",
    "Getal = partiële correlatie van de interactie: hoe sterk het behandelingseffect (kolom) meeloopt met de beginwaarde van het vak (rij).\n",
    "Oranje (> 0) = bij een hogere beginwaarde een groter (positiever) effect; blauw (< 0) = kleiner of negatiever.\n",
    "* p < 0,05; ** p (BH, per contrast) < 0,05. Grijs: te weinig sloten voor dit model (M − R). Verkennend."
  ),
  legenda = "r (partieel)"
)
print(plot_effect_begin_vak)
sla_figuur_op(plot_effect_begin_vak, "effect_beginwaarde_vak_overzicht", width = 16, height = 11)

# 5b. Spreidingsplots voor de combinaties met p < 0,05 (zonder correctie),
# hoogstens de twaalf sterkste, met het gebied als kleur.
sterkste <- resultaten_effect_begin |>
  filter(p_waarde < 0.05) |>
  slice_min(p_waarde, n = 12, with_ties = FALSE)

if (nrow(sterkste) > 0) {
  plot_effect_begin_punten <- effect_begin |>
    inner_join(
      transmute(sterkste, contrast, kolom_effect, kolom_begin, facet = paste0(
        contrast, ": ", effect_label, "\ntegen ", begin_label,
        "\nρ = ", nl_getal(rho, 2), ", p = ", p_label(p_waarde), "; ", n_sloten, " sloten, ", n_gebieden, " gebieden"
      )),
      by = c("contrast", "kolom_effect", "kolom_begin")
    ) |>
    mutate(facet = factor(facet, levels = unique(facet[order(match(paste(contrast, kolom_effect, kolom_begin),
                                                                     paste(sterkste$contrast, sterkste$kolom_effect, sterkste$kolom_begin)))]))) |>
    ggplot(aes(x = beginwaarde, y = effect)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.4) +
    geom_point(aes(colour = gebied), size = 2.2) +
    scale_colour_manual(values = gebied_hoofdkleur) +
    facet_wrap(~facet, scales = "free", ncol = 4) +
    labs(
      title = "WP2 - Effect per sloot tegen de beginwaarde (combinaties met p < 0,05 zonder correctie)",
      subtitle = "Punt = sloot. y = verschil in verandering tussen de twee vakken; x = gemiddelde beginwaarde van beide vakken (eenheid per paneel).",
      x = "Beginwaarde", y = "Effect (verschil in verandering)", colour = "Gebied"
    ) +
    theme_wp2 +
    theme(strip.text = element_text(size = 8))
  print(plot_effect_begin_punten)
  sla_figuur_op(plot_effect_begin_punten, "effect_beginwaarde_sterkste", width = 16, height = 3.3 * ceiling(nrow(sterkste) / 4) + 1.5)
}

sla_tabel_op(resultaten_effect_begin, "tabel_effect_beginwaarde")
sla_tabel_op(resultaten_effect_begin_vak, "tabel_effect_beginwaarde_vak")
sla_tabel_op(select(effect_sloot, -vakken), "tabel_effect_per_sloot")
sla_tabel_op(begin_vars, "tabel_effect_beginwaarde_variabelen")
