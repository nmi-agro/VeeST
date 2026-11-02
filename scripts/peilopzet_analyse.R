# =============================================================================
# Peilopzet en de 2a-zone (WP1): rapportparagraaf "Creëren van een 2a-zone
# door peilopzet"
# =============================================================================
# Doel: nagaan of locaties waar het peil is opgezet een andere 2a-zone
# (breedte, emerse vegetatie, kragge/drijftil, onderholling), taludhoek en
# slootmorfologie hebben dan andere locaties.
#
# Peilopzet in drie groepen:
#  - "peilopzet (Vernat?)": kolom `Vernat?` uit wp_locaties_naam_correcties.xlsx
#    (vernat_loc = 1). Dit is leidend: `Vernat?` klopt beter dan de enquête.
#  - "alleen volgens beheerenquête": volgens Vernat? geen peilopzet, maar in de
#    beheerenquête (Verandering_waterpeil_5jaar_cm) is het peil "omhoog" gegaan.
#  - "geen peilopzet": de overige locaties.
#
# Data: WP1 (o.a. Hegewarren, De Nesse, Idzega, Mijnden, Stein). Stein Noord
# (vernat) en Stein Zuid (niet vernat) tellen samen als één gebied. In WP2
# heeft geen enkele locatie peilopzet. Elke WP1-locatie is één keer gemeten,
# dus dit is een vergelijking van de toestand (geen verandering in de tijd):
# verschillen kunnen ook komen door andere verschillen tussen locaties.
#
# Toetsen per kenmerk, BH-gecorrigeerd over de kenmerken:
#  (a) alle WP1-locaties: waarde ~ peilgroep + (1 | gebied); gebied random.
#      Geeft Vernat? vs geen peilopzet en beheerenquête vs geen peilopzet.
#  (b) binnen gebied: alleen gebieden met vernatte én andere locaties;
#      waarde ~ vernat (Vernat? vs alle andere locaties) + gebied.
# =============================================================================

library(tidyverse)
library(data.table)
library(lme4)
library(lmerTest)

if (!exists("workspace")) {
  workspace <- paste0(Sys.getenv("NMI-SITE"), "/O 1900 - O 2000/1922.N.23 VeeST vwsloot vd toekomst/05. Data/")
}
if (!exists("workspace2")) {
  workspace2 <- paste0("c:/Users/LauraMoria/Stichting Veenweiden Innovatiecentrum/VIPNL Themas - VIPNL Veenweidensloot/D. Data en analyse/")
}
figuren_dir <- paste0(workspace2, "figuren_wp2_analyse_LM/")
dir.create(paste0(figuren_dir, "rapport"), recursive = TRUE, showWarnings = FALSE)
dir.create(paste0(figuren_dir, "bijlage"), recursive = TRUE, showWarnings = FALSE)

if (!exists("abio_proj")) {
  abio_proj <- readRDS(paste0(workspace, "output/rapport/abio_proj.rds"))
}
# Geometriekolommen (sfc) zijn hier niet nodig en breken tibble-bewerkingen
abio_wp1 <- as.data.frame(abio_proj)
abio_wp1 <- abio_wp1[, !vapply(abio_wp1, inherits, logical(1), what = "sfc"), drop = FALSE]

okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
theme_peil <- theme_minimal(base_size = 12) +
  theme(
    panel.border = element_rect(colour = "grey70", fill = NA),
    strip.background = element_rect(fill = "grey95", colour = "grey70"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )
p_label <- function(p) if_else(is.na(p), "n.v.t.", trimws(formatC(p, digits = 2, format = "g")))

peilgroepen <- c("geen peilopzet", "alleen volgens beheerenquête", "peilopzet (Vernat?)")
kleuren_peilgroep <- setNames(c(okabe_ito[1], "grey55", okabe_ito[5]), peilgroepen)

# 1. Kenmerken: 2a-zone, 2b-zone en slootmorfologie --------------------------
kenmerken <- tribble(
  ~groep,            ~naam,                   ~kolom,                       ~eenheid,  ~richting,
  "2a-zone",         "breedte zone 2a",       "oeverzone_2a_breedte_cm",    "cm",      "+ = breder",
  "2a-zone",         "emers zone 2a",         "oeverzone_2a_emers_m2",      "m",       "+ = bredere emerse strook",
  "2a-zone",         "kragge zone 2a",        "oeverzone_2a_kraggen_perc",  "%",       "+ = meer kragge/drijftil",
  "2a-zone",         "onderholling",          "holleoever",                 "cm",      "+ = dieper uitgehold",
  "2b-zone",         "breedte zone 2b",       "oeverzone_2b_breedte_cm",    "cm",      "+ = breder",
  "2b-zone",         "kragge zone 2b",        "oeverzone_2b_kraggen_perc",  "%",       "+ = meer kragge",
  "2b-zone",         "kale oever zone 2b",    "oeverzone_2b_kaal_perc",     "%",       "+ = meer kale oever",
  "slootmorfologie", "taludhoek oever",       "tldk_oevrwtr_perc",          "%",       "+ = steiler, \u2212 = flauwer",
  "slootmorfologie", "taludhoek onder water", "tldk_wtrwtr_perc",           "%",       "+ = steiler, \u2212 = flauwer",
  "slootmorfologie", "waterdiepte",           "max_wtd",                    "m",       "+ = dieper",
  "slootmorfologie", "slibdikte",             "max_slib",                   "m",       "+ = dikkere sliblaag",
  "slootmorfologie", "breedte waterlijn",     "breedtewl",                  "m",       "+ = bredere sloot",
  "slootmorfologie", "afscheuring",           "afscheur_veg_lengte_perc",   "%",       "+ = meer afscheuring",
  "slootmorfologie", "draagkracht oever",     "draagkracht_oever",          "MPa",     "+ = stevigere oever",
  "waterbodem",      "redox slib",            "slib_redox_pH7",             "mV",      "+ = minder reducerend",
  "waterbodem",      "P-AL waterbodem",       "P-AL mg p2o5/100g_SB",       "mg P2O5/100g", "+ = meer fosfaat",
  "waterbodem",      "P-AL oever",            "P-AL mg p2o5/100g_OR_25",    "mg P2O5/100g", "+ = meer fosfaat",
  "controle",        "drooglegging",          "drglg",                      "m",       "+ = meer drooglegging"
) |>
  filter(kolom %in% names(abio_wp1)) |>
  mutate(
    label = paste0(naam, " (", eenheid, ")\n", richting),
    groep = factor(groep, levels = c("2a-zone", "2b-zone", "slootmorfologie", "waterbodem", "controle"))
  )

# Kenmerken waarop vooraf een effect van peilopzet werd verwacht. Voor deze
# set is er een aparte figuur binnen gebieden, met BH-correctie alleen over
# deze kenmerken (vooraf gestelde hypotheses).
verwacht_kolommen <- c(
  "oeverzone_2a_breedte_cm", "oeverzone_2b_breedte_cm", "tldk_oevrwtr_perc", "tldk_wtrwtr_perc",
  "breedtewl", "max_wtd", "max_slib", "slib_redox_pH7", "P-AL mg p2o5/100g_SB", "P-AL mg p2o5/100g_OR_25"
)

# 2. Data: WP1, één rij per locatie --------------------------------------------
# Gebieden die samen als één vergelijkingsgebied tellen (zie hieronder)
samen_vergelijken <- c(SN = "STEIN", SZ = "STEIN")
samen_vergelijken_naam <- c(STEIN = "Stein (Noord + Zuid)")

# In Mijnden staan locaties dubbel in de data; numerieke kenmerken worden per
# locatie gemiddeld (de dubbele rijen zijn gelijk of vrijwel gelijk).
wp1 <- abio_wp1 |>
  as_tibble() |>
  filter(WP == "WP1", !is.na(vernat_loc)) |>
  select(SlootID, gebied, Gebiedsnaam, jaar, vernat_loc, Verandering_waterpeil_5jaar_cm, all_of(kenmerken$kolom)) |>
  mutate(across(all_of(kenmerken$kolom), as.numeric)) |>
  summarise(
    .by = c(SlootID, gebied, Gebiedsnaam, jaar),
    vernat = first(vernat_loc) == 1,
    peil_verandering_5jaar = first(Verandering_waterpeil_5jaar_cm),
    across(all_of(kenmerken$kolom), \(x) mean(x, na.rm = TRUE))
  ) |>
  mutate(
    across(all_of(kenmerken$kolom), \(x) if_else(is.nan(x), NA_real_, x)),
    # Aangrenzende gebieden die voor de vergelijking samen één gebied vormen:
    # Stein Noord (vernat) en Stein Zuid (niet vernat). De oorspronkelijke
    # gebiedscode blijft bewaard in gebied_orig.
    gebied_orig = gebied,
    gebied = coalesce(unname(samen_vergelijken[gebied]), gebied),
    Gebiedsnaam = coalesce(unname(samen_vergelijken_naam[gebied]), Gebiedsnaam),
    enquete_omhoog = coalesce(str_detect(peil_verandering_5jaar, "omhoog"), FALSE),
    peilgroep = factor(case_when(
      vernat ~ "peilopzet (Vernat?)",
      enquete_omhoog ~ "alleen volgens beheerenquête",
      TRUE ~ "geen peilopzet"
    ), levels = peilgroepen)
  )

# Overzicht per gebied en vergelijking Vernat? met de beheerenquête
gebieden_peil <- wp1 |>
  summarise(
    .by = c(gebied, Gebiedsnaam), n = n(),
    n_vernat = sum(peilgroep == "peilopzet (Vernat?)"),
    n_enquete = sum(peilgroep == "alleen volgens beheerenquête")
  ) |>
  arrange(desc(n_vernat), desc(n_enquete))
print(gebieden_peil, n = 50)
fwrite(gebieden_peil, paste0(figuren_dir, "tabel_peilopzet_gebieden.csv"), sep = ";", dec = ",")

vernat_vs_enquete <- wp1 |>
  mutate(enquete = case_when(
    enquete_omhoog ~ "omhoog",
    str_detect(coalesce(peil_verandering_5jaar, ""), "^nee") ~ "nee",
    TRUE ~ "anders/onbekend"
  )) |>
  count(vernat, enquete)
print(vernat_vs_enquete)
fwrite(vernat_vs_enquete, paste0(figuren_dir, "tabel_peilopzet_vernat_vs_enquete.csv"), sep = ";", dec = ",")

# Gebieden met vernatte én andere locaties (voor de vergelijking binnen gebied)
gebieden_vergelijk <- gebieden_peil |> filter(n_vernat > 0, n_vernat < n) |> pull(gebied)
cat("Gebieden met vernatte én andere locaties:", paste(gebieden_vergelijk, collapse = ", "), "\n")

wp1_lang <- wp1 |>
  pivot_longer(all_of(kenmerken$kolom), names_to = "kolom", values_to = "waarde") |>
  filter(!is.na(waarde)) |>
  left_join(kenmerken, by = "kolom")

# 3. Toetsen ---------------------------------------------------------------------
#' Toets verschillen tussen de peilgroepen voor één kenmerk
#'
#' @param d Data voor één kenmerk met waarde, peilgroep, vernat en gebied.
#' @return Tibble met n en mediaan per groep, en schatting en p voor
#'   (a) Vernat? vs geen peilopzet en beheerenquête vs geen peilopzet (alle
#'   locaties, gebied random) en (b) Vernat? vs andere locaties binnen
#'   dezelfde gebieden (gebied als vast blok).
toets_peilopzet <- function(d) {
  med <- \(g) median(d$waarde[d$peilgroep == g])
  uit <- tibble(
    n_vernat = sum(d$peilgroep == "peilopzet (Vernat?)"),
    n_enquete = sum(d$peilgroep == "alleen volgens beheerenquête"),
    n_geen = sum(d$peilgroep == "geen peilopzet"),
    mediaan_vernat = med("peilopzet (Vernat?)"),
    mediaan_enquete = med("alleen volgens beheerenquête"),
    mediaan_geen = med("geen peilopzet"),
    schatting_alle = NA_real_, p_alle = NA_real_,
    schatting_enquete = NA_real_, p_enquete = NA_real_,
    schatting_binnen = NA_real_, p_binnen = NA_real_, n_binnen = NA_integer_,
    mediaan_vernat_binnen = NA_real_, mediaan_ander_binnen = NA_real_
  )
  fit <- tryCatch(suppressWarnings(suppressMessages(
    lmerTest::lmer(waarde ~ peilgroep + (1 | gebied), data = d)
  )), error = \(e) NULL)
  if (!is.null(fit)) {
    co <- summary(fit)$coefficients
    rij_v <- "peilgroeppeilopzet (Vernat?)"
    rij_e <- "peilgroepalleen volgens beheerenquête"
    if (rij_v %in% rownames(co)) {
      uit$schatting_alle <- co[rij_v, "Estimate"]
      uit$p_alle <- co[rij_v, "Pr(>|t|)"]
    }
    if (rij_e %in% rownames(co)) {
      uit$schatting_enquete <- co[rij_e, "Estimate"]
      uit$p_enquete <- co[rij_e, "Pr(>|t|)"]
    }
  }
  d_b <- filter(d, gebied %in% gebieden_vergelijk)
  if (n_distinct(d_b$vernat) == 2 && nrow(d_b) > n_distinct(d_b$gebied) + 2) {
    co_b <- summary(lm(waarde ~ vernat + gebied, data = d_b))$coefficients
    uit$schatting_binnen <- co_b["vernatTRUE", "Estimate"]
    uit$p_binnen <- co_b["vernatTRUE", "Pr(>|t|)"]
    uit$n_binnen <- nrow(d_b)
    uit$mediaan_vernat_binnen <- median(d_b$waarde[d_b$vernat])
    uit$mediaan_ander_binnen <- median(d_b$waarde[!d_b$vernat])
  }
  uit
}

resultaten_peilopzet <- wp1_lang |>
  group_by(groep, naam, label, kolom, eenheid) |>
  group_modify(\(d, sleutel) toets_peilopzet(d)) |>
  ungroup() |>
  mutate(
    p_alle_bh = p.adjust(p_alle, method = "BH"),
    p_enquete_bh = p.adjust(p_enquete, method = "BH"),
    p_binnen_bh = p.adjust(p_binnen, method = "BH")
  )
print(resultaten_peilopzet |> select(naam, mediaan_geen, mediaan_enquete, mediaan_vernat, p_alle, p_enquete, p_binnen, p_binnen_bh), n = 30)
fwrite(resultaten_peilopzet, paste0(figuren_dir, "tabel_peilopzet_wp1.csv"), sep = ";", dec = ",")

# 4. Figuren -----------------------------------------------------------------------
labels_facet <- resultaten_peilopzet |>
  transmute(
    kolom,
    facet = paste0(
      label, "\nVernat? p = ", p_label(p_alle), "; enquête p = ", p_label(p_enquete),
      "; binnen gebied p = ", p_label(p_binnen)
    ),
    niet_sign = (is.na(p_alle) | p_alle >= 0.05) & (is.na(p_enquete) | p_enquete >= 0.05) &
      (is.na(p_binnen) | p_binnen >= 0.05)
  )
facet_volgorde <- labels_facet$facet[match(kenmerken$kolom, labels_facet$kolom)]
tekst_niet_sign <- labels_facet |>
  filter(niet_sign) |>
  mutate(facet = factor(facet, levels = facet_volgorde))

# Kleur per gebied met peilopzet (Vernat?); overige gebieden grijs
gebieden_met_peil <- gebieden_peil |> filter(n_vernat > 0) |> pull(Gebiedsnaam)
kleuren_gebied <- setNames(
  c(okabe_ito[1:7], "#882255", "#44AA99", "#332288", "#999933", "#AA4499")[seq_along(gebieden_met_peil)],
  gebieden_met_peil
)
kleuren_gebied <- c(kleuren_gebied, "overige gebieden" = "grey70")

n_groep <- count(wp1, peilgroep)
groep_tekst <- paste0(n_groep$n, " ", n_groep$peilgroep, collapse = ", ")

# 4a. Alle WP1-locaties, drie groepen
plot_peilopzet <- wp1_lang |>
  left_join(select(labels_facet, kolom, facet), by = "kolom") |>
  mutate(
    facet = factor(facet, levels = facet_volgorde),
    gebied_kleur = if_else(Gebiedsnaam %in% gebieden_met_peil, Gebiedsnaam, "overige gebieden")
  ) |>
  ggplot(aes(x = peilgroep, y = waarde)) +
  geom_boxplot(outlier.shape = NA, fill = "grey95", colour = "grey55", width = 0.55) +
  geom_point(aes(colour = gebied_kleur), position = position_jitter(width = 0.15, height = 0, seed = 1), size = 1.9, alpha = 0.85) +
  geom_text(
    data = tekst_niet_sign, aes(x = -Inf, y = Inf, label = "niet significant"), inherit.aes = FALSE,
    hjust = -0.06, vjust = 1.5, size = 3, colour = "grey35", fontface = "italic"
  ) +
  scale_x_discrete(labels = \(x) str_wrap(x, 14)) +
  scale_colour_manual(values = kleuren_gebied) +
  facet_wrap(~facet, scales = "free_y", ncol = 5) +
  labs(
    title = "Peilopzet en de 2a-zone: WP1-locaties met en zonder peilopzet",
    subtitle = paste0(
      "Eén meting per locatie (", groep_tekst, "). Vergelijking van de toestand, geen verandering in de tijd.\n",
      "Vernat? p en enquête p: t.o.v. geen peilopzet, gemengd model met gebied random. ",
      "Binnen gebied p: Vernat? t.o.v. andere locaties in dezelfde gebieden (", paste(gebieden_vergelijk, collapse = ", "), ")."
    ),
    caption = "\"niet significant\" = alle drie de p \u2265 0,05 (zonder correctie voor meervoudig toetsen).",
    x = NULL, y = "Waarde (eenheid per paneel)", colour = "Gebied"
  ) +
  theme_peil +
  guides(colour = guide_legend(ncol = 4))
print(plot_peilopzet)
ggsave(paste0(figuren_dir, "rapport/peilopzet_wp1_alle.png"), plot_peilopzet, width = 19, height = 16, dpi = 300)

# 4b. Binnen gebieden met vernatte én andere locaties, alle kenmerken (bijlage)
plot_peilopzet_binnen <- wp1_lang |>
  filter(gebied %in% gebieden_vergelijk) |>
  left_join(select(labels_facet, kolom, facet), by = "kolom") |>
  mutate(facet = factor(facet, levels = facet_volgorde)) |>
  ggplot(aes(x = Gebiedsnaam, y = waarde, colour = peilgroep)) +
  geom_point(position = position_dodge(width = 0.6), size = 2.2) +
  scale_colour_manual(values = kleuren_peilgroep) +
  facet_wrap(~facet, scales = "free_y", ncol = 5) +
  labs(
    title = "Peilopzet en de 2a-zone: binnen gebieden met vernatte én andere locaties (WP1)",
    subtitle = "Punt = locatie. Binnen gebied p: waarde ~ Vernat? (ja/nee) + gebied.",
    x = NULL, y = "Waarde (eenheid per paneel)", colour = NULL
  ) +
  theme_peil +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(plot_peilopzet_binnen)
ggsave(paste0(figuren_dir, "bijlage/peilopzet_wp1_binnen_gebied.png"), plot_peilopzet_binnen, width = 19, height = 16, dpi = 300)

# 5. Verwachte effecten: vernatte locaties t.o.v. andere locaties in --------
# dezelfde gebieden, voor de kenmerken waarop vooraf een effect werd verwacht
# (breedte zone 2a/2b, taludhoek, waterbreedte, waterdiepte, slibdikte, redox
# slib, P-AL). Per gebied de locaties en de mediaan per groep; p uit
# waarde ~ Vernat? (ja/nee) + gebied, met BH-correctie alleen over deze set.
verwacht_res <- resultaten_peilopzet |>
  filter(kolom %in% verwacht_kolommen) |>
  mutate(p_binnen_bh_verwacht = p.adjust(p_binnen, method = "BH"))
fwrite(verwacht_res, paste0(figuren_dir, "tabel_peilopzet_verwachte_effecten.csv"), sep = ";", dec = ",")

verwacht_labels <- verwacht_res |>
  transmute(
    kolom,
    facet = paste0(
      label, "\nverschil vernat \u2212 andere = ", trimws(formatC(schatting_binnen, digits = 2, format = "g")), " ", eenheid,
      "\np = ", p_label(p_binnen), "; p (BH) = ", p_label(p_binnen_bh_verwacht), " (n = ", n_binnen, ")"
    ),
    niet_sign = is.na(p_binnen) | p_binnen >= 0.05
  )
verwacht_volgorde <- verwacht_labels$facet[match(intersect(verwacht_kolommen, verwacht_labels$kolom), verwacht_labels$kolom)]

verwacht_data <- wp1_lang |>
  filter(kolom %in% verwacht_kolommen, gebied %in% gebieden_vergelijk) |>
  mutate(vergelijk = if_else(vernat, "vernat (Vernat?)", "andere locaties in het gebied")) |>
  left_join(select(verwacht_labels, kolom, facet), by = "kolom") |>
  mutate(facet = factor(facet, levels = verwacht_volgorde))

verwacht_verschil_gebied <- verwacht_data |>
  summarise(.by = c(naam, Gebiedsnaam), verschil = median(waarde[vernat]) - median(waarde[!vernat]))
fwrite(verwacht_verschil_gebied, paste0(figuren_dir, "tabel_peilopzet_verwachte_effecten_per_gebied.csv"), sep = ";", dec = ",")
print(verwacht_verschil_gebied, n = 80)

plot_verwacht <- verwacht_data |>
  ggplot(aes(x = Gebiedsnaam, y = waarde, colour = vergelijk)) +
  geom_point(position = position_dodge(width = 0.5), size = 2.1, alpha = 0.85) +
  stat_summary(aes(group = vergelijk), fun = median, geom = "point", shape = 18, size = 4.2,
               position = position_dodge(width = 0.5)) +
  geom_text(
    data = filter(verwacht_labels, niet_sign) |> mutate(facet = factor(facet, levels = verwacht_volgorde)),
    aes(x = -Inf, y = Inf, label = "niet significant"), inherit.aes = FALSE,
    hjust = -0.06, vjust = 1.5, size = 3, colour = "grey35", fontface = "italic"
  ) +
  scale_colour_manual(values = c("andere locaties in het gebied" = okabe_ito[1], "vernat (Vernat?)" = okabe_ito[5])) +
  facet_wrap(~facet, scales = "free_y", ncol = 5) +
  labs(
    title = "Verwachte effecten van peilopzet: vernatte locaties en andere locaties in dezelfde gebieden (WP1)",
    subtitle = paste0(
      "Punt = locatie, ruit = mediaan per gebied en groep. Toets: waarde ~ Vernat? (ja/nee) + gebied, alleen gebieden met vernatte én andere locaties.\n",
      "BH-correctie over deze ", nrow(verwacht_res), " vooraf verwachte kenmerken. Stein Noord (vernat) en Stein Zuid (niet vernat) samen als één gebied."
    ),
    caption = "\"niet significant\" = p \u2265 0,05 (zonder correctie).",
    x = NULL, y = "Waarde (eenheid per paneel)", colour = NULL
  ) +
  theme_peil +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
print(plot_verwacht)
ggsave(paste0(figuren_dir, "rapport/peilopzet_verwachte_effecten_binnen_gebied.png"), plot_verwacht, width = 20, height = 11, dpi = 300)
