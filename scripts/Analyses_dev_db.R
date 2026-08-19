## ---------- helpers ----------
norm_name <- function(x) {
  x <- gsub("[µμ]", "u", x, perl = TRUE)
  tolower(gsub("[^a-z0-9]", "", x, perl = TRUE))
}

clean_micro <- function(x) gsub("[µμ]", "u", x, perl = TRUE)

safe_median_num <- function(x) {
  x <- as.numeric(x)
  if (all(is.na(x))) NA_real_ else median(x, na.rm = TRUE)
}

is_constant_within_year <- function(dt, col) {
  if (is.list(dt[[col]])) return(FALSE)
  yrs <- dt[!is.na(jaar), unique(jaar)]
  if (length(yrs) == 0L) return(FALSE)
  all(vapply(yrs, function(y) {
    v <- dt[jaar == y, get(col)]
    uniqueN(v[!is.na(v)]) <= 1L
  }, logical(1)))
}


# 0. filter correct data 4 analysis ----------------------------------------------------
# abio_proj <- abio_proj[WP %in% c('WP1','WP2'),]
# abio_proj <- abio_proj[!is.na(SlootID) & !is.na(jaar) & !is.na(instanceID_veg) & !is.na(instanceID_abio) , ]
# abio_proj_complete <- abio_proj
abio_proj <- abio_proj_complete #reset
abio_proj <- abio_proj[MeenemenDataAnalyse_totaal == 'ja',] # voor xgboost
# 1. cluster analyse vergelijking met kaart clusters uit verkenningsfase--------------------------------------------------------
setDT(abio_proj)
# abio variabelen volgens jouw mapping:
# drglg -> drlg, watbte -> breedtewl, OS_perc_OR_25 -> SOM_LOI, Z_CLAY_SA_OR_25 -> A_CLAY_MI
# 1. Cluster vergelijking: variabele-gebaseerd vs ruimtelijk per SlootID ----------------
vars_clust  <- c("drglg", "watbte", "OS_perc_OR_25", "Z_CLAY_SA_OR_25") # in abio_proj
scale_means <- sapply(vars_clust, function(v) mean(abio_proj[[v]], na.rm = TRUE))
scale_sds   <- sapply(vars_clust, function(v) sd(abio_proj[[v]],   na.rm = TRUE))

# Gestandaardiseerde clustermediane (uit data import/ waar varaibelen al zijn gemapped) per cluster
med_s <- data.table(
  clusters = cluster.med$clusters,
  drglg_z  = (cluster.med$drglg           - scale_means["drglg"])           / scale_sds["drglg"],
  watbte_z = (cluster.med$watbte          - scale_means["watbte"])          / scale_sds["watbte"],
  OS_z     = (cluster.med$OS_perc_OR_25   - scale_means["OS_perc_OR_25"])   / scale_sds["OS_perc_OR_25"],
  klei_z   = (cluster.med$Z_CLAY_SA_OR_25 - scale_means["Z_CLAY_SA_OR_25"]) / scale_sds["Z_CLAY_SA_OR_25"]
)

# Gestandaardiseerde waarden per SlootID
locs <- abio_proj[
  !is.na(drglg) & !is.na(watbte) & !is.na(OS_perc_OR_25) & !is.na(Z_CLAY_SA_OR_25),
  .(SlootID,
    cluster_ruimtelijk = clusters,
    drglg_z  = (drglg           - scale_means["drglg"])           / scale_sds["drglg"],
    watbte_z = (watbte          - scale_means["watbte"])          / scale_sds["watbte"],
    OS_z     = (OS_perc_OR_25   - scale_means["OS_perc_OR_25"])   / scale_sds["OS_perc_OR_25"],
    klei_z   = (Z_CLAY_SA_OR_25 - scale_means["Z_CLAY_SA_OR_25"]) / scale_sds["Z_CLAY_SA_OR_25"]
  )
] |> unique(by = "SlootID")

# Cross join: elke locatie x elk cluster, dan dichtstbijzijnde via Euclidische afstand
locs[, row_id := .I]
cross <- CJ(row_id = locs$row_id, cl_idx = seq_len(nrow(med_s)))
cross[, `:=`(
  drglg_z_loc  = locs$drglg_z [row_id],
  watbte_z_loc = locs$watbte_z[row_id],
  OS_z_loc     = locs$OS_z    [row_id],
  klei_z_loc   = locs$klei_z  [row_id],
  drglg_z_med  = med_s$drglg_z [cl_idx],
  watbte_z_med = med_s$watbte_z[cl_idx],
  OS_z_med     = med_s$OS_z    [cl_idx],
  klei_z_med   = med_s$klei_z  [cl_idx],
  clusters     = med_s$clusters[cl_idx]
)]
cross[, afstand := sqrt(
  (drglg_z_loc  - drglg_z_med)^2 +
  (watbte_z_loc - watbte_z_med)^2 +
  (OS_z_loc     - OS_z_med)^2    +
  (klei_z_loc   - klei_z_med)^2
)]

best <- cross[, .SD[which.min(afstand)], by = row_id][, .(row_id, cluster_var = clusters, afstand_min = afstand)]

vergelijking <- locs[best, on = "row_id"][,
  .(SlootID, cluster_ruimtelijk, cluster_var, afstand_min,
    match = cluster_ruimtelijk == cluster_var)]

cat("Overeenkomst ruimtelijk vs. variabelen:", round(mean(vergelijking$match, na.rm = TRUE) * 100, 1), "%\n")

# Verwarringsmatrix
conf <- vergelijking[, .N, by = .(cluster_ruimtelijk, cluster_var)]
ggplot(conf, aes(x = factor(cluster_var), y = factor(cluster_ruimtelijk))) +
  geom_tile(aes(fill = N), color = "white") +
  geom_text(aes(label = N), size = 4) +
  scale_fill_gradient(low = "white", high = "#2166ac") +
  labs(
    x     = "Cluster o.b.v. variabelen (abio_proj)",
    y     = "Cluster ruimtelijk",
    title = "Vergelijking ruimtelijk vs. variabele-gebaseerde cluster",
    fill  = "Aantal locaties"
  ) +
  theme_minimal(base_size = 13)

# visualisatie
# locaties -> sf
if (inherits(locaties, "sf")) {
  loc_sf <- locaties
} else if ("geom" %in% names(locaties)) {
  loc_sf <- st_as_sf(locaties, sf_column_name = "geom")
} else if ("geometry" %in% names(locaties)) {
  loc_sf <- st_as_sf(locaties, sf_column_name = "geometry")
}

# Koppel clusters
cluster_map <- unique(vergelijking[, .(SlootID, cluster_ruimtelijk, cluster_var)])
loc_cl <- loc_sf |>
  left_join(as.data.frame(cluster_map), by = "SlootID") |>
  filter(!is.na(cluster_var)) |>
  group_by(SlootID, cluster_var) |>
  summarise(do_union = TRUE, .groups = "drop")

# CRS afdwingen naar RD
if (is.na(st_crs(loc_cl))) {
  st_crs(loc_cl) <- 28992
} else {
  loc_cl <- st_transform(loc_cl, 28992)
}

# Zoom op data-extent (meters, RD)
bb <- st_bbox(loc_cl)
pad <- 15000  # 15 km marge
xlim <- c(bb["xmin"] - pad, bb["xmax"] + pad)
ylim <- c(bb["ymin"] - pad, bb["ymax"] + pad)

# Maak van (multi)lijnen één representatief punt per feature
loc_mid <- loc_cl |>
  st_cast("MULTILINESTRING", warn = FALSE) |>
  st_line_merge() |>
  st_cast("LINESTRING", do_split = FALSE, warn = FALSE) |>
  st_point_on_surface()

p_clusters_nl <- ggplot() +
  ggspatial::annotation_map_tile(
    type = "cartolight",
    cachedir = "/osm_cache",
    zoomin = 1,
    progress = "none",
    quiet = TRUE
  ) +
  geom_sf(data = loc_cl, color = "grey60", linewidth = 0.6, alpha = 0.4) +
  geom_sf(
    data = loc_mid,
    aes(fill = factor(cluster_var)),
    shape = 21,
    size = 6,
    color = "black",
    stroke = 0.4,
    alpha = 0.95
  ) +
  coord_sf(crs = st_crs(28992), xlim = xlim, ylim = ylim, expand = FALSE) +
  scale_fill_brewer(palette = "Set2", name = "Cluster abio") +
  theme_minimal(base_size = 14)

print(p_clusters_nl)
ggsave(
  "output/AlleGebieden/Tussenrapportage/Clusters_kaart_sectie1.png",
  plot = p_clusters_nl,
  width = 30, height = 25, units = "cm", dpi = 300
)

# Bereik in cluster (GIS-laag) vs abio_proj per variabele
# cluster gebruikt: drlg, breedtewl, A_SOM_LOI, A_CLAY_MI (voor cluster.med)
# abio_proj gebruikt: drglg, watbte, OS_perc_OR_25, Z_CLAY_SA_OR_25

vars_info <- list(
  list(label = "Drooglegging",  cluster_col = "drlg",        abio_col = "drglg"),
  list(label = "Waterbreedte",  cluster_col = "breedtewl",   abio_col = "watbte"),
  list(label = "OS (%)",        cluster_col = "A_SOM_LOI",   abio_col = "OS_perc_OR_25"),
  list(label = "Klei (%)",      cluster_col = "A_CLAY_MI",   abio_col = "Z_CLAY_SA_OR_25")
)

rows <- rbindlist(lapply(vars_info, function(v) {
  cl <- cluster[[v$cluster_col]]
  ab <- abio_proj[[v$abio_col]]
  data.table(
    variabele = v$label,
    bron      = c("GIS-laag (cluster)", "Velddata (abio_proj)"),
    p10       = c(quantile(cl, 0.10, na.rm=TRUE), quantile(ab, 0.10, na.rm=TRUE)),
    mediaan   = c(median(cl, na.rm=TRUE),          median(ab, na.rm=TRUE)),
    p90       = c(quantile(cl, 0.90, na.rm=TRUE),  quantile(ab, 0.90, na.rm=TRUE))
  )
}))

rows[, bereik_p10_p90 := p90 - p10]

# Ratio van bereiken als maat voor schaalbereik-verschil
ratio <- rows[, .(ratio = bereik_p10_p90[bron == "GIS-laag (cluster)"] /
                          bereik_p10_p90[bron == "Velddata (abio_proj)"]), by = variabele]
print(ratio)

# Visualisatie: P10-mediaan-P90 per bron per variabele
ggplot(rows, aes(x = bron, y = mediaan, color = bron)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = p10, ymax = p90), width = 0.2, linewidth = 0.8) +
  facet_wrap(~variabele, scales = "free_y") +
  labs(
    title = "Schaalbereik GIS-laag vs. velddata (P10–P90)",
    x = NULL, y = NULL, color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", axis.text.x = element_blank())

# Voeg gebiedsnaam toe aan vergelijking via SlootID (eerste deel voor underscore)
locs_analysis <- copy(locs)
locs_analysis[, gebied := sub("_.*", "", SlootID)]
# Voeg ruimtelijke clusterinfo toe
locs_analysis <- locs_analysis[vergelijking[, .(SlootID, cluster_ruimtelijk, cluster_var, match)], on = "SlootID"]

# Variatie binnen ruimtelijke cluster per variabele: IQR en SD
# Gebruik abio_proj met gebied
abio_var <- abio_proj[
  !is.na(drglg) & !is.na(watbte) & !is.na(OS_perc_OR_25) & !is.na(Z_CLAY_SA_OR_25),
  .(SlootID, clusters,
    drglg        = drglg,
    watbte       = watbte,
    OS_perc      = OS_perc_OR_25,
    klei         = Z_CLAY_SA_OR_25)
] |> unique(by = "SlootID")
abio_var[, gebied := sub("_.*", "", SlootID)]

# Spreiding per cluster: SD per variabele
spreiding <- abio_var[, .(
  n            = .N,
  drglg_sd     = sd(drglg,   na.rm=TRUE),
  watbte_sd    = sd(watbte,  na.rm=TRUE),
  OS_sd        = sd(OS_perc, na.rm=TRUE),
  klei_sd      = sd(klei,    na.rm=TRUE)
), by = clusters][order(clusters)]

spreiding# Afwijking per locatie per variabele tov GIS-clustermediane (ongestandaardiseerd)
# Voeg clustermediane toe op basis van ruimtelijke cluster
med_orig <- cluster.med[, .(clusters, drglg_med = drglg, watbte_med = watbte,
                             OS_med = OS_perc_OR_25, klei_med = Z_CLAY_SA_OR_25)]

afwijking <- abio_var[med_orig, on = c(clusters = "clusters"), nomatch = 0]
afwijking[, `:=`(
  drglg_afw = drglg   - drglg_med,
  watbte_afw= watbte  - watbte_med,
  OS_afw    = OS_perc - OS_med,
  klei_afw  = klei    - klei_med
)]
# Gemiddelde absolute afwijking per gebied
afw_gebied <- afwijking[, .(
  drglg_mae  = mean(abs(drglg_afw),  na.rm=TRUE),
  watbte_mae = mean(abs(watbte_afw), na.rm=TRUE),
  OS_mae     = mean(abs(OS_afw),     na.rm=TRUE),
  klei_mae   = mean(abs(klei_afw),   na.rm=TRUE),
  drglg_bias = mean(drglg_afw,       na.rm=TRUE),  # systematisch te hoog/laag
  OS_bias    = mean(OS_afw,          na.rm=TRUE)
), by = gebied][order(-drglg_mae)]

afw_gebied# Long format voor plot: afwijking per variabele per gebied
afw_long <- melt(afw_gebied, 
  id.vars = "gebied",
  measure.vars = c("drglg_mae", "OS_mae", "watbte_mae", "klei_mae"),
  variable.name = "variabele", value.name = "mae"
)
afw_long[, variabele := factor(variabele,
  levels = c("drglg_mae", "watbte_mae", "OS_mae", "klei_mae"),
  labels = c("Drooglegging (m)", "Waterbreedte (m)", "OS (%)", "Klei (%)")
)]
# Schaal per variabele zodat vergelijking visueel zinvol is
afw_long[, mae_scaled := mae / max(mae), by = variabele]

ggplot(afw_long, aes(x = mae_scaled, y = reorder(gebied, mae_scaled))) +
  geom_col() +
  facet_wrap(~variabele, scales = "free_x", nrow = 1) +
  labs(
    title = "Gemiddelde absolute afwijking veldmeting t.o.v. GIS-clustermediane per gebied",
    subtitle = "Waarden geschaald per variabele (1 = grootste afwijking)",
    x = "Relatieve afwijking (geschaald)", y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))# Bias drooglegging: negatief = veldmeting lager dan GIS (zoals Hegewarren verwacht)
# Toon ook OS bias: negatief = veldmeting lager dan GIS
afw_gebied[, .(gebied, drglg_bias = round(drglg_bias, 3), OS_bias = round(OS_bias, 1))][order(drglg_bias)]


# Spreiding binnen ruimtelijke cluster: boxplot per cluster per variabele
afwijking_long <- melt(afwijking,
  id.vars = c("SlootID", "gebied", "clusters"),
  measure.vars = c("drglg_afw", "watbte_afw", "OS_afw", "klei_afw"),
  variable.name = "variabele", value.name = "afwijking"
)
afwijking_long[, variabele := factor(variabele,
  levels = c("drglg_afw", "watbte_afw", "OS_afw", "klei_afw"),
  labels = c("Drooglegging (m)", "Waterbreedte (m)", "OS (%)", "Klei (%)")
)]
ggplot(afwijking_long, aes(x = factor(clusters), y = afwijking)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = gebied), width = 0.2, size = 1.5, alpha = 0.7) +
  facet_wrap(~variabele, scales = "free_y", nrow = 2) +
  labs(
    title = "Afwijking veldmeting t.o.v. GIS-clustermediane per cluster",
    subtitle = "Elke punt = één locatie (SlootID), gekleurd per gebied",
    x = "Ruimtelijke cluster", y = "Afwijking (veld − GIS mediaan)", color = "Gebied"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right", legend.text = element_text(size = 7),
        legend.key.size = unit(0.4, "cm"))

# 2. clusteranalyse van alle veestvariabelen in abio_proj waar op minstens 3/4 van waarnemingen data van beschikbaar is-------------------------------------------------------------------------
setDT(abio_proj)
setDT(pars)
## 1) basisfilter ----------
abio_base <- copy(abio_proj)[
  WP %in% c("WP1", "WP2") &
    !is.na(SlootID) & !is.na(jaar) &
    !is.na(instanceID_veg) & !is.na(instanceID_abio)
]

## 2) drop: admin + geen variatie + LIAB/M3 via pars ----------
keep_cols <- c("SlootID", "jaar", "WP", "clusters")

admin_pattern <- paste(
  c(
    "instanceid", "^id$", "_id$", "^id_", "uuid", "objectid", "aan_id",
    "^wp$", "datum", "date", "complete", "accur", "^start_", "^end_",
    "shape_", "^geom$", "^geometry$", "creator", "device", "submission", "versie", "sync"
  ),
  collapse = "|"
)

admin_cols <- setdiff(
  names(abio_base)[grepl(admin_pattern, tolower(names(abio_base)), perl = TRUE)],
  keep_cols
)

candidate_cols_nv <- setdiff(names(abio_base), c(keep_cols, admin_cols))
no_variation_cols <- candidate_cols_nv[
  vapply(candidate_cols_nv, function(cl) is_constant_within_year(abio_base, cl), logical(1))
]

## --- LIAB/M3 DROP via pars$methode (behalve klei/clay) ---
abio_map <- data.table(
  abio_name = names(abio_base),
  var_norm = norm_name(names(abio_base))
)

pars_map <- copy(pars)[
  ,
  .(
    variable,
    parameter,
    methode,
    var_norm = norm_name(variable),
    meth_norm = tolower(trimws(methode)),
    keep_klei = grepl("clay|klei", tolower(parameter))
  )
]

drop_norm <- unique(
  pars_map[meth_norm %chin% c("liab", "m3") & !keep_klei, var_norm]
)

liab_m3_drop <- unique(
  abio_map[var_norm %chin% drop_norm, abio_name]
)

drop_cols <- unique(c(admin_cols, no_variation_cols, liab_m3_drop))
drop_cols <- intersect(drop_cols, names(abio_base))

abio_proj_clean <- abio_base[, setdiff(names(abio_base), drop_cols), with = FALSE]

## Harde check: via pars mogen geen liab/m3 (excl klei) over zijn
remaining_liab_m3 <- unique(
  abio_map[abio_name %in% names(abio_proj_clean)][
    pars_map,
    on = "var_norm",
    nomatch = 0L
  ][meth_norm %chin% c("liab", "m3") & !keep_klei, abio_name]
)

if (length(remaining_liab_m3) > 0L) {
  stop(
    paste0(
      "Nog LIAB/M3 kolommen aanwezig na cleaning: ",
      paste(remaining_liab_m3, collapse = ", ")
    )
  )
}



## 3) variabelenselectie >= 75% ----------
id_exclude <- c(
  "SlootID", "jaar", "WP", "instanceID_veg", "instanceID_abio",
  "geom", "geometry", "clusters", "cluster_abio", "cluster_loc"
)

logi_cols <- names(abio_proj_clean)[vapply(abio_proj_clean, is.logical, logical(1))]
if (length(logi_cols) > 0L) {
  abio_proj_clean[, (logi_cols) := lapply(.SD, as.numeric), .SDcols = logi_cols]
}

is_num <- vapply(abio_proj_clean, function(x) is.numeric(x) || is.integer(x), logical(1))
candidate_vars <- setdiff(names(abio_proj_clean)[is_num], id_exclude)

coverage_dt <- data.table(
  variable = candidate_vars,
  non_na_frac = vapply(candidate_vars, function(v) mean(!is.na(abio_proj_clean[[v]])), numeric(1))
)[order(-non_na_frac)]

selected_vars <- coverage_dt[non_na_frac >= 0.75, variable]

if (length(selected_vars) < 3L) {
  stop("Te weinig variabelen met >=75% dekking voor clusteranalyse.")
}

## 4) naar SlootID niveau ----------
abio_sid <- abio_proj_clean[
  ,
  lapply(.SD, safe_median_num),
  by = .(SlootID),
  .SDcols = selected_vars
]

abio_sid[, (selected_vars) := lapply(.SD, function(x) {
  x <- as.numeric(x)
  x[!is.finite(x)] <- NA_real_
  x
}), .SDcols = selected_vars]

for (v in selected_vars) {
  med_v <- abio_sid[is.finite(get(v)), median(get(v), na.rm = TRUE)]
  abio_sid[!is.finite(get(v)) | is.na(get(v)), (v) := med_v]
}

var_ok <- vapply(selected_vars, function(v) sd(abio_sid[[v]], na.rm = TRUE) > 0, logical(1))
vars_km <- selected_vars[var_ok]

if (length(vars_km) < 3L) {
  stop("Te weinig variabelen met variatie na opschonen.")
}

## 5) kmeans ----------
X <- scale(as.matrix(abio_sid[, ..vars_km]))
keep <- apply(X, 1, function(r) all(is.finite(r)))
X_km <- X[keep, , drop = FALSE]

if (nrow(X_km) < 3L) {
  stop("Te weinig complete rijen voor clustering.")
}

k <- if ("clusters" %in% names(abio_proj_clean) && any(!is.na(abio_proj_clean$clusters))) {
  uniqueN(as.character(abio_proj_clean[!is.na(clusters), clusters]))
} else {
  6L
}
k <- as.integer(max(2L, k))
k_use <- min(k, nrow(X_km) - 1L)
k_use <- max(2L, k_use)

set.seed(123)
km <- kmeans(X_km, centers = k_use, nstart = 100)

abio_sid[, cluster_abio := NA_character_]
abio_sid[which(keep), cluster_abio := as.character(km$cluster)]
abio_sid[, cluster_abio := factor(cluster_abio)]

## 6) variabele-invloed (eta²) ----------
eta2_dt <- rbindlist(lapply(vars_km, function(v) {
  d <- abio_sid[!is.na(cluster_abio), .(y = get(v), g = cluster_abio)]
  y_bar <- mean(d$y, na.rm = TRUE)
  ss_total <- sum((d$y - y_bar)^2, na.rm = TRUE)
  grp <- d[, .(n = .N, m = mean(y, na.rm = TRUE)), by = g]
  ss_between <- sum(grp$n * (grp$m - y_bar)^2, na.rm = TRUE)
  data.table(variable = v, eta2 = ifelse(ss_total > 0, ss_between / ss_total, NA_real_))
}), use.names = TRUE)[order(-eta2)]

top_n <- min(12L, nrow(eta2_dt))
top_vars <- eta2_dt[1:top_n, variable]

## 7) boxplots ----------
box_dt <- melt(
  abio_sid[!is.na(cluster_abio), c("SlootID", "cluster_abio", top_vars), with = FALSE],
  id.vars = c("SlootID", "cluster_abio"),
  variable.name = "variabele",
  value.name = "waarde"
)

p_box <- ggplot(box_dt, aes(x = cluster_abio, y = waarde, fill = cluster_abio)) +
  geom_boxplot(outlier.alpha = 0.25) +
  facet_wrap(~ variabele, scales = "free_y", ncol = 4) +
  labs(
    title = "Spreiding per cluster voor belangrijkste variabelen",
    x = "Cluster abio",
    y = "Waarde"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")

## 8) kaart ----------
if (inherits(locaties, "sf")) {
  loc_sf <- locaties
} else if ("geom" %in% names(locaties)) {
  loc_sf <- st_as_sf(locaties, sf_column_name = "geom")
} else if ("geometry" %in% names(locaties)) {
  loc_sf <- st_as_sf(locaties, sf_column_name = "geometry")
} else {
  stop("Geen geometriekolom gevonden in 'locaties'.")
}

cluster_map <- unique(abio_sid[!is.na(cluster_abio), .(SlootID, cluster_abio)])

loc_cl <- loc_sf |>
  left_join(as.data.frame(cluster_map), by = "SlootID") |>
  filter(!is.na(cluster_abio)) |>
  group_by(SlootID, cluster_abio) |>
  summarise(do_union = TRUE, .groups = "drop")

if (nrow(loc_cl) == 0L) {
  stop("Geen ruimtelijke matches tussen locaties en clusterresultaten.")
}

if (is.na(st_crs(loc_cl))) {
  st_crs(loc_cl) <- 28992
} else {
  loc_cl <- st_transform(loc_cl, 28992)
}

bb <- st_bbox(loc_cl)
pad <- 15000
xlim <- c(bb["xmin"] - pad, bb["xmax"] + pad)
ylim <- c(bb["ymin"] - pad, bb["ymax"] + pad)

loc_mid <- st_point_on_surface(loc_cl)

p_clusters_nl <- ggplot() +
  ggspatial::annotation_map_tile(
    type = "cartolight",
    cachedir = "/osm_cache",
    zoomin = 1,
    progress = "none",
    quiet = TRUE
  ) +
  geom_sf(data = loc_cl, color = "grey60", linewidth = 0.6, alpha = 0.35) +
  geom_sf(
    data = loc_mid,
    aes(fill = cluster_abio),
    shape = 21,
    size = 6,
    color = "black",
    stroke = 0.4,
    alpha = 0.95
  ) +
  coord_sf(crs = st_crs(28992), xlim = xlim, ylim = ylim, expand = FALSE) +
  scale_fill_brewer(palette = "Set2", name = "Cluster abio") +
  theme_minimal(base_size = 13)

print(p_box)
ggsave(
  "output/AlleGebieden/Tussenrapportage/Clusteranalyse_boxplots.png",
  plot = p_box,
  width = 35, height = 25, units = "cm", dpi = 300
)

print(p_clusters_nl)
ggsave(
  "output/AlleGebieden/Tussenrapportage/Clusteranalyse_kaart.png",
  plot = p_clusters_nl,
  width = 30, height = 25, units = "cm", dpi = 300
)

## 9) output ----------
clusteranalyse_veest <- list(
  checks = list(
    n_liab_m3_drop = length(liab_m3_drop),
    liab_m3_drop = liab_m3_drop,
    remaining_liab_m3 = remaining_liab_m3
  ),
  gebruikte_variabelen = coverage_dt[variable %in% selected_vars][order(-non_na_frac)],
  gebruikte_variabelen_kmeans = vars_km,
  variabelen_belang_eta2 = eta2_dt,
  top_variabelen = eta2_dt[1:top_n],
  data_met_clusters = abio_sid,
  plot_box = p_box,
  plot_kaart = p_clusters_nl,
  qc = list(
    n_cols_orig = ncol(abio_base),
    n_cols_clean = ncol(abio_proj_clean),
    n_admin_removed = length(admin_cols),
    n_no_variation_removed = length(no_variation_cols),
    n_liab_m3_removed = length(liab_m3_drop),
    n_vars_selected_75 = length(selected_vars),
    n_vars_kmeans = length(vars_km),
    n_rows_sid = nrow(abio_sid),
    n_rows_kmeans = nrow(X_km),
    k_used = k_use
  )
)
clusteranalyse_veest

# 3. XGBoost model --------------------------------------------------------
## versie met meerdere target variabelen tegelijk ---------------------------------
target_vars <- c("n_soorten_oev_zone2","oeverindex","Soortensamenstelling Helofyten","waterzone_1_subm_tot_perc","n_soorten_sub_zone1","Soortensamenstelling Hydrofyten","draagkracht_oever", 
                 "slib_redox_pH7","P-AL mg p2o5/100g_SB","max_slib")
# redox, draagkracht oever, aantal soorten, slibdikte
# Create Dutch translation mapping for target variables
target_names_dutch <- c(
  "waterzone_1_subm_tot_perc" = "Bedekking ondergedoken planten (%)",
  "n_soorten_oev_zone2" = "Aantal oeversoorten",
  "oeverindex" = "oeverindex",
  "n_soorten_sub_zone1" = "Aantal waterplantensoorten",
  "Soortensamenstelling Helofyten" = "Soortensamenstelling Helofyten",
  "Soortensamenstelling Hydrofyten" = "Soortensamenstelling Hydrofyten",
  "draagkracht_oever" = "Draagkracht oever (MPa)",
  "slib_redox_pH7" = "Redox slib bij pH7 (mV)",
  "P-AL mg p2o5/100g_SB" ="P-AL slib (mg P2O5/100g)",
  "max_slib" = "Slibdikte (m)")
# Define predictor variables with readable Dutch names
cols_corr <- c("drglg", "max_wtd", "zichtdiepte", "max_slib", "watbte","oeverzone_2b_breedte_cm", "oeverzone_2b_kaal_perc", 
               "holleoever", "tldk_wtrwtr_perc", "tldk_oevrwtr_perc", "slib_redox_pH7","slib_pH",
               "oevbte", "veentype_num", "Z_CLAY_SA_OR_25","OS_perc_OR_25","CEC_CO_mmol+/kg_OR_25",
               "draagkracht_oever", "dieptebin_min","draagkracht_perceel", "water_pH", "watertemp_C",
               'Cl_µmol/l_PW', "NH4_µmol/l_PW","P-AL mg p2o5/100g_SB","feP_PW",
              "Baggerfrequentie_per_jaar","Baggermoment_maand","Maaifrequentie_oever_per_jaar","Methode_toedienen_dierlijke_mest",
              "koebelasting_drinkende_koeien", "koeien_drinken_correctie","vernat_loc")
# Create readable Dutch names mapping
nederlandse_namen <- c(
  "drglg" = "Drooglegging (m)",
  "max_wtd" = "Maximale waterdiepte (m)", 
  "zichtdiepte" = "Doorzicht/waterdiepte",
  "max_slib" = "Maximale slibdikte (m)",
  "watbte" = "Waterbreedte (m)",
  "oeverzone_2b_breedte_cm" = "Breedte oevervegetatiezone 2b (cm)",
  "oeverzone_2b_kaal_perc" = "Bedekking kale oever zone 2b (%)",
  "holleoever" = "Onderholling (cm)",
  "tldk_wtrwtr_perc" = "Taludhoek onder waterlijn (%)",
  "tldk_oevrwtr_perc" = "Taludhoek oever (%)",
  "slib_redox_pH7" = "Redox slib bij pH7 (mV)",
  "slib_pH" = "pH slib",
  "oevbte" = "Oeverbreedte (m)",
  "veentype_num" = "Veentype (numeriek)",
  "Z_CLAY_SA_OR_25" = "Kleigehalte 25cm (%)",
  "OS_perc_OR_25" = "Organisch stofgehalte 25cm (%)",
  "CEC_CO_mmol+/kg_OR_25" = "Cation Exchange Capacity (mmol+/kg)",
  "draagkracht_oever" = "Draagkracht oever (MPa)",
  "dieptebin_min" = "Diepte laagste draagkracht (m)",
  "draagkracht_perceel" = "Draagkracht perceel (MPa)",
  "water_pH" = "Water pH",
  "watertemp_C" = "Watertemperatuur (°C)",
  "Cl_µmol/l_PW" = "Chloride poriewater(µmol/l)",
  "NH4_µmol/l_PW" = "Ammonium poriewater (µmol/l)",
  "P-AL mg p2o5/100g_SB" = "P-AL slib (mg P2O5/100g)",
  "feP_PW" = "FeP (mol/mol)",
  "Baggerfrequentie_per_jaar" = "Baggerfrequentie per jaar",
  "Baggermoment_maand" = "Baggermoment (maand)",
  "Maaifrequentie_oever_per_jaar"= "Maaifrequentie oever per jaar",
  "Methode_toedienen_dierlijke_mest" = "Methode toedienen dierlijke mest",
  "koebelasting_drinkende_koeien" = "Koebelasting drinkende koeien",
  "Koeien_drinken_sloot" = "Koeien drinken uit sloot correctie (ja/nee)"
)

## Preparation-------------------------------------------------------------------
library(xgboost)
# alle parameters naar nummeriek
# Create correlation matrix and p-value matrix with the SAME variables
abio_proj[,trofie := as.numeric(trofie)]
abio_proj[,draagkracht_perceel := as.numeric(draagkracht_perceel)]
# Handle non-numeric columns
abio_proj[,Maaifrequentie_oever_per_jaar := as.numeric(Maaifrequentie_oever_per_jaar)]
abio_proj[,Maaifrequentie_perceel_per_jaar := as.numeric(Maaifrequentie_perceel_per_jaar)]
abio_proj[, Baggermoment_maand := {
  # Reset naar originele character waarden via join (voorkomt problemen als kolom al numeriek is)
  orig <- unique(abio_proj_complete[, .(instanceID_abio, bagger_orig = as.character(Baggermoment_maand))])
  x <- orig[.SD, on = "instanceID_abio", bagger_orig]
  # n.v.t./onbekend -> NA, anders gemiddelde van komma-gescheiden maanden
  ifelse(
    is.na(x) | trimws(x) %in% c("n.v.t./onbekend", ""),
    NA_real_,
    sapply(strsplit(x, ","), function(m) mean(as.numeric(trimws(m)), na.rm = TRUE))
  )
}]
abio_proj[,Methode_toedienen_dierlijke_mest := fcase(
  Methode_toedienen_dierlijke_mest == "n.v.t./onbekend",              0,
  Methode_toedienen_dierlijke_mest == "n.v.t",            0,
  Methode_toedienen_dierlijke_mest == "sleepslang",          1,
  Methode_toedienen_dierlijke_mest == "sleepslang en mesttank", 2,
  Methode_toedienen_dierlijke_mest == "bovengronds_strooier", 3,
  Methode_toedienen_dierlijke_mest == "mesttank",            4,
  Methode_toedienen_dierlijke_mest == "injecteren",          5,
  default = NA_real_
)]
abio_proj[,Baggerfrequentie_per_jaar := as.numeric(Baggerfrequentie_per_jaar)]
abio_proj[,Aantal_koeien_vee_perceel_dag := as.numeric(Aantal_koeien_vee_perceel_dag)]
abio_proj[,Aantal_Koedagen_per_jaar := as.numeric(Aantal_Koedagen_per_jaar)]
abio_proj[,koebelasting_drinkende_koeien := as.numeric(koebelasting_drinkende_koeien)]
abio_proj[vernat_loc %in% c("ja", "tijdelijk","beperkt"), vernat_loc := 1]
abio_proj[vernat_loc %in% c("nee"," ")|is.na(vernat_loc), vernat_loc := 0]
abio_proj[,vernat_loc := as.numeric(vernat_loc)]
abio_proj[zichtdiepte>1,zichtdiepte := 1]
## Function to create XGBoost model for single target ---------------------------------
create_xgb_model <- function(target_var, predictors, data,
                             train_frac        = 0.6,
                             val_frac          = 0.2,
                             # Boom-complexiteit: verlaag max_depth + verhoog min_child_weight
                             # bij kleine n om overfitting te beperken
                             max_depth         = 4,
                             eta               = 0.05,
                             nrounds           = 500,
                             early_stopping    = 20,
                             subsample         = 0.8,
                             colsample_bytree  = 0.8,
                             min_child_weight  = 5,   # min obs per blad; hoger = minder overfit
                             gamma             = 0.1, # min gain voor split; hoger = conservatiever
                             lambda            = 2    # L2 regularisatie op gewichten
                             ) {
  # Select variables for model
  model_vars <- c("SlootID", target_var, predictors)
  model_data <- data[complete.cases(data[, ..model_vars]), ..model_vars]

  # Convert factors to numeric for xgboost ------------------------------
  factor_cols <- unique(c(
    names(model_data)[sapply(model_data, is.character)],
    names(model_data)[sapply(model_data, is.factor)]
  ))
  if (length(factor_cols) > 0) {
    model_data[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]
    model_data[, (factor_cols) := lapply(.SD, as.numeric), .SDcols = factor_cols]
  }

  # Remove target from predictors if it exists there
  predictors_clean <- predictors[!predictors %in% target_var]

  # Three-way split: train / validatie / test
  set.seed(123)
  n <- nrow(model_data)
  idx <- sample(n)
  train_end <- floor(train_frac * n)
  val_end   <- floor((train_frac + val_frac) * n)

  train_idx <- idx[1:train_end]
  val_idx   <- idx[(train_end + 1):val_end]
  test_idx  <- idx[(val_end + 1):n]

  X_train <- as.matrix(model_data[train_idx, ..predictors_clean]); storage.mode(X_train) <- "double"
  y_train <- as.double(model_data[train_idx, get(target_var)])

  X_val   <- as.matrix(model_data[val_idx,   ..predictors_clean]); storage.mode(X_val)   <- "double"
  y_val   <- as.double(model_data[val_idx,   get(target_var)])

  X_test  <- as.matrix(model_data[test_idx,  ..predictors_clean]); storage.mode(X_test)  <- "double"
  y_test  <- as.double(model_data[test_idx,  get(target_var)])

  # Create DMatrix
  dtrain <- xgb.DMatrix(data = X_train, label = y_train)
  dval   <- xgb.DMatrix(data = X_val,   label = y_val)
  dtest  <- xgb.DMatrix(data = X_test,  label = y_test)

  # Parameters
  params <- list(
    objective        = "reg:squarederror",
    eval_metric      = "rmse",
    max_depth        = max_depth,
    eta              = eta,
    subsample        = subsample,
    colsample_bytree = colsample_bytree,
    min_child_weight = min_child_weight,
    gamma            = gamma,
    lambda           = lambda
  )

  # Train: early stopping op validatieset (niet op test)
  xgb_model <- xgb.train(
    params                = params,
    data                  = dtrain,
    nrounds               = nrounds,
    watchlist             = list(train = dtrain, val = dval),
    early_stopping_rounds = early_stopping,
    verbose               = 2
  )

  # Feature importance
  importance <- xgb.importance(feature_names = colnames(X_train), model = xgb_model)
  importance[, Nederlandse_naam := nederlandse_namen[Feature]]

  # Predictions
  pred_train <- predict(xgb_model, X_train)
  pred_val   <- predict(xgb_model, X_val)
  pred_test  <- predict(xgb_model, X_test)

  # Performance per set
  rmse_fn <- function(y, yhat) sqrt(mean((y - yhat)^2))
  r2_fn   <- function(y, yhat) cor(y, yhat)^2

  return(list(
    model      = xgb_model,
    importance = importance,
    performance = data.table(
      target     = target_var,
      rmse_train = rmse_fn(y_train, pred_train),
      rmse_val   = rmse_fn(y_val,   pred_val),
      rmse_test  = rmse_fn(y_test,  pred_test),
      r2_train   = r2_fn(y_train,   pred_train),
      r2_val     = r2_fn(y_val,     pred_val),
      r2_test    = r2_fn(y_test,    pred_test),
      n_train    = length(y_train),
      n_val      = length(y_val),
      n_test     = length(y_test)
    ),
    predictions = data.table(
      SlootID   = model_data[c(train_idx, val_idx, test_idx), SlootID],
      actual    = c(y_train, y_val, y_test),
      predicted = c(pred_train, pred_val, pred_test),
      set       = c(rep("train", length(y_train)),
                    rep("val",   length(y_val)),
                    rep("test",  length(y_test)))
    )
  ))
}

# Train models for all target variables
xgb_models <- list()
model_performance <- list()
feature_importance_all <- list()

for(target in target_vars) {
  if(target %in% colnames(abio_proj)) {
    cat("Training model for:", target, "\n")
    
    # Get predictors (exclude current target from predictors)
    predictors <- cols_corr[!cols_corr %in% target & cols_corr %in% colnames(abio_proj)]
    
    # Train model
    model_result <- create_xgb_model(target, predictors, abio_proj)
    
    # Store results
    xgb_models[[target]] <- model_result$model
    model_performance[[target]] <- model_result$performance
    feature_importance_all[[target]] <- model_result$importance[, .(Feature, Nederlandse_naam, Gain)][order(-Gain)][1:10]
    feature_importance_all[[target]][, target_var := target]
  }
}

# Combine performance results
performance_summary <- rbindlist(model_performance)
print("Model Performance Summary:")
print(performance_summary)

## Permutation importance (model-agnostisch, op validatieset) -----------------
# Per predictor: permuteer kolom, meet RMSE-stijging en R²-daling op val-set.
# Hogere delta = predictor is belangrijker voor voorspellend vermogen.

calculate_permutation_importance <- function(model, X_val, y_val, n_rep = 5) {
  baseline_pred <- predict(model, X_val)
  baseline_rmse <- sqrt(mean((y_val - baseline_pred)^2))
  baseline_r2   <- cor(y_val, baseline_pred)^2

  results <- rbindlist(lapply(seq_len(ncol(X_val)), function(j) {
    feat <- colnames(X_val)[j]

    # Herhaal permutatie n_rep keer en neem gemiddelde (stabielere schatting)
    reps <- vapply(seq_len(n_rep), function(r) {
      X_perm       <- X_val
      X_perm[, j]  <- sample(X_perm[, j])
      pred_perm    <- predict(model, X_perm)
      rmse_perm    <- sqrt(mean((y_val - pred_perm)^2))
      r2_perm      <- tryCatch(cor(y_val, pred_perm)^2, error = function(e) NA_real_)
      c(rmse_perm, r2_perm)
    }, numeric(2))

    data.table(
      Feature      = feat,
      delta_rmse   = mean(reps[1, ], na.rm = TRUE) - baseline_rmse,
      delta_r2     = baseline_r2 - mean(reps[2, ], na.rm = TRUE)
    )
  }))

  results[order(-delta_rmse)]
}

# Loop over alle getrainde modellen
perm_importance_all <- list()

for (target in names(xgb_models)) {
  if (!target %in% colnames(abio_proj)) next
  cat("Permutation importance voor:", target, "\n")

  # Bouw validatieset opnieuw op (zelfde seed als in create_xgb_model)
  model_vars <- c("SlootID", target, cols_corr)
  model_vars <- model_vars[model_vars %in% colnames(abio_proj)]
  model_data <- abio_proj[complete.cases(abio_proj[, ..model_vars]), ..model_vars]

  factor_cols <- unique(c(
    names(model_data)[sapply(model_data, is.character)],
    names(model_data)[sapply(model_data, is.factor)]
  ))
  if (length(factor_cols) > 0) {
    model_data[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]
    model_data[, (factor_cols) := lapply(.SD, as.numeric), .SDcols = factor_cols]
  }

  predictors_clean <- colnames(model_data)[!colnames(model_data) %in% c("SlootID", target)]

  set.seed(123)
  n         <- nrow(model_data)
  idx       <- sample(n)
  train_end <- floor(0.6 * n)
  val_end   <- floor(0.8 * n)
  val_idx   <- idx[(train_end + 1):val_end]

  X_val <- as.matrix(model_data[val_idx, ..predictors_clean])
  y_val <- model_data[val_idx, get(target)]

  pi_dt <- calculate_permutation_importance(xgb_models[[target]], X_val, y_val)
  pi_dt[, Nederlandse_naam := nederlandse_namen[Feature]]
  pi_dt[is.na(Nederlandse_naam), Nederlandse_naam := Feature]
  pi_dt[, target_var := target]

  perm_importance_all[[target]] <- pi_dt
}

all_perm_importance <- rbindlist(perm_importance_all)
# Combine and visualize feature importance
all_importance <- rbindlist(feature_importance_all)
# Add Dutch target names and performance metrics to importance data
all_importance[, target_dutch := target_names_dutch[target_var]]
# Merge with performance summary to get RMSE and R² values
all_importance <- merge(all_importance, performance_summary[, .(target, rmse_test, r2_test)], 
                       by.x = "target_var", by.y = "target", all.x = TRUE)


# Voeg rmse_unit direct toe zodat deze altijd beschikbaar is na aanmaken all_importance
rmse_units <- c(
  "waterzone_1_subm_tot_perc"       = "%",
  "n_soorten_oev_zone2"             = "soorten",
  "n_soorten_sub_zone1"             = "soorten",
  "draagkracht_oever"               = "MPa",
  "slib_redox_pH7"                  = "mV",
  "max_slib"                        = "m",
  "oeverindex"                      = "-",
  "Soortensamenstelling Helofyten"  = "-",
  "Soortensamenstelling Hydrofyten" = "-"
)
all_importance[, rmse_unit := rmse_units[target_var]]

## VIP plots -------------------------------------------------------
# Bereken RMSE percentage PER TARGET en sla direct op in all_importance
for(target in unique(all_importance$target_var)) {
  if(target %in% colnames(abio_proj)) {
    predictors <- cols_corr[!cols_corr %in% target & cols_corr %in% colnames(abio_proj)]
    
    if(length(predictors) > 0) {
      rmse_pct <- calculate_rmse_percentage(abio_proj, target, predictors)
      
      # Sla DIRECT op in all_importance voor deze specifieke target
      all_importance[target_var == target, rmse_pct_target := rmse_pct]
      
      cat("Target:", target, "- RMSE als % van gemiddelde:", rmse_pct, "%\n")
    }
  }
}
# Check of het werkt
print("RMSE percentages per target in all_importance:")
for(target in unique(all_importance$target_var)) {
  target_data <- all_importance[target_var == target]
  cat(target, ":", unique(target_data$rmse_pct_target), "%\n")
}
# Voeg ontbrekende kolommen toe als ze niet bestaan
target_names_dutch_multiline <- c(
  "Soortensamenstelling Hydrofyten" = "Soortensamenstelling\nHydrofyten",
  "Soortensamenstelling Helofyten" = "Soortensamenstelling\nHelofyten",
  "oeverindex" = "Oeverindex",
  "waterzone_1_subm_tot_perc" = "Bedekking\nondergedoken\nplanten (%)",
  "n_soorten_oev_zone2" = "Aantal\noeversoorten",
  "n_soorten_sub_zone1" = "Aantal\nwaterplantensoorten",
  "draagkracht_oever" = "Draagkracht\noever (MPa)",
  "slib_redox_pH7" = "Redox slib\nbij pH7 (mV)",
  "P-AL mg p2o5/100g_SB" ="P-AL slib (mg P2O5/100g)",
  "max_slib" = "Slibdikte (m)"
)
all_importance[, target_dutch_multiline := target_names_dutch_multiline[target_var]]
if(!"correlation_direction" %in% colnames(all_importance)) {
  all_importance[, correlation_direction := mapply(
    function(target_var, predictor_var) {
      tryCatch({
        if(!target_var %in% colnames(abio_proj) || !predictor_var %in% colnames(abio_proj)) {
          return(NA_character_)
        }
        
        target_col <- abio_proj[[target_var]]
        pred_col <- abio_proj[[predictor_var]]
        
        if(is.numeric(target_col) && is.numeric(pred_col)) {
          corr <- cor(target_col, pred_col, use = "complete.obs")
          ifelse(corr > 0, "+", "-")
        } else {
          NA_character_
        }
      }, error = function(e) NA_character_)
    },
    target_var = target_var, 
    predictor_var = Feature,
    USE.NAMES = FALSE
  )]
}

# Definieer Okabe-Ito kleuren voor elke target
targets_present <- unique(na.omit(all_importance$target_var))
okabe_ito_base <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442",
  "#0072B2", "#D55E00", "#CC79A7", "#999999", "#000000"
)
okabe_ito_colors <- setNames(
  rep(okabe_ito_base, length.out = length(targets_present)),
  targets_present
)

# Maak plot titels met kleinere R² en RMSE tekst
all_importance[, plot_title_clean := paste0(target_dutch_multiline, 
                                            "\nR²: ", round(r2_test * 100, 1), "% | RMSE: ", round(rmse_test, 3), " ", rmse_unit)]

# VIP Plot met correlatierichting als kleur (Okabe-Ito), gesorteerd per facet op Gain
okabe_dir <- c("+" = "#0072B2", "-" = "#D55E00")

plot_data <- all_importance[!is.na(correlation_direction)][
  order(target_var, Gain)
][, facet_label := paste0(target_var, "__", Nederlandse_naam)
][, facet_label := factor(facet_label, levels = unique(facet_label))]

p_vip <- ggplot(plot_data, aes(
    x = facet_label,
    y = Gain,
    fill = correlation_direction
  )) +
  geom_col() +
  geom_text(
    aes(label = correlation_direction),
    hjust = -0.2,
    size = 3.5,
    fontface = "bold",
    color = "grey20"
  ) +
  facet_wrap(~plot_title_clean, scales = "free", ncol = 3) +
  scale_x_discrete(labels = function(x) sub(".*__", "", x)) +
  coord_flip() +
  scale_fill_manual(
    values = okabe_dir,
    labels = c("+" = "Positief verband", "-" = "Negatief verband"),
    name = "Correlatierichting"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "Belangrijkste verklarende variabelen wensbeelden (XGBoost)",
    subtitle = "Variable Importance (Gain) met correlatierichting op basis van Pearson correlatie",
    x = NULL,
    y = "Informatiewinst (Gain)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 8),
    axis.title.x = element_text(size = 10, face = "bold", margin = margin(t = 6)),
    strip.text = element_text(size = 8.5, lineheight = 1.1, face = "bold"),
    strip.background = element_rect(fill = "grey95", colour = "grey70", linewidth = 0.6),
    panel.border = element_rect(colour = "grey80", fill = NA, linewidth = 0.5),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 9.5, hjust = 0.5, color = "grey40"),
    plot.margin = margin(10, 15, 10, 10)
  )

print(p_vip)
ggsave(file = 'output/AlleGebieden/Tussenrapportage/XGBoost_feature_importance_okabe_clean.png', 
       plot = p_vip,
       width = 35, height = 30, units = 'cm', dpi = 800)

## VIP vergelijkingsplot: Gain vs Permutation importance ---------------------

# Top-10 per methode, daarna union zodat features uit beide lijsten zichtbaar zijn
perm_top10 <- all_perm_importance[
  , .SD[order(-delta_rmse)][1:min(.N, 10)], by = target_var
][, .(target_var, Feature, Nederlandse_naam, delta_rmse)]

gain_top10 <- all_importance[
  , .SD[order(-Gain)][1:min(.N, 10)], by = target_var
][, .(target_var, Feature, Nederlandse_naam, Gain)]

# Volle union van features per target
all_features <- unique(rbind(
  perm_top10[, .(target_var, Feature, Nederlandse_naam)],
  gain_top10[, .(target_var, Feature, Nederlandse_naam)]
))

# Koppel beide scores; ontbrekend = 0 (feature staat niet in die top-10)
all_features <- merge(all_features, perm_top10[, .(target_var, Feature, delta_rmse)],
                      by = c("target_var", "Feature"), all.x = TRUE)
all_features <- merge(all_features, gain_top10[, .(target_var, Feature, Gain)],
                      by = c("target_var", "Feature"), all.x = TRUE)
all_features[is.na(delta_rmse), delta_rmse := 0]
all_features[is.na(Gain),       Gain       := 0]

# Normaliseer binnen target naar 0-1
all_features[, delta_rmse_norm := delta_rmse / max(delta_rmse, na.rm = TRUE), by = target_var]
all_features[, gain_norm       := Gain       / max(Gain,       na.rm = TRUE), by = target_var]

# Plottitels koppelen
all_features[, target_dutch := target_names_dutch[target_var]]
all_features <- merge(
  all_features,
  performance_summary[, .(target, rmse_val, r2_val)],
  by.x = "target_var", by.y = "target", all.x = TRUE
)
all_features[, rmse_unit := rmse_units[target_var]]
all_features[, plot_title := paste0(
  target_dutch, "\nR²(val): ", round(r2_val * 100, 1),
  "% | RMSE(val): ", round(rmse_val, 3), " ", rmse_unit
)]

# Sorteer op gemiddeld belang over beide methoden
all_features[, mean_belang := (delta_rmse_norm + gain_norm) / 2]

plot_vip_compare <- melt(
  all_features[, .(Feature, Nederlandse_naam, plot_title, gain_norm, delta_rmse_norm)],
  id.vars       = c("Feature", "Nederlandse_naam", "plot_title"),
  variable.name = "methode",
  value.name    = "belang_norm"
)[, methode := fifelse(methode == "gain_norm", "XGBoost Gain", "Permutation (ΔRMSE)")]

# Sorteer features binnen elk facet op gemiddeld belang
feat_order <- all_features[order(plot_title, mean_belang),
                            paste0(plot_title, "__", Nederlandse_naam)]
plot_vip_compare[, feat_label := factor(
  paste0(plot_title, "__", Nederlandse_naam),
  levels = unique(feat_order)
)]

p_vip_compare <- ggplot(plot_vip_compare, aes(
    x    = feat_label,
    y    = belang_norm,
    fill = methode
  )) +
  geom_col(position = "dodge") +
  facet_wrap(~ plot_title, scales = "free_y", ncol = 3) +
  scale_x_discrete(labels = function(x) sub(".*__", "", x)) +
  coord_flip() +
  scale_fill_manual(
    values = c("XGBoost Gain" = "#0072B2", "Permutation (ΔRMSE)" = "#E69F00"),
    name   = "Methode"
  ) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    title    = "VIP vergelijking: XGBoost Gain vs. Permutation Importance",
    subtitle = "Genormaliseerd binnen target (1 = hoogste belang). Permutation op validatieset. 0% = niet in top-10 van die methode.",
    x        = NULL,
    y        = "Relatief belang (genormaliseerd)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text         = element_text(size = 8, face = "bold", lineheight = 1.1),
    strip.background   = element_rect(fill = "grey95", colour = "grey70", linewidth = 0.6),
    panel.border       = element_rect(colour = "grey80", fill = NA, linewidth = 0.5),
    panel.grid.major.y = element_blank(),
    axis.text.y        = element_text(size = 8),
    legend.position    = "bottom",
    plot.title         = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle      = element_text(size = 9.5, hjust = 0.5, color = "grey40")
  )

print(p_vip_compare)
ggsave(
  "output/AlleGebieden/Tussenrapportage/VIP_gain_vs_permutation.png",
  plot = p_vip_compare,
  width = 35, height = 30, units = "cm", dpi = 300
)

## Manual ALE calculation function (without ALEPlot package) ------------------
calculate_ale_manual <- function(model, X_data, feature_idx, K = 50) {
  feature_values <- suppressWarnings(as.numeric(X_data[, feature_idx]))
  feature_name <- colnames(X_data)[feature_idx]

  ok <- is.finite(feature_values)
  feature_values_ok <- feature_values[ok]

  if (length(feature_values_ok) < 2L || length(unique(feature_values_ok)) < 2L) {
    return(list(x_values = numeric(0), ale_effects = numeric(0), feature_name = feature_name))
  }

  quantiles <- unique(as.numeric(
    quantile(feature_values_ok, probs = seq(0, 1, length.out = K + 1), na.rm = TRUE, names = FALSE)
  ))

  if (length(quantiles) < 2L) {
    return(list(x_values = numeric(0), ale_effects = numeric(0), feature_name = feature_name))
  }

  ale_values <- rep(NA_real_, length(quantiles) - 1L)
  x_values <- rep(NA_real_, length(quantiles) - 1L)

  for (i in seq_len(length(quantiles) - 1L)) {
    in_interval <- ok & feature_values >= quantiles[i] & feature_values <= quantiles[i + 1]
    n_int <- sum(in_interval, na.rm = TRUE)

    if (n_int > 0L) {
      X_low <- X_data[in_interval, , drop = FALSE]
      X_high <- X_data[in_interval, , drop = FALSE]

      X_low[, feature_idx] <- quantiles[i]
      X_high[, feature_idx] <- quantiles[i + 1]

      pred_low <- predict(model, X_low)
      pred_high <- predict(model, X_high)

      ale_values[i] <- mean(pred_high - pred_low, na.rm = TRUE)
      x_values[i] <- (quantiles[i] + quantiles[i + 1]) / 2
    }
  }

  valid <- is.finite(ale_values) & is.finite(x_values)
  if (!any(valid)) {
    return(list(x_values = numeric(0), ale_effects = numeric(0), feature_name = feature_name))
  }

  ale_cumulative <- cumsum(ale_values[valid])
  ale_centered <- ale_cumulative - mean(ale_cumulative, na.rm = TRUE)

  list(
    x_values = x_values[valid],
    ale_effects = ale_centered,
    feature_name = feature_name
  )
}
# Function to create ALE plots without ALEPlot package
# r
rescale_to <- function(x, to_min, to_max) {
  xr <- range(x, na.rm = TRUE)
  if (!all(is.finite(xr)) || diff(xr) == 0) {
    return(rep((to_min + to_max) / 2, length(x)))
  }
  (x - xr[1]) / diff(xr) * (to_max - to_min) + to_min
}
create_ale_plots_manual <- function(model, X_data, y_data, target_name) {
  ale_plots <- list()
  available_features <- colnames(X_data)

  for (feature in available_features) {
    feature_idx <- which(colnames(X_data) == feature)

    ale_result <- calculate_ale_manual(model, X_data, feature_idx, K = 30)
    if (length(ale_result$x_values) == 0L) next
    ale_df <- data.frame(
      x = ale_result$x_values,
      ale_effect = ale_result$ale_effects
    )

    real_df <- data.frame(
      x = X_data[, feature_idx],
      y = y_data
    )
    real_df <- real_df[is.finite(real_df$x) & is.finite(real_df$y), ]

    # Schaal echte y-waarden naar ALE-range voor achtergrondweergave
    ale_rng <- range(ale_df$ale_effect, na.rm = TRUE)
    y_rng   <- range(real_df$y, na.rm = TRUE)
    real_df$y_bg <- rescale_to(real_df$y, ale_rng[1], ale_rng[2])

    # Inverse transformatie voor tweede y-as: ALE-eenheid -> echte y-eenheid
    # y_real = y_rng[1] + (y_bg - ale_rng[1]) / diff(ale_rng) * diff(y_rng)
    sec_trans <- if (diff(ale_rng) > 0) {
      scale_fac <- diff(y_rng) / diff(ale_rng)
      list(
        trans  = ~ y_rng[1] + (. - ale_rng[1]) * scale_fac,
        inv    = ~ ale_rng[1] + (. - y_rng[1]) / scale_fac
      )
    } else NULL

    feature_name_dutch <- nederlandse_namen[feature]
    if (is.na(feature_name_dutch)) feature_name_dutch <- feature

    target_name_dutch <- target_names_dutch[target_name]
    if (is.na(target_name_dutch)) target_name_dutch <- target_name

    effect_range <- max(ale_df$ale_effect, na.rm = TRUE) - min(ale_df$ale_effect, na.rm = TRUE)

    p <- ggplot() +
      geom_point(
        data = real_df,
        aes(x = x, y = y_bg),
        color = "grey70",
        alpha = 0.35,
        size = 1.2
      ) +
      geom_line(
        data = ale_df,
        aes(x = x, y = ale_effect),
        color = "#1f77b4",
        linewidth = 1
      ) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
      labs(
        title    = feature_name_dutch,
        subtitle = paste0("Effect range: ", round(effect_range, 3)),
        x        = feature_name_dutch,
        y        = paste("ALE effect op", target_name_dutch)
      ) +
      theme_minimal()

    # Voeg tweede y-as toe met meetwaarden van de target
    if (!is.null(sec_trans)) {
      p <- p + scale_y_continuous(
        sec.axis = sec_axis(
          trans  = sec_trans$trans,
          name   = target_name_dutch,
          labels = scales::label_number(accuracy = 0.1)
        )
      ) +
        theme(
          axis.title.y.right = element_text(color = "grey50", size = 8),
          axis.text.y.right  = element_text(color = "grey50", size = 7)
        )
    }

    ale_plots[[feature]] <- p
  }

  ale_plots
}
# Create ALE plots for all models
all_ale_plots <- list()
# r
for (target in names(xgb_models)) {
  cat("Creating ALE plots for:", target, "\n")
  
  # target MOET in model_data blijven
  model_vars <- c("SlootID", target, cols_corr)
  model_vars <- model_vars[model_vars %in% colnames(abio_proj)]
  model_data <- abio_proj[complete.cases(abio_proj[, ..model_vars]), ..model_vars]
  
  factor_cols <- names(model_data)[sapply(model_data, is.character)]
  factor_cols_2 <- names(model_data)[sapply(model_data, is.factor)]
  factor_cols <- c(factor_cols, factor_cols_2)
  model_data[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]
  model_data[, (factor_cols) := lapply(.SD, as.numeric), .SDcols = factor_cols]
  
  predictors_clean <- colnames(model_data)[!colnames(model_data) %in% c("SlootID", target)]
  X_data <- as.matrix(model_data[, ..predictors_clean])
  y_data <- model_data[[target]]

  if (length(y_data) == 0L || nrow(X_data) != length(y_data)) next
  
  ale_plots <- create_ale_plots_manual(xgb_models[[target]], X_data, y_data, target)
  all_ale_plots[[target]] <- ale_plots
}
# Display and save ALE plots for each target
for (tgt in names(all_ale_plots)) {
  
  target_dutch <- target_names_dutch[tgt]
  perf <- performance_summary[target == tgt]
  
  cat("\n=== ALE Plots for", target_dutch, "===\n")
  cat("RMSE:", round(perf$rmse_test, 3), "| R²:", round(perf$r2_test * 100, 1), "%\n")
  
  if (length(all_ale_plots[[tgt]]) > 0) {
    for (plot_name in names(all_ale_plots[[tgt]])) {
      cat("Showing ALE plot for:", plot_name, "\n")
      all_ale_plots[[tgt]][[plot_name]]
    }
  }
}

## ALE plots combinatie: sturende variabelen op alle targets ------------------
# Functie om gecombineerde plots te maken met echte data + ALE effect
create_combined_ale_plots <- function() {
  ale_variables <- cols_corr
  combined_plots <- list()

  for (var in ale_variables) {
    if (!var %in% colnames(abio_proj)) next

    plot_data_list <- list()
    ale_data_list <- list()

    for (tgt in names(xgb_models)) {
      if (!tgt %in% colnames(abio_proj)) next

      real_data <- abio_proj[
        !is.na(get(var)) & !is.na(get(tgt)),
        .(x = get(var), y = get(tgt), target = tgt)
      ]

      if (nrow(real_data) <= 10) next
      plot_data_list[[tgt]] <- real_data

      model_vars <- c("SlootID", tgt, cols_corr)
      model_vars <- model_vars[!model_vars %in% tgt & model_vars %in% colnames(abio_proj)]
      model_data <- abio_proj[complete.cases(abio_proj[, ..model_vars]), ..model_vars]

      factor_cols <- names(model_data)[sapply(model_data, is.character) | sapply(model_data, is.factor)]
      if (length(factor_cols) > 0) {
        model_data[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]
        model_data[, (factor_cols) := lapply(.SD, as.numeric), .SDcols = factor_cols]
      }

      predictors_clean <- colnames(model_data)[!colnames(model_data) %in% c("SlootID", tgt)]
      X_data <- as.matrix(model_data[, ..predictors_clean])
      if (!var %in% colnames(X_data)) next

      feature_idx <- which(colnames(X_data) == var)
      ale_result <- calculate_ale_manual(xgb_models[[tgt]], X_data, feature_idx, K = 30)

      ale_data_list[[tgt]] <- data.table(
        x = ale_result$x_values,
        ale_effect = ale_result$ale_effects,
        target = tgt
      )
    }

    if (length(plot_data_list) == 0 || length(ale_data_list) == 0) next

    all_real_data <- rbindlist(plot_data_list, fill = TRUE)
    all_ale_data  <- rbindlist(ale_data_list, fill = TRUE)

    all_real_data[, target_dutch := target_names_dutch[target]]
    all_ale_data[, target_dutch := target_names_dutch[target]]

    # stats per target voor schaling
    y_stats <- all_real_data[, .(
      y_min = min(y, na.rm = TRUE),
      y_max = max(y, na.rm = TRUE),
      y_median = median(y, na.rm = TRUE)
    ), by = target]

    ale_stats <- all_ale_data[, .(
      ale_min = min(ale_effect, na.rm = TRUE),
      ale_max = max(ale_effect, na.rm = TRUE)
    ), by = target]

    all_ale_data <- merge(all_ale_data, y_stats, by = "target", all.x = TRUE)
    all_ale_data <- merge(all_ale_data, ale_stats, by = "target", all.x = TRUE)

    all_ale_data[, scale_factor := fifelse(
      (ale_max - ale_min) > 0,
      (y_max - y_min) * 0.30 / (ale_max - ale_min),
      1
    )]

    all_ale_data[, ale_scaled := y_median + ale_effect * scale_factor]

    var_name_dutch <- nederlandse_namen[var]
    if (is.na(var_name_dutch)) var_name_dutch <- var

    # Maak per target een aparte plot met tweede y-as (ALE-eenheden)
    # zodat sec_axis de correcte inverse transformatie per facet kan tonen
    targets_in_data <- unique(all_ale_data$target)

    sub_plots <- lapply(targets_in_data, function(tgt) {

      ale_tgt  <- all_ale_data[target == tgt]
      real_tgt <- all_real_data[target == tgt]

      sf       <- ale_tgt$scale_factor[1]
      y_med    <- ale_tgt$y_median[1]
      tgt_nl   <- ale_tgt$target_dutch[1]

      ale_min_val <- min(ale_tgt$ale_effect, na.rm = TRUE)
      ale_max_val <- max(ale_tgt$ale_effect, na.rm = TRUE)

      x_min_pos <- ale_tgt$x[which.min(ale_tgt$ale_effect)]
      x_max_pos <- ale_tgt$x[which.max(ale_tgt$ale_effect)]
      y_min_pos <- ale_tgt$ale_scaled[which.min(ale_tgt$ale_effect)]
      y_max_pos <- ale_tgt$ale_scaled[which.max(ale_tgt$ale_effect)]

      ggplot() +
        geom_point(
          data = real_tgt,
          aes(x = x, y = y),
          color = "grey70", alpha = 0.35, size = 1.2
        ) +
        geom_hline(
          yintercept = y_med,
          linetype = "dashed", color = "red", alpha = 0.7, linewidth = 0.5
        ) +
        geom_line(
          data = ale_tgt,
          aes(x = x, y = ale_scaled),
          color = "black", linewidth = 1.1
        ) +
        geom_text(
          x = x_min_pos, y = y_min_pos,
          label = paste("Min:", round(ale_min_val, 3)),
          color = "blue", size = 2.8, fontface = "bold", vjust = -0.4
        ) +
        geom_text(
          x = x_max_pos, y = y_max_pos,
          label = paste("Max:", round(ale_max_val, 3)),
          color = "darkgreen", size = 2.8, fontface = "bold", vjust = 1.3
        ) +
        scale_y_continuous(
          name = tgt_nl,
          sec.axis = sec_axis(
            trans  = ~ (. - y_med) / sf,
            name   = "ALE effect",
            labels = scales::label_number(accuracy = 0.01)
          )
        ) +
        labs(
          title = tgt_nl,
          x     = NULL
        ) +
        theme_minimal(base_size = 10) +
        theme(
          plot.title         = element_text(size = 9, face = "bold", hjust = 0.5),
          axis.title.y.left  = element_text(size = 8, color = "grey30"),
          axis.title.y.right = element_text(size = 8, color = "#1f77b4", face = "italic"),
          axis.text.y.right  = element_text(size = 7, color = "#1f77b4"),
          panel.border       = element_rect(colour = "grey80", fill = NA, linewidth = 0.4),
          plot.margin        = margin(4, 6, 4, 6)
        )
    })

    n_tgts  <- length(sub_plots)
    n_cols  <- 3L
    n_rows  <- ceiling(n_tgts / n_cols)

    p <- wrap_plots(sub_plots, ncol = n_cols) +
      plot_annotation(
        title    = paste("Effect van", var_name_dutch, "op alle doelvariabelen"),
        subtitle = paste0(
          "Grijze punten = waarnemingen  |  Zwarte lijn = ALE (geschaald naar linkeras)  |",
          "  Rode lijn = mediaan (ALE=0)  |  Rechteras = ALE effect"
        ),
        theme = theme(
          plot.title    = element_text(size = 13, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey40")
        )
      ) &
      labs(x = var_name_dutch)

    combined_plots[[var]] <- p
  }

  combined_plots
}
cat("Creating combined ALE + data plots...\n")
combined_ale_plots <- create_combined_ale_plots()
# Display en save de plots
for(var in names(combined_ale_plots)) {
  var_name_dutch <- nederlandse_namen[var]
  if(is.na(var_name_dutch)) var_name_dutch <- var
  
  cat("\n=== Combined Plot:", var_name_dutch, "===\n")
  print(combined_ale_plots[[var]])
  
  # Save plot
  ggsave(
    filename = paste0('output/AlleGebieden/Tussenrapportage/Combined_ALE_', gsub("[^A-Za-z0-9]", "_", var), '.png'),
    plot = combined_ale_plots[[var]],
    width = 40, height = 25, units = 'cm', dpi = 300
  )
}
cat("\nAlle gecombineerde ALE plots zijn voltooid en opgeslagen!\n")

### ALE combi-plot per target: top predictoren per doelvariabele ----
# ALE combi-plot per target: top predictoren per doelvariabele in één figuur
library(patchwork)
top_n <- 5

for (tgt in names(all_ale_plots)) {
  
  top_feats <- all_importance[target_var == tgt][order(-Gain)][1:min(.N, top_n), Feature]
  top_feats <- top_feats[top_feats %in% names(all_ale_plots[[tgt]])]
  if (length(top_feats) == 0) next
  
  plots <- lapply(top_feats, function(feat) {
    dutch_name <- all_importance[target_var == tgt & Feature == feat, Nederlandse_naam]
    gain_val   <- all_importance[target_var == tgt & Feature == feat, round(Gain, 3)]
    p <- all_ale_plots[[tgt]][[feat]]
    p + labs(title = paste0(dutch_name, "
(Gain: ", gain_val, ")"), subtitle = NULL, x = NULL, y = "ALE effect") +
      theme_minimal(base_size = 9) +
      theme(
        plot.title   = element_text(size = 8, face = "bold", hjust = 0.5),
        axis.text    = element_text(size = 7),
        axis.title.y = element_text(size = 7.5),
        panel.border = element_rect(colour = "grey80", fill = NA, linewidth = 0.4),
        plot.margin  = margin(4, 8, 4, 8)
      )
  })
  
  title_str <- all_importance[target_var == tgt, target_dutch[1]]
  perf_str  <- all_importance[target_var == tgt, paste0("R²: ", round(r2_test[1]*100,1), "%  |  RMSE: ", round(rmse_test[1],3), " ", rmse_unit[1])]
  
  panel <- wrap_plots(plots, nrow = 1) +
    plot_annotation(
      title    = paste0("ALE plots — ", title_str),
      subtitle = perf_str,
      theme = theme(
        plot.title    = element_text(size = 11, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 9, color = "grey40", hjust = 0.5)
      )
    )
  
  outfile <- paste0("output/AlleGebieden/Tussenrapportage/ALE_top5_", tgt, ".png")
  print(panel)
  ggsave(outfile, panel, width = 35, height = 12, units = "cm", dpi = 200)
  cat("Opgeslagen:", outfile, "\n")
}

## Kantelpunten detectie in ALE-curves ----------------------------------------

detect_tipping_points <- function(x, y, min_effect_range = 0.01, min_jump_pct = 0.15) {
  effect_range <- diff(range(y, na.rm = TRUE))

  # Sla hele curve over als totaal bereik te klein is
  if (effect_range < min_effect_range || length(x) < 4) return(NULL)

  dy <- diff(y)
  dx <- diff(x)

  # 1. Lokale extrema via tekenwissel in dy
  sign_ch <- which(diff(sign(dy)) != 0) + 1

  # Filter: alleen bewaren als de sprong rondom het punt >= min_jump_pct * effect_range
  min_jump_abs <- min_jump_pct * effect_range
  sign_ch_filtered <- sign_ch[vapply(sign_ch, function(i) {
    left  <- if (i > 1)           abs(dy[i - 1]) else 0
    right <- if (i <= length(dy)) abs(dy[i])     else 0
    max(left, right) >= min_jump_abs
  }, logical(1))]

  # 2. Steilste helling
  slopes       <- dy / dx
  steepest_idx <- which.max(abs(slopes)) + 1

  local_extrema <- if (length(sign_ch_filtered)) {
    data.frame(x = x[sign_ch_filtered], y = y[sign_ch_filtered], type = "Lokaal extremum")
  } else NULL

  list(
    local_extrema = local_extrema,
    steepest      = data.frame(x = x[steepest_idx], y = y[steepest_idx], type = "Steilste helling")
  )
}

## Loop over alle targets: ALE plots met kantelpunten -------------------------

tipping_records <- list()

for (tgt in names(all_ale_plots)) {

  top_feats <- all_importance[target_var == tgt][order(-Gain)][1:min(.N, 5), Feature]
  top_feats <- top_feats[top_feats %in% names(all_ale_plots[[tgt]])]
  if (length(top_feats) == 0) next

  plots_tgt <- lapply(top_feats, function(feat) {

    # Haal ALE x/y op uit de bestaande ggplot-objecten (layer 2 = geom_line)
    ale_df <- layer_data(all_ale_plots[[tgt]][[feat]], 2)[, c("x", "y")]
    names(ale_df) <- c("x", "ale_effect")

    tp <- detect_tipping_points(ale_df$x, ale_df$ale_effect)

    # Sla op voor overzichtstabel
    if (!is.null(tp)) {
      tp_all <- rbind(
        if (!is.null(tp$local_extrema)) tp$local_extrema else NULL,
        tp$steepest
      )
      if (!is.null(tp_all) && nrow(tp_all) > 0) {
        tipping_records[[length(tipping_records) + 1]] <<- data.table(
          target     = tgt,
          target_nl  = target_names_dutch[tgt],
          predictor  = feat,
          pred_nl    = nederlandse_namen[feat],
          type       = tp_all$type,
          x_waarde   = round(tp_all$x, 4),
          ale_effect = round(tp_all$y, 4)
        )
      }
    }

    dutch_name <- all_importance[target_var == tgt & Feature == feat, Nederlandse_naam][1]
    gain_val   <- all_importance[target_var == tgt & Feature == feat, round(Gain, 3)][1]

    # Basisplot overnemen
    p <- all_ale_plots[[tgt]][[feat]] +
      labs(
        title    = paste0(dutch_name, "\n(Gain: ", gain_val, ")"),
        subtitle = NULL,
        x        = NULL,
        y        = "ALE effect"
      ) +
      theme_minimal(base_size = 9) +
      theme(
        plot.title   = element_text(size = 8, face = "bold", hjust = 0.5),
        axis.text    = element_text(size = 7),
        axis.title.y = element_text(size = 7.5),
        panel.border = element_rect(colour = "grey80", fill = NA, linewidth = 0.4),
        plot.margin  = margin(4, 8, 4, 8)
      )

    # Voeg annotaties toe als detect_tipping_points iets retourneert
    if (!is.null(tp)) {

      # Verticale lijn alleen voor steilste helling
      p <- p +
        geom_vline(
          xintercept = tp$steepest$x,
          linetype   = "dashed",
          color      = "#CC79A7",
          linewidth  = 0.7,
          alpha      = 0.9
        ) +
        annotate(
          "label",
          x      = tp$steepest$x,
          y      = Inf,
          label  = round(tp$steepest$x, 2),
          vjust  = 1.3,
          size   = 2.5,
          fontface = "bold",
          color  = "#CC79A7",
          fill   = "white",
          label.padding = unit(0.15, "lines")
        )

      # Rode stippen op de ALE-lijn voor lokale extrema
      if (!is.null(tp$local_extrema) && nrow(tp$local_extrema) > 0) {
        p <- p +
          geom_point(
            data  = tp$local_extrema,
            aes(x = x, y = y),
            color = "red",
            size  = 2.5,
            shape = 16
          ) +
          geom_label(
            data  = tp$local_extrema,
            aes(x = x, y = y, label = round(x, 2)),
            vjust = -0.6,
            size  = 2.2,
            color = "red",
            fill  = "white",
            label.padding = unit(0.12, "lines")
          )
      }
    }

    p
  })

  title_str <- all_importance[target_var == tgt, target_dutch[1]]
  perf_str  <- all_importance[target_var == tgt,
    paste0("R²: ", round(r2_test[1] * 100, 1), "%  |  RMSE: ",
           round(rmse_test[1], 3), " ", rmse_unit[1])]

  panel <- wrap_plots(plots_tgt, nrow = 1) +
    plot_annotation(
      title    = paste0("ALE + Kantelpunten — ", title_str),
      subtitle = perf_str,
      theme    = theme(
        plot.title    = element_text(size = 11, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 9, color = "grey40", hjust = 0.5)
      )
    )

  outfile <- paste0("output/AlleGebieden/Tussenrapportage/ALE_tipping_", tgt, ".png")
  print(panel)
  ggsave(outfile, panel, width = 35, height = 12, units = "cm", dpi = 200)
  cat("Opgeslagen:", outfile, "\n")
}

## Overzichtstabel alle kantelpunten ------------------------------------------
tipping_points_summary <- rbindlist(tipping_records, fill = TRUE)
print(tipping_points_summary)
write.csv(
  tipping_points_summary,
  file = "output/AlleGebieden/Tussenrapportage/ALE_tipping_points_summary.csv",
  row.names = FALSE
)

## Functie voor XGBoost model diagnostiek en validatie ------------------------
# Functie voor XGBoost model diagnostiek
create_xgb_diagnostics <- function(xgb_models, abio_proj, target_vars, target_names_dutch, nederlandse_namen) {
  
  library(ggplot2)
  library(data.table)
  library(patchwork)
  
  diagnostic_plots <- list()
  
  for(target in target_vars) {
    if(target %in% colnames(abio_proj) && target %in% names(xgb_models)) {
      
      cat("Creating diagnostics for:", target, "\n")
      
      # Verzamel model data
      model_vars <- c("SlootID", target, cols_corr)
      model_vars <- model_vars[model_vars %in% colnames(abio_proj)]
      model_data <- abio_proj[complete.cases(abio_proj[, ..model_vars]), ..model_vars]
      
      # Convert factors
      factor_cols <- names(model_data)[sapply(model_data, is.character)]
      factor_cols_2 <- names(model_data)[sapply(model_data, is.factor)]
      factor_cols <- c(factor_cols, factor_cols_2)
      if(length(factor_cols) > 0) {
        model_data[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]
        model_data[, (factor_cols) := lapply(.SD, as.numeric), .SDcols = factor_cols]
      }
      
      # Prepare data FIRST
      predictors_clean <- colnames(model_data)[!colnames(model_data) %in% c("SlootID", target)]
      X_data <- as.matrix(model_data[, ..predictors_clean])
      y_actual <- model_data[[target]]
      
      # CHECK if numeric AFTER preparing data
      if(!is.numeric(y_actual)) {
        cat("Skipping", target, "- target is not numeric\n")
        next
      }
      
      # Predictions
      y_pred <- predict(xgb_models[[target]], X_data)
      
      # Calculate residuals
      residuals <- y_actual - y_pred
      
      # Create diagnostics data
      diag_data <- data.table(
        actual = y_actual,
        predicted = y_pred,
        residuals = residuals,
        standardized_residuals = residuals / sd(residuals, na.rm = TRUE)
      )
      
      # Calculate statistics
      rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
      mae <- mean(abs(residuals), na.rm = TRUE)
      r2 <- cor(y_actual, y_pred, use = "complete.obs")^2
      skewness <- (mean(residuals^3, na.rm = TRUE)) / (sd(residuals, na.rm = TRUE)^3)
      
      target_dutch <- target_names_dutch[target]
      
      # 1. Scatter plot
      p1 <- ggplot(diag_data, aes(x = actual, y = predicted)) +
        geom_point(alpha = 0.6, size = 2.5, color = "#0072B2") +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 1) +
        labs(
          title = paste0("Gemeten vs Voorspeld: ", target_dutch),
          subtitle = paste0("RMSE: ", round(rmse, 2), " | MAE: ", round(mae, 2), " | R²: ", round(r2, 3)),
          x = paste("Gemeten", target_dutch),
          y = paste("Voorspeld", target_dutch)
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 12, face = "bold"),
          plot.subtitle = element_text(size = 10),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8)
        )
      
      # 2. Residuals vs Fitted
      p2 <- ggplot(diag_data, aes(x = predicted, y = residuals)) +
        geom_point(alpha = 0.6, size = 2.5, color = "#009E73") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
        geom_hline(yintercept = c(-2*sd(residuals, na.rm = TRUE), 2*sd(residuals, na.rm = TRUE)), 
                   linetype = "dotted", color = "orange", linewidth = 0.8) +
        labs(
          title = "Residuals vs Fitted Values",
          x = "Voorspelde waarden",
          y = "Residuals"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 12, face = "bold"),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8)
        )
      
      # 3. Histogram
      p3 <- ggplot(diag_data, aes(x = residuals)) +
        geom_histogram(aes(y = after_stat(density)), bins = 15, 
                       fill = "#E69F00", alpha = 0.7, color = "black", linewidth = 0.5) +
        stat_function(fun = dnorm, 
                      args = list(mean = mean(diag_data$residuals, na.rm = TRUE), 
                                 sd = sd(diag_data$residuals, na.rm = TRUE)),
                      color = "red", linewidth = 1.2, linetype = "dashed") +
        labs(
          title = "Verdeling van Residuals",
          subtitle = paste0("Skewness: ", round(skewness, 3)),
          x = "Residuals",
          y = "Density"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 12, face = "bold"),
          plot.subtitle = element_text(size = 10),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8)
        )
      
      # 4. Q-Q plot
      p4 <- ggplot(diag_data, aes(sample = standardized_residuals)) +
        stat_qq(color = "#56B4E9", size = 2.5, alpha = 0.7) +
        stat_qq_line(color = "red", linewidth = 1, linetype = "dashed") +
        labs(
          title = "Q-Q Plot (Normaalverdeling Check)",
          x = "Theoretische Quantiles",
          y = "Sample Quantiles"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 12, face = "bold"),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8)
        )
      
      # 5. Density plot
      p5 <- ggplot(diag_data, aes(x = residuals)) +
        geom_density(fill = "#D55E00", alpha = 0.6, color = "black", linewidth = 1) +
        stat_function(fun = dnorm,
                      args = list(mean = mean(diag_data$residuals, na.rm = TRUE),
                                 sd = sd(diag_data$residuals, na.rm = TRUE)),
                      color = "blue", linewidth = 1.2, linetype = "dashed") +
        labs(
          title = "Dichtheidsverdeling Residuals",
          x = "Residuals",
          y = "Density"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 12, face = "bold"),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8)
        )
      
      # Combine plots
      diagnostic_plots[[target]] <- list(
        scatter = p1,
        residuals_fitted = p2,
        histogram = p3,
        qq_plot = p4,
        density = p5,
        statistics = data.table(
          Target = target_dutch,
          RMSE = rmse,
          MAE = mae,
          R2 = r2,
          Skewness = skewness,
          N = nrow(diag_data)
        )
      )
    }
  }
  
  return(diagnostic_plots)
}

# Maak diagnostics voor alle targets
xgb_diagnostics <- create_xgb_diagnostics(xgb_models, abio_proj, target_vars, 
                                          target_names_dutch, nederlandse_namen)

# Display en save de diagnostische plots
for(target in names(xgb_diagnostics)) {
  
  cat("\n=== Diagnostics voor", target_names_dutch[target], "===\n")
  
  # Print statistics
  print(xgb_diagnostics[[target]]$statistics)
  
  # Maak combined plot met patchwork
  combined_diag <- (xgb_diagnostics[[target]]$scatter + xgb_diagnostics[[target]]$residuals_fitted) /
                   (xgb_diagnostics[[target]]$histogram + xgb_diagnostics[[target]]$qq_plot) /
                   (xgb_diagnostics[[target]]$density)
  
  print(combined_diag)
  
  # Save
  ggsave(
    filename = paste0('output/AlleGebieden/Tussenrapportage/XGBoost_diagnostics_', 
                     gsub("[^A-Za-z0-9]", "_", target), '.png'),
    plot = combined_diag,
    width = 40, height = 45, units = 'cm', dpi = 300
  )
}

# Samenvatting statistieken
diagnostics_summary <- rbindlist(lapply(xgb_diagnostics, function(x) x$statistics))
print("=== Samenvattende Diagnostische Statistieken ===")
print(diagnostics_summary)




## 9-fold waterschap cross-validatie ----------------------------------------
# Elke fold laat één waterschap weg als testset.
# Test of het model generaliseert naar een nieuw gebied.

waterschappen <- abio_proj[!is.na(waterschap), unique(waterschap)]

cv_records <- list()

for (target in target_vars) {
  if (!target %in% colnames(abio_proj)) next
  cat("CV voor:", target, "\n")

  predictors <- cols_corr[!cols_corr %in% target & cols_corr %in% colnames(abio_proj)]

  for (ws in waterschappen) {

    test_idx  <- which(abio_proj$waterschap == ws)
    train_idx <- which(abio_proj$waterschap != ws & !is.na(abio_proj$waterschap))

    # Minimale trainset vereist
    if (length(test_idx) < 3 || length(train_idx) < 20) next

    model_vars <- c("SlootID", target, predictors)
    model_vars <- model_vars[model_vars %in% colnames(abio_proj)]

    # complete.cases bepalen op model_vars; waterschap apart ophouden voor fold-toewijzing
    complete_mask <- complete.cases(abio_proj[, ..model_vars])
    model_data    <- abio_proj[complete_mask, ..model_vars]
    ws_complete   <- abio_proj[complete_mask, waterschap]

    test_rows  <- which(ws_complete == ws)
    train_rows <- which(ws_complete != ws & !is.na(ws_complete))

    if (length(test_rows) < 3 || length(train_rows) < 20) next

    factor_cols <- unique(c(
      names(model_data)[sapply(model_data, is.character)],
      names(model_data)[sapply(model_data, is.factor)]
    ))
    if (length(factor_cols) > 0) {
      model_data[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]
      model_data[, (factor_cols) := lapply(.SD, as.numeric), .SDcols = factor_cols]
    }

    predictors_clean <- colnames(model_data)[!colnames(model_data) %in% c("SlootID", target)]

    X_train <- matrix(as.numeric(as.matrix(model_data[train_rows, ..predictors_clean])),
                      nrow = length(train_rows))
    colnames(X_train) <- predictors_clean
    y_train <- model_data[train_rows, get(target)]

    X_test <- matrix(as.numeric(as.matrix(model_data[test_rows, ..predictors_clean])),
                     nrow = length(test_rows))
    colnames(X_test) <- predictors_clean
    y_test <- model_data[test_rows, get(target)]

    # Interne val-split binnen trainset voor early stopping (20%)
    set.seed(123)
    val_rows_inner <- sample(seq_along(train_rows), max(3, floor(0.2 * length(train_rows))))
    train_rows_inner <- setdiff(seq_along(train_rows), val_rows_inner)

    dtrain <- xgb.DMatrix(data = X_train[train_rows_inner, ], label = y_train[train_rows_inner])
    dval   <- xgb.DMatrix(data = X_train[val_rows_inner, ],   label = y_train[val_rows_inner])

    params <- list(
      objective        = "reg:squarederror",
      eval_metric      = "rmse",
      max_depth        = 6,
      eta              = 0.1,
      subsample        = 0.8,
      colsample_bytree = 0.8
    )

    cv_model <- tryCatch(
      xgb.train(
        params                = params,
        data                  = dtrain,
        nrounds               = 100,
        watchlist             = list(train = dtrain, val = dval),
        early_stopping_rounds = 10,
        verbose               = 0
      ),
      error = function(e) NULL
    )

    if (is.null(cv_model)) next

    pred_test <- predict(cv_model, X_test)
    residuals <- y_test - pred_test
    rmse_ws   <- sqrt(mean(residuals^2))
    r2_ws     <- tryCatch(cor(y_test, pred_test)^2, error = function(e) NA_real_)
    mae_ws    <- mean(abs(residuals))

    cv_records[[length(cv_records) + 1]] <- data.table(
      target      = target,
      target_nl   = target_names_dutch[target],
      waterschap  = ws,
      n_test      = length(y_test),
      n_train     = length(train_rows),
      rmse        = rmse_ws,
      r2          = r2_ws,
      mae         = mae_ws,
      mean_actual = mean(y_test, na.rm = TRUE),
      rmse_pct    = 100 * rmse_ws / mean(abs(y_test), na.rm = TRUE)
    )
  }
}

cv_results <- rbindlist(cv_records, fill = TRUE)

### CV resultaten samenvatten per waterschap -----------------------------------

# Gemiddelde RMSE% per waterschap over alle targets
cv_ws_summary <- cv_results[, .(
  mean_rmse_pct = mean(rmse_pct,  na.rm = TRUE),
  mean_r2       = mean(r2,        na.rm = TRUE),
  n_targets     = .N
), by = waterschap][order(-mean_rmse_pct)]

cat("\n=== CV prestatie per waterschap (gemiddeld over targets) ===\n")
print(cv_ws_summary)

### Plot: R² per waterschap per target ----------------------------------------
# Waterschappen met < 10 testlocaties worden weggelaten: te klein voor
# betrouwbare R²-schatting (toevallige uitschieters domineren anders het beeld).
min_n_test_cv <- 10

cv_plot_dt <- cv_results[!is.na(r2) & n_test >= min_n_test_cv]
cv_plot_dt[, target_nl := target_names_dutch[target]]

ws_order <- cv_plot_dt[, .(mean_r2 = mean(r2, na.rm = TRUE)), by = waterschap][
  order(mean_r2), waterschap]
cv_plot_dt[, waterschap := factor(waterschap, levels = ws_order)]

r2_clip_lo_cv <- -2
r2_clip_hi_cv <-  1

cv_plot_clipped <- copy(cv_plot_dt)
cv_plot_clipped[, r2_clipped := pmax(r2_clip_lo_cv, pmin(r2_clip_hi_cv, r2))]
cv_plot_clipped[, is_clipped := r2 < r2_clip_lo_cv | r2 > r2_clip_hi_cv]

p_cv_r2 <- ggplot(cv_plot_clipped, aes(x = waterschap, y = r2_clipped, fill = waterschap)) +
  geom_col() +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  geom_hline(
    data = cv_plot_clipped[, .(overall_r2 = mean(r2_clipped, na.rm = TRUE)), by = target_nl],
    aes(yintercept = overall_r2),
    linetype = "dashed", color = "grey40", linewidth = 0.5
  ) +
  geom_text(
    aes(label = n_test, y = r2_clip_lo_cv + 0.05),
    size = 2.3, color = "grey20", fontface = "bold", vjust = 0
  ) +
  geom_text(
    data = cv_plot_clipped[is_clipped == TRUE],
    aes(label = paste0("*", round(r2, 1)), y = r2_clip_lo_cv + 0.05),
    size = 2.4, color = "red", fontface = "bold", vjust = 0, hjust = 0.5
  ) +
  facet_wrap(~ target_nl, scales = "free_y", ncol = 3) +
  scale_y_continuous(
    labels = scales::label_percent(),
    limits = c(r2_clip_lo_cv, r2_clip_hi_cv),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  labs(
    title    = "9-fold waterschap CV (XGBoost, gecorrigeerd): R² per waterschap",
    subtitle = paste0(
      "Elk waterschap is één keer weggelaten uit de trainset. ",
      "Waterschappen met < ", min_n_test_cv, " testlocaties weggelaten (te klein voor betrouwbare R²).\n",
      "R² > 0: beter dan gemiddelde  |  R² < 0: slechter dan gemiddelde  |  ",
      "*Getal = werkelijke R² buiten bereik [", r2_clip_lo_cv, ", 1]. Getal in balk = n testlocaties."
    ),
    x = NULL,
    y = "R² (uit trainset weggelaten waterschap als testset)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x       = element_text(angle = 45, hjust = 1, size = 8),
    strip.text        = element_text(size = 8, face = "bold"),
    strip.background  = element_rect(fill = "grey95", colour = "grey70", linewidth = 0.6),
    panel.border      = element_rect(colour = "grey80", fill = NA, linewidth = 0.5),
    plot.title        = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 8, hjust = 0.5, color = "grey40")
  )

print(p_cv_r2)
ggsave(
  "output/AlleGebieden/Tussenrapportage/CV_waterschap_r2.png",
  plot = p_cv_r2,
  width = 35, height = 30, units = "cm", dpi = 300
)

### Plot: RMSE% per waterschap (genormaliseerd voor vergelijkbaarheid) ----------

# Clip grens op 200% — alles daarboven is een model dat volledig faalt
clip_pct <- 200
n_outliers <- cv_results[!is.na(rmse_pct) & rmse_pct > clip_pct, .N]
outlier_dt <- cv_results[!is.na(rmse_pct) & rmse_pct > clip_pct,
  .(waterschap, target_nl = target_names_dutch[target], rmse_pct = round(rmse_pct, 0), n_test)
][order(-rmse_pct)]

cat("\n=== Uitgesloten uitschieters (RMSE% >", clip_pct, ") ===\n")
print(outlier_dt)

ws_rmse_order <- cv_ws_summary[order(-mean_rmse_pct), waterschap]
cv_results[, waterschap_f := factor(waterschap, levels = ws_rmse_order)]

p_cv_rmse <- ggplot(cv_results[!is.na(rmse_pct) & rmse_pct <= clip_pct],
       aes(x = waterschap_f, y = rmse_pct, fill = waterschap_f)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1.5) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "red", alpha = 0.6) +
  annotate(
    "text", x = Inf, y = clip_pct, hjust = 1.1, vjust = -0.4,
    label = paste0(n_outliers, " combinaties boven ", clip_pct, "% weggelaten (model faalt volledig)"),
    size = 3, color = "grey40", fontface = "italic"
  ) +
  scale_fill_brewer(palette = "Set3", guide = "none") +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1),
    limits = c(0, clip_pct)
  ) +
  labs(
    title    = "CV prestatie per waterschap: RMSE als % van gemiddelde targetwaarde",
    subtitle = paste0(
      "Rode lijn = 100% (RMSE = gemiddelde targetwaarde). Lager = beter. ",
      "Y-as geclipped op ", clip_pct, "%."
    ),
    x = NULL,
    y = "RMSE (% van gemiddelde)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1),
    plot.title       = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle    = element_text(size = 9, hjust = 0.5, color = "grey40"),
    panel.border     = element_rect(colour = "grey80", fill = NA, linewidth = 0.5)
  )

print(p_cv_rmse)
ggsave(
  "output/AlleGebieden/Tussenrapportage/CV_waterschap_rmse.png",
  plot = p_cv_rmse,
  width = 25, height = 15, units = "cm", dpi = 300
)
## Belangrijkste predictoren over alle targets heen op basis van permutation importance-------------------------------------
top_preds <- all_perm_importance[
  , .SD[order(-delta_rmse)][1:5], by = target_var
][, .(Feature, Nederlandse_naam)
][, .N, by = .(Feature, Nederlandse_naam)][order(-N)][1:8, Feature]

cat("Top predictoren voor vergelijking:\n")

# Groepeer waterschappen: slecht vs goed voorspeld
ws_groep <- data.table(
  waterschap = c("WDOD", "HDL", "AGV", "HHNK", "Rijnland", "HDSR", "HHSK", "Fryslân", "WVV"),
  groep      = c("Slecht voorspeld", "Slecht voorspeld",
                 "Goed voorspeld", "Goed voorspeld", "Goed voorspeld",
                 "Gemiddeld", "Gemiddeld", "Gemiddeld", "Klein (n=4)")
)

# Vergelijkingsdata
compare_dt <- abio_proj[waterschap %in% ws_groep$waterschap]
compare_dt <- merge(compare_dt, ws_groep, by = "waterschap")

# Haal beschikbare top predictoren op
preds_avail <- top_preds[top_preds %in% names(compare_dt)]
cat("\nBeschikbare predictoren:", preds_avail, "\n")
# Zet om naar long formaat voor plotting
plot_vars <- preds_avail
nl_namen  <- nederlandse_namen[plot_vars]
nl_namen[is.na(nl_namen)] <- plot_vars[is.na(nl_namen)]

long_dt <- melt(
  compare_dt[, c("waterschap", "groep", plot_vars), with = FALSE],
  id.vars       = c("waterschap", "groep"),
  measure.vars  = plot_vars,
  variable.name = "predictor",
  value.name    = "waarde"
)
long_dt[, predictor_nl := nl_namen[as.character(predictor)]]
long_dt <- long_dt[is.finite(waarde)]
# Sorteer groep
long_dt[, groep := factor(groep, levels = c("Goed voorspeld", "Gemiddeld", "Slecht voorspeld", "Klein (n=4)"))]
long_dt[, waterschap := factor(waterschap,
  levels = c("AGV", "HHNK", "Rijnland", "HDSR", "HHSK", "Fryslân", "WDOD", "HDL", "WVV"))]

p_ws_pred <- ggplot(long_dt, aes(x = waterschap, y = waarde, fill = groep)) +
  geom_boxplot(alpha = 0.75, outlier.alpha = 0.3, outlier.size = 0.8) +
  facet_wrap(~ predictor_nl, scales = "free_y", ncol = 2) +
  scale_fill_manual(
    values = c(
      "Goed voorspeld"   = "#0072B2",
      "Gemiddeld"        = "#009E73",
      "Slecht voorspeld" = "#D55E00",
      "Klein (n=4)"      = "#999999"
    ),
    name = "CV prestatie"
  ) +
  labs(
    title    = "Verdeling van belangrijkste predictoren per waterschap",
    subtitle = "Oranje = slecht voorspeld (WDOD, HDL), blauw = goed voorspeld (AGV, HHNK, Rijnland)",
    x        = NULL,
    y        = "Waarde"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
    strip.text       = element_text(size = 8.5, face = "bold"),
    strip.background = element_rect(fill = "grey95", colour = "grey70", linewidth = 0.5),
    panel.border     = element_rect(colour = "grey80", fill = NA, linewidth = 0.4),
    legend.position  = "bottom",
    plot.title       = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle    = element_text(size = 9, hjust = 0.5, color = "grey40")
  )

print(p_ws_pred)
ggsave(
  "output/AlleGebieden/Tussenrapportage/Waterschap_predictoren_vergelijking.png",
  plot = p_ws_pred,
  width = 30, height = 20, units = "cm", dpi = 300
)

## Doelvariabelen per waterschap ------------------------------------------------

## Beheervariabelen per waterschap --------------------------------------------

beheer_vars <- c(
  "Maaifrequentie_oever_per_jaar",
  "Maaifrequentie_perceel_per_jaar",
  "Baggerfrequentie_per_jaar",
  "Baggermoment_maand",
  "koebelasting_drinkende_koeien",
  "Methode_toedienen_dierlijke_mest",
  "Breedte_bemestingsvrije_zone_huidig_m"
)

beheer_nl <- c(
  "Maaifrequentie_oever_per_jaar"          = "Maaifrequentie oever (p/jr)",
  "Maaifrequentie_perceel_per_jaar"        = "Maaifrequentie perceel (p/jr)",
  "Baggerfrequentie_per_jaar"              = "Baggerfrequentie (p/jr)",
  "Baggermoment_maand"                     = "Baggermoment (maand gem.)",
  "koebelasting_drinkende_koeien"          = "Koebelasting drinkend",
  "Methode_toedienen_dierlijke_mest"       = "Methode mesttoediening (0-5)",
  "Breedte_bemestingsvrije_zone_huidig_m"  = "Bemestingsvrije zone (m)"
)

beheer_avail <- beheer_vars[beheer_vars %in% names(abio_proj)]

beheer_dt <- abio_proj[waterschap %in% ws_groep$waterschap,
  c("waterschap", beheer_avail), with = FALSE]
beheer_dt <- merge(beheer_dt, ws_groep, by = "waterschap")

long_beheer <- melt(
  beheer_dt,
  id.vars       = c("waterschap", "groep"),
  measure.vars  = beheer_avail,
  variable.name = "variabele",
  value.name    = "waarde"
)
long_beheer[, variabele_nl := beheer_nl[as.character(variabele)]]
long_beheer <- long_beheer[is.finite(waarde)]
long_beheer[, groep := factor(groep,
  levels = c("Goed voorspeld", "Gemiddeld", "Slecht voorspeld", "Klein (n=4)"))]
long_beheer[, waterschap := factor(waterschap,
  levels = c("AGV", "HHNK", "Rijnland", "HDSR", "HHSK", "Fryslân", "WDOD", "HDL", "WVV"))]

p_beheer <- ggplot(long_beheer, aes(x = waterschap, y = waarde, fill = groep)) +
  geom_boxplot(alpha = 0.75, outlier.alpha = 0.3, outlier.size = 0.8) +
  facet_wrap(~ variabele_nl, scales = "free_y", ncol = 2) +
  scale_fill_manual(
    values = c(
      "Goed voorspeld"   = "#0072B2",
      "Gemiddeld"        = "#009E73",
      "Slecht voorspeld" = "#D55E00",
      "Klein (n=4)"      = "#999999"
    ),
    name = "CV prestatie"
  ) +
  labs(
    title    = "Beheervariabelen per waterschap",
    subtitle = "Oranje = slecht voorspeld (WDOD, HDL), blauw = goed voorspeld (AGV, HHNK, Rijnland)",
    x        = NULL,
    y        = "Waarde"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
    strip.text       = element_text(size = 8.5, face = "bold"),
    strip.background = element_rect(fill = "grey95", colour = "grey70", linewidth = 0.5),
    panel.border     = element_rect(colour = "grey80", fill = NA, linewidth = 0.4),
    legend.position  = "bottom",
    plot.title       = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle    = element_text(size = 9, hjust = 0.5, color = "grey40")
  )

print(p_beheer)
ggsave(
  "output/AlleGebieden/Tussenrapportage/Waterschap_beheer_vergelijking.png",
  plot = p_beheer,
  width = 28, height = 22, units = "cm", dpi = 300
)


# 4. GAM met ruimtelijke smoothing per target -----------------------------------
# Voor de 5 belangrijkste predictoren per target (op basis van permutation importance)
# + een 2D ruimtelijke smooth (lon/lat) om gebiedseffecten op te vangen.
# Gebruik mgcv::gam met REML voor optimale smooth-selectie.
library(mgcv)
# Voeg coördinaten toe aan abio_proj (middelpunt traject, WGS84)
abio_proj[, lon := (Start_traject_long_abio + End_traject_long_abio) / 2]
abio_proj[, lat := (Start_traject_lat_abio  + End_traject_lat_abio)  / 2]
gam_results  <- list()
gam_plots    <- list()

for (tgt in target_vars) {
  if (!tgt %in% colnames(abio_proj)) next
  cat("GAM voor:", tgt, "\n")

  # Top-5 predictoren op basis van permutation importance
  top5 <- all_perm_importance[
    target_var == tgt
  ][order(-delta_rmse)][1:min(.N, 5), Feature]
  top5 <- top5[top5 %in% colnames(abio_proj)]
  if (length(top5) == 0) next

  # Modeldata: top5 + target + coördinaten
  gam_vars <- unique(c(tgt, top5, "lon", "lat"))
  gam_data <- as.data.frame(abio_proj[
    complete.cases(abio_proj[, ..gam_vars]),
    ..gam_vars
  ])

  if (nrow(gam_data) < 20) next

  orig_names  <- names(gam_data)
  safe_names  <- make.names(orig_names, unique = TRUE)
  names(gam_data) <- safe_names

  # Mapping origineel -> veilig en terug
  name_map    <- setNames(safe_names, orig_names)
  name_map_rv <- setNames(orig_names, safe_names)

  tgt_safe  <- name_map[tgt]
  top5_safe <- name_map[top5]

  # Zorg dat target numeriek is (kan character worden na as.data.frame)
  gam_data[[tgt_safe]] <- as.numeric(gam_data[[tgt_safe]])

  k_val <- min(5, floor(nrow(gam_data) / 10))

  smooth_terms <- paste0('s(', top5_safe, ', k=', k_val, ')', collapse = ' + ')
  spatial_term <- paste0('s(lon, lat, bs="sos", k=', min(10, floor(nrow(gam_data) / 8)), ')')
  gam_formula  <- as.formula(paste(tgt_safe, '~', smooth_terms, '+', spatial_term))

  gam_fit <- tryCatch(
    mgcv::gam(gam_formula, data = gam_data, method = "REML",
               select = TRUE),   # select=TRUE: automatisch nullify niet-significante smooths
    error = function(e) { cat("  GAM fout:", conditionMessage(e), "\n"); NULL }
  )
  if (is.null(gam_fit)) next

  # Performance
  pred_gam  <- predict(gam_fit, gam_data)
  r2_gam    <- cor(gam_data[[tgt_safe]], pred_gam)^2
  rmse_gam  <- sqrt(mean((gam_data[[tgt_safe]] - pred_gam)^2))
  dev_exp   <- summary(gam_fit)$dev.expl

  gam_results[[tgt]] <- data.table(
    target   = tgt,
    target_nl = target_names_dutch[tgt],
    r2       = r2_gam,
    rmse     = rmse_gam,
    dev_expl = dev_exp,
    n        = nrow(gam_data),
    top5_preds = paste(top5, collapse = ", ")
  )

  # Smooth plots per predictor -----------------------------------------------
  # Gebruik plot.gam-data via mgcv::plot.gam → capture via gratia indien beschikbaar,
  # anders handmatig via predict over een grid
  plot_list <- lapply(seq_along(top5_safe), function(i) {
    feat_safe <- top5_safe[i]
    feat_orig <- top5[i]
    feat_nl   <- nederlandse_namen[feat_orig]
    if (is.na(feat_nl)) feat_nl <- feat_orig

    x_seq   <- seq(min(gam_data[[feat_safe]], na.rm = TRUE),
                   max(gam_data[[feat_safe]], na.rm = TRUE),
                   length.out = 100)

    # Grid: varieer één predictor, rest op mediaan; gebruik veilige namen
    safe_pred_names <- safe_names[safe_names != tgt_safe]
    grid_df <- as.data.frame(lapply(safe_pred_names, function(v) {
      if (v == feat_safe) x_seq else rep(median(gam_data[[v]], na.rm = TRUE), 100)
    }))
    names(grid_df) <- safe_pred_names

    pred_obj       <- predict(gam_fit, newdata = grid_df, se.fit = TRUE)
    grid_df$fit    <- pred_obj$fit
    grid_df$se     <- pred_obj$se.fit
    grid_df$lo     <- grid_df$fit - 1.96 * grid_df$se
    grid_df$hi     <- grid_df$fit + 1.96 * grid_df$se
    grid_df$x_feat <- x_seq

    # Rug-data: gebruik originele kolomnaam via veilige naam
    rug_df <- data.frame(x = gam_data[[feat_safe]])

    ggplot(grid_df, aes(x = x_feat)) +
      geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#0072B2", alpha = 0.2) +
      geom_line(aes(y = fit), color = "#0072B2", linewidth = 1) +
      geom_rug(data = rug_df, aes(x = x),
               sides = "b", alpha = 0.3, length = unit(0.03, "npc")) +
      labs(
        title = feat_nl,
        x     = feat_nl,
        y     = paste("Effect op", target_names_dutch[tgt])
      ) +
      theme_minimal(base_size = 9) +
      theme(
        plot.title   = element_text(size = 8, face = "bold", hjust = 0.5),
        panel.border = element_rect(colour = "grey80", fill = NA, linewidth = 0.4)
      )
  })

  # Combineer in patchwork panel
  perf_str <- paste0(
    "GAM — ", target_names_dutch[tgt],
    "  |  R²: ", round(r2_gam * 100, 1), "%",
    "  |  Dev. verklaard: ", round(dev_exp * 100, 1), "%",
    "  |  RMSE: ", round(rmse_gam, 3), "  |  n=", nrow(gam_data)
  )

  panel <- wrap_plots(plot_list, nrow = 1) +
    plot_annotation(
      title    = perf_str,
      subtitle = "Smooth effecten met 95% betrouwbaarheidsband. Overige predictoren op mediaan gehouden.",
      theme    = theme(
        plot.title    = element_text(size = 10, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 8, hjust = 0.5, color = "grey40")
      )
    )

  gam_plots[[tgt]] <- panel

  outfile <- paste0("output/AlleGebieden/Tussenrapportage/GAM_smooths_", tgt, ".png")
  print(panel)
  ggsave(outfile, panel, width = 35, height = 10, units = "cm", dpi = 200)
  cat("  Opgeslagen:", outfile, "\n")

  # Residuals per waterschap ------------------------------------------------
  # Voeg waterschap toe aan gam_data (matcht op complete-cases rijen)
  gam_data$resid      <- residuals(gam_fit)
  gam_data$waterschap <- abio_proj[
    complete.cases(abio_proj[, ..gam_vars]), waterschap
  ]

  # Sorteer op mediaan residu voor leesbaarheid
  ws_resid_order <- tapply(gam_data$resid, gam_data$waterschap, median, na.rm = TRUE)
  gam_data$waterschap <- factor(
    gam_data$waterschap,
    levels = names(sort(ws_resid_order))
  )

  # Voeg CV R² per waterschap toe als annotatie
  cv_r2_tgt <- cv_results[target == tgt & !is.na(r2),
    .(waterschap, cv_r2 = round(r2, 2), n_test)]

  p_resid <- ggplot(gam_data, aes(x = waterschap, y = resid, fill = waterschap)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.6) +
    geom_boxplot(alpha = 0.7, outlier.alpha = 0.4, outlier.size = 1) +
    geom_text(
      data = cv_r2_tgt,
      aes(x = waterschap, y = Inf,
          label = paste0("CV R²=", cv_r2, "\n(n=", n_test, ")")),
      vjust = 1.3, size = 2.5, color = "grey30", inherit.aes = FALSE
    ) +
    scale_fill_brewer(palette = "Set3", guide = "none") +
    labs(
      title    = paste0("GAM residuals per waterschap — ", target_names_dutch[tgt]),
      subtitle = "Systematische afwijking van 0 = gebiedsspecifiek effect niet gevangen door model",
      x        = NULL,
      y        = "Residu (werkelijk − voorspeld)"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x      = element_text(angle = 45, hjust = 1),
      plot.title       = element_text(size = 10, face = "bold", hjust = 0.5),
      plot.subtitle    = element_text(size = 8, hjust = 0.5, color = "grey40"),
      panel.border     = element_rect(colour = "grey80", fill = NA, linewidth = 0.4)
    )

  resid_file <- paste0("output/AlleGebieden/Tussenrapportage/GAM_residuals_ws_", tgt, ".png")
  print(p_resid)
  ggsave(resid_file, p_resid, width = 20, height = 12, units = "cm", dpi = 200)
  cat("  Residuals opgeslagen:", resid_file, "\n")

  # By-waterschap smooth: s(top_predictor, by = waterschap) voor top predictor --
  # Alleen als er >= 4 waterschappen met >= 5 obs zijn
  ws_counts <- table(gam_data$waterschap)
  ws_enough <- names(ws_counts)[ws_counts >= 5]

  if (length(ws_enough) >= 4 && length(top5_safe) >= 1) {
    feat1_safe <- top5_safe[1]
    feat1_orig <- top5[1]
    feat1_nl   <- nederlandse_namen[feat1_orig]
    if (is.na(feat1_nl)) feat1_nl <- feat1_orig

    gam_data_by <- gam_data[gam_data$waterschap %in% ws_enough, ]
    gam_data_by$waterschap <- droplevels(gam_data_by$waterschap)

    by_formula <- as.formula(paste0(
      tgt_safe, ' ~ s(', feat1_safe, ', by = waterschap, k=', k_val, ') + ',
      's(lon, lat, bs="sos", k=', min(8, floor(nrow(gam_data_by) / 8)), ') + ',
      'waterschap'
    ))

    gam_by <- tryCatch(
      mgcv::gam(by_formula, data = gam_data_by, method = "REML"),
      error = function(e) { cat("  by-waterschap GAM fout:", conditionMessage(e), "\n"); NULL }
    )

    if (!is.null(gam_by)) {
      # Voorspelgrid per waterschap
      x_seq  <- seq(min(gam_data_by[[feat1_safe]], na.rm = TRUE),
                    max(gam_data_by[[feat1_safe]], na.rm = TRUE),
                    length.out = 80)

      by_grid <- rbindlist(lapply(ws_enough, function(ws) {
        grid_row <- as.data.frame(lapply(
          names(gam_data_by)[!names(gam_data_by) %in% c("resid", tgt_safe)],
          function(v) {
            if (v == feat1_safe)   x_seq
            else if (v == "waterschap") rep(ws, 80)
            else rep(median(gam_data_by[[v]], na.rm = TRUE), 80)
          }
        ))
        names(grid_row) <- names(gam_data_by)[!names(gam_data_by) %in% c("resid", tgt_safe)]
        grid_row$waterschap <- factor(ws, levels = levels(gam_data_by$waterschap))
        grid_row$x_feat <- x_seq
        grid_row
      }))

      pred_by        <- predict(gam_by, newdata = by_grid, se.fit = TRUE)
      by_grid$fit    <- pred_by$fit
      by_grid$lo     <- pred_by$fit - 1.96 * pred_by$se.fit
      by_grid$hi     <- pred_by$fit + 1.96 * pred_by$se.fit

      p_by <- ggplot(by_grid, aes(x = x_feat, color = waterschap, fill = waterschap)) +
        geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.12, color = NA) +
        geom_line(aes(y = fit), linewidth = 0.9) +
        scale_color_brewer(palette = "Set2", name = "Waterschap") +
        scale_fill_brewer(palette = "Set2", guide = "none") +
        labs(
          title    = paste0("Gebiedsspecifieke smooth: ", feat1_nl,
                            " → ", target_names_dutch[tgt]),
          subtitle = "Aparte smooth per waterschap. Divergentie = gebiedsspecifieke relatie.",
          x        = feat1_nl,
          y        = target_names_dutch[tgt]
        ) +
        theme_minimal(base_size = 10) +
        theme(
          plot.title    = element_text(size = 10, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 8, hjust = 0.5, color = "grey40"),
          panel.border  = element_rect(colour = "grey80", fill = NA, linewidth = 0.4),
          legend.position = "bottom"
        )

      by_file <- paste0("output/AlleGebieden/Tussenrapportage/GAM_by_ws_", tgt, ".png")
      print(p_by)
      ggsave(by_file, p_by, width = 18, height = 12, units = "cm", dpi = 200)
      cat("  By-waterschap smooth opgeslagen:", by_file, "\n")
    }
  }
}

# 4a. Overzicht GAM vs XGBoost performance ----------------------------------------
gam_summary <- rbindlist(gam_results)
gam_summary <- merge(
  gam_summary,
  performance_summary[, .(target, r2_test, rmse_test)],
  by = "target", all.x = TRUE
)
gam_summary[, r2_diff := r2 - r2_test]

cat("\n=== GAM vs XGBoost (testset) vergelijking ===\n")
print(gam_summary[, .(target_nl, n,
                       r2_gam   = round(r2 * 100, 1),
                       r2_xgb   = round(r2_test * 100, 1),
                       diff_pct = round(r2_diff * 100, 1),
                       dev_expl = round(dev_exp * 100, 1))])



# 5. Random Forest analyse met ruimtelijke cross-validatie ----------------------
## Zelfde target- en predictorvariabelen als sectie 3 (XGBoost)
## Ruimtelijke CV: leave-one-waterschap-out (analoog aan sectie 3)
## Package: ranger (snelle RF-implementatie, ondersteunt importance = "permutation")

library(ranger)
library(patchwork)
## ---------- herstel target/predictor definities (kunnen overschreven zijn in sectie 4) ----------
rf_target_vars <- c(
  "n_soorten_oev_zone2", "oeverindex", "Soortensamenstelling Helofyten",
  "waterzone_1_subm_tot_perc", "n_soorten_sub_zone1", "Soortensamenstelling Hydrofyten",
  "draagkracht_oever", "slib_redox_pH7", "P-AL mg p2o5/100g_SB", "max_slib"
)

rf_target_names_dutch <- c(
  "waterzone_1_subm_tot_perc"       = "Bedekking ondergedoken planten (%)",
  "n_soorten_oev_zone2"             = "Aantal oeversoorten",
  "oeverindex"                      = "Oeverindex",
  "n_soorten_sub_zone1"             = "Aantal waterplantensoorten",
  "Soortensamenstelling Helofyten"  = "Soortensamenstelling Helofyten",
  "Soortensamenstelling Hydrofyten" = "Soortensamenstelling Hydrofyten",
  "draagkracht_oever"               = "Draagkracht oever (MPa)",
  "slib_redox_pH7"                  = "Redox slib bij pH7 (mV)",
  "P-AL mg p2o5/100g_SB"           = "P-AL slib (mg P2O5/100g)",
  "max_slib"                        = "Slibdikte (m)"
)

# Identiek aan cols_corr in sectie 3 (XGBoost) zodat beide modellen
# op dezelfde rijen en predictoren worden vergeleken
rf_cols_corr <- cols_corr

rf_nederlandse_namen <- c(
  "drglg"                          = "Drooglegging (m)",
  "max_wtd"                        = "Maximale waterdiepte (m)",
  "zichtdiepte"                    = "Doorzicht/waterdiepte",
  "max_slib"                       = "Maximale slibdikte (m)",
  "watbte"                         = "Waterbreedte (m)",
  "oeverzone_2b_breedte_cm"        = "Breedte oevervegetatiezone 2b (cm)",
  "oeverzone_2b_kaal_perc"         = "Bedekking kale oever zone 2b (%)",
  "holleoever"                     = "Onderholling (cm)",
  "tldk_wtrwtr_perc"               = "Taludhoek onder waterlijn (%)",
  "tldk_oevrwtr_perc"              = "Taludhoek oever (%)",
  "slib_redox_pH7"                 = "Redox slib bij pH7 (mV)",
  "slib_pH"                        = "pH slib",
  "oevbte"                         = "Oeverbreedte (m)",
  "veentype_num"                   = "Veentype (numeriek)",
  "Z_CLAY_SA_OR_25"                = "Kleigehalte 25cm (%)",
  "OS_perc_OR_25"                  = "Organisch stofgehalte 25cm (%)",
  "CEC_CO_mmol+/kg_OR_25"         = "Cation Exchange Capacity (mmol+/kg)",
  "draagkracht_oever"              = "Draagkracht oever (MPa)",
  "dieptebin_min"                  = "Diepte laagste draagkracht (m)",
  "draagkracht_perceel"            = "Draagkracht perceel (MPa)",
  "water_pH"                       = "Water pH",
  "watertemp_C"                    = "Watertemperatuur (°C)",
  "Cl_µmol/l_PW"                  = "Chloride poriewater (µmol/l)",
  "NH4_µmol/l_PW"                 = "Ammonium poriewater (µmol/l)",
  "P-AL mg p2o5/100g_SB"          = "P-AL slib (mg P2O5/100g)",
  "feP_PW"                         = "FeP (mol/mol)",
  "Baggerfrequentie_per_jaar"      = "Baggerfrequentie per jaar",
  "Baggermoment_maand"             = "Baggermoment (maand)",
  "Maaifrequentie_oever_per_jaar"  = "Maaifrequentie oever per jaar",
  "Methode_toedienen_dierlijke_mest" = "Methode toedienen dierlijke mest",
  "koebelasting_drinkende_koeien"  = "Koebelasting drinkende koeien",
  "koeien_drinken_correctie"       = "Koeien drinken correctie",
  "vernat_loc"                     = "Vernatting locatie"
)

rf_rmse_units <- c(
  "waterzone_1_subm_tot_perc"       = "%",
  "n_soorten_oev_zone2"             = "soorten",
  "n_soorten_sub_zone1"             = "soorten",
  "draagkracht_oever"               = "MPa",
  "slib_redox_pH7"                  = "mV",
  "max_slib"                        = "m",
  "oeverindex"                      = "-",
  "Soortensamenstelling Helofyten"  = "-",
  "Soortensamenstelling Hydrofyten" = "-",
  "P-AL mg p2o5/100g_SB"           = "mg P2O5/100g"
)

## ---------- hulpfunctie: data voorbereiden voor één target ----------
rf_prepare_data <- function(target, data, predictors) {
  model_vars <- c(target, predictors)
  model_vars <- model_vars[model_vars %in% colnames(data)]

  df <- as.data.frame(data[, ..model_vars])

  # Karakter-/factor-kolommen naar numeriek
  char_cols <- names(df)[sapply(df, function(x) is.character(x) | is.factor(x))]
  for (col in char_cols) df[[col]] <- as.numeric(as.factor(df[[col]]))

  # Verwijder rijen met NA in target of predictoren
  df <- df[complete.cases(df), ]
  df
}

## ---------- 1) RF modellen trainen (60 / 20 / 20 split) ----------
# Split: 60% train → model leren
#        20% validatie → onafhankelijke tussentijdse evaluatie
#        20% test      → finale, eenmalige evaluatie
# De validatieset is hier niet gebruikt voor hyperparameter-tuning,
# maar maakt het mogelijk om overfit te signaleren vóórdat de testset
# wordt bekeken (train ≫ val wijst op overfit; val ≈ test is gezond).

set.seed(5823)

rf_models          <- list()
rf_performance     <- list()
rf_importance_all  <- list()

rmse_fn <- function(y, yhat) sqrt(mean((y - yhat)^2, na.rm = TRUE))
r2_fn   <- function(y, yhat) {
  ss_res <- sum((y - yhat)^2, na.rm = TRUE)
  ss_tot <- sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
  if (ss_tot == 0) NA_real_ else 1 - ss_res / ss_tot
}

for (target in rf_target_vars) {
  if (!target %in% colnames(abio_proj)) next
  cat("RF training voor:", target, "\n")

  preds <- rf_cols_corr[!rf_cols_corr %in% target & rf_cols_corr %in% colnames(abio_proj)]
  df    <- rf_prepare_data(target, abio_proj, preds)
  if (nrow(df) < 30) next

  # Drie-weg split
  n         <- nrow(df)
  idx       <- sample(n)
  train_end <- floor(0.6 * n)
  val_end   <- floor(0.8 * n)

  train_df <- df[idx[1:train_end], ]
  val_df   <- df[idx[(train_end + 1):val_end], ]
  test_df  <- df[idx[(val_end + 1):n], ]

  # ranger verwacht kolomnamen zonder speciale tekens in de formula →
  # gebruik dependent.variable.name + data
  safe_target <- make.names(target)
  names(train_df)[names(train_df) == target] <- safe_target
  names(val_df  )[names(val_df)   == target] <- safe_target
  names(test_df )[names(test_df)  == target] <- safe_target

  rf_fit <- ranger(
    dependent.variable.name = safe_target,
    data                    = train_df,
    num.trees               = 500,
    mtry                    = max(1L, floor(sqrt(ncol(train_df) - 1))),
    min.node.size           = 5,
    importance              = "permutation",
    seed                    = 5823
  )

  pred_val  <- predict(rf_fit, data = val_df )$predictions
  pred_test <- predict(rf_fit, data = test_df)$predictions

  rf_models[[target]]      <- rf_fit
  rf_performance[[target]] <- data.table(
    target     = target,
    r2_oob     = rf_fit$r.squared,
    rmse_oob   = sqrt(rf_fit$prediction.error),
    r2_val     = r2_fn(val_df[[safe_target]],   pred_val),
    rmse_val   = rmse_fn(val_df[[safe_target]],  pred_val),
    r2_test    = r2_fn(test_df[[safe_target]],  pred_test),
    rmse_test  = rmse_fn(test_df[[safe_target]], pred_test),
    n_train    = nrow(train_df),
    n_val      = nrow(val_df),
    n_test     = nrow(test_df)
  )

  # Variable importance (permutation) → top 10
  imp_vec <- rf_fit$variable.importance
  imp_dt  <- data.table(
    Feature    = names(imp_vec),
    Importance = imp_vec,
    target_var = target
  )[order(-Importance)][1:min(.N, 10)]
  imp_dt[, Nederlandse_naam := rf_nederlandse_namen[Feature]]
  imp_dt[is.na(Nederlandse_naam), Nederlandse_naam := Feature]
  rf_importance_all[[target]] <- imp_dt
}

rf_perf_summary <- rbindlist(rf_performance)
cat("\n=== Random Forest – OOB / validatie / testset prestaties ===\n")
print(rf_perf_summary[, .(
  target,
  r2_oob   = round(r2_oob  * 100, 1),
  r2_val   = round(r2_val  * 100, 1),
  r2_test  = round(r2_test * 100, 1),
  rmse_val  = round(rmse_val,  3),
  rmse_test = round(rmse_test, 3),
  n_train, n_val, n_test
)])

## ---------- 2) VIP plot Random Forest ----------
all_rf_importance <- rbindlist(rf_importance_all)
all_rf_importance[, target_dutch := rf_target_names_dutch[target_var]]
all_rf_importance[, rmse_unit    := rf_rmse_units[target_var]]
all_rf_importance <- merge(
  all_rf_importance,
  rf_perf_summary[, .(target, r2_test, rmse_test)],
  by.x = "target_var", by.y = "target", all.x = TRUE
)
all_rf_importance[, plot_title := paste0(
  target_dutch, "\nR²: ", round(r2_test * 100, 1),
  "% | RMSE: ", round(rmse_test, 3), " ", rmse_unit
)]

# Correlatierichting (Pearson) voor kleur
all_rf_importance[, correlation_direction := mapply(
  function(tgt, feat) {
    tryCatch({
      if (!tgt %in% colnames(abio_proj) || !feat %in% colnames(abio_proj)) return(NA_character_)
      corr <- cor(as.numeric(abio_proj[[tgt]]), as.numeric(abio_proj[[feat]]), use = "complete.obs")
      ifelse(corr > 0, "+", "-")
    }, error = function(e) NA_character_)
  },
  tgt  = target_var,
  feat = Feature,
  USE.NAMES = FALSE
)]

# Sorteer per facet op importance
plot_rf_vip <- all_rf_importance[!is.na(correlation_direction)][
  order(target_var, Importance)
][, feat_label := factor(
  paste0(target_var, "__", Nederlandse_naam),
  levels = unique(paste0(target_var, "__", Nederlandse_naam))
)]

p_rf_vip <- ggplot(plot_rf_vip, aes(
    x    = feat_label,
    y    = Importance,
    fill = correlation_direction
  )) +
  geom_col() +
  geom_text(
    aes(label = correlation_direction),
    hjust = -0.2, size = 3.5, fontface = "bold", color = "grey20"
  ) +
  facet_wrap(~ plot_title, scales = "free", ncol = 3) +
  scale_x_discrete(labels = function(x) sub(".*__", "", x)) +
  coord_flip() +
  scale_fill_manual(
    values = c("+" = "#0072B2", "-" = "#D55E00"),
    labels = c("+" = "Positief verband", "-" = "Negatief verband"),
    name   = "Correlatierichting"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title    = "Belangrijkste verklarende variabelen (Random Forest – permutation importance)",
    subtitle = "Permutation importance met correlatierichting op basis van Pearson correlatie",
    x        = NULL,
    y        = "Permutation importance (ΔMSE)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.y        = element_text(size = 9),
    axis.text.x        = element_text(size = 8),
    strip.text         = element_text(size = 8.5, face = "bold", lineheight = 1.1),
    strip.background   = element_rect(fill = "grey95", colour = "grey70", linewidth = 0.6),
    panel.border       = element_rect(colour = "grey80", fill = NA, linewidth = 0.5),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "bottom",
    plot.title         = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle      = element_text(size = 9.5, hjust = 0.5, color = "grey40"),
    plot.margin        = margin(10, 15, 10, 10)
  )

print(p_rf_vip)
ggsave(
  "output/AlleGebieden/Tussenrapportage/RF_feature_importance.png",
  plot  = p_rf_vip,
  width = 35, height = 30, units = "cm", dpi = 300
)

## ---------- 3) Ruimtelijke cross-validatie: leave-one-waterschap-out ----------
# Elke fold: train op alle waterschappen behalve één, test op dat waterschap.
# Dit test of het model generaliseert naar een ruimtelijk nieuw gebied.

waterschappen_rf <- abio_proj[!is.na(waterschap), unique(waterschap)]
rf_cv_records    <- list()

for (target in rf_target_vars) {
  if (!target %in% colnames(abio_proj)) next
  cat("Ruimtelijke CV (RF) voor:", target, "\n")

  preds <- rf_cols_corr[!rf_cols_corr %in% target & rf_cols_corr %in% colnames(abio_proj)]

  for (ws in waterschappen_rf) {

    train_rows <- which(abio_proj$waterschap != ws & !is.na(abio_proj$waterschap))
    test_rows  <- which(abio_proj$waterschap == ws)

    if (length(train_rows) < 20 || length(test_rows) < 3) next

    train_df <- rf_prepare_data(
      target,
      abio_proj[train_rows],
      preds
    )
    test_df <- rf_prepare_data(
      target,
      abio_proj[test_rows],
      preds
    )

    # Houd alleen kolommen die in beide sets aanwezig zijn
    shared_cols <- intersect(names(train_df), names(test_df))
    train_df    <- train_df[, shared_cols]
    test_df     <- test_df[, shared_cols]

    if (nrow(train_df) < 20 || nrow(test_df) < 3) next

    safe_target <- make.names(target)
    names(train_df)[names(train_df) == target] <- safe_target
    names(test_df )[names(test_df)  == target] <- safe_target

    cv_fit <- tryCatch(
      ranger(
        dependent.variable.name = safe_target,
        data                    = train_df,
        num.trees               = 300,
        mtry                    = max(1L, floor(sqrt(ncol(train_df) - 1))),
        min.node.size           = 5,
        seed                    = 5823
      ),
      error = function(e) NULL
    )

    if (is.null(cv_fit)) next

    pred_test  <- predict(cv_fit, data = test_df)$predictions
    y_test     <- test_df[[safe_target]]
    residuals  <- y_test - pred_test
    rmse_ws    <- sqrt(mean(residuals^2))
    r2_ws      <- tryCatch({
      ss_res <- sum(residuals^2)
      ss_tot <- sum((y_test - mean(y_test))^2)
      if (ss_tot == 0) NA_real_ else 1 - ss_res / ss_tot
    }, error = function(e) NA_real_)
    mae_ws <- mean(abs(residuals))

    rf_cv_records[[length(rf_cv_records) + 1]] <- data.table(
      target      = target,
      target_nl   = rf_target_names_dutch[target],
      waterschap  = ws,
      n_test      = length(y_test),
      n_train     = nrow(train_df),
      rmse        = rmse_ws,
      r2          = r2_ws,
      mae         = mae_ws,
      mean_actual = mean(y_test, na.rm = TRUE),
      rmse_pct    = 100 * rmse_ws / mean(abs(y_test), na.rm = TRUE)
    )
  }
}

rf_cv_results <- rbindlist(rf_cv_records, fill = TRUE)

### CV samenvatting per waterschap
rf_cv_ws_summary <- rf_cv_results[, .(
  mean_r2       = mean(r2,       na.rm = TRUE),
  mean_rmse_pct = mean(rmse_pct, na.rm = TRUE),
  n_targets     = .N
), by = waterschap][order(-mean_rmse_pct)]

cat("\n=== RF ruimtelijke CV – prestatie per waterschap ===\n")
print(rf_cv_ws_summary)

### Plot: R² per waterschap per target
rf_cv_plot_dt <- rf_cv_results[!is.na(r2)]
rf_cv_plot_dt[, target_nl := rf_target_names_dutch[target]]

ws_order_rf <- rf_cv_results[, .(mean_r2 = mean(r2, na.rm = TRUE)), by = waterschap][
  order(mean_r2), waterschap
]
rf_cv_plot_dt[, waterschap := factor(waterschap, levels = ws_order_rf)]

# Clip y-as per facet op [-5, 1] om extreme uitschieters (bijv. HHSK Hydrofyten = -483)
# niet de schaal te laten domineren. Geclipt waterschap krijgt een annotatie.
r2_clip_lo <- -5
r2_clip_hi  <- 1

rf_cv_plot_clipped <- copy(rf_cv_plot_dt)
rf_cv_plot_clipped[, r2_clipped  := pmax(r2_clip_lo, pmin(r2_clip_hi, r2))]
rf_cv_plot_clipped[, is_clipped  := !is.na(r2) & (r2 < r2_clip_lo | r2 > r2_clip_hi)]
rf_cv_plot_clipped[, clip_label  := fifelse(is_clipped, paste0("R²=", round(r2, 0)), NA_character_)]

p_rf_cv_r2 <- ggplot(rf_cv_plot_clipped, aes(x = waterschap, y = r2_clipped, fill = waterschap)) +
  geom_col() +
  geom_hline(
    data = rf_cv_results[!is.na(r2), .(overall_r2 = mean(r2, na.rm = TRUE)), by = target][
      , .(target_nl = rf_target_names_dutch[target],
          overall_r2 = pmax(r2_clip_lo, pmin(r2_clip_hi, overall_r2)))
    ],
    aes(yintercept = overall_r2),
    linetype = "dashed", color = "grey40", linewidth = 0.5
  ) +
  geom_text(
    aes(label = n_test, y = -0.08),
    size = 2.3, color = "grey20", fontface = "bold", vjust = 1
  ) +
  # Rode annotatie voor extreme waarden (buiten y-as bereik)
  geom_text(
    data = rf_cv_plot_clipped[is_clipped == TRUE],
    aes(label = paste0("*", round(r2, 0)), y = -0.05),
    size = 2.4, color = "red", fontface = "bold", vjust = 0, hjust = 0.5
  ) +
  facet_wrap(~ target_nl, scales = "free_y", ncol = 3) +
  scale_y_continuous(
    labels = scales::label_percent(),
    limits = c(r2_clip_lo, r2_clip_hi),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  scale_fill_brewer(palette = "Set3", guide = "none") +
  labs(
    title    = "Ruimtelijke CV (RF): R² per waterschap (leave-one-out)",
    subtitle = paste0(
      "Elk waterschap is één keer weggelaten uit de trainset; de balk toont hoe goed het model dat waterschap voorspelt.\n",
      "R² > 0: beter dan gemiddelde  |  R² < 0: slechter dan gemiddelde  |  *Getal = werkelijke R² als buiten y-as bereik [-5, 1].\n",
      "Kleine getallen in de balkjes = aantal locaties in de testset."
    ),
    x        = NULL,
    y        = "R² (uit trainset weggelaten waterschap als testset)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 7),
    strip.text       = element_text(size = 8, face = "bold"),
    strip.background = element_rect(fill = "grey95", colour = "grey70", linewidth = 0.6),
    panel.border     = element_rect(colour = "grey80", fill = NA, linewidth = 0.5),
    plot.title       = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle    = element_text(size = 9, hjust = 0.5, color = "grey40")
  )

print(p_rf_cv_r2)
ggsave(
  "output/AlleGebieden/Tussenrapportage/RF_CV_waterschap_r2.png",
  plot  = p_rf_cv_r2,
  width = 35, height = 30, units = "cm", dpi = 300
)

### Plot: RMSE% per waterschap
rf_clip_pct    <- 200
rf_n_outliers  <- rf_cv_results[!is.na(rmse_pct) & rmse_pct > rf_clip_pct, .N]
rf_ws_rmse_ord <- rf_cv_ws_summary[order(-mean_rmse_pct), waterschap]
rf_cv_results[, waterschap_f := factor(waterschap, levels = rf_ws_rmse_ord)]

p_rf_cv_rmse <- ggplot(
    rf_cv_results[!is.na(rmse_pct) & rmse_pct <= rf_clip_pct],
    aes(x = waterschap_f, y = rmse_pct, fill = waterschap_f)
  ) +
  geom_boxplot(alpha = 0.7, outlier.size = 1.5) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "red", alpha = 0.6) +
  annotate(
    "text", x = Inf, y = rf_clip_pct, hjust = 1.1, vjust = -0.4,
    label  = paste0(rf_n_outliers, " combinaties boven ", rf_clip_pct, "% weggelaten"),
    size   = 3, color = "grey40", fontface = "italic"
  ) +
  scale_fill_brewer(palette = "Set3", guide = "none") +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1),
    limits = c(0, rf_clip_pct)
  ) +
  labs(
    title    = "Ruimtelijke CV (RF): RMSE als % van gemiddelde targetwaarde",
    subtitle = paste0(
      "Rode lijn = 100% (RMSE = gemiddelde targetwaarde). Lager = beter. Y-as geclipped op ",
      rf_clip_pct, "%."
    ),
    x = NULL,
    y = "RMSE (% van gemiddelde)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x   = element_text(angle = 45, hjust = 1),
    plot.title    = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey40"),
    panel.border  = element_rect(colour = "grey80", fill = NA, linewidth = 0.5)
  )

print(p_rf_cv_rmse)
ggsave(
  "output/AlleGebieden/Tussenrapportage/RF_CV_waterschap_rmse.png",
  plot  = p_rf_cv_rmse,
  width = 25, height = 15, units = "cm", dpi = 300
)

## ---------- 4) RF vs XGBoost vergelijking (testset R²) ----------
# Koppel RF testset R² aan XGBoost testset R² voor alle targets
rf_vs_xgb <- merge(
  rf_perf_summary[, .(target, r2_rf = r2_test, rmse_rf = rmse_test)],
  performance_summary[, .(target, r2_xgb = r2_test, rmse_xgb = rmse_test)],
  by = "target", all = TRUE
)
rf_vs_xgb[, target_nl := rf_target_names_dutch[target]]
rf_vs_xgb[, rmse_unit := rf_rmse_units[target]]

cat("\n=== RF vs XGBoost (testset R²) ===\n")
print(rf_vs_xgb[, .(
  target_nl,
  r2_rf  = round(r2_rf  * 100, 1),
  r2_xgb = round(r2_xgb * 100, 1),
  diff   = round((r2_rf - r2_xgb) * 100, 1)
)][order(-r2_rf)])

# Vergelijkingsplot: RF vs XGBoost R²
rf_vs_xgb_long <- melt(
  rf_vs_xgb[, .(target_nl, r2_rf, r2_xgb)],
  id.vars       = "target_nl",
  variable.name = "model",
  value.name    = "r2"
)[, model := fifelse(model == "r2_rf", "Random Forest", "XGBoost")]

p_rf_xgb_compare <- ggplot(
    rf_vs_xgb_long[!is.na(r2)],
    aes(x = reorder(target_nl, r2), y = r2, fill = model)
  ) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = c("Random Forest" = "#009E73", "XGBoost" = "#0072B2"),
    name   = "Model"
  ) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    title    = "Random Forest vs XGBoost: R² op testset",
    subtitle = "Beide modellen getraind op dezelfde predictoren en 80/20 train-test split",
    x        = NULL,
    y        = "R² (testset)"
  ) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle   = element_text(size = 9.5, hjust = 0.5, color = "grey40"),
    panel.border    = element_rect(colour = "grey80", fill = NA, linewidth = 0.5)
  )

print(p_rf_xgb_compare)
ggsave(
  "output/AlleGebieden/Tussenrapportage/RF_vs_XGBoost_r2.png",
  plot  = p_rf_xgb_compare,
  width = 22, height = 16, units = "cm", dpi = 300
)

## ---------- 5) Ruimtelijke CV vergelijking RF vs XGBoost ----------
rf_cv_summary_tgt <- rf_cv_results[!is.na(r2), .(
  r2_rf_cv = mean(r2, na.rm = TRUE)
), by = target]

xgb_cv_summary_tgt <- cv_results[!is.na(r2), .(
  r2_xgb_cv = mean(r2, na.rm = TRUE)
), by = target]

cv_compare <- merge(rf_cv_summary_tgt, xgb_cv_summary_tgt, by = "target", all = TRUE)
cv_compare[, target_nl := rf_target_names_dutch[target]]

cat("\n=== Ruimtelijke CV: RF vs XGBoost (gemiddelde R² over waterschappen) ===\n")
print(cv_compare[, .(
  target_nl,
  r2_rf_cv  = round(r2_rf_cv  * 100, 1),
  r2_xgb_cv = round(r2_xgb_cv * 100, 1),
  diff      = round((r2_rf_cv - r2_xgb_cv) * 100, 1)
)][order(-r2_rf_cv)])

cv_compare_long <- melt(
  cv_compare[, .(target_nl, r2_rf_cv, r2_xgb_cv)],
  id.vars       = "target_nl",
  variable.name = "model",
  value.name    = "r2_cv"
)[, model := fifelse(model == "r2_rf_cv", "Random Forest", "XGBoost")]

p_cv_compare <- ggplot(
    cv_compare_long[!is.na(r2_cv)],
    aes(x = reorder(target_nl, r2_cv), y = r2_cv, fill = model)
  ) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = c("Random Forest" = "#009E73", "XGBoost" = "#0072B2"),
    name   = "Model"
  ) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    title    = "Ruimtelijke CV: Random Forest vs XGBoost",
    subtitle = "Gemiddelde R² over alle leave-one-waterschap-out folds",
    x        = NULL,
    y        = "Gemiddelde R² (ruimtelijke CV)"
  ) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle   = element_text(size = 9.5, hjust = 0.5, color = "grey40"),
    panel.border    = element_rect(colour = "grey80", fill = NA, linewidth = 0.5)
  )

print(p_cv_compare)
ggsave(
  "output/AlleGebieden/Tussenrapportage/RF_vs_XGBoost_CV_r2.png",
  plot  = p_cv_compare,
  width = 22, height = 16, units = "cm", dpi = 300
)

cat("\nSectie 5 (Random Forest + ruimtelijke CV) voltooid.\n")

# 6. Sla alle benodigde resultaatobjecten op als RDS -----------------------------------------------------
rds_dir <- paste0(workspace, "output/rapport/")
dir.create(rds_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(rf_perf_summary,    paste0(rds_dir, "rf_perf_summary.rds"))
saveRDS(rf_cv_results,      paste0(rds_dir, "rf_cv_results.rds"))
saveRDS(cv_results,         paste0(rds_dir, "cv_results.rds"))
saveRDS(performance_summary,paste0(rds_dir, "performance_summary.rds"))
saveRDS(all_rf_importance,  paste0(rds_dir, "all_rf_importance.rds"))
saveRDS(all_importance,     paste0(rds_dir, "all_importance.rds"))
saveRDS(rf_target_names_dutch, paste0(rds_dir, "rf_target_names_dutch.rds"))
saveRDS(rf_rmse_units,      paste0(rds_dir, "rf_rmse_units.rds"))

cat("RDS bestanden opgeslagen in:", rds_dir, "\n")
list.files(rds_dir)

# 7. redox XGBOOST----------------------------------------------------------------------------------------
## versie redox en poriewaterconcentraties erbij-----------------------------------
target_vars <- c("slib_redox_pH7")
# Selecteer alleen poriewater µmol concentraties
# Selecteer alle kolommen die µmol bevatten
cols_umol_pw <- colnames(abio_proj)[grepl('µmol/l_PW', colnames(abio_proj), fixed = TRUE) | grepl('mmol/l FW', colnames(abio_proj), fixed = TRUE)]
# Verwijder specifieke kolommen
cols_umol_pw <- cols_umol_pw[!cols_umol_pw %in% c("Cl_2_µmol/l_PW", "Na_2_µmol/l_PW", "K_2_µmol/l_PW")]
# Update cols_corr met de gefilterde kolommen
cols_corr <- c(cols_umol_pw, "P-AL mg p2o5/100g_SB", "pH_CC_SB", "bulk density_kg DW/L FW_SB")
# Update nederlandse_namen mapping
nederlandse_namen <- c(
  setNames(cols_corr, cols_corr),
  "P-AL mg p2o5/100g_SB" = "P-AL slib (mg P2O5/100g)",
  "pH_CC_SB" = "pH slib",
  "bulk density_kg DW/L FW_SB" = "Bulk density (kg DW/L FW)"
)
# Train models for all target variables
xgb_models <- list()
model_performance <- list()
feature_importance_all <- list()

for(target in target_vars) {
  if(target %in% colnames(abio_proj)) {
    cat("Training model for:", target, "\n")
    
    # Get predictors (exclude current target from predictors)
    predictors <- cols_corr[!cols_corr %in% target & cols_corr %in% colnames(abio_proj)]
    
    # Train model
    model_result <- create_xgb_model(target, predictors, abio_proj)
    
    # Store results
    xgb_models[[target]] <- model_result$model
    model_performance[[target]] <- model_result$performance
    feature_importance_all[[target]] <- model_result$importance[, .(Feature, Nederlandse_naam, Gain)][order(-Gain)][1:10]
    feature_importance_all[[target]][, target_var := target]
  }
}

# Combine performance results
performance_summary <- rbindlist(model_performance)
print("Model Performance Summary:")
print(performance_summary)


## VIP plots -------------------------------------------------------
# Bereken RMSE percentage PER TARGET en sla direct op in all_importance
for(target in unique(all_importance$target_var)) {
  if(target %in% colnames(abio_proj)) {
    predictors <- cols_corr[!cols_corr %in% target & cols_corr %in% colnames(abio_proj)]
    
    if(length(predictors) > 0) {
      rmse_pct <- calculate_rmse_percentage(abio_proj, target, predictors)
      
      # Sla DIRECT op in all_importance voor deze specifieke target
      all_importance[target_var == target, rmse_pct_target := rmse_pct]
      
      cat("Target:", target, "- RMSE als % van gemiddelde:", rmse_pct, "%\n")
    }
  }
}
# Check of het werkt
print("RMSE percentages per target in all_importance:")
for(target in unique(all_importance$target_var)) {
  target_data <- all_importance[target_var == target]
  cat(target, ":", unique(target_data$rmse_pct_target), "%\n")
}
# Voeg ontbrekende kolommen toe als ze niet bestaan
target_names_dutch_multiline <- c(
  "Soortensamenstelling Hydrofyten" = "Soortensamenstelling\nHydrofyten",
  "Soortensamenstelling Helofyten" = "Soortensamenstelling\nHelofyten",
  "oeverindex" = "Oeverindex",
  "waterzone_1_subm_tot_perc" = "Bedekking\nondergedoken\nplanten (%)",
  "n_soorten_oev_zone2" = "Aantal\noeversoorten",
  "n_soorten_sub_zone1" = "Aantal\nwaterplantensoorten",
  "draagkracht_oever" = "Draagkracht\noever (MPa)",
  "slib_redox_pH7" = "Redox slib\nbij pH7 (mV)",
  "P-AL mg p2o5/100g_SB" ="P-AL slib (mg P2O5/100g)",
  "max_slib" = "Slibdikte (m)"
)
all_importance[, target_dutch_multiline := target_names_dutch_multiline[target_var]]
if(!"correlation_direction" %in% colnames(all_importance)) {
  all_importance[, correlation_direction := mapply(
    function(target_var, predictor_var) {
      tryCatch({
        if(!target_var %in% colnames(abio_proj) || !predictor_var %in% colnames(abio_proj)) {
          return(NA_character_)
        }
        
        target_col <- abio_proj[[target_var]]
        pred_col <- abio_proj[[predictor_var]]
        
        if(is.numeric(target_col) && is.numeric(pred_col)) {
          corr <- cor(target_col, pred_col, use = "complete.obs")
          ifelse(corr > 0, "+", "-")
        } else {
          NA_character_
        }
      }, error = function(e) NA_character_)
    },
    target_var = target_var, 
    predictor_var = Feature,
    USE.NAMES = FALSE
  )]
}

# Definieer Okabe-Ito kleuren voor elke target
targets_present <- unique(na.omit(all_importance$target_var))
okabe_ito_base <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442",
  "#0072B2", "#D55E00", "#CC79A7", "#999999", "#000000"
)
okabe_ito_colors <- setNames(
  rep(okabe_ito_base, length.out = length(targets_present)),
  targets_present
)

# Maak plot titels met kleinere R² en RMSE tekst
all_importance[, plot_title_clean := paste0(target_dutch_multiline, 
                                            "\nR²: ", round(r2_test * 100, 1), "% | RMSE: ", round(rmse_test, 3), " ", rmse_unit)]

# VIP Plot met correlatierichting als kleur (Okabe-Ito), gesorteerd per facet op Gain
okabe_dir <- c("+" = "#0072B2", "-" = "#D55E00")

plot_data <- all_importance[!is.na(correlation_direction)][
  order(target_var, Gain)
][, facet_label := paste0(target_var, "__", Nederlandse_naam)
][, facet_label := factor(facet_label, levels = unique(facet_label))]

p_vip <- ggplot(plot_data, aes(
    x = facet_label,
    y = Gain,
    fill = correlation_direction
  )) +
  geom_col() +
  geom_text(
    aes(label = correlation_direction),
    hjust = -0.2,
    size = 3.5,
    fontface = "bold",
    color = "grey20"
  ) +
  facet_wrap(~plot_title_clean, scales = "free", ncol = 3) +
  scale_x_discrete(labels = function(x) sub(".*__", "", x)) +
  coord_flip() +
  scale_fill_manual(
    values = okabe_dir,
    labels = c("+" = "Positief verband", "-" = "Negatief verband"),
    name = "Correlatierichting"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "Belangrijkste verklarende variabelen wensbeelden (XGBoost)",
    subtitle = "Variable Importance (Gain) met correlatierichting op basis van Pearson correlatie",
    x = NULL,
    y = "Informatiewinst (Gain)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 8),
    axis.title.x = element_text(size = 10, face = "bold", margin = margin(t = 6)),
    strip.text = element_text(size = 8.5, lineheight = 1.1, face = "bold"),
    strip.background = element_rect(fill = "grey95", colour = "grey70", linewidth = 0.6),
    panel.border = element_rect(colour = "grey80", fill = NA, linewidth = 0.5),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 9.5, hjust = 0.5, color = "grey40"),
    plot.margin = margin(10, 15, 10, 10)
  )

print(p_vip)
ggsave(file = 'output/AlleGebieden/Tussenrapportage/XGBoost_feature_importance_okabe_clean.png', 
       plot = p_vip,
       width = 35, height = 30, units = 'cm', dpi = 800)

## VIP vergelijkingsplot: Gain vs Permutation importance ---------------------

# Top-10 per methode, daarna union zodat features uit beide lijsten zichtbaar zijn
perm_top10 <- all_perm_importance[
  , .SD[order(-delta_rmse)][1:min(.N, 10)], by = target_var
][, .(target_var, Feature, Nederlandse_naam, delta_rmse)]

gain_top10 <- all_importance[
  , .SD[order(-Gain)][1:min(.N, 10)], by = target_var
][, .(target_var, Feature, Nederlandse_naam, Gain)]

# Volle union van features per target
all_features <- unique(rbind(
  perm_top10[, .(target_var, Feature, Nederlandse_naam)],
  gain_top10[, .(target_var, Feature, Nederlandse_naam)]
))

# Koppel beide scores; ontbrekend = 0 (feature staat niet in die top-10)
all_features <- merge(all_features, perm_top10[, .(target_var, Feature, delta_rmse)],
                      by = c("target_var", "Feature"), all.x = TRUE)
all_features <- merge(all_features, gain_top10[, .(target_var, Feature, Gain)],
                      by = c("target_var", "Feature"), all.x = TRUE)
all_features[is.na(delta_rmse), delta_rmse := 0]
all_features[is.na(Gain),       Gain       := 0]

# Normaliseer binnen target naar 0-1
all_features[, delta_rmse_norm := delta_rmse / max(delta_rmse, na.rm = TRUE), by = target_var]
all_features[, gain_norm       := Gain       / max(Gain,       na.rm = TRUE), by = target_var]

# Plottitels koppelen
all_features[, target_dutch := target_names_dutch[target_var]]
all_features <- merge(
  all_features,
  performance_summary[, .(target, rmse_val, r2_val)],
  by.x = "target_var", by.y = "target", all.x = TRUE
)
all_features[, rmse_unit := rmse_units[target_var]]
all_features[, plot_title := paste0(
  target_dutch, "\nR²(val): ", round(r2_val * 100, 1),
  "% | RMSE(val): ", round(rmse_val, 3), " ", rmse_unit
)]

# Sorteer op gemiddeld belang over beide methoden
all_features[, mean_belang := (delta_rmse_norm + gain_norm) / 2]

plot_vip_compare <- melt(
  all_features[, .(Feature, Nederlandse_naam, plot_title, gain_norm, delta_rmse_norm)],
  id.vars       = c("Feature", "Nederlandse_naam", "plot_title"),
  variable.name = "methode",
  value.name    = "belang_norm"
)[, methode := fifelse(methode == "gain_norm", "XGBoost Gain", "Permutation (ΔRMSE)")]

# Sorteer features binnen elk facet op gemiddeld belang
feat_order <- all_features[order(plot_title, mean_belang),
                            paste0(plot_title, "__", Nederlandse_naam)]
plot_vip_compare[, feat_label := factor(
  paste0(plot_title, "__", Nederlandse_naam),
  levels = unique(feat_order)
)]

p_vip_compare <- ggplot(plot_vip_compare, aes(
    x    = feat_label,
    y    = belang_norm,
    fill = methode
  )) +
  geom_col(position = "dodge") +
  facet_wrap(~ plot_title, scales = "free_y", ncol = 3) +
  scale_x_discrete(labels = function(x) sub(".*__", "", x)) +
  coord_flip() +
  scale_fill_manual(
    values = c("XGBoost Gain" = "#0072B2", "Permutation (ΔRMSE)" = "#E69F00"),
    name   = "Methode"
  ) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    title    = "VIP vergelijking: XGBoost Gain vs. Permutation Importance",
    subtitle = "Genormaliseerd binnen target (1 = hoogste belang). Permutation op validatieset. 0% = niet in top-10 van die methode.",
    x        = NULL,
    y        = "Relatief belang (genormaliseerd)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text         = element_text(size = 8, face = "bold", lineheight = 1.1),
    strip.background   = element_rect(fill = "grey95", colour = "grey70", linewidth = 0.6),
    panel.border       = element_rect(colour = "grey80", fill = NA, linewidth = 0.5),
    panel.grid.major.y = element_blank(),
    axis.text.y        = element_text(size = 8),
    legend.position    = "bottom",
    plot.title         = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle      = element_text(size = 9.5, hjust = 0.5, color = "grey40")
  )

print(p_vip_compare)
ggsave(
  "output/AlleGebieden/Tussenrapportage/VIP_gain_vs_permutation.png",
  plot = p_vip_compare,
  width = 35, height = 30, units = "cm", dpi = 300
)


## ALE plots voor redox modellen-------------------------------------------------------

# Functie om de top predictors voor redox te identificeren en samen te plotten
create_redox_ale_grid <- function() {
  
  # Haal de top variabelen op voor redox uit importance data
  redox_importance <- feature_importance_all[["slib_redox_pH7"]]
  top_redox_vars <- redox_importance[order(-Gain)][1:8]$Feature  # Top 6 meest belangrijke
  
  cat("Top variabelen voor redox:", paste(top_redox_vars, collapse = ", "), "\n")
  
  # Maak ALE plots voor alleen deze top variabelen
  redox_ale_plots <- list()
  
  target <- "slib_redox_pH7"
  
  # Get model data
  model_vars <- c("SlootID", target, cols_corr)
  model_vars <- model_vars[!model_vars %in% target & model_vars %in% colnames(abio_proj)]
  model_data <- abio_proj[complete.cases(abio_proj[, ..model_vars]), ..model_vars]
  
  # Convert factors to numeric
  factor_cols <- names(model_data)[sapply(model_data, is.character)]
  factor_cols_2 <- names(model_data)[sapply(model_data, is.factor)]
  factor_cols <- c(factor_cols, factor_cols_2)
  if(length(factor_cols) > 0) {
    model_data[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]
    model_data[, (factor_cols) := lapply(.SD, as.numeric), .SDcols = factor_cols]
  }
  
  # Remove SlootID and target for X_data
  predictors_clean <- colnames(model_data)[!colnames(model_data) %in% c("SlootID", target)]
  X_data <- as.matrix(model_data[, ..predictors_clean])
  
  # Maak ALE plots voor top variabelen
  for(var in top_redox_vars) {
    if(var %in% colnames(X_data)) {
      
      cat("Creating ALE plot for:", var, "\n")
      
      feature_idx <- which(colnames(X_data) == var)
      
      # Calculate ALE
      ale_result <- calculate_ale_manual(xgb_models[[target]], X_data, feature_idx, K = 30)
      
      # Create ALE data frame
      ale_df <- data.frame(
        x = ale_result$x_values,
        ale_effect = ale_result$ale_effects
      )
      
      # Get real data for scatter plot
      real_data <- abio_proj[!is.na(get(var)) & !is.na(get(target)), 
                            .(x = get(var), y = get(target))]
      
      # Get Dutch variable name
      var_name_dutch <- nederlandse_namen[var]
      if(is.na(var_name_dutch)) var_name_dutch <- var
      
      # Get importance rank and value
      var_rank <- which(top_redox_vars == var)
      var_importance <- round(redox_importance[Feature == var]$Gain, 3)
      
      # Scale ALE effect voor visualisatie
      target_median <- median(real_data$y, na.rm = TRUE)
      y_range <- max(real_data$y, na.rm = TRUE) - min(real_data$y, na.rm = TRUE)
      ale_range <- max(ale_df$ale_effect, na.rm = TRUE) - min(ale_df$ale_effect, na.rm = TRUE)
      
      if(ale_range > 0) {
        scale_factor <- (y_range * 0.3) / ale_range
        ale_df$ale_scaled <- target_median + ale_df$ale_effect * scale_factor
      } else {
        ale_df$ale_scaled <- target_median
      }
      
      # Get correlation direction
      correlation_direction <- ifelse(cor(real_data$x, real_data$y, use = "complete.obs") > 0, "+", "-")
      
      # Create plot
      p <- ggplot() +
        # Real data points
        geom_point(data = real_data, 
                  aes(x = x, y = y), 
                  alpha = 0.6, size = 1.5, color = "#56B4E9") +
        # Median reference line
        geom_hline(yintercept = target_median, 
                  linetype = "dashed", color = "#CC79A7", alpha = 0.8, linewidth = 0.8) +
        # ALE line
        geom_line(data = ale_df, 
                 aes(x = x, y = ale_scaled), 
                 linewidth = 1.5, color = "black") +
        # Min/Max labels
        annotate("text", 
                x = ale_df$x[which.min(ale_df$ale_effect)], 
                y = ale_df$ale_scaled[which.min(ale_df$ale_effect)],
                label = paste("Min:", round(min(ale_df$ale_effect), 3)),
                color = "#0072B2", fontface = "bold", size = 3.5, vjust = -0.5) +
        annotate("text", 
                x = ale_df$x[which.max(ale_df$ale_effect)], 
                y = ale_df$ale_scaled[which.max(ale_df$ale_effect)],
                label = paste("Max:", round(max(ale_df$ale_effect), 3)),
                color = "#009E73", fontface = "bold", size = 3.5, vjust = 1.5) +
        labs(
          title = paste0("#", var_rank, ": ", var_name_dutch),
          subtitle = paste0("Importance: ", var_importance, " | Correlatie: ", correlation_direction),
          x = var_name_dutch,
          y = "Redox slib bij pH7 (mV)"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 12, face = "bold"),
          plot.subtitle = element_text(size = 10),
          axis.title = element_text(size = 11),
          axis.text = element_text(size = 10),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8)
        )
      
      redox_ale_plots[[var]] <- p
    }
  }
  
  return(redox_ale_plots)
}

## Maak de redox ALE plots voor redoxmodel-------------------------------------------------

redox_ale_plots <- create_redox_ale_grid()
# Combineer in een grid
library(gridExtra)
if(length(redox_ale_plots) > 0) {
  
  # Maak grid met 2 rijen, 3 kolommen
  grid_plot <- do.call(grid.arrange, c(redox_ale_plots, list(ncol = 3)))
  
  # Toon de plot
  print(grid_plot)
  
  # Save de gecombineerde plot
  ggsave(
    filename = 'output/AlleGebieden/Tussenrapportage/Redox_ALE_top_predictors.png',
    plot = grid_plot,
    width = 45, height = 30, units = 'cm', dpi = 300
  )
  
  cat("\nRedox ALE grid plot opgeslagen!\n")
}

# Optioneel: Maak ook individuele plots voor grotere details
for(var_name in names(redox_ale_plots)) {
  var_clean <- gsub("[^A-Za-z0-9]", "_", var_name)
  
  ggsave(
    filename = paste0('output/AlleGebieden/Tussenrapportage/Redox_ALE_', var_clean, '.png'),
    plot = redox_ale_plots[[var_name]],
    width = 15, height = 12, units = 'cm', dpi = 300
  )
}

cat("Alle individuele redox ALE plots opgeslagen!\n")



