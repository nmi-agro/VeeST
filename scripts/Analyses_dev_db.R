# 0. filter correct data 4 analysis ----------------------------------------------------
abio_proj <- abio_proj[WP %in% c('WP1','WP2'),]
abio_proj <- abio_proj[!is.na(SlootID) & !is.na(jaar) & !is.na(instanceID_veg) & !is.na(instanceID_abio) , ]
abio_proj_complete <- abio_proj
abio_proj <- abio_proj_complete #reset
abio_proj <- abio_proj[MeenemenDataAnalyse_totaal == 'ja',] # voor xgboost
# 1. cluster analyse vergelijking met kaart --------------------------------------------------------
setDT(abio_proj)
# abio variabelen volgens jouw mapping:
# drglg -> drlg, watbte -> breedtewl, OS_perc_OR_25 -> SOM_LOI, Z_CLAY_SA_OR_25 -> A_CLAY_MI
# Zet hier de bestaande clusterkolom in abio_proj
cluster_col <- "clusters"  # of "clusters", als dat jouw kolom is

# 1) Input met vaste mapping + oude clusters (abio_loc = cluster_loc uit kolom 'clusters')
abio_clust <- abio_proj[
  !is.na(SlootID) & !is.na(drglg) & !is.na(watbte) & !is.na(OS_perc_OR_25) & !is.na(Z_CLAY_SA_OR_25) & !is.na(clusters),
  .(
    SlootID,
    drlg = as.numeric(drglg),
    breedtewl = as.numeric(watbte),
    SOM_LOI = as.numeric(OS_perc_OR_25),
    A_CLAY_MI = as.numeric(Z_CLAY_SA_OR_25),
    cluster_loc = as.character(clusters)
  )
]

# 2) 1 regel per SlootID
mode_char <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

abio_sid <- abio_clust[
  ,
  .(
    drlg = median(drlg, na.rm = TRUE),
    breedtewl = median(breedtewl, na.rm = TRUE),
    SOM_LOI = median(SOM_LOI, na.rm = TRUE),
    A_CLAY_MI = median(A_CLAY_MI, na.rm = TRUE),
    cluster_loc = mode_char(cluster_loc)
  ),
  by = SlootID
][complete.cases(drlg, breedtewl, SOM_LOI, A_CLAY_MI, cluster_loc)]

# 3) Zelfde aantal clusters als unieke oude clusters
k <- uniqueN(abio_sid$cluster_loc)

X <- scale(abio_sid[, .(drlg, breedtewl, SOM_LOI, A_CLAY_MI)])
set.seed(123)
km <- kmeans(X, centers = k, nstart = 100)

abio_sid[, cluster_abio_raw := as.integer(km$cluster)]

# 4) Sorteer nieuwe clusters op drooglegging: cluster 1 = kleinste drlg
ord <- abio_sid[
  , .(drlg_med = median(drlg, na.rm = TRUE)),
  by = cluster_abio_raw
][order(-drlg_med)]

relabel <- ord[, .(cluster_abio_raw, cluster_abio = as.character(seq_len(.N)))]
abio_sid <- merge(abio_sid, relabel, by = "cluster_abio_raw", all.x = TRUE)

# 5) 1-op-1 mapping cluster_abio -> cluster_loc (max overlap)
xt <- xtabs(~ cluster_abio + cluster_loc, data = abio_sid)
assign <- solve_LSAP(xt, maximum = TRUE)

map_1op1 <- data.table(
  cluster_abio = rownames(xt),
  cluster_loc_mapped = colnames(xt)[as.integer(assign)],
  overlap_n = xt[cbind(seq_len(nrow(xt)), as.integer(assign))]
)

# 6) Toepassen + controle
abio_sid <- merge(abio_sid, map_1op1, by = "cluster_abio", all.x = TRUE)
abio_sid[, match_1op1 := cluster_loc == cluster_loc_mapped]

kwaliteit <- abio_sid[, .(
  n = .N,
  n_match = sum(match_1op1, na.rm = TRUE),
  pct_match = 100 * mean(match_1op1, na.rm = TRUE)
)]

list(
  k_gebruikt = k,
  drlg_volgorde_clusters_abio = ord,
  mapping_1op1 = map_1op1,
  kwaliteit = kwaliteit,
  resultaat = abio_sid
)
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
cluster_map <- unique(abio_sid[, .(SlootID, cluster_abio)])
loc_cl <- loc_sf |>
  left_join(as.data.frame(cluster_map), by = "SlootID") |>
  filter(!is.na(cluster_abio)) |>
  group_by(SlootID, cluster_abio) |>
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
    aes(fill = cluster_abio),
    shape = 21,
    size = 6,
    color = "black",
    stroke = 0.4,
    alpha = 0.95
  ) +
  coord_sf(crs = st_crs(28992), xlim = xlim, ylim = ylim, expand = FALSE) +
  scale_fill_brewer(palette = "Set2", name = "Cluster abio") +
  theme_minimal(base_size = 14)

p_clusters_nl

# 2. clusteranalyse van alle veestvariabelen in abio_proj waar op minstens 3/4 van waarnemingen data van beschikbaar is-------------------------------------------------------------------------
setDT(abio_proj)
setDT(pars)
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

## ---------- 0) basisfilter ----------
abio_base <- copy(abio_proj)[
  WP %in% c("WP1", "WP2") &
    !is.na(SlootID) & !is.na(jaar) &
    !is.na(instanceID_veg) & !is.na(instanceID_abio)
]

## ---------- 1) drop: admin + geen variatie + LIAB/M3 via pars ----------
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

## ---------- 2) kolomnamen normaliseren ----------
old_nms <- names(abio_proj_clean)
new_nms <- make.unique(clean_micro(old_nms))
setnames(abio_proj_clean, old = old_nms, new = new_nms)

## ---------- 3) variabelenselectie >= 75% ----------
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

## ---------- 4) naar SlootID niveau ----------
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

## ---------- 5) kmeans ----------
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

## ---------- 6) variabele-invloed (eta²) ----------
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

## ---------- 7) boxplots ----------
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

## ---------- 8) kaart ----------
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

## ---------- 9) output ----------
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
               "draagkracht_oever", "dieptebin_min","draagkracht_perceel", "water_pH", "watertemp_C","NH4_µmol/l_PW","P-AL mg p2o5/100g_SB","feP_PW",
              "Baggerfrequentie_per_jaar","Baggermoment_maand","Maaifrequentie_oever_per_jaar",
              "koebelasting_drinkende_koeien", "koeien_drinken_correctie","vernat_loc"
            )
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
  "NH4_µmol/l_PW" = "Ammonium (µmol/l)",
  "P-AL mg p2o5/100g_SB" = "P-AL slib (mg P2O5/100g)",
  "feP_PW" = "FeP (mol/mol)",
  "Baggerfrequentie_per_jaar" = "Baggerfrequentie per jaar",
  "Baggermoment_maand" = "Baggermoment (maand)",
  "Maaifrequentie_oever_per_jaar"= "Maaifrequentie oever per jaar",
  "koebelasting_drinkende_koeien" = "Koebelasting drinkende koeien",
  "Koeien_drinken_sloot" = "Koeien drinken uit sloot correctie (ja/nee)"
)

## Preparation------------------------------------------------
library(xgboost)
# alle parameters naar nummeriek
# Create correlation matrix and p-value matrix with the SAME variables
abio_proj[,trofie := as.numeric(trofie)]
abio_proj[,draagkracht_perceel := as.numeric(draagkracht_perceel)]
# Handle non-numeric columns
abio_proj[,Maaifrequentie_oever_per_jaar := as.numeric(Maaifrequentie_oever_per_jaar)]
abio_proj[,Baggerfrequentie_per_jaar := as.numeric(Baggerfrequentie_per_jaar)]
abio_proj[,Aantal_koeien_vee_perceel_dag := as.numeric(Aantal_koeien_vee_perceel_dag)]
abio_proj[,Aantal_Koedagen_per_jaar := as.numeric(Aantal_Koedagen_per_jaar)]
abio_proj[,koebelasting_drinkende_koeien := as.numeric(koebelasting_drinkende_koeien)]
abio_proj[vernat_loc %in% c("ja", "tijdelijk","beperkt"), vernat_loc := 1]
abio_proj[vernat_loc %in% c("nee"," ")|is.na(vernat_loc), vernat_loc := 0]
abio_proj[,vernat_loc := as.numeric(vernat_loc)]

## Function to create XGBoost model for single target ---------------------------------
create_xgb_model <- function(target_var, predictors, data) {
  
  # Select variables for model
  model_vars <- c("SlootID", target_var, predictors)
  model_data <- data[complete.cases(data[, ..model_vars]), ..model_vars]
  # Convert factors to numeric for xgboost
    factor_cols <- names(model_data)[sapply(model_data, is.character)]
    factor_cols_2 <- names(model_data)[sapply(model_data, is.factor)]
    factor_cols <- c(factor_cols, factor_cols_2)
    model_data[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]
    model_data[, (factor_cols) := lapply(.SD, as.numeric), .SDcols = factor_cols]
  
  # Remove target from predictors if it exists there
  predictors_clean <- predictors[!predictors %in% target_var]
  
  # Split train/test
  set.seed(123)
  train_idx <- sample(1:nrow(model_data), nrow(model_data) * 0.8)
  
  # Prepare data for XGBoost
  X_train <- as.matrix(model_data[train_idx, ..predictors_clean])
  y_train <- model_data[train_idx, get(target_var)]
  X_test <- as.matrix(model_data[-train_idx, ..predictors_clean])
  y_test <- model_data[-train_idx, get(target_var)]
  
  # Create DMatrix
  dtrain <- xgb.DMatrix(data = X_train, label = y_train)
  dtest <- xgb.DMatrix(data = X_test, label = y_test)
  
  # Set parameters
  params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    max_depth = 6,
    eta = 0.1,
    subsample = 0.8,
    colsample_bytree = 0.8
  )
  
  # Train model
  xgb_model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 100,
    watchlist = list(train = dtrain, test = dtest),
    early_stopping_rounds = 10,
    verbose = 2
  )
  
  # Feature importance
  importance <- xgb.importance(
    feature_names = colnames(X_train),
    model = xgb_model
  )
  
  # Add readable names
  importance[, Nederlandse_naam := nederlandse_namen[Feature]]
  
  # Predictions
  pred_train <- predict(xgb_model, X_train)
  pred_test <- predict(xgb_model, X_test)
  
  # Model performance
  rmse_train <- sqrt(mean((y_train - pred_train)^2))
  rmse_test <- sqrt(mean((y_test - pred_test)^2))
  r2_train <- cor(y_train, pred_train)^2
  r2_test <- cor(y_test, pred_test)^2
  
  return(list(
    model = xgb_model,
    importance = importance,
    performance = data.table(
      target = target_var,
      rmse_train = rmse_train,
      rmse_test = rmse_test,
      r2_train = r2_train,
      r2_test = r2_test,
      n_train = length(y_train),
      n_test = length(y_test)
    ),
    predictions = data.table(
      SlootID = model_data[, SlootID],
      actual = model_data[, get(target_var)],
      predicted = c(pred_train, pred_test),
      set = c(rep("train", length(pred_train)), rep("test", length(pred_test)))
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
## VIP plots-------------------------------------------------------
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

ggplot(plot_data, aes(
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

ggsave(file = 'output/AlleGebieden/Tussenrapportage/XGBoost_feature_importance_okabe_clean.png', 
       width = 35, height = 30, units = 'cm', dpi = 800)

## Manual ALE calculation function (without ALEPlot package)---------------------------------------------
# r
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

## ALE plots nieuw combinatie plots van sturende var op alle targets -------------------------------------------------------
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

    median_lines <- unique(all_ale_data[, .(target, target_dutch, y_median)])

    ale_minmax <- all_ale_data[, .(
      ale_min_val = min(ale_effect, na.rm = TRUE),
      ale_max_val = max(ale_effect, na.rm = TRUE),
      x_pos_min = x[which.min(ale_effect)],
      x_pos_max = x[which.max(ale_effect)],
      y_pos_min = ale_scaled[which.min(ale_effect)],
      y_pos_max = ale_scaled[which.max(ale_effect)]
    ), by = .(target, target_dutch)]

    var_name_dutch <- nederlandse_namen[var]
    if (is.na(var_name_dutch)) var_name_dutch <- var

    p <- ggplot() +
      geom_point(
        data = all_real_data,
        aes(x = x, y = y),
        color = "grey70", alpha = 0.35, size = 1.2
      ) +
      geom_hline(
        data = median_lines,
        aes(yintercept = y_median),
        linetype = "dashed", color = "red", alpha = 0.7, linewidth = 0.5
      ) +
      geom_line(
        data = all_ale_data,
        aes(x = x, y = ale_scaled),
        color = "black", linewidth = 1.1
      ) +
      geom_text(
        data = ale_minmax,
        aes(x = x_pos_min, y = y_pos_min, label = paste("Min:", round(ale_min_val, 3))),
        color = "blue", size = 3, fontface = "bold", vjust = -0.4
      ) +
      geom_text(
        data = ale_minmax,
        aes(x = x_pos_max, y = y_pos_max, label = paste("Max:", round(ale_max_val, 3))),
        color = "darkgreen", size = 3, fontface = "bold", vjust = 1.3
      ) +
      facet_wrap(~target_dutch, scales = "free_y", ncol = 3) +
      labs(
        title = paste("Effect van", var_name_dutch, "op alle doelvariabelen"),
        subtitle = "Grijze punten = waarnemingen, zwarte lijn = ALE (geschaald), rode lijn = mediaan (ALE=0)",
        x = var_name_dutch,
        y = "Doelvariabele"
      ) +
      theme_minimal(base_size = 11)

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

### combine ale per target in 1 plot met echte data en mediaan als referentie (ALE=0) + labels min/max ALE effect-----------------
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
  ggsave(outfile, panel, width = 35, height = 12, units = "cm", dpi = 200)
  cat("Opgeslagen:", outfile, "
")
}

## Kantelpunten detectie in ALE-curves --------------------------------------------------------

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

## Loop over alle targets: ALE plots met kantelpunten -----------------------------------------

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
  ggsave(outfile, panel, width = 35, height = 12, units = "cm", dpi = 200)
  cat("Opgeslagen:", outfile, "\n")
}

## Overzichtstabel alle kantelpunten ---------------------------------------------------
tipping_points_summary <- rbindlist(tipping_records, fill = TRUE)
print(tipping_points_summary)

## Functie voor XGBoost model diagnostiek en validatie ------------------------------------------
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



# 4. redox XGBOOST----------------------------------------------------------------------------------------
## versie redox en poriewaterconcentraties erbij-----------------------------------

target_vars <- c("slib_redox_pH7")
# Selecteer alleen poriewater µmol concentraties
# Selecteer alle kolommen die µmol bevatten
cols_umol_pw <- colnames(abio_proj)[grepl('µmol/l_PW', colnames(abio_proj), fixed = TRUE)]
# Verwijder specifieke kolommen
cols_umol_pw <- cols_umol_pw[!cols_umol_pw %in% c("Cl_2_µmol/l_PW", "Na_2_µmol/l_PW", "K_2_µmol/l_PW")]
# Update cols_corr met de gefilterde kolommen
cols_corr <- c(cols_umol_pw, "P-AL mg p2o5/100g_SB", "pH_CC_SB", "bulk density_kg DW/L FW_SB")

# Update nederlandse_namen mapping
nederlandse_namen <- c(
  setNames(paste0(cols_umol_pw, " (µmol/l)"), cols_umol_pw),
  "P-AL mg p2o5/100g_SB" = "P-AL slib (mg P2O5/100g)",
  "pH_CC_SB" = "pH slib",
  "bulk density_kg DW/L FW_SB" = "Bulk density (kg DW/L FW)"
)
# Update target_names_dutch mapping
target_names_dutch <- c(
  target_names_dutch,
  "slib_redox_pH7" = "Redox slib bij pH7 (mV)"
)

## ALE plots voor redox modellen -------------------------------------------------------

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



