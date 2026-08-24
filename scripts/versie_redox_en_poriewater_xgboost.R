# XGBoost modellen - versie redox en poriewater
# Doelen: redox slib (pH7), fosfor poriewater, en gerelateerde variabelen
# Predictoren: uitgebreide set incl. poriewaterchemie en bodemparameters
# Gebaseerd op create_xgb_model() structuur uit Analyses_dev_db.R
# -----------------------------------------------------------------------

library(data.table)
library(xgboost)

# Hernoem kolommen met speciale tekens (µ, /, spaties) in abio_proj.
# R's interne naam-normalisatie bij matrix/data.frame conversie vervangt
# deze tekens inconsistent, wat leidt tot type-corruptie en XGBoost
# pointer-uitlijnfouten. Schone namen voorkomen dit volledig.
clean_colname <- function(x) {
  x <- gsub("µ", "u", x, fixed = TRUE)
  x <- gsub("%", "_perc", x, fixed = TRUE)
  x <- gsub("/", "_per_", x, fixed = TRUE)
  x <- gsub(" ", "_", x, fixed = TRUE)
  x <- gsub("+", "plus", x, fixed = TRUE)
  x <- gsub("-", "_", x, fixed = TRUE)
  x <- gsub("_+", "_", x)  # meerdere underscores samenvoegen
  x
}

old_names <- colnames(abio_proj)
new_names <- clean_colname(old_names)
changed   <- old_names != new_names
if (any(changed)) {
  setnames(abio_proj, old = old_names[changed], new = new_names[changed])
}


# 1. Target variabelen ---------------------------------------------------
target_vars <- c(
  "slib_redox_pH7"
)

target_names_dutch <- c(
  "slib_redox_pH7"           = "Redox slib bij pH7 (mV)"
)

# 2. Predictorvariabelen -------------------------------------------------
# Selecteer alle poriewater- en slibvloeistofkolommen (na hernoeming)
cols_umol_pw <- colnames(abio_proj)[
  grepl('umol_per_l_PW', colnames(abio_proj), fixed = TRUE) |
  grepl('mmol_per_kg_DW', colnames(abio_proj), fixed = TRUE)
]
cols_umol_pw <- cols_umol_pw[!cols_umol_pw %in% c(
  "Cl_2_umol_per_l_PW", "Na_2_umol_per_l_PW", "K_2_umol_per_l_PW", "DOC_umol_per_l_PW", "DON_umol_per_l_PW"
)]

cols_corr <- c(
  cols_umol_pw,
  "OS_gehalte_perc_SB",
  "P_AL_mg_p2o5_per_100g_SB",
  "pH_CC_SB",
  "bulk_density_kg_DW_per_L_FW_SB"
)

nederlandse_namen <- c(
  "Al_mmol_per_kg_DW_SB"            = "Al (mmol/kg drooggewicht SB)",
  "Ca_mmol_per_kg_DW_SB"            = "Ca (mmol/kg drooggewicht SB)",
  "Cl_mmol_per_kg_DW_SB"            = "Cl (mmol/kg drooggewicht SB)",
  "Fe_mmol_per_kg_DW_SB"            = "Fe (mmol/kg drooggewicht SB)",
  "K_mmol_per_kg_DW_SB"             = "K (mmol/kg drooggewicht SB)",
  "Mg_mmol_per_kg_DW_SB"            = "Mg (mmol/kg drooggewicht SB)",
  "Mn_mmol_per_kg_DW_SB"            = "Mn (mmol/kg drooggewicht SB)",
  "Na_mmol_per_kg_DW_SB"            = "Na (mmol/kg drooggewicht SB)",
  "P_mmol_per_kg_DW_SB"             = "P (mmol/kg drooggewicht SB)",
  "S_mmol_per_kg_DW_SB"             = "S (mmol/kg drooggewicht SB)",
  "Si_mmol_per_kg_DW_SB"            = "Si (mmol/kg drooggewicht SB)",
  "Zn_mmol_per_kg_DW_SB"            = "Zn (mmol/kg drooggewicht SB)",
  "TIC_conc_umol_per_l_PW"         = "TIC (umol/l poriewater)",
  "CO2_umol_per_l_PW"              = "CO2 (umol/l poriewater)",
  "HCO3_umol_per_l_PW"             = "HCO3 (umol/l poriewater)",
  "NO3_umol_per_l_PW"              = "NO3 (umol/l poriewater)",
  "NH4_umol_per_l_PW"              = "NH4 (umol/l poriewater)",
  "Na_umol_per_l_PW"               = "Na (umol/l poriewater)",
  "K_umol_per_l_PW"                = "K (umol/l poriewater)",
  "Ca_umol_per_l_PW"               = "Ca (umol/l poriewater)",
  "Mg_umol_per_l_PW"               = "Mg (umol/l poriewater)",
  "Cl_umol_per_l_PW"               = "Cl (umol/l poriewater)",
  "Al_umol_per_l_PW"               = "Al (umol/l poriewater)",
  "Fe_umol_per_l_PW"               = "Fe (umol/l poriewater)",
  "Mn_umol_per_l_PW"               = "Mn (umol/l poriewater)",
  "Si_umol_per_l_PW"               = "Si (umol/l poriewater)",
  "P_umol_per_l_PW"                = "P (umol/l poriewater)",
  "S_umol_per_l_PW"                = "S (umol/l poriewater)",
  "Zn_umol_per_l_PW"               = "Zn (umol/l poriewater)",
  "DOC_umol_per_l_PW"              = "DOC (umol/l poriewater)",
  "DON_umol_per_l_PW"              = "DON (umol/l poriewater)",
  "OS_gehalte_perc_SB"                = "Organische stofgehalte (%) slib",
  "P_AL_mg_p2o5_per_100g_SB"       = "P-AL slib (mg P2O5/100g)",
  "pH_CC_SB"                       = "pH slib",
  "bulk_density_kg_DW_per_L_FW_SB" = "Bulk density (kg DW/L versgewicht SB)",
  "OS_gehalte__perc_SB"            = "OS gehalte (%) slib"
)




# 3. Model functie -------------------------------------------------------
create_xgb_model <- function(target_var, predictors, data,
                             train_frac       = 0.6,
                             val_frac         = 0.2,
                             max_depth        = 4,
                             eta              = 0.05,
                             nrounds          = 500,
                             early_stopping   = 20,
                             subsample        = 0.8,
                             colsample_bytree = 0.8,
                             min_child_weight = 5,
                             gamma            = 0.1,
                             lambda           = 2) {

  # target_var <- target
  # data <- abio_proj
  model_vars <- c("SlootID", target_var, predictors)
  model_data <- copy(data[complete.cases(data[, ..model_vars]), ..model_vars])

  # Converteer niet-double kolommen naar double
  non_dbl <- names(model_data)[names(model_data) != "SlootID" &
               sapply(model_data, function(x) !is.double(x))]
  for (.col in non_dbl) {
    v <- model_data[[.col]]
    set(model_data, j = .col,
        value = if (is.character(v) || is.factor(v)) as.double(as.factor(v))
                else as.double(v))
  }

  predictors_clean <- predictors[!predictors %in% target_var]

  set.seed(123)
  n         <- nrow(model_data)
  idx       <- sample(n)
  train_end <- floor(train_frac * n)
  val_end   <- floor((train_frac + val_frac) * n)

  train_idx <- idx[1:train_end]
  val_idx   <- idx[(train_end + 1):val_end]
  test_idx  <- idx[(val_end + 1):n]

  # Hulpfunctie: bouw matrix. Kolomnamen zijn schoon (geen µ, /, spaties)
  # dus as.data.frame() normaliseert namen niet, type blijft double.
  make_xgb_matrix <- function(dt, rows, cols) {
    as.matrix(as.data.frame(lapply(as.data.frame(dt[rows, ..cols]), as.double)))
  }

  X_train <- make_xgb_matrix(model_data, train_idx, predictors_clean)
  y_train <- as.double(model_data[[target_var]][train_idx])

  X_val   <- make_xgb_matrix(model_data, val_idx,   predictors_clean)
  y_val   <- as.double(model_data[[target_var]][val_idx])

  X_test  <- make_xgb_matrix(model_data, test_idx,  predictors_clean)
  y_test  <- as.double(model_data[[target_var]][test_idx])

  dtrain <- xgb.DMatrix(data = X_train, label = y_train)
  dval   <- xgb.DMatrix(data = X_val,   label = y_val)
  dtest  <- xgb.DMatrix(data = X_test,  label = y_test)

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

  xgb_model <- xgb.train(
    params                = params,
    data                  = dtrain,
    nrounds               = nrounds,
    watchlist             = list(train = dtrain, val = dval),
    early_stopping_rounds = early_stopping,
    verbose               = 2
  )

  importance <- xgb.importance(feature_names = colnames(X_train), model = xgb_model)
  importance[, Nederlandse_naam := nederlandse_namen[Feature]]

  pred_train <- predict(xgb_model, X_train)
  pred_val   <- predict(xgb_model, X_val)
  pred_test  <- predict(xgb_model, X_test)

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

# 4. Train modellen ------------------------------------------------------
xgb_models_rdx      <- list()
model_performance_rdx   <- list()
feature_importance_rdx  <- list()

for (target in target_vars) {
  if (!target %in% colnames(abio_proj)) {
    cat("Overgeslagen (kolom niet aanwezig):", target, "\n")
    next
  }
  cat("Training model voor:", target, "\n")

  predictors <- cols_corr[!cols_corr %in% target & cols_corr %in% colnames(abio_proj)]

  model_result <- create_xgb_model(target, predictors, abio_proj)

  xgb_models_rdx[[target]]     <- model_result$model
  model_performance_rdx[[target]]  <- model_result$performance
  feature_importance_rdx[[target]] <- model_result$importance[
    , .(Feature, Nederlandse_naam, Gain)][order(-Gain)][1:10]
  feature_importance_rdx[[target]][, target_var := target]
}

# 5. Resultaten ----------------------------------------------------------
performance_summary_rdx <- rbindlist(model_performance_rdx)
print(performance_summary_rdx)

# Feature importance top-10 per target
importance_all_rdx <- rbindlist(feature_importance_rdx)

library(ggplot2)
library(patchwork)

# Voeg Nederlandse namen en performantie toe aan importance tabel
importance_all_rdx[, Nederlandse_naam := ifelse(is.na(Nederlandse_naam), Feature, Nederlandse_naam)]
importance_all_rdx[, target_dutch := target_names_dutch[target_var]]
importance_all_rdx <- merge(
  importance_all_rdx,
  performance_summary_rdx[, .(target, rmse_test, r2_test)],
  by.x = "target_var", by.y = "target", all.x = TRUE
)

rmse_units_rdx <- c(
  "slib_redox_pH7" = "mV"
)
importance_all_rdx[, rmse_unit := rmse_units_rdx[target_var]]

# Correlatierichting berekenen (Pearson)
importance_all_rdx[, correlation_direction := mapply(
  function(tvar, feat) {
    tryCatch({
      if (!tvar %in% colnames(abio_proj) || !feat %in% colnames(abio_proj)) return(NA_character_)
      corr <- cor(abio_proj[[tvar]], abio_proj[[feat]], use = "complete.obs")
      ifelse(corr > 0, "+", "-")
    }, error = function(e) NA_character_)
  },
  tvar = target_var,
  feat = Feature,
  USE.NAMES = FALSE
)]

# Plot titels met R² en RMSE
importance_all_rdx[, plot_title_clean := paste0(
  target_dutch, "\nR²: ", round(r2_test * 100, 1),
  "% | RMSE: ", round(rmse_test, 3), " ", rmse_unit
)]

# 5a. VIP plot: Gain met correlatierichting als kleur -------------------------
okabe_dir <- c("+" = "#0072B2", "-" = "#D55E00")

plot_data_rdx <- importance_all_rdx[!is.na(correlation_direction)][
  order(target_var, Gain)
][, facet_label := paste0(target_var, "__", Nederlandse_naam)
][, facet_label := factor(facet_label, levels = unique(facet_label))]

p_vip_rdx <- ggplot(plot_data_rdx, aes(
    x    = facet_label,
    y    = Gain,
    fill = correlation_direction
  )) +
  geom_col() +
  geom_text(
    aes(label = correlation_direction),
    hjust = -0.2, size = 3.5, fontface = "bold", color = "grey20"
  ) +
  facet_wrap(~plot_title_clean, scales = "free", ncol = 2) +
  scale_x_discrete(labels = function(x) sub(".*__", "", x)) +
  coord_flip() +
  scale_fill_manual(
    values = okabe_dir,
    labels = c("+" = "Positief verband", "-" = "Negatief verband"),
    name   = "Correlatierichting"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title    = "Belangrijkste verklarende variabelen redox (XGBoost)",
    subtitle = "Variable Importance (Gain) met correlatierichting op basis van Pearson correlatie",
    x = NULL, y = "Informatiewinst (Gain)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.y        = element_text(size = 14),
    strip.text         = element_text(size = 14, face = "bold", lineheight = 1.1),
    strip.background   = element_rect(fill = "grey95", colour = "grey70", linewidth = 0.6),
    panel.border       = element_rect(colour = "grey80", fill = NA, linewidth = 0.5),
    panel.grid.major.y = element_blank(),
    legend.position    = "bottom",
    plot.title         = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle      = element_text(size = 9.5, hjust = 0.5, color = "grey40")
  )

print(p_vip_rdx)
ggsave(
  "output/AlleGebieden/Tussenrapportage/Redox_VIP_gain_okabe.png",
  plot = p_vip_rdx, width = 30, height = 20, units = "cm", dpi = 300
)

# 5b. Permutation importance -------------------------------------------------
calculate_permutation_importance <- function(model, X_val, y_val, n_rep = 5) {
  baseline_pred <- predict(model, X_val)
  baseline_rmse <- sqrt(mean((y_val - baseline_pred)^2))
  baseline_r2   <- cor(y_val, baseline_pred)^2

  results <- rbindlist(lapply(seq_len(ncol(X_val)), function(j) {
    feat <- colnames(X_val)[j]
    reps <- vapply(seq_len(n_rep), function(r) {
      X_perm      <- X_val
      X_perm[, j] <- sample(X_perm[, j])
      pred_perm   <- predict(model, X_perm)
      rmse_perm   <- sqrt(mean((y_val - pred_perm)^2))
      r2_perm     <- tryCatch(cor(y_val, pred_perm)^2, error = function(e) NA_real_)
      c(rmse_perm, r2_perm)
    }, numeric(2))
    data.table(
      Feature    = feat,
      delta_rmse = mean(reps[1, ], na.rm = TRUE) - baseline_rmse,
      delta_r2   = baseline_r2 - mean(reps[2, ], na.rm = TRUE)
    )
  }))
  results[order(-delta_rmse)]
}

perm_importance_rdx <- list()

for (target in names(xgb_models_rdx)) {
  cat("Permutation importance voor:", target, "\n")

  model_vars    <- c("SlootID", target, predictors)
  model_vars    <- model_vars[model_vars %in% colnames(abio_proj)]
  model_data_pi <- copy(abio_proj[complete.cases(abio_proj[, ..model_vars]), ..model_vars])

  non_dbl_pi <- names(model_data_pi)[names(model_data_pi) != "SlootID" &
                  sapply(model_data_pi, function(x) !is.double(x))]
  for (.col in non_dbl_pi) {
    v <- model_data_pi[[.col]]
    set(model_data_pi, j = .col,
        value = if (is.character(v) || is.factor(v)) as.double(as.factor(v)) else as.double(v))
  }

  predictors_pi <- colnames(model_data_pi)[!colnames(model_data_pi) %in% c("SlootID", target)]

  set.seed(123)
  n         <- nrow(model_data_pi)
  idx       <- sample(n)
  train_end <- floor(0.6 * n)
  val_end   <- floor(0.8 * n)
  val_idx   <- idx[(train_end + 1):val_end]

  X_val_pi <- as.matrix(as.data.frame(lapply(
    as.data.frame(model_data_pi[val_idx, ..predictors_pi]), as.double)))
  y_val_pi <- as.double(model_data_pi[[target]][val_idx])

  pi_dt <- calculate_permutation_importance(xgb_models_rdx[[target]], X_val_pi, y_val_pi)
  pi_dt[, Nederlandse_naam := nederlandse_namen[Feature]]
  pi_dt[is.na(Nederlandse_naam), Nederlandse_naam := Feature]
  pi_dt[, target_var := target]

  perm_importance_rdx[[target]] <- pi_dt
}

all_perm_importance_rdx <- rbindlist(perm_importance_rdx)

# 5c. VIP vergelijkingsplot: Gain vs Permutation -----------------------------
perm_top10_rdx <- all_perm_importance_rdx[
  , .SD[order(-delta_rmse)][1:min(.N, 10)], by = target_var
][, .(target_var, Feature, Nederlandse_naam, delta_rmse)]

gain_top10_rdx <- importance_all_rdx[
  , .SD[order(-Gain)][1:min(.N, 10)], by = target_var
][, .(target_var, Feature, Nederlandse_naam, Gain)]

all_feat_rdx <- unique(rbind(
  perm_top10_rdx[, .(target_var, Feature, Nederlandse_naam)],
  gain_top10_rdx[, .(target_var, Feature, Nederlandse_naam)]
))
all_feat_rdx <- merge(all_feat_rdx, perm_top10_rdx[, .(target_var, Feature, delta_rmse)],
                      by = c("target_var", "Feature"), all.x = TRUE)
all_feat_rdx <- merge(all_feat_rdx, gain_top10_rdx[, .(target_var, Feature, Gain)],
                      by = c("target_var", "Feature"), all.x = TRUE)
all_feat_rdx[is.na(delta_rmse), delta_rmse := 0]
all_feat_rdx[is.na(Gain),       Gain       := 0]

all_feat_rdx[, delta_rmse_norm := delta_rmse / max(delta_rmse, na.rm = TRUE), by = target_var]
all_feat_rdx[, gain_norm       := Gain       / max(Gain,       na.rm = TRUE), by = target_var]
all_feat_rdx[, mean_belang     := (delta_rmse_norm + gain_norm) / 2]
all_feat_rdx[, target_dutch    := target_names_dutch[target_var]]
all_feat_rdx <- merge(
  all_feat_rdx,
  performance_summary_rdx[, .(target, rmse_val, r2_val)],
  by.x = "target_var", by.y = "target", all.x = TRUE
)
all_feat_rdx[, rmse_unit   := rmse_units_rdx[target_var]]
all_feat_rdx[, plot_title  := paste0(
  target_dutch, "\nR²(val): ", round(r2_val * 100, 1),
  "% | RMSE(val): ", round(rmse_val, 3), " ", rmse_unit
)]

feat_order_rdx <- all_feat_rdx[order(plot_title, mean_belang),
                                paste0(plot_title, "__", Nederlandse_naam)]

plot_vip_cmp_rdx <- melt(
  all_feat_rdx[, .(Feature, Nederlandse_naam, plot_title, gain_norm, delta_rmse_norm)],
  id.vars       = c("Feature", "Nederlandse_naam", "plot_title"),
  variable.name = "methode",
  value.name    = "belang_norm"
)[, methode := fifelse(methode == "gain_norm", "XGBoost Gain", "Permutation (ΔRMSE)")]

plot_vip_cmp_rdx[, feat_label := factor(
  paste0(plot_title, "__", Nederlandse_naam),
  levels = unique(feat_order_rdx)
)]

p_vip_compare_rdx <- ggplot(plot_vip_cmp_rdx, aes(
    x    = feat_label,
    y    = belang_norm,
    fill = methode
  )) +
  geom_col(position = "dodge") +
  facet_wrap(~plot_title, scales = "free_y", ncol = 2) +
  scale_x_discrete(labels = function(x) sub(".*__", "", x)) +
  coord_flip() +
  scale_fill_manual(
    values = c("XGBoost Gain" = "#0072B2", "Permutation (ΔRMSE)" = "#E69F00"),
    name   = "Methode"
  ) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    title    = "VIP vergelijking: XGBoost Gain vs. Permutation Importance (redox)",
    subtitle = "Genormaliseerd binnen target (1 = hoogste belang). Permutation op validatieset.",
    x        = NULL,
    y        = "Relatief belang (genormaliseerd)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text         = element_text(size = 14, face = "bold", lineheight = 1.1),
    strip.background   = element_rect(fill = "grey95", colour = "grey70", linewidth = 0.6),
    panel.border       = element_rect(colour = "grey80", fill = NA, linewidth = 0.5),
    panel.grid.major.y = element_blank(),
    axis.text.y        = element_text(size = 14),
    legend.position    = "bottom",
    plot.title         = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle      = element_text(size = 14, hjust = 0.5, color = "grey40")
  )

print(p_vip_compare_rdx)
ggsave(
  "output/AlleGebieden/Tussenrapportage/Redox_VIP_gain_vs_permutation.png",
  plot = p_vip_compare_rdx, width = 30, height = 20, units = "cm", dpi = 300
)

# 5d. ALE hulpfuncties -------------------------------------------------------
rescale_to <- function(x, to_min, to_max) {
  xr <- range(x, na.rm = TRUE)
  if (!all(is.finite(xr)) || diff(xr) == 0) return(rep((to_min + to_max) / 2, length(x)))
  (x - xr[1]) / diff(xr) * (to_max - to_min) + to_min
}

calculate_ale_manual <- function(model, X_data, feature_idx, K = 50) {
  feature_values <- suppressWarnings(as.numeric(X_data[, feature_idx]))
  feature_name   <- colnames(X_data)[feature_idx]
  ok             <- is.finite(feature_values)
  fv_ok          <- feature_values[ok]
  if (length(fv_ok) < 2L || length(unique(fv_ok)) < 2L)
    return(list(x_values = numeric(0), ale_effects = numeric(0), feature_name = feature_name))
  quants <- unique(as.numeric(
    quantile(fv_ok, probs = seq(0, 1, length.out = K + 1), na.rm = TRUE, names = FALSE)
  ))
  if (length(quants) < 2L)
    return(list(x_values = numeric(0), ale_effects = numeric(0), feature_name = feature_name))
  ale_vals <- rep(NA_real_, length(quants) - 1L)
  x_vals   <- rep(NA_real_, length(quants) - 1L)
  for (i in seq_len(length(quants) - 1L)) {
    in_int <- ok & feature_values >= quants[i] & feature_values <= quants[i + 1]
    if (sum(in_int) > 0L) {
      Xl <- X_data[in_int, , drop = FALSE]; Xh <- Xl
      Xl[, feature_idx] <- quants[i];       Xh[, feature_idx] <- quants[i + 1]
      ale_vals[i] <- mean(predict(model, Xh) - predict(model, Xl), na.rm = TRUE)
      x_vals[i]   <- (quants[i] + quants[i + 1]) / 2
    }
  }
  valid <- is.finite(ale_vals) & is.finite(x_vals)
  if (!any(valid))
    return(list(x_values = numeric(0), ale_effects = numeric(0), feature_name = feature_name))
  ale_cum <- cumsum(ale_vals[valid])
  list(
    x_values    = x_vals[valid],
    ale_effects = ale_cum - mean(ale_cum, na.rm = TRUE),
    feature_name = feature_name
  )
}

create_ale_plots_manual <- function(model, X_data, y_data, target_name) {
  ale_plots <- list()
  for (feature in colnames(X_data)) {
    fidx     <- which(colnames(X_data) == feature)
    ale_res  <- calculate_ale_manual(model, X_data, fidx, K = 30)
    if (length(ale_res$x_values) == 0L) next
    ale_df   <- data.frame(x = ale_res$x_values, ale_effect = ale_res$ale_effects)
    real_df  <- data.frame(x = X_data[, fidx], y = y_data)
    real_df  <- real_df[is.finite(real_df$x) & is.finite(real_df$y), ]
    ale_rng  <- range(ale_df$ale_effect, na.rm = TRUE)
    y_rng    <- range(real_df$y, na.rm = TRUE)
    real_df$y_bg <- rescale_to(real_df$y, ale_rng[1], ale_rng[2])
    sec_trans <- if (diff(ale_rng) > 0) {
      sf <- diff(y_rng) / diff(ale_rng)
      list(trans = ~ y_rng[1] + (. - ale_rng[1]) * sf,
           inv   = ~ ale_rng[1] + (. - y_rng[1]) / sf)
    } else NULL
    feat_nl   <- nederlandse_namen[feature]; if (is.na(feat_nl)) feat_nl <- feature
    tgt_nl    <- target_names_dutch[target_name]; if (is.na(tgt_nl)) tgt_nl <- target_name
    eff_range <- diff(range(ale_df$ale_effect, na.rm = TRUE))
    p <- ggplot() +
      geom_point(data = real_df, aes(x = x, y = y_bg),
                 color = "grey70", alpha = 0.35, size = 1.2) +
      geom_line(data = ale_df, aes(x = x, y = ale_effect),
                color = "#1f77b4", linewidth = 1) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
      labs(title = feat_nl, subtitle = paste0("Effect range: ", round(eff_range, 3)),
           x = feat_nl, y = paste("ALE effect op", tgt_nl)) +
      theme_minimal()
    if (!is.null(sec_trans)) {
      p <- p + scale_y_continuous(
        sec.axis = sec_axis(trans = sec_trans$trans, name = tgt_nl,
                            labels = scales::label_number(accuracy = 0.1))
      ) + theme(axis.title.y.right = element_text(color = "grey50", size = 8),
                axis.text.y.right  = element_text(color = "grey50", size = 7))
    }
    ale_plots[[feature]] <- p
  }
  ale_plots
}

detect_tipping_points <- function(x, y, min_effect_range = 0.01, min_jump_pct = 0.15) {
  eff_range <- diff(range(y, na.rm = TRUE))
  if (eff_range < min_effect_range || length(x) < 4) return(NULL)
  dy <- diff(y); dx <- diff(x)
  sign_ch <- which(diff(sign(dy)) != 0) + 1
  min_jump_abs <- min_jump_pct * eff_range
  sign_ch <- sign_ch[vapply(sign_ch, function(i) {
    left  <- if (i > 1)           abs(dy[i - 1]) else 0
    right <- if (i <= length(dy)) abs(dy[i])     else 0
    max(left, right) >= min_jump_abs
  }, logical(1))]
  steepest_idx <- which.max(abs(dy / dx)) + 1
  local_extrema <- if (length(sign_ch)) {
    data.frame(x = x[sign_ch], y = y[sign_ch], type = "Lokaal extremum")
  } else NULL
  list(
    local_extrema = local_extrema,
    steepest      = data.frame(x = x[steepest_idx], y = y[steepest_idx], type = "Steilste helling")
  )
}

# 5e. ALE plots per target ---------------------------------------------------
all_ale_plots_rdx <- list()

for (target in names(xgb_models_rdx)) {
  cat("ALE plots voor:", target, "\n")
  model_vars_ale <- c("SlootID", target, cols_corr)
  model_vars_ale <- model_vars_ale[model_vars_ale %in% colnames(abio_proj)]
  md_ale <- copy(abio_proj[complete.cases(abio_proj[, ..model_vars_ale]), ..model_vars_ale])

  non_dbl_ale <- names(md_ale)[names(md_ale) != "SlootID" &
                   sapply(md_ale, function(x) !is.double(x))]
  for (.col in non_dbl_ale) {
    v <- md_ale[[.col]]
    set(md_ale, j = .col,
        value = if (is.character(v) || is.factor(v)) as.double(as.factor(v)) else as.double(v))
  }
  preds_ale <- colnames(md_ale)[!colnames(md_ale) %in% c("SlootID", target)]
  X_ale <- as.matrix(as.data.frame(lapply(as.data.frame(md_ale[, ..preds_ale]), as.double)))
  y_ale <- as.double(md_ale[[target]])
  if (length(y_ale) == 0L || nrow(X_ale) != length(y_ale)) next
  all_ale_plots_rdx[[target]] <- create_ale_plots_manual(xgb_models_rdx[[target]], X_ale, y_ale, target)
}

# ALE top-5 combinatieplots per target
top_n_ale <- 5
for (tgt in names(all_ale_plots_rdx)) {
  top_feats <- importance_all_rdx[target_var == tgt][order(-Gain)][1:min(.N, top_n_ale), Feature]
  top_feats <- top_feats[top_feats %in% names(all_ale_plots_rdx[[tgt]])]
  if (length(top_feats) == 0) next
  plots_ale <- lapply(top_feats, function(feat) {
    dutch_name <- importance_all_rdx[target_var == tgt & Feature == feat, Nederlandse_naam]
    gain_val   <- importance_all_rdx[target_var == tgt & Feature == feat, round(Gain, 3)]
    all_ale_plots_rdx[[tgt]][[feat]] +
      labs(title = paste0(dutch_name, "\n(Gain: ", gain_val, ")"),
           subtitle = NULL, x = NULL, y = "ALE effect") +
      theme_minimal(base_size = 9) +
      theme(plot.title   = element_text(size = 8, face = "bold", hjust = 0.5),
            axis.text    = element_text(size = 7),
            panel.border = element_rect(colour = "grey80", fill = NA, linewidth = 0.4),
            plot.margin  = margin(4, 8, 4, 8))
  })
  title_str <- importance_all_rdx[target_var == tgt, target_dutch[1]]
  perf_str  <- importance_all_rdx[target_var == tgt,
    paste0("R²: ", round(r2_test[1]*100,1), "%  |  RMSE: ", round(rmse_test[1],3), " ", rmse_unit[1])]
  panel_ale <- wrap_plots(plots_ale, nrow = 1) +
    plot_annotation(
      title = paste0("ALE plots — ", title_str), subtitle = perf_str,
      theme = theme(plot.title    = element_text(size = 11, face = "bold", hjust = 0.5),
                    plot.subtitle = element_text(size = 9, color = "grey40", hjust = 0.5))
    )
  print(panel_ale)
  ggsave(paste0("output/AlleGebieden/Tussenrapportage/Redox_ALE_top5_", tgt, ".png"),
         panel_ale, width = 35, height = 12, units = "cm", dpi = 200)
}

# 5f. ALE met kantelpunten ---------------------------------------------------
tipping_records_rdx <- list()

for (tgt in names(all_ale_plots_rdx)) {
  top_feats <- importance_all_rdx[target_var == tgt][order(-Gain)][1:min(.N, 5), Feature]
  top_feats <- top_feats[top_feats %in% names(all_ale_plots_rdx[[tgt]])]
  if (length(top_feats) == 0) next

  plots_tp <- lapply(top_feats, function(feat) {
    ale_df <- layer_data(all_ale_plots_rdx[[tgt]][[feat]], 2)[, c("x", "y")]
    names(ale_df) <- c("x", "ale_effect")
    tp <- detect_tipping_points(ale_df$x, ale_df$ale_effect)

    if (!is.null(tp)) {
      tp_all <- rbind(if (!is.null(tp$local_extrema)) tp$local_extrema else NULL, tp$steepest)
      if (!is.null(tp_all) && nrow(tp_all) > 0)
        tipping_records_rdx[[length(tipping_records_rdx) + 1]] <<- data.table(
          target = tgt, target_nl = target_names_dutch[tgt],
          predictor = feat, pred_nl = nederlandse_namen[feat],
          type = tp_all$type, x_waarde = round(tp_all$x, 4), ale_effect = round(tp_all$y, 4)
        )
    }

    dutch_name <- importance_all_rdx[target_var == tgt & Feature == feat, Nederlandse_naam][1]
    gain_val   <- importance_all_rdx[target_var == tgt & Feature == feat, round(Gain, 3)][1]

    p <- all_ale_plots_rdx[[tgt]][[feat]] +
      labs(title = paste0(dutch_name, "\n(Gain: ", gain_val, ")"),
           subtitle = NULL, x = NULL, y = "ALE effect") +
      theme_minimal(base_size = 9) +
      theme(plot.title   = element_text(size = 15, face = "bold", hjust = 0.5),
            axis.text    = element_text(size = 14),
            panel.border = element_rect(colour = "grey80", fill = NA, linewidth = 0.4),
            plot.margin  = margin(4, 8, 4, 8))

    if (!is.null(tp)) {
      p <- p +
        geom_vline(xintercept = tp$steepest$x, linetype = "dashed",
                   color = "#CC79A7", linewidth = 0.7, alpha = 0.9) +
        annotate("label", x = tp$steepest$x, y = Inf,
                 label = round(tp$steepest$x, 2), vjust = 1.3, size = 2.5,
                 fontface = "bold", color = "#CC79A7", fill = "white",
                 label.padding = unit(0.15, "lines"))
      if (!is.null(tp$local_extrema) && nrow(tp$local_extrema) > 0)
        p <- p +
          geom_point(data = tp$local_extrema, aes(x = x, y = y),
                     color = "red", size = 2.5, shape = 16) +
          geom_label(data = tp$local_extrema, aes(x = x, y = y, label = round(x, 2)),
                     vjust = -0.6, size = 2.2, color = "red", fill = "white",
                     label.padding = unit(0.12, "lines"))
    }
    p
  })

  title_str <- importance_all_rdx[target_var == tgt, target_dutch[1]]
  perf_str  <- importance_all_rdx[target_var == tgt,
    paste0("R²: ", round(r2_test[1]*100,1), "%  |  RMSE: ", round(rmse_test[1],3), " ", rmse_unit[1])]

  panel_tp <- wrap_plots(plots_tp, nrow = 1) +
    plot_annotation(
      title = paste0("ALE + Kantelpunten — ", title_str), subtitle = perf_str,
      theme = theme(plot.title    = element_text(size = 15, face = "bold", hjust = 0.5),
                    plot.subtitle = element_text(size = 14, color = "grey40", hjust = 0.5))
    )
  print(panel_tp)
  ggsave(paste0("output/AlleGebieden/Tussenrapportage/Redox_ALE_tipping_", tgt, ".png"),
         panel_tp, width = 35, height = 12, units = "cm", dpi = 200)
}

tipping_points_rdx <- rbindlist(tipping_records_rdx, fill = TRUE)
print(tipping_points_rdx)

# 5g. Diagnostische plots ----------------------------------------------------
for (target in names(xgb_models_rdx)) {
  cat("Diagnostics voor:", target, "\n")
  model_vars_d <- c("SlootID", target, cols_corr)
  model_vars_d <- model_vars_d[model_vars_d %in% colnames(abio_proj)]
  md_d <- copy(abio_proj[complete.cases(abio_proj[, ..model_vars_d]), ..model_vars_d])

  non_dbl_d <- names(md_d)[names(md_d) != "SlootID" &
                 sapply(md_d, function(x) !is.double(x))]
  for (.col in non_dbl_d) {
    v <- md_d[[.col]]
    set(md_d, j = .col,
        value = if (is.character(v) || is.factor(v)) as.double(as.factor(v)) else as.double(v))
  }
  preds_d <- colnames(md_d)[!colnames(md_d) %in% c("SlootID", target)]
  X_d     <- as.matrix(as.data.frame(lapply(as.data.frame(md_d[, ..preds_d]), as.double)))
  y_act   <- as.double(md_d[[target]])
  y_pred  <- predict(xgb_models_rdx[[target]], X_d)
  resid   <- y_act - y_pred

  diag_dt <- data.table(
    actual   = y_act,
    predicted = y_pred,
    residuals = resid,
    std_resid = resid / sd(resid, na.rm = TRUE)
  )
  rmse_d    <- sqrt(mean(resid^2, na.rm = TRUE))
  mae_d     <- mean(abs(resid), na.rm = TRUE)
  r2_d      <- cor(y_act, y_pred, use = "complete.obs")^2
  skew_d    <- mean(resid^3, na.rm = TRUE) / sd(resid, na.rm = TRUE)^3
  tgt_nl    <- target_names_dutch[target]

  pd1 <- ggplot(diag_dt, aes(x = actual, y = predicted)) +
    geom_point(alpha = 0.6, size = 2.5, color = "#0072B2") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 1) +
    labs(title = paste0("Gemeten vs Voorspeld: ", tgt_nl),
         subtitle = paste0("RMSE: ", round(rmse_d,2), " | MAE: ", round(mae_d,2), " | R²: ", round(r2_d,3)),
         x = paste("Gemeten", tgt_nl), y = paste("Voorspeld", tgt_nl)) +
    theme_minimal() +
    theme(plot.title = element_text(size=15, face="bold"),
          panel.border = element_rect(colour="black", fill=NA, linewidth=0.8))

  pd2 <- ggplot(diag_dt, aes(x = predicted, y = residuals)) +
    geom_point(alpha = 0.6, size = 2.5, color = "#009E73") +
    geom_hline(yintercept = 0, linetype="dashed", color="red", linewidth=1) +
    geom_hline(yintercept = c(-2*sd(resid,na.rm=TRUE), 2*sd(resid,na.rm=TRUE)),
               linetype="dotted", color="orange", linewidth=0.8) +
    labs(title="Residuals vs Fitted Values", x="Voorspelde waarden", y="Residuals") +
    theme_minimal() +
    theme(plot.title = element_text(size=15, face="bold"),
          panel.border = element_rect(colour="black", fill=NA, linewidth=0.8))

  pd3 <- ggplot(diag_dt, aes(x = residuals)) +
    geom_histogram(aes(y = after_stat(density)), bins=15,
                   fill="#E69F00", alpha=0.7, color="black", linewidth=0.5) +
    stat_function(fun=dnorm,
                  args=list(mean=mean(resid,na.rm=TRUE), sd=sd(resid,na.rm=TRUE)),
                  color="red", linewidth=1.2, linetype="dashed") +
    labs(title="Verdeling van Residuals",
         subtitle=paste0("Skewness: ", round(skew_d,3)),
         x="Residuals", y="Density") +
    theme_minimal() +
    theme(plot.title = element_text(size=15, face="bold"),
          panel.border = element_rect(colour="black", fill=NA, linewidth=0.8))

  pd4 <- ggplot(diag_dt, aes(sample = std_resid)) +
    stat_qq(color="#56B4E9", size=2.5, alpha=0.7) +
    stat_qq_line(color="red", linewidth=1, linetype="dashed") +
    labs(title="Q-Q Plot (Normaalverdeling Check)",
         x="Theoretische Quantiles", y="Sample Quantiles") +
    theme_minimal() +
    theme(plot.title = element_text(size=15, face="bold"),
          panel.border = element_rect(colour="black", fill=NA, linewidth=0.8))

  pd5 <- ggplot(diag_dt, aes(x = residuals)) +
    geom_density(fill="#D55E00", alpha=0.6, color="black", linewidth=1) +
    stat_function(fun=dnorm,
                  args=list(mean=mean(resid,na.rm=TRUE), sd=sd(resid,na.rm=TRUE)),
                  color="blue", linewidth=1.2, linetype="dashed") +
    labs(title="Dichtheidsverdeling Residuals", x="Residuals", y="Density") +
    theme_minimal() +
    theme(plot.title = element_text(size=15, face="bold"),
          panel.border = element_rect(colour="black", fill=NA, linewidth=0.8))

  combined_diag <- (pd1 + pd2) / (pd3 + pd4) / pd5
  print(combined_diag)
  ggsave(
    paste0("output/AlleGebieden/Tussenrapportage/Redox_diagnostics_",
           gsub("[^A-Za-z0-9]", "_", target), ".png"),
    combined_diag, width = 40, height = 45, units = "cm", dpi = 300
  )
}


# Voeg veentype toe als beschikbaar

diag_dt[, SlootID := md_d$SlootID]
diag_dt[, gebied := sub("_.*", "", SlootID)]
if ("veentype" %in% colnames(abio_proj)) {
  veen2 <- unique(abio_proj[, .(SlootID, veentype)])
  diag_dt <- merge(diag_dt, veen2, by = "SlootID", all.x = TRUE)
}

# Top uitbijters
diag_dt[order(-abs(residuals))][1:20, .(SlootID, gebied, actual, predicted, residuals, veentype)]

# Verwijder duplicaten (meerdere jaren per SlootID)
diag_uniq <- diag_dt[, .(
  actual    = mean(actual,    na.rm = TRUE),
  predicted = mean(predicted, na.rm = TRUE),
  residuals = mean(residuals, na.rm = TRUE)
), by = .(SlootID, gebied)]

diag_uniq[, abs_resid := abs(residuals)]

# 1. Boxplot residuals per gebied, gesorteerd op mediaan absolute residu
gebied_order <- diag_uniq[, .(med_abs = median(abs_resid)), by = gebied][order(-med_abs), gebied]

ggplot(diag_uniq, aes(x = factor(gebied, levels = gebied_order), y = residuals)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.6) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = abs_resid > 100), width = 0.2, size = 2, alpha = 0.8) +
  scale_color_manual(values = c("FALSE" = "grey50", "TRUE" = "#D55E00"),
                     labels = c("FALSE" = "< 100 mV", "TRUE" = "> 100 mV"),
                     name = "Abs. residual") +
  labs(title = "Residuals per gebied",
       subtitle = "Gesorteerd op mediaan absolute afwijking",
       x = "Gebied", y = "Residual (mV)") +
  theme_minimal(base_size = 12) +
  theme(panel.border = element_rect(colour = "grey80", fill = NA))