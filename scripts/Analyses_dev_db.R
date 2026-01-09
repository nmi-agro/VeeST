
# 1. cluster analyse --------------------------------------------------------
setDT(abio_proj)
cluster.med<- cluster.med[!clusters %in% 7:8,]
setorder(cluster.med)
# trofie/afwateringsopp uit GIS, watbte en drooglegging uit dwarsprofielen
clust_abio <- abio_proj[,c('SlootID','drglg','watbte',"OS_perc_OR_25" ,"Z_CLAY_SA_OR_25" )]
clust_abio <- clust_abio[complete.cases(clust_abio)] # 2 missen
# tot do dist 2 medians, sclae closest
clust_m <- clust_abio[, drglg-cluster.med$drglg, by ='SlootID'] 
clust_m[,cluster:= rep(1:6, nrow(clust_m)/6)]
clust_m[,drglg_d := V1];clust_m[,V1:= NULL]
clust_m_2 <- clust_abio[, watbte-cluster.med$watbte, by ='SlootID']
clust_m_2[,cluster:= rep(1:6, nrow(clust_m_2)/6)]
clust_m_2[,watbte_d := V1]
clust_m_2[,V1:= NULL]
clust_m <- merge(clust_m, clust_m_2, by = c('SlootID','cluster'))
clust_m_2 <- clust_abio[, OS_perc_OR_25-cluster.med$OS_perc_OR_25, by ='SlootID']
clust_m_2[,cluster:= rep(1:6, nrow(clust_m_2)/6)]
clust_m_2[,OS_perc_OR_25_d := V1];clust_m_2[,V1:= NULL]
clust_m <- merge(clust_m, clust_m_2, by = c('SlootID','cluster'))
clust_m_2 <- clust_abio[, Z_CLAY_SA_OR_25-cluster.med$Z_CLAY_SA_OR_25, by ='SlootID']
clust_m_2[,cluster:= rep(1:6, nrow(clust_m_2)/6)]
clust_m_2[,Z_CLAY_SA_OR_25_d := V1];clust_m_2[,V1:= NULL]
clust_m <- merge(clust_m, clust_m_2, by = c('SlootID','cluster'))
clust_m_s <- data.frame(lapply(abs(clust_m[,3:6]), function(x) scale(x, center = FALSE, scale = max(x, na.rm = TRUE)/1)))
# clust_m_s <- data.frame(lapply(clust_m[,3:6], function(x) (x - min(x))/diff(range(x))))
clust_m <- cbind(clust_m[,c('SlootID','cluster')], clust_m_s)
clust_m[, sum_val := rowSums(.SD), .SDcols = c('drglg_d','watbte_d',"OS_perc_OR_25_d" ,"Z_CLAY_SA_OR_25_d")]
clust_m[, cluster_def := cluster[min(sum_val)==sum_val], by = c('SlootID')]
clust_m <- clust_m[cluster==cluster_def,]
## merge with data 
abio_proj <- merge(abio_proj, clust_m, by = c('SlootID'), all.x = T, suffixes = c('','_clustber'))

# 2. RF model ----------------------------------------

## Preparation 
# Column with point ID names
var_id <- "SlootID"
# fraction of training set
fr_train <- 0.8
##  Define response & explanatory variables 
# response variable/ target/ dependent
var_res <-"max_wtd"
#"Oeverzone_2a_emers_perc","Oeverzone_2b_emers_perc",
#"Waterzone_1_subm_tot_perc","Waterzone_1_emers_perc") #"max_wtd"
#"Oeverzone_2a_kaal_perc", "Oeverzone_2b_kaal_perc", 
#"oever_(10,20]","oever_(20,30]","oever_(30,40]",

# covariables/ predictors
var_cov <- c("perceel_(10,20]",
             "Oeverzone_2a_breedte_cm","Oeverzone_2b_breedte_cm" ,
             "Oeverzone_2a_emers_perc", "Oeverzone_2b_emers_perc",
             "tldk_oevrwtr_perc", "tldk_vastbodem_perc",
             "holleoever","drglg", "max_slib", "watbte", 
             "veentype", 
             "water_pH", "doorzicht2_mid_cm","water_conductiviteit_uS_cm","water_O2_mgL",
             "slib_redox_mgL", 
             "beheer") 
# Select complete records
var <- c(var_id, var_res, var_cov)
loc_t <- abio_proj[complete.cases(abio_proj[, ..var]), ..var]
# change column names of response variables
setnames(loc_t, old = c(var_res,"perceel_(10,20]","oever_(10,20]","oever_(30,40]"), new = c("varres","perceel_1020","oever2030","oever3040"),skip_absent=TRUE)

# Split training and test dataset
set.seed(123)
train <- sample(1:nrow(loc_t),nrow(loc_t) * fr_train)
# add train or not
loc_t[train, set := "training"]
loc_t[!train, set := "test"]
loc_t$set <- as.factor(loc_t$set)

print(paste0("training N = ", loc_t[set == "training", .N], 
             ", testing N = ", loc_t[set == "test", .N]))

## Random forest 
# Run random forest
require(randomForest)
cols <- c(var_id,"set")
cols <- colnames(loc_t)[!colnames(loc_t)%in%cols]
rf_res <- randomForest(varres ~ ., data = loc_t[set == "training", ..cols], mtry = round(length(var_cov)/3, 0), ntree = 400)
print(rf_res)
importance(rf_res)
#prediction
loc_t[, pred_rf := predict(rf_res, loc_t)]
loc_t[, resid_rf := varres - pred_rf]
varImpPlot(rf_res, type=2, main = "importance of variables")
# show figure of error vs number of trees
plot(rf_res, main = "Error vs number of trees")
## Check different numbers of Variables randomly chosen at each split (mtry)
# Check OBB errors in errors in test set
nvar <- length(var_cov) # number of explanatory variables
oob.err=double(nvar)
test.err=double(nvar)
#mtry is no of Variables randomly chosen at each split
for(mtry_t in 1:nvar){
    rf <- randomForest(varres ~ ., data = loc_t[set == "training", ..cols], 
                       mtry=mtry_t, ntree=400)
    oob.err[mtry_t] = rf$mse[400] #Error of all Trees fitted
    pred <- predict(rf, loc_t[-train,]) #Predictions on Test Set for each Tree
    test.err[mtry_t] = with(loc_t[-train,], mean( (varres - pred)^2)) #Mean Squared Test Error
    cat(mtry_t," ") #printing the output to the console
}
# plot errors
dt_f <- data.table(mtry = rep(1:nvar, times = 2),
                     val = c(oob.err, test.err),
                     type = c(rep("oob.err", nvar),
                              rep("test.err", nvar)))
gp_error <- ggplot(dt_f) + geom_point(aes(x = mtry, y = val, col = type), size = 3) +
    geom_line(aes(x = mtry, y = val, col = type), linewidth = 1) +
    xlab("Number of Predictors Considered at each Split") + ylab("Mean Squared Error") +
    scale_color_discrete(name="", breaks=c("oob.err", "test.err"),
                         labels=c("Out of Bag Error", "Test Error"))  +
    theme(legend.position="top")
show(gp_error)
#pdp
partialPlot(rf_res, loc_t[set == "training", ..cols], watbte)
partialPlot(rf_res, loc_t[set == "training", ..cols], drglg)
partialPlot(rf_res, loc_t[set == "training", ..cols], slib_redox_mgL)
partialPlot(rf_res, loc_t[set == "training", ..cols], water_O2_mgL)

# 3. XGBoost model --------------------------------------------------------

## versie met meerdere target variabelen tegelijk ---------------------------------
target_vars <- c("waterzone_1_subm_tot_perc","2","1","draagkracht_oever", 
                 "slib_redox_pH7","max_slib")
# redox, draagkracht oever, aantal soorten, slibdikte
# Create Dutch translation mapping for target variables
target_names_dutch <- c(
  "waterzone_1_subm_tot_perc" = "Bedekking ondergedoken planten (%)",
  "2" = "Aantal oeversoorten",
  "1" = "Aantal waterplantensoorten",
  "draagkracht_oever" = "Draagkracht oever (MPa)",
  "slib_redox_pH7" = "Redox slib bij pH7 (mV)",
  "max_slib" = "Slibdikte (m)")
# Define predictor variables with readable Dutch names
cols_corr <- c("drglg", "max_wtd", "zichtdiepte", "max_slib", "watbte","oeverzone_2b_breedte_cm", "oeverzone_2b_kaal_perc", 
               "holleoever", "tldk_wtrwtr_perc", "tldk_oevrwtr_perc", "slib_redox_pH7","slib_pH",
               "oevbte", "veentype_num", "Z_CLAY_SA_OR_50",
               "draagkracht_oever", "draagkracht_perceel", "water_pH", "NH4_µmol/l_PW","P-AL mg p2o5/100g_SB")
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
  "Z_CLAY_SA_OR_50" = "Kleigehalte 50cm (%)",
  "draagkracht_oever" = "Draagkracht oever (MPa)",
  "draagkracht_perceel" = "Draagkracht perceel (MPa)",
  "water_pH" = "Water pH",
  "NH4_µmol/l_PW" = "Ammonium (µmol/l)",
  "P-AL mg p2o5/100g_SB" = "P-AL slib (mg P2O5/100g)"

)

## versie redox en poriewaterconcentraties erbij
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

## Preparation------------------------------------------------
library(xgboost)

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
    verbose = 0
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

# Create enhanced plot titles with RMSE and R² values
all_importance[, plot_title := paste0(target_dutch, "\nRMSE: ", round(rmse_test, 2), 
                                      " | R²: ", round(r2_test * 100, 1), "%")]

## Plot feature importance with enhanced titles----------------------------------------------
ggplot(all_importance, aes(x = reorder(Nederlandse_naam, Gain), y = Gain, fill = target_var)) +
  geom_col() +
  facet_wrap(~plot_title, scales = "free") +
  coord_flip() +
  labs(
    title = "Belangrijste verklarende variabelen (XGBoost)",
    x = "Voorspellende variabelen",
    y = "Informatiewinst (Gain)",
    fill = "Doelvariable"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 10),
    legend.position = "none"
  )

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
if(!"target_dutch_multiline" %in% colnames(all_importance)) {
   target_names_dutch_multiline <- c(
    "waterzone_1_subm_tot_perc" = "Bedekking\nondergedoken\nplanten (%)",
    "2" = "Aantal\noeversoorten", 
    "1" = "Aantal\nwaterplantensoorten",  # Consistent met oeversoorten
    "draagkracht_oever" = "Draagkracht\noever (MPa)",
    "slib_redox_pH7" = "Redox slib\nbij pH7 (mV)",
    "max_slib" = "Slibdikte (m)"
  )
  all_importance[, target_dutch_multiline := target_names_dutch_multiline[target_var]]
}

if(!"correlation_direction" %in% colnames(all_importance)) {
  all_importance[, correlation_direction := mapply(
    get_correlation_direction, 
    target_var = target_var, 
    predictor_var = Feature,
    MoreArgs = list(data = abio_proj),
    USE.NAMES = FALSE
  )]
}

if(!"rmse_unit" %in% colnames(all_importance)) {
  rmse_units <- c(
    "waterzone_1_subm_tot_perc" = "%",
    "2" = "soorten",
    "draagkracht_oever" = "MPa",
    "slib_redox_pH7" = "mV",
    "max_slib" = "m"
  )
  all_importance[, rmse_unit := rmse_units[target_var]]
}

# Gebruik de nieuwe kolom in plot titel (gebruik rmse_pct_target in plaats van rmse_pct)
all_importance[, plot_title := paste0(target_dutch_multiline, 
                                      "\nR²: ", round(r2_test * 100, 1), "% | RMSE: ", round(rmse_test, 3), " ", rmse_unit,
                                      "\nRMSE: ", round(rmse_pct_target, 1), "% van gemiddelde")]

# Definieer Okabe-Ito kleuren voor elke target
okabe_ito_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
names(okabe_ito_colors) <- target_vars

# Maak plot titels met kleinere R² en RMSE tekst
all_importance[, plot_title_clean := paste0(target_dutch_multiline, 
                                            "\nR²: ", round(r2_test * 100, 1), "% | RMSE: ", round(rmse_test, 3), " ", rmse_unit)]

# VIP Plot met Okabe kleuren en aangepaste styling
ggplot(all_importance, aes(x = reorder(Nederlandse_naam, Gain), y = Gain, fill = target_var)) +
  geom_col() +
  geom_text(aes(label = correlation_direction), 
            hjust = -0.2, vjust = 0.5, 
            size = 5, fontface = "bold", 
            color = "black") +
  facet_wrap(~plot_title_clean, scales = "free", ncol = 2) +
  coord_flip() +
  scale_fill_manual(values = okabe_ito_colors) +
  labs(
    title = "Belangrijste verklarende variabelen wensbeelden (XGBoost)",
    subtitle = "Variable Importance met correlatierichting (+/-) en modelperformantie",
    x = "Voorspellende variabelen",
    y = "Informatiewinst (Gain)",
    fill = "Doelvariable"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 14),  # Grotere y-as labels (gebieden)
    axis.title = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 14, lineheight = 0.9),  # Kleinere strip tekst voor R² en RMSE
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    strip.background = element_rect(colour = "black", fill = "white", linewidth = 0.8),
    plot.margin = margin(10, 10, 10, 10)  # Minder ruimte onderaan (geen caption)
  )

ggsave(file = 'output/AlleGebieden/Tussenrapportage/XGBoost_feature_importance_okabe_clean.png', 
       width = 35, height = 30, units = 'cm', dpi = 800)

## ALE plots nieuw combinatie plots van sturende var op alle targets -------------------------------------------------------
# Functie om gecombineerde plots te maken met echte data + ALE effect
create_combined_ale_plots <- function() {
  
  # Definieer de 10 variabelen
  ale_variables <- cols_corr
  
  combined_plots <- list()
  
  for(var in ale_variables) {
    if(var %in% colnames(abio_proj)) {
      
      cat("Creating combined plot for:", var, "\n")
      
      # Verzamel alle target data voor deze variabele
      plot_data_list <- list()
      ale_data_list <- list()
      
      for(target in names(xgb_models)) {
        if(target %in% colnames(abio_proj)) {
          
          # Echte data voor scatter plot
          real_data <- abio_proj[!is.na(get(var)) & !is.na(get(target)), 
                                .(x = get(var), y = get(target), target = target)]
          
          if(nrow(real_data) > 10) {
            plot_data_list[[target]] <- real_data
            
            # Bereken ALE effect
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
            
            # Check if variable exists in model
            if(var %in% colnames(X_data)) {
              feature_idx <- which(colnames(X_data) == var)
              
              # Calculate ALE
              ale_result <- calculate_ale_manual(xgb_models[[target]], X_data, feature_idx, K = 30)
              
              # Create ALE data frame
              ale_df <- data.frame(
                x = ale_result$x_values,
                ale_effect = ale_result$ale_effects,
                target = target
              )
              
              ale_data_list[[target]] <- ale_df
            }
          }
        }
      }
      
      if(length(plot_data_list) > 0 && length(ale_data_list) > 0) {
        
        # Combine all real data
        all_real_data <- rbindlist(plot_data_list)
        all_real_data[, target_dutch := target_names_dutch[target]]
        
        # Combine all ALE data
        all_ale_data <- rbindlist(ale_data_list, fill = TRUE)
        all_ale_data[, target_dutch := target_names_dutch[target]]
        
        # Get Dutch variable name
        var_name_dutch <- nederlandse_namen[var]
        if(is.na(var_name_dutch)) var_name_dutch <- var
        
        # CORRECTE SCHALING: ALE=0 op mediaan van doelvariabele
        all_ale_data[, target_median := median(all_real_data[target == get("target"), y], na.rm = TRUE), by = target]
        
        # Bereken schaalfactor op basis van bereik
        y_range_per_target <- all_real_data[, .(y_min = min(y, na.rm = TRUE), 
                                               y_max = max(y, na.rm = TRUE),
                                               y_median = median(y, na.rm = TRUE)), by = target]
        
        all_ale_data <- merge(all_ale_data, y_range_per_target, by = "target")
        
        # Schaal ALE effecten zodat ze goed zichtbaar zijn maar ALE=0 op mediaan ligt
        ale_range_per_target <- all_ale_data[, .(ale_min = min(ale_effect, na.rm = TRUE),
                                                ale_max = max(ale_effect, na.rm = TRUE)), by = target]
        
        all_ale_data <- merge(all_ale_data, ale_range_per_target, by = "target")
        
        # Bereken schaalfactor per target zodat ALE range past binnen ~30% van y range
        all_ale_data[, scale_factor := ifelse(ale_max - ale_min > 0, 
                                             (y_max - y_min) * 0.3 / (ale_max - ale_min), 
                                             1), by = target]
        
        # Schaal ALE effecten met mediaan als centrum (ALE=0 op y_median)
        all_ale_data[, ale_scaled := y_median + ale_effect * scale_factor]
        
        # Maak referentielijn data voor ALE=0 (mediaan)
        median_lines <- unique(all_ale_data[, .(target, target_dutch, y_median)])
        
        # Bereken min/max ALE effecten per target voor labels
        ale_minmax <- all_ale_data[, .(
          ale_min_val = min(ale_effect, na.rm = TRUE),
          ale_max_val = max(ale_effect, na.rm = TRUE),
          x_pos_min = x[which.min(ale_effect)],
          x_pos_max = x[which.max(ale_effect)],
          y_pos_min = ale_scaled[which.min(ale_effect)],
          y_pos_max = ale_scaled[which.max(ale_effect)]
        ), by = .(target, target_dutch)]
        
        # Create the plot
        p <- ggplot() +
          # Echte datapunten
          geom_point(data = all_real_data, 
                    aes(x = x, y = y, color = target_dutch), 
                    alpha = 0.6, size = 1.5) +
          # Mediaan lijnen (ALE = 0 referentie)
          geom_hline(data = median_lines, 
                    aes(yintercept = y_median), 
                    linetype = "dashed", color = "red", alpha = 0.7, size = 0.5) +
          # ALE lijnen (geschaald met mediaan als centrum)
          geom_line(data = all_ale_data, 
                   aes(x = x, y = ale_scaled), 
                   size = 1.2, color = "black") +
          # Labels voor minimum ALE effect
          geom_text(data = ale_minmax,
                   aes(x = x_pos_min, y = y_pos_min, 
                       label = paste("Min:", round(ale_min_val, 3))),
                   color = "blue", fontface = "bold", size = 3,
                   hjust = 0.5, vjust = -0.5) +
          # Labels voor maximum ALE effect  
          geom_text(data = ale_minmax,
                   aes(x = x_pos_max, y = y_pos_max,
                       label = paste("Max:", round(ale_max_val, 3))),
                   color = "darkgreen", fontface = "bold", size = 3,
                   hjust = 0.5, vjust = 1.5) +
          facet_wrap(~target_dutch, scales = "free_y", ncol = 3) +
          labs(
            title = paste("Effect van", var_name_dutch, "op alle doelvariabelen"),
            subtitle = "Punten = echte data, zwarte lijn = ALE effect, rode lijn = mediaan (ALE=0), labels = min/max ALE",
            x = var_name_dutch,
            y = "Waarde doelvariabele",
            color = "Doelvariabele"
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 12),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10),
            strip.text = element_text(size = 11),
            legend.position = "bottom",
            legend.text = element_text(size = 9)
          ) +
          guides(color = guide_legend(ncol = 3))
        
        combined_plots[[var]] <- p
      }
    }
  }
  
  return(combined_plots)
}

# Rest van de code blijft hetzelfde
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



## Manual ALE calculation function (without ALEPlot package)---------------------------------------------

calculate_ale_manual <- function(model, X_data, feature_idx, K = 50) {
  
  # Get the feature column
  feature_values <- X_data[, feature_idx]
  feature_name <- colnames(X_data)[feature_idx]
  
  # Create quantile breaks
  quantiles <- quantile(feature_values, probs = seq(0, 1, length.out = K + 1), na.rm = TRUE)
  quantiles <- unique(quantiles) # Remove duplicates
  
  # Initialize ALE values
  ale_values <- numeric(length(quantiles) - 1)
  x_values <- numeric(length(quantiles) - 1)
  
  for(i in 1:(length(quantiles) - 1)) {
    # Get data points in this interval
    in_interval <- feature_values >= quantiles[i] & feature_values <= quantiles[i + 1]
    
    if(sum(in_interval) > 0) {
      # Create data for prediction at interval boundaries
      X_low <- X_data[in_interval, , drop = FALSE]
      X_high <- X_data[in_interval, , drop = FALSE]
      
      # Set feature values to interval boundaries
      X_low[, feature_idx] <- quantiles[i]
      X_high[, feature_idx] <- quantiles[i + 1]
      
      # Get predictions
      pred_low <- predict(model, X_low)
      pred_high <- predict(model, X_high)
      
      # Calculate local effect
      ale_values[i] <- mean(pred_high - pred_low, na.rm = TRUE)
      x_values[i] <- (quantiles[i] + quantiles[i + 1]) / 2
    }
  }
  
  # Calculate cumulative ALE effects
  ale_cumulative <- cumsum(ale_values)
  
  # Center the ALE values
  ale_centered <- ale_cumulative - mean(ale_cumulative, na.rm = TRUE)
  
  return(list(
    x_values = x_values[!is.na(ale_values)],
    ale_effects = ale_centered[!is.na(ale_values)],
    feature_name = feature_name
  ))
}

# Function to create ALE plots without ALEPlot package
create_ale_plots_manual <- function(model, X_data, target_name) {
  
  ale_plots <- list()
  
  # Key features to plot
  key_features <- c("watbte", "drglg", "holleoever", "tldk_oevrwtr_perc", 
                   "P-AL mg p2o5/100g_SB", "draagkracht_perceel", 
                   "Z_CLAY_SA_OR_50", "oeverzone_2b_breedte_cm")
  
  # Filter to available features
  available_features <- key_features[key_features %in% colnames(X_data)]
  
  for(feature in available_features) {
    feature_idx <- which(colnames(X_data) == feature)
    
    # Calculate ALE manually
    ale_result <- calculate_ale_manual(model, X_data, feature_idx, K = 30)
    
    # Create data frame for plotting
    ale_df <- data.frame(
      x = ale_result$x_values,
      ale_effect = ale_result$ale_effects
    )
    
    # Get Dutch name
    feature_name_dutch <- nederlandse_namen[feature]
    if(is.na(feature_name_dutch)) feature_name_dutch <- feature
    
    # Calculate effect range
    effect_range <- max(ale_df$ale_effect, na.rm = TRUE) - min(ale_df$ale_effect, na.rm = TRUE)
    
    # Create ggplot
    p <- ggplot(ale_df, aes(x = x, y = ale_effect)) +
      geom_line(color = "#1f77b4", size = 1) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
      labs(
        title = paste0(feature_name_dutch),
        subtitle = paste0("Effect range: ", round(effect_range, 3)),
        x = feature_name_dutch,
        y = paste("ALE Effect on", target_names_dutch[target_name])
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 9)
      )
    
    ale_plots[[feature]] <- p
  }
  
  return(ale_plots)
}

# Create ALE plots for all models
all_ale_plots <- list()

for(target in names(xgb_models)) {
  cat("Creating ALE plots for:", target, "\n")
  
  # Get model data
  model_vars <- c("SlootID", target, cols_corr)
  model_vars <- model_vars[!model_vars %in% target & model_vars %in% colnames(abio_proj)]
  model_data <- abio_proj[complete.cases(abio_proj[, ..model_vars]), ..model_vars]
  
  # Convert factors to numeric
  factor_cols <- names(model_data)[sapply(model_data, is.character)]
  factor_cols_2 <- names(model_data)[sapply(model_data, is.factor)]
  factor_cols <- c(factor_cols, factor_cols_2)
  model_data[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]
  model_data[, (factor_cols) := lapply(.SD, as.numeric), .SDcols = factor_cols]
  
  # Remove SlootID and target for X_data
  predictors_clean <- colnames(model_data)[!colnames(model_data) %in% c("SlootID", target)]
  X_data <- as.matrix(model_data[, ..predictors_clean])
  
  # Create ALE plots
  ale_plots <- create_ale_plots_manual(xgb_models[[target]], X_data, target)
  
  all_ale_plots[[target]] <- ale_plots
}

# Display and save ALE plots for each target
for(target in names(all_ale_plots)) {
  
  target_dutch <- target_names_dutch[target]
  perf <- performance_summary[target == target]
  
  cat("\n=== ALE Plots for", target_dutch, "===\n")
  cat("RMSE:", round(perf$rmse_test, 3), "| R²:", round(perf$r2_test * 100, 1), "%\n")
  
  # Display individual plots
  if(length(all_ale_plots[[target]]) > 0) {
    for(plot_name in names(all_ale_plots[[target]])) {
      cat("Showing ALE plot for:", plot_name, "\n")
      print(all_ale_plots[[target]][[plot_name]])
    }
  }
}

# Create summary of ALE effect strengths
create_ale_summary_plot <- function() {
  
  ale_summary_data <- list()
  
  for(target in names(all_ale_plots)) {
    for(var in names(all_ale_plots[[target]])) {
      
      # Get the plot data to extract effect range
      plot_data <- all_ale_plots[[target]][[var]]$data
      effect_range <- max(plot_data$ale_effect, na.rm = TRUE) - min(plot_data$ale_effect, na.rm = TRUE)
      
      ale_summary_data[[paste(target, var, sep = "_")]] <- data.table(
        target = target,
        target_dutch = target_names_dutch[target],
        target_dutch_multiline = target_names_dutch_multiline[target],  # Voeg meerregelige versie toe
        variable = var,
        variable_dutch = nederlandse_namen[var],
        effect_range = effect_range
      )
    }
  }
  
  # Combine summary data
  if(length(ale_summary_data) > 0) {
    ale_summary_dt <- rbindlist(ale_summary_data)
    
    # Okabe-Ito kleuren voor consistentie
    okabe_ito_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
    names(okabe_ito_colors) <- target_vars
    
    # Create summary plot met verbeterde styling en meerregelige titels
    ggplot(ale_summary_dt, aes(x = reorder(variable_dutch, effect_range), 
                              y = effect_range, fill = target)) +  
      geom_col(alpha = 0.8) +
      facet_wrap(~target_dutch_multiline, scales = "free", ncol = 3) +  # Gebruik meerregelige versie
      coord_flip() +
      scale_fill_manual(values = okabe_ito_colors) +
      labs(
        title = "ALE Effect Sterkte per Variabele en Doelvariabele",
        subtitle = "Grootte van het effect (range) van elke voorspellende variabele",
        x = "Voorspellende Variabelen",
        y = "ALE Effect Range",
        fill = "Doelvariabele"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, hjust = 0.5),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12, face = "bold"),  
        strip.text = element_text(size = 14, face = "bold", lineheight = 0.8),   # Lineheight voor meerregelige tekst
        legend.position = "none",
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
        strip.background = element_rect(colour = "black", fill = "white", linewidth = 0.8)
      )
  }
}

# Create and display summary plot
if(length(all_ale_plots) > 0 && sum(lengths(all_ale_plots)) > 0) {
  ale_summary_plot <- create_ale_summary_plot()
  print(ale_summary_plot)
  
  # Save de plot
  ggsave(
    filename = 'output/AlleGebieden/Tussenrapportage/ALE_effect_summary.png',
    plot = ale_summary_plot,
    width = 40, height = 30, units = 'cm', dpi = 300
  )
}

## VIP voor redox modellen -------------------------------------------------------
# Maak VIP plot specifiek voor redox modellen
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

# Maak de redox ALE plots
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
