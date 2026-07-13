# Correlatie matrix abiotische parameters----------------
# Handle non-numeric columns----------------------------------------------------------
abio_proj[Maaifrequentie_oever_per_jaar %in% c('0,5'), Maaifrequentie_oever_per_jaar := 0.5]
abio_proj[is.na(Maaifrequentie_oever_per_jaar), Maaifrequentie_oever_per_jaar := 0]
abio_proj[,Maaifrequentie_oever_per_jaar := as.numeric(Maaifrequentie_oever_per_jaar)]
abio_proj[,Baggerfrequentie_per_jaar := as.numeric(Baggerfrequentie_per_jaar)]
abio_proj[is.na(Baggerfrequentie_per_jaar), Baggerfrequentie_per_jaar := 0]
abio_proj[, Aantal_koeien_vee_perceel_dag := as.numeric(Aantal_koeien_vee_perceel_dag)]
abio_proj[,trofie := as.numeric(trofie)]
abio_proj[,draagkracht_perceel := as.numeric(draagkracht_perceel)]
abio_proj[,diepte_min_weerstand := as.numeric(diepte_min_weerstand)]

## Select the variables for correlation analysis tussenrapport-------------------------------------------------------------
cols_corr <- c("drglg", "max_wtd", "doorzicht2_mid_cm", "max_slib", "watbte",
               "oeverzone_2a_breedte_cm", "oeverzone_2b_breedte_cm", 
               "holleoever", "tldk_wtrwtr_perc", "tldk_oevrwtr_perc",
              "Baggerfrequentie_per_jaar","Maaifrequentie_oever_per_jaar","Aantal_Koedagen_per_jaar","Aantal_koeien_vee_perceel_dag")  

nederlandse_namen <- c(
  "drglg" = "Drooglegging (m)",
  "max_wtd" = "Maximale waterdiepte (m)",
  "doorzicht2_mid_cm" = "Doorzicht/waterdiepte (cm)",
  "max_slib" = "Maximale slibdikte (m)",
  "watbte" = "Waterbreedte (m)",
  "oeverzone_2a_breedte_cm" = "Breedte oevervegetatiezone 2a (cm)",
  "oeverzone_2b_breedte_cm" = "Breedte oevervegetatiezone 2b (cm)",
  "holleoever" = "Onderholling (cm)",
  "tldk_wtrwtr_perc" = "Taludhoek waterlijn (%)",
  "tldk_oevrwtr_perc" = "Taludhoek oever (%)",
   "Baggerfrequentie_per_jaar" = "Baggerfrequentie per jaar",
   "Maaifrequentie_oever_per_jaar"= "Maaifrequentie oever per jaar",
   "Aantal_Koedagen_per_jaar"= "Aantal koedagen per jaar",
   "Aantal_koeien_vee_perceel_dag"= "Aantal koeien per perceel per dag")

## waterbodemchemie ---------------------------------------------------------
cols_corr <- c(
    "pH_PW", "EGV_µs/cm_PW", "NH4_µmol/l_PW", "NO3_µmol/l_PW",
    "P_µmol/l_PW", "Fe_µmol/l_PW", "S_µmol/l_PW", "Ca_µmol/l_PW",
    "Mg_µmol/l_PW", "K_µmol/l_PW", "Na_µmol/l_PW", "Al_µmol/l_PW",
    "Mn_µmol/l_PW", "HCO3_µmol/l_PW",
    "N-NH4_CC_mg/kg_OR_25", "N-NO3_CC_mg/kg_OR_25",
    "P-AL mg p2o5/100g_OR_25", "Fe_CC_mg/kg_OR_25", "S_CC_mg/kg_OR_25", "Ca_CC_mg/kg_OR_25",
    "Mg_CC_mg/kg_OR_25", "K_CC_mg/kg_OR_25", "Na_CC_mg/kg_OR_25", "Al_CC_µg/kg_OR_25",
    "Mn_CC_mg/kg_OR_25", "OS_perc_OR_25"
  )
nederlandse_namen <- c(
  "pH_PW" = "pH poriewater",
  "EGV_µs/cm_PW" = "Elektrisch geleidingsvermogen PW (us/cm)",
  "NH4_µmol/l_PW" = "Ammonium PW (umol/l)",
  "NO3_µmol/l_PW" = "Nitraat PW (umol/l)",
  "P_µmol/l_PW" = "Fosfor PW (umol/l)",
  "Fe_µmol/l_PW" = "IJzer PW (umol/l)",
  "S_µmol/l_PW" = "Zwavel PW (umol/l)",
  "Ca_µmol/l_PW" = "Calcium PW (umol/l)",
  "Mg_µmol/l_PW" = "Magnesium PW (umol/l)",
  "K_µmol/l_PW" = "Kalium PW (umol/l)",
  "Na_µmol/l_PW" = "Natrium PW (umol/l)",
  "Al_µmol/l_PW" = "Aluminium PW (umol/l)",
  "Mn_µmol/l_PW" = "Mangaan PW (umol/l)",
  "HCO3_µmol/l_PW" = "Bicarbonaat PW (umol/l)",
  "N-NH4_CC_mg/kg_OR_25" = "Ammonium bodem CC (mg/kg, 25cm)",
  "N-NO3_CC_mg/kg_OR_25" = "Nitraat bodem CC (mg/kg, 25cm)",
  "P-AL mg p2o5/100g_OR_25" = "Fosfor bodem P-AL (mg p2o5/100g, 25cm)",
  "Fe_CC_mg/kg_OR_25" = "IJzer bodem CC (mg/kg, 25cm)",
  "S_CC_mg/kg_OR_25" = "Zwavel bodem CC (mg/kg, 25cm)",
  "Ca_M3_mg/kg_OR_25" = "Calcium bodem M3 (mg/kg, 25cm)",
  "Mg_CC_mg/kg_OR_25" = "Magnesium bodem CC (mg/kg, 25cm)",
  "K_CC_mg/kg_OR_25" = "Kalium bodem CC (mg/kg, 25cm)",
  "Na_CC_mg/kg_OR_25" = "Natrium bodem CC (mg/kg, 25cm)",
  "Al_CC_µg/kg_OR_25" = "Aluminium bodem CC (µg/kg, 25cm)",
  "Mn_CC_mg/kg_OR_25" = "Mangaan bodem CC (mg/kg, 25cm)",
  "OS_perc_OR_25" = "Organische stof bodem (%, 25cm)"
)

## abio + oeverbreedte en veentype ------------------------------------
cols_corr <- c("drglg", "watbte","oevbte",'max_slib','max_wtd',
               "oeverzone_2a_breedte_cm", "oeverzone_2b_breedte_cm", 
               "holleoever", "veentype_num", "Z_CLAY_SA_OR_25",
               "draagkracht_oever","draagkracht_perceel","oever_[0,15]","oever_(15,40]","diepte_min_weerstand",
               "Baggerfrequentie_per_jaar","Maaifrequentie_oever_per_jaar",
               "Aantal_Koedagen_per_jaar","Aantal_koeien_vee_perceel_dag",
               'tldk_wtrwtr_perc','tldk_oevrwtr_perc',
               'tldk_vastbodem_perc','max_hgt_or')  
nederlandse_namen <- c(
  "drglg" = "Drooglegging (m)",
  "watbte" = "Waterbreedte (m)",
  "oevbte" = "Oeverbreedte (m)",
  "max_slib" = "Maximale slibdikte (m)",
  "max_wtd" = "Maximale waterdiepte (m)",
  "oeverzone_2a_breedte_cm" = "Breedte oevervegetatiezone 2a (cm)",
  "oeverzone_2b_breedte_cm" = "Breedte oevervegetatiezone 2b (cm)",
  "holleoever" = "Onderholling (cm)",
  "veentype_num" = "Veentype",
  "Z_CLAY_SA_OR_25" = "Kleigehalte veen (%)",
  "draagkracht_oever" = "Draagkracht oever (mPa)",
  "draagkracht_perceel" = "Draagkracht perceel (mPa)",
  "oever_[0,15]" = "Draagkracht oever 0-15 cm (mPa)",
  "oever_(15,40]" = "Draagkracht oever  15-40cm (mPa)",
  "diepte_min_weerstand" = "Diepte minimale indringsingsweerstand (cm)",
  "Baggerfrequentie_per_jaar" = "Baggerfrequentie per jaar",
  "Maaifrequentie_oever_per_jaar" = "Maaifrequentie oever per jaar",
  "Aantal_Koedagen_per_jaar" = "Aantal koedagen per jaar",
  "Aantal_koeien_vee_perceel_dag" = "Aantal koeien per perceel per dag",
  "tldk_wtrwtr_perc" = "Taludhoek waterlijn (%)",
  "tldk_oevrwtr_perc" = "Taludhoek oever (%)",
  "tldk_vastbodem_perc" = "Taludhoek vastebodem (%)",
  "max_hgt_or" = "Max hoogte oeverrand (m)"
)
## alle xgboost parameters (tussenrapportage) -------------------------------------------------
cols_corr <- c("waterzone_1_subm_tot_perc","2","draagkracht_oever", 
                 "slib_redox_pH7","max_slib",
                 "drglg", "max_wtd", "zichtdiepte", "max_slib", "watbte","oeverzone_2b_breedte_cm", "oeverzone_2b_kaal_perc",
                 "holleoever", "tldk_wtrwtr_perc", "tldk_oevrwtr_perc", "slib_redox_pH7","slib_pH",
                 "oevbte", "Z_CLAY_SA_OR_25",
                 "draagkracht_oever", "draagkracht_perceel", "water_pH", "NH4_µmol/l_PW","P-AL mg p2o5/100g_SB")
# Create readable Dutch names mapping
nederlandse_namen <- c(
  "waterzone_1_subm_tot_perc" = "Bedekking submerse vegetatie in water (%)",
  "2" = "Aantal oeversoorten",
  "draagkracht_oever" = "Draagkracht oever (MPa)",
  "slib_redox_pH7" = "Redox slib bij pH7 (mV)",
  "max_slib" = "Slibdikte (m)",
  "drglg" = "Drooglegging (m)",
  "max_wtd" = "Maximale waterdiepte (m)", 
  "zichtdiepte" = "Doorzicht/waterdiepte",
  "max_slib" = "Maximale slibdikte (m)",
  "watbte" = "Waterbreedte (m)",
  "oeverzone_2b_breedte_cm" = "Breedte oevervegetatiezone 2b (cm)",
  "oeverzone_2b_kaal_perc" = "Bedekking kale oever zone 2b (%)",
  "holleoever" = "Onderholling (cm)",
  "tldk_wtrwtr_perc" = "Taludhoek waterlijn (%)",
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
  "P-AL mg p2o5/100g_SB" = "P-AL slib (mg P2O5/100g)",
   "Baggerfrequentie_per_jaar" = "Baggerfrequentie per jaar",
   "Maaifrequentie_oever_per_jaar"= "Maaifrequentie oever per jaar",
   "Aantal_Koedagen_per_jaar"= "Aantal koedagen per jaar",
   "Aantal_koeien_vee_perceel_dag"= "Aantal koeien per perceel per dag")  

## alle xgboost parameters inclusief beheer -------------------------------------------------
cols_corr <- c("n_soorten_oev_zone2","oeverindex","Soortensamenstelling Helofyten","waterzone_1_subm_tot_perc","n_soorten_sub_zone1","Soortensamenstelling Hydrofyten","draagkracht_oever", 
                 "slib_redox_pH7","drglg", "max_wtd", "zichtdiepte", "max_slib", "watbte","oeverzone_2b_breedte_cm", "oeverzone_2b_kaal_perc", 
               "holleoever", "tldk_wtrwtr_perc", "tldk_oevrwtr_perc", "slib_redox_pH7","slib_pH",
               "oevbte", "veentype_num", "Z_CLAY_SA_OR_25",
               "draagkracht_oever", "draagkracht_perceel", "water_pH", "NH4_µmol/l_PW","P-AL mg p2o5/100g_SB",
              "Baggerfrequentie_per_jaar","Maaifrequentie_oever_per_jaar","Aantal_Koedagen_per_jaar","Aantal_koeien_vee_perceel_dag")
# Create readable Dutch names mapping
nederlandse_namen <- c(
   "waterzone_1_subm_tot_perc" = "Bedekking ondergedoken planten (%)",
  "n_soorten_oev_zone2" = "Aantal oeversoorten",
  "oeverindex" = "oeverindex",
  "n_soorten_sub_zone1" = "Aantal waterplantensoorten",
  "Soortensamenstelling Helofyten" = "Soortensamenstelling Helofyten",
  "Soortensamenstelling Hydrofyten" = "Soortensamenstelling Hydrofyten",
  "draagkracht_oever" = "Draagkracht oever (MPa)",
  "slib_redox_pH7" = "Redox slib bij pH7 (mV)",
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
  "Z_CLAY_SA_OR_25" = "Kleigehalte 25 cm (%)",
  "draagkracht_oever" = "Draagkracht oever (MPa)",
  "draagkracht_perceel" = "Draagkracht perceel (MPa)",
  "water_pH" = "Water pH",
  "NH4_µmol/l_PW" = "Ammonium (µmol/l)",
  "P-AL mg p2o5/100g_SB" = "P-AL slib (mg P2O5/100g)",
  "Baggerfrequentie_per_jaar" = "Baggerfrequentie per jaar",
  "Maaifrequentie_oever_per_jaar"= "Maaifrequentie oever per jaar",
  "Aantal_Koedagen_per_jaar"= "Aantal koedagen per jaar",
  "Aantal_koeien_vee_perceel_dag"= "Aantal koeien per perceel per dag"
)

## create corplot with all variables--------------------------------------------------------
cols_corr <- names(abio_proj)[sapply(abio_proj, is.numeric)]
nederlandse_namen <- cols_corr
# Check which variables actually exist in the dataset -------------------------------------
available_cols <- cols_corr[cols_corr %in% colnames(abio_proj)]
print("Available variables:")
print(available_cols)


# Create correlation matrix with available columns only--------------------------------------------------
cormatrix <- abio_proj[, c(available_cols), with = FALSE]
cormatrix <- cormatrix[, lapply(.SD, as.numeric)]
M <- cor(cormatrix, use = "pairwise.complete.obs")
M[is.nan(M) | is.infinite(M)] <- NA
rownames(M) <- colnames(cormatrix)
colnames(M) <- colnames(cormatrix)
# Nu kunt u de namen vervangen
rownames(M) <- nederlandse_namen[rownames(M)]
colnames(M) <- nederlandse_namen[colnames(M)]

# Bereken p.mat passend bij M (zelfde kolommen, zelfde volgorde)
p.mat <- cor_pmat(cormatrix, use = "pairwise.complete.obs")
rownames(p.mat) <- nederlandse_namen[rownames(p.mat)]
colnames(p.mat) <- nederlandse_namen[colnames(p.mat)]

ggcorrplot(M, 
           type = "lower",
           hc.order = TRUE,
           outline.color = "white",
           colors = c("#D55E00", "#FFFFFF", "#0072B2"),
           lab = TRUE,
           lab_size = 4,
           digits = 1,
           tl.cex = 12,
           title = "Correlatiematrix: Draagkracht, profiel en beheer",
           p.mat = p.mat,
           insig = "blank",
          show.diag = TRUE,
          legend.title = "") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12)
  )+
  labs(legend.title ='')

# specifieke relaties-------------------------------
## draagkracht perceel en oeverbreedte 2b -------------------------------------------
ggplot(data = abio_proj, aes(x= draagkracht_perceel , y= oeverzone_2b_breedte_cm))+
  geom_jitter(aes(col= gebied, text = SlootID))+
  geom_smooth(method="gam") +
  # stat_regline_equation(label.x=200, label.y=470)+
  stat_cor(aes(label=after_stat(rr.label)), label.x=1, label.y=200)+
  # ylim(0,150)+
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size=10), 
    strip.text.y = element_text(size = 10), 
    axis.text.x = element_text(size = 10, angle = 90),
    axis.text.y = element_text(size= 12),
    axis.ticks =  element_line(colour = "black"),
    plot.title = element_text(size =12, face="bold", hjust = 0.5),
    panel.background = element_blank(),
    plot.background = element_blank(),
  )+
  ggtitle("profiel") +
  labs(x= "draagkracht insteek",y="breedte 2b")

## poriewater P en kroos------------------------------------------------------------
p <- ggplot(data = abio_proj, aes(x= `P_µmol/l_PW` , y= Waterzone_1_kroos_perc))+
  geom_jitter(aes(col= gebied, text = SlootID))+
  geom_smooth(method="gam") +
  # stat_regline_equation(label.x=200, label.y=470)+
  stat_cor(aes(label=after_stat(rr.label)), label.x=200, label.y=20)+
  # ylim(0,150)+
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size=10), 
    strip.text.y = element_text(size = 10), 
    axis.text.x = element_text(size = 10, angle = 90),
    axis.text.y = element_text(size= 12),
    axis.ticks =  element_line(colour = "black"),
    plot.title = element_text(size =12, face="bold", hjust = 0.5),
    panel.background = element_blank(),
    plot.background = element_blank(),
  )+
  ggtitle("profiel") +
  labs(x= "P poriewater (µmol/l)",y="kroos (%)")
ggplotly(p, tooltip = c('text'))

## poriewater P en algen------------------------------------------------------------
p <- ggplot(data = abio_proj, aes(x= `P_µmol/l_PW` , y= `Chl-a totaal_ug/l_OW`))+
  geom_jitter(aes(col= gebied, text = SlootID))+
  geom_smooth(method="gam") +
  # stat_regline_equation(label.x=200, label.y=470)+
  stat_cor(aes(label=after_stat(rr.label)), label.x=200, label.y=100)+
  ylim(0,150)+
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size=10), 
    strip.text.y = element_text(size = 10), 
    axis.text.x = element_text(size = 10, angle = 90),
    axis.text.y = element_text(size= 12),
    axis.ticks =  element_line(colour = "black"),
    plot.title = element_text(size =12, face="bold", hjust = 0.5),
    panel.background = element_blank(),
    plot.background = element_blank(),
  )+
  ggtitle("profiel") +
  labs(x= "P poriewater (µmol/l)",y="algen (µmol/l)")
ggplotly(p, tooltip = c('text'))

## poriewater P en redox------------------------------------------------------------
p <- ggplot(data = abio_proj, aes(x= `P_µmol/l_PW` , y= slib_redox_mgL))+
  geom_jitter(aes(col= gebied, text = SlootID))+
  geom_smooth(method="gam") +
  # stat_regline_equation(label.x=200, label.y=470)+
  stat_cor(aes(label=after_stat(rr.label)), label.x=200, label.y=20)+
  # ylim(0,150)+
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size=10), 
    strip.text.y = element_text(size = 10), 
    axis.text.x = element_text(size = 10, angle = 90),
    axis.text.y = element_text(size= 12),
    axis.ticks =  element_line(colour = "black"),
    plot.title = element_text(size =12, face="bold", hjust = 0.5),
    panel.background = element_blank(),
    plot.background = element_blank(),
  )+
  ggtitle("profiel") +
  labs(x= "P poriewater (µmol/l)",y="redox")
ggplotly(p, tooltip = c('text'))

## waterdiepte drooglegging------------------------------------------------------------
ggplot() +
  geom_jitter(data = abio_proj[max_wtd < 0.8,], aes(x = max_wtd, y = drglg, col = clusters)) + 
  geom_smooth(data = abio_proj[max_wtd < 0.8,], aes(x = max_wtd, y = drglg)) +
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size=10), 
    strip.text.y = element_text(size = 10), 
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size= 12),
    axis.ticks =  element_line(colour = "black"),
    plot.title = element_text(size =12, face="bold", hjust = 0.5),
    panel.background = element_blank(),
    plot.background = element_blank(),
  )+
  ggtitle("Relatie waterdiepte - drooglegging") +
  labs(x= "waterdiepte (m)",y="drooglegging (m)")
ggsave(file=paste0('output/clusters/watdte_drglg.png'),width = 15,height = 15,units='cm',dpi=1000)

## drooglegging ------------------------------------------------------------
p <- ggplot(data = abio_proj, aes(x= drglg , y= max_wtd))+
  geom_jitter(aes(col= gebied, text = SlootID))+
  geom_smooth(method="gam") +
  # stat_regline_equation(label.x=200, label.y=470)+
  stat_cor(aes(label=after_stat(rr.label)), label.x=0.5, label.y=0.5)+
  # ylim(0,150)+
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size=10), 
    strip.text.y = element_text(size = 10), 
    axis.text.x = element_text(size = 10, angle = 90),
    axis.text.y = element_text(size= 12),
    axis.ticks =  element_line(colour = "black"),
    plot.title = element_text(size =12, face="bold", hjust = 0.5),
    panel.background = element_blank(),
    plot.background = element_blank(),
  )+
  ggtitle("profiel") +
  labs(x= "drooglegging (m)",y="waterdiepte (m)")
ggplotly(p, tooltip = c('text'))
## drooglegging onderholling ------------------------------------------------------------
p <- ggplot(data = abio_proj, aes(x= drglg , y= holleoever))+
  geom_jitter(aes(col= gebied, text = SlootID))+
  geom_smooth(method="gam") +
  # stat_regline_equation(label.x=200, label.y=470)+
  stat_cor(aes(label=after_stat(rr.label)), label.x=0.5, label.y=0.5)+
  # ylim(0,150)+
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size=10), 
    strip.text.y = element_text(size = 10), 
    axis.text.x = element_text(size = 10, angle = 90),
    axis.text.y = element_text(size= 12),
    axis.ticks =  element_line(colour = "black"),
    plot.title = element_text(size =12, face="bold", hjust = 0.5),
    panel.background = element_blank(),
    plot.background = element_blank(),
  )+
  ggtitle("profiel") +
  labs(x= "drooglegging (m)",y="onderholling (m)")
ggplotly(p, tooltip = c('text'))
## relatie waterdiepte en redox slib bij pH7 ---------------------------
# R² berekenen voor waterzone vs waterbreedte (in meters)
r2_waterzone_watbte <- get_r_squared(abio_proj[!is.na(max_wtd) & !is.na(slib_redox_pH7)], 
                                     "max_wtd", "slib_redox_pH7")
p1 <- ggplot()+
  geom_jitter(data=abio_proj[!is.na(max_wtd) & !is.na(slib_redox_pH7),],
              aes(y=slib_redox_pH7, x=max_wtd), alpha=0.3, size=2)+
  geom_smooth(data=abio_proj[!is.na(max_wtd) & !is.na(slib_redox_pH7),],
              aes(y=slib_redox_pH7, x=max_wtd), method='lm', color='#1B9E77', size=1.5)+
  # R² annotatie  
  annotate("text", x=Inf, y=Inf, 
           label=paste0("R² = ", round(r2_waterzone_watbte, 3)), 
           hjust=1.1, vjust=1.5, size=4, fontface="bold") +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.ticks = element_line(colour = "black"),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    plot.background = element_blank()
  ) +
  ggtitle('Relatie waterdiepte en redox slib bij pH7') +
  labs(y = 'Redox slib bij pH7 (mV)', x = 'Maximale waterdiepte (m)')
# Toon de gecombineerde plot
print(p1)
## Plot slibdikte tegen drooglegging ---------------------------
# R² berekenen voor waterzone vs waterbreedte (in meters)
r2_waterzone_watbte <- get_r_squared(abio_proj[!is.na(max_slib) & !is.na(watbte)], 
                                     "watbte", "max_slib")
p1 <- ggplot()+
  geom_jitter(data=abio_proj[!is.na(max_slib) & !is.na(watbte),],
              aes(y=max_slib, x=watbte), alpha=0.3, size=2)+
  geom_smooth(data=abio_proj[!is.na(max_slib) & !is.na(watbte),],
              aes(y=max_slib, x=watbte), method='lm', color='#1B9E77', size=1.5)+
  # R² annotatie
  annotate("text", x=Inf, y=Inf, 
           label=paste0("R² = ", round(r2_waterzone_watbte, 3)), 
           hjust=1.1, vjust=1.5, size=4, fontface="bold") +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.ticks = element_line(colour = "black"),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    plot.background = element_blank()
  ) +
  ggtitle('Relatie slibdikte en waterbreedte') +
  labs(y = 'slibdikte (m)', x = 'waterbreedte (m)')

# Toon de gecombineerde plot
print(p1)
ggsave(file=paste0('output/AlleGebieden/Tussenrapportage/slibwaterbreedte_relatie.png'), width = 25,height = 15,units='cm',dpi=800)
## Plot slibdikte tegen drooglegging ---------------------------
# R² berekenen voor waterzone vs waterbreedte (in meters)
r2_waterzone_watbte <- get_r_squared(abio_proj[!is.na(max_slib) & !is.na(drglg)], 
                                     "drglg", "max_slib")
p1 <- ggplot()+
  geom_jitter(data=abio_proj[!is.na(max_slib) & !is.na(drglg),],
              aes(y=max_slib, x=drglg), alpha=0.3, size=2)+
  geom_smooth(data=abio_proj[!is.na(max_slib) & !is.na(drglg),],
              aes(y=max_slib, x=drglg), method='lm', color='#1B9E77', size=1.5)+
  # R² annotatie
  annotate("text", x=Inf, y=Inf, 
           label=paste0("R² = ", round(r2_waterzone_watbte, 3)), 
           hjust=1.1, vjust=1.5, size=4, fontface="bold") +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.ticks = element_line(colour = "black"),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    plot.background = element_blank()
  ) +
  ggtitle('Relatie slibdikte en drooglegging') +
  labs(y = 'slibdikte (m)', x = 'drooglegging (m)')

# Toon de gecombineerde plot
print(p1)
ggsave(file=paste0('output/AlleGebieden/Tussenrapportage/slibdrooglegging_relatie.png'), width = 25,height = 15,units='cm',dpi=800)

## relatie koeien drinken uit sloot en redox slib bij pH7 ---------------------------
dt_plot <- copy(abio_proj)[
  !is.na(Aantal_Koedagen_per_jaar) & !is.na(slib_redox_pH7)
]
### Versie koeien drinken uit sloot (wat soms op ja staat bij uitrastering)-----------------------------------------------
dt_plot[, koeien_drinken := fifelse(
  Aantal_Koedagen_per_jaar > 0,
  "Wel drinken uit sloot",
  "Geen drinken uit sloot"
)]
dt_plot[, koeien_drinken := factor(
  koeien_drinken,
  levels = c("Geen drinken uit sloot", "Wel drinken uit sloot")
)]
p_koeien_redox <- ggplot(
  dt_plot,
  aes(x = koeien_drinken, y = slib_redox_pH7, fill = koeien_drinken)
) +
  geom_boxplot(outlier.shape = NA, alpha = 0.75, width = 0.65) +
  geom_jitter(width = 0.12, alpha = 0.35, size = 1.6, color = "grey30") +
  scale_fill_manual(values = c("#56B4E9", "#D55E00"), guide = "none") +
  labs(
    x = "Koeien drinken uit sloot",
    y = "Redox slib bij pH7 (mV)",
    title = "Relatie koeien drinken uit sloot en slib-redox"
  ) +
  theme_minimal(base_size = 13)
### Versie koebelasting drinkende koeien ---------------------------------------------
dt_plot[, koeien_drinken_correctie := fifelse(
  koebelasting_drinkende_koeien > 0,
  "Wel drinken uit sloot",
  "Geen drinken uit sloot"
)]
dt_plot[, koeien_drinken_correctie := factor(
  koeien_drinken,
  levels = c("Geen drinken uit sloot", "Wel drinken uit sloot")
)]
p_koeien_redox <- ggplot(
  dt_plot,
  aes(x = koeien_drinken_correctie, y = slib_redox_pH7, fill = koeien_drinken_correctie)
) +
  geom_boxplot(outlier.shape = NA, alpha = 0.75, width = 0.65) +
  geom_jitter(width = 0.12, alpha = 0.35, size = 1.6, color = "grey30") +
  scale_fill_manual(values = c("#56B4E9", "#D55E00"), guide = "none") +
  labs(
    x = "Koeien drinken uit sloot",
    y = "Redox slib bij pH7 (mV)",
    title = "Relatie koeien drinken uit sloot en slib-redox"
  ) +
  theme_minimal(base_size = 13)
### versie jitter met koebelasting drinkende koeien (zonder boxplot) ---------------------------------------------
p_koeien_redox <- ggplot(
  dt_plot[koebelasting_drinkende_koeien < 5000,],
  aes(x = koebelasting_drinkende_koeien, y = slib_redox_pH7)) +
  # geom_boxplot(outlier.shape = NA, alpha = 0.75, width = 0.65) +
  geom_jitter(width = 0.12, alpha = 0.35, size = 1.6, color = "grey30") +
  geom_smooth(method = "glm", color = "#1B9E77", size = 1.5) +
  # scale_fill_manual(values = c("#56B4E9", "#D55E00"), guide = "none") +
  labs(
    x = "Koeibelasting van koeien die drinken uit sloot",
    y = "Redox slib bij pH7 (mV)",
    title = "Relatie koeien drinken uit sloot en slib-redox"
  ) +
  theme_minimal(base_size = 13)
p_koeien_redox



## generiek pars aanpasbaar ----------------------------------------------------------------------------------------
