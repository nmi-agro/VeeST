### Correlatie matrix abiotische parameters----------------
# Select the variables for correlation analysis tussenrapport
cols_corr <- c("drglg", "max_wtd", "doorzicht2_mid_cm", "max_slib", "watbte",
               "oeverzone_2a_breedte_cm", "oeverzone_2b_breedte_cm", 
               "holleoever", "tldk_wtrwtr_perc", "tldk_oevrwtr_perc",
              "Baggerfrequentie_per_jaar","Maaifrequentie_oever_per_jaar","Aantal_Koedagen_per_jaar","Aantal_koeien_vee_perceel_dag")  
nederlandse_namen <- c(
  "Drooglegging (m)",
  "Maximale waterdiepte (m)", 
  "Doorzicht (cm)",
  "Maximale slibdikte (m)",
  "Waterbreedte (m)",
  "Breedte oeverzone 2a (cm)",
  "Breedte oeverzone 2b (cm)",
  "Onderholling (cm)",
  "Taludhoek waterlijn (%)",
  "Taludhoek oever (%)",
  "Baggerfrequentie per jaar",
  "Maaifrequentie oever per jaar",
  "Aantal koedagen per jaar",
  "Aantal koeien per perceel per dag"
)
# abio + oeverbreedte en veentype ------------------------------------
cols_corr <- c("drglg", "watbte","oevbte",
               "oeverzone_2a_breedte_cm", "oeverzone_2b_breedte_cm", 
               "holleoever", "veentype_num", "Z_CLAY_SA_OR_50","draagkracht_oever","draagkracht_perceel",
               "Baggerfrequentie_per_jaar","Maaifrequentie_oever_per_jaar","Aantal_Koedagen_per_jaar","Aantal_koeien_vee_perceel_dag")  # voorbeeldparameters, pas aan op basis van beschikbaarheid)  # nieuwe parameters toegevoegd
nederlandse_namen <- c(
  "Drooglegging (m)",
  "Waterbreedte (m)",
  "Oeverbreedte (m)",
  "Breedte oevervegetatiezone 2a (cm)",
  "Breedte oevervegetatiezone 2b (cm)",
  "Onderholling (cm)",
  "Veentype",
  "Kleigehalte veen (%)",
  "Draagkracht oever (mPa)",
  "Draagkracht perceel (mPa)",
  "Baggerfrequentie per jaar",
  "Maaifrequentie oever per jaar",
  "Aantal koedagen per jaar",
  "Aantal koeien per perceel per dag"
)
# alle xgboost parameters (tussenrapportage) -------------------------------------------------
cols_corr <- c("waterzone_1_subm_tot_perc","2","draagkracht_oever", 
                 "slib_redox_pH7","max_slib",
                 "drglg", "max_wtd", "zichtdiepte", "max_slib", "watbte","oeverzone_2b_breedte_cm", "oeverzone_2b_kaal_perc",
                 "holleoever", "tldk_wtrwtr_perc", "tldk_oevrwtr_perc", "slib_redox_pH7","slib_pH",
                 "oevbte", "Z_CLAY_SA_OR_50",
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

# alle xgboost parameters inclusief beheer -------------------------------------------------
cols_corr <- c("waterzone_1_subm_tot_perc","2","draagkracht_oever", 
                 "slib_redox_pH7","max_slib",
                 "drglg", "max_wtd", "zichtdiepte", "max_slib", "watbte","oeverzone_2b_breedte_cm", "oeverzone_2b_kaal_perc",
                 "holleoever", "tldk_wtrwtr_perc", "tldk_oevrwtr_perc", "slib_redox_pH7","slib_pH",
                 "oevbte", "Z_CLAY_SA_OR_50",
                 "draagkracht_oever", "draagkracht_perceel", "water_pH", "NH4_µmol/l_PW","P-AL mg p2o5/100g_SB",
                "Baggerfrequentie_per_jaar","Maaifrequentie_oever_per_jaar","Aantal_Koedagen_per_jaar","Aantal_koeien_vee_perceel_dag")
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

# create corplot with all variables--------------------------------------------------------
cols_corr <- names(abio_proj)[sapply(abio_proj, is.numeric)]
## Check which variables actually exist in the dataset -------------------------------------
available_cols <- cols_corr[cols_corr %in% colnames(abio_proj)]
print("Available variables:")
print(available_cols)

# Create correlation matrix and p-value matrix with the SAME variables
abio_proj[,trofie := as.numeric(trofie)]
abio_proj[,draagkracht_perceel := as.numeric(draagkracht_perceel)]

# Handle non-numeric columns----------------------------------------------------------
abio_proj[Maaifrequentie_oever_per_jaar %in% c('0,5'), Maaifrequentie_oever_per_jaar := 0.5]
abio_proj[is.na(Maaifrequentie_oever_per_jaar), Maaifrequentie_oever_per_jaar := 0]
abio_proj[,Maaifrequentie_oever_per_jaar := as.numeric(Maaifrequentie_oever_per_jaar)]
abio_proj[,Baggerfrequentie_per_jaar := as.numeric(Baggerfrequentie_per_jaar)]
abio_proj[is.na(Baggerfrequentie_per_jaar), Baggerfrequentie_per_jaar := 0]
abio_proj[, Aantal_koeien_vee_perceel_dag := as.numeric(Aantal_koeien_vee_perceel_dag)]

# Create correlation matrix with available columns only
cormatrix <- abio_proj[, available_cols, with = FALSE]
cormatrix <- cormatrix[, lapply(.SD, as.numeric)]

is_bad <- function(x) all(is.na(x)) || all(is.infinite(x)) || sd(x, na.rm = TRUE) == 0
good_cols <- !sapply(cormatrix, is_bad)
cormatrix_clean <- cormatrix[, good_cols, with = FALSE]

M <- cor(cormatrix_clean, use = "pairwise.complete.obs")
M[is.nan(M) | is.infinite(M)] <- NA

# Remove rows/cols that are still all-NA after correlation
keep <- rowSums(!is.na(M)) > 1
M <- M[keep, keep]

p.mat <- cor_pmat(cormatrix_clean[, names(keep)[keep], with = FALSE])
rownames(M) <- colnames(cormatrix_clean)
colnames(M) <- colnames(cormatrix_clean)
# Create both matrices with the same data
M <- cor(cormatrix)
# Apply Dutch names to the same matrix
rownames(M) <- colnames(cormatrix)
colnames(M) <- colnames(cormatrix)
# Nu kunt u de namen vervangen
rownames(M) <- nederlandse_namen[rownames(M)]
colnames(M) <- nederlandse_namen[colnames(M)]

# Now use M (not M_clean) with matching p.mat
ggcorrplot(M, 
           type = "lower",
           hc.order = TRUE,
           outline.color = "white",
           colors = c("#D55E00", "#FFFFFF", "#0072B2"),
           lab = TRUE,
           lab_size = 4,
           digits = 1,
           tl.cex = 12,
           title = "Correlatiematrix: Slootprofiel en vegetatiezones",
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

# plot relaties database --------------------
## pairs ---------------------------
pairs(abio_proj[,c("holleoever","water_conductiviteit_uS_cm","water_redox","water_pH","max_slib","max_wtd","slib_pH","oever_[0,10]" ,"oever_(30,40]" ) ,with=FALSE])
pairs(abio_proj[,c("holleoever","water_pH","max_slib","max_wtd","oever_(30,40]", "slib_redox_mgL"),with=FALSE], col = abio_proj$clusters_clustber)
pairs(abio_proj[,c("holleoever","oever_(30,40]","Oeverzone_2b_emers_perc","max_slib","max_wtd","Waterzone_1_subm_tot_perc"),with=FALSE], col = abio_proj$clusters_clustber)
pairs(abio_proj[,c("drglg","Oeverzone_2b_breedte_cm","Oeverzone_2a_breedte_cm"),with=FALSE], col = abio_proj$clusters_clustber)
pairs(abio_proj[,c("holleoever","drglg","slib_redox_mgL","water_pH","max_wtd") ,with=FALSE])

## specifieke relaties-------------------------------

# poriewater P en kroos
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

# poriewater P en algen
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

# poriewater P en redox
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

#waterdiepte drooglegging
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

# drooglegging 
# breedte oever watbte, max diepte max_wtd
# breedte aquatische oever (cm) Oeverzone_2a_breedte_cm
# tldk_vastbodem_perc tldk_oevrwtr_perc
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

