# 1. Load packages -----------------------------------------------------------
library(data.table)
library(sf)
library(R.utils)
library(dplyr)
library(ggplot2)
library(ggnewscale)
library(nngeo) #azimuth
library(readxl)
library(plotly)
library(RColorBrewer)
require(ggpubr)
library(ggrepel)

# 2. Load processed data VeeST ----------------------------------------------------------------
abio_proj <- fread(paste0(Sys.getenv("NMI-SITE"), 'O 1900 - O 2000/1922.N.23 VeeST vwsloot vd toekomst/05. Data/output/Database/db_veest202511211624.csv'))

# 3. Filter locations Lisanne NH4 analysis -------------------------------------------------------------
locslis <- abio_proj[SlootID %in% c('AD_1_WP1_N','AD_5_WP_N','AR_2_WP1_W','DR_3_R_Z','DR_3_R_W','DR_3_M-AUG_N','ZG_2_M_O','ZG_8_WP1_N','KW_3_R_W'),]

### Ammonium toxicity all ------------------------

# Maak dataframe van ammonium toxiciteitsklassen
ammonium <- data.frame(xmin = -Inf, 
                    xmax = Inf,
                    ymin = c(0,100,400,750,5000),  
                    ymax = c(100,400,750,5000,Inf),
                    fill = c("green","yellow", "orange","red","purple"),
                    label = c("niet", "voor gevoelige soorten","voor veel soorten","voor bijna alle soorten","voor alle soorten"))

legend_colors <- setNames(c("green","yellow", "orange","red","purple"), ammonium$label)
# Calculate summary statistics first
ammonium_summary <- abio_proj[!is.na(Gebiedsnaam) & !is.na(`NH4_µmol/l_PW`), .(
  mean_nh4 = mean(`NH4_µmol/l_PW`, na.rm = TRUE),
  sd_nh4 = sd(`NH4_µmol/l_PW`, na.rm = TRUE)
), by = Gebiedsnaam]

# Sorteer 
sort_order <- ammonium_summary[order(mean_nh4)]
ammonium_summary[, Gebiedsnaam := factor(Gebiedsnaam, levels = unique(sort_order$Gebiedsnaam))]

ggplot() +
  geom_rect(data = ammonium, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), 
            inherit.aes = FALSE, alpha = 0.25) +
  scale_fill_identity('Giftig:', breaks = legend_colors, 
                     labels = c("niet", "voor gevoelige soorten", "voor veel soorten", 
                               "voor bijna alle soorten", "voor alle soorten"), 
                     guide = guide_legend(override.aes = list(alpha = 0.15))) +
  geom_col(data = ammonium_summary,
           aes(x = Gebiedsnaam, y = mean_nh4), 
           fill = "#1B9E77", col = "#1B9E77",alpha = 0.2) +
  geom_errorbar(data = ammonium_summary,
                aes(x = Gebiedsnaam, 
                    ymin = mean_nh4 - sd_nh4, 
                    ymax = mean_nh4 + sd_nh4),
                width = 0.2, color = "black") +
  theme_minimal(base_size = 15) +
  theme(
    strip.background = element_blank(),
    strip.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 14, vjust = 0.5, hjust = 1, angle = 90),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.ticks = element_line(colour = "black"),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    plot.background = element_blank(),
    legend.position = "right",
    legend.box.just = "center"
  ) +
  ggtitle('Ammonium concentratie per gebied') +
  labs(x = 'Gebied', y = 'µmol/l')
ggsave(file=paste0('output/AlleGebieden/Tussenrapportage/nh4_tox.png'), width = 25,height = 15,units='cm',dpi=800)

### Ammonium toxicity Lisanne locations ------------------------

# Calculate summary statistics first
ammonium_summary_lis <- locslis[!is.na(Gebiedsnaam) & !is.na(`NH4_µmol/l_PW`), .(
  mean_nh4 = mean(`NH4_µmol/l_PW`, na.rm = TRUE),
  sd_nh4 = sd(`NH4_µmol/l_PW`, na.rm = TRUE)
), by = .(Gebiedsnaam, SlootID)]
# Sorteer
sort_order_lis <- ammonium_summary_lis[order(mean_nh4)]
ammonium_summary_lis[, Gebiedsnaam := factor(Gebiedsnaam, levels = unique(sort_order_lis$Gebiedsnaam))]
ggplot() +
  geom_rect(data = ammonium, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), 
            inherit.aes = FALSE, alpha = 0.25) +
  scale_fill_identity('Giftig:', breaks = legend_colors, 
                     labels = c("niet", "voor gevoelige soorten","voor veel soorten", 
                                "voor bijna alle soorten", "voor alle soorten"), 
                     guide = guide_legend(override.aes = list(alpha = 0.15))) +
  geom_col(data = locslis[!is.na(SlootID) & !is.na(`NH4_µmol/l_PW`),],
           aes(x = SlootID, y = `NH4_µmol/l_PW`), 
           fill = "#1B9E77", col = "#1B9E77",alpha = 0.2, position = 'dodge') +
  theme_minimal(base_size = 15) +
  theme(
    strip.background = element_blank(),
    strip.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 14, vjust = 0.5, hjust = 1, angle = 90),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.ticks = element_line(colour = "black"),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    plot.background = element_blank(),
    legend.position = "right",
    legend.box.just = "center"
  ) +
  ggtitle('Ammonium concentratie Lisanne locaties') +
  labs(x = 'Gebied', y = 'µmol/l')

### Other parameters Lisanne locations ------------------------
ggplot() +
  geom_boxplot(data = locslis[!is.na(SlootID) & !is.na(`NH4_µmol/l_PW`),],
           aes(x = Gebiedsnaam, y = `bulk density_kg DW/L FW_SB`), 
           fill = "#1B9E77", col = "#1B9E77",alpha = 0.2) +
  theme_minimal(base_size = 15) +
  theme(
    strip.background = element_blank(),
    strip.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 14, vjust = 0.5, hjust = 1, angle = 90),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.ticks = element_line(colour = "black"),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    plot.background = element_blank(),
    legend.position = "right",
    legend.box.just = "center"
  ) +
  ggtitle('') +
  labs(x = 'Gebied', y = '')

## bulk density, pH, organic fraction, water content and redox potential-----------------------------
abio_proj[, bulkdensity_kgDW_L_FW_SB := as.numeric(`bulk density_kg DW/L FW_SB`)]
abio_proj[, OS_SB := `OS gehalte_%_SB`]
abio_proj[, vochtgeh_SB := `vochtgeh_%_SB`]
params <- c('bulkdensity_kgDW_L_FW_SB', 'pH_CC_SB', 'OS_SB', 'vochtgeh_SB', 'slib_redox_pH7','zichtdiepte')

for (param in params) {
  p <- ggplot() +
    geom_boxplot(data = abio_proj[!is.na(SlootID) & !is.na(get(param)),],
                 aes_string(x = 'Gebiedsnaam', y = param), 
                 fill = "#1B9E77", col = "#1B9E77", alpha = 0.2) +
    theme_minimal(base_size = 15) +
    theme(
      strip.background = element_blank(),
      strip.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 14, vjust = 0.5, hjust = 1, angle = 90),
      axis.text.y = element_text(size = 14),
      axis.title = element_text(size = 14),
      axis.ticks = element_line(colour = "black"),
      axis.line = element_line(colour = 'black'),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      panel.background = element_blank(),
      panel.border = element_rect(colour = 'black', fill = NA),
      plot.background = element_blank(),
      legend.position = "right",
      legend.box.just = "center"
    ) +
    ggtitle(paste(param, 'Lisanne locaties')) +
    labs(x = 'SlootID', y = param)
  
  print(p)
}

# grafiek zichtdiepte met locs lisanne uitgelicht
abio_proj[zichtdiepte > 1, zichtdiepte := 1]
p_zicht <- ggplot() +
  geom_boxplot(data = abio_proj[!is.na(SlootID) & !is.na(zichtdiepte),],
               aes(x = Gebiedsnaam, y = zichtdiepte), 
               fill = "#1B9E77", col = "#1B9E77", alpha = 0.2) +
  geom_jitter(data = locslis[!is.na(SlootID) & !is.na(zichtdiepte),],
               aes(x = Gebiedsnaam, y = zichtdiepte),
               color = "red", size = 2, width = 0.2) +
  geom_hline(yintercept = 0.60, linetype = "dashed", color = "red") +
  theme_minimal(base_size = 15) +
  theme(
    strip.background = element_blank(),
    strip.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 14, vjust = 0.5, hjust = 1, angle = 90),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.ticks = element_line(colour = "black"),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    plot.background = element_blank(),
    legend.position = "right",
    legend.box.just = "center"
  ) +
  ggtitle('Zichtdiepte met Lisanne locaties uitgelicht') +
  labs(x = 'Gebiedsnaam', y = 'zichtdiepte')
print(p_zicht)
