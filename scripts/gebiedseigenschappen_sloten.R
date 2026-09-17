# Gebiedseigenschappen sloten------------------------------------------------------------------------------
# Losstaand script voor de figuren in de rapportbijlage "Gebiedseigenschappen
# sloten" (rapport_modellering_VeeST.qmd). Oorspronkelijk onderdeel van
# scripts/visualisatiesEnExport.R; hierheen verplaatst zodat het los gedraaid kan
# worden zonder de rest van visualisatiesEnExport.R.
# Draai dit script vóór het renderen van het rapport, in dezelfde sessie/volgorde
# als main_veest.R -> Analyses_dev_db.R -> gebiedseigenschappen_sloten.R, zodat de
# benodigde dataframes abio_proj, penmerge en melt al in de environment staan.
# Het rapport zelf source't dit script niet: het leest alleen de acht .rds-bestanden
# hieronder in vanuit rds_dir.
# Output: acht ggplot-objecten (p_bodemtype_classificatie,
# p_bodemfractie_gebied, p_ir_egv, p_draagkracht_diepte_veentype,
# p_doorzicht_waterdiepte_slib, p_redox_slib_water, p_ammonium_toxiciteit,
# p_p_nalevering_gebied), telkens direct opgeslagen als .rds in rds_dir.

library(data.table)
library(ggplot2)
library(patchwork)
library(ggrepel)

workspace <- paste0(Sys.getenv("NMI-SITE"), 'O 1900 - O 2000/1922.N.23 VeeST vwsloot vd toekomst/05. Data/')
rds_dir <- paste0(workspace, "output/rapport/")


# Standaard figurethema (consistente opmaak voor alle grafieken)-----------------------------------------------
theme_figuur <- theme_minimal(base_size = 15) +
  theme(
    axis.text.x   = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y   = element_text(size = 14),
    axis.title    = element_text(size = 14),
    axis.ticks    = element_line(colour = "black"),
    axis.line     = element_line(colour = "black"),
    plot.title    = element_text(size = 16, face = "bold", hjust = 0.5),
    panel.background = element_blank(),
    panel.border  = element_rect(colour = "black", fill = NA),
    plot.background  = element_blank()
  )

## 1. Doorzicht, waterdiepte & slibdikte per gebied, gefacetteerd op waterschap ----------------
abio_proj[,slibdiepte := max_slib + max_wtd]
abio_proj[,doorzicht2_mid_m :=  doorzicht2_mid_cm/100]
median_slibdiepte <- median(abio_proj$slibdiepte, na.rm = TRUE)
median_max_wtd <- median(abio_proj$max_wtd, na.rm = TRUE)
median_doorzicht <- median(abio_proj$doorzicht2_mid_m, na.rm = TRUE)
abio_proj_cast <- dcast(abio_proj, Gebiedsnaam+waterschap ~ ., value.var = c('slibdiepte','max_slib','max_wtd','doorzicht2_mid_m','waterzone_1_subm_tot_perc'), fun.aggregate = mean, na.rm=TRUE)
abio_proj_cast[,zichtdiepte:= doorzicht2_mid_m/max_wtd]

# Sorteer de data op slibdiepte en zet Gebiedsnaam om naar een factor met de juiste volgorde
plot_data <- abio_proj_cast[!is.na(Gebiedsnaam), ]
plot_data <- plot_data[order(slibdiepte)]
plot_data[, Gebiedsnaam := factor(Gebiedsnaam, levels = unique(Gebiedsnaam))]

ggplot(data = plot_data) +
    geom_col(aes(x= Gebiedsnaam, y = -1*slibdiepte, fill = 'slibdikte (m)'),alpha = 0.7) +
    geom_col(aes(x= Gebiedsnaam, y = -1*max_wtd, fill = 'maximale waterdiepte (m)'),alpha = 0.8) +
    geom_col(aes(x= Gebiedsnaam, y = -1*doorzicht2_mid_m, fill = 'doorzicht (m)'),alpha = 0.8) +
    # Add median lines
    geom_hline(yintercept = -1*median_slibdiepte, color = "brown", linetype = "dashed", size = 1) +
    geom_hline(yintercept = -1*median_max_wtd, color = "skyblue", linetype = "dashed", size = 1) +
    geom_hline(yintercept = -1*median_doorzicht, color = "darkblue", linetype = "dashed", size = 1) +
    scale_fill_manual(values = c("darkblue","skyblue","brown"), na.value = "#A6761D")+
    facet_grid(. ~ waterschap, space = 'free_x', scales = 'free_x', switch = 'x',
               labeller = labeller(waterschap = label_wrap_gen(width = 15)))+
    theme_minimal(base_size = 15)+
    theme(
      strip.background = element_blank(),
      strip.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 15, vjust = 0.8, hjust =1, angle = 90),
      axis.text.y = element_text(size = 15),
      axis.title = element_text(size= 15),
      axis.ticks =  element_line(colour = "black"),
      axis.line = element_line(colour='black'),
      plot.title = element_text(size =18, face="bold", hjust = 0.5),
      panel.background = element_blank(),
      panel.border = element_rect(colour='black', fill = NA),
      plot.background = element_blank(),
      legend.position = "bottom",
      legend.box.just = "center"
    )+
    guides(fill = guide_legend(title = '', title.vjust = 1))+
    guides(color = guide_legend(title = ''))+
    ggtitle(paste0("Doorzicht, waterdiepte & slibdikte")) +
    labs(x= 'Gebied' , y= 'meter') -> p_doorzicht_waterdiepte_slib

p_doorzicht_waterdiepte_slib
saveRDS(p_doorzicht_waterdiepte_slib, paste0(rds_dir, "p_doorzicht_waterdiepte_slib.rds"))

## 2. Draagkracht over diepte per veentype -----------------------------------
penmerge_plot <- penmerge[!is.na(indringingsweerstand) & !is.na(Diept) & !is.na(veentype) & Diept <= 80 & !is.na(Gebiedsnaam) & sectie_f %in% c("oever", "perceel"),]
drooglg_data <- unique(abio_proj[, .(SlootID, drglg, jaar)])
penmerge_plot <- merge(penmerge_plot, drooglg_data, by = c("SlootID", "jaar"), all.x = TRUE)
penmerge_plot[, diepte_5cm := round(Diept/5)*5]
penmerge_gemiddelde_veentype <- penmerge_plot[!is.na(indringingsweerstand),
                                              .(gemiddelde_draagkracht = mean(indringingsweerstand, na.rm = TRUE),
                                                n_metingen = .N),
                                              by = .(veentype, diepte_5cm, jaar, sectie_f)]
drglg_bandbreedte_per_veentype <- penmerge_plot[!is.na(drglg) & !is.na(veentype),
                                               .(min_drglg = min(drglg, na.rm = TRUE),
                                                 max_drglg = max(drglg, na.rm = TRUE)),
                                               by = .(veentype, sectie_f)]
kritieke_draagkracht <- 0.5
plot_lines <- penmerge_gemiddelde_veentype[
  !is.na(gemiddelde_draagkracht)
][order(sectie_f, veentype, jaar, diepte_5cm)]

ggplot() +
  # DROOGLEGGING BANDBREEDTE ALS LICHTBLAUWE ACHTERGROND
  geom_rect(data = drglg_bandbreedte_per_veentype,
            aes(xmin = -Inf, xmax = Inf,
                ymin = min_drglg * 100, ymax = max_drglg * 100),
            fill = "lightblue", alpha = 0.3, inherit.aes = FALSE) +

  # ALLE PUNTEN IN LICHTGRIJS ZONDER FILL
  geom_point(data = penmerge_plot,
             aes(x = indringingsweerstand, y = Diept),
             color = "lightgrey", alpha = 0.6, size = 1.2) +

  # Gemiddelde lijn per veentype
  geom_path(
    data = plot_lines,
    aes(
      x = gemiddelde_draagkracht,
      y = diepte_5cm,
      color = factor(jaar),
      group = interaction(jaar, sectie_f, veentype)
    ),
    linetype = "dashed",
    linewidth = 1.2
  ) +
  geom_point(
    data = plot_lines,
    aes(x = gemiddelde_draagkracht, y = diepte_5cm, color = factor(jaar)),
    size = 2.5, shape = 17
  ) +

  # Facet per veentype met BETERE LABELS
  facet_wrap(sectie_f~factor(veentype, levels = c("kleiig veen", "veenmosveen",
                                          "zeggerietveen_rietveen", "zeggeveen_rietzeggeveen_broekveen",
                                          "broekveen", "bagger_verslagenveen_gyttja_anders")),
             ncol = 5,
             scales = "free_x",
             labeller = labeller(.default = function(x) {
               case_when(
                 x == "kleiig veen" ~ "Kleiig veen",
                 x == "veenmosveen" ~ "Veenmosveen",
                 x == "zeggerietveen_rietveen" ~ "Zegge- en rietveen",
                 x == "zeggeveen_rietzeggeveen_broekveen" ~ "Zegge-, rietzegge-\nen broekveen",
                 x == "broekveen" ~ "Broekveen",
                 x == "bagger_verslagenveen_gyttja_anders" ~ "Bagger, verslage veen,\ngyttja en overig",
                 TRUE ~ as.character(x)
               )
             })) +

  # Y-as omgekeerd (diepte)
  scale_y_reverse(
    name = "Diepte (cm)",
    breaks = seq(0, 80, 20),
    limits = c(80, 0)
  ) +

  # Kritieke draagkracht lijn
  geom_vline(xintercept = kritieke_draagkracht, color = "red", linetype = "dotted", size = 1) +

  # Thema en styling - AANGEPASTE STRIP TEXT GROOTTE
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13),
    strip.text = element_text(size = 11, face = "bold", lineheight = 0.9),  # Kleinere tekst, betere lijnafstand
    legend.position = "bottom",
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    strip.background = element_rect(colour = "black", fill = "white", linewidth = 0.8),
    panel.grid.major = element_line(color = "grey90", size = 0.5),
    panel.grid.minor = element_blank()
  ) +

  # Kritieke waarde annotatie
  annotate("text", x = kritieke_draagkracht, y = 5,
           label = "Kritiek",
           hjust = 0, vjust = 1, size = 3,
           color = "red", fontface = "bold") +

  # Titels en labels
  labs(
    title = "Draagkracht over Diepte per Veentype",
    subtitle = "Zwarte stippellijn = gemiddelde per veentype\nLichtblauwe zone = drooglegging bandbreedte\nRode stippellijn = kritische draagkracht voor beweiding",
    caption = paste0("Gebaseerd op ", nrow(penmerge_plot), " oevermetingen, diepte 0-80 cm")
  ) -> p_draagkracht_diepte_veentype

p_draagkracht_diepte_veentype
saveRDS(p_draagkracht_diepte_veentype, paste0(rds_dir, "p_draagkracht_diepte_veentype.rds"))

## 3. Basis voor bodemtype classificatie sloot + fractiebalk per gebied ------
bodem_long <- melt(
  unique(abio_proj[, .(SlootID_kort, Gebiedsnaam, Z_CLAY_SA_OR_25, OS_perc_OR_25,
                        Z_SAND_SA_OR_25, Z_SILT_SA_OR_25)]),
  id.vars       = c("SlootID_kort", "Gebiedsnaam"),
  measure.vars  = c("Z_CLAY_SA_OR_25", "OS_perc_OR_25", "Z_SAND_SA_OR_25", "Z_SILT_SA_OR_25"
),
  variable.name = "bodem_var",
  value.name    = "value"
)
bodem_long[, bodem_label := fcase(
  bodem_var == "Z_CLAY_SA_OR_25", "Kleigehalte (%)",
  bodem_var == "OS_perc_OR_25",   "Organisch stofgehalte (%)",
  bodem_var == "Z_SAND_SA_OR_25", "Zandgehalte (%)",
  bodem_var == "Z_SILT_SA_OR_25", "Siltgehalte (%)"
)]
# Bodemtype classificatie per SlootID_kort
bodem_wide <- dcast(
  unique(bodem_long),
  SlootID_kort + Gebiedsnaam ~ bodem_var,
  value.var = "value",
  fun.aggregate = function(x) mean(x, na.rm = TRUE)
)
bodem_wide[, totaal := Z_CLAY_SA_OR_25 + OS_perc_OR_25 + Z_SAND_SA_OR_25 + Z_SILT_SA_OR_25]
bodem_wide <- bodem_wide[!is.na(OS_perc_OR_25) & !is.na(Z_CLAY_SA_OR_25),]
bodem_wide[, bodemtype := fcase(
  Z_CLAY_SA_OR_25 <  17 & OS_perc_OR_25 >= 16, "Veen",
  Z_CLAY_SA_OR_25 >= 17 & OS_perc_OR_25 >= 16, "Klei-in-veen",
  Z_CLAY_SA_OR_25 >= 35 & OS_perc_OR_25 <  16, "Klei",
  Z_CLAY_SA_OR_25 >= 17 & OS_perc_OR_25 <  16, "Moerige klei",
  Z_CLAY_SA_OR_25 <  17 & OS_perc_OR_25 >= 10, "Moerige grond",
  Z_CLAY_SA_OR_25 <  17 & OS_perc_OR_25 <  10, "Zand/leem",
  default = "Onbekend"
)]

bodemtype_colors <- c(
  "Veen"          = "#8B4513",
  "Klei-in-veen"  = "#9ACD32",
  "Klei"          = "#4682B4",
  "Moerige klei"  = "#6495ED",
  "Moerige grond" = "#F4A460",
  "Zand/leem"     = "#D3D3D3",
  "Onbekend"      = "grey70"
)

## Labels voor een selectie van gebieden, zodat niet elk punt een gebiedslabel
## krijgt: (a) sloten dicht bij een classificatiegrens (x=17, y=16 of y=10),
## en (b) de meest extreme sloten op klei- en organisch-stofgehalte (rechts en
## boven in de plot), die anders zonder label bleven omdat ze niet dicht bij
## een grens liggen. Per Gebiedsnaam wordt maximaal één punt gelabeld (het
## meest kenmerkende: kleinste afstand tot een grens, hoogste kleigehalte of
## hoogste OS%), om overlappende labels van hetzelfde gebied te voorkomen.
bodem_wide_lbl <- bodem_wide[!is.na(Z_CLAY_SA_OR_25) & !is.na(OS_perc_OR_25)]
bodem_wide_lbl[, afstand_grens := pmin(
  abs(Z_CLAY_SA_OR_25 - 17),
  abs(OS_perc_OR_25 - 16),
  abs(OS_perc_OR_25 - 10)
)]
grens_labels   <- bodem_wide_lbl[order(afstand_grens)][1:min(10, .N)]
hoge_klei      <- bodem_wide_lbl[order(-Z_CLAY_SA_OR_25)][1:min(5, .N)]
hoge_os        <- bodem_wide_lbl[order(-OS_perc_OR_25)][1:min(5, .N)]
label_kandidaten <- unique(rbind(grens_labels, hoge_klei, hoge_os), by = "SlootID_kort")
# Eén label per Gebiedsnaam: de rij met de kleinste afstand tot een grens
# heeft voorrang; ontbreekt dat (want geselecteerd via hoge klei/OS), dan het
# punt met de meest extreme klei- of OS-waarde binnen dat gebied.
label_kandidaten[, prioriteit := pmin(
  afstand_grens,
  fifelse(SlootID_kort %in% hoge_klei$SlootID_kort, 0, Inf),
  fifelse(SlootID_kort %in% hoge_os$SlootID_kort, 0, Inf)
)]
grens_labels <- label_kandidaten[order(prioriteit)][!duplicated(Gebiedsnaam)]

p_bodemtype_classificatie <- ggplot(bodem_wide_lbl,
       aes(x = Z_CLAY_SA_OR_25, y = OS_perc_OR_25, color = bodemtype)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_vline(xintercept = 17, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 16, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 10, linetype = "dotted", color = "grey60") +
  ggrepel::geom_text_repel(
    data = grens_labels,
    aes(label = Gebiedsnaam),
    size = 3.5, color = "grey15", fontface = "bold",
    bg.color = "white", bg.r = 0.15,
    max.overlaps = Inf, min.segment.length = 0,
    box.padding = 0.4, point.padding = 0.3,
    segment.color = "grey50", segment.size = 0.3,
    seed = 4218
  ) +
  annotate("text", x = 3,  y = 55, label = "Veen",          fontface = "bold", color = "#8B4513") +
  annotate("text", x = 30, y = 55, label = "Klei-in-veen",  fontface = "bold", color = "#9ACD32") +
  annotate("text", x = 30, y = 8,  label = "Moerige klei",  fontface = "bold", color = "#4682B4") +
  annotate("text", x = 3,  y = 13, label = "Moerige grond", fontface = "bold", color = "#F4A460") +
  annotate("text", x = 3,  y = 3,  label = "Zand/leem",     fontface = "bold", color = "grey50") +
  scale_color_manual(values = bodemtype_colors) +
  labs(
    x     = "Kleigehalte (%)",
    y     = "Organisch stofgehalte (%)",
    color = "Bodemtype",
    title = "Basis voor bodemtype classificatie per sloot",
    caption = "Labels: per gebied het meest kenmerkende punt (dicht bij een classificatiegrens, of het hoogste klei- of organisch-stofgehalte)."
  ) +
  theme_figuur

p_bodemtype_classificatie
saveRDS(p_bodemtype_classificatie, paste0(rds_dir, "p_bodemtype_classificatie.rds"))

#### Fractie klei-veen-zand-moerig-overig per gebied (gestapelde 100% balk) ------------------------
## Bodemtype per sloot (bodem_wide, o.b.v. gemiddelde klei%/OS% per SlootID_kort) wordt
## samengevoegd tot 6 hoofdklassen; per gebied wordt vervolgens de fractie sloten per
## hoofdklasse berekend en als 100%-gestapelde balk weergegeven.
## Aannames bij de samenvoeging (te herzien indien gewenst):
##  - Klei-in-veen  -> eigen klasse (zelfde OS-drempel >=16% als Veen; alleen kleigehalte
##                     is hoger, dus inhoudelijk een tussenvorm en niet gelijk aan Klei)
##  - Moerige klei  -> Moerig
##  - Moerige grond -> Moerig
##  - Zand/leem     -> Zand
##  - Onbekend      -> Overig
bodem_wide[, bodemfractie_klasse := fcase(
  bodemtype == "Veen",          "Veen",
  bodemtype == "Klei-in-veen",  "Klei-in-veen",
  bodemtype == "Klei",          "Klei",
  bodemtype == "Moerige klei",  "Moerig",
  bodemtype == "Moerige grond", "Moerig",
  bodemtype == "Zand/leem",     "Zand",
  default = "Overig"
)]

bodemfractie_klasse_colors <- c(
  "Klei"         = "#4682B4",
  "Klei-in-veen" = "#9ACD32",
  "Veen"         = "#8B4513",
  "Zand"         = "#D3D3D3",
  "Moerig"       = "#6495ED",
  "Overig"       = "grey70"
)

bodem_fractie_gebied <- bodem_wide[!is.na(Gebiedsnaam) & !is.na(bodemfractie_klasse),
  .(n = .N), by = .(Gebiedsnaam, bodemfractie_klasse)]
bodem_fractie_gebied[, fractie := n / sum(n), by = Gebiedsnaam]
bodem_fractie_gebied[, bodemfractie_klasse := factor(
  bodemfractie_klasse, levels = c("Klei", "Klei-in-veen", "Veen", "Zand", "Moerig", "Overig")
)]
# Sorteer gebieden op aandeel veen (aflopend)
veen_order <- bodem_fractie_gebied[bodemfractie_klasse == "Veen"][order(-fractie)]
gebied_levels_fractie <- c(
  veen_order$Gebiedsnaam,
  setdiff(unique(bodem_fractie_gebied$Gebiedsnaam), veen_order$Gebiedsnaam)
)
bodem_fractie_gebied[, Gebiedsnaam := factor(Gebiedsnaam, levels = gebied_levels_fractie)]

p_bodemfractie_gebied <- ggplot(bodem_fractie_gebied,
       aes(x = Gebiedsnaam, y = fractie, fill = bodemfractie_klasse)) +
  geom_col(width = 0.8) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
  scale_fill_manual(values = bodemfractie_klasse_colors, name = "Bodemklasse") +
  labs(
    x = "Gebiedsnaam",
    y = "Fractie sloten",
    title = "Bodemsamenstelling per gebied (klei / klei-in-veen / veen / zand / moerig / overig)",
    caption = "Fractie sloten per gebied o.b.v. bodemtype-classificatie per sloot (gemiddelde klei%/OS% per sloot)."
  ) +
  theme_figuur +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_bodemfractie_gebied
saveRDS(p_bodemfractie_gebied, paste0(rds_dir, "p_bodemfractie_gebied.rds"))

## 4. IR-EGV diagram (gesorteerd op IR) ---------------------------------------
LATframework <- fread(paste0(workspace,"/hulp_tabellen/coordinates_LAT_framework.csv"))
referencepoints <- fread(paste0(workspace,"/hulp_tabellen/reference.points.csv"))

egv_long <- melt(
  abio_proj,
  id.vars = c("SlootID", "Gebiedsnaam"),
  measure.vars = list(
    Ca = c("Ca_µmol/l_OW", "Ca_µmol/l_PW"),
    Cl = c("Cl_µmol/l_OW", "Cl_µmol/l_PW"),
    EGV = c("EGV_µs/cm_OW", "EGV_µs/cm_PW")
  ),
  variable.name = "compartiment"
)

egv_long[, compartiment := fifelse(compartiment == 1, "OW", "PW")]
egv_long[, Ca_meq_l := Ca * 2 / 1000]
egv_long[, Cl_meq_l := Cl / 1000]
egv_long[, IR := Ca_meq_l / (Ca_meq_l + Cl_meq_l)]
egv_long <- egv_long[!is.na(IR) & !is.na(EGV) & !is.na(Gebiedsnaam)]

# Sorteervolgorde op IR (laag -> hoog)
ir_order <- egv_long[
  !is.na(IR) & !is.na(Gebiedsnaam),
  .(IR_med = median(IR, na.rm = TRUE)),
  by = Gebiedsnaam
][order(IR_med)]

egv_long[, Gebiedsnaam := factor(Gebiedsnaam, levels = ir_order$Gebiedsnaam)]

gebied_kleuren <- setNames(
  colorspace::qualitative_hcl(nlevels(egv_long$Gebiedsnaam), palette = "Dark 3"),
  levels(egv_long$Gebiedsnaam)
)

p_ir_egv <- ggplot(
  egv_long[!is.na(IR) & !is.na(EGV)],
  aes(x = EGV, y = IR, color = Gebiedsnaam, shape = compartiment)
) +
  geom_point(size = 4, alpha = 0.7) +
  geom_path(
    data = LATframework,
    aes(x = EC25 * 10, y = IR / 100),
    inherit.aes = FALSE,
    linetype = "dotdash",
    linewidth = 0.7
  ) +
  geom_text(
    data = referencepoints,
    aes(x = EC25 * 10, y = IR / 100, label = Name),
    inherit.aes = FALSE,
    size = 3
  ) +
  scale_x_log10(name = "EGV (µS/cm)") +
  scale_y_continuous(name = "IR-ratio (Ca/(Ca+Cl))", limits = c(0, 1)) +
  scale_color_manual(values = gebied_kleuren, name = "Gebied") +
  scale_shape_manual(values = c(16, 17), name = "Compartiment",
                     labels = c("Oppervlaktewater", "Poriewater")) +
  theme_minimal(base_size = 15)

p_ir_egv
saveRDS(p_ir_egv, paste0(rds_dir, "p_ir_egv.rds"))

## 5. Redoxpotentiaal slib en water per gebied --------------------------------
redox_slib_summary <- abio_proj[!is.na(Gebiedsnaam) & !is.na(slib_redox_pH7), .(
  median_redox = median(slib_redox_pH7, na.rm = TRUE),
  sd_redox = sd(slib_redox_pH7, na.rm = TRUE),
  q25_redox = quantile(slib_redox_pH7, 0.25, na.rm = TRUE),
  q75_redox = quantile(slib_redox_pH7, 0.75, na.rm = TRUE),
  min_redox = min(slib_redox_pH7, na.rm = TRUE),
  max_redox = max(slib_redox_pH7, na.rm = TRUE)
), by = Gebiedsnaam]
redox_slib_summary[, iqr_redox := q75_redox - q25_redox]
redox_slib_summary[, `:=`(
  whisker_lower = pmax(min_redox, q25_redox - 1.5 * iqr_redox),
  whisker_upper = pmin(max_redox, q75_redox + 1.5 * iqr_redox),
  compartiment = "Slib"
)]
redox_water_summary <- abio_proj[!is.na(Gebiedsnaam) & !is.na(water_redox_pH7), .(
  median_redox = median(water_redox_pH7, na.rm = TRUE),
  sd_redox = sd(water_redox_pH7, na.rm = TRUE),
  q25_redox = quantile(water_redox_pH7, 0.25, na.rm = TRUE),
  q75_redox = quantile(water_redox_pH7, 0.75, na.rm = TRUE),
  min_redox = min(water_redox_pH7, na.rm = TRUE),
  max_redox = max(water_redox_pH7, na.rm = TRUE)
), by = Gebiedsnaam]
redox_water_summary[, iqr_redox := q75_redox - q25_redox]
redox_water_summary[, `:=`(
  whisker_lower = pmax(min_redox, q25_redox - 1.5 * iqr_redox),
  whisker_upper = pmin(max_redox, q75_redox + 1.5 * iqr_redox),
  compartiment = "Water"
)]

redox_combined <- rbind(redox_slib_summary, redox_water_summary)

sort_order <- redox_slib_summary[order(median_redox)]
redox_combined[, Gebiedsnaam := factor(Gebiedsnaam, levels = sort_order$Gebiedsnaam)]

rects <- data.frame(xmin = -Inf,
                    xmax = Inf,
                    ymin = c(-Inf,-250,-100,0,200,300),
                    ymax = c(-250,-100,0,200,300, 800),
                    fill = c("#8B0000", "#FF4500", "#FFB347", "#d0ff00ff", "#4169E1","#062992ff"),
                    label = c("methonogenese", "sulfaatreductie","ijzeroxidereductie","mangaanreductie","denitrificatie","zuurstofreductie"))

legend_colors <- setNames(c("#062992ff", "#4169E1", "#d0ff00ff", "#FFB347", "#FF4500", "#8B0000"),
                         c("oxisch", "nitraatreductie", "mangaanreductie", "ijzeroxidereductie", "sulfaatreductie", "methonogenese"))

ggplot() +
  geom_rect(data = rects, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
            inherit.aes = FALSE, alpha = 0.25) +
  scale_fill_identity('Redoxtoestand:',
                      breaks = legend_colors,
                      labels = c("Zuurstofreductie", "Denitrificatie", "Mangaanreductie","IJzeroxide reductie", "Sulfaatreductie", "Methonogenese"),
                      guide = guide_legend(override.aes = list(alpha = 0.25))) +

  geom_col(data = redox_combined,
           aes(x = Gebiedsnaam, y = median_redox),
           fill = "#7570B3", alpha = 0.7) +

  geom_errorbar(data = redox_combined,
                aes(x = Gebiedsnaam,
                    ymin = whisker_lower,
                    ymax = whisker_upper),
                width = 0.2, color = "black", size = 0.8) +

  geom_point(data = melt[variable %in% c("slib_redox_pH7") & !is.na(Gebiedsnaam),],
             aes(x = Gebiedsnaam, y = `gemiddelde VeeST`, col = '* gemiddelde VeeST'),
             shape = 95, size = 10) +

  geom_point(data = melt[variable %in% c("water_redox_pH7") & !is.na(Gebiedsnaam),],
             aes(x = Gebiedsnaam, y = `gemiddelde VeeST`, col = '* gemiddelde VeeST'),
             shape = 95, size = 10) +

  scale_colour_manual(values = c('grey2')) +

  facet_wrap(~ compartiment, scales = "free_x", ncol = 2) +

  coord_flip() +

  theme_minimal(base_size = 15) +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.ticks = element_line(colour = "black"),
    axis.line = element_line(colour = 'black'),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA),
    plot.background = element_blank(),
    legend.position = "right",
    legend.box.just = "center",
    strip.background = element_rect(colour = "black", fill = "white", linewidth = 0.8)
  ) +
  guides(col = guide_legend(title = ''), fill = guide_legend(title = 'Redoxtoestand')) +

  labs(
    title = "Redoxpotentiaal in slib en water per gebied",
    subtitle = "Errorbars tonen whisker-range (Q1-1.5×IQR tot Q3+1.5×IQR)",
    x = 'Gebied',
    y = 'mV (bij pH7)'
  ) -> p_redox_slib_water

p_redox_slib_water
saveRDS(p_redox_slib_water, paste0(rds_dir, "p_redox_slib_water.rds"))

## 6. Ammoniumtoxiciteit per gebied -------------------------------------------
ammonium <- data.frame(xmin = -Inf,
                    xmax = Inf,
                    ymin = c(0,100,400,750,5000),
                    ymax = c(100,400,750,5000,Inf),
                    fill = c("green","yellow", "orange","red","purple"),
                    label = c("niet", "voor gevoelige soorten","voor veel soorten","voor bijna alle soorten","voor alle soorten"))

legend_colors <- setNames(c("green","yellow", "orange","red","purple"), ammonium$label)
setDT(abio_proj)
ammonium_summary <- abio_proj[
  !is.na(Gebiedsnaam) & !is.na(`NH4_µmol/l_PW`),
  .(
    mean_nh4 = median(`NH4_µmol/l_PW`, na.rm = TRUE),
    sd_nh4   = sd(`NH4_µmol/l_PW`, na.rm = TRUE)
  ),
  by = Gebiedsnaam
]

sort_order <- ammonium_summary[order(mean_nh4)]
abio_proj[, Gebiedsnaam := factor(Gebiedsnaam, levels = unique(sort_order$Gebiedsnaam))]

ggplot() +
  geom_rect(data = ammonium, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
            inherit.aes = FALSE, alpha = 0.25) +
  scale_fill_identity('Giftig:', breaks = legend_colors,
                     labels = c("niet", "voor gevoelige soorten", "voor veel soorten",
                               "voor bijna alle soorten", "voor alle soorten"),
                     guide = guide_legend(override.aes = list(alpha = 0.15))) +
  geom_boxplot(data = abio_proj[!is.na(`NH4_µmol/l_PW`),],
               aes(x = Gebiedsnaam, y = `NH4_µmol/l_PW`),
               outlier.shape = NA, width=0.6, fill="#1B9E77", alpha=0.7) +
  coord_flip() +
  scale_y_log10(
    name = "Ammonium concentratie (µmol/l)",
    breaks = c(1,10,100,250,500,1000,5000,10000),
    labels = c(1,10,100,250,500,1000,5000,10000)
  ) +
  theme_minimal(base_size = 15) +
  theme(
    strip.background = element_blank(),
    strip.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 14),
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
  labs(x = 'Gebied', y = 'µmol/l') -> p_ammonium_toxiciteit

p_ammonium_toxiciteit
saveRDS(p_ammonium_toxiciteit, paste0(rds_dir, "p_ammonium_toxiciteit.rds"))

## 7. P-nalevering uit slib naar water en Fe-ratio's per gebied ---------------
abio_proj[, `:=`(
  P_nalevering_formule1 = 0.00004807 * (`P_µmol/l_PW`)^2 + 0.03344949 * (`P_µmol/l_PW`),
  P_nalevering_formule2 = fifelse(
    feP_PW < 3,
    0.00012907 * (`P_µmol/l_PW`)^2 + 0.00055877 * (`P_µmol/l_PW`),
    -0.00002   * (`P_µmol/l_PW`)^2 + 0.004       * (`P_µmol/l_PW`)
  ),
  P_nalevering_baggernut = 0.80951 * `P_mg_l_PW` - 0.2905
)]
abio_proj[, O2_category := fifelse(
  water_O2_mgL > 2.5,
  "Zuurstofrijk (>2.5 mg/l)",
  "Zuurstofarm (≤2.5 mg/l)"
)]
p_nalevering_summary <- abio_proj[!is.na(P_nalevering_formule1) & !is.na(P_nalevering_formule2) & !is.na(P_nalevering_baggernut) & !is.na(Gebiedsnaam), .(
  mean_f1 = mean(P_nalevering_formule1, na.rm = TRUE),
  mean_f2 = mean(P_nalevering_formule2, na.rm = TRUE),
  mean_baggernut = mean(P_nalevering_baggernut, na.rm = TRUE),
  q25_f1 = quantile(P_nalevering_formule1, 0.25, na.rm = TRUE),
  q75_f1 = quantile(P_nalevering_formule1, 0.75, na.rm = TRUE),
  min_f1 = min(P_nalevering_formule1, na.rm = TRUE),
  max_f1 = max(P_nalevering_formule1, na.rm = TRUE),
  q25_f2 = quantile(P_nalevering_formule2, 0.25, na.rm = TRUE),
  q75_f2 = quantile(P_nalevering_formule2, 0.75, na.rm = TRUE),
  min_f2 = min(P_nalevering_formule2, na.rm = TRUE),
  max_f2 = max(P_nalevering_formule2, na.rm = TRUE),
  q25_baggernut = quantile(P_nalevering_baggernut, 0.25, na.rm = TRUE),
  q75_baggernut = quantile(P_nalevering_baggernut, 0.75, na.rm = TRUE),
  min_baggernut = min(P_nalevering_baggernut, na.rm = TRUE),
  max_baggernut = max(P_nalevering_baggernut, na.rm = TRUE),
  mean_Fe_P_ratio = mean(feP_PW, na.rm = TRUE),
  mean_O2 = mean(water_O2_mgL, na.rm = TRUE),
  n_obs = .N
), by = Gebiedsnaam]
p_nalevering_summary[, `:=`(
  iqr_f1 = q75_f1 - q25_f1,
  whisker_lower_f1 = pmax(min_f1, q25_f1 - 1.5 * (q75_f1 - q25_f1)),
  whisker_upper_f1 = pmin(max_f1, q75_f1 + 1.5 * (q75_f1 - q25_f1)),
  iqr_f2 = q75_f2 - q25_f2,
  whisker_lower_f2 = pmax(min_f2, q25_f2 - 1.5 * (q75_f2 - q25_f2)),
  whisker_upper_f2 = pmin(max_f2, q75_f2 + 1.5 * (q75_f2 - q25_f2)),
  iqr_baggernut = q75_baggernut - q25_baggernut,
  whisker_lower_baggernut = pmax(min_baggernut, q25_baggernut - 1.5 * (q75_baggernut - q25_baggernut)),
  whisker_upper_baggernut = pmin(max_baggernut, q75_baggernut + 1.5 * (q75_baggernut - q25_baggernut))
)]
fe_ratio_summary <- abio_proj[!is.na(Gebiedsnaam), .(
  median_feP_PW = median(feP_PW, na.rm = TRUE),
  median_feS_SB = median(feS_DW_SB, na.rm = TRUE),
  median_feS_PW = median(feS_PW, na.rm = TRUE),
  q25_feP_PW = quantile(feP_PW, 0.25, na.rm = TRUE),
  q75_feP_PW = quantile(feP_PW, 0.75, na.rm = TRUE),
  min_feP_PW = min(feP_PW, na.rm = TRUE),
  max_feP_PW = max(feP_PW, na.rm = TRUE),
  q25_feS_SB = quantile(feS_DW_SB, 0.25, na.rm = TRUE),
  q75_feS_SB = quantile(feS_DW_SB, 0.75, na.rm = TRUE),
  min_feS_SB = min(feS_DW_SB, na.rm = TRUE),
  max_feS_SB = max(feS_DW_SB, na.rm = TRUE),
  q25_feS_PW = quantile(feS_PW, 0.25, na.rm = TRUE),
  q75_feS_PW = quantile(feS_PW, 0.75, na.rm = TRUE),
  min_feS_PW = min(feS_PW, na.rm = TRUE),
  max_feS_PW = max(feS_PW, na.rm = TRUE),
  mean_O2 = mean(water_O2_mgL, na.rm = TRUE),
  n_obs = .N
), by = Gebiedsnaam]
fe_ratio_summary[, `:=`(
  iqr_feP_PW = q75_feP_PW - q25_feP_PW,
  whisker_lower_feP_PW = pmax(min_feP_PW, q25_feP_PW - 1.5 * (q75_feP_PW - q25_feP_PW)),
  whisker_upper_feP_PW = pmin(max_feP_PW, q75_feP_PW + 1.5 * (q75_feP_PW - q25_feP_PW)),
  iqr_feS_SB = q75_feS_SB - q25_feS_SB,
  whisker_lower_feS_SB = pmax(min_feS_SB, q25_feS_SB - 1.5 * (q75_feS_SB - q25_feS_SB)),
  whisker_upper_feS_SB = pmin(max_feS_SB, q75_feS_SB + 1.5 * (q75_feS_SB - q25_feS_SB)),
  iqr_feS_PW = q75_feS_PW - q25_feS_PW,
  whisker_lower_feS_PW = pmax(min_feS_PW, q25_feS_PW - 1.5 * (q75_feS_PW - q25_feS_PW)),
  whisker_upper_feS_PW = pmin(max_feS_PW, q75_feS_PW + 1.5 * (q75_feS_PW - q25_feS_PW))
)]
p_nalevering_summary <- p_nalevering_summary[!is.na(Gebiedsnaam) & Gebiedsnaam != ""]
fe_ratio_summary <- fe_ratio_summary[!is.na(Gebiedsnaam) & Gebiedsnaam != ""]
p_nalevering_summary[, Gebiedsnaam_marked := fifelse(
  mean_Fe_P_ratio >= 3,
  paste0(Gebiedsnaam, " *"),
  as.character(Gebiedsnaam)
)]
p_nalevering_summary[, is_zuurstofarm := mean_O2 < 2.5]
fe_ratio_summary <- merge(fe_ratio_summary, p_nalevering_summary[, .(Gebiedsnaam, Gebiedsnaam_marked, is_zuurstofarm)],
                         by = "Gebiedsnaam", all.x = TRUE)
fe_ratio_summary[is.na(Gebiedsnaam_marked), Gebiedsnaam_marked := as.character(Gebiedsnaam)]
fe_ratio_summary[is.na(is_zuurstofarm), is_zuurstofarm := mean_O2 < 2.5]

sort_order <- p_nalevering_summary[order(mean_f1)]

p_nalevering_long <- melt(p_nalevering_summary,
                         id.vars = c("Gebiedsnaam_marked", "is_zuurstofarm", "n_obs"),
                         measure.vars = list(
                           mean = c("mean_f1", "mean_f2", "mean_baggernut"),
                           whisker_lower = c("whisker_lower_f1", "whisker_lower_f2", "whisker_lower_baggernut"),
                           whisker_upper = c("whisker_upper_f1", "whisker_upper_f2", "whisker_upper_baggernut")
                         ),
                         variable.name = "formule")

p_nalevering_long[, formule_label := fifelse(formule == 1, "BWare - anaeroob",
                                     fifelse(formule == 2, "BWare - aeroob", "BaggerNut - aeroob"))]
p_nalevering_long[, Gebiedsnaam_marked := factor(Gebiedsnaam_marked, levels = sort_order$Gebiedsnaam_marked)]
fe_ratio_long <- melt(fe_ratio_summary,
                     id.vars = c("Gebiedsnaam_marked", "is_zuurstofarm", "n_obs"),
                     measure.vars = list(
                       median = c("median_feP_PW", "median_feS_SB", "median_feS_PW"),
                       whisker_lower = c("whisker_lower_feP_PW", "whisker_lower_feS_SB", "whisker_lower_feS_PW"),
                       whisker_upper = c("whisker_upper_feP_PW", "whisker_upper_feS_SB", "whisker_upper_feS_PW")
                     ),
                     variable.name = "ratio_type")

fe_ratio_long[, ratio_label := fifelse(ratio_type == 1, "Fe:P poriewater",
                              fifelse(ratio_type == 2, "Fe:S sediment", "Fe:S poriewater"))]
fe_ratio_long[, Gebiedsnaam_marked := factor(Gebiedsnaam_marked, levels = sort_order$Gebiedsnaam_marked)]
fe_ratio_long <- fe_ratio_long[Gebiedsnaam_marked %in% sort_order$Gebiedsnaam_marked]

# Plot 1: P-nalevering
p1 <- ggplot(p_nalevering_long, aes(x = Gebiedsnaam_marked, y = mean, fill = formule_label)) +
  geom_col(position = position_dodge(width = 0.8), alpha = 0.8, width = 0.7) +
  geom_errorbar(aes(ymin = whisker_lower, ymax = whisker_upper),
                position = position_dodge(width = 0.8),
                width = 0.2, color = "black", size = 0.5) +
  scale_fill_manual(
    values = c("BWare - anaeroob" = "#D55E00", "BWare - aeroob" = "#0072B2", "BaggerNut - aeroob" = "#56B4E9"),
    name = "Nalevering"
  ) +
  coord_flip() +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14, face = "plain"),
    axis.title = element_text(size = 14),
    legend.position = "bottom",
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 13),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    panel.grid.major.y = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 14, face = "bold")
  ) +
  labs(
    title = "Berekende P-nalevering naar oppervlaktewater",
    x = "Gebied",
    y = "P-nalevering (mg/m2/dag)"
  ) +
  guides(fill = guide_legend(
    title = "",
    nrow = 1,
    override.aes = list(alpha = 1)
  ))

max_x_value <- 10

fe_ratio_long[, `:=`(
  median_capped = pmin(median, max_x_value),
  is_capped = median > max_x_value,
  median_text = fifelse(median > max_x_value, as.character(round(median, 1)), ""),
  whisker_lower_capped = pmin(whisker_lower, max_x_value),
  whisker_upper_capped = pmin(whisker_upper, max_x_value)
)]

# Plot 2: Fe-ratio's
p2 <- ggplot(fe_ratio_long, aes(x = Gebiedsnaam_marked, y = median_capped, fill = ratio_label)) +
  geom_col(position = position_dodge(width = 0.8), alpha = 0.8, width = 0.7) +
  geom_errorbar(aes(ymin = whisker_lower_capped,
                    ymax = whisker_upper_capped),
                position = position_dodge(width = 0.8),
                width = 0.2, color = "black", size = 0.5) +
  geom_hline(yintercept = 5, color = "purple", linetype = "dashed", size = 1) +  # Fe/S = 5
  geom_hline(yintercept = 1, color = "red", linetype = "dashed", size = 1) +     # Fe/P = 1
  geom_text(aes(x = Gebiedsnaam_marked, y = median_capped - 0.5,
                label = median_text),
            position = position_dodge(width = 1.5),
            size = 4, color = "black", fontface = "bold") +
  scale_fill_manual(
    values = c("Fe:P poriewater" = "#0072B2", "Fe:S sediment" = "#D55E00", "Fe:S poriewater" = "#56B4E9"),
    name = ""
  ) +
  scale_y_continuous(limits = c(0, max_x_value), expand = c(0, 0)) +
  coord_flip() +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_blank(),
    axis.title = element_text(size = 14),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 14, face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    panel.grid.major.y = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 14, face = "bold")
  ) +
  labs(
    title = "Fe-ratio's per gebied (mediaan)",
    x = NULL,
    y = "Fe-ratio (mol/mol)"
  ) +
  guides(fill = guide_legend(
    title = "",
    nrow = 1,
    override.aes = list(alpha = 1)
  ))

combined_plot <- p1 + p2 + plot_layout(ncol = 2, guides = 'keep')
combined_plot <- combined_plot +
  plot_annotation(
    subtitle = "Markering gebieden:\n * = Fe/P ≥ 3\nvetgedrukt = zuurstofarm water (<2.5 mg/l)",
    theme = theme(plot.subtitle = element_text(size = 14, hjust = 0.5))
  )

print(combined_plot)
p_p_nalevering_gebied <- combined_plot
saveRDS(p_p_nalevering_gebied, paste0(rds_dir, "p_p_nalevering_gebied.rds"))
