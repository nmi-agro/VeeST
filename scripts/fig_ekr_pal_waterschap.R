# Figuur: EKR-score watervegetatie tegen P-AL in het slib, gekleurd per waterschap
#
# Herziening van de figuur in het concept-eindrapport (EKR vs. P-AL slib met
# logaritmische fit). Zelfde selectie en model als het origineel:
#   - sloten met MeenemenDataAnalyse_totaal == "ja"
#   - lm(EKR_score ~ log(P-AL)), met 95%-betrouwbaarheidsband
# Kleuren: Okabe-Ito (8 kleuren) + grijs = 9 waterschappen. Punten krijgen een
# donkere rand, zodat ook geel/oranje/lichtblauw goed zichtbaar zijn op wit.
#
# Uitvoer:
#   output/AlleGebieden/Eindrapportage/ekr_pal_waterschap.png        (overzicht)
#   output/AlleGebieden/Eindrapportage/ekr_pal_waterschap_facet.png  (per waterschap)
#   output/AlleGebieden/Eindrapportage/ekr_pal_regio.png             (west vs. noordoost)

library(data.table)
library(ggplot2)
source("scripts/functions/functions_veest.R") # bepaal_regio(), kleuren_regio

workspace <- paste0(
  Sys.getenv("NMI-SITE"),
  "/O 1900 - O 2000/1922.N.23 VeeST vwsloot vd toekomst/05. Data/"
)
abio_proj <- as.data.table(readRDS(paste0(workspace, "output/rapport/abio_proj.rds")))

uit_dir <- "output/AlleGebieden/Eindrapportage"
dir.create(uit_dir, showWarnings = FALSE, recursive = TRUE)

# Data ------------------------------------------------------------------------
ekr_pal <- abio_proj[
  MeenemenDataAnalyse_totaal == "ja" &
    !is.na(`P-AL mg p2o5/100g_SB`) & `P-AL mg p2o5/100g_SB` > 0 &
    !is.na(EKR_score) & !is.na(waterschap),
  .(SlootID_kort, waterschap, pal = `P-AL mg p2o5/100g_SB`, ekr = EKR_score)
]

# Vaste kleur per waterschap (grootste groepen de sterkste kleuren,
# geel/zwart/grijs voor de kleinste groepen)
kleuren_ws <- c(
  "AGV"      = "#0072B2",
  "HHNK"     = "#D55E00",
  "Fryslân"  = "#009E73",
  "Rijnland" = "#E69F00",
  "HDSR"     = "#56B4E9",
  "HHSK"     = "#CC79A7",
  "WDOD"     = "#F0E442",
  "HDL"      = "#000000",
  "WVV"      = "#999999"
)
n_ws <- ekr_pal[, .N, by = waterschap]
ekr_pal[, waterschap := factor(waterschap, levels = names(kleuren_ws))]
labels_ws <- setNames(
  paste0(names(kleuren_ws), " (n = ", n_ws[match(names(kleuren_ws), waterschap), N], ")"),
  names(kleuren_ws)
)

# Model (zelfde als origineel) ------------------------------------------------
fit <- lm(ekr ~ log(pal), data = ekr_pal)
r2 <- summary(fit)$r.squared
fit_label <- sprintf(
  "EKR = %s %s %s · ln(P-AL)\nR² = %s, n = %d sloten",
  format(round(coef(fit)[1], 2), decimal.mark = ","),
  ifelse(coef(fit)[2] < 0, "−", "+"),
  format(round(abs(coef(fit)[2]), 3), decimal.mark = ","),
  format(round(r2, 2), decimal.mark = ","),
  nrow(ekr_pal)
)

theme_fig <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
    axis.line        = element_line(colour = "grey40", linewidth = 0.4),
    axis.ticks       = element_line(colour = "grey40", linewidth = 0.4),
    axis.title       = element_text(colour = "grey15"),
    axis.text        = element_text(colour = "grey30"),
    legend.title     = element_text(face = "bold", size = 11),
    legend.text      = element_text(size = 10),
    plot.background  = element_rect(fill = "white", colour = NA)
  )

komma <- function(x) format(x, decimal.mark = ",", trim = TRUE)

# Figuur 1: overzicht ---------------------------------------------------------
p_ekr_pal <- ggplot(ekr_pal, aes(x = pal, y = ekr)) +
  geom_smooth(
    method = "lm", formula = y ~ log(x), se = TRUE,
    colour = "grey15", fill = "grey75", linewidth = 0.9, alpha = 0.5
  ) +
  geom_point(
    aes(fill = waterschap),
    shape = 21, size = 2.1, stroke = 0.3, colour = "grey20", alpha = 0.9
  ) +
  annotate(
    "text", x = Inf, y = Inf, label = fit_label,
    hjust = 1.05, vjust = 1.4, size = 3.5, colour = "grey20", lineheight = 1.1
  ) +
  scale_fill_manual(values = kleuren_ws, labels = labels_ws, name = "Waterschap") +
  scale_x_continuous(
    labels = komma, limits = c(0, NA), breaks = seq(0, 200, 25),
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  scale_y_continuous(labels = komma, limits = c(0, NA), expand = expansion(mult = c(0.01, 0.05))) +
  labs(
    x = expression("P-AL slib (mg P"[2]*"O"[5]*" per 100 g)"),
    y = "EKR-score watervegetatie"
  ) +
  guides(fill = guide_legend(override.aes = list(size = 3.5, alpha = 1))) +
  theme_fig

ggsave(
  file.path(uit_dir, "ekr_pal_waterschap.png"), p_ekr_pal,
  width = 20, height = 13, units = "cm", dpi = 300, bg = "white"
)

# Figuur 2: per waterschap (alle sloten grijs op de achtergrond) --------------
achtergrond <- ekr_pal[, .(pal, ekr)]

p_ekr_pal_facet <- ggplot(ekr_pal, aes(x = pal, y = ekr)) +
  geom_point(data = achtergrond, colour = "grey85", size = 1.4) +
  geom_smooth(
    data = achtergrond, method = "lm", formula = y ~ log(x), se = FALSE,
    colour = "grey15", linewidth = 0.7
  ) +
  geom_point(
    aes(fill = waterschap),
    shape = 21, size = 2.3, stroke = 0.35, colour = "grey20"
  ) +
  facet_wrap(~waterschap, ncol = 3, labeller = as_labeller(labels_ws)) +
  scale_fill_manual(values = kleuren_ws, guide = "none") +
  scale_x_continuous(labels = komma) +
  scale_y_continuous(labels = komma, limits = c(0, NA)) +
  labs(
    x = expression("P-AL slib (mg P"[2]*"O"[5]*" per 100 g)"),
    y = "EKR-score watervegetatie"
  ) +
  theme_fig +
  theme(
    strip.text = element_text(face = "bold", hjust = 0, size = 10.5),
    panel.border = element_rect(colour = "grey80", fill = NA, linewidth = 0.4),
    axis.line = element_blank()
  )

ggsave(
  file.path(uit_dir, "ekr_pal_waterschap_facet.png"), p_ekr_pal_facet,
  width = 20, height = 18, units = "cm", dpi = 300, bg = "white"
)

# Figuur 3: regio's (westelijk veenweidegebied vs. noordoosten) ---------------
# Eén log-fit over alle sloten (zelfde model als figuur 1), punten gekleurd per
# regio. Regio-indeling: bepaal_regio() in scripts/functions/functions_veest.R.
ekr_pal[, regio := bepaal_regio(waterschap)]
n_regio <- ekr_pal[, .N, keyby = regio]
labels_regio <- setNames(
  sprintf("%s (n = %d)", n_regio$regio, n_regio$N),
  as.character(n_regio$regio)
)

p_ekr_pal_regio <- ggplot(ekr_pal, aes(x = pal, y = ekr)) +
  geom_smooth(
    method = "lm", formula = y ~ log(x), se = TRUE,
    colour = "grey15", fill = "grey75", linewidth = 0.9, alpha = 0.5
  ) +
  geom_point(
    aes(fill = regio),
    shape = 21, size = 2.1, stroke = 0.3, colour = "grey20", alpha = 0.9
  ) +
  annotate(
    "text", x = Inf, y = Inf, label = fit_label,
    hjust = 1.05, vjust = 1.4, size = 3.5, colour = "grey20", lineheight = 1.1
  ) +
  scale_fill_manual(values = kleuren_regio, labels = labels_regio, name = "Regio") +
  scale_x_continuous(
    labels = komma, limits = c(0, NA), breaks = seq(0, 200, 25),
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  scale_y_continuous(labels = komma, limits = c(0, NA), expand = expansion(mult = c(0.01, 0.05))) +
  labs(
    x = expression("P-AL slib (mg P"[2]*"O"[5]*" per 100 g)"),
    y = "EKR-score watervegetatie"
  ) +
  guides(fill = guide_legend(override.aes = list(size = 3.5, alpha = 1))) +
  theme_fig +
  theme(legend.position = "bottom")

ggsave(
  file.path(uit_dir, "ekr_pal_regio.png"), p_ekr_pal_regio,
  width = 18, height = 13, units = "cm", dpi = 300, bg = "white"
)
