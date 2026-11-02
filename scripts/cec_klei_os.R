# =============================================================================
# CEC van de oever tegen klei, organische stof en klei + OS (0-25 cm)
# =============================================================================
# Verkennend: hoe hangt de CEC (CEC_CO_mmol+/kg_OR_25) samen met het
# kleigehalte (Z_CLAY_SA_OR_25), het organische-stofgehalte (OS_perc_OR_25,
# berekend uit TOC) en de som van beide? Daarnaast een lineair model
# CEC ~ klei + OS (gemeten tegen voorspeld) en de samenhang tussen klei en OS.
#
# Het kleigehalte is een percentage van de minerale delen, het OS-gehalte
# een percentage van de totale bodem. Voor de som en het model is klei eerst
# omgerekend naar de totale bodem: klei_totaal = klei × (100 − OS) / 100.
# Daardoor hangt klei_totaal per definitie negatief samen met OS; de
# samenhang tussen klei en OS staat daarom met klei van de minerale delen.
#
# Monsters met TOC > 58% (OS > 100%) zijn fysiek onmogelijk en worden als
# kruisje getoond, maar tellen niet mee in de correlaties en het model.
#
# Gebruik: los te draaien; leest abio_proj.rds uit Analyses_dev_db.R.
# =============================================================================

library(data.table)
library(ggplot2)
library(patchwork)

workspace <- paste0(Sys.getenv("NMI-SITE"), "/O 1900 - O 2000/1922.N.23 VeeST vwsloot vd toekomst/05. Data/")
figuren_dir <- "C:/Users/LauraMoria/Stichting Veenweiden Innovatiecentrum/VIPNL Themas - VIPNL Veenweidensloot/D. Data en analyse/figuren_wp2_analyse_LM/overig/"

abio <- as.data.table(readRDS(paste0(workspace, "output/rapport/abio_proj.rds")))
d <- abio[, .(
  WP, SlootID, jaar,
  cec = `CEC_CO_mmol+/kg_OR_25`, klei = Z_CLAY_SA_OR_25, os = OS_perc_OR_25, toc = `TOC [%]_OR_25`
)][!is.na(cec) & !is.na(klei) & !is.na(os)]
d[, verdacht := toc > 58]
# klei als % van de totale bodem (bij OS > 100% niet te berekenen)
d[, klei_tot := fifelse(os <= 100, klei * (100 - os) / 100, NA_real_)]
d[, som := klei_tot + os]

#' Getal in Nederlandse notatie
#'
#' @param x Numerieke vector.
#' @param digits Aantal decimalen.
#' @return Character vector.
nl <- function(x, digits = 2) formatC(x, digits = digits, format = "f", decimal.mark = ",", big.mark = ".")

# Spearman per verklarende variabele, zonder de verdachte monsters
assen <- c(
  klei = "Klei (% van minerale delen)", klei_tot = "Klei (% van totale bodem)",
  os = "Organische stof (% van totale bodem)", som = "Klei + organische stof (% van totale bodem)"
)
lang <- melt(d, id.vars = c("WP", "SlootID", "jaar", "cec", "verdacht"), measure.vars = names(assen),
             variable.name = "as", value.name = "waarde")[!is.na(waarde)]
labels <- lang[verdacht == FALSE, {
  ct <- suppressWarnings(cor.test(waarde, cec, method = "spearman", exact = FALSE))
  .(label = paste0(assen[as.character(as[1])], "\nρ = ", nl(unname(ct$estimate)), ", n = ", .N))
}, by = as]
lang <- merge(lang, labels, by = "as")
lang[, label := factor(label, levels = labels$label[match(names(assen), labels$as)])]

kleuren_wp <- c(WP1 = "#E69F00", WP2 = "#0072B2")

p_scatter <- ggplot(lang, aes(x = waarde, y = cec)) +
  geom_smooth(data = lang[verdacht == FALSE], method = "lm", formula = y ~ x,
              colour = "black", fill = "grey85", linewidth = 0.7) +
  geom_point(data = lang[verdacht == FALSE], aes(colour = WP), size = 1.8, alpha = 0.75) +
  geom_point(data = lang[verdacht == TRUE], shape = 4, size = 2.5, stroke = 1, colour = "black") +
  scale_colour_manual(values = kleuren_wp) +
  facet_wrap(~label, scales = "free_x", nrow = 1) +
  labs(x = NULL, y = "CEC oever (mmol+/kg)", colour = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.border = element_rect(colour = "grey70", fill = NA), legend.position = "bottom")

# Samenhang klei (minerale delen) en OS, zonder de verdachte monsters
ko <- d[verdacht == FALSE]
rho_ko <- suppressWarnings(cor.test(ko$os, ko$klei, method = "spearman", exact = FALSE))$estimate
rho_ko_tot <- cor(ko$os, ko$klei_tot, method = "spearman")
p_klei_os <- ggplot(ko, aes(x = os, y = klei)) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "black", fill = "grey85", linewidth = 0.7) +
  geom_point(aes(colour = WP), size = 1.8, alpha = 0.75) +
  scale_colour_manual(values = kleuren_wp, guide = "none") +
  labs(
    title = paste0(
      "Klei tegen organische stof\nρ = ", nl(unname(rho_ko)), " (klei van minerale delen)\n",
      "ρ = ", nl(rho_ko_tot), " (klei van totale bodem, deels per definitie)"
    ),
    x = "Organische stof (% van totale bodem)", y = "Klei (% van minerale delen)"
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.border = element_rect(colour = "grey70", fill = NA), plot.title = element_text(size = 10))

# Lineair model CEC ~ klei (totale bodem) + OS, zonder de verdachte monsters
fit <- lm(cec ~ klei_tot + os, data = d[verdacht == FALSE])
co <- coef(fit)
d[verdacht == FALSE, voorspeld := fitted(fit)]
p_model <- ggplot(d[verdacht == FALSE], aes(x = voorspeld, y = cec)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey50", linetype = "dashed") +
  geom_point(aes(colour = WP), size = 1.8, alpha = 0.75) +
  scale_colour_manual(values = kleuren_wp, guide = "none") +
  labs(
    title = paste0(
      "CEC = ", nl(co[["(Intercept)"]], 0), if (co[["klei_tot"]] < 0) " − " else " + ", nl(abs(co[["klei_tot"]]), 1), " × klei",
      if (co[["os"]] < 0) " − " else " + ", nl(abs(co[["os"]]), 1), " × OS\nR² = ", nl(summary(fit)$r.squared)
    ),
    x = "Voorspelde CEC (mmol+/kg)", y = "Gemeten CEC (mmol+/kg)"
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.border = element_rect(colour = "grey70", fill = NA), plot.title = element_text(size = 10))

p_totaal <- p_scatter / (p_klei_os + p_model + plot_spacer() + plot_layout(widths = c(1, 1, 1.4))) +
  plot_layout(heights = c(1, 1.1)) +
  plot_annotation(
    title = "CEC van de oever (0–25 cm) tegen klei, organische stof en klei + organische stof",
    subtitle = paste0(
      "Punt = monster (locatie × jaar), WP1 en WP2. × = TOC > 58% (OS > 100%, onmogelijk), niet in ρ en model. ",
      "Lijn = lineaire trend met 95%-band; ρ = Spearman.\n",
      "Klei is gemeten als % van de minerale delen; klei van totale bodem = klei × (100 − OS) / 100. ",
      "Rechtsonder: lineair model CEC ~ klei (totale bodem) + OS, gemeten tegen voorspeld (stippellijn = 1:1)."
    ),
    theme = theme(plot.title = element_text(face = "bold"))
  )
print(summary(fit))
ggsave(paste0(figuren_dir, "cec_klei_os.png"), p_totaal, width = 16, height = 10, dpi = 300, bg = "white")
