# Fingerprint-boxplots per compartiment (slib, oever 0-25, oever 25-50 cm),
# oost tegen west, in drie vormen: per kg droge stof (zoals gemeten), per kg
# minerale delen (waarde / (1 - OS)) en per kg organische stof (waarde / OS).
# Zelfde elementselecties als de fingerprints in visualisatiesEnExport.R
# (sectie 5.4a). Verhoudingen (Fe/P, Fe/S) en pH worden niet gecorrigeerd en
# krijgen alleen de rij 'zoals gemeten'.
#
# Let op bij de correctie:
# - OS slib = gloeiverlies (B-WARE); OS oever = TOC x 1,72 (lab). Dat zijn
#   verschillende methoden; OS oever wordt afgetopt op 99%.
# - 'per kg minerale delen' is instabiel bij bijna puur veen (OS > 90%):
#   delen door een klein getal geeft uitschieters. Voor inerte elementen zijn
#   verhoudingen (bv. Si/Al) stabieler.
# Standalone: leest abio_proj.rds, hoeft niet na main_veest.R.

library(data.table)
library(ggplot2)
library(patchwork)

workspace <- paste0(Sys.getenv('NMI-SITE'), '/O 1900 - O 2000/1922.N.23 VeeST vwsloot vd toekomst/05. Data/')
rds_dir <- paste0(workspace, 'output/rapport/')
out_dir <- paste0(workspace, 'output/AlleGebieden/fingerprints/')
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

abio <- as.data.table(readRDS(paste0(rds_dir, 'abio_proj.rds')))
abio <- abio[WP %in% c('WP1', 'WP2') & !is.na(SlootID) & !is.na(Gebiedsnaam)]

oost_gebieden <- c('Staphorsterveld', 'Olde Maten', 'Mastenbroek', 'Poppenhuizen',
                   'Hegewarren', 'Idzega', 'Blesdijke (Nijkspolder)')
lagen <- c(SB = 'slib', OR_25 = 'oever\n0–25 cm', OR_50 = 'oever\n25–50 cm')
os_cols <- c(SB = 'OS gehalte_%_SB', OR_25 = 'OS_perc_OR_25', OR_50 = 'OS_perc_OR_50')
kleuren_regio <- c('Friesland/Overijssel' = '#E69F00', 'West-Nederland' = '#56B4E9')
nl_num <- scales::label_number(decimal.mark = ',', big.mark = '.')

num <- function(x) suppressWarnings(as.numeric(x))

# Per sloot gemiddelde over jaren, plus OS per laag en regio
basis_per_sloot <- function(kolommen) {
  kol <- unique(c(kolommen, unname(os_cols)))
  kol <- kol[kol %in% names(abio)]
  d <- abio[, lapply(.SD, function(z) mean(num(z), na.rm = TRUE)), by = .(SlootID, Gebiedsnaam), .SDcols = kol]
  for (cc in kol) d[is.nan(get(cc)), (cc) := NA_real_]
  d[, regio := ifelse(Gebiedsnaam %in% oost_gebieden, 'Friesland/Overijssel', 'West-Nederland')]
  for (l in names(os_cols)) if (os_cols[[l]] %in% names(d)) d[, (os_cols[[l]]) := pmin(get(os_cols[[l]]), 99)]
  d
}

#' Fingerprint-boxplot met verdunningscorrectie
#'
#' @param sel named list: naam = list(stam = kolomnaam zonder laagsuffix,
#'   label = facetlabel, corrigeer = TRUE/FALSE)
#' @param bestand bestandsnaam (zonder pad) van de png
#' @param breedte_cm breedte van de figuur
plot_verdunning <- function(sel, bestand, breedte_cm = 18) {
  kolommen <- unlist(lapply(sel, function(s) paste0(s$stam, '_', names(lagen))))
  d <- basis_per_sloot(kolommen)
  long <- rbindlist(lapply(names(sel), function(nm) {
    s <- sel[[nm]]
    rbindlist(lapply(names(lagen), function(l) {
      kol <- paste0(s$stam, '_', l)
      if (!kol %in% names(d)) { message('  ontbreekt: ', kol); return(NULL) }
      x <- d[[kol]]
      if (!isTRUE(s$corrigeer)) {
        return(data.table(par = s$label, laag = l, regio = d$regio, vorm = 'zoals gemeten', waarde = x))
      }
      os <- d[[os_cols[[l]]]] / 100
      rbindlist(list(
        data.table(par = s$label, laag = l, regio = d$regio, vorm = 'per kg droge stof', waarde = x),
        data.table(par = s$label, laag = l, regio = d$regio, vorm = 'per kg minerale delen', waarde = x / (1 - os)),
        data.table(par = s$label, laag = l, regio = d$regio, vorm = 'per kg organische stof', waarde = x / os)
      ))
    }))
  }))
  long <- long[is.finite(waarde)]
  if (nrow(long) == 0L) { message('geen data voor ', bestand); return(invisible(NULL)) }
  long[, laag := factor(lagen[laag], levels = lagen)]
  long[, vorm := factor(vorm, c('zoals gemeten', 'per kg droge stof', 'per kg minerale delen', 'per kg organische stof'))]
  long[, par := factor(par, levels = vapply(sel, function(s) s$label, character(1)))]

  # Twee deelfiguren, gestapeld met patchwork: de rij 'zoals gemeten' (pH,
  # verhoudingen) op een lineaire as, tenzij de waarden positief zijn en meer
  # dan twee ordes van grootte beslaan (Fe/P, Fe/S); de gecorrigeerde rijen
  # altijd op een log-as. (ggh4x::facetted_pos_scales gaf hier boxplots met
  # afwijkende whiskers/uitschieters, vandaar twee losse ggplots.)
  tst <- long[, .(p = tryCatch(suppressWarnings(wilcox.test(waarde ~ regio))$p.value, error = function(e) NA_real_),
                  y = max(waarde)), by = .(par, vorm, laag)]
  tst[, lab := fifelse(is.na(p), '', fifelse(p < 0.001, '***', fifelse(p < 0.01, '**', fifelse(p < 0.05, '*', 'n.s.'))))]

  deelplot <- function(dd, tt, log_as, y_lab) {
    p <- ggplot(dd, aes(x = laag, y = waarde, fill = regio)) +
      geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.4, width = 0.7) +
      geom_text(data = tt, aes(x = laag, y = y, label = lab), inherit.aes = FALSE, vjust = -0.3, size = 2.8) +
      facet_grid(vorm ~ par, scales = 'free_y', drop = TRUE) +
      scale_fill_manual(values = kleuren_regio, name = NULL) +
      labs(x = NULL, y = y_lab) +
      theme_minimal(base_size = 10) +
      theme(legend.position = 'top', panel.grid.minor = element_blank(),
            strip.text = element_text(face = 'bold'), axis.text.x = element_text(size = 8))
    if (log_as) p + scale_y_log10(labels = nl_num, expand = expansion(mult = c(0.05, 0.15))) else
      p + scale_y_continuous(labels = nl_num, expand = expansion(mult = c(0.05, 0.15)))
  }
  gemeten <- long[vorm == 'zoals gemeten']
  gecorr <- long[vorm != 'zoals gemeten']
  log_gemeten <- nrow(gemeten) > 0L && gemeten[, all(waarde > 0) && max(waarde) / min(waarde) > 100]
  delen <- list()
  if (nrow(gemeten) > 0L) delen$gemeten <- deelplot(gemeten, tst[vorm == 'zoals gemeten'], log_gemeten,
                                                    if (log_gemeten) 'Waarde (log-as)' else 'Waarde')
  if (nrow(gecorr) > 0L) delen$gecorr <- deelplot(gecorr, tst[vorm != 'zoals gemeten'], TRUE, 'Gehalte (log-as)')
  n_rij <- uniqueN(long$vorm)
  p <- if (length(delen) == 2L) {
    (delen$gemeten / (delen$gecorr + theme(legend.position = 'none'))) +
      patchwork::plot_layout(heights = c(1, uniqueN(gecorr$vorm)))
  } else delen[[1]]
  ggsave(paste0(out_dir, bestand), p, width = breedte_cm, height = 5 + 4.5 * n_rij, units = 'cm', dpi = 300, bg = 'white')
  message('geschreven: ', bestand)
  invisible(p)
}

xrf <- function(el, eenheid = 'g/kg', label = el) list(stam = paste0(el, '_xrf_', eenheid), label = paste0(label, ' (', eenheid, ')'), corrigeer = TRUE)

selecties <- list(
  # visualisatiesEnExport.R: AlleGebieden_al_mg_OS_si / _minerale_delen
  verdunning_minerale_delen = list(
    SiO2 = xrf('SiO2', label = 'SiO2'), Al2O3 = xrf('Al2O3', label = 'Al2O3'),
    TiO2 = xrf('TiO2', label = 'TiO2'), Ga = xrf('Ga', 'mg/kg'), MgO = xrf('MgO', label = 'MgO')),
  # AlleGebieden_ca_fe_SO
  verdunning_ca_mg_fe_s = list(
    CaO = xrf('CaO', label = 'CaO'), MgO = xrf('MgO', label = 'MgO'),
    Fe2O3 = xrf('Fe2O3', label = 'Fe2O3'), SO3 = xrf('SO3', label = 'SO3')),
  # AlleGebieden_metalen (+ pH-CaCl2, niet gecorrigeerd)
  verdunning_metalen = list(
    Zn = xrf('Zn', 'mg/kg'), Pb = xrf('Pb', 'mg/kg'), Cu = xrf('Cu', 'mg/kg'), Ni = xrf('Ni', 'mg/kg'),
    pH = list(stam = 'pH_CC', label = 'pH-CaCl2 (–)', corrigeer = FALSE)),
  # fosfor: P2O5 (xrf), P-AL, P-CaCl2
  verdunning_fosfor = list(
    P2O5 = xrf('P2O5', label = 'P2O5'),
    PAL = list(stam = 'P-AL mg p2o5/100g', label = 'P-AL (mg P2O5/100 g)', corrigeer = TRUE),
    PCC = list(stam = 'P_CC_mg/kg', label = 'P-CaCl2 (mg/kg)', corrigeer = TRUE)),
  # stikstof (CaCl2)
  verdunning_stikstof = list(
    NH4 = list(stam = 'N-NH4_CC_mg/kg', label = 'N-NH4 CaCl2 (mg/kg)', corrigeer = TRUE),
    NO3 = list(stam = 'N-NO3_CC_mg/kg', label = 'N-NO3 CaCl2 (mg/kg)', corrigeer = TRUE)),
  # Fe/P en Fe/S: verhoudingen, alleen zoals gemeten
  verdunning_fe_p_fe_s = list(
    feP_xrf = list(stam = 'feP_XRF', label = 'Fe/P (xrf, –)', corrigeer = FALSE),
    feP_cc = list(stam = 'feP_CC', label = 'Fe/P (CaCl2, –)', corrigeer = FALSE),
    feS_xrf = list(stam = 'feS_XRF', label = 'Fe/S (xrf, –)', corrigeer = FALSE),
    feS_cc = list(stam = 'feS_CC', label = 'Fe/S (CaCl2, –)', corrigeer = FALSE))
)

for (nm in names(selecties)) {
  message('== ', nm)
  plot_verdunning(selecties[[nm]], paste0(nm, '.png'), breedte_cm = 4 + 4 * length(selecties[[nm]]))
}
