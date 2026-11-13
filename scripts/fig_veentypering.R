# Gemeten veentypering (C/N, uitwisselbaar Ca, pH-CaCl2 van de oeverlaag
# 25-50 cm) en drie figuren:
#   1. per gebied, gesplitst in oost en west: overeenkomst slib met de laag
#      25-50 cm en verdeling van het gemeten veentype
#   2. kaart van het gemeten veentype per sloot
#   3. draagkracht (indringingsweerstand) over diepte per gemeten veentype,
#      oever en perceel, met lijnen voor oost en west
# Schrijft ook veentypering_sloot.rds (typering per sloot) naar output/rapport.
#
# Typering op twee assen (laag 25-50 cm):
#  - voedselrijkdom: C/N (omgekeerd) en uitwisselbaar Ca, elk in tertielen
#    gescoord 1-3; som 2-3 = arm, 4 = midden, 5-6 = rijk. pH is bewust
#    weggelaten: die hangt nauwelijks samen met C/N en Ca (eigen PCA-as).
#  - minerale bijmenging: TOC < toc_grens g/kg = 'mineraalrijk' (kleiig of
#    zandig veen), anders 'veen'. TOC is de eenvoudigste maat voor de
#    hoofdas van de PCA (TOC/zand tegenover klei/Si/Al/Fe/Mg).
# Standalone: leest abio_proj.rds, hoeft niet na main_veest.R.

library(data.table)
library(ggplot2)
library(sf)
library(patchwork)

workspace <- paste0(Sys.getenv('NMI-SITE'), '/O 1900 - O 2000/1922.N.23 VeeST vwsloot vd toekomst/05. Data/')
rds_dir <- paste0(workspace, 'output/rapport/')
out_dir <- paste0(workspace, 'output/AlleGebieden/veentypering/')
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

abio <- as.data.table(readRDS(paste0(rds_dir, 'abio_proj.rds')))
abio <- abio[WP %in% c('WP1', 'WP2') & !is.na(SlootID) & !is.na(Gebiedsnaam)]

oost_gebieden <- c('Staphorsterveld', 'Olde Maten', 'Mastenbroek', 'Poppenhuizen',
                   'Hegewarren', 'Idzega', 'Blesdijke (Nijkspolder)')
kleuren_regio <- c('Friesland/Overijssel' = '#E69F00', 'West-Nederland' = '#56B4E9')
kleuren_trofie <- c(arm = '#E69F00', midden = '#999999', rijk = '#009E73')
kleuren_mineraal <- c(veen = '#8B4513', mineraalrijk = '#4682B4')
# Zes gecombineerde typen, zes duidelijk verschillende Okabe-Ito-kleuren
# (warm = arm, koel = rijk); elke sloot valt in precies één type
kleuren_type <- c('arm veen' = '#E69F00', 'arm mineraalrijk' = '#F0E442',
                  'midden veen' = '#7F7F7F', 'midden mineraalrijk' = '#CC79A7',
                  'rijk veen' = '#009E73', 'rijk mineraalrijk' = '#56B4E9')
toc_grens <- 300  # g C/kg; eronder = mineraalrijk (kleiig/zandig veen)
nl_num <- scales::label_number(decimal.mark = ',', big.mark = '.')
num <- function(x) suppressWarnings(as.numeric(x))

## 1) per sloot: typering, slibovereenkomst, coördinaten, draagkrachtbakken ----
bakken <- c('[0,10]', '(10,20]', '(20,30]', '(30,40]', '(40,50]', '(50,85]')
bak_mid <- c(5, 15, 25, 35, 45, 67.5)
kol_dk <- c(paste0('oever_', bakken), paste0('perceel_', bakken))
kol_xrf <- unlist(lapply(c('SiO2_xrf_g/kg', 'Al2O3_xrf_g/kg', 'TiO2_xrf_g/kg', 'Ga_xrf_mg/kg'),
                         function(s) paste0(s, '_', c('SB', 'OR_25', 'OR_50'))))
kol <- c('A_CN_OF_OR_50', 'CA_CO_mmol+/kg_OR_50', 'pH_CC_OR_50', 'TOC [g/kg]_OR_50', 'drglg',
         'Start_traject_long', 'Start_traject_lat', kol_dk, kol_xrf)
kol <- kol[kol %in% names(abio)]
sl <- abio[, lapply(.SD, function(z) mean(num(z), na.rm = TRUE)), by = .(SlootID, Gebiedsnaam), .SDcols = kol]
for (cc in kol) sl[is.nan(get(cc)), (cc) := NA_real_]
sl[, regio := ifelse(Gebiedsnaam %in% oost_gebieden, 'Friesland/Overijssel', 'West-Nederland')]

# Typering
tert <- function(x) quantile(x, c(1 / 3, 2 / 3), na.rm = TRUE)
score3 <- function(x, omgekeerd = FALSE) {
  b <- tert(x); k <- 1 + (x > b[1]) + (x > b[2])
  if (omgekeerd) 4 - k else k
}
sl[, score := score3(A_CN_OF_OR_50, omgekeerd = TRUE) + score3(`CA_CO_mmol+/kg_OR_50`)]
sl[, trofie := cut(score, c(1, 3, 4, 6), labels = c('arm', 'midden', 'rijk'))]
sl[, mineraal := factor(fifelse(`TOC [g/kg]_OR_50` < toc_grens, 'mineraalrijk', 'veen'), levels = c('veen', 'mineraalrijk'))]
sl[!is.na(trofie) & !is.na(mineraal), type := factor(paste(trofie, mineraal), levels = names(kleuren_type))]
grenzen <- list(CN = tert(sl$A_CN_OF_OR_50), Ca = tert(sl$`CA_CO_mmol+/kg_OR_50`))
message('tertielgrenzen: C/N ', paste(round(grenzen$CN, 1), collapse = ' / '),
        ' | Ca ', paste(round(grenzen$Ca), collapse = ' / '), ' | TOC-grens ', toc_grens, ' g/kg')
message('trofie: ', paste(names(table(sl$trofie)), table(sl$trofie), collapse = ', '))
message('mineraal: ', paste(names(table(sl$mineraal)), table(sl$mineraal), collapse = ', '))
message('type: ', paste(names(table(sl$type)), table(sl$type), collapse = ', '))

# Slibovereenkomst: afstand (log-ratio's, clr) slib tot 0-25 en tot 25-50 cm
xrf_mat <- function(laag) {
  m <- sapply(c('SiO2_xrf_g/kg', 'Al2O3_xrf_g/kg', 'TiO2_xrf_g/kg', 'Ga_xrf_mg/kg'),
              function(s) log(sl[[paste0(s, '_', laag)]]))
  m[!is.finite(m)] <- NA
  m - rowMeans(m)
}
afst <- function(a, b) sqrt(rowSums((a - b)^2))
sb <- xrf_mat('SB')
sl[, delta_clr := afst(sb, xrf_mat('OR_25')) - afst(sb, xrf_mat('OR_50'))]  # > 0: slib lijkt meer op 25-50

saveRDS(sl[, .(SlootID, Gebiedsnaam, regio, A_CN_OF_OR_50, `CA_CO_mmol+/kg_OR_50`, pH_CC_OR_50, `TOC [g/kg]_OR_50`,
               score, trofie, mineraal, type, delta_clr)],
        paste0(rds_dir, 'veentypering_sloot.rds'))

## 2) figuur per gebied, oost en west gesplitst ----------------------------------
geb <- sl[!is.na(type), .(n = .N, pct_25_50 = 100 * mean(delta_clr > 0, na.rm = TRUE)), by = .(Gebiedsnaam, regio)][n >= 4]
geb[, lab := sprintf('%s (n = %d)', Gebiedsnaam, n)]
geb <- geb[order(regio, pct_25_50)]
geb[, lab := factor(lab, levels = lab)]
geb_type <- sl[!is.na(type) & Gebiedsnaam %in% geb$Gebiedsnaam, .(n_type = .N), by = .(Gebiedsnaam, regio, type)]
geb_type[, aandeel := n_type / sum(n_type), by = Gebiedsnaam]
geb_type <- merge(geb_type, geb[, .(Gebiedsnaam, lab)], by = 'Gebiedsnaam')

thema <- theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), strip.text = element_text(face = 'bold'),
        axis.text.y = element_text(size = 8), legend.position = 'bottom')
strip_regio <- as_labeller(c('Friesland/Overijssel' = 'Friesland/\nOverijssel', 'West-Nederland' = 'West-\nNederland'))

p_geb_a <- ggplot(geb, aes(x = pct_25_50, y = lab, fill = regio)) +
  geom_col(width = 0.75, show.legend = FALSE) +
  geom_vline(xintercept = 50, colour = 'grey40', linetype = 'dashed') +
  facet_grid(regio ~ ., scales = 'free_y', space = 'free_y', labeller = strip_regio) +
  scale_fill_manual(values = kleuren_regio) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25), labels = function(x) paste0(x, '%'),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(x = 'Aandeel sloten waar slib het meest\nop de laag 25–50 cm lijkt', y = NULL,
       title = 'Overeenkomst slib met laag 25–50 cm') + thema

p_geb_b <- ggplot(geb_type, aes(x = aandeel, y = lab, fill = type)) +
  geom_col(width = 0.75, position = position_stack(reverse = TRUE)) +
  facet_grid(regio ~ ., scales = 'free_y', space = 'free_y', labeller = strip_regio) +
  scale_fill_manual(values = kleuren_type, name = 'Gemeten veentype', drop = FALSE) +
  scale_x_continuous(labels = scales::label_percent(decimal.mark = ','), expand = expansion(mult = c(0, 0.02))) +
  labs(x = 'Aandeel sloten per gemeten veentype', y = NULL, title = 'Gemeten veentype') +
  guides(fill = guide_legend(ncol = 3, byrow = TRUE)) +
  thema + theme(axis.text.y = element_blank())

p_geb <- (p_geb_a + p_geb_b + plot_layout(widths = c(1.3, 1), guides = 'collect')) &
  theme(legend.position = 'bottom')
ggsave(paste0(out_dir, 'veentypering_per_gebied_oost_west.png'), p_geb, width = 22, height = 17, units = 'cm', dpi = 300, bg = 'white')
message('geschreven: veentypering_per_gebied_oost_west.png')

## 3) kaart gemeten veentype -----------------------------------------------------
# Let op: in abio_proj staat de breedtegraad in Start_traject_long en de
# lengtegraad in Start_traject_lat (kolommen verwisseld). Hier gecorrigeerd.
pt <- sl[!is.na(type) & !is.na(Start_traject_long) & !is.na(Start_traject_lat)]
lon <- if (median(pt$Start_traject_long) > 20) pt$Start_traject_lat else pt$Start_traject_long
lat <- if (median(pt$Start_traject_long) > 20) pt$Start_traject_long else pt$Start_traject_lat
pt_sf <- st_as_sf(data.frame(pt[, .(SlootID, Gebiedsnaam, regio, trofie, mineraal, type)], lon = lon, lat = lat),
                  coords = c('lon', 'lat'), crs = 4326) |> st_transform(28992)
bb <- st_bbox(pt_sf); pad <- 15000
xlim <- c(bb['xmin'] - pad, bb['xmax'] + pad); ylim <- c(bb['ymin'] - pad, bb['ymax'] + pad)
pt_alle <- pt_sf; pt_alle$trofie <- NULL; pt_alle$mineraal <- NULL; pt_alle$type <- NULL  # grijs in elk paneel
n_tr <- table(pt_sf$trofie); n_mi <- table(pt_sf$mineraal)
lab_tr <- setNames(sprintf('%s (n = %d)', names(n_tr), n_tr), names(n_tr))
lab_mi <- setNames(sprintf('%s (n = %d)', names(n_mi), n_mi), names(n_mi))

# Tegellaag per facet: ggspatial wil daarvoor data met de facetvariabelen en
# een mapping van de tegelbron (aesthetic 'type') -- zonder dat komt de kaart
# in een apart NA-paneel terecht.
pdok_url <- 'https://service.pdok.nl/brt/achtergrondkaart/wmts/v2_0/grijs/EPSG:3857/${z}/${x}/${y}.png'
tegels <- CJ(trofie = factor(levels(pt_sf$trofie), levels = levels(pt_sf$trofie)),
             mineraal = factor(levels(pt_sf$mineraal), levels = levels(pt_sf$mineraal)))
tegels[, bron := pdok_url]
# Bepalende kenmerken per paneel (linksboven): tertielgrenzen uit de typering
f1 <- function(x, d = 1) format(round(x, d), decimal.mark = ',', nsmall = d)
# Eenvoudige labels; de precieze regels staan in het bijschrift
lab_trofie <- c(arm = 'arm: hoge C/N, weinig calcium',
                midden = 'midden: C/N en calcium gemiddeld',
                rijk = 'rijk: lage C/N, veel calcium')
lab_mineraal <- c(veen = 'veen: weinig klei of zand',
                  mineraalrijk = 'mineraalrijk: klei of zand bijgemengd')
bijschrift <- sprintf(paste0(
  'Voedselrijkdom: C/N en uitwisselbaar Ca van de laag 25–50 cm elk in drie gelijke klassen\n',
  '(C/N-grenzen %s en %s; Ca-grenzen %s en %s mmol+/kg), gescoord 1–3 (C/N omgekeerd) en opgeteld: 2–3 = arm, 4 = midden, 5–6 = rijk.\n',
  'Minerale bijmenging: TOC < %d g/kg = mineraalrijk. Grijze stippen = alle sloten.'),
  f1(grenzen$CN[1]), f1(grenzen$CN[2]), f1(grenzen$Ca[1], 0), f1(grenzen$Ca[2], 0), toc_grens)
hoek <- CJ(trofie = factor(levels(pt_sf$trofie), levels = levels(pt_sf$trofie)),
           mineraal = factor(levels(pt_sf$mineraal), levels = levels(pt_sf$mineraal)))
hoek[, lab := paste0(lab_trofie[as.character(trofie)], '\n', lab_mineraal[as.character(mineraal)])]
hoek[, `:=`(x = xlim[1] + 0.02 * diff(xlim), y = ylim[2] - 0.02 * diff(ylim))]

p_kaart <- ggplot() +
  ggspatial::annotation_map_tile(
    data = as.data.frame(tegels), mapping = aes(type = bron),
    cachedir = '/osm_cache', zoomin = 0, progress = 'none', quiet = TRUE) +
  geom_sf(data = pt_alle, colour = 'grey55', size = 1.2, alpha = 0.6) +
  geom_sf(data = pt_sf, aes(fill = type), shape = 21, size = 3, colour = 'black', stroke = 0.3, alpha = 0.95) +
  geom_label(data = as.data.frame(hoek), aes(x = x, y = y, label = lab), hjust = 0, vjust = 1, size = 2.8,
             fill = 'white', alpha = 0.85, linewidth = 0, lineheight = 0.9) +
  facet_grid(mineraal ~ trofie, labeller = labeller(trofie = lab_tr, mineraal = lab_mi)) +
  coord_sf(crs = st_crs(28992), xlim = xlim, ylim = ylim, expand = FALSE) +
  scale_fill_manual(values = kleuren_type, guide = 'none') +
  labs(x = NULL, y = NULL, title = 'Gemeten veentype van de oeverlaag 25–50 cm',
       subtitle = 'Kolommen: voedselrijkdom; rijen: minerale bijmenging',
       caption = bijschrift) +
  theme_minimal(base_size = 12) +
  theme(axis.text = element_blank(), panel.grid = element_blank(), strip.text = element_text(size = 12, face = 'bold'),
        plot.caption = element_text(hjust = 0, size = 9), plot.caption.position = 'plot')
ggsave(paste0(out_dir, 'veentypering_kaart.png'), p_kaart, width = 30, height = 24, units = 'cm', dpi = 300, bg = 'white')
message('geschreven: veentypering_kaart.png')

## 4) draagkracht over diepte per gemeten veentype --------------------------------
dk <- melt(sl[!is.na(type), c('SlootID', 'regio', 'trofie', 'mineraal', 'type', 'drglg', kol_dk), with = FALSE],
           id.vars = c('SlootID', 'regio', 'trofie', 'mineraal', 'type', 'drglg'), variable.name = 'kolom', value.name = 'weerstand')
dk <- dk[!is.na(weerstand)]
dk[, sectie := sub('_.*', '', kolom)]
dk[, bak := sub('^[a-z]+_', '', kolom)]
dk[, diepte := bak_mid[match(bak, bakken)]]
dk[, sectie := factor(sectie, c('oever', 'perceel'), c('Oever', 'Perceel'))]

# Panelen = voedselrijkdom; lijnen = veen / mineraalrijk; stippellijn = alle sloten van die trofieklasse
gem_trofie <- dk[, .(gem = mean(weerstand), n = .N), by = .(trofie, sectie, diepte)]
gem_min <- dk[, .(gem = mean(weerstand), n = .N), by = .(trofie, mineraal, sectie, diepte)][n >= 5]
drglg_band <- sl[!is.na(trofie) & !is.na(drglg), .(p10 = quantile(drglg, 0.1) * 100, p90 = quantile(drglg, 0.9) * 100), by = trofie]
kritieke_draagkracht <- 0.5
n_sloten <- dk[, uniqueN(SlootID)]
x_max <- as.numeric(quantile(dk$weerstand, 0.98))

# Toetsen per sectie en diepte (Kruskal-Wallis): links verschil tussen de drie
# trofieklassen (zelfde label in elk paneel), rechts verschil veen tegen
# mineraalrijk binnen de trofieklasse van het paneel.
kw_tr <- dk[, .(p = kruskal.test(weerstand, trofie)$p.value), by = .(sectie, diepte)]
kw_tr <- kw_tr[, .(trofie = factor(levels(dk$trofie), levels = levels(dk$trofie))), by = .(sectie, diepte, p)]
kw_mi <- dk[, .(p = if (uniqueN(mineraal) == 2L && min(table(mineraal)) >= 5) kruskal.test(weerstand, mineraal)$p.value else NA_real_),
            by = .(sectie, diepte, trofie)]
ster <- function(p) fifelse(is.na(p), '', fifelse(p < 0.001, '***', fifelse(p < 0.01, '**', fifelse(p < 0.05, '*', 'n.s.'))))
kw_tr[, lab := paste0('T: ', ster(p))]; kw_mi[, lab := paste0('M: ', ster(p))]

p_dk <- ggplot() +
  geom_rect(data = drglg_band, aes(xmin = -Inf, xmax = Inf, ymin = p10, ymax = p90), fill = 'lightblue', alpha = 0.3) +
  geom_point(data = dk, aes(x = weerstand, y = diepte), colour = 'grey75', alpha = 0.5, size = 0.8,
             position = position_jitter(width = 0, height = 2, seed = 1)) +
  geom_path(data = gem_min, aes(x = gem, y = diepte, colour = mineraal), linewidth = 0.9) +
  geom_point(data = gem_min, aes(x = gem, y = diepte, colour = mineraal), size = 1.8) +
  geom_path(data = gem_trofie, aes(x = gem, y = diepte), colour = 'black', linetype = 'dashed', linewidth = 1) +
  geom_vline(xintercept = kritieke_draagkracht, colour = 'red', linetype = 'dotted', linewidth = 0.8) +
  geom_text(data = kw_tr, aes(x = x_max * 0.78, y = diepte, label = lab), hjust = 1, size = 2.6, colour = 'grey30') +
  geom_text(data = kw_mi, aes(x = x_max, y = diepte, label = lab), hjust = 1, size = 2.6, colour = 'grey30') +
  facet_grid(sectie ~ trofie, labeller = labeller(trofie = function(x) paste0('Voedselrijkdom: ', x))) +
  scale_y_reverse(name = 'Diepte (cm)', breaks = seq(0, 80, 20), limits = c(85, 0)) +
  scale_x_continuous(name = 'Indringingsweerstand (MPa)', labels = nl_num) +
  coord_cartesian(xlim = c(0, x_max)) +
  scale_colour_manual(values = kleuren_mineraal, name = NULL, labels = c(veen = 'veen (TOC ≥ 300 g/kg)', mineraalrijk = 'mineraalrijk veen (TOC < 300 g/kg)')) +
  labs(title = 'Draagkracht over diepte per gemeten veentype',
       subtitle = paste0('Zwarte stippellijn = gemiddelde per voedselrijkdomklasse; gekleurde lijnen = veen en mineraalrijk veen (n ≥ 5)\n',
                         'Lichtblauw = drooglegging P10–P90; rode stippellijn = kritische draagkracht voor beweiding (0,5 MPa)\n',
                         'T = verschil tussen de drie voedselrijkdomklassen; M = verschil veen/mineraalrijk binnen het paneel\n',
                         '(Kruskal-Wallis; *** p < 0,001, ** p < 0,01, * p < 0,05)'),
       caption = sprintf('Per sloot gemiddeld over jaren, 10-cm-dieptebakken (laatste bak 50–85 cm), %d sloten', n_sloten)) +
  theme_minimal(base_size = 11) +
  theme(legend.position = 'bottom', panel.grid.minor = element_blank(),
        strip.text = element_text(face = 'bold'), panel.border = element_rect(colour = 'grey60', fill = NA))
ggsave(paste0(out_dir, 'draagkracht_diepte_gemeten_veentype.png'), p_dk, width = 22, height = 16, units = 'cm', dpi = 300, bg = 'white')
message('geschreven: draagkracht_diepte_gemeten_veentype.png')

## 4b) draagkracht over diepte per regio, lijnen per voedselrijkdom -----------------
# Panelen = regio (oost/west) x sectie; lijnen = arm/midden/rijk; per diepte de
# Kruskal-Wallis-toets op verschil tussen de drie voedselrijkdomklassen binnen
# die regio.
gem_rt <- dk[, .(gem = mean(weerstand), n = .N), by = .(regio, trofie, sectie, diepte)][n >= 5]
kw_rt <- dk[, .(p = if (uniqueN(trofie) >= 2L && min(table(trofie)) >= 5) kruskal.test(weerstand, trofie)$p.value else NA_real_),
            by = .(regio, sectie, diepte)]
kw_rt[, lab := ster(p)]
n_rt <- dk[, .(n = uniqueN(SlootID)), by = regio]
lab_regio <- setNames(sprintf('%s (n = %d sloten)', n_rt$regio, n_rt$n), n_rt$regio)
drglg_band_regio <- sl[!is.na(trofie) & !is.na(drglg), .(p10 = quantile(drglg, 0.1) * 100, p90 = quantile(drglg, 0.9) * 100), by = regio]

p_dk_rw <- ggplot() +
  geom_rect(data = drglg_band_regio, aes(xmin = -Inf, xmax = Inf, ymin = p10, ymax = p90), fill = 'lightblue', alpha = 0.3) +
  geom_point(data = dk, aes(x = weerstand, y = diepte), colour = 'grey75', alpha = 0.5, size = 0.8,
             position = position_jitter(width = 0, height = 2, seed = 1)) +
  geom_path(data = gem_rt, aes(x = gem, y = diepte, colour = trofie), linewidth = 1) +
  geom_point(data = gem_rt, aes(x = gem, y = diepte, colour = trofie), size = 2) +
  geom_vline(xintercept = kritieke_draagkracht, colour = 'red', linetype = 'dotted', linewidth = 0.8) +
  geom_text(data = kw_rt, aes(x = x_max, y = diepte, label = lab), hjust = 1, size = 3, colour = 'grey30') +
  facet_grid(sectie ~ regio, labeller = labeller(regio = lab_regio)) +
  scale_y_reverse(name = 'Diepte (cm)', breaks = seq(0, 80, 20), limits = c(85, 0)) +
  scale_x_continuous(name = 'Indringingsweerstand (MPa)', labels = nl_num) +
  coord_cartesian(xlim = c(0, x_max)) +
  scale_colour_manual(values = kleuren_trofie, name = 'Voedselrijkdom veen (25–50 cm)') +
  labs(title = 'Draagkracht over diepte per regio en voedselrijkdom van het veen',
       subtitle = paste0('Lijnen = gemiddelde per voedselrijkdomklasse (n ≥ 5 sloten)\n',
                         'Lichtblauw = drooglegging P10–P90 per regio; rode stippellijn = kritische draagkracht (0,5 MPa)\n',
                         'Rechts per diepte: verschil tussen de klassen binnen de regio\n',
                         '(Kruskal-Wallis; *** p < 0,001, ** p < 0,01, * p < 0,05)'),
       caption = sprintf('Per sloot gemiddeld over jaren, 10-cm-dieptebakken (laatste bak 50–85 cm), %d sloten', n_sloten)) +
  theme_minimal(base_size = 11) +
  theme(legend.position = 'bottom', panel.grid.minor = element_blank(),
        strip.text = element_text(face = 'bold'), panel.border = element_rect(colour = 'grey60', fill = NA))
ggsave(paste0(out_dir, 'draagkracht_diepte_oost_west.png'), p_dk_rw, width = 18, height = 17, units = 'cm', dpi = 300, bg = 'white')
message('geschreven: draagkracht_diepte_oost_west.png')

## 5) bodemklasse (klei% en OS%) per gebied, oost en west, beide oeverlagen ---------
# Zelfde indeling als p_bodemfractie_gebied in gebiedseigenschappen_sloten.R
# (klei% van de minerale delen en OS% per sloot), hier voor 0-25 en 25-50 cm en
# gegroepeerd naar regio. Let op: OS in abio_proj is TOC x 1,72 (het
# onderschrift van de bestaande figuur noemt 1,5).
bodemklasse <- function(klei, os) {
  fcase(
    klei <  17 & os >= 16, 'Veen',
    klei >= 17 & os >= 16, 'Klei-in-veen',
    klei >= 35 & os <  16, 'Klei',
    klei >= 17 & os <  16, 'Moerig',
    klei <  17 & os >= 10, 'Moerig',
    klei <  17 & os <  10, 'Zand',
    default = 'Overig'
  )
}
kleuren_bodemklasse <- c('Klei' = '#4682B4', 'Klei-in-veen' = '#9ACD32', 'Veen' = '#8B4513',
                         'Zand' = '#D3D3D3', 'Moerig' = '#6495ED', 'Overig' = 'grey70')
kol_bk <- c('Z_CLAY_SA_OR_25', 'OS_perc_OR_25', 'Z_CLAY_SA_OR_50', 'OS_perc_OR_50')
bk <- abio[, lapply(.SD, function(z) mean(num(z), na.rm = TRUE)), by = .(SlootID, Gebiedsnaam), .SDcols = kol_bk]
for (cc in kol_bk) bk[is.nan(get(cc)), (cc) := NA_real_]
bk[, regio := ifelse(Gebiedsnaam %in% oost_gebieden, 'Friesland/Overijssel', 'West-Nederland')]
bk_long <- rbindlist(list(
  bk[, .(SlootID, Gebiedsnaam, regio, laag = 'Oever 0–25 cm', klasse = bodemklasse(Z_CLAY_SA_OR_25, OS_perc_OR_25))],
  bk[, .(SlootID, Gebiedsnaam, regio, laag = 'Oever 25–50 cm', klasse = bodemklasse(Z_CLAY_SA_OR_50, OS_perc_OR_50))]
))[!is.na(klasse) & klasse != 'Overig']
bk_long[, klasse := factor(klasse, levels = names(kleuren_bodemklasse))]
bk_geb <- bk_long[, .(n = .N), by = .(regio, laag, Gebiedsnaam, klasse)]
bk_geb[, fractie := n / sum(n), by = .(regio, laag, Gebiedsnaam)]
bk_geb[, n_geb := sum(n), by = .(regio, laag, Gebiedsnaam)]
bk_geb <- bk_geb[n_geb >= 4]
# Gebieden sorteren op aandeel veen in de toplaag
veen_orde <- bk_geb[laag == 'Oever 0–25 cm' & klasse == 'Veen'][order(regio, fractie), Gebiedsnaam]
bk_geb[, Gebiedsnaam := factor(Gebiedsnaam, levels = unique(c(veen_orde, unique(Gebiedsnaam))))]
bk_regio <- bk_long[, .(n = .N), by = .(regio, laag, klasse)][, fractie := n / sum(n), by = .(regio, laag)]
message('bodemklasse per regio (0-25 cm):'); print(dcast(bk_regio[laag == 'Oever 0–25 cm'], regio ~ klasse, value.var = 'fractie', fill = 0)[, lapply(.SD, function(x) if (is.numeric(x)) round(100 * x) else x)])

p_bk <- ggplot(bk_geb, aes(y = Gebiedsnaam, x = fractie, fill = klasse)) +
  geom_col(width = 0.8) +
  facet_grid(regio ~ laag, scales = 'free_y', space = 'free_y', labeller = labeller(regio = strip_regio)) +
  scale_x_continuous(labels = scales::label_percent(decimal.mark = ','), expand = c(0, 0)) +
  scale_fill_manual(values = kleuren_bodemklasse, name = 'Bodemklasse') +
  labs(y = NULL, x = 'Aandeel sloten per gebied',
       title = 'Bodemklasse van de oever per gebied, oost en west',
       caption = 'Klasse per sloot op basis van klei% (van minerale delen) en OS% (= 1,72 x TOC), gemiddeld over jaren; gebieden met minstens 4 sloten.') +
  thema + theme(plot.caption = element_text(hjust = 0), plot.caption.position = 'plot')
ggsave(paste0(out_dir, 'bodemklasse_per_gebied_oost_west.png'), p_bk, width = 22, height = 17, units = 'cm', dpi = 300, bg = 'white')
message('geschreven: bodemklasse_per_gebied_oost_west.png')
