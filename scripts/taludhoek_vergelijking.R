# Vergelijking taludhoeken rond de waterlijn met taludhoeken over 3 m / 1 m
#
# Vergelijkt per profielsectie (profiel x sectie_2 x jaar) de taludhoek rond de
# waterlijn (tldk_oevrwtr_perc, tldk_wtrwtr_perc) met de taludhoek over een
# vaste horizontale afstand (tldk_bvwtr_perc: 3 m boven water,
# tldk_ondwtr_perc: 1 m onder water). Bekijkt:
#   1. hoe vaak in de modeldata de hoek rond de waterlijn is aangevuld met de
#      hoek over 3 m / 1 m (zie data_import_ppr.R, "select best talud");
#   2. de samenhang tussen beide varianten (Spearman);
#   3. de stabiliteit tussen meetjaren (Spearman eerste vs. laatste jaar per
#      profielsectie);
#   4. de samenhang met de grilligheid van de oeverlijn (oeverzone_2b_grillig).
#
# Invoer: Processed_data_workspace.RData (object `profiel`, ruwe taludhoeken
# vóór het aanvullen) en output/rapport/abio_proj.rds (modeldata).
# Uitvoer: output/rapport/taludhoek_vergelijking.rds, ingelezen door
# rapport_modellering_VeeST.qmd.

library(data.table)
library(sf)

workspace <- paste0(
  Sys.getenv("NMI-SITE"),
  "/O 1900 - O 2000/1922.N.23 VeeST vwsloot vd toekomst/05. Data/"
)
rds_dir <- paste0(workspace, "output/rapport/")

ws <- new.env()
load(paste0(workspace, "/Processed_data_workspace.RData"), envir = ws)
abio_proj <- as.data.table(readRDS(paste0(rds_dir, "abio_proj.rds")))

tldk_vars <- c(
  "tldk_oevrwtr_perc", "tldk_bvwtr_perc",
  "tldk_wtrwtr_perc", "tldk_ondwtr_perc"
)

# Eén rij per profielsectie en jaar, met de ruwe (niet aangevulde) hoeken
secties <- unique(
  ws$profiel[, c("name", "sectie_2", "jaar", "SlootID", tldk_vars), with = FALSE]
)
secties <- secties[,
  lapply(.SD, function(x) x[!is.na(x)][1]),
  by = .(name, sectie_2, jaar),
  .SDcols = c("SlootID", tldk_vars)
]
secties[, groep := paste(name, sectie_2)]

#' Spearman-correlatie met p-waarde en aantal paren
#'
#' @param x,y Numerieke vectoren van gelijke lengte.
#' @return Named numeric vector met `rho`, `p` en `n` (aantal complete paren).
spearman <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  ct <- suppressWarnings(cor.test(x[ok], y[ok], method = "spearman"))
  c(rho = unname(ct$estimate), p = ct$p.value, n = sum(ok))
}

# 1. Aanvullen in de modeldata --------------------------------------------------
model <- merge(
  abio_proj[, .(
    SlootID,
    jaar = as.numeric(jaar),
    oevr_model = tldk_oevrwtr_perc,
    wtr_model = tldk_wtrwtr_perc
  )],
  secties[!is.na(SlootID)],
  by = c("SlootID", "jaar")
)
aanvulling <- list(
  n_gekoppeld = nrow(model),
  n_boven = model[!is.na(oevr_model) & is.na(tldk_oevrwtr_perc), .N],
  n_onder = model[!is.na(wtr_model) & is.na(tldk_wtrwtr_perc), .N]
)

# 2. Verdeling en samenhang -----------------------------------------------------
verdeling <- secties[, lapply(.SD, function(x) {
  quantile(x, c(0.1, 0.5, 0.9), na.rm = TRUE)
}), .SDcols = tldk_vars]
verdeling[, kwantiel := c("P10", "P50", "P90")]
samenhang <- rbind(
  boven = spearman(secties$tldk_oevrwtr_perc, secties$tldk_bvwtr_perc),
  onder = spearman(secties$tldk_wtrwtr_perc, secties$tldk_ondwtr_perc)
)

# 3. Stabiliteit tussen meetjaren -----------------------------------------------
herhaald <- secties[groep %in% secties[, .N, by = groep][N >= 2, groep]]
setorder(herhaald, groep, jaar)
eerste_laatste <- herhaald[, .SD[c(1, .N)], by = groep]
eerste_laatste[, meting := c("eerste", "laatste"), by = groep]
breed <- dcast(eerste_laatste, groep ~ meting, value.var = tldk_vars)
stabiliteit <- t(sapply(tldk_vars, function(v) {
  spearman(breed[[paste0(v, "_eerste")]], breed[[paste0(v, "_laatste")]])
}))

# 4. Grilligheid van de oeverlijn -----------------------------------------------
grillig <- unique(abio_proj[, .(
  SlootID,
  jaar = as.numeric(jaar),
  grillig = as.numeric(factor(
    oeverzone_2b_grillig,
    levels = c("uniform", "matig", "zeer")
  ))
)])
met_grillig <- merge(secties[!is.na(SlootID)], grillig, by = c("SlootID", "jaar"))
met_grillig[, `:=`(
  verschil_boven = abs(tldk_oevrwtr_perc - tldk_bvwtr_perc),
  verschil_onder = abs(tldk_wtrwtr_perc - tldk_ondwtr_perc)
)]
grillig_verschil <- rbind(
  boven = spearman(met_grillig$verschil_boven, met_grillig$grillig),
  onder = spearman(met_grillig$verschil_onder, met_grillig$grillig)
)
verandering <- met_grillig[
  groep %in% met_grillig[, .N, by = groep][N >= 2, groep]
][order(jaar)][,
  c(
    lapply(.SD, function(x) abs(last(x) - first(x))),
    .(grillig = mean(grillig, na.rm = TRUE))
  ),
  by = groep,
  .SDcols = tldk_vars
]
grillig_verandering <- t(sapply(tldk_vars, function(v) {
  c(
    spearman(verandering[[v]], verandering$grillig),
    mediaan_delta = median(verandering[[v]], na.rm = TRUE)
  )
}))

taludhoek_vergelijking <- list(
  n_secties = nrow(secties),
  n_secties_herhaald = uniqueN(herhaald$groep),
  aanvulling = aanvulling,
  verdeling = verdeling,
  samenhang = samenhang,
  stabiliteit = stabiliteit,
  n_grillig = met_grillig[!is.na(grillig), .N],
  grillig_verschil = grillig_verschil,
  grillig_verandering = grillig_verandering
)
saveRDS(taludhoek_vergelijking, paste0(rds_dir, "taludhoek_vergelijking.rds"))
print(taludhoek_vergelijking)
