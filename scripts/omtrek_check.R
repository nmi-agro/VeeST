library(data.table)
library(sf)
library(ggplot2)

workspace <- paste0(Sys.getenv("NMI-SITE"), "O 1900 - O 2000/1922.N.23 VeeST vwsloot vd toekomst/05. Data/")
workspace2 <- "c:/Users/LauraMoria/Stichting Veenweiden Innovatiecentrum/VIPNL Themas - VIPNL Veenweidensloot/D. Data en analyse/"
out <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(out)) out <- paste0(workspace, "output/")

cluster <- read_sf(paste0(workspace, "./GIS/clusters_aan_drglg_peil.gpkg"))
cat("Kolommen:\n"); print(names(cluster))

locaties <- readxl::read_excel(paste0(workspace2, "analysePlan/wp_locaties_naam_correcties.xlsx"), sheet = "locaties")
setDT(locaties)
locaties$geom <- sprintf("LINESTRING(%s %s, %s %s)", locaties$Start_traject_lat, locaties$Start_traject_long, locaties$End_traject_lat, locaties$End_traject_long)
locaties <- locaties[geom == "LINESTRING(NA NA, NA NA)", geom := "LINESTRING(0 0, 0 0)"]
locaties <- st_as_sf(locaties, wkt = "geom", crs = 4326) |> st_transform(28992)
cl_locs <- st_join(locaties[!is.na(locaties$instanceID_abio) & !locaties$WP %in% "WP2-prenul", ], cluster, st_nearest_feature, left = TRUE)
setDT(cl_locs)
cl_locs <- unique(cl_locs[, .(SlootID, omtrek_nat, omtrek_length, GEOMETRIE_Area, afwatopp)])

cl <- as.data.table(st_drop_geometry(cluster))
for (d in list(alle_percelen = cl, veest_locaties = cl_locs)) {
  cat("\n---\n")
  print(summary(d[, .(omtrek_nat, omtrek_length, GEOMETRIE_Area, afwatopp)]))
  cat("ratio omtrek_nat/omtrek_length:\n"); print(summary(d$omtrek_nat / d$omtrek_length))
  cat("check afwatopp == Area/(omtrek_nat/2):\n"); print(summary(d$afwatopp / (d$GEOMETRIE_Area / (d$omtrek_nat / 2))))
  cat("perimeter vierkant uit Area (4*sqrt(A)) / omtrek_length:\n"); print(summary(4 * sqrt(d$GEOMETRIE_Area) / d$omtrek_length))
}
cat("\nn VeeST-sloten:", nrow(cl_locs), "\n")

p <- ggplot(cl_locs, aes(x = omtrek_nat)) +
  geom_histogram(bins = 30, fill = "#4C8C6B", colour = "white") +
  labs(x = "omtrek_nat (m natte perceelsrand)", y = "Aantal sloten",
       title = "Natte omtrek van percelen bij VeeST-sloten") +
  theme_minimal()
ggsave(file.path(out, "hist_omtrek_nat.png"), p, width = 7, height = 4.5, dpi = 120)
