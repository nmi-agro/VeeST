# validatie abio------------------------------------
## Maak frequentietabel van combinatie SlootID en jaar in clusters_locs--------------------------------------------
frequency_table <- clusters_locs[, .N, by = .(SlootID, jaar)]

# Bekijk de tabel
print(frequency_table)

# Of als je een kruistabel wilt (SlootID in rijen, jaar in kolommen)
crosstab <- dcast(clusters_locs, SlootID ~ jaar, value.var = "SlootID", fun.aggregate = length)
print(crosstab)

# Voor een overzicht van de totalen
summary_table <- clusters_locs[, .(
  total_records = .N,
  unique_slootid = uniqueN(SlootID),
  unique_jaar = uniqueN(jaar)
), by = .(SlootID, jaar)]

print("Totaal overzicht:")
print(summary_table)

# Aantal unieke combinaties
cat("Totaal aantal unieke SlootID-jaar combinaties:", nrow(frequency_table), "\n")

# Aantal records per jaar
jaar_summary <- clusters_locs[, .N, by = jaar]
print("Records per jaar:")
print(jaar_summary)

# Aantal records per SlootID (top 10)
slootid_summary <- clusters_locs[, .N, by = SlootID][order(-N)][1:10]
print("Top 10 SlootID's met meeste records:")
print(slootid_summary)

## Maak frequentietabel van combinatie SlootID en jaar in abio_proj--------------------------------------------

frequency_table_abio <- abio_proj[, .N, by = .(SlootID, jaar)]

# Bekijk de tabel
print(frequency_table_abio)

# Of als je een kruistabel wilt (SlootID in rijen, jaar in kolommen)
crosstab_abio <- dcast(abio_proj, SlootID ~ jaar, value.var = "SlootID", fun.aggregate = length)
print(crosstab_abio)

# Voor een overzicht van de totalen
summary_table_abio <- abio_proj[, .(
  total_records = .N,
  unique_slootid = uniqueN(SlootID),
  unique_jaar = uniqueN(jaar)
), by = .(SlootID, jaar)]

print("Totaal overzicht abio_proj:")
print(summary_table_abio)

# Aantal unieke combinaties
cat("Totaal aantal unieke SlootID-jaar combinaties in abio_proj:", nrow(frequency_table_abio), "\n")

# Aantal records per jaar
jaar_summary_abio <- abio_proj[, .N, by = jaar]
print("Records per jaar in abio_proj:")
print(jaar_summary_abio)

# Aantal records per SlootID (top 10)
slootid_summary_abio <- abio_proj[, .N, by = c('SlootID','jaar')][order(-N)]
print("Top 10 SlootID's met meeste records in abio_proj:")
print(slootid_summary_abio)

# Vergelijking met clusters_locs
cat("\nVergelijking datasets:\n")
cat("Unieke SlootID-jaar combinaties in clusters_locs:", nrow(frequency_table), "\n")
cat("Unieke SlootID-jaar combinaties in abio_proj:", nrow(frequency_table_abio), "\n")

# Check welke combinaties in clusters_locs zitten maar niet in abio_proj
missing_in_abio <- merge(frequency_table[, .(SlootID, jaar)], 
                         frequency_table_abio[, .(SlootID, jaar)], 
                         by = c("SlootID", "jaar"), 
                         all.x = TRUE, all.y = FALSE)
missing_in_abio <- missing_in_abio[is.na(SlootID)]

if(nrow(missing_in_abio) > 0) {
  cat("Aantal combinaties die wel in clusters_locs maar niet in abio_proj zitten:", nrow(missing_in_abio), "\n")
  print(head(missing_in_abio))
} else {
  cat("Alle SlootID-jaar combinaties uit clusters_locs zitten ook in abio_proj\n")
}

