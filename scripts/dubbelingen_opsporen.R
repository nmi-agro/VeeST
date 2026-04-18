# Find duplicate SlootID-jaar combinations
# abio_proj <- abio_proj[WP %in% c('WP1','WP2'),]
dubbelen <- abio_proj[, .N, by = c('SlootID','jaar')][N > 1]
# Select duplicate rows
dub_rows <- abio_proj[dubbelen, on = c('SlootID','jaar')]
# Maak geldige kolomnamen
setnames(dub_rows, make.names(names(dub_rows)))
# Herbereken cols_to_compare
cols_to_compare <- setdiff(names(dub_rows), c('SlootID','jaar'))
# Vergelijk alleen bestaande kolommen
verschillen <- dub_rows[, lapply(.SD, function(x) length(unique(x)) > 1), .SDcols = cols_to_compare, by = .(SlootID, jaar)]
verschillen_long <- melt(verschillen, id.vars = c('SlootID','jaar'))
verschillen_long <- verschillen_long[value == TRUE]

## abio aggregaties controleren
# aggregate numeric columns by sloot en gebied
cols_num <- colnames(abio)[sapply(abio, is.numeric)]
cols_num <- cols_num[!grepl('traject',cols_num)&!grepl('subsamples',cols_num)]
cols_wat <- cols_num[grepl('*water*',cols_num)]
cols_wat <- c(cols_wat)
cols_wat <- cols_wat[!cols_wat%in%c("waterdiepte1_mid_cm","waterdiepte2_mid_cm","beheersporen_water1_2a_hoeveel")]
# 1 per sloot, gebied, behandeling (kan verschillen als behandeling verschilt per oeverzijde dan is projectie nodig)
cols_slib <- cols_num[grepl('*slib*',cols_num)]
cols_slib <- c(cols_slib,'holleoever','doorzicht2_mid_cm')
# anders per slootID/ monster
cols_overig <-  c("uitraster_perc","uitraster_afstand_sloot_m","afscheur_veg_lengte_perc","afscheur_veg_breedte_cm",
                  "landgebruik_traject","landgebruik_overkant",
                  "beheersporen_water1_2a_hoeveel","beheersporen_water1_2a_welke","beheersporen_water1_2a_overig",
                  "beheersporen_oever2b_3_hoeveel","beheersporen_oever2b_3_welke","beheersporen_oever2b_3_overig",
                  "peilsporen_hoeveel","peilsporen_richting","peilsporen_uitleg","ondergrondse_drainage")  
cols_overig <- cols_overig[!cols_overig %in% cols_wat&!cols_overig %in% cols_slib]
# watersamples 1 per sloot
abio_wat_agg <- abio[,lapply(.SD,mean,na.rm=TRUE),.SDcols=cols_wat,by=c('gebied','sloot','jaar')]
# slibsamples 1 per sloot and behandeling
abio_slib_agg <- abio[,lapply(.SD,mean,na.rm=TRUE),.SDcols=cols_slib,by=c('SlootID','gebied','Gebiedsnaam','WP','sloot','Sloot_nr','Behandeling','jaar')]
# als NA dan waarde zelfde gebied, sloot en meest gelijkende behandeling projecteren
abio_slib_agg[,behandeling_1 := sapply(strsplit(Behandeling, '-'), `[`, 1)]
abio_slib_agg[,behandeling_2 := ifelse(grepl('AF', Behandeling),"AF",NA)]
setDT(abio_slib_agg)
abio_mis <- abio_slib_agg[is.na(slib_redox_mgL)&is.na(holleoever),]
abio_mis <- merge(abio_mis, abio_slib_agg[!is.na(slib_redox_mgL)&!is.na(holleoever),], by = c('gebied','sloot','behandeling_1','behandeling_2','jaar'), suffixes =c('_mis',''))
abio_mis[,Behandeling:=Behandeling_mis]
abio_mis[,SlootID:=SlootID_mis]
abio_mis <-  abio_mis[,!grepl('_mis',colnames(abio_mis)), with = FALSE]
abio_slib_agg <- rbind(abio_slib_agg[!is.na(slib_redox_mgL)|!is.na(holleoever),],abio_mis, fill=TRUE)
