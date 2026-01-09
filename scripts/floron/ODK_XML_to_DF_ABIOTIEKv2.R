#Compile ODK instances from collect VeeST-ABIOforms to dataframe using form def. xml
#Abiotiek

#packages####
if (!require('xml2')) install.packages('xml2'); library('xml2')
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('reshape2')) install.packages('reshape2'); library('reshape2')
if (!require('tools')) install.packages('tools'); library('tools')
df_from_names <- function(colname){
  df <- as.data.frame(matrix(ncol=length(colname), nrow=0))
  colnames(df)<-colname
  df
} #maak een df van een vector met namen
if.na.report.na <- function(x){ifelse(identical(x,factor(0)) | identical(x,character(0)) | identical(x,numeric(0)) | identical(x,integer(0)) | identical(x,NULL),NA,x )}
if.na.report.natxt <- function(x){ifelse(identical(x,factor(0)) | identical(x,character(0)) | identical(x,numeric(0)) | identical(x,integer(0))| identical(x,NULL),"NA",x )}
#####


#Load data
print("Zet alle instances mappen in 1 overkoepelende map en geef in 'xlm_path' op waar deze overkoepelende map staat")
xlm_path <- "C:/Users/Michiel (FLORON)/Downloads/Harm_abiotiek_ODKinstances/instances/" #DIT IS DE ODK-COLLECT 'instances' folder! Alleen dit aanpassen als t goed is. 



#SCRIPT####
xml_paths <-list.dirs(xlm_path, recursive=F)
results_path <- "//miii/Ravon-Projecten2/Projecten/2021(deel verplaatst naar archief)/2021.000 FLORON projecten/2021.188 Veenweidesloot van de toekomst/Databestanden/ABIO/ODK/"
photo_path <-   "//miii/Ravon-Projecten2/Projecten/2021(deel verplaatst naar archief)/2021.000 FLORON projecten/2021.188 Veenweidesloot van de toekomst/Foto/ODK/"
kolommen <- c("startnotitie",
              "Date_start_auto", "Date_end_auto","Device_ID",
              "uitleg",
              "Datemanual",
              "Waarnemer",
              "SlootID",
              "Welke_oever",
              "Start_traject",
              "photo1",
              "uitraster",
              "WP2_beheer",
              "WP2_beheer_anders",
              "watersample_note",
              "watermonster",
              "watermonster_subsamples",
              "watermonster_code",
              "watertemp_C",
              "water_conductiviteit_uS_cm",
              "water_redox",
              "water_pH",
              "water_O2_mgL",
              "notitie",
              "doorzicht1_mid_cm",
              "waterdiepte1_mid_cm",
              "bodemdiepte1_mid_cm",
              "doorzicht2_mid_cm",
              "waterdiepte2_mid_cm",
              "bodemdiepte2_mid_cm",
              "doorzicht3_mid_cm",
              "waterdiepte3_mid_cm",
              "bodemdiepte3_mid_cm",
              "holleoever",
              "profiel",
              "profiel_subsamples",
              "tekening_profiel",
              "peneterometer",
              "peneterometer_subsamples",
              "slib_conductiviteit_uS_cm",
              "slib_pH",
              "slib_O2_mgL",
              "slib_redox_mgL",
              "uitraster_perc",
              "uitraster_afstand_sloot_m",
              "landgebruik_traject",
              "landgebruik_overkant",
              "beheersporen_oever2b_3_welke",
              "beheersporen_oever2b_3_hoeveel",
              "beheersporen_oever2b_3_overig",
              "beheersporen_water1_2a_welke",
              "beheersporen_water1_2a_hoeveel",
              "beheersporen_water1_2a_overig",
              "afscheur_veg_lengte_perc",
              "afscheur_veg_breedte_cm",
              "peilsporen_richting",
              "peilsporen_hoeveel",
              "peilsporen_uitleg",
              "ondergrondse_drainage",
              "comment",
              "End_traject",
              "photo2",
              "Start_eindpunt_container",
              "Afstand_startpunt_eindpunt_m",
              "display_distance_to_shortlong",
              "akkoord_met_te_kortlang",
              "uitleg_trajectlengte",
              "porievochtmonster",
              "porievochtmonster_subsamples",
              "porievochtmonster_code",
              "slibmonster",
              "slibmonster_subsamples",
              "bodemmonster_water",
              "bodemmonster_water_subsamples",
              "bodemmonster_oeverzone2b3",
              "bodemmonster_oeverzone2b3_subsamples",
              "bodemmonster_oever2b3_boven",
              "bodemmonster_oever2b3_boven_subsamples",
              "bodemmonster_oeverzone2b3_diep",
              "bodemmonster_oeverzone2b3_diep_subsamples",
              "endnote",
              "instanceID") #Kolomnamen opgehaald nieuwste versie formulier (langste versie)
# xml_df_wide <- df_from_names(xml_df$data_id)
# write.table(colnames(xml_df_wide), "clipboard", sep=",", row.names=FALSE, col.names=FALSE)

#voor de eerste instance los om DF te maken
xml_address <- paste0(xml_paths[1],"/",
                     grep(".xml",list.files(xml_paths[1], include.dirs = F), value=T))

xml_instance = as_list(read_xml(xml_address)) #laad xml form

xml_df = tibble::as_tibble(xml_instance) %>%
  unnest_longer(data, keep_empty = T) #data is de eerste lijst in de XML

xml_df_wide <- df_from_names(kolommen)

#DF vullen met rest van de instances
for(xml in xml_paths){

  xml_address <- paste0(xml,"/",
                       grep(".xml",list.files(xml, include.dirs = F), value=T))
  
  xml_instance = as_list(read_xml(xml_address))

  xml_df = tibble::as_tibble(xml_instance) %>%
    unnest_longer(data, keep_empty = T) #data is de eerste lijst in de XML
  #check welke velden miste
  
  #stript voor veldform v240507 & v240419
  if (grepl("v3",xml,fixed=T)){  
  
    #missende namen invullen 
    if(xml_df[2,"data_id"]=="" | is.na(xml_df[2,"data_id"])){
      xml_df[2,"data_id"] <- "Date_start_auto"
    }
    if(xml_df[3,"data_id"]==""| is.na(xml_df[3,"data_id"])){
      xml_df[3,"data_id"] <- "Date_end_auto"
    }
    if(xml_df[4,"data_id"]==""| is.na(xml_df[4,"data_id"])){
      xml_df[4,"data_id"] <- "Device_ID"
    }
    if(xml_df[63,"data_id"]==""| is.na(xml_df[63,"data_id"])){
      xml_df[63,"data_id"] <- "Start_eindpunt_container"
    }
    if(xml_df[64,"data_id"]==""| is.na(xml_df[64,"data_id"])){
      xml_df[64,"data_id"] <- "Afstand_startpunt_eindpunt_m"
    }
  }else if(grepl("v240426",xml,fixed=T)){
      #missende namen invullen 
      if(xml_df[2,"data_id"]=="" | is.na(xml_df[2,"data_id"])){
        xml_df[2,"data_id"] <- "Date_start_auto"
      }
      if(xml_df[3,"data_id"]==""| is.na(xml_df[3,"data_id"])){
        xml_df[3,"data_id"] <- "Date_end_auto"
      }
      if(xml_df[4,"data_id"]==""| is.na(xml_df[4,"data_id"])){
        xml_df[4,"data_id"] <- "Device_ID"
      }
      if(xml_df[61,"data_id"]==""| is.na(xml_df[61,"data_id"])){
        xml_df[61,"data_id"] <- "Start_eindpunt_container"
      }
      if(xml_df[62,"data_id"]==""| is.na(xml_df[62,"data_id"])){
        xml_df[62,"data_id"] <- "Afstand_startpunt_eindpunt_m"
      }
    }  else {
      print("ERROR: Check kolomnamen!")
      stop()
    }

  #pas voorwaardelijke kolomnamen aan
  xml_df[xml_df$data_id %in% c("akkoord_met_te_lang","akkoord_met_te_kort"),"data_id"] <- "akkoord_met_te_kortlang"
  xml_df[xml_df$data_id %in% c("display_distance_to_long","display_distance_to_short"),"data_id"] <- "display_distance_to_shortlong"

  #Verwijder lege kolommen (niet ingevulde waarden)
  xml_df <- xml_df[!is.na(xml_df$data_id) &
                     xml_df$data_id!="",]
  
  if(all(xml_df$data_id %in% colnames(xml_df_wide))){
    xml_df_wide[nrow(xml_df_wide)+1,] <- NA #add row
    for(col in xml_df$data_id){
      xml_df_wide[nrow(xml_df_wide),col]<- if.na.report.na(unlist(xml_df$data[xml_df$data_id==col]))
    }
  } else {
    print("ERROR, check colnames!")
    stop()
  }
  
  #rename photos and place in photo folder
  startfoto <- if.na.report.na(unlist(xml_df[xml_df$data_id=="photo1","data"]))
  eindfoto <- if.na.report.na(unlist(xml_df[xml_df$data_id=="photo2","data"]))
  tekening <- if.na.report.na(unlist(xml_df[xml_df$data_id=="tekening_profiel","data"]))
  slootID <- if.na.report.natxt(unlist(xml_df[xml_df$data_id=="SlootID","data"]))
  oever <- if.na.report.natxt(unlist(xml_df[xml_df$data_id=="Welke_oever","data"]))
  datum <- if.na.report.natxt(unlist(xml_df[xml_df$data_id=="Datemanual","data"]))
  if(!is.na(startfoto)){
    file.copy(from=paste0(xml,"/",startfoto),
              to=paste0(photo_path, slootID,"-",oever,"-start-",datum,"_ABIO.",file_ext(startfoto)),
             overwrite=F, recursive=F)
  }
  if(!is.na(eindfoto)){
    file.copy(from=paste0(xml,"/",eindfoto),
              to=paste0(photo_path, slootID,"-",oever,"-eind-",datum,"_ABIO.",file_ext(eindfoto)),
              overwrite=F, recursive=F)
  }
  if(!is.na(tekening)){
    file.copy(from=paste0(xml,"/",tekening),
              to=paste0(photo_path, slootID,"-",oever,"-profiel-",datum,"_ABIO.",file_ext(tekening)),
              overwrite=F, recursive=F)
  }
rm(startfoto,eindfoto,tekening,slootID,oever,datum)
}


#kolommen toevoegen
for(r in 1:nrow(xml_df_wide)){
  xml_df_wide[r,"Start_traject_long"] <- if.na.report.na(strsplit(x=as.character(xml_df_wide[r,"Start_traject"]),split=" ", fixed=T)[[1]][1])
  xml_df_wide[r,"Start_traject_lat"] <- if.na.report.na(strsplit(x=as.character(xml_df_wide[r,"Start_traject"]),split=" ", fixed=T)[[1]][2])
  xml_df_wide[r,"Start_traject_accur_m"] <- if.na.report.na(strsplit(x=as.character(xml_df_wide[r,"Start_traject"]),split=" ", fixed=T)[[1]][4])
  
  xml_df_wide[r,"End_traject_long"] <- if.na.report.na(strsplit(x=as.character(xml_df_wide[r,"End_traject"]),split=" ", fixed=T)[[1]][1])
  xml_df_wide[r,"End_traject_lat"] <- if.na.report.na(strsplit(x=as.character(xml_df_wide[r,"End_traject"]),split=" ", fixed=T)[[1]][2])
  xml_df_wide[r,"End_traject_accur_m"] <- if.na.report.na(strsplit(x=as.character(xml_df_wide[r,"End_traject"]),split=" ", fixed=T)[[1]][4])
  
  if(!is.na(xml_df_wide[r,"photo1"])){
  xml_df_wide[r,"photo1_newname"] <- paste0(xml_df_wide[r,"SlootID"],"-",xml_df_wide[r,"Welke_oever"],"-start-",xml_df_wide[r,"Datemanual"],"_ABIO")
  }
  if(!is.na(xml_df_wide[r,"photo2"])){
    xml_df_wide[r,"photo2_newname"] <- paste0(xml_df_wide[r,"SlootID"],"-",xml_df_wide[r,"Welke_oever"],"-eind-",xml_df_wide[r,"Datemanual"],"_ABIO")
  }
  if(!is.na(xml_df_wide[r,"tekening_profiel"])){
    xml_df_wide[r,"tekening_profiel_newname"] <- paste0(xml_df_wide[r,"SlootID"],"-",xml_df_wide[r,"Welke_oever"],"-profiel-",xml_df_wide[r,"Datemanual"],"_ABIO")
  }
}



#nummerkolommen opslaan als nummers
nummerkolommen <- c("watermonster_subsamples",
                    "watertemp_C",
                    "water_conductiviteit_uS_cm",
                    "water_redox",
                    "water_pH",
                    "water_O2_mgL",
                    "doorzicht1_mid_cm",
                    "waterdiepte1_mid_cm",
                    "bodemdiepte1_mid_cm",
                    "doorzicht2_mid_cm",
                    "waterdiepte2_mid_cm",
                    "bodemdiepte2_mid_cm",
                    "doorzicht3_mid_cm",
                    "waterdiepte3_mid_cm",
                    "bodemdiepte3_mid_cm",
                    "holleoever",
                    "profiel_subsamples",
                    "peneterometer_subsamples",
                    "slib_conductiviteit_uS_cm",
                    "slib_pH",
                    "slib_O2_mgL",
                    "slib_redox_mgL",
                    "uitraster_perc",
                    "uitraster_afstand_sloot_m",
                    "beheersporen_oever2b_3_hoeveel",
                    "beheersporen_water1_2a_hoeveel",
                    "afscheur_veg_lengte_perc",
                    "afscheur_veg_breedte_cm",
                    "peilsporen_hoeveel",
                    "Afstand_startpunt_eindpunt_m",
                    "porievochtmonster_subsamples",
                    "slibmonster_subsamples",
                    "bodemmonster_water_subsamples",
                    "bodemmonster_oeverzone2b3_subsamples",
                    "bodemmonster_oever2b3_boven_subsamples",
                    "bodemmonster_oeverzone2b3_diep_subsamples",
                    "Start_traject_long",
                    "Start_traject_lat",
                    "Start_traject_accur_m",
                    "End_traject_long",
                    "End_traject_lat",
                    "End_traject_accur_m")
for(kolom in nummerkolommen){
  xml_df_wide[,kolom] <- as.numeric(as.character(xml_df_wide[,kolom]))
}

#tabel opslaan
write.csv2(xml_df_wide, paste0(results_path,"VeeST_veldformulier_ABIO_fromR_",max(as.Date(x=xml_df_wide$Datemanual,format="%Y-%m-%d"),na.rm=T),".csv"),
           row.names=F)
#####
