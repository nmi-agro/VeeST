#Compile ODK instances from collect VeeST-VEGforms to dataframe using form def. xml
#Veg

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
xlm_path <- "C:/Users/Michiel (FLORON)/Downloads/ODK_combi/instances/" #DIT IS DE ODK-COLLECT 'instances' folder! Alleen dit aanpassen als t goed is. 



#SCRIPT####
xml_paths <-list.dirs(xlm_path, recursive=F)
results_path <- "//miii/Ravon-Projecten2/Projecten/2021(deel verplaatst naar archief)/2021.000 FLORON projecten/2021.188 Veenweidesloot van de toekomst/Databestanden/VEG/ODK/"
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
              "WP2_raster",
              "WP2_beheer",
              "WP2_beheer_anders",
              "Waterzone_1_breedte_cm",
              "Oeverzone_2a_breedte_cm",
              "Oeverzone_2a_grillig",
              "Oeverzone_2b_breedte_cm",
              "Oeverzone_2b_grillig",
              "Terzone_3_breedte_cm",
              "Waterzone_1_kaal_perc",
              "Waterzone_1_subm_tot_perc",
              "Waterzone_1_natans_perc",
              "Waterzone_1_kroos_perc",
              "Waterzone_1_FLAB_perc",
              "Waterzone_1_emers_perc",
              "Oeverzone_2a_kaal_perc",
              "Oeverzone_2a_subm_tot_perc",
              "Oeverzone_2a_natans_perc",
              "Oeverzone_2a_emers_perc",
              "Oeverzone_2a_kraggen_perc",
              "Oeverzone_2a_ter_gras_kruid_perc",
              "Oeverzone_2a_hout_perc",
              "Oeverzone_2b_kaal_perc",
              "Oeverzone_2b_emers_perc",
              "Oeverzone_2b_kraggen_perc",
              "Oeverzone_2b_ter_gras_kruid_perc",
              "Oeverzone_2b_ter_prodgras_perc",
              "Oeverzone_2b_hout_perc",
              "Terzone_3_kaal_perc",
              "Terzone_3_ter_gras_kruid_perc",
              "Terzone_3_emers_perc",
              "Terzone_3_ter_prodgras_perc",
              "Terzone_3_hout_perc",
              "Perceel_4_kaal_perc",
              "Perceel_4_ter_gras_kruid_perc",
              "Perceel_4_ter_prodgras_perc",
              "Perceel_4_hout_perc",
              "Waterzone_1_gem_veg_diepte_cm",
              "Waterzone_1_min_veg_diepte_cm",
              "Oeverzone_2a_gem_veg_hoogte_cm",
              "Oeverzone_2b_gem_veg_hoogte_cm",
              "Terzone_3_gem_veg_hoogte_cm",
              "Oeverzone_2b_Vertrapping_schade",
              "Oeverzone_2b_Vertrapping_lengte_perc",
              "Terzone_3_Vertrapping_schade",
              "Terzone_3_Vertrapping_lengte_perc",
              "Waterzone_1_veg_schade_totaal",
              "Waterzone_1_veg_schade_oorzaak",
              "Oeverzone_2a_veg_schade_totaal",
              "Oeverzone_2a_veg_schade_oorzaak",
              "Oeverzone_2b_veg_schade_totaal",
              "Oeverzone_2b_veg_schade_oorzaak",
              "Terzone_3_veg_schade_totaal",
              "Terzone_3_veg_schade_oorzaak",
              "oorzaak_veg_schade",
              "Oeverzone_2_veg_vorigjaar",
              "Karper_brasem",
              "rivierkreeft",
              "rivierkreeft_uitleg",
              "fauna_obs_overig",
              "End_traject",
              "photo2",
              "comment",
              "Start_eindpunt_container",
              "Afstand_startpunt_eindpunt_m",
              "display_trajectlengte",
              "display_distance_to_shortlong",
              "akkoord_met_te_kortlang",
              "uitleg_trajectlengte",
              "veg_opname_VERA_zone1",
              "veg_opname_VERA_zone2a_los",
              "veg_opname_VERA_zone2b_los",
              "veg_opname_VERA_zone2a2b_samen",
              "veg_opname_VERA_zone3",
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
  if (grepl("v240507",xml,fixed=T)){  
  
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
    if(xml_df[75,"data_id"]==""| is.na(xml_df[75,"data_id"])){
      xml_df[75,"data_id"] <- "Start_eindpunt_container"
    }
    if(xml_df[76,"data_id"]==""| is.na(xml_df[76,"data_id"])){
      xml_df[76,"data_id"] <- "Afstand_startpunt_eindpunt_m"
    }
  }else if(grepl("v240419",xml,fixed=T)){
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
      if(xml_df[73,"data_id"]==""| is.na(xml_df[73,"data_id"])){
        xml_df[73,"data_id"] <- "Start_eindpunt_container"
      }
      if(xml_df[74,"data_id"]==""| is.na(xml_df[74,"data_id"])){
        xml_df[74,"data_id"] <- "Afstand_startpunt_eindpunt_m"
      }
  }else if(grepl("v3",xml,fixed=T)){
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
    if(xml_df[75,"data_id"]==""| is.na(xml_df[73,"data_id"])){
      xml_df[75,"data_id"] <- "Start_eindpunt_container"
    }
    if(xml_df[76,"data_id"]==""| is.na(xml_df[74,"data_id"])){
      xml_df[76,"data_id"] <- "Afstand_startpunt_eindpunt_m"
    }
  } else {
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
  slootID <- if.na.report.natxt(unlist(xml_df[xml_df$data_id=="SlootID","data"]))
  oever <- if.na.report.natxt(unlist(xml_df[xml_df$data_id=="Welke_oever","data"]))
  datum <- if.na.report.natxt(unlist(xml_df[xml_df$data_id=="Datemanual","data"]))
  if(!is.na(startfoto)){
    file.copy(from=paste0(xml,"/",startfoto),
              to=paste0(photo_path, slootID,"-",oever,"-start-",datum,"_VEG.",file_ext(startfoto)),
             overwrite=F, recursive=F)
  }
  if(!is.na(eindfoto)){
    file.copy(from=paste0(xml,"/",eindfoto),
              to=paste0(photo_path, slootID,"-",oever,"-eind-",datum,"_VEG.",file_ext(eindfoto)),
              overwrite=F, recursive=F)
  }
rm(startfoto,eindfoto,slootID,oever,datum)
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
    xml_df_wide[r,"photo1_newname"] <- paste0(xml_df_wide[r,"SlootID"],"-",xml_df_wide[r,"Welke_oever"],"-start-",xml_df_wide[r,"Datemanual"],"_VEG")
  }
  if(!is.na(xml_df_wide[r,"photo2"])){
    xml_df_wide[r,"photo2_newname"] <- paste0(xml_df_wide[r,"SlootID"],"-",xml_df_wide[r,"Welke_oever"],"-eind-",xml_df_wide[r,"Datemanual"],"_VEG")
  }
}

#nummerkolommen opslaan als nummers
nummerkolommen <- c("Waterzone_1_breedte_cm",
                    "Oeverzone_2a_breedte_cm",
                    "Oeverzone_2b_breedte_cm",
                    "Terzone_3_breedte_cm",
                    "Waterzone_1_kaal_perc",
                    "Waterzone_1_subm_tot_perc",
                    "Waterzone_1_natans_perc",
                    "Waterzone_1_kroos_perc",
                    "Waterzone_1_FLAB_perc",
                    "Waterzone_1_emers_perc",
                    "Oeverzone_2a_kaal_perc",
                    "Oeverzone_2a_subm_tot_perc",
                    "Oeverzone_2a_natans_perc",
                    "Oeverzone_2a_emers_perc",
                    "Oeverzone_2a_kraggen_perc",
                    "Oeverzone_2a_ter_gras_kruid_perc",
                    "Oeverzone_2a_hout_perc",
                    "Oeverzone_2b_kaal_perc",
                    "Oeverzone_2b_emers_perc",
                    "Oeverzone_2b_kraggen_perc",
                    "Oeverzone_2b_ter_gras_kruid_perc",
                    "Oeverzone_2b_ter_prodgras_perc",
                    "Oeverzone_2b_hout_perc",
                    "Terzone_3_kaal_perc",
                    "Terzone_3_ter_gras_kruid_perc",
                    "Terzone_3_emers_perc",
                    "Terzone_3_ter_prodgras_perc",
                    "Terzone_3_hout_perc",
                    "Perceel_4_kaal_perc",
                    "Perceel_4_ter_gras_kruid_perc",
                    "Perceel_4_ter_prodgras_perc",
                    "Perceel_4_hout_perc",
                    "Waterzone_1_gem_veg_diepte_cm",
                    "Waterzone_1_min_veg_diepte_cm",
                    "Oeverzone_2a_gem_veg_hoogte_cm",
                    "Oeverzone_2b_gem_veg_hoogte_cm",
                    "Terzone_3_gem_veg_hoogte_cm",
                    "Oeverzone_2b_Vertrapping_lengte_perc",
                    "Terzone_3_Vertrapping_lengte_perc",
                    "Afstand_startpunt_eindpunt_m",
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
write.csv2(xml_df_wide, paste0(results_path,"VeeST_veldformulier_veg_fromR_",max(as.Date(x=xml_df_wide$Datemanual,format="%Y-%m-%d"),na.rm=T),".csv"),
           row.names=F)
#####
