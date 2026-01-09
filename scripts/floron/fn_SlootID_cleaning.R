### ### ### ### ### ### ### ### ### ### 
### CUSTOM FUNCTION FOR
### CLEANING SlootIDs and SlootCodes 
### ### ### ### ### ### ### ### ### ###

slootid_clean_veg <- function(veg_df){
  veg_df |> 
    mutate(
      SlootID_old = SlootID,
      across(
        c(SlootID, Welke_oever), ~ str_to_upper(.)
      ),
      SlootID = str_replace_all(SlootID, pattern = "-|,", replacement = "_"),
      SlootID = str_replace(SlootID, pattern = "²", replacement = "2"),
      SlootID = case_when(
        str_sub(SlootID, 3,3) != "_" ~ paste0(str_sub(SlootID, 1, 2), "_", str_sub(SlootID, 3)),
        TRUE ~ SlootID),
      SlootID = case_when(
        str_sub(SlootID, -1) != Welke_oever ~ paste0(SlootID, "_", Welke_oever),
        TRUE ~ SlootID),
      SlootID = str_remove_all(SlootID, pattern = "\\s"),
      SlootID = str_replace(SlootID, pattern = "AF_DRGLG", replacement = "DRGLG_AF"),
      SlootID = str_replace(SlootID, pattern = "AF_NVO", replacement = "NVO_AF"),
      SlootID = str_replace(SlootID, pattern = "WP_1", replacement = "WP1"),
      # SlootID = case_when(
      #   SlootID == "SW_4_M_AF_NO" & Datemanual == "2024-06-24 22:00:00",
      #   TRUE ~ SlootID),
      ## SlootCode
      SlootCode = tolower(str_replace(SlootID, pattern = "^([^_]*_[^_]*)_.*$", replacement = "\\1")),
      SlootCode = str_replace(SlootCode, pattern = "_", replacement = ""),
      SlootCode = if_else(
        condition = str_detect(SlootID, pattern = "WP1"),
        true = paste0(SlootCode, "wp1"), false = SlootCode
      )
    )
}


slootid_clean_abio <- function(abio_df){
  abio_df |> 
    mutate(
      SlootID_old = SlootID,
      across(
        c(SlootID, Welke_oever), ~ str_to_upper(.)
      ),
      SlootID = str_replace_all(SlootID, pattern = "-", replacement = "_"),
      SlootID = str_replace(SlootID, pattern = "²", replacement = "2"),
      SlootID = case_when(
        str_sub(SlootID, 3,3) != "_" ~ paste0(str_sub(SlootID, 1, 2), "_", str_sub(SlootID, 3)),
        TRUE ~ SlootID),
      SlootID = case_when(
        str_sub(SlootID, -1) != Welke_oever ~ paste0(SlootID, "_", Welke_oever),
        TRUE ~ SlootID),
      SlootID = str_remove_all(SlootID, pattern = "\\s"),
      SlootID = str_replace(SlootID, pattern = "BRGLG", replacement = "DRGLG"),
      SlootID = str_replace(SlootID, pattern = "AF_DRGLG", replacement = "DRGLG_AF"),
      SlootID = str_replace(SlootID, pattern = "AF_NVO", replacement = "NVO_AF"),
      SlootID = str_replace(SlootID, pattern = "AF9", replacement = "AF"),
      SlootID = case_when(
        SlootID == "SW_M_NVO_Z" & Datemanual == "2024-07-02 22:00:00" ~ "SW_3_M_NVO_Z",
        SlootID == "SW_2_M_AF_N" & Datemanual == "2024-06-25 22:00:00" ~ "SW_3_M_AF_N",
        SlootID == "RH_8_M_AF_N" & Datemanual == "2024-05-16 22:00:00" ~ "RH_9_M_AF_N",
        TRUE ~ SlootID
      ),
      ## SlootCode
      SlootCode = tolower(str_replace(SlootID, pattern = "^([^_]*_[^_]*)_.*$", replacement = "\\1")),
      SlootCode = str_replace(SlootCode, pattern = "_", replacement = ""),
      SlootCode = if_else(
        condition = str_detect(SlootID, pattern = "WP1"),
        true = paste0(SlootCode, "wp1"), false = SlootCode
      )
    )
}
