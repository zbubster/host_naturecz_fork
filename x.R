
data_orig <- load_vmb(vmb_x = 1)
data_akt1 <- load_vmb(vmb_x = 2)
data_akt <- load_vmb(vmb_x = 0)

vmb_shp_sjtsk_orig <- data_orig$vmb_shp_sjtsk_orig
vmb_shp_sjtsk_a1 <- data_akt1$vmb_shp_sjtsk_a1
vmb_pb_x_akt <- data_akt$vmb_pb_x_akt
vmb_pb_x_a1 <- data_akt1$vmb_pb_x_a1

evl_codes <- unique(evl$SITECODE)
bio_codes <- unique(vmb_pb_x_akt$BIOTOP)
bio_codes <- bio_codes[substr(bio_codes, 1, 1) == "L"]

#########################################

vmb_aktu <- vmb_pb_x_akt
vmb_zakl <- vmb_shp_sjtsk_orig

zakl <- "VMB1"
aktu <- "VMB0"
typ_chu <- "EVL"

uzemi <- evl

#########################################

for(i in seq_along(evl_codes)){
  
  evl_site <- evl_codes[i]
  
  # get filter
  uzemi_filter <- dplyr::filter(uzemi, SITECODE == evl_site)
  
  # compute VMB intersection for given EVL
  vmb_target_sjtsk_update <- 
    sf::st_intersection(
      vmb_aktu, 
      uzemi_filter
    ) %>%
    dplyr::mutate(
      AREA_real_update = units::drop_units(sf::st_area(geometry))
    ) %>%
    dplyr::mutate(
      PLO_BIO_M2_EVL_update = STEJ_PR/100 * AREA_real_update
    ) %>%
    dplyr::rename(
      FSB_update = FSB,
      BIOTOP_update = BIOTOP,
      STEJ_PR_update = STEJ_PR,
      ROK_AKT_update = ROK_AKT.x
    )
  
  for(j in seq_along(bio_codes)){
    
    hab_code <- bio_codes[j]
    
    message(hab_code)
    
    vmb_target_sjtsk_orig <- 
      vmb_zakl %>%
      sf::st_intersection(., uzemi_filter) %>%
      dplyr::filter(HABITAT == hab_code | BIOTOP == hab_code) %>%
      dplyr::mutate(
        AREA_real_orig = units::drop_units(sf::st_area(geometry))
      ) %>%
      dplyr::mutate(
        PLO_BIO_M2_EVL_orig = STEJ_PR/100 * AREA_real_orig
      ) %>%
      dplyr::rename(
        FSB_orig = FSB,
        BIOTOP_orig = BIOTOP,
        STEJ_PR_orig = STEJ_PR
      )
    
    result <- 
      sf::st_intersection(
        vmb_target_sjtsk_update, 
        vmb_target_sjtsk_orig
      ) %>%
      dplyr::mutate(
        PASEKA = dplyr::case_when(
          BIOTOP_update %in% c("LP", "X10") ~ 1,
          BIOTOP_update %in% c("X11", "X12A", "X12B") & ROK_AKT_update %in% c(2007:2012) ~ 1,
          TRUE ~ 0
        )
      ) %>%
      dplyr::mutate(
        AREA_real_intersection = units::drop_units(sf::st_area(geometry))
      ) %>%
      dplyr::mutate(
        PLO_BIO_M2_EVL_intersection = AREA_real_intersection * STEJ_PR_orig/100 * STEJ_PR_update/100
      ) %>%
      dplyr::mutate(
        HOLINA = dplyr::case_when(
          PASEKA == 1 & PLO_BIO_M2_EVL_intersection > 10000 ~ 1,
          TRUE ~ 0
        )
      )
    
    file_path <- paste0("Outputs/Data/stanoviste/paseky/", typ_chu, "_", evl_site, "_", hab_code, "_", zakl, "_", aktu, ".gpkg")
    
    if (!is.null(result) && nrow(result) > 0) {
      # 2. Samotný zápis
      sf::st_write(
        obj = result, 
        dsn = file_path, 
        layer = paste0(typ_chu, "_", evl_site, "_", hab_code, "_", zakl, "_", aktu), 
        driver = "GPKG",
        quiet = FALSE
        # delete_dsn už není potřeba, smazali jsme ho ručně o krok výše
      )
      
      message(paste("Pro", evl_site, "a", hab_code, "vrstva zapsána."))
      
    } else {
      message(paste("Pro", evl_site, "a", hab_code, "nevznikl žádný průnik."))
    }
  }
}


