#----------------------------------------------------------#
# Nacteni VMB ----
#----------------------------------------------------------#

data_orig <- load_vmb(vmb_x = 1)
data_akt1 <- load_vmb(vmb_x = 2)
data_akt <- load_vmb(vmb_x = 0)

vmb_shp_sjtsk_orig <- data_orig$vmb_shp_sjtsk_orig
vmb_shp_sjtsk_a1 <- data_akt1$vmb_shp_sjtsk_a1
vmb_pb_x_akt <- data_akt$vmb_pb_x_akt
vmb_pb_x_a1 <- data_akt1$vmb_pb_x_a1

evl_codes <- unique(evl$SITECODE)
bio_codes <- unique(vmb_pb_x_akt$BIOTOP)

evl_site <- evl[evl$NAZEV == "Krkonoše",]$SITECODE
hab_code <- "L1"

#----------------------------------------------------------#
# Prostorova funkce pro vypocet pasek ----
#----------------------------------------------------------#
paseky_spat <- function(
    hab_code, 
    evl_site, 
    zakl = "VMB1", 
    aktu = "VMB0", 
    typ_chu
) {
  
  #--------------------------------------------------#
  ## Načtení podkladových dat (na základě argumentů) ----
  #--------------------------------------------------#
  if(typ_chu == "EVL") {
    uzemi <- evl
  } else if(typ_chu == "MZCHU") {
    uzemi <- mzchu
  } else if(typ_chu == "OKRSEK") {
    uzemi <- akt_okrsek
  }
  
  if(aktu == "VMB0") {
    vmb_aktu <- vmb_pb_x_akt
  } else if(aktu == "VMB2") {
    vmb_aktu <- vmb_pb_x_a1
  }
  
  if(zakl == "VMB1") {
    vmb_zakl <- vmb_shp_sjtsk_orig
  } else if(zakl == "VMB2") {
    vmb_zakl <- vmb_shp_sjtsk_a1
  }
  
  # Inicializace proměnné result pro případ, že se nespustí hlavní blok
  result <- NULL
  
  # if(substr(hab_code, 1, 1) != 9 | substr(hab_code, 1, 1) != "L") {
  #   message(hab_code, "-skip")
  #   return()
  # }
  
  if(substr(hab_code, 1, 1) == 9 | substr(hab_code, 1, 1) == "L") {
    
    # Zjistíme CRS referenčního území
    target_crs <- sf::st_crs(uzemi)
    
    # Kontrola a transformace vmb_aktu, pokud nesedí CRS
    if (sf::st_crs(vmb_aktu) != target_crs) {
      # Volitelně: vypíše hlášku, pokud dochází k transformaci
      message("Transformuji vmb_aktu na shodný CRS...") 
      vmb_aktu <- sf::st_transform(vmb_aktu, target_crs)
    }
    
    # Kontrola a transformace vmb_zakl, pokud nesedí CRS
    if (sf::st_crs(vmb_zakl) != target_crs) {
      message("Transformuji vmb_zakl na shodný CRS...")
      vmb_zakl <- sf::st_transform(vmb_zakl, target_crs)
    }
    
    # Filtrace území pro konkrétní site (zrychlí následný intersection)
    uzemi_filter <- dplyr::filter(uzemi, SITECODE == evl_site)
    
    #--------------------------------------------------#
    ## Výpočet pro aktuální VMB ----
    #--------------------------------------------------#
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
    
    #--------------------------------------------------#
    ## Výpočet pro základní VMB ----
    #--------------------------------------------------#
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
    
    #--------------------------------------------------#
    ## Finální průnik ----
    #--------------------------------------------------#
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
    
  } 
  
  # Definice cesty a názvu souboru
  file_path <- paste0("Outputs/Data/stanoviste/paseky/", typ_chu, "_", evl_site, "_", hab_code, "_", zakl, "_", aktu, ".gpkg")
  
  # Aplikace clean_names na celý objekt (pokud existuje)
  if (!is.null(result) && nrow(result) > 0) {
    result <- janitor::clean_names(result)
  }
  print(file_path)
  #--------------------------------------------------#
  ## Zápis do GeoPackage ----
  #--------------------------------------------------#
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
#----------------------------------------------------------#
# Vypocet GIS vrstvy ----
#----------------------------------------------------------#

# Loop s "odchytáváním" zpráv
for(i in seq_along(evl_codes)) {
  for(j in seq_along(bio_codes)){
  x <- bio_codes[j]
  paseky_spat(typ_chu = "EVL",
              zakl = "VMB1",
              aktu = "VMB0",
              evl_site = evl_codes[i],
              hab_code = x)
  }
}
