packages <- c(
  "tidyverse", 
  "sf", 
  "sp", 
  "proj4", 
  "openxlsx",
  "fuzzyjoin", 
  "remotes",
  "ggplot2",
  "progress",
  "fs"
)

# Standardni package
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

vmb_shp_sjtsk_orig <- sf::st_read("../host_data/vmb_shp_sjtsk_orig.gpkg")
vmb_pb_x_akt <- sf::st_read("../host_data/vmb_pb_x_akt.gpkg")
vmb_shp_sjtsk_a1 <- sf::st_read("../host_data/vmb_shp_sjtsk_a1.gpkg")
vmb_pb_x_a1 <- sf::st_read("../host_data/vmb_pb_x_a1.gpkg")

n2k_oop <- readr::read_csv2(
  "Data/Input/n2k_oop_25.csv", 
  locale = readr::locale(encoding = "Windows-1250")
) %>%
  mutate(oop = gsub(";", ",", oop)) %>%
  dplyr::rename(SITECODE = sitecode) %>%
  dplyr::select(SITECODE, oop)

endpoint <- "http://gis.nature.cz/arcgis/services/Aplikace/Opendata/MapServer/WFSServer?"
caps_url <- paste0(endpoint, "request=GetCapabilities&service=WFS")

layer_name_evl <- "Opendata:Evropsky_vyznamne_lokality"
getfeature_url_evl <- paste0(
  endpoint,
  "service=WFS&version=2.0.0&request=GetFeature&typeName=", layer_name_evl
)

read_layer <- function(local_path, wfs_url, n2k = NULL) {
  if (file.exists(local_path)) {
    message("Reading local file: ", local_path)
    shp <- sf::st_read(local_path, options = "ENCODING=CP1250", quiet = TRUE)
  } else {
    message("Local file not found, downloading from WFS: ", wfs_url)
    shp <- sf::st_read(wfs_url, quiet = TRUE)
  }
  
  shp <- sf::st_transform(
    shp, 
    st_crs("+init=epsg:5514")
  )
  
  if (!is.null(n2k) & local_path != "Data/Input/MaloplZCHU.shp") {
    shp <- dplyr::left_join(shp, n2k, by = "SITECODE")
  }
  
  return(shp)
}

evl <- read_layer("Data/Input/EvVyzLok.shp", getfeature_url_evl, n2k = n2k_oop)

#########################################

# evl_site <- "CZ0210708"
# biotop <- "L5.4"
# evl_codes <- "CZ0214002"
# evl_codes <- "CZ0210107"


# VMB 2, aktu
vmb_aktu <- vmb_pb_x_a1
# VMB 0, aktu
vmb_aktu <- vmb_pb_x_akt
# VMB 1, zakl
vmb_zakl <- vmb_shp_sjtsk_orig
# VMB 2, zakl
vmb_zakl <- vmb_shp_sjtsk_a1

zakl <- "VMB2"
zakl <- "VMB1"
aktu <- "VMB0"
aktu <- "VMB2"

typ_chu <- "EVL"

uzemi <- evl

out_dir <- "Outputs/Data/stanoviste/paseky/VMB1_VMB0/"
# out_dir <- "Outputs/Data/stanoviste/paseky/VMB2_VMB0/"
# out_dir <- "Outputs/Data/stanoviste/paseky/VMB1_VMB0/"
if(!dir_exists(out_dir)) dir_create(out_dir)

#########################################

evl_codes <- unique(evl$SITECODE)
bio_codes_zaḱl <- unique(vmb_zakl$BIOTOP)
bio_codes_aktu <- unique(vmb_aktu$BIOTOP)
bio_codes <- unique(c(bio_codes_zaḱl, bio_codes_aktu))
bio_codes <- bio_codes[substr(bio_codes, 1, 1) == "L"]
bio_codes


bio_codes_aktu
bio_codes_zaḱl %in% bio_codes_aktu

#########################################

log_file <- paste0(out_dir, "__log_paseky_spat.txt")

dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)

log_msg <- function(..., file = log_file) {
  txt <- base::paste0(...)
  line <- base::sprintf(
    "[%s] %s",
    base::format(base::Sys.time(), "%Y-%m-%d %H:%M:%S"),
    txt
  )
  
  base::message(line)
  base::cat(line, "\n", file = file, append = TRUE)
}

#########################################

# split uzemi podle SITECODE, at se stale nefiltruje celé sf
uzemi_split <- base::split(uzemi, uzemi$SITECODE)
names(uzemi_split)

# vmb_zakl nechat jen lesni biotopy
vmb_zakl_rel <- 
  vmb_zakl %>%
  dplyr::filter(HABITAT %in% bio_codes | BIOTOP %in% bio_codes)

# LOOP
for (evl_site in evl_codes) {
  
  log_msg("UZEMI: ", evl_site)
  
  # uzemi filter je jenom jeden prvek z celeho sf
  uzemi_filter <- uzemi_split[[evl_site]]
  
  ##################################################################################
  
  # nejdřív spatial prefilter, nechat jen ty prvky z vmb_aktu, ktere intersects s danym evl
  vmb_update_filtered <- 
    sf::st_filter(
      vmb_aktu,
      uzemi_filter,
      .predicate = sf::st_intersects
    )
  
  # pokud se EVL neprekryva s vmb_aktu
  if (nrow(vmb_update_filtered) == 0) {
    log_msg("Pro ", evl_site, " není v novějším mapování žádný segment.")
    next
  }
  
  # intersection vmb_aktu X evl
  vmb_aktu_evl <- 
    sf::st_intersection(vmb_update_filtered, uzemi_filter) %>%
    dplyr::filter(
      base::as.character(sf::st_geometry_type(.)) %in% base::c("POLYGON", "MULTIPOLYGON")
    ) %>%
    dplyr::mutate(
      AREA_real_update = units::drop_units(sf::st_area(.)),
      PLO_BIO_M2_EVL_update = STEJ_PR / 100 * AREA_real_update
    ) %>%
    dplyr::rename(
      FSB_update = FSB,
      BIOTOP_update = BIOTOP,
      STEJ_PR_update = STEJ_PR,
      ROK_AKT_update = ROK_AKT.x
    )
  
  # safe check
  if (nrow(vmb_aktu_evl) == 0) {
    log_msg("FAIL: Pro ", evl_site, " jsou ve vmb_aktu segmenty, ale po průniku nevznikla žádná geometrie.")
    next
  }
  
  ##################################################################################
  # 
  
  # vmb_zakl ale pouze lesni biotopy
  vmb_orig_filtered <- 
    sf::st_filter(
      vmb_zakl_rel, # resi se pouze lesni biotopy v puvodnim mapovani, pokud nejsou, skip evl
      uzemi_filter,
      .predicate = sf::st_intersects
    )
  
  # pokud v evl nebyly puvodne zadne lesni biotopy
  if (nrow(vmb_orig_filtered) == 0) {
    log_msg("Pro ", evl_site, " není ve starším mapování žádný LESNÍ segment.")
    next
  }
  
  # pokud byly puvodne vymapovany lesni biotopy
  vmb_zakl_evl <- 
    sf::st_intersection(vmb_orig_filtered, uzemi_filter) %>%
    dplyr::filter(
      base::as.character(sf::st_geometry_type(.)) %in% base::c("POLYGON", "MULTIPOLYGON")
    ) %>%
    dplyr::mutate(
      AREA_real_orig = units::drop_units(sf::st_area(.)),
      PLO_BIO_M2_EVL_orig = STEJ_PR / 100 * AREA_real_orig
    ) %>%
    dplyr::rename(
      FSB_orig = FSB,
      BIOTOP_orig = BIOTOP,
      STEJ_PR_orig = STEJ_PR
    )
  
  # safe check
  if (nrow(vmb_zakl_evl) == 0) {
    log_msg("FAIL: Pro ", evl_site, " jsou ve vmb_zakl segmenty, ale po průniku nevznikla žádná geometrie.")
    next
  }
  
  ##################################################################################
  
  # HABITAT loop
  # ted uz se pocita jenom pro relevantni biotopy v danem evl
  # sem se kod vubec nedostane, pokud nebyly puvodne v evl vymapovany L
  for (biotop in bio_codes) {
    
    #log_msg(evl_site, ":", biotop)
    
    # vem vmb_zakl s danym lesnim biotopem
    vmb_zakl_evl_lesni_biotop <- 
      vmb_zakl_evl %>%
      dplyr::filter(HABITAT == biotop | BIOTOP_orig == biotop)
    
    # je tam?
    if (nrow(vmb_zakl_evl_lesni_biotop) == 0) {
      log_msg("Pro ", evl_site, " není ", biotop, " ve starším mapování.")
      next
    }
    
    # pokud je biotop v danem evl ve starsim mapovani, ktere segmenty jsou jim dotceny (co jsou ty puvodni, co jsou ty nove)
    TARGET <- sf::st_intersects(
      vmb_aktu_evl,
      vmb_zakl_evl_lesni_biotop
    )
    
    # mame nova data pro stary biotop?
    if (!base::any(base::lengths(TARGET) > 0)) {
      log_msg("Pro ", evl_site, "je biotop ", biotop, " ve starem mapovani, ale neprekryva se s zadnym segmentem noveho mapovani.")
      next
    }
    
    # subset data na pouze relevantni segmenty
    update_sub <- vmb_aktu_evl[base::lengths(TARGET) > 0, ]
    orig_sub <- vmb_zakl_evl_lesni_biotop[
      base::sort(base::unique(base::unlist(TARGET))),
    ]
    
    # vypocitat result
    result <- 
      sf::st_intersection(update_sub, orig_sub) %>%
      dplyr::filter(
        base::as.character(sf::st_geometry_type(.)) %in% base::c("POLYGON", "MULTIPOLYGON")
      ) %>%
      dplyr::mutate(
        PASEKA = dplyr::case_when(
          BIOTOP_update %in% base::c("LP", "X10") ~ 1,
          BIOTOP_update %in% base::c("X11", "X12A", "X12B") &
            ROK_AKT_update %in% 2007:2012 ~ 1,
          TRUE ~ 0
        ),
        AREA_real_intersection = units::drop_units(sf::st_area(.)),
        PLO_BIO_M2_EVL_intersection =
          AREA_real_intersection * STEJ_PR_orig / 100 * STEJ_PR_update / 100,
        HOLINA = dplyr::case_when(
          PASEKA == 1 & PLO_BIO_M2_EVL_intersection > 10000 ~ 1,
          TRUE ~ 0
        )
      ) %>%
      dplyr::select(-dplyr::any_of("SHAPE_Area") # dvakrat SHAPE_Area dela problem
      ) %>%
      dplyr::select(-dplyr::any_of("SHAPE_AREA") # dvakrat SHAPE_Area dela problem
      ) %>%
      dplyr::select(-dplyr::any_of("Shape_Area") # dvakrat SHAPE_Area dela problem
      ) %>%
      dplyr::select(-dplyr::any_of("SHAPE_Leng"))
    
    
    # safe check
    if (nrow(result) == 0) {
      log_msg("Pro ", evl_site, " a ", biotop, " nevznikl žádný průnik.")
      next
    }
    
    # zapis
    file_path <- paste0(out_dir, typ_chu, "_", evl_site, "_", biotop, "_", zakl, "_", aktu, ".gpkg")
    
    sf::st_write(
      obj = result,
      dsn = file_path,
      layer = paste0(typ_chu, "_", evl_site, "_", biotop, "_", zakl, "_", aktu),
      driver = "GPKG",
      quiet = FALSE,
      delete_dsn = TRUE
    )
    
    log_msg("Pro ", evl_site, " a ", biotop, " vrstva zapsána.")
  }
}

radky <- readLines(log_file, encoding = "UTF-8", warn = FALSE)
uzemi_radky <- grep("UZEMI:", radky, value = TRUE)
kody <- regmatches(uzemi_radky, gregexpr("CZ[0-9]{7}", uzemi_radky))
all(unique(kody) == unique(evl$SITECODE))
