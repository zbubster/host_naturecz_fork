# Stanoviste - validace vstupu orchestratoru
#
# stanoviste_validate_inputs()
#
# Funkce overuje PRED samotnym vypoctem:
#   - zakladni argumenty,
#   - strukturu listu data a tables,
#   - dostupnost potrebnych funkci,
#   - typy vstupnich objektu,
#   - pritomnost pozadovanych sloupcu,
#   - existenci site_code.
#
# Pri uspechu vraci invisible(TRUE).
# Pri chybe ukonci vypocet informativni chybovou hlaskou.

stanoviste_validate_inputs <- function(
    hab_code,
    site_code,
    data,
    tables
) {
  
  # ---------------------------------------------------------------------------
  # Pomocne validacni funkce
  # ---------------------------------------------------------------------------
  
  require_sf <- function(x, object_name) {
    
    if (!base::inherits(x, "sf")) {
      base::stop(
        "stanoviste_validate_inputs(): `",
        object_name,
        "` musi byt sf objekt.",
        call. = FALSE
      )
    }
    
    base::invisible(TRUE)
  }
  
  require_table <- function(x, object_name) {
    
    if (!base::is.data.frame(x)) {
      base::stop(
        "stanoviste_validate_inputs(): `",
        object_name,
        "` musi byt data.frame/tibble nebo sf.",
        call. = FALSE
      )
    }
    
    base::invisible(TRUE)
  }
  
  require_cols <- function(x, cols, object_name) {
    
    missing_cols <- base::setdiff(
      cols,
      base::names(x)
    )
    
    if (base::length(missing_cols) > 0) {
      base::stop(
        "stanoviste_validate_inputs(): v `",
        object_name,
        "` chybi sloupce: ",
        base::paste(missing_cols, collapse = ", "),
        call. = FALSE
      )
    }
    
    base::invisible(TRUE)
  }
  
  # ---------------------------------------------------------------------------
  # 1. Zakladni argumenty
  # ---------------------------------------------------------------------------
  
  if (
    base::length(hab_code) != 1 ||
    base::is.na(hab_code) ||
    !base::nzchar(base::as.character(hab_code))
  ) {
    base::stop(
      "stanoviste_validate_inputs(): `hab_code` musi byt jedna neprazdna hodnota.",
      call. = FALSE
    )
  }
  
  if (
    base::length(site_code) != 1 ||
    base::is.na(site_code) ||
    !base::nzchar(base::as.character(site_code))
  ) {
    base::stop(
      "stanoviste_validate_inputs(): `site_code` musi byt jedna neprazdna hodnota.",
      call. = FALSE
    )
  }
  
  hab_code <- base::as.character(hab_code)
  site_code <- base::as.character(site_code)
  
  if (!base::is.list(data)) {
    base::stop(
      "stanoviste_validate_inputs(): `data` musi byt pojmenovany list.",
      call. = FALSE
    )
  }
  
  if (!base::is.list(tables)) {
    base::stop(
      "stanoviste_validate_inputs(): `tables` musi byt pojmenovany list.",
      call. = FALSE
    )
  }
  
  if (base::is.null(base::names(data))) {
    base::stop(
      "stanoviste_validate_inputs(): `data` musi byt pojmenovany list.",
      call. = FALSE
    )
  }
  
  if (base::is.null(base::names(tables))) {
    base::stop(
      "stanoviste_validate_inputs(): `tables` musi byt pojmenovany list.",
      call. = FALSE
    )
  }
  
  # ---------------------------------------------------------------------------
  # 2. Dostupnost potrebnych funkci
  # ---------------------------------------------------------------------------
  
  required_functions <- base::c(
    "paseky_select_pairs",
    "paseky_spat",
    "paseky_sum",
    "stanoviste_paseky",
    "stanoviste_klic",
    "stanoviste_druhy",
    "stanoviste_prostor"
  )
  
  missing_functions <- required_functions[
    !base::vapply(
      required_functions,
      FUN = function(x) {
        base::exists(
          x,
          mode = "function",
          inherits = TRUE
        )
      },
      FUN.VALUE = base::logical(1)
    )
  ]
  
  if (base::length(missing_functions) > 0) {
    base::stop(
      "stanoviste_validate_inputs(): nejsou nacteny potrebne funkce: ",
      base::paste(missing_functions, collapse = ", "),
      call. = FALSE
    )
  }
  
  # ---------------------------------------------------------------------------
  # 3. Struktura data a tables
  # ---------------------------------------------------------------------------
  
  required_data <- base::c(
    "site",
    "vmb",
    "czechia_line",
    "vmb1_base",
    "vmb2_base",
    "vmb2_update",
    "vmb0_update"
  )
  
  required_tables <- base::c(
    "habitat_areas",
    "minimisize",
    "red_list_species",
    "invasive_species",
    "expansive_species",
    "vmb1_meta",
    "vmb2_meta",
    "vmb0_meta"
  )
  
  missing_data <- base::setdiff(
    required_data,
    base::names(data)
  )
  
  missing_tables <- base::setdiff(
    required_tables,
    base::names(tables)
  )
  
  if (base::length(missing_data) > 0) {
    base::stop(
      "stanoviste_validate_inputs(): v `data` chybi: ",
      base::paste(missing_data, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (base::length(missing_tables) > 0) {
    base::stop(
      "stanoviste_validate_inputs(): v `tables` chybi: ",
      base::paste(missing_tables, collapse = ", "),
      call. = FALSE
    )
  }
  
  null_data <- required_data[
    base::vapply(
      data[required_data],
      base::is.null,
      FUN.VALUE = base::logical(1)
    )
  ]
  
  null_tables <- required_tables[
    base::vapply(
      tables[required_tables],
      base::is.null,
      FUN.VALUE = base::logical(1)
    )
  ]
  
  if (base::length(null_data) > 0) {
    base::stop(
      "stanoviste_validate_inputs(): tyto polozky v `data` jsou NULL: ",
      base::paste(null_data, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (base::length(null_tables) > 0) {
    base::stop(
      "stanoviste_validate_inputs(): tyto polozky v `tables` jsou NULL: ",
      base::paste(null_tables, collapse = ", "),
      call. = FALSE
    )
  }
  
  # ---------------------------------------------------------------------------
  # 4. Prostorove vstupy
  # ---------------------------------------------------------------------------
  
  base::lapply(
    base::c(
      "site",
      "vmb",
      "vmb1_base",
      "vmb2_base",
      "vmb2_update",
      "vmb0_update"
    ),
    FUN = function(x) {
      require_sf(
        data[[x]],
        base::paste0("data$", x)
      )
    }
  )
  
  if (
    !base::inherits(data$czechia_line, "sf") &&
    !base::inherits(data$czechia_line, "sfc")
  ) {
    base::stop(
      "stanoviste_validate_inputs(): `data$czechia_line` musi byt sf nebo sfc objekt.",
      call. = FALSE
    )
  }
  
  # ---------------------------------------------------------------------------
  # 5. Site
  # ---------------------------------------------------------------------------
  
  require_cols(
    data$site,
    base::c(
      "SITECODE",
      "NAZEV",
      "SHAPE_AREA"
    ),
    "data$site"
  )
  
  if (!site_code %in% base::as.character(data$site$SITECODE)) {
    base::stop(
      "stanoviste_validate_inputs(): site `",
      site_code,
      "` nebyla nalezena v `data$site`.",
      call. = FALSE
    )
  }
  
  # ---------------------------------------------------------------------------
  # 6. Aktualni VMB - spolecne potreby KLIC / DRUHY / PROSTOR
  # ---------------------------------------------------------------------------
  
  require_cols(
    data$vmb,
    base::c(
      "OBJECTID",
      "HABITAT",
      "BIOTOP",
      "STEJ_PR",
      "DG",
      "RB",
      "TD",
      "SF",
      "KVALITA",
      "MD",
      "DATUM",
      "FSB_EVAL",
      "BIOTOP_SEZ"
    ),
    "data$vmb"
  )
  
  # ---------------------------------------------------------------------------
  # 7. Pasekove prostorove vrstvy
  # ---------------------------------------------------------------------------
  
  require_cols(
    data$vmb1_base,
    base::c(
      "HABITAT",
      "BIOTOP",
      "STEJ_PR",
      "SEGMENT_ID",
      "DATUM"
    ),
    "data$vmb1_base"
  )
  
  require_cols(
    data$vmb2_base,
    base::c(
      "HABITAT",
      "BIOTOP",
      "STEJ_PR",
      "SEGMENT_ID",
      "DATUM"
    ),
    "data$vmb2_base"
  )
  
  require_cols(
    data$vmb2_update,
    base::c(
      "BIOTOP",
      "STEJ_PR",
      "SEGMENT_ID",
      "REGION_ID",
      "DATUM",
      "ROK_AKT"
    ),
    "data$vmb2_update"
  )
  
  require_cols(
    data$vmb0_update,
    base::c(
      "BIOTOP",
      "STEJ_PR",
      "SEGMENT_ID",
      "REGION_ID",
      "DATUM",
      "ROK_AKT"
    ),
    "data$vmb0_update"
  )
  
  # ---------------------------------------------------------------------------
  # 8. Lookup tabulky
  # ---------------------------------------------------------------------------
  
  require_table(
    tables$habitat_areas,
    "tables$habitat_areas"
  )
  
  require_cols(
    tables$habitat_areas,
    base::c(
      "HABITAT",
      "TOTAL_AREA_ALL"
    ),
    "tables$habitat_areas"
  )
  
  require_table(
    tables$minimisize,
    "tables$minimisize"
  )
  
  require_cols(
    tables$minimisize,
    base::c(
      "HABITAT",
      "MINIMISIZE"
    ),
    "tables$minimisize"
  )
  
  # ---------------------------------------------------------------------------
  # 9. Druhova prostorova data
  # ---------------------------------------------------------------------------
  
  require_sf(
    tables$red_list_species,
    "tables$red_list_species"
  )
  
  require_sf(
    tables$invasive_species,
    "tables$invasive_species"
  )
  
  require_sf(
    tables$expansive_species,
    "tables$expansive_species"
  )
  
  require_cols(
    tables$red_list_species,
    "DRUH",
    "tables$red_list_species"
  )
  
  require_cols(
    tables$invasive_species,
    base::c(
      "DRUH",
      "DATUM_OD",
      "NEGATIVNI"
    ),
    "tables$invasive_species"
  )
  
  require_cols(
    tables$expansive_species,
    base::c(
      "DRUH",
      "DATUM_OD",
      "NEGATIVNI",
      "POKRYVN"
    ),
    "tables$expansive_species"
  )
  
  # ---------------------------------------------------------------------------
  # 10. Metadata VMB
  # ---------------------------------------------------------------------------
  
  base::lapply(
    base::c(
      "vmb1_meta",
      "vmb2_meta",
      "vmb0_meta"
    ),
    FUN = function(x) {
      
      require_table(
        tables[[x]],
        base::paste0("tables$", x)
      )
      
      require_cols(
        tables[[x]],
        base::c(
          "REGION_ID",
          "DATUM"
        ),
        base::paste0("tables$", x)
      )
    }
  )
  
  base::invisible(TRUE)
}
