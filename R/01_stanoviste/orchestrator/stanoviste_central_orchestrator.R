# Stanoviste - centralni orchestrator
#
# stanoviste_eval()
#
# Uloha:
#   - zavolat validaci vstupu,
#   - spocitat paseky,
#   - spustit klicove, druhove a prostorove parametry,
#   - overit konzistenci dilcich vystupu,
#   - spojit je do jednoho radku.
#
# Pred zavolanim musi byt nactena:
#   stanoviste_validate_inputs()
# a vsechny vypocetni funkce.

stanoviste_eval <- function(
    hab_code,
    site_code,
    data,
    tables,
    return_components = FALSE
) {
  
  # ---------------------------------------------------------------------------
  # 1. Validace vstupu
  # ---------------------------------------------------------------------------
  
  if (
    !base::exists(
      "stanoviste_validate_inputs",
      mode = "function",
      inherits = TRUE
    )
  ) {
    base::stop(
      "stanoviste_eval(): neni nactena funkce `stanoviste_validate_inputs()`.",
      call. = FALSE
    )
  }
  
  stanoviste_validate_inputs(
    hab_code = hab_code,
    site_code = site_code,
    data = data,
    tables = tables
  )
  
  hab_code <- base::as.character(hab_code)
  site_code <- base::as.character(site_code)
  
  # ---------------------------------------------------------------------------
  # 2. Pomocne funkce pro beh komponent
  # ---------------------------------------------------------------------------
  
  run_component <- function(component_name, expr) {
    
    base::tryCatch(
      expr,
      error = function(e) {
        base::stop(
          "stanoviste_eval(): chyba v `",
          component_name,
          "` pro ",
          site_code,
          " x ",
          hab_code,
          ": ",
          base::conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }
  
  check_one_row <- function(x, component_name) {
    
    if (!base::is.data.frame(x)) {
      base::stop(
        "stanoviste_eval(): `",
        component_name,
        "` nevratil tabulkovy objekt.",
        call. = FALSE
      )
    }
    
    if (base::nrow(x) != 1) {
      base::stop(
        "stanoviste_eval(): `",
        component_name,
        "` musi vratit prave 1 radek, ale vratil ",
        base::nrow(x),
        ".",
        call. = FALSE
      )
    }
    
    required_cols <- base::c(
      "SITECODE",
      "HABITAT_CODE"
    )
    
    missing_cols <- base::setdiff(
      required_cols,
      base::names(x)
    )
    
    if (base::length(missing_cols) > 0) {
      base::stop(
        "stanoviste_eval(): v `",
        component_name,
        "` chybi sloupce: ",
        base::paste(missing_cols, collapse = ", "),
        call. = FALSE
      )
    }
    
    if (
      !base::identical(
        base::as.character(x$SITECODE[[1]]),
        site_code
      )
    ) {
      base::stop(
        "stanoviste_eval(): `",
        component_name,
        "` vratil jiny SITECODE nez pozadovany.",
        call. = FALSE
      )
    }
    
    if (
      !base::identical(
        base::as.character(x$HABITAT_CODE[[1]]),
        hab_code
      )
    ) {
      base::stop(
        "stanoviste_eval(): `",
        component_name,
        "` vratil jiny HABITAT_CODE nez pozadovany.",
        call. = FALSE
      )
    }
    
    base::invisible(TRUE)
  }
  
  # ---------------------------------------------------------------------------
  # 3. Paseky
  # ---------------------------------------------------------------------------
  
  paseky <- run_component(
    "stanoviste_paseky",
    stanoviste_paseky(
      hab_code = hab_code,
      site_code = site_code,
      site = data$site,
      vmb1_base = data$vmb1_base,
      vmb2_base = data$vmb2_base,
      vmb2_update = data$vmb2_update,
      vmb0_update = data$vmb0_update,
      vmb1_meta = tables$vmb1_meta,
      vmb2_meta = tables$vmb2_meta,
      vmb0_meta = tables$vmb0_meta
    )
  )
  
  check_one_row(
    paseky,
    "stanoviste_paseky"
  )
  
  # ---------------------------------------------------------------------------
  # 4. Klicove parametry - reprezentativni zaklad vysledku
  # ---------------------------------------------------------------------------
  
  klic <- run_component(
    "stanoviste_klic",
    stanoviste_klic(
      hab_code = hab_code,
      site_code = site_code,
      vmb = data$vmb,
      site = data$site,
      paseky = paseky,
      habitat_areas = tables$habitat_areas
    )
  )
  
  check_one_row(
    klic,
    "stanoviste_klic"
  )
  
  # ---------------------------------------------------------------------------
  # 5. Druhove parametry
  # ---------------------------------------------------------------------------
  
  druhy <- run_component(
    "stanoviste_druhy",
    stanoviste_druhy(
      hab_code = hab_code,
      site_code = site_code,
      vmb = data$vmb,
      site = data$site,
      paseky = paseky,
      red_list_species = tables$red_list_species,
      invasive_species = tables$invasive_species,
      expansive_species = tables$expansive_species
    )
  )
  
  check_one_row(
    druhy,
    "stanoviste_druhy"
  )
  
  # ---------------------------------------------------------------------------
  # 6. Prostorove parametry
  # ---------------------------------------------------------------------------
  
  prostor <- run_component(
    "stanoviste_prostor",
    stanoviste_prostor(
      hab_code = hab_code,
      site_code = site_code,
      vmb = data$vmb,
      site = data$site,
      paseky = paseky,
      minimisize = tables$minimisize,
      czechia_line = data$czechia_line
    )
  )
  
  check_one_row(
    prostor,
    "stanoviste_prostor"
  )
  
  # ---------------------------------------------------------------------------
  # 7. Kontrola vzajemne konzistence dilcich vypoctu
  # ---------------------------------------------------------------------------
  
  component_areas <- base::c(
    KLIC = base::as.numeric(klic$ROZLOHA[[1]]),
    DRUHY = base::as.numeric(druhy$ROZLOHA[[1]]),
    PROSTOR = base::as.numeric(prostor$ROZLOHA[[1]])
  )
  
  area_values <- component_areas[
    !base::is.na(component_areas) &
      base::is.finite(component_areas)
  ]
  
  if (
    base::length(area_values) > 1 &&
    base::diff(base::range(area_values)) > 1e-6
  ) {
    base::stop(
      "stanoviste_eval(): dilci funkce vratily rozdilnou `ROZLOHA`: ",
      base::paste(
        base::names(component_areas),
        component_areas,
        sep = "=",
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  if (
    "PASEKY_AREA_HA" %in% base::names(klic) &&
    "ROZLOHA_PASEKY" %in% base::names(paseky)
  ) {
    
    klic_paseky <- base::as.numeric(
      klic$PASEKY_AREA_HA[[1]]
    )
    
    paseky_area <- base::as.numeric(
      paseky$ROZLOHA_PASEKY[[1]]
    )
    
    if (
      !base::is.na(klic_paseky) &&
      !base::is.na(paseky_area) &&
      base::abs(klic_paseky - paseky_area) > 1e-8
    ) {
      base::stop(
        "stanoviste_eval(): `PASEKY_AREA_HA` z KLIC se neshoduje ",
        "s `ROZLOHA_PASEKY` z pasek.",
        call. = FALSE
      )
    }
  }
  
  # ---------------------------------------------------------------------------
  # 8. Spojeni vysledku
  # ---------------------------------------------------------------------------
  
  druhy_join <- druhy |>
    dplyr::select(
      SITECODE,
      HABITAT_CODE,
      RED_LIST,
      INVASIVE,
      EXPANSIVE,
      RED_LIST_SPECIES,
      INVASIVE_LIST,
      EXPANSIVE_LIST
    )
  
  prostor_join <- prostor |>
    dplyr::select(
      SITECODE,
      HABITAT_CODE,
      MINIMIAREAL,
      MINIMIAREAL_JADRA,
      MINIMIAREAL_HODNOTA,
      MOZAIKA_VNEJSI,
      MOZAIKA_VNITRNI,
      MOZAIKA_FIN,
      VYPLNENOST_MOZAIKA,
      PERC_SPAT
    )
  
  paseky_join <- paseky |>
    dplyr::select(
      SITECODE,
      HABITAT_CODE,
      ROZLOHA_HOLINY,
      POCET_SEGMENTU_PASEKY
    )
  
  result <- klic |>
    dplyr::left_join(
      druhy_join,
      by = base::c(
        "SITECODE",
        "HABITAT_CODE"
      )
    ) |>
    dplyr::left_join(
      prostor_join,
      by = base::c(
        "SITECODE",
        "HABITAT_CODE"
      )
    ) |>
    dplyr::left_join(
      paseky_join,
      by = base::c(
        "SITECODE",
        "HABITAT_CODE"
      )
    ) |>
    dplyr::distinct()
  
  if (base::nrow(result) != 1) {
    base::stop(
      "stanoviste_eval(): po spojeni komponent nevznikl prave jeden radek.",
      call. = FALSE
    )
  }
  
  # ---------------------------------------------------------------------------
  # 9. Vystup
  # ---------------------------------------------------------------------------
  
  if (base::isTRUE(return_components)) {
    
    return(
      base::list(
        result = result,
        paseky = paseky,
        klic = klic,
        druhy = druhy,
        prostor = prostor
      )
    )
  }
  
  result
}
