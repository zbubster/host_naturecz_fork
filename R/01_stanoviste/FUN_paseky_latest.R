# Paseky - vyber nejaktualnejsi dvojice VMB pro kazdy aktualizacni okrsek
#
# Vstup:
#   paseky_all - sf objekt vznikly spojenim kandidatu ze vsech dvojic VMB
#
# Vystup:
#   stejny sf objekt, ale pro kazdy SITECODE x HABITAT_CODE x REGION_ID
#   zustane pouze dvojice VMB vybrana podle priority a data aktualizace.

paseky_latest <- function(paseky_all) {
  
  if (base::is.null(paseky_all) || base::nrow(paseky_all) == 0) {
    return(paseky_all)
  }
  
  required_cols <- base::c(
    "SITECODE",
    "HABITAT_CODE",
    "REGION_ID",
    "PAIR",
    "DATUM_NEW",
    "DATUM_OLD"
  )
  
  missing_cols <- required_cols[
    !required_cols %in% base::names(paseky_all)
  ]
  
  if (base::length(missing_cols) > 0) {
    base::stop(
      "V `paseky_all` chybi potrebne sloupce: ",
      base::paste(missing_cols, collapse = ", ")
    )
  }
  
  # Jedna sumarizace pro region a dvojici VMB ---------------------------------
  
  pair_summary <- paseky_all |>
    sf::st_drop_geometry() |>
    dplyr::filter(!base::is.na(REGION_ID)) |>
    dplyr::group_by(
      SITECODE,
      HABITAT_CODE,
      REGION_ID,
      PAIR
    ) |>
    dplyr::summarise(
      DATUM_NEW = dplyr::if_else(
        base::all(base::is.na(DATUM_NEW)),
        base::as.Date(NA),
        base::max(DATUM_NEW, na.rm = TRUE)
      ),
      DATUM_OLD = dplyr::if_else(
        base::all(base::is.na(DATUM_OLD)),
        base::as.Date(NA),
        base::max(DATUM_OLD, na.rm = TRUE)
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      has_update =
        !base::is.na(DATUM_NEW) &
        !base::is.na(DATUM_OLD) &
        DATUM_NEW > DATUM_OLD,
      
      pair_priority = dplyr::case_when(
        PAIR == "VMB2_VMB0" ~ 1L,
        PAIR == "VMB1_VMB2" ~ 2L,
        PAIR == "VMB1_VMB0" ~ 3L,
        TRUE ~ 999L
      )
    )
  
  # Vyber dvojice zvlast pro kazdy aktualizacni okrsek -------------------------
  
  latest_choice <- pair_summary |>
    dplyr::filter(has_update) |>
    dplyr::group_by(
      SITECODE,
      HABITAT_CODE,
      REGION_ID
    ) |>
    dplyr::arrange(
      pair_priority,
      dplyr::desc(DATUM_NEW),
      dplyr::desc(DATUM_OLD),
      .by_group = TRUE
    ) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(
      SITECODE,
      HABITAT_CODE,
      REGION_ID,
      PAIR
    )
  
  if (base::nrow(latest_choice) == 0) {
    return(paseky_all[0, ])
  }
  
  # Zachovat vsechny prostorove zaznamy z vybrane dvojice ----------------------
  
  paseky_all |>
    dplyr::semi_join(
      latest_choice,
      by = base::c(
        "SITECODE",
        "HABITAT_CODE",
        "REGION_ID",
        "PAIR"
      )
    )
}
