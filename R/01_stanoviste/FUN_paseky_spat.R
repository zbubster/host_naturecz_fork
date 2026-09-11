# Paseky - prostorovy vypocet jedne dvojice VMB
#
# Funkce porovna starsi a novejsi VMB pro jednu kombinaci site x habitat.
#
# Vystup:
#   sf objekt s jednotlivymi pruniky starsiho a novejsiho mapovani.
#   REGION_ID, DATUM_NEW, DATUM_OLD, PASEKA,
#   HOLINA a PLO_BIO_M2_INTERSECTION pro dalsi kroky workflow.

paseky_spat <- function(
    hab_code,
    site_code,
    site,
    vmb_old,
    vmb_new,
    pair,
    habitat_col = "HABITAT",
    biotop_col = "BIOTOP",
    share_col = "STEJ_PR",
    segment_id_col = "SEGMENT_ID",
    region_id_col = NULL,
    date_col_old = NULL,
    date_col_new = NULL,
    update_year_col = NULL
) {
  
  # Lesni stanoviste -----------------------------------------------------------
  
  if (!base::substr(base::as.character(hab_code), 1, 1) %in% base::c("9", "L")) {
    return(NULL)
  }
  
  # Kontrola site --------------------------------------------------------------
  
  if (!"SITECODE" %in% base::names(site)) {
    base::stop("Vrstva `site` neobsahuje sloupec `SITECODE`.")
  }
  
  site_target <- site |>
    dplyr::filter(SITECODE == site_code)
  
  if (base::nrow(site_target) == 0) {
    base::stop("`site_code` nebyl nalezen ve vrstve `site`: ", site_code)
  }
  
  site_target <- sf::st_make_valid(site_target)
  
  site_geom <- site_target |>
    sf::st_geometry() |>
    sf::st_union()
  
  target_crs <- sf::st_crs(site_target)
  
  if (!base::isTRUE(sf::st_crs(vmb_old) == target_crs)) {
    vmb_old <- sf::st_transform(vmb_old, target_crs)
  }
  
  if (!base::isTRUE(sf::st_crs(vmb_new) == target_crs)) {
    vmb_new <- sf::st_transform(vmb_new, target_crs)
  }
  
  # Automaticke dohledani sloupcu, ktere se mezi verzemi VMB mohou lisit ------
  
  if (base::is.null(region_id_col)) {
    region_candidates <- base::c(
      "REGION_ID",
      "REGION_ID.x",
      "REGION_ID_X",
      "region_id",
      "region_id.x",
      "region_id_x"
    )
    
    region_id_col <- region_candidates[
      region_candidates %in% base::names(vmb_new)
    ][1]
  }
  
  if (base::is.null(date_col_old)) {
    date_candidates <- base::c(
      "DATUM",
      "DATUM.x",
      "DATUM_X",
      "datum",
      "datum.x",
      "datum_x"
    )
    
    date_col_old <- date_candidates[
      date_candidates %in% base::names(vmb_old)
    ][1]
  }
  
  if (base::is.null(date_col_new)) {
    date_candidates <- base::c(
      "DATUM",
      "DATUM.x",
      "DATUM_X",
      "datum",
      "datum.x",
      "datum_x"
    )
    
    date_col_new <- date_candidates[
      date_candidates %in% base::names(vmb_new)
    ][1]
  }
  
  if (base::is.null(update_year_col)) {
    update_year_candidates <- base::c(
      "ROK_AKT.x",
      "ROK_AKT",
      "ROK_AKT_X",
      "rok_akt.x",
      "rok_akt",
      "rok_akt_x"
    )
    
    update_year_col <- update_year_candidates[
      update_year_candidates %in% base::names(vmb_new)
    ][1]
  }
  
  # Kontrola nutnych atributu --------------------------------------------------
  
  required_old <- base::c(
    habitat_col,
    biotop_col,
    share_col,
    segment_id_col,
    date_col_old
  )
  
  required_new <- base::c(
    biotop_col,
    share_col,
    segment_id_col,
    region_id_col,
    date_col_new,
    update_year_col
  )
  
  missing_old <- required_old[
    base::is.na(required_old) |
      !required_old %in% base::names(vmb_old)
  ]
  
  missing_new <- required_new[
    base::is.na(required_new) |
      !required_new %in% base::names(vmb_new)
  ]
  
  if (base::length(missing_old) > 0) {
    base::stop(
      "V `vmb_old` chybi potrebne sloupce: ",
      base::paste(missing_old, collapse = ", ")
    )
  }
  
  if (base::length(missing_new) > 0) {
    base::stop(
      "V `vmb_new` chybi potrebne sloupce: ",
      base::paste(missing_new, collapse = ", ")
    )
  }
  
  # Starsi mapovani ------------------------------------------------------------
  
  vmb_old_target <- vmb_old |>
    dplyr::filter(
      base::as.character(.data[[habitat_col]]) == base::as.character(hab_code) |
        base::as.character(.data[[biotop_col]]) == base::as.character(hab_code)
    )
  
  if (base::nrow(vmb_old_target) == 0) {
    return(NULL)
  }
  
  vmb_old_target <- sf::st_filter(
    x = vmb_old_target,
    y = site_geom,
    .predicate = sf::st_intersects
  )
  
  if (base::nrow(vmb_old_target) == 0) {
    return(NULL)
  }
  
  vmb_old_target <- sf::st_intersection(
    vmb_old_target,
    site_geom
  ) |>
    sf::st_make_valid() |>
    dplyr::filter(
      base::as.character(sf::st_geometry_type(geometry)) %in%
        base::c("POLYGON", "MULTIPOLYGON")
    ) |>
    dplyr::mutate(
      SEGMENT_ID_OLD = base::as.character(.data[[segment_id_col]]),
      BIOTOP_ORIG = base::as.character(.data[[biotop_col]]),
      STEJ_PR_ORIG = base::as.numeric(.data[[share_col]]),
      DATUM_OLD = base::as.Date(.data[[date_col_old]])
    ) |>
    dplyr::select(
      SEGMENT_ID_OLD,
      BIOTOP_ORIG,
      STEJ_PR_ORIG,
      DATUM_OLD,
      geometry
    )
  
  if (base::nrow(vmb_old_target) == 0) {
    return(NULL)
  }
  
  # Novejsi mapovani -----------------------------------------------------------
  
  vmb_new_target <- sf::st_filter(
    x = vmb_new,
    y = site_geom,
    .predicate = sf::st_intersects
  )
  
  if (base::nrow(vmb_new_target) == 0) {
    return(NULL)
  }
  
  vmb_new_target <- sf::st_intersection(
    vmb_new_target,
    site_geom
  ) |>
    sf::st_make_valid() |>
    dplyr::filter(
      base::as.character(sf::st_geometry_type(geometry)) %in%
        base::c("POLYGON", "MULTIPOLYGON")
    ) |>
    dplyr::mutate(
      SEGMENT_ID_NEW = base::as.character(.data[[segment_id_col]]),
      REGION_ID = base::as.character(.data[[region_id_col]]),
      BIOTOP_UPDATE = base::as.character(.data[[biotop_col]]),
      STEJ_PR_UPDATE = base::as.numeric(.data[[share_col]]),
      DATUM_NEW = base::as.Date(.data[[date_col_new]]),
      ROK_AKT_UPDATE = base::as.integer(.data[[update_year_col]])
    ) |>
    dplyr::select(
      SEGMENT_ID_NEW,
      REGION_ID,
      BIOTOP_UPDATE,
      STEJ_PR_UPDATE,
      DATUM_NEW,
      ROK_AKT_UPDATE,
      geometry
    )
  
  if (base::nrow(vmb_new_target) == 0) {
    return(NULL)
  }
  
  # Omezeni na segmenty, ktere se opravdu prekryvaji --------------------------
  
  hit_list <- sf::st_intersects(
    vmb_new_target,
    vmb_old_target
  )
  
  if (!base::any(base::lengths(hit_list) > 0)) {
    return(NULL)
  }
  
  new_sub <- vmb_new_target[
    base::lengths(hit_list) > 0,
  ]
  
  old_index <- base::sort(
    base::unique(
      base::unlist(hit_list)
    )
  )
  
  old_sub <- vmb_old_target[
    old_index,
  ]
  
  # Vypocet pasek -------------------------------------------------------------
  
  result <- sf::st_intersection(
    new_sub,
    old_sub
  ) |>
    sf::st_make_valid() |>
    dplyr::filter(
      base::as.character(sf::st_geometry_type(geometry)) %in%
        base::c("POLYGON", "MULTIPOLYGON")
    ) |>
    dplyr::mutate(
      SITECODE = base::as.character(site_code),
      HABITAT_CODE = base::as.character(hab_code),
      PAIR = base::as.character(pair),
      
      PASEKA = dplyr::case_when(
        BIOTOP_UPDATE %in% base::c("LP", "X10") ~ 1L,
        BIOTOP_UPDATE %in% base::c("X11", "X12A", "X12B") &
          ROK_AKT_UPDATE %in% 2007:2012 ~ 1L,
        TRUE ~ 0L
      ),
      
      area_intersection_m2 = units::drop_units(
        sf::st_area(geometry)
      ),
      
      PLO_BIO_M2_INTERSECTION =
        area_intersection_m2 *
        STEJ_PR_ORIG / 100 *
        STEJ_PR_UPDATE / 100,
      
      HOLINA = dplyr::case_when(
        PASEKA == 1L &
          PLO_BIO_M2_INTERSECTION > 10000 ~ 1L,
        TRUE ~ 0L
      )
    ) |>
    dplyr::select(
      SITECODE,
      HABITAT_CODE,
      PAIR,
      REGION_ID,
      DATUM_NEW,
      DATUM_OLD,
      SEGMENT_ID_OLD,
      SEGMENT_ID_NEW,
      BIOTOP_ORIG,
      BIOTOP_UPDATE,
      ROK_AKT_UPDATE,
      PASEKA,
      HOLINA,
      PLO_BIO_M2_INTERSECTION,
      geometry
    )
  
  if (base::nrow(result) == 0) {
    return(NULL)
  }
  
  result
}
