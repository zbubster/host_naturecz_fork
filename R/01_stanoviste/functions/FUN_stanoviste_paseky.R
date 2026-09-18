# Stanoviste - pasekove parametry, metadata-first workflow
#
# Poradi:
#   1. paseky_select_pairs() vybere par VMB pro KAZDY REGION_ID pouze z metadat
#   2. zjisti se REGION_ID, ktere prostorove zasahuji hodnocenou site
#   3. paseky_spat() se spusti pouze pro skutecne potrebne pary a regiony
#   4. paseky_sum() agreguje vysledek na site x habitat
#
# Metadatove vstupy mohou byt data.frame z HAB_BIOTOP.dbf nebo sf.
# Pokud nejsou zadany samostatne, pouziji se atributy prostorovych vrstev.

stanoviste_paseky <- function(
    hab_code,
    site_code,
    site,
    vmb1_base,
    vmb2_base,
    vmb2_update,
    vmb0_update,
    vmb1_meta = vmb1_base,
    vmb2_meta = vmb2_update,
    vmb0_meta = vmb0_update,
    habitat_col = "HABITAT",
    biotop_col = "BIOTOP",
    share_col = "STEJ_PR",
    segment_id_col = "SEGMENT_ID",
    region_id_col = "REGION_ID",
    date_col = "DATUM",
    update_year_col = "ROK_AKT"
) {
  
  # Nelesni habitat ------------------------------------------------------------
  
  if (!base::substr(base::as.character(hab_code), 1, 1) %in% base::c("9", "L")) {
    return(
      paseky_sum(
        hab_code = hab_code,
        site_code = site_code,
        paseky_detail = NULL
      )
    )
  }
  
  # 1. Vyber dvojice VMB pouze podle metadat ----------------------------------
  
  selected_pairs <- paseky_select_pairs(
    vmb1_meta = vmb1_meta,
    vmb2_meta = vmb2_meta,
    vmb0_meta = vmb0_meta,
    region_id_col = region_id_col,
    date_col = date_col
  )
  
  if (base::nrow(selected_pairs) == 0) {
    return(
      paseky_sum(
        hab_code = hab_code,
        site_code = site_code,
        paseky_detail = NULL
      )
    )
  }
  
  # 2. Site --------------------------------------------------------------------
  
  if (!"SITECODE" %in% base::names(site)) {
    base::stop("Vrstva `site` neobsahuje sloupec `SITECODE`.")
  }
  
  site_target <- site |>
    dplyr::filter(SITECODE == site_code)
  
  if (base::nrow(site_target) == 0) {
    base::stop("`site_code` nebyl nalezen ve vrstve `site`: ", site_code)
  }
  
  site_target <- sf::st_make_valid(site_target)
  
  # Lehky spatial predicate: pouze zjisteni REGION_ID v site -------------------
  
  get_site_regions <- function(vmb) {
    
    if (!region_id_col %in% base::names(vmb)) {
      base::stop(
        "Prostorova VMB neobsahuje sloupec `",
        region_id_col, "`."
      )
    }
    
    if (!base::isTRUE(sf::st_crs(vmb) == sf::st_crs(site_target))) {
      vmb <- sf::st_transform(
        vmb,
        sf::st_crs(site_target)
      )
    }
    
    vmb |>
      dplyr::select(
        dplyr::all_of(region_id_col),
        geometry
      ) |>
      sf::st_filter(
        site_target,
        .predicate = sf::st_intersects
      ) |>
      sf::st_drop_geometry() |>
      dplyr::pull(
        dplyr::all_of(region_id_col)
      ) |>
      base::as.character() |>
      base::unique()
  }
  
  # Regiony bereme z vrstev, ktere mohou byt novejsi stranou paru.
  # Zde se jeste zadny st_intersection neprovadi.
  
  site_regions <- base::unique(
    base::c(
      get_site_regions(vmb2_update),
      get_site_regions(vmb0_update)
    )
  )
  
  site_regions <- site_regions[
    !base::is.na(site_regions)
  ]
  
  selected_pairs <- selected_pairs |>
    dplyr::filter(REGION_ID %in% site_regions)
  
  if (base::nrow(selected_pairs) == 0) {
    return(
      paseky_sum(
        hab_code = hab_code,
        site_code = site_code,
        paseky_detail = NULL
      )
    )
  }
  
  # 3. Prostorovy vypocet jen pro skutecne potrebne pary ----------------------
  
  regions_vmb2_vmb0 <- selected_pairs |>
    dplyr::filter(PAIR == "VMB2_VMB0") |>
    dplyr::pull(REGION_ID)
  
  regions_vmb1_vmb2 <- selected_pairs |>
    dplyr::filter(PAIR == "VMB1_VMB2") |>
    dplyr::pull(REGION_ID)
  
  regions_vmb1_vmb0 <- selected_pairs |>
    dplyr::filter(PAIR == "VMB1_VMB0") |>
    dplyr::pull(REGION_ID)
  
  result_vmb2_vmb0 <- if (base::length(regions_vmb2_vmb0) > 0) {
    paseky_spat(
      hab_code = hab_code,
      site_code = site_code,
      site = site,
      vmb_old = vmb2_base,
      vmb_new = vmb0_update,
      region_ids = regions_vmb2_vmb0,
      pair = "VMB2_VMB0",
      habitat_col = habitat_col,
      biotop_col = biotop_col,
      share_col = share_col,
      segment_id_col = segment_id_col,
      region_id_col = region_id_col,
      date_col = date_col,
      update_year_col = update_year_col
    )
  } else {
    NULL
  }
  
  result_vmb1_vmb2 <- if (base::length(regions_vmb1_vmb2) > 0) {
    paseky_spat(
      hab_code = hab_code,
      site_code = site_code,
      site = site,
      vmb_old = vmb1_base,
      vmb_new = vmb2_update,
      region_ids = regions_vmb1_vmb2,
      pair = "VMB1_VMB2",
      habitat_col = habitat_col,
      biotop_col = biotop_col,
      share_col = share_col,
      segment_id_col = segment_id_col,
      region_id_col = region_id_col,
      date_col = date_col,
      update_year_col = update_year_col
    )
  } else {
    NULL
  }
  
  result_vmb1_vmb0 <- if (base::length(regions_vmb1_vmb0) > 0) {
    paseky_spat(
      hab_code = hab_code,
      site_code = site_code,
      site = site,
      vmb_old = vmb1_base,
      vmb_new = vmb0_update,
      region_ids = regions_vmb1_vmb0,
      pair = "VMB1_VMB0",
      habitat_col = habitat_col,
      biotop_col = biotop_col,
      share_col = share_col,
      segment_id_col = segment_id_col,
      region_id_col = region_id_col,
      date_col = date_col,
      update_year_col = update_year_col
    )
  } else {
    NULL
  }
  
  detail_list <- base::Filter(
    Negate(base::is.null),
    base::list(
      result_vmb2_vmb0,
      result_vmb1_vmb2,
      result_vmb1_vmb0
    )
  )
  
  if (base::length(detail_list) == 0) {
    paseky_detail <- NULL
  } else {
    paseky_detail <- dplyr::bind_rows(detail_list)
  }
  
  # 4. Agregace na site x habitat ---------------------------------------------
  
  paseky_sum(
    hab_code = hab_code,
    site_code = site_code,
    paseky_detail = paseky_detail
  )
}
