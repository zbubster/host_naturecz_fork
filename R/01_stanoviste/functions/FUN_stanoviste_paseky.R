# FUN_stanoviste_paseky.R
#
# Pasekove parametry pro jednu kombinaci site x habitat.
#
# Metadata-first workflow:
#   1. vybere dvojici VMB pro kazdy REGION_ID,
#   2. zjisti REGION_ID zasahujici hodnocenou site,
#   3. spusti prostorovy vypocet jen pro skutecne potrebne pary,
#   4. agreguje vysledek na site x habitat.
#
# Pro batch workflow lze dodat:
#   selected_pairs - predem spocitany vystup paseky_select_pairs()
#   site_regions   - predem ziskane REGION_ID zasahujici site
#
# Tim se pri batch vypoctu neopakuje stejna prace pro kazdy habitat.

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
    selected_pairs = NULL,
    site_regions = NULL,
    habitat_col = "HABITAT",
    biotop_col = "BIOTOP",
    share_col = "STEJ_PR",
    segment_id_col = "SEGMENT_ID",
    region_id_col = "REGION_ID",
    date_col = "DATUM",
    update_year_col = "ROK_AKT"
) {

  hab_code <- base::as.character(hab_code)
  site_code <- base::as.character(site_code)

  if (
    !base::substr(hab_code, 1, 1) %in% base::c("9", "L")
  ) {
    return(
      paseky_sum(
        hab_code = hab_code,
        site_code = site_code,
        paseky_detail = NULL
      )
    )
  }

  if (base::is.null(selected_pairs)) {
    selected_pairs <- paseky_select_pairs(
      vmb1_meta = vmb1_meta,
      vmb2_meta = vmb2_meta,
      vmb0_meta = vmb0_meta,
      region_id_col = region_id_col,
      date_col = date_col
    )
  }

  required_pair_cols <- base::c(
    "REGION_ID",
    "PAIR",
    "DATUM_OLD",
    "DATUM_NEW"
  )

  missing_pair_cols <- base::setdiff(
    required_pair_cols,
    base::names(selected_pairs)
  )

  if (base::length(missing_pair_cols) > 0) {
    base::stop(
      "stanoviste_paseky(): v `selected_pairs` chybi sloupce: ",
      base::paste(missing_pair_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (base::nrow(selected_pairs) == 0) {
    return(
      paseky_sum(
        hab_code = hab_code,
        site_code = site_code,
        paseky_detail = NULL
      )
    )
  }

  if (!"SITECODE" %in% base::names(site)) {
    base::stop(
      "stanoviste_paseky(): vrstva `site` neobsahuje `SITECODE`.",
      call. = FALSE
    )
  }

  site_target <- site |>
    dplyr::filter(
      base::as.character(SITECODE) == site_code
    )

  if (base::nrow(site_target) == 0) {
    base::stop(
      "stanoviste_paseky(): site `",
      site_code,
      "` nebyla nalezena.",
      call. = FALSE
    )
  }

  site_target <- sf::st_make_valid(site_target)

  if (base::is.null(site_regions)) {

    get_site_regions <- function(vmb) {

      if (!region_id_col %in% base::names(vmb)) {
        base::stop(
          "stanoviste_paseky(): prostorova VMB neobsahuje `",
          region_id_col,
          "`.",
          call. = FALSE
        )
      }

      if (
        !base::isTRUE(
          sf::st_crs(vmb) == sf::st_crs(site_target)
        )
      ) {
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

    site_regions <- base::unique(
      base::c(
        get_site_regions(vmb2_update),
        get_site_regions(vmb0_update)
      )
    )
  }

  site_regions <- base::unique(
    base::as.character(site_regions)
  )

  site_regions <- site_regions[
    !base::is.na(site_regions)
  ]

  selected_pairs_site <- selected_pairs |>
    dplyr::mutate(
      REGION_ID = base::as.character(REGION_ID)
    ) |>
    dplyr::filter(
      REGION_ID %in% site_regions
    )

  if (base::nrow(selected_pairs_site) == 0) {
    return(
      paseky_sum(
        hab_code = hab_code,
        site_code = site_code,
        paseky_detail = NULL
      )
    )
  }

  regions_vmb2_vmb0 <- selected_pairs_site |>
    dplyr::filter(PAIR == "VMB2_VMB0") |>
    dplyr::pull(REGION_ID)

  regions_vmb1_vmb2 <- selected_pairs_site |>
    dplyr::filter(PAIR == "VMB1_VMB2") |>
    dplyr::pull(REGION_ID)

  regions_vmb1_vmb0 <- selected_pairs_site |>
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

  paseky_detail <- if (base::length(detail_list) == 0) {
    NULL
  } else {
    dplyr::bind_rows(detail_list)
  }

  paseky_sum(
    hab_code = hab_code,
    site_code = site_code,
    paseky_detail = paseky_detail
  )
}
