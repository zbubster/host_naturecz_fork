# Stanoviste - pasekove parametry
#
# Hlavni wrapper pasekoveho workflow pro jednu kombinaci site x habitat.
#
# Dulezite:
# VMB2 vstupuje do workflow ve dvou pripravenych podobach:
#   vmb2_base   - VMB2 pouzivana jako starsi / zakladni vrstva
#   vmb2_update - VMB2 pouzivana jako novejsi / aktualizacni vrstva
#
# Typicke mapovani na stavajici objekty:
#   vmb1_base   = vmb_shp_sjtsk_orig
#   vmb2_base   = vmb_shp_sjtsk_a1
#   vmb2_update = vmb_pb_x_a1
#   vmb0_update = vmb_pb_x_akt
#
# Funkce predpoklada, ze jsou nacteny:
#   paseky_spat()
#   paseky_latest()
#   paseky_sum()

stanoviste_paseky <- function(
    hab_code,
    site_code,
    site,
    vmb1_base,
    vmb2_base,
    vmb2_update,
    vmb0_update,
    habitat_col = "HABITAT",
    biotop_col = "BIOTOP",
    share_col = "STEJ_PR",
    segment_id_col = "SEGMENT_ID"
) {
  
  # Kontrola zavislosti --------------------------------------------------------
  
  required_functions <- base::c(
    "FUN_paseky_spat",
    "FUN_paseky_latest",
    "FUN_paseky_sum"
  )
  
  missing_functions <- required_functions[
    !base::vapply(
      required_functions,
      base::exists,
      logical(1),
      mode = "function"
    )
  ]
  
  if (base::length(missing_functions) > 0) {
    base::stop(
      "Nejsou nacteny potrebne funkce: ",
      base::paste(missing_functions, collapse = ", ")
    )
  }
  
  # Nelesni habitat lze ukoncit bez prostorovych vypoctu ----------------------
  
  if (!base::substr(base::as.character(hab_code), 1, 1) %in% base::c("9", "L")) {
    return(
      paseky_sum(
        hab_code = hab_code,
        site_code = site_code,
        paseky_selected = NULL
      )
    )
  }
  
  # Vsechny tri kandidatske casove dvojice ------------------------------------
  
  pair_vmb1_vmb2 <- paseky_spat(
    hab_code = hab_code,
    site_code = site_code,
    site = site,
    vmb_old = vmb1_base,
    vmb_new = vmb2_update,
    pair = "VMB1_VMB2",
    habitat_col = habitat_col,
    biotop_col = biotop_col,
    share_col = share_col,
    segment_id_col = segment_id_col
  )
  
  pair_vmb2_vmb0 <- paseky_spat(
    hab_code = hab_code,
    site_code = site_code,
    site = site,
    vmb_old = vmb2_base,
    vmb_new = vmb0_update,
    pair = "VMB2_VMB0",
    habitat_col = habitat_col,
    biotop_col = biotop_col,
    share_col = share_col,
    segment_id_col = segment_id_col
  )
  
  pair_vmb1_vmb0 <- paseky_spat(
    hab_code = hab_code,
    site_code = site_code,
    site = site,
    vmb_old = vmb1_base,
    vmb_new = vmb0_update,
    pair = "VMB1_VMB0",
    habitat_col = habitat_col,
    biotop_col = biotop_col,
    share_col = share_col,
    segment_id_col = segment_id_col
  )
  
  pair_results <- base::Filter(
    Negate(base::is.null),
    base::list(
      pair_vmb1_vmb2,
      pair_vmb2_vmb0,
      pair_vmb1_vmb0
    )
  )
  
  # Pokud nevznikl zadny kandidat, lesni habitat ma nulovou plochu pasek -------
  
  if (base::length(pair_results) == 0) {
    return(
      paseky_sum(
        hab_code = hab_code,
        site_code = site_code,
        paseky_selected = NULL
      )
    )
  }
  
  paseky_all <- dplyr::bind_rows(pair_results)
  
  # dvojice VMB se vybira zvlast pro kazdy REGION_ID.
  # Jedna site tedy muze soucasne pouzivat nekolik ruznych casovych dvojic.
  
  paseky_selected <- paseky_latest(
    paseky_all = paseky_all
  )
  
  # Az po regionovem vyberu se vysledek agreguje na site x habitat -------------
  
  paseky_sum(
    hab_code = hab_code,
    site_code = site_code,
    paseky_selected = paseky_selected
  )
}
