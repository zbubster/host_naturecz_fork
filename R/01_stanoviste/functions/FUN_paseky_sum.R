# Paseky - sumarizace pro jednu kombinaci site x habitat
#
# Vstup:
#   paseky_detail - sf/data.frame s vysledky pouze z predem vybranych
#                   dvojic VMB pro jednotlive REGION_ID.
#
# Vystup:
#   tibble s jednim radkem pro site x habitat.

paseky_sum <- function(
    hab_code,
    site_code,
    paseky_detail
) {
  
  if (!base::substr(base::as.character(hab_code), 1, 1) %in% base::c("9", "L")) {
    return(
      dplyr::tibble(
        SITECODE = base::as.character(site_code),
        HABITAT_CODE = base::as.character(hab_code),
        ROZLOHA_PASEKY = NA_real_,
        ROZLOHA_HOLINY = NA_real_,
        POCET_SEGMENTU_PASEKY = NA_integer_
      )
    )
  }
  
  if (
    base::is.null(paseky_detail) ||
    base::nrow(paseky_detail) == 0
  ) {
    return(
      dplyr::tibble(
        SITECODE = base::as.character(site_code),
        HABITAT_CODE = base::as.character(hab_code),
        ROZLOHA_PASEKY = 0,
        ROZLOHA_HOLINY = 0,
        POCET_SEGMENTU_PASEKY = 0L
      )
    )
  }
  
  if (base::inherits(paseky_detail, "sf")) {
    target <- sf::st_drop_geometry(paseky_detail)
  } else {
    target <- paseky_detail
  }
  
  target <- target |>
    dplyr::filter(
      SITECODE == site_code,
      HABITAT_CODE == hab_code
    )
  
  rozloha_paseky <- target |>
    dplyr::filter(PASEKA == 1L) |>
    dplyr::pull(PLO_BIO_M2_INTERSECTION) |>
    base::sum(na.rm = TRUE)
  
  rozloha_paseky <- rozloha_paseky / 10000
  
  rozloha_holiny <- target |>
    dplyr::filter(HOLINA == 1L) |>
    dplyr::pull(PLO_BIO_M2_INTERSECTION) |>
    base::sum(na.rm = TRUE)
  
  rozloha_holiny <- rozloha_holiny / 10000
  
  pocet_segmentu <- target |>
    dplyr::filter(PASEKA == 1L) |>
    dplyr::pull(SEGMENT_ID_OLD) |>
    dplyr::n_distinct(na.rm = TRUE)
  
  dplyr::tibble(
    SITECODE = base::as.character(site_code),
    HABITAT_CODE = base::as.character(hab_code),
    ROZLOHA_PASEKY = base::as.numeric(rozloha_paseky),
    ROZLOHA_HOLINY = base::as.numeric(rozloha_holiny),
    POCET_SEGMENTU_PASEKY = base::as.integer(pocet_segmentu)
  )
}
