# FUN_paseky_select_pairs.R
#
# Paseky - priprava kandidatnich dvojic VMB podle metadat.
#
# SITECODE x HABITAT_CODE x REGION_ID.
#
# Tato funkce proto pouze:
#   1. sestavi vsechny tri mozne dvojice,
#   2. vyradi dvojice bez skutecneho casoveho posunu,
#   3. priradi prioritu shodnou se starym workflow.
#
# Definitivni vyber paru se provede az ve stanoviste_paseky(),
# po overeni, ze pro danou site x habitat x REGION_ID skutecne vznikl
# prostorovy vysledek.
#
# Priorita:
#   1. VMB2 -> VMB0
#   2. VMB1 -> VMB2
#   3. VMB1 -> VMB0
#
# Vystup:
#   0 az 3 kandidatni radky na REGION_ID.

paseky_select_pairs <- function(
    vmb1_meta,
    vmb2_meta,
    vmb0_meta,
    region_id_col = "REGION_ID",
    date_col = "DATUM"
) {
  
  summarise_meta <- function(
    x,
    version
  ) {
    
    if (base::inherits(x, "sf")) {
      x <- sf::st_drop_geometry(x)
    }
    
    required_cols <- base::c(
      region_id_col,
      date_col
    )
    
    missing_cols <- base::setdiff(
      required_cols,
      base::names(x)
    )
    
    if (base::length(missing_cols) > 0) {
      base::stop(
        "paseky_select_pairs(): v metadatech ",
        version,
        " chybi sloupce: ",
        base::paste(
          missing_cols,
          collapse = ", "
        ),
        call. = FALSE
      )
    }
    
    out <- x |>
      dplyr::transmute(
        REGION_ID = base::as.character(
          .data[[region_id_col]]
        ),
        DATUM = base::as.Date(
          .data[[date_col]]
        )
      ) |>
      dplyr::filter(
        !base::is.na(REGION_ID)
      ) |>
      dplyr::group_by(
        REGION_ID
      ) |>
      dplyr::summarise(
        N_DATES = dplyr::n_distinct(
          DATUM,
          na.rm = TRUE
        ),
        DATUM = if (
          base::all(
            base::is.na(DATUM)
          )
        ) {
          base::as.Date(NA)
        } else {
          base::max(
            DATUM,
            na.rm = TRUE
          )
        },
        .groups = "drop"
      )
    
    n_multi <- out |>
      dplyr::filter(
        N_DATES > 1
      ) |>
      base::nrow()
    
    if (n_multi > 0) {
      base::warning(
        version,
        ": ",
        n_multi,
        " REGION_ID ma vice nez jedno DATUM; ",
        "pro metadata kandidatu se pouzije nejpozdejsi datum.",
        call. = FALSE
      )
    }
    
    out |>
      dplyr::select(
        REGION_ID,
        DATUM
      )
  }
  
  
  meta1 <- summarise_meta(
    vmb1_meta,
    "VMB1"
  ) |>
    dplyr::rename(
      DATUM_VMB1 = DATUM
    )
  
  meta2 <- summarise_meta(
    vmb2_meta,
    "VMB2"
  ) |>
    dplyr::rename(
      DATUM_VMB2 = DATUM
    )
  
  meta0 <- summarise_meta(
    vmb0_meta,
    "VMB0"
  ) |>
    dplyr::rename(
      DATUM_VMB0 = DATUM
    )
  
  
  dates <- meta1 |>
    dplyr::full_join(
      meta2,
      by = "REGION_ID"
    ) |>
    dplyr::full_join(
      meta0,
      by = "REGION_ID"
    )
  
  
  candidates <- dplyr::bind_rows(
    
    dates |>
      dplyr::transmute(
        REGION_ID,
        PAIR = "VMB2_VMB0",
        DATUM_OLD = DATUM_VMB2,
        DATUM_NEW = DATUM_VMB0,
        PAIR_PRIORITY = 1L
      ),
    
    dates |>
      dplyr::transmute(
        REGION_ID,
        PAIR = "VMB1_VMB2",
        DATUM_OLD = DATUM_VMB1,
        DATUM_NEW = DATUM_VMB2,
        PAIR_PRIORITY = 2L
      ),
    
    dates |>
      dplyr::transmute(
        REGION_ID,
        PAIR = "VMB1_VMB0",
        DATUM_OLD = DATUM_VMB1,
        DATUM_NEW = DATUM_VMB0,
        PAIR_PRIORITY = 3L
      )
  ) |>
    dplyr::mutate(
      HAS_UPDATE =
        !base::is.na(DATUM_NEW) &
        !base::is.na(DATUM_OLD) &
        DATUM_NEW > DATUM_OLD
    ) |>
    dplyr::filter(
      HAS_UPDATE
    ) |>
    dplyr::arrange(
      REGION_ID,
      PAIR_PRIORITY,
      dplyr::desc(DATUM_NEW),
      dplyr::desc(DATUM_OLD)
    ) |>
    dplyr::select(
      REGION_ID,
      PAIR,
      DATUM_OLD,
      DATUM_NEW,
      PAIR_PRIORITY
    )
  
  candidates
}
