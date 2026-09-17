# FUN_stanoviste_load_previous.R
#
# Načtení historického širokého výstupu starého workflow stanovišť.
#
# Funkce je určena zejména pro soubory typu:
#
#   Outputs/Data/stanoviste/results_habitats_24_20250806.csv
#
# které vznikaly pomocí write.csv2(..., fileEncoding = "Windows-1250").
#
# `source` může být:
#   - lokální cesta k CSV,
#   - raw GitHub URL,
#   - běžná GitHub "blob" URL; ta se automaticky převede na raw URL.
#
# Funkce:
#   - načte CSV2 (oddělovač ";"),
#   - sjednotí problematický kód 91E0,
#   - převede ROZLOHA NA -> 0,
#   - sjednotí pomlčky v NAZEV,
#   - parsuje dostupné DATE_* sloupce jako Date,
#   - odstraní přesné duplicity,
#   - ověří, že zbývá právě jeden řádek pro SITECODE × HABITAT_CODE.
#
# Výstup:
#   široký tibble kompatibilní se stanoviste_hodnoceni() a stanoviste_trend().

stanoviste_load_previous <- function(
    source,
    encoding = "Windows-1250"
) {
  
  # ---------------------------------------------------------------------------
  # 1. Validace zdroje
  # ---------------------------------------------------------------------------
  
  if (
    base::length(source) != 1 ||
    base::is.na(source) ||
    !base::nzchar(base::as.character(source))
  ) {
    base::stop(
      "stanoviste_load_previous(): `source` musí být jedna neprázdná cesta nebo URL.",
      call. = FALSE
    )
  }
  
  source <- base::as.character(source)
  
  # GitHub blob URL -> raw URL.
  source_read <- base::sub(
    "^https://github\\.com/([^/]+)/([^/]+)/blob/([^/]+)/(.+)$",
    "https://raw.githubusercontent.com/\\1/\\2/\\3/\\4",
    source
  )
  
  # ---------------------------------------------------------------------------
  # 2. Načtení
  # ---------------------------------------------------------------------------
  
  previous <- readr::read_csv2(
    source_read,
    locale = readr::locale(
      encoding = encoding
    ),
    show_col_types = FALSE,
    progress = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # 3. Povinné sloupce
  # ---------------------------------------------------------------------------
  
  required_cols <- base::c(
    "SITECODE",
    "HABITAT_CODE",
    "ROZLOHA",
    "KVALITA"
  )
  
  missing_cols <- base::setdiff(
    required_cols,
    base::names(previous)
  )
  
  if (base::length(missing_cols) > 0) {
    base::stop(
      "stanoviste_load_previous(): historický soubor nemá povinné sloupce: ",
      base::paste(
        missing_cols,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  # ---------------------------------------------------------------------------
  # 4. Normalizace
  # ---------------------------------------------------------------------------
  
  previous <- previous |>
    dplyr::mutate(
      SITECODE = base::as.character(SITECODE),
      HABITAT_CODE = base::as.character(HABITAT_CODE),
      HABITAT_CODE = dplyr::case_when(
        HABITAT_CODE == "91" ~ "91E0",
        HABITAT_CODE == "9.10E+01" ~ "91E0",
        HABITAT_CODE == "9,10E+01" ~ "91E0",
        TRUE ~ HABITAT_CODE
      ),
      ROZLOHA = tidyr::replace_na(
        base::as.numeric(ROZLOHA),
        0
      )
    )
  
  if ("NAZEV" %in% base::names(previous)) {
    previous <- previous |>
      dplyr::mutate(
        NAZEV = stringr::str_replace_all(
          NAZEV,
          "–|—",
          "-"
        )
      )
  }
  
  date_cols <- base::intersect(
    base::c(
      "DATE_MIN",
      "DATE_MAX",
      "DATE_MEAN",
      "DATE_MEDIAN"
    ),
    base::names(previous)
  )
  
  if (base::length(date_cols) > 0) {
    previous <- previous |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(date_cols),
          ~ base::as.Date(.x)
        )
      )
  }
  
  previous <- previous |>
    dplyr::distinct()
  
  # ---------------------------------------------------------------------------
  # 5. Kontrola unikátnosti site × habitat
  # ---------------------------------------------------------------------------
  
  duplicate_keys <- previous |>
    sf::st_drop_geometry() |>
    dplyr::count(
      SITECODE,
      HABITAT_CODE,
      name = "n"
    ) |>
    dplyr::filter(
      n > 1
    )
  
  if (base::nrow(duplicate_keys) > 0) {
    base::stop(
      "stanoviste_load_previous(): historický soubor obsahuje více různých řádků ",
      "pro stejnou kombinaci SITECODE × HABITAT_CODE. ",
      "Nejdřív je nutné vyřešit duplicity.",
      call. = FALSE
    )
  }
  
  previous
}
