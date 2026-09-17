# FUN_stanoviste_trend.R
#
# Výpočet trendu mezi dvěma hodnoticími obdobími.
#
# Vstupy:
#   current  - aktuální široké výsledky po stanoviste_hodnoceni()
#   previous - historické široké výsledky po stanoviste_hodnoceni()
#
# Oba vstupy musí mít:
#   SITECODE
#   HABITAT_CODE
#
# Pro plnou reprodukci trendu klíčových parametrů se očekává:
#   ROZLOHA
#   KVALITA
#   CELKOVE_HODNOCENI
#
# Logika zachovává n2k_stanoviste_srovnani.R:
#
#   ROZLOHA:
#     změna v pásmu ±5 % -> stabilní
#     pokles > 5 %       -> zhoršující se
#     růst > 5 %         -> zlepšující se
#
#   KVALITA:
#     změna v pásmu ±5 % -> stabilní
#     růst > 5 %         -> zhoršující se
#     pokles > 5 %       -> zlepšující se
#
#   CELKOVE_HODNOCENI:
#     dobrý -> zhoršený/špatný = zhoršující se
#     zhoršený -> špatný       = zhoršující se
#     špatný -> zhoršený/dobrý = zlepšující se
#     zhoršený -> dobrý        = zlepšující se
#     stejná kategorie         = stabilní
#
# U dalších indikátorů starý skript neměl definovaný směr změny.
# Proto:
#   - stejná hodnota / numerická změna v toleranci -> stabilní
#   - změna mimo toleranci -> neznámý
#
# Výstup:
#   standardně current + sloupce TREND_<indikátor>
#
# Při return_detail = TRUE:
#   list(
#     result = ...,
#     detail = ...
#   )

stanoviste_trend <- function(
    current,
    previous,
    tolerance = 0.05,
    indicators = base::c(
      "ROZLOHA",
      "KVALITA",
      "TYPICKE_DRUHY",
      "MINIMIAREAL",
      "MINIMIAREAL_JADRA",
      "MINIMIAREAL_HODNOTA",
      "MOZAIKA_VNEJSI",
      "MOZAIKA_VNITRNI",
      "MOZAIKA_FIN",
      "RED_LIST",
      "INVASIVE",
      "EXPANSIVE",
      "MRTVE_DREVO",
      "KALAMITA_POLOM",
      "RED_LIST_SPECIES",
      "INVASIVE_LIST",
      "EXPANSIVE_LIST",
      "CELKOVE_HODNOCENI"
    ),
    return_detail = FALSE
) {
  
  # ---------------------------------------------------------------------------
  # 1. Validace
  # ---------------------------------------------------------------------------
  
  if (!base::is.data.frame(current)) {
    base::stop(
      "stanoviste_trend(): `current` musí být data.frame/tibble.",
      call. = FALSE
    )
  }
  
  if (!base::is.data.frame(previous)) {
    base::stop(
      "stanoviste_trend(): `previous` musí být data.frame/tibble.",
      call. = FALSE
    )
  }
  
  if (
    base::length(tolerance) != 1 ||
    base::is.na(tolerance) ||
    !base::is.numeric(tolerance) ||
    tolerance < 0
  ) {
    base::stop(
      "stanoviste_trend(): `tolerance` musí být jedno nezáporné číslo.",
      call. = FALSE
    )
  }
  
  key_cols <- base::c(
    "SITECODE",
    "HABITAT_CODE"
  )
  
  for (object_name in base::c(
    "current",
    "previous"
  )) {
    
    object <- base::get(
      object_name
    )
    
    missing_keys <- base::setdiff(
      key_cols,
      base::names(object)
    )
    
    if (base::length(missing_keys) > 0) {
      base::stop(
        "stanoviste_trend(): v `",
        object_name,
        "` chybí klíčové sloupce: ",
        base::paste(
          missing_keys,
          collapse = ", "
        ),
        call. = FALSE
      )
    }
    
    duplicate_keys <- object |>
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
        "stanoviste_trend(): `",
        object_name,
        "` nemá unikátní SITECODE × HABITAT_CODE.",
        call. = FALSE
      )
    }
  }
  
  # ---------------------------------------------------------------------------
  # 2. Sjednocení klíčů
  # ---------------------------------------------------------------------------
  
  normalize_keys <- function(x) {
    
    x |>
      dplyr::mutate(
        SITECODE = base::as.character(
          SITECODE
        ),
        HABITAT_CODE = base::as.character(
          HABITAT_CODE
        ),
        HABITAT_CODE = dplyr::case_when(
          HABITAT_CODE == "91" ~ "91E0",
          HABITAT_CODE == "9.10E+01" ~ "91E0",
          HABITAT_CODE == "9,10E+01" ~ "91E0",
          TRUE ~ HABITAT_CODE
        )
      )
  }
  
  current <- normalize_keys(
    current
  )
  
  previous <- normalize_keys(
    previous
  )
  
  # ---------------------------------------------------------------------------
  # 3. Indikátory dostupné v obou obdobích
  # ---------------------------------------------------------------------------
  
  indicators_common <- indicators[
    indicators %in% base::names(current) &
      indicators %in% base::names(previous)
  ]
  
  if (base::length(indicators_common) == 0) {
    base::stop(
      "stanoviste_trend(): current a previous nemají žádný společný ",
      "sledovaný indikátor.",
      call. = FALSE
    )
  }
  
  # ---------------------------------------------------------------------------
  # 4. Wide -> long
  # ---------------------------------------------------------------------------
  
  current_long <- current |>
    dplyr::select(
      dplyr::all_of(key_cols),
      dplyr::all_of(indicators_common)
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(
        indicators_common
      ),
      names_to = "parametr_nazev",
      values_to = "current_value",
      values_transform = base::list(
        current_value = base::as.character
      )
    )
  
  previous_long <- previous |>
    dplyr::select(
      dplyr::all_of(key_cols),
      dplyr::all_of(indicators_common)
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(
        indicators_common
      ),
      names_to = "parametr_nazev",
      values_to = "previous_value",
      values_transform = base::list(
        previous_value = base::as.character
      )
    )
  
  # ---------------------------------------------------------------------------
  # 5. Spojení období
  # ---------------------------------------------------------------------------
  
  detail <- current_long |>
    dplyr::full_join(
      previous_long,
      by = base::c(
        "SITECODE",
        "HABITAT_CODE",
        "parametr_nazev"
      )
    ) |>
    dplyr::mutate(
      current_num = base::suppressWarnings(
        base::as.numeric(
          current_value
        )
      ),
      previous_num = base::suppressWarnings(
        base::as.numeric(
          previous_value
        )
      )
    )
  
  # ---------------------------------------------------------------------------
  # 6. Výpočet trendu
  # ---------------------------------------------------------------------------
  #
  # Pořadí pravidel odpovídá smyslu původního case_when:
  #   1) chybějící období
  #   2) CELKOVE_HODNOCENI
  #   3) numerická stabilita ± tolerance
  #   4) KVALITA
  #   5) ROZLOHA
  #   6) ostatní indikátory bez definovaného směru
  
  detail <- detail |>
    dplyr::mutate(
      
      trend = dplyr::case_when(
        
        base::is.na(current_value) |
          base::is.na(previous_value) ~
          "neznámý",
        
        # ---------------------------------------------------------------
        # Celkové hodnocení
        # ---------------------------------------------------------------
        
        parametr_nazev ==
          "CELKOVE_HODNOCENI" &
          current_value ==
          previous_value ~
          "stabilní",
        
        parametr_nazev ==
          "CELKOVE_HODNOCENI" &
          previous_value == "dobrý" &
          current_value %in%
          base::c(
            "zhoršený",
            "špatný"
          ) ~
          "zhoršující se",
        
        parametr_nazev ==
          "CELKOVE_HODNOCENI" &
          previous_value == "zhoršený" &
          current_value == "špatný" ~
          "zhoršující se",
        
        parametr_nazev ==
          "CELKOVE_HODNOCENI" &
          previous_value == "špatný" &
          current_value %in%
          base::c(
            "zhoršený",
            "dobrý"
          ) ~
          "zlepšující se",
        
        parametr_nazev ==
          "CELKOVE_HODNOCENI" &
          previous_value == "zhoršený" &
          current_value == "dobrý" ~
          "zlepšující se",
        
        parametr_nazev ==
          "CELKOVE_HODNOCENI" ~
          "neznámý",
        
        # ---------------------------------------------------------------
        # Numerické indikátory - stabilita
        # ---------------------------------------------------------------
        
        !base::is.na(current_num) &
          !base::is.na(previous_num) &
          current_num ==
          previous_num ~
          "stabilní",
        
        !base::is.na(current_num) &
          !base::is.na(previous_num) &
          current_num <=
          previous_num *
          (1 + tolerance) &
          current_num >=
          previous_num *
          (1 - tolerance) ~
          "stabilní",
        
        # ---------------------------------------------------------------
        # KVALITA - nižší hodnota je lepší
        # ---------------------------------------------------------------
        
        parametr_nazev == "KVALITA" &
          !base::is.na(current_num) &
          !base::is.na(previous_num) &
          current_num >
          previous_num *
          (1 + tolerance) ~
          "zhoršující se",
        
        parametr_nazev == "KVALITA" &
          !base::is.na(current_num) &
          !base::is.na(previous_num) &
          current_num <
          previous_num *
          (1 - tolerance) ~
          "zlepšující se",
        
        # ---------------------------------------------------------------
        # ROZLOHA - vyšší hodnota je lepší
        # ---------------------------------------------------------------
        
        parametr_nazev == "ROZLOHA" &
          !base::is.na(current_num) &
          !base::is.na(previous_num) &
          current_num <
          previous_num *
          (1 - tolerance) ~
          "zhoršující se",
        
        parametr_nazev == "ROZLOHA" &
          !base::is.na(current_num) &
          !base::is.na(previous_num) &
          current_num >
          previous_num *
          (1 + tolerance) ~
          "zlepšující se",
        
        # ---------------------------------------------------------------
        # Textové indikátory
        # ---------------------------------------------------------------
        
        current_value ==
          previous_value ~
          "stabilní",
        
        # U ostatních změněných indikátorů starý skript neurčoval směr.
        TRUE ~
          "neznámý"
      )
    )
  
  # ---------------------------------------------------------------------------
  # 7. Trendy do wide formátu
  # ---------------------------------------------------------------------------
  
  trend_wide <- detail |>
    dplyr::select(
      SITECODE,
      HABITAT_CODE,
      parametr_nazev,
      trend
    ) |>
    tidyr::pivot_wider(
      names_from = parametr_nazev,
      values_from = trend,
      names_prefix = "TREND_"
    )
  
  result <- current |>
    dplyr::left_join(
      trend_wide,
      by = base::c(
        "SITECODE",
        "HABITAT_CODE"
      )
    )
  
  # ---------------------------------------------------------------------------
  # 8. Výstup
  # ---------------------------------------------------------------------------
  
  if (base::isTRUE(return_detail)) {
    
    return(
      base::list(
        result = result,
        detail = detail |>
          dplyr::select(
            SITECODE,
            HABITAT_CODE,
            parametr_nazev,
            previous_value,
            current_value,
            trend
          )
      )
    )
  }
  
  result
}
