# stanoviste_trend_workflow.R
#
# Orchestrace výpočtu trendu mezi aktuálním během nového workflow
# a historickým wide výstupem starého workflow.
#
# Očekává načtené funkce:
#   stanoviste_load_previous()
#   stanoviste_hodnoceni()
#   stanoviste_trend()
#
# `current_results` mají být SUROVÉ široké výsledky z:
#   stanoviste_eval()
#   nebo stanoviste_batch()
#
# `previous_source` může být např.:
#
#   "Outputs/Data/stanoviste/results_habitats_24_20250806.csv"
#
# nebo přímo:
#
#   "https://github.com/BiodivMonCZ/host_naturecz/blob/main/Outputs/Data/stanoviste/results_habitats_24_20250806.csv"
#
# Historické i aktuální období se nejdřív vyhodnotí stejnou funkcí
# stanoviste_hodnoceni(). Teprve potom se porovnají funkcí stanoviste_trend().
#
# Tím jsou obě období hodnocena podle stejné sady pravidel a limitů.

stanoviste_trend_workflow <- function(
    current_results,
    previous_source,
    limits,
    minimisize,
    site_context,
    sdo_ii_sites,
    tolerance = 0.05,
    return_components = FALSE
) {
  
  # ---------------------------------------------------------------------------
  # 1. Kontrola dostupnosti funkcí
  # ---------------------------------------------------------------------------
  
  required_functions <- base::c(
    "stanoviste_load_previous",
    "stanoviste_hodnoceni",
    "stanoviste_trend"
  )
  
  missing_functions <- required_functions[
    !base::vapply(
      required_functions,
      FUN = function(x) {
        base::exists(
          x,
          mode = "function",
          inherits = TRUE
        )
      },
      FUN.VALUE = base::logical(1)
    )
  ]
  
  if (base::length(missing_functions) > 0) {
    base::stop(
      "stanoviste_trend_workflow(): nejsou načteny funkce: ",
      base::paste(
        missing_functions,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  # ---------------------------------------------------------------------------
  # 2. Historický široký výstup
  # ---------------------------------------------------------------------------
  
  previous_raw <- stanoviste_load_previous(
    source = previous_source
  )
  
  # ---------------------------------------------------------------------------
  # 3. Hodnocení obou období stejnými pravidly
  # ---------------------------------------------------------------------------
  
  previous_evaluated <- stanoviste_hodnoceni(
    results = previous_raw,
    limits = limits,
    minimisize = minimisize,
    site_context = site_context,
    sdo_ii_sites = sdo_ii_sites
  )
  
  current_evaluated <- stanoviste_hodnoceni(
    results = current_results,
    limits = limits,
    minimisize = minimisize,
    site_context = site_context,
    sdo_ii_sites = sdo_ii_sites
  )
  
  # ---------------------------------------------------------------------------
  # 4. Trend
  # ---------------------------------------------------------------------------
  
  trend_result <- stanoviste_trend(
    current = current_evaluated,
    previous = previous_evaluated,
    tolerance = tolerance,
    return_detail = return_components
  )
  
  # ---------------------------------------------------------------------------
  # 5. Výstup
  # ---------------------------------------------------------------------------
  
  if (base::isTRUE(return_components)) {
    
    return(
      base::list(
        result = trend_result$result,
        trend_detail = trend_result$detail,
        current_evaluated = current_evaluated,
        previous_evaluated = previous_evaluated,
        previous_raw = previous_raw
      )
    )
  }
  
  trend_result
}
