base::source("R/01_stanoviste/RUN_stanoviste.R")

targets_test <- tibble::tribble(
  ~SITECODE,   ~HABITAT_CODE,
  "CZ0414127", "3150",   # Hradiště - nelesní
  "CZ0414127", "6210",   # Hradiště - nelesní
  "CZ0414127", "9130",   # Hradiště - lesní, paseky
  "CZ0414127", "9180",   # Hradiště - lesní, paseky
  "CZ0210101", "3150"    # Dymokursko - druhá lokalita
)

test_run <- run_stanoviste_batch(
  targets = targets_test,
  parallel = FALSE,
  return_components = TRUE,
  stop_on_error = FALSE
)

test_run$log

test_run$results

test_run$results |>
  dplyr::select(
    SITECODE,
    NAZEV,
    HABITAT_CODE,
    ROZLOHA,
    KVALITA,
    TYPICKE_DRUHY,
    MINIMIAREAL,
    MOZAIKA_VNEJSI,
    MOZAIKA_VNITRNI,
    RED_LIST,
    INVASIVE,
    EXPANSIVE,
    dplyr::any_of(
      base::c(
        "MRTVE_DREVO",
        "KALAMITA_POLOM"
      )
    )
  )

################################################################################

## md_seg edits

base::source(
  "R/01_stanoviste/functions/FUN_stanoviste_klicove_parametry.R"
)

################################################################################

# large parallel batch

targets_large <- tibble::tribble(
  ~SITECODE,   ~HABITAT_CODE,
  
  # Hradiště - známá reference
  "CZ0414127", "3150",
  "CZ0414127", "40A0",
  "CZ0414127", "6210",
  "CZ0414127", "9130",
  "CZ0414127", "9180",
  "CZ0414127", "91E0",
  "CZ0414127", "91I0",
  
  # další nelesní habitaty
  "CZ0210101", "3150",
  "CZ0214017", "3260",
  "CZ0110049", "4030",
  "CZ0210011", "5130",
  "CZ0110040", "6210",
  "CZ0210058", "6230",
  "CZ0110142", "6410",
  "CZ0210056", "6430",
  "CZ0210023", "6510",
  "CZ0210054", "7110",
  "CZ0210003", "7140",
  "CZ0210100", "7220",
  "CZ0210008", "7230",
  "CZ0524044", "8110",
  "CZ0110154", "8220",
  "CZ0210044", "8230",
  "CZ0214002", "8310",
  
  # lesní habitaty
  "CZ0210027", "9110",
  "CZ0210028", "9130",
  "CZ0210105", "9150",
  "CZ0114001", "9170",
  "CZ0110040", "9180",
  "CZ0210100", "91E0",
  "CZ0210008", "91F0",
  "CZ0210010", "91I0",
  "CZ0314021", "91T0"
)

################################################################################
# multicore

# set maximim worker RAm to 20 GB

getOption("future.globals.maxSize")
getOption("future.globals.maxSize") / 1024^3
options(
  future.globals.maxSize = 20 * 1024^3
)

# run

test_large <- run_stanoviste_batch(
  targets = targets_large,
  parallel = TRUE,
  workers = 2,
  parallel_plan = "multicore",
  return_components = FALSE,
  stop_on_error = FALSE
)

# set worker RAM back to default (500 MB)

options(future.globals.maxSize = NULL)

# check results

test_large$log$error_message
test_large$log
test_large$log |>
  dplyr::count(status)
test_large$log |>
  dplyr::filter(status != "ok")

################################################################################
# multisession


base::source(
  "R/01_stanoviste/functions/FUN_stanoviste_batch.R"
)

# set maximim worker RAm to 20 GB

getOption("future.globals.maxSize")
getOption("future.globals.maxSize") / 1024^3
options(
  future.globals.maxSize = 20 * 1024^3
)

test_large_ms <- run_stanoviste_batch(
  targets = targets_large,
  parallel = TRUE,
  workers = 2,
  parallel_plan = "multisession",
  return_components = FALSE,
  stop_on_error = FALSE
)

# set worker RAM back to default (500 MB)

options(future.globals.maxSize = NULL)

# check results

test_large_ms$log |>
  dplyr::count(status)

################################################################################

# porovnani multicore miltisession

dplyr::all_equal(
  test_large$results,
  test_large_ms$results
)


################################################################################

# cele workflow

# reference pro trend
reference_raw <- test_large_ms$results |>
  dplyr::select(
    -dplyr::any_of(
      base::c(
        ".target_id",
        "SITECODE_TARGET",
        "HABITAT_CODE_TARGET"
      )
    )
  )

# set maximim worker RAm to 20 GB

getOption("future.globals.maxSize")
getOption("future.globals.maxSize") / 1024^3
options(
  future.globals.maxSize = 20 * 1024^3
)

workflow_test <- run_stanoviste_workflow(
  targets = targets_large,
  previous_results = reference_raw,
  calculate_trend = TRUE,
  period_id = "test",
  assessment_year = 2026,
  output_dir = base::file.path(
    repo_root,
    "Outputs",
    "Data",
    "stanoviste",
    "workflow_test"
  ),
  write_results = TRUE,
  overwrite = TRUE,
  parallel = TRUE,
  workers = 2,
  return_components = TRUE,
  stop_on_error = TRUE
)

# set worker RAM back to default (500 MB)

options(future.globals.maxSize = NULL)

# check results

workflow_test$batch_log
workflow_test$export
workflow_test$result |>
  dplyr::select(
    SITECODE,
    HABITAT_CODE,
    dplyr::starts_with("TREND_")
  )
workflow_test$batch_log |>
  dplyr::count(status)
workflow_test$result |>
  dplyr::select(
    SITECODE,
    HABITAT_CODE,
    dplyr::starts_with("TREND_")
  )
trend_values <- workflow_test$result |>
  dplyr::select(
    dplyr::starts_with("TREND_")
  ) |>
  base::unlist(
    use.names = FALSE
  ) |>
  base::unique()

trend_values
base::stopifnot(
  base::all(
    stats::na.omit(trend_values) == "stabilní"
  )
)
