# RUN_stanoviste.R
#
# Spouštěcí skript nového workflow hodnocení stanovišť.
#
# Existující R/00_config zůstává zdrojem dat.
# stanoviste_load_inputs.R jej načte do izolovaného prostředí a vytvoří
# strukturu očekávanou funkcemi stanoviste_eval() a stanoviste_batch().
#
# Interaktivně - jedna kombinace:
#
#   base::source("R/01_stanoviste/RUN_stanoviste.R")
#
#   x <- run_stanoviste_once(
#     site_code = "CZ...",
#     hab_code = "9130",
#     return_components = TRUE
#   )
#
#   x$result
#
# Interaktivně - batch:
#
#   targets <- tibble::tibble(
#     SITECODE = c("CZ...", "CZ..."),
#     HABITAT_CODE = c("9130", "6510")
#   )
#
#   batch <- run_stanoviste_batch(
#     targets = targets,
#     parallel = TRUE,
#     workers = 8
#   )
#
#   batch$results
#   batch$log
#
# Z příkazové řádky z kořene repozitáře:
#
#   Rscript R/01_stanoviste/RUN_stanoviste.R CZ... 9130


# =============================================================================
# 0. Kořen repozitáře
# =============================================================================

get_script_path <- function() {
  
  source_path <- base::tryCatch(
    base::sys.frame(1)$ofile,
    error = function(e) NULL
  )
  
  if (
    !base::is.null(source_path) &&
    base::length(source_path) == 1 &&
    base::nzchar(source_path)
  ) {
    return(
      base::normalizePath(
        source_path,
        winslash = "/",
        mustWork = TRUE
      )
    )
  }
  
  args_all <- base::commandArgs(
    trailingOnly = FALSE
  )
  
  file_arg <- base::grep(
    "^--file=",
    args_all,
    value = TRUE
  )
  
  if (base::length(file_arg) > 0) {
    return(
      base::normalizePath(
        base::sub(
          "^--file=",
          "",
          file_arg[[1]]
        ),
        winslash = "/",
        mustWork = TRUE
      )
    )
  }
  
  NULL
}

script_path <- get_script_path()

if (!base::is.null(script_path)) {
  
  repo_root <- base::normalizePath(
    base::file.path(
      base::dirname(script_path),
      "..",
      ".."
    ),
    winslash = "/",
    mustWork = TRUE
  )
  
} else {
  
  repo_root <- base::normalizePath(
    ".",
    winslash = "/",
    mustWork = TRUE
  )
}


# =============================================================================
# 1. Adresáře nového workflow
# =============================================================================

stanoviste_dir <- base::file.path(
  repo_root,
  "R",
  "01_stanoviste"
)

functions_dir <- base::file.path(
  stanoviste_dir,
  "functions"
)

orchestrator_dir <- base::file.path(
  stanoviste_dir,
  "orchestrator"
)


# =============================================================================
# 2. Input adapter
# =============================================================================

base::source(
  base::file.path(
    orchestrator_dir,
    "stanoviste_load_inputs.R"
  )
)


# =============================================================================
# 3. Výpočetní funkce
# =============================================================================

function_files <- base::c(
  "FUN_paseky_select_pairs.R",
  "FUN_paseky_spat.R",
  "FUN_paseky_sum.R",
  "FUN_stanoviste_paseky.R",
  "FUN_stanoviste_klicove_parametry.R",
  "FUN_stanoviste_druhy.R",
  "FUN_stanoviste_prostor.R",
  "FUN_stanoviste_batch.R"
)

function_paths <- base::file.path(
  functions_dir,
  function_files
)

missing_function_files <- function_paths[
  !base::file.exists(function_paths)
]

if (base::length(missing_function_files) > 0) {
  base::stop(
    "RUN_stanoviste.R: chybí funkční skripty: ",
    base::paste(
      missing_function_files,
      collapse = ", "
    ),
    call. = FALSE
  )
}

base::invisible(
  base::lapply(
    function_paths,
    FUN = base::source
  )
)


# =============================================================================
# 4. Validator a centrální orchestrátor
# =============================================================================

orchestrator_files <- base::c(
  "stanoviste_input_validator.R",
  "stanoviste_central_orchestrator.R"
)

orchestrator_paths <- base::file.path(
  orchestrator_dir,
  orchestrator_files
)

missing_orchestrator_files <- orchestrator_paths[
  !base::file.exists(orchestrator_paths)
]

if (base::length(missing_orchestrator_files) > 0) {
  base::stop(
    "RUN_stanoviste.R: chybí orchestrátorové skripty: ",
    base::paste(
      missing_orchestrator_files,
      collapse = ", "
    ),
    call. = FALSE
  )
}

base::invisible(
  base::lapply(
    orchestrator_paths,
    FUN = base::source
  )
)


# =============================================================================
# 5. Načtení všech vstupů
# =============================================================================

stanoviste_inputs <- load_stanoviste_inputs(
  repo_root = repo_root
)

base::message(
  "\nMapování vstupů nového workflow:"
)

base::print(
  stanoviste_inputs$manifest,
  n = base::Inf
)


# =============================================================================
# 6. Výpočet jedné kombinace site x habitat
# =============================================================================

run_stanoviste_once <- function(
    site_code,
    hab_code,
    return_components = FALSE
) {
  
  stanoviste_eval(
    hab_code = hab_code,
    site_code = site_code,
    data = stanoviste_inputs$data,
    tables = stanoviste_inputs$tables,
    return_components = return_components
  )
}


# =============================================================================
# 7. Batch výpočet site x habitat
# =============================================================================
#
# `targets` musí obsahovat minimálně:
#   SITECODE
#   HABITAT_CODE
#
# Společné vstupy se předávají z `stanoviste_inputs`; při batch běhu se tedy
# znovu nenačítají.

run_stanoviste_batch <- function(
    targets,
    parallel = TRUE,
    workers = NULL,
    parallel_plan = NULL,
    future_seed = TRUE,
    return_components = FALSE,
    stop_on_error = FALSE
) {
  
  stanoviste_batch(
    targets = targets,
    data = stanoviste_inputs$data,
    tables = stanoviste_inputs$tables,
    parallel = parallel,
    workers = workers,
    parallel_plan = parallel_plan,
    future_seed = future_seed,
    return_components = return_components,
    stop_on_error = stop_on_error
  )
}


# =============================================================================
# 8. Volitelné spuštění jedné kombinace z příkazové řádky
# =============================================================================
#
# CLI zatím zachovává původní jednoduchý režim:
#
#   Rscript R/01_stanoviste/RUN_stanoviste.R <site_code> <hab_code>
#
# Batch se spouští přes run_stanoviste_batch(). Až bude definitivní zdroj
# target tabulky, lze CLI rozšířit i o batch režim.

args <- base::commandArgs(
  trailingOnly = TRUE
)

if (
  !base::interactive() &&
  base::length(args) > 0
) {
  
  if (base::length(args) != 2) {
    base::stop(
      "Použití: Rscript R/01_stanoviste/RUN_stanoviste.R ",
      "<site_code> <hab_code>",
      call. = FALSE
    )
  }
  
  result <- run_stanoviste_once(
    site_code = args[[1]],
    hab_code = args[[2]]
  )
  
  base::print(result)
}
