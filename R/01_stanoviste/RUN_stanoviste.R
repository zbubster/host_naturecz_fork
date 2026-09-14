# RUN_stanoviste.R
#
# Spouštěcí skript workflow pro jednu kombinaci site x habitat.
#
# Existující R/00_config zůstává zdrojem dat.
# stanoviste_load_inputs.R jej načte do izolovaného prostředí a vytvoří
# strukturu očekávanou funkcí stanoviste_eval().
#
# Spusteni Interaktivne | Terminal
#
# Interaktivně:
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
# Z příkazové řádky z kořene repozitáře:
#
#   Rscript R/01_stanoviste/RUN_stanoviste.R CZ... 9130


# =============================================================================
# 0. Kořen repozitáře
# =============================================================================

get_script_path <- function() {
  
  # source(...)
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
  
  # Rscript ...
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
  
  # RUN_stanoviste.R leží v:
  #   <repo>/R/01_stanoviste/RUN_stanoviste.R
  #
  # takže kořen repozitáře je o dvě úrovně výš.
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
  
  # Fallback pro nestandardní interaktivní spuštění.
  # Předpokládá, že working directory je kořen repozitáře.
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
  "FUN_stanoviste_prostor.R"
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
#
# stanoviste_load_inputs.R načte existující configy:
#
#   R/00_config/00_n2k_config.R
#   R/00_config/02_n2k_data_druhy.R
#   R/00_config/03_n2k_data_stanoviste.R
#   R/00_config/load_vmb.R
#
# a převede jejich objekty do:
#
#   stanoviste_inputs$data
#   stanoviste_inputs$tables

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
# 7. Volitelné spuštění z příkazové řádky
# =============================================================================

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
