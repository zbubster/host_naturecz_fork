typ_chu <- "mzchu"
parallel <- FALSE
workers <- 2L
parallel_plan <- "multisession"

test_dir <- base::file.path(
  "Outputs", "Data", "stanoviste", "mzchu", "test_10_mzchu"
)
paseky_test_dir <- base::file.path(test_dir, "paseky")
base::dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)

base::source("R/01_stanoviste/RUN_paseky.R")
paseky_inputs <- get_paseky_inputs(typ_chu = typ_chu)

preferovane_biotopy <- base::c(
  "L3.4", "L3.1", "L2.2", "L5.1", "L6.4", "L4",
  "T1.1", "T5.2", "T3.4", "R2.2", "K3", "S1.2"
)

kandidati <- paseky_inputs$targets |>
  dplyr::mutate(
    SITECODE = base::as.character(SITECODE),
    HABITAT_CODE = base::as.character(HABITAT_CODE)
  ) |>
  dplyr::filter(
    SITECODE %in% base::as.character(paseky_inputs$data$site$SITECODE),
    base::grepl("^[A-Z][0-9]", HABITAT_CODE)
  ) |>
  dplyr::mutate(
    FSB = base::substr(HABITAT_CODE, 1, 1),
    skupina = dplyr::if_else(FSB == "L", "lesni", "nelesni"),
    priorita = base::match(HABITAT_CODE, preferovane_biotopy),
    priorita = dplyr::coalesce(priorita, 999L)
  ) |>
  dplyr::filter(FSB %in% base::c("L", "T", "R", "K", "S", "M", "V", "P")) |>
  dplyr::distinct(SITECODE, HABITAT_CODE, .keep_all = TRUE)

lesni_ruzne <- kandidati |>
  dplyr::filter(FSB == "L") |>
  dplyr::arrange(priorita, HABITAT_CODE, SITECODE) |>
  dplyr::distinct(HABITAT_CODE, .keep_all = TRUE) |>
  dplyr::distinct(SITECODE, .keep_all = TRUE)

lesni <- dplyr::bind_rows(
  lesni_ruzne,
  kandidati |>
    dplyr::filter(FSB == "L") |>
    dplyr::arrange(priorita, HABITAT_CODE, SITECODE)
) |>
  dplyr::distinct(SITECODE, .keep_all = TRUE) |>
  dplyr::slice_head(n = 5L)

nelesni_ruzne <- kandidati |>
  dplyr::filter(FSB != "L", !SITECODE %in% lesni$SITECODE) |>
  dplyr::arrange(priorita, FSB, HABITAT_CODE, SITECODE) |>
  dplyr::distinct(FSB, .keep_all = TRUE)

nelesni <- dplyr::bind_rows(
  nelesni_ruzne,
  kandidati |>
    dplyr::filter(FSB != "L", !SITECODE %in% lesni$SITECODE) |>
    dplyr::arrange(priorita, FSB, HABITAT_CODE, SITECODE)
) |>
  dplyr::distinct(SITECODE, .keep_all = TRUE) |>
  dplyr::slice_head(n = 5L)

zakladni_vyber <- dplyr::bind_rows(lesni, nelesni)

if (base::nrow(lesni) != 5L || base::nrow(nelesni) != 5L) {
  base::stop("Ve vstupnich datech neni 5 lesnich a 5 dalsich nelesnich MZCHU.")
}

dalsi_biotopy <- kandidati |>
  dplyr::filter(SITECODE %in% zakladni_vyber$SITECODE) |>
  dplyr::anti_join(
    zakladni_vyber,
    by = base::c("SITECODE", "HABITAT_CODE")
  ) |>
  dplyr::left_join(
    zakladni_vyber |>
      dplyr::select(SITECODE, puvodni_skupina = skupina),
    by = "SITECODE"
  ) |>
  dplyr::mutate(jina_skupina = skupina != puvodni_skupina) |>
  dplyr::arrange(SITECODE, dplyr::desc(jina_skupina), priorita) |>
  dplyr::group_by(SITECODE) |>
  dplyr::slice_head(n = 1L) |>
  dplyr::ungroup() |>
  dplyr::select(dplyr::all_of(base::names(zakladni_vyber)))

vyber <- dplyr::bind_rows(zakladni_vyber, dalsi_biotopy) |>
  dplyr::distinct(SITECODE, HABITAT_CODE, .keep_all = TRUE)

targets <- vyber |>
  dplyr::select(SITECODE, HABITAT_CODE)

base::stopifnot(base::length(base::unique(targets$SITECODE)) == 10L)
base::print(vyber)
readr::write_csv2(vyber, base::file.path(test_dir, "vybrane_kombinace.csv"))

# Paseky

paseky_test <- run_paseky_workflow(
  typ_chu = typ_chu,
  targets = targets,
  output_dir = paseky_test_dir,
  parallel = parallel,
  workers = workers,
  parallel_plan = parallel_plan,
  stop_on_error = TRUE,
  write_results = TRUE,
  overwrite_archive = TRUE
)

base::print(dplyr::count(paseky_test$log, status))
base::stopifnot(
  base::nrow(paseky_test$result) == base::nrow(targets),
  base::all(paseky_test$log$status == "ok")
)

# Hlavni workflow

base::source("R/01_stanoviste/RUN_stanoviste.R")

stanoviste_inputs <- load_stanoviste_inputs(
  repo_root = ".",
  typ_chu = typ_chu,
  paseky_file = base::file.path(paseky_test_dir, "paseky_results_latest.csv")
)

# DOCASNA OPRAVA ZNAMEHO PROBLEMU:
# Config vraci minimisize po HABITAT a prevedene na ha.
# Pro ceske biotopy potrebujeme tabulku po BIOTOP a hodnoty v m2.
# Oprava plati pouze v pameti tohoto testu, nemeni soubory repozitare.
minimi_raw <- readr::read_csv(
  "Data/Input/minimisize.csv",
  locale = readr::locale(encoding = "Windows-1250"),
  show_col_types = FALSE
)

stanoviste_inputs$tables$minimisize <- minimi_raw |>
  dplyr::group_by(HABITAT, BIOTOP) |>
  dplyr::summarise(
    MINIMISIZE = base::max(MINIMISIZE, na.rm = TRUE),
    .groups = "drop"
  )

# Runner pouziva cached inputs: vlozime do nej testovaci paseky a minimisize.
.stanoviste_inputs_cache[[typ_chu]] <- stanoviste_inputs

stanoviste_test <- run_stanoviste_workflow(
  targets = targets,
  typ_chu = typ_chu,
  calculate_trend = FALSE,
  output_dir = test_dir,
  write_results = TRUE,
  overwrite = TRUE,
  parallel = parallel,
  workers = workers,
  parallel_plan = parallel_plan,
  stop_on_error = FALSE
)

# Kontrola

base::print(dplyr::count(stanoviste_test$batch_log, status))
base::print(
  dplyr::filter(stanoviste_test$batch_log, status != "ok"),
  n = Inf
)

vysledky <- stanoviste_test$raw

kontrola <- vysledky |>
  dplyr::select(
    dplyr::any_of(base::c(
      "TYP_CHU", "SITECODE", "NAZEV", "HABITAT_CODE", "ROZLOHA",
      "KVALITA", "MINIMIAREAL", "MOZAIKA_FIN", "MRTVE_DREVO",
      "ROZLOHA_PASEKY", "ROZLOHA_HOLINY", "POCET_SEGMENTU_PASEKY"
    ))
  )

base::print(kontrola, n = Inf, width = Inf)
readr::write_csv2(kontrola, base::file.path(test_dir, "kontrola_vysledku.csv"))

base::stopifnot(
  base::nrow(vysledky) == base::nrow(targets),
  base::all(vysledky$TYP_CHU == "MZCHU"),
  base::all(stanoviste_test$batch_log$status == "ok")
)

base::message("Test hotovy: ", test_dir)
