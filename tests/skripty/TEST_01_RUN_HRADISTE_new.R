### NEW

# Spoustet z korene repozitare host_naturecz_fork:
# Rscript /cesta/k/01_RUN_Hradiste_NEW.R
# Vyuziva aktualni RUN_stanoviste.R bez uprav vypocetnich funkci.

repo_root <- base::normalizePath('.', mustWork = TRUE)
runner <- base::file.path(repo_root, 'R/01_stanoviste/RUN_stanoviste.R')
if (!base::file.exists(runner)) base::stop('Spustte skript z korene host_naturecz_fork.')

base::source(runner, local = base::globalenv(), chdir = FALSE)

# Predmety ochrany prebiráme z oficialniho seznamu nacteneho konfiguraci.
config <- stanoviste_inputs$config_env
if (base::is.null(config) || !base::exists('sites_habitats', envir = config, inherits = FALSE)) {
  base::stop('V configu chybi sites_habitats.')
}
all_targets <- base::get('sites_habitats', envir = config)
need <- base::c('site_code', 'site_name', 'feature_code')
if (!base::all(need %in% base::names(all_targets))) base::stop('V sites_habitats chybi pozadovane sloupce.')

hr <- all_targets |>
  dplyr::filter(base::trimws(base::as.character(site_name)) == 'Hradiště') |>
  dplyr::transmute(SITECODE = base::as.character(site_code),
                   HABITAT_CODE = base::as.character(feature_code)) |>
  dplyr::distinct()
if (base::nrow(hr) == 0L) base::stop('EVL Hradiště nenalezena v sites_habitats.')
if (base::length(base::unique(hr$SITECODE)) != 1L) base::stop('Nazev Hradiště odpovida vice SITECODE.')

out_dir <- base::file.path(repo_root, 'Outputs', 'Test', 'Hradiste', 'NEW')
base::dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
readr::write_csv(hr, base::file.path(out_dir, 'targets.csv'))

# Pro prime porovnani surovych vypoctu nepocitame trend.
# stop_on_error = FALSE ulozi chyby jednotlivych kombinaci do batch logu.
result <- run_stanoviste_workflow(
  targets = hr,
  calculate_trend = FALSE,
  output_dir = out_dir,
  period_id = base::format(base::Sys.Date(), '%y'),
  assessment_year = base::as.integer(base::format(base::Sys.Date(), '%Y')),
  write_results = TRUE,
  overwrite = TRUE,
  parallel = FALSE,
  stop_on_error = FALSE
)
base::saveRDS(result, base::file.path(out_dir, 'workflow_result.rds'))
if (!base::is.null(result$batch_log)) {
  readr::write_csv(result$batch_log, base::file.path(out_dir, 'batch_log.csv'))
  if (base::any(result$batch_log$status != 'ok')) {
    base::warning('Nektere kombinace selhaly. Viz batch_log.csv.')
  }
}
base::message('NEW dokonceno: ', out_dir)
