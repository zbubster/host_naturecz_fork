### OLD

# Spoustet z korene repozitare host_naturecz_fork:
# Rscript /cesta/k/02_RUN_Hradiste_OLD.R
# Puvodni tela funkci se z legacy skriptu pouze nactou, neprepisuji se.
# Zamerne se NESOURCUJE cely historicky soubor, protoze obsahuje
# neomezene smycky pro vsechny EVL a pevne cesty pro drivejsi exporty.

repo_root <- base::normalizePath('.', mustWork = TRUE)
loader <- base::file.path(repo_root, 'R/01_stanoviste/orchestrator/stanoviste_load_inputs.R')
if (!base::file.exists(loader)) base::stop('Spustte skript z korene host_naturecz_fork.')
base::source(loader, local = base::globalenv())
inputs <- load_stanoviste_inputs(repo_root = repo_root, keep_config_env = TRUE)
config <- inputs$config_env

# Historicke funkce spolehaji na globalni objekty z puvodni konfigurace.
for (nm in base::ls(config, all.names = TRUE)) {
  base::assign(nm, base::get(nm, envir = config, inherits = FALSE), envir = base::globalenv())
}
# Legacy nazvy objektu; hodnoty data/paseky jsou sjednoceny s NEW behom.
data_akt <- base::get('load_vmb', envir = config)(vmb_x = 0)
vmb_shp_sjtsk_akt <- data_akt$vmb_shp_sjtsk_akt
vmb_pb_x_akt <- data_akt$vmb_pb_x_akt
paseky <- inputs$tables$paseky
red_list_species <- redlist_species
if (!base::exists('evl_sjtsk')) evl_sjtsk <- sf::st_transform(evl, 5514)
if (!base::exists('czechia_line') && base::exists('czechia')) {
  czechia_line <- sf::st_boundary(sf::st_union(sf::st_geometry(czechia)))
}

# Nacti z historickeho souboru pouze definici puvodni funkce.
load_legacy_function <- function(path, fun_name) {
  exprs <- base::parse(path, keep.source = FALSE)
  hits <- base::Filter(function(e) {
    base::is.call(e) && base::identical(e[[1]], base::as.name('<-')) &&
      base::identical(e[[2]], base::as.name(fun_name))
  }, base::as.list(exprs))
  if (base::length(hits) != 1L) base::stop('Funkce ', fun_name, ' nenalezena jednoznacne v ', path)
  base::eval(hits[[1]], envir = base::globalenv())
}
legacy_dir <- base::file.path(repo_root, 'R/01_stanoviste')
load_legacy_function(base::file.path(legacy_dir, '11_n2k_stanoviste_klic.R'), 'n2k_hab_klic')
load_legacy_function(base::file.path(legacy_dir, 'n2k_stanoviste_prostor.R'), 'hvezdice_eval')
load_legacy_function(base::file.path(legacy_dir, 'n2k_stanoviste_druhy.R'), 'n2k_hab_druhy')

hr <- sites_habitats |>
  dplyr::filter(base::trimws(base::as.character(site_name)) == 'Hradiště') |>
  dplyr::transmute(SITECODE = base::as.character(site_code),
                   HABITAT_CODE = base::as.character(feature_code)) |>
  dplyr::distinct()
if (base::nrow(hr) == 0L || base::length(base::unique(hr$SITECODE)) != 1L) {
  base::stop('EVL Hradiště nenalezena nebo nejednoznacna.')
}
out_dir <- base::file.path(repo_root, 'Outputs', 'Test', 'Hradiste', 'OLD')
base::dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
readr::write_csv(hr, base::file.path(out_dir, 'targets.csv'))

stages <- base::list(
  klic = function(site, hab) n2k_hab_klic(hab_code = hab, evl_site = site),
  prostor = function(site, hab) hvezdice_eval(hab_code = hab, evl_site = site),
  druhy = function(site, hab) n2k_hab_druhy(hab_code = hab, evl_site = site, typ_chu = 'EVL')
)
logs <- base::list()
for (stage in base::names(stages)) {
  rows <- base::list()
  for (i in base::seq_len(base::nrow(hr))) {
    site <- hr$SITECODE[[i]]
    hab <- hr$HABITAT_CODE[[i]]
    base::message(stage, ' ', i, '/', base::nrow(hr), ': ', site, ' / ', hab)
    ans <- base::tryCatch(stages[[stage]](site, hab), error = function(e) e)
    ok <- !base::inherits(ans, 'error')
    logs[[base::length(logs) + 1L]] <- tibble::tibble(
      stage = stage, SITECODE = site, HABITAT_CODE = hab,
      status = if (ok) 'ok' else 'error',
      error_message = if (ok) NA_character_ else base::conditionMessage(ans))
    if (ok) {
      row <- base::as.data.frame(ans)
      if (!'SITECODE' %in% base::names(row)) row$SITECODE <- site
      if (!'HABITAT_CODE' %in% base::names(row)) row$HABITAT_CODE <- hab
      rows[[base::length(rows) + 1L]] <- row
    }
  }
  combined <- dplyr::bind_rows(rows)
  readr::write_csv(combined, base::file.path(out_dir, base::paste0('legacy_', stage, '.csv')))
  base::saveRDS(combined, base::file.path(out_dir, base::paste0('legacy_', stage, '.rds')))
}
log_tbl <- dplyr::bind_rows(logs)
readr::write_csv(log_tbl, base::file.path(out_dir, 'batch_log.csv'))
if (base::any(log_tbl$status != 'ok')) base::warning('Nektere legacy vypocty selhaly. Viz batch_log.csv.')
base::message('OLD mezivysledky ulozeny: ', out_dir)
base::message('POZOR: Historicke n2k_stanoviste_srovnani.R a n2k_stanoviste_zapis.R ' ,
              'nejsou automaticky spusteny: pracuji s pevnymi soubory, vsemi EVL a historickymi lety.')
