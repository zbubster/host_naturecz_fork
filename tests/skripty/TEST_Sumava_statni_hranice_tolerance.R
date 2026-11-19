# TEST Sumava statni hranice

# TEST_Sumava_tolerance_statni_hranice.R
# ================================================================
# CIL: Diagnostika nesouladu obvodu stanovist 9410/9110 a hranice CR.
# Bez zasahu do produkcniho workflow a bez snappovani geometrii.
#
# Spusteni z korene repozitare host_naturecz_fork:
#   Rscript tests/skripty/TEST_Sumava_tolerance_statni_hranice.R
# nebo v RStudiu:
#   base::source('tests/skripty/TEST_Sumava_tolerance_statni_hranice.R')
#
# Vystupy: Outputs/Test/Sumava/TEST_TOLERANCE_HRANICE/
#   tolerance_delky.csv          - delky obvodu v pasmech [m] a podily
#   prirustky_intervalu.csv      - prirustky mezi tolerancemi [m]
#   souhrn_stanovist.csv         - prehled obvodu, presne shody, 10m
#   test_log.csv                 - stav, pripadne chyby
#   hranice_9410.gpkg           - prostorove vrstvy pro QGIS
#   hranice_9110.gpkg
#
# PODSTATNE: Prunik obvodu s BUFFEREM statni hranice neni
# automaticky skutecna spolecna hranice; je to pouze prostorova
# tolerance (pro diagnostiku). Hodnoty se nepouziji v produkcnim
# vzorci MOZAIKA_VNEJSI a zadne objekty VMB se nemeni.
# ================================================================

# 1. NASTAVENI --------------------------------------------------
site_code <- 'CZ0314024'  # EVL Sumava
habitat_codes <- base::c('9410', '9110')
habitat_col <- 'HABITAT'
tolerances_m <- base::c(0, 0.1, 0.5, 1, 2, 5, 10)
site_buffer_m <- 500
max_example_segments <- 20L

# 2. NACITANI STEJNYCH VSTUPU JAKO NEW ---------------------------
repo_root <- base::normalizePath('.', winslash = '/', mustWork = TRUE)
runner <- base::file.path(repo_root, 'R/01_stanoviste/RUN_stanoviste.R')
if (!base::file.exists(runner)) {
  base::stop('Spustte test z korene repozitare host_naturecz_fork.')
}
base::source(runner, local = base::globalenv(), chdir = FALSE)

vmb <- stanoviste_inputs$data$vmb
site <- stanoviste_inputs$data$site
state <- stanoviste_inputs$data$czechia_line
if (!base::inherits(vmb, 'sf') || !base::inherits(site, 'sf')) {
  base::stop('Vstupy VMB a SITE musi byt sf objekty.')
}
if (!habitat_col %in% base::names(vmb) ||
    !base::all(base::c('FSB_EVAL', 'SEGMENT_ID') %in% base::names(vmb))) {
  base::stop('Chybi sloupce HABITAT/FSB_EVAL/SEGMENT_ID v aktualni VMB.')
}
if (sf::st_is_longlat(vmb)) base::stop('VMB neni v projektovanem CRS v metrech.')
crs <- sf::st_crs(vmb)
if (!base::tolower(crs$units_gdal) %in% base::c('metre', 'meter', 'm')) {
  base::stop('Pro tolerance v metrech je potreba metricky CRS.')
}

out_dir <- base::file.path(repo_root, 'Outputs', 'Test', 'Sumava',
                           'TEST_TOLERANCE_HRANICE')
base::dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# 3. POMOCNE FUNKCE ---------------------------------------------
empty_lines <- function() sf::st_sfc(crs = crs)

as_geom <- function(x) {
  if (base::inherits(x, 'sf')) return(sf::st_geometry(x))
  x
}

line_length <- function(x) {
  x <- as_geom(x)
  if (base::length(x) == 0L) return(0)
  x <- x[!sf::st_is_empty(x)]
  if (base::length(x) == 0L) return(0)
  base::sum(units::drop_units(sf::st_length(x)), na.rm = TRUE)
}

line_intersection <- function(x, y) {
  x <- as_geom(x)
  y <- as_geom(y)
  if (base::length(x) == 0L || base::length(y) == 0L) return(empty_lines())
  base::suppressWarnings(sf::st_intersection(x, y))
}

line_difference <- function(x, y) {
  x <- as_geom(x)
  y <- as_geom(y)
  if (base::length(x) == 0L) return(empty_lines())
  if (base::length(y) == 0L) return(x)
  base::suppressWarnings(sf::st_difference(x, sf::st_union(y)))
}

# Prevede vysledky na liniove casti; body, ktere maji nulovou delku,
# se pri exportu ignoruji. Nezasahuje do poloh ani hranic.
only_lines <- function(x) {
  x <- as_geom(x)
  if (base::length(x) == 0L) return(empty_lines())
  x <- sf::st_collection_extract(x, 'LINESTRING', warn = FALSE)
  if (base::length(x) == 0L) return(empty_lines())
  x[!sf::st_is_empty(x)]
}

write_layer <- function(x, gpkg, layer, attrs = NULL) {
  g <- only_lines(x)
  if (base::length(g) == 0L) return(base::invisible(FALSE))
  if (base::is.null(attrs)) {
    data <- sf::st_sf(geometry = g)
  } else {
    if (base::nrow(attrs) != base::length(g)) {
      base::stop('Nesouhlasici pocet atributu pro vrstvu ', layer)
    }
    data <- sf::st_sf(attrs, geometry = g)
  }
  sf::st_write(data, gpkg, layer = layer,
               delete_layer = TRUE, quiet = TRUE)
  base::invisible(TRUE)
}

# 4. VYBER EVL, OBCI A STATNI HRANICE ----------------------------
site_sf <- site |>
  dplyr::filter(.data$SITECODE == site_code)
if (base::nrow(site_sf) == 0L) base::stop('Neni nalezena EVL ', site_code)
if (!base::isTRUE(sf::st_crs(site_sf) == crs)) {
  site_sf <- sf::st_transform(site_sf, crs)
}
site_geom <- sf::st_make_valid(sf::st_union(sf::st_geometry(site_sf)))
site_buffer <- sf::st_buffer(site_geom, dist = site_buffer_m)

state_geom <- as_geom(state)
if (!base::isTRUE(sf::st_crs(state_geom) == crs)) {
  state_geom <- sf::st_transform(state_geom, crs)
}
state_geom <- sf::st_union(state_geom)
state_local <- line_intersection(state_geom, site_buffer)
state_local <- only_lines(state_local)
if (base::length(state_local) == 0L) {
  base::stop('Statni hranice se nedotyka okoli EVL; zkontrolujte CRS/vstupy.')
}
state_local <- sf::st_union(state_local)
# PREDPOCITEJ pasma jednou, pro oba habitaty.
tolerance_buffers <- base::lapply(tolerances_m[-1L], function(d) {
  sf::st_buffer(state_local, dist = d)
})
base::names(tolerance_buffers) <- base::as.character(tolerances_m[-1L])

prepare_boundary <- function(hab_code) {
  target <- vmb |>
    dplyr::filter(.data[[habitat_col]] == hab_code) |>
    sf::st_filter(site_geom) |>
    sf::st_intersection(site_geom) |>
    sf::st_make_valid() |>
    sf::st_collection_extract('POLYGON', warn = FALSE) |>
    dplyr::mutate(.area_m2 = units::drop_units(sf::st_area(geometry))) |>
    dplyr::filter(.area_m2 > 0,
                  !base::is.na(FSB_EVAL), FSB_EVAL != 'X')
  if (base::nrow(target) == 0L) {
    base::stop('Zadne hodnotitelne polygony stanoviste ', hab_code)
  }
  target_geom <- sf::st_make_valid(sf::st_union(sf::st_geometry(target)))
  base::list(boundary = sf::st_boundary(target_geom), n = base::nrow(target))
}

# 5. VLASTNI DIAGNOSTIKA ----------------------------------------
calculate_habitat <- function(hab_code) {
  base::message('\nZpracovavam stanoviste ', hab_code)
  target <- prepare_boundary(hab_code)
  target_boundary <- target$boundary
  border_all_m <- line_length(target_boundary)
  if (border_all_m <= 0) base::stop('Obvod stanoviste ma nulovou delku.')
  
  # Rovnost geometrii (0 m), potom jednotlive prostorove tolerance.
  within_geom <- base::vector('list', base::length(tolerances_m))
  within_geom[[1L]] <- only_lines(line_intersection(target_boundary, state_local))
  for (k in base::seq_along(tolerance_buffers)) {
    within_geom[[k + 1L]] <- only_lines(line_intersection(
      target_boundary, tolerance_buffers[[k]]
    ))
  }
  base::names(within_geom) <- base::as.character(tolerances_m)
  lengths <- base::vapply(within_geom, line_length, base::numeric(1))
  near_percent <- lengths / border_all_m * 100
  
  measured <- dplyr::tibble(
    SITECODE = site_code,
    HABITAT_CODE = hab_code,
    tolerance_m = tolerances_m,
    border_all_m = border_all_m,
    border_in_tolerance_m = base::as.numeric(lengths),
    percent_of_total = base::as.numeric(near_percent),
    remaining_border_m = border_all_m - base::as.numeric(lengths),
    # POUZE citlivost jmenovatele (nikoliv nova mozaika):
    known_border_ratio = (border_all_m - base::as.numeric(lengths)) / border_all_m
  )
  
  increment <- dplyr::tibble(
    SITECODE = site_code,
    HABITAT_CODE = hab_code,
    interval_od_m = tolerances_m[-base::length(tolerances_m)],
    interval_do_m = tolerances_m[-1L],
    # Rozdil delkovych ukazatelu; geometricke vrstvy pro kontrolu nize.
    prirustek_delky_m = base::diff(base::as.numeric(lengths))
  )
  if (base::any(increment$prirustek_delky_m < -1e-5)) {
    base::warning('Nenavazujici delky pasem u ', hab_code,
                  '. Proverte validitu geometrii; nepricitejte zaporne rozdily.')
  }
  
  gpkg <- base::file.path(out_dir, base::paste0('hranice_', hab_code, '.gpkg'))
  if (base::file.exists(gpkg)) base::file.remove(gpkg)
  
  # Stavajici obvod, statni hranice a presne shodne useky.
  write_layer(target_boundary, gpkg, 'obvod_stanoviste')
  write_layer(state_local, gpkg, 'hranice_CR_v_okoli')
  write_layer(within_geom[[1L]], gpkg, 'presna_shoda_0m')
  
  # Kazda vrstva predstavuje cast obvodu v pasmu od (prev) do (current).
  # Tolerance nazvy obsahují p misto desetinne tecky.
  for (k in base::seq.int(2L, base::length(tolerances_m))) {
    to <- tolerances_m[[k]]
    from <- tolerances_m[[k - 1L]]
    suffix <- base::gsub('\\.', 'p', base::as.character(to))
    all_in_band <- within_geom[[k]]
    write_layer(all_in_band, gpkg,
                base::paste0('v_pasu_do_', suffix, 'm'))
    
    # Mimo predchozi pasmo; geometrii neupravujeme tolerancnim snapem.
    just_added <- only_lines(line_difference(all_in_band, within_geom[[k - 1L]]))
    suffix_from <- base::gsub('\\.', 'p', base::as.character(from))
    write_layer(just_added, gpkg,
                base::paste0('prirustek_', suffix_from, '_', suffix, 'm'))
  }
  
  # Konkretni useky s nepresnou shodou (1-10 m) pro manualni kontrolu.
  # Vybirame nejdelsi samostatne liniove casti; geometrie nepreformovavame.
  candidates <- only_lines(line_difference(
    within_geom[[base::length(within_geom)]], within_geom[[4L]] # nad 1 m
  ))
  if (base::length(candidates) > 0L) {
    candidate_lines <- base::suppressWarnings(sf::st_cast(candidates, 'LINESTRING'))
    candidate_lines <- candidate_lines[!sf::st_is_empty(candidate_lines)]
    candidate_lengths <- units::drop_units(sf::st_length(candidate_lines))
    keep <- utils::head(base::order(candidate_lengths, decreasing = TRUE),
                        max_example_segments)
    g <- candidate_lines[keep]
    meta <- base::data.frame(
      usek_id = base::seq_along(g),
      delka_m = base::as.numeric(candidate_lengths[keep]),
      min_vzdalenost_m = base::as.numeric(sf::st_distance(g, state_local)[, 1L])
    )
    write_layer(g, gpkg, 'ukazky_nesouladu_1_10m', attrs = meta)
  }
  
  summary <- dplyr::tibble(
    SITECODE = site_code,
    HABITAT_CODE = hab_code,
    n_cilovych_zaznamu = target$n,
    border_all_m = border_all_m,
    border_hsl_exact_m = lengths[[1L]],
    border_v_10m_pasu_m = lengths[[base::length(lengths)]],
    podil_presne_shody_pct = near_percent[[1L]],
    podil_v_10m_pasu_pct = near_percent[[base::length(near_percent)]],
    geometrie_gpkg = base::basename(gpkg)
  )
  base::list(measured = measured, increment = increment, summary = summary)
}

results <- base::lapply(habitat_codes, function(h) {
  base::tryCatch(calculate_habitat(h), error = function(e) {
    base::message('CHYBA ', h, ': ', base::conditionMessage(e))
    base::list(error = base::conditionMessage(e), habitat = h)
  })
})

ok <- base::Filter(function(x) base::is.null(x$error), results)
errors <- base::Filter(function(x) !base::is.null(x$error), results)
if (base::length(ok) > 0L) {
  readr::write_csv(dplyr::bind_rows(base::lapply(ok, `[[`, 'measured')),
                   base::file.path(out_dir, 'tolerance_delky.csv'))
  readr::write_csv(dplyr::bind_rows(base::lapply(ok, `[[`, 'increment')),
                   base::file.path(out_dir, 'prirustky_intervalu.csv'))
  readr::write_csv(dplyr::bind_rows(base::lapply(ok, `[[`, 'summary')),
                   base::file.path(out_dir, 'souhrn_stanovist.csv'))
}
log <- dplyr::tibble(
  HABITAT_CODE = habitat_codes,
  status = base::vapply(results, function(x) {
    if (base::is.null(x$error)) 'ok' else 'error'
  }, base::character(1)),
  error_message = base::vapply(results, function(x) {
    if (base::is.null(x$error)) NA_character_ else x$error
  }, base::character(1))
)
readr::write_csv(log, base::file.path(out_dir, 'test_log.csv'))
base::message('\nHOTOVO. Vysledky: ', out_dir)
base::print(log)
