# Oprava 2026-10-30: dynamicky vyber sloupce v dplyr::filter() pouziva .data, nikoli rlang::.data.
# TEST_Hradiste_vnejsi_mozaika.R
# ---------------------------------------------------------------
# Diagnostika OLD vs NEW vypoctu vnejsi mozaiky v EVL Hradiste.
# Produkcni funkce ani zdrojova data se NEMENI.
# Spusteni z korene repozitare:
#   Rscript tests/skripty/TEST_Hradiste_vnejsi_mozaika.R
# nebo v RStudiu:
#   base::source('tests/skripty/TEST_Hradiste_vnejsi_mozaika.R')
#
# Vystupy:
#   Outputs/Test/Hradiste/MOZAIKA_DIAGNOSTIKA/
#     souhrn_variant.csv, detail_delky.csv, diagnosticky_log.csv
#     hranice_6210.gpkg, hranice_9130.gpkg, nahled_6210.png, ...
#
# Varianty:
#   OLD_original               = stary prunik + slice(1)
#   OLD_bez_slice              = totez, bez slice(1)
#   NEW_original               = aktualni novy vypocet
#   NEW_bez_stejneho_segmentu  = NEW, ale vylouceny shodne SEGMENT_ID
#   NEW_jen_spolecne_hrany    = navic prunik hranice s HRANICI souseda
#
# POZOR: stara verze pracuje s historickou interpretaci geom.
# Rozdily v presnosti geometrie / knihoven sf mohou zpusobit
# male odchylky od historicky exportovaneho CSV.
# ---------------------------------------------------------------

# Nastaveni -- lze menit
site_code <- 'CZ0414127'
habitat_codes <- base::c('6210', '9130')
habitat_col <- 'HABITAT'
buffer_m <- 500

repo_root <- base::normalizePath('.', winslash = '/', mustWork = TRUE)
runner <- base::file.path(repo_root, 'R', '01_stanoviste', 'RUN_stanoviste.R')
if (!base::file.exists(runner)) {
  base::stop('Spustte skript z korene repozitare host_naturecz_fork.')
}

# Jednou nacti stejne vstupy jako v produkcnim NEW workflow.
# Samotnou hodnotici smycku runner automaticky nespousti.
base::source(runner, local = base::globalenv(), chdir = FALSE)

vmb <- stanoviste_inputs$data$vmb
site <- stanoviste_inputs$data$site
czechia_line <- stanoviste_inputs$data$czechia_line

required_cols <- base::c('SEGMENT_ID', habitat_col, 'FSB_EVAL', 'STEJ_PR')
missing_cols <- base::setdiff(required_cols, base::names(vmb))
if (base::length(missing_cols) > 0) {
  base::stop('VMB nema pozadovane sloupce: ', base::paste(missing_cols, collapse = ', '))
}

out_dir <- base::file.path(repo_root, 'Outputs', 'Test', 'Hradiste',
                           'MOZAIKA_DIAGNOSTIKA')
base::dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Pomocne geometricke funkce ----------------------------------
empty_sfc <- function(crs) {
  sf::st_sfc(crs = crs)
}

geom_len_m <- function(g) {
  if (base::inherits(g, 'sf')) g <- sf::st_geometry(g)
  if (base::length(g) == 0L) return(0)
  base::sum(units::drop_units(sf::st_length(g)), na.rm = TRUE)
}

safe_intersection <- function(g1, g2, crs) {
  if (base::inherits(g1, 'sf')) g1 <- sf::st_geometry(g1)
  if (base::inherits(g2, 'sf')) g2 <- sf::st_geometry(g2)
  if (base::length(g1) == 0L || base::length(g2) == 0L) {
    return(empty_sfc(crs))
  }
  sf::st_intersection(g1, g2)
}

safe_union <- function(x, crs) {
  if (base::inherits(x, 'sf')) x <- sf::st_geometry(x)
  if (base::length(x) == 0L) return(empty_sfc(crs))
  sf::st_make_valid(sf::st_union(x))
}

line_parts <- function(g, crs) {
  if (base::inherits(g, 'sf')) g <- sf::st_geometry(g)
  if (base::length(g) == 0L) return(empty_sfc(crs))
  g <- sf::st_collection_extract(g, 'LINESTRING', warn = FALSE)
  if (base::length(g) == 0L) return(empty_sfc(crs))
  g[!sf::st_is_empty(g)]
}

# Pro kontrolu v QGIS zapis geometrie do samostatnych vrstev.
write_geom <- function(g, gpkg, layer, kind = 'geom') {
  if (base::inherits(g, 'sf')) {
    # Pri exportu OLD pruniku zachovej take ID ciloveho a sousedniho segmentu.
    keep <- base::intersect(
      base::c('SEGMENT_ID', 'SEGMENT_ID_buff', 'HABITAT', 'BIOTOP',
              'FSB_EVAL', '.contact_row'),
      base::names(g)
    )
    attributes <- sf::st_drop_geometry(g)[keep]
    geom <- sf::st_geometry(g)
    if (base::length(geom) == 0L) return(base::invisible(FALSE))
    valid <- !sf::st_is_empty(geom)
    geom <- geom[valid]
    if (base::length(geom) == 0L) return(base::invisible(FALSE))
    x <- sf::st_sf(kategorie = base::rep(kind, base::length(geom)),
                   attributes[valid, , drop = FALSE], geometry = geom)
  } else {
    geom <- g
    if (base::length(geom) == 0L) return(base::invisible(FALSE))
    geom <- geom[!sf::st_is_empty(geom)]
    if (base::length(geom) == 0L) return(base::invisible(FALSE))
    x <- sf::st_sf(kategorie = base::rep(kind, base::length(geom)),
                   geometry = geom)
  }
  base::tryCatch(
    sf::st_write(x, dsn = gpkg, layer = layer,
                 delete_layer = TRUE, quiet = TRUE),
    error = function(e) {
      base::warning('GPKG vrstva ', layer, ': ', base::conditionMessage(e))
    }
  )
  base::invisible(TRUE)
}

# Pokud GEOS nema presne shodne hrany, geometrie dvou linii
# muze davat prunik nulove delky. Diagnostika to NEOPRAVUJE
# umelou toleranci: chceme videt skutecne chovani kodu.

calc_one <- function(hab_code) {
  base::message('\nMozaika: ', site_code, ' x ', hab_code)
  crs <- sf::st_crs(vmb)
  
  site_target <- site |>
    dplyr::filter(base::as.character(SITECODE) == site_code)
  if (base::nrow(site_target) == 0L) {
    base::stop('SITECODE nenalezen: ', site_code)
  }
  if (!base::isTRUE(sf::st_crs(site_target) == crs)) {
    site_target <- sf::st_transform(site_target, crs)
  }
  site_geom <- sf::st_union(sf::st_geometry(site_target))
  site_buffer <- sf::st_buffer(site_geom, buffer_m)
  
  # Nova verze: vyber nejdrive podle kodu a nasledne orez podle EVL.
  target_new <- vmb |>
    dplyr::filter(.data[[habitat_col]] == hab_code) |>
    sf::st_intersection(site_geom) |>
    sf::st_make_valid() |>
    sf::st_collection_extract('POLYGON', warn = FALSE)
  
  # Stara verze: nejdrive geometricky orez, potom atributovy vyber.
  target_old <- vmb |>
    sf::st_filter(site_geom) |>
    sf::st_intersection(site_geom) |>
    dplyr::filter(.data[[habitat_col]] == hab_code) |>
    sf::st_make_valid() |>
    sf::st_collection_extract('POLYGON', warn = FALSE)
  
  if (base::nrow(target_new) == 0L || base::nrow(target_old) == 0L) {
    base::stop('Neni nalezen zadny polygon stanoviste: ', hab_code)
  }
  # Pracujeme s kody segmentu, nikoliv OBJECTID atributovych tabulek.
  target_ids <- base::unique(base::as.character(target_new$SEGMENT_ID))
  
  # NEW --------------------------------------------------------
  spat_new <- target_new |>
    dplyr::filter(!base::is.na(FSB_EVAL), FSB_EVAL != 'X')
  if (base::nrow(spat_new) == 0L) {
    base::stop('V NEW chybi hodnotitelne segmenty FSB_EVAL != X: ', hab_code)
  }
  
  target_union <- safe_union(spat_new, crs)
  target_boundary <- sf::st_boundary(target_union)
  new_border_all <- geom_len_m(target_boundary)
  
  neighbours <- vmb |>
    sf::st_filter(site_buffer) |>
    dplyr::filter(
      !base::is.na(FSB_EVAL),
      !FSB_EVAL %in% base::c('X', '-', '-1'),
      base::is.na(.data[[habitat_col]]) |
        .data[[habitat_col]] != hab_code
    ) |>
    sf::st_make_valid() |>
    sf::st_collection_extract('POLYGON', warn = FALSE)
  
  # Rozdeleni sousedu podle shodnosti ID s jakymkoli hodnocenym polygonem.
  neighbours_same <- neighbours |>
    dplyr::filter(base::as.character(SEGMENT_ID) %in% target_ids)
  neighbours_other <- neighbours |>
    dplyr::filter(!base::as.character(SEGMENT_ID) %in% target_ids)
  
  natural_all <- safe_union(neighbours, crs)
  natural_other <- safe_union(neighbours_other, crs)
  natural_same <- safe_union(neighbours_same, crs)
  
  contact_new_original <- safe_intersection(target_boundary, natural_all, crs)
  contact_new_other <- safe_intersection(target_boundary, natural_other, crs)
  contact_new_strict <- if (base::length(natural_other) > 0L) {
    safe_intersection(target_boundary, sf::st_boundary(natural_other), crs)
  } else {
    empty_sfc(crs)
  }
  contact_same <- safe_intersection(target_boundary, natural_same, crs)
  
  # Co novy plosny prunik povazuje za dotyk, ale nejde o shodu
  # hranice s hranici? Je to indikator problemu s prekryvem polygonu.
  contact_overlap_only <- if (base::length(contact_new_other) > 0L) {
    if (base::length(contact_new_strict) > 0L) {
      sf::st_difference(contact_new_other, sf::st_union(contact_new_strict))
    } else {
      contact_new_other
    }
  } else {
    empty_sfc(crs)
  }
  
  # OLD --------------------------------------------------------
  # Replika hlavni casti puvodni funkce hvezdice_eval(); bez jejiho
  # spusteni: umozni export mezikroku, ktere puvodni funkce nevraci.
  spat_old <- target_old |>
    dplyr::filter(!base::is.na(FSB_EVAL), FSB_EVAL != 'X') |>
    sf::st_cast('POLYGON', warn = FALSE)
  
  old_border_all <- geom_len_m(sf::st_cast(spat_old, 'LINESTRING', warn = FALSE))
  
  neighbours_old <- vmb |>
    sf::st_filter(site_buffer) |>
    sf::st_make_valid() |>
    dplyr::filter(
      base::as.character(sf::st_geometry_type(geometry)) %in%
        base::c('POLYGON', 'MULTIPOLYGON'),
      !base::is.na(FSB_EVAL),
      !FSB_EVAL %in% base::c('X', '-', '-1'),
      !base::is.na(.data[[habitat_col]]),
      .data[[habitat_col]] != hab_code
    ) |>
    dplyr::rename(SEGMENT_ID_buff = SEGMENT_ID) |>
    dplyr::group_by(SEGMENT_ID_buff) |>
    dplyr::slice(1) |>
    dplyr::ungroup()
  
  if (base::nrow(neighbours_old) > 0L) {
    # Prunik s kazdym polygonem hodnoceneho stanoviste (OLD nema union).
    old_contacts_all <- sf::st_intersection(
      dplyr::select(spat_old, SEGMENT_ID),
      dplyr::select(neighbours_old, SEGMENT_ID_buff)
    ) |>
      dplyr::filter(!base::as.character(SEGMENT_ID_buff) %in% target_ids) |>
      dplyr::mutate(.contact_row = dplyr::row_number())
  } else {
    old_contacts_all <- sf::st_sf(
      SEGMENT_ID = base::character(),
      SEGMENT_ID_buff = base::character(),
      .contact_row = base::integer(),
      geometry = empty_sfc(crs)
    )
  }
  
  old_contacts_kept <- old_contacts_all |>
    dplyr::group_by(SEGMENT_ID_buff) |>
    dplyr::slice(1) |>
    dplyr::ungroup()
  
  old_contacts_discarded <- old_contacts_all |>
    dplyr::filter(!.contact_row %in% old_contacts_kept$.contact_row)
  
  old_border_nat <- geom_len_m(old_contacts_kept)
  old_border_nat_no_slice <- geom_len_m(old_contacts_all)
  
  # Statni hranice: OLD meri na nespojenych polygonech,
  # NEW meri na sjednocenem obvodu; proto oddelene delky.
  state_geom <- if (base::inherits(czechia_line, 'sf')) {
    sf::st_geometry(czechia_line)
  } else {
    czechia_line
  }
  if (!base::isTRUE(sf::st_crs(state_geom) == crs)) {
    state_geom <- sf::st_transform(state_geom, crs)
  }
  state_geom <- sf::st_union(state_geom)
  state_old <- safe_intersection(sf::st_geometry(spat_old), state_geom, crs)
  state_new <- safe_intersection(target_boundary, state_geom, crs)
  old_border_hsl <- geom_len_m(state_old)
  new_border_hsl <- geom_len_m(state_new)
  
  old_known <- old_border_all - old_border_hsl
  new_known <- new_border_all - new_border_hsl
  # Procenta: OLD ma formalne kompenzaci * (old_known/old_all),
  # ta se pri nenulovych hodnotach vykrati (viz stary skript).
  old_pct <- function(nat) {
    if (old_border_all <= 0 || old_known <= 0) return(NA_real_)
    nat / old_known * (old_known / old_border_all) * 100
  }
  new_pct <- function(nat) {
    if (new_border_all <= 0 || new_known <= 0) return(NA_real_)
    base::min(100, base::max(0, nat / new_known * 100))
  }
  
  metrics <- dplyr::tibble(
    SITECODE = site_code,
    HABITAT_CODE = hab_code,
    varianta = base::c(
      'OLD_original', 'OLD_bez_slice', 'NEW_original',
      'NEW_bez_stejneho_segmentu', 'NEW_jen_spolecne_hrany'
    ),
    border_all_m = base::c(old_border_all, old_border_all,
                           new_border_all, new_border_all, new_border_all),
    border_nat_m = base::c(old_border_nat, old_border_nat_no_slice,
                           geom_len_m(contact_new_original),
                           geom_len_m(contact_new_other),
                           geom_len_m(contact_new_strict)),
    border_hsl_m = base::c(old_border_hsl, old_border_hsl,
                           new_border_hsl, new_border_hsl, new_border_hsl),
    border_known_m = base::c(old_known, old_known, new_known, new_known, new_known),
    mozaika_pct = base::c(
      old_pct(old_border_nat), old_pct(old_border_nat_no_slice),
      new_pct(geom_len_m(contact_new_original)),
      new_pct(geom_len_m(contact_new_other)),
      new_pct(geom_len_m(contact_new_strict))
    )
  )
  
  diagnostics <- dplyr::tibble(
    SITECODE = site_code,
    HABITAT_CODE = hab_code,
    n_target_polygonu_OLD = base::nrow(spat_old),
    n_target_zaznamu_NEW = base::nrow(spat_new),
    n_target_segmentu = base::length(target_ids),
    n_sousedu_se_stejnym_ID = base::nrow(neighbours_same),
    n_sousedu_jine_ID = base::nrow(neighbours_other),
    n_pruniku_OLD_vse = base::nrow(old_contacts_all),
    n_pruniku_OLD_ponechanych = base::nrow(old_contacts_kept),
    n_pruniku_OLD_zahozenych = base::nrow(old_contacts_discarded),
    old_ztracena_delka_m = geom_len_m(old_contacts_discarded),
    kontakt_stejne_SEGMENT_ID_m = geom_len_m(contact_same),
    new_plosny_minus_hranicni_m = geom_len_m(contact_overlap_only),
    rozdil_NEW_po_vylouceni_ID_m =
      geom_len_m(contact_new_original) - geom_len_m(contact_new_other)
  )
  
  # Export pro kontrolu v QGIS -------------------------------
  gpkg <- base::file.path(out_dir, base::paste0('hranice_', hab_code, '.gpkg'))
  if (base::file.exists(gpkg)) base::file.remove(gpkg)
  write_geom(site_target, gpkg, 'EVL', 'hranice_EVL')
  write_geom(spat_new, gpkg, 'CIL_stanoviste', 'cil')
  write_geom(neighbours_same, gpkg, 'SOUSED_stejne_SEGMENT_ID', 'stejne_id')
  write_geom(neighbours_other, gpkg, 'SOUSED_jine_SEGMENT_ID', 'jine_id')
  write_geom(target_boundary, gpkg, 'HRANICE_cile', 'obvod')
  write_geom(contact_new_original, gpkg, 'NEW_kontakt_puvodni', 'new_pred')
  write_geom(contact_new_other, gpkg, 'NEW_kontakt_bez_stejneho_ID', 'new_po')
  write_geom(contact_new_strict, gpkg, 'NEW_pouze_spolecne_hrany', 'new_hrany')
  write_geom(contact_same, gpkg, 'KONTAKT_stejne_SEGMENT_ID', 'stejne_id')
  write_geom(contact_overlap_only, gpkg, 'KONTAKT_mimo_spolecne_hrany', 'prekryv')
  write_geom(old_contacts_all, gpkg, 'OLD_pruniky_pred_slice', 'old_pred')
  write_geom(old_contacts_kept, gpkg, 'OLD_pruniky_po_slice', 'old_po')
  write_geom(old_contacts_discarded, gpkg, 'OLD_pruniky_ztracene', 'old_ztrata')
  write_geom(state_new, gpkg, 'STATNI_hranice_na_obvodu', 'statni_hranice')
  
  # Nahled cele EVL; pro detailni prohlizeni je urcen GPKG.
  png_path <- base::file.path(out_dir, base::paste0('nahled_', hab_code, '.png'))
  grDevices::png(png_path, width = 1600, height = 1100, res = 160)
  base::tryCatch({
    graphics::par(mar = base::c(1, 1, 4, 1))
    graphics::plot(sf::st_geometry(site_target), col = NA, border = 'grey60',
                   main = base::paste('EVL Hradiste, stanoviste', hab_code))
    if (base::nrow(neighbours_other) > 0L) {
      graphics::plot(sf::st_geometry(neighbours_other), col = '#B5D6B5',
                     border = NA, add = TRUE)
    }
    graphics::plot(sf::st_geometry(spat_new), col = '#B2CBE5',
                   border = '#436A94', add = TRUE)
    lines_before <- line_parts(contact_new_original, crs)
    lines_after <- line_parts(contact_new_other, crs)
    lines_same <- line_parts(contact_same, crs)
    if (base::length(lines_before) > 0L) {
      graphics::plot(lines_before, col = '#E69F00', lwd = 3, add = TRUE)
    }
    if (base::length(lines_after) > 0L) {
      graphics::plot(lines_after, col = '#157E45', lwd = 2, add = TRUE)
    }
    if (base::length(lines_same) > 0L) {
      graphics::plot(lines_same, col = '#C82333', lwd = 1.5, add = TRUE)
    }
    graphics::legend('bottomleft', bty = 'n', cex = .8,
                     legend = base::c('hodnocene stanoviste', 'ostatni prirodni',
                                      'NEW pred upravou', 'NEW po vylouceni stejneho ID',
                                      'kontakt se stejnym ID'),
                     fill = base::c('#B2CBE5', '#B5D6B5', NA, NA, NA),
                     border = base::c('#436A94', NA, NA, NA, NA),
                     lty = base::c(NA, NA, 1, 1, 1),
                     col = base::c(NA, NA, '#E69F00', '#157E45', '#C82333'),
                     lwd = base::c(NA, NA, 3, 2, 2))
  }, error = function(e) {
    base::warning('PNG nahled ', hab_code, ': ', base::conditionMessage(e))
  }, finally = grDevices::dev.off())
  
  base::message('Hotovo ', hab_code, ': ', gpkg)
  base::list(metrics = metrics, diagnostics = diagnostics)
}

# Hlavni smycka ----------------------------------------------
result_list <- base::lapply(habitat_codes, function(h) {
  base::tryCatch(calc_one(h), error = function(e) {
    base::message('CHYBA ', h, ': ', base::conditionMessage(e))
    base::list(error = base::conditionMessage(e), HABITAT_CODE = h)
  })
})

successful <- base::Filter(function(x) !('error' %in% base::names(x)), result_list)
errors <- base::Filter(function(x) 'error' %in% base::names(x), result_list)

if (base::length(successful) > 0L) {
  metrics <- dplyr::bind_rows(base::lapply(successful, `[[`, 'metrics'))
  diagnostics <- dplyr::bind_rows(base::lapply(successful, `[[`, 'diagnostics'))
  readr::write_csv(metrics, base::file.path(out_dir, 'souhrn_variant.csv'))
  readr::write_csv(diagnostics, base::file.path(out_dir, 'detail_delky.csv'))
  base::print(metrics)
  base::print(diagnostics)
}

if (base::length(errors) > 0L) {
  error_table <- dplyr::tibble(
    HABITAT_CODE = base::vapply(errors, `[[`, base::character(1), 'HABITAT_CODE'),
    error_message = base::vapply(errors, `[[`, base::character(1), 'error')
  )
  readr::write_csv(error_table, base::file.path(out_dir, 'diagnosticky_log.csv'))
  base::warning('Nektere habitaty selhaly. Viz diagnosticky_log.csv.')
} else {
  readr::write_csv(dplyr::tibble(HABITAT_CODE = habitat_codes,
                                 status = 'ok'),
                   base::file.path(out_dir, 'diagnosticky_log.csv'))
}
base::message('Diagnostika dokoncena. Vystupy: ', out_dir)
