# TEST Sumava vnejsi mozaika a statni hranice

# TEST_Sumava_vnejsi_mozaika.R
# ------------------------------------------------------------
# Diagnosticky test VNEJSI MOZAIKY pro EVL Sumava (CZ0314024).
# Neprepise ani neupravi produkcni funkce ci jejich vstupni data.
# Spusteni z KORENE repozitare host_naturecz_fork:
#   Rscript tests/skripty/TEST_Sumava_vnejsi_mozaika.R
#
# Vystupy: Outputs/Test/Sumava/MOZAIKA_DIAGNOSTIKA/
# - souhrn_variant.csv: OLD/NEW + delky vsech tri hranic
# - kandidati_statni_hranice.csv: vyber stanovist
# - detail_delky.csv: diagnostika SEGMENT_ID, slice(1), prekryvu
# - diagnosticky_log.csv: chyby a uspesne vypocty
# - hranice_<kod>.gpkg: prostorove vrstvy pro QGIS
# - nahled_<kod>.png: orientacni mapa
#
# DULEZITE: EVL muze lezet na hranici CR, ale jednotliva stanoviste
# se ji vubec nemusi dotykat. Automaticky vyber proto overuje
# delku PRUNIKU OBVODU STANOVISTE se statni hranici.
# ------------------------------------------------------------

# 1. NASTAVENI ------------------------------------------------
site_code <- "CZ0314024"
habitat_col <- "HABITAT"

# NULL = automaticky vybrat az 2 predmety ochrany, ktere primo
# sousedi se statni hranici. Alternativne napr. c("9410", "91D0").
habitat_codes <- NULL
max_habitats <- 2L
max_candidates_to_check <- 20L
site_buffer_m <- 500
near_border_m <- 10  # POUZE DIAGNOSTIKA nesouladu map, NE korekce vzorce

repo_root <- base::normalizePath(".", winslash = "/", mustWork = TRUE)
runner <- base::file.path(repo_root, "R/01_stanoviste/RUN_stanoviste.R")
if (!base::file.exists(runner)) {
  base::stop("Spustte test z korene repozitare host_naturecz_fork.")
}

# Stejne vstupy jako v aktualnim NEW; produkcni vypocet se NESPOUSTI.
base::source(runner, local = base::globalenv(), chdir = FALSE)
vmb <- stanoviste_inputs$data$vmb
site <- stanoviste_inputs$data$site
state <- stanoviste_inputs$data$czechia_line

required <- base::c("SEGMENT_ID", habitat_col, "FSB_EVAL")
missing <- base::setdiff(required, base::names(vmb))
if (base::length(missing) > 0L) {
  base::stop("Chybi sloupce VMB: ", base::paste(missing, collapse = ", "))
}
if (sf::st_is_longlat(vmb)) base::stop("Ocekavan projektovany souradnicovy system v metrech.")

out_dir <- base::file.path(repo_root, "Outputs/Test/Sumava/MOZAIKA_DIAGNOSTIKA")
base::dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
crs <- sf::st_crs(vmb)

# 2. POMOCNE FUNKCE ------------------------------------------
len_m <- function(geom) {
  if (base::inherits(geom, "sf")) geom <- sf::st_geometry(geom)
  if (base::length(geom) == 0L) return(0)
  base::sum(units::drop_units(sf::st_length(geom)), na.rm = TRUE)
}
empty_geom <- function() sf::st_sfc(crs = crs)
union_geom <- function(x) {
  if (base::inherits(x, "sf")) x <- sf::st_geometry(x)
  if (base::length(x) == 0L) return(empty_geom())
  sf::st_make_valid(sf::st_union(x))
}
intersect_geom <- function(a, b) {
  if (base::inherits(a, "sf")) a <- sf::st_geometry(a)
  if (base::inherits(b, "sf")) b <- sf::st_geometry(b)
  if (base::length(a) == 0L || base::length(b) == 0L) return(empty_geom())
  base::suppressWarnings(sf::st_intersection(a, b))
}
line_geom <- function(x) {
  if (base::inherits(x, "sf")) x <- sf::st_geometry(x)
  if (base::length(x) == 0L) return(empty_geom())
  y <- sf::st_collection_extract(x, "LINESTRING", warn = FALSE)
  if (base::length(y) == 0L) return(empty_geom())
  y[!sf::st_is_empty(y)]
}
write_layer <- function(x, gpkg, name) {
  if (base::inherits(x, "sf")) {
    if (base::nrow(x) == 0L) return(base::invisible(NULL))
    attrs <- sf::st_drop_geometry(x)
    geom <- sf::st_geometry(x)
    idx <- !sf::st_is_empty(geom)
    if (!base::any(idx)) return(base::invisible(NULL))
    # Zachovat ID pro identifikaci v QGIS, nepovolene nazvy vynechat.
    selected <- base::intersect(base::c("SEGMENT_ID", "SEGMENT_ID_buff",
                                        "HABITAT", "BIOTOP", "FSB_EVAL", ".contact_row"), base::names(attrs))
    y <- sf::st_sf(attrs[idx, selected, drop = FALSE], geometry = geom[idx])
  } else {
    if (base::length(x) == 0L) return(base::invisible(NULL))
    x <- x[!sf::st_is_empty(x)]
    if (base::length(x) == 0L) return(base::invisible(NULL))
    y <- sf::st_sf(geometry = x)
  }
  base::tryCatch(
    sf::st_write(y, gpkg, layer = name, delete_layer = TRUE, quiet = TRUE),
    error = function(e) base::warning("Export vrstvy ", name, ": ", base::conditionMessage(e))
  )
  base::invisible(NULL)
}

# 3. VYBER EVL A CESKE STATNI HRANICE -------------------------
site_sf <- site |>
  dplyr::filter(.data$SITECODE == site_code)
if (base::nrow(site_sf) == 0L) base::stop("Nenalezena EVL ", site_code)
if (!base::isTRUE(sf::st_crs(site_sf) == crs)) {
  site_sf <- sf::st_transform(site_sf, crs)
}
site_geom <- sf::st_make_valid(sf::st_union(sf::st_geometry(site_sf)))
site_buffer <- sf::st_buffer(site_geom, site_buffer_m)

state_geom <- if (base::inherits(state, "sf")) sf::st_geometry(state) else state
if (!base::isTRUE(sf::st_crs(state_geom) == crs)) {
  state_geom <- sf::st_transform(state_geom, crs)
}
state_geom <- sf::st_union(state_geom)
state_local <- intersect_geom(state_geom, site_buffer)
if (base::length(state_local) == 0L || base::all(sf::st_is_empty(state_local))) {
  base::stop("Statni hranice neprotina 500m okoli EVL; zkontrolujte vrstvy a CRS.")
}
state_near <- sf::st_buffer(state_local, near_border_m)
site_boundary <- sf::st_boundary(site_geom)
site_border_exact_m <- len_m(intersect_geom(site_boundary, state_local))
site_border_near_m <- len_m(intersect_geom(site_boundary, state_near))
base::message("Sumava: EVL x statni hranice, presne = ",
              base::round(site_border_exact_m, 2), " m; ",
              "do ", near_border_m, " m = ", base::round(site_border_near_m, 2), " m")

# 4. AUTOMATICKY VYBER PREDMETU OCHRANY U HRANICE -------------
# Tato priprava probiha pouze pro vyber kodu; zadne vysledky
# NEW/OLD se zde neprepocitavaji ani nenahrazuji aproximaci.
protected <- base::get("sites_habitats", envir = stanoviste_inputs$config_env)
protected_codes <- protected |>
  dplyr::filter(base::as.character(.data$site_code) == site_code) |>
  dplyr::pull(.data$feature_code) |>
  base::as.character() |>
  base::unique()
protected_codes <- protected_codes[!base::is.na(protected_codes)]
if (base::length(protected_codes) == 0L) {
  base::stop("V sites_habitats nejsou nalezeny predmety ochrany EVL Sumava.")
}

# Orez vstupni VMB az pri vypoctu pro jednotlive stanoviste.
prepare_target <- function(code) {
  target <- vmb |>
    dplyr::filter(.data[[habitat_col]] == code) |>
    sf::st_filter(site_geom) |>
    sf::st_intersection(site_geom) |>
    sf::st_make_valid() |>
    sf::st_collection_extract("POLYGON", warn = FALSE)
  if (base::nrow(target) == 0L) return(NULL)
  target <- target |>
    dplyr::mutate(.area = units::drop_units(sf::st_area(geometry))) |>
    dplyr::filter(.area > 0, !base::is.na(FSB_EVAL), FSB_EVAL != "X") |>
    dplyr::select(-.area)
  if (base::nrow(target) == 0L) return(NULL)
  target
}

measure_border <- function(code) {
  target <- prepare_target(code)
  if (base::is.null(target)) return(base::c(exact_m = 0, near_m = 0))
  boundary <- sf::st_boundary(union_geom(target))
  base::c(exact_m = len_m(intersect_geom(boundary, state_local)),
          near_m = len_m(intersect_geom(boundary, state_near)))
}

if (base::is.null(habitat_codes)) {
  base::message("Vyhledavam predmety ochrany v blizkosti statni hranice...")
  edge_vmb <- vmb |>
    sf::st_filter(state_near) |>
    sf::st_filter(site_geom) |>
    dplyr::filter(.data[[habitat_col]] %in% protected_codes,
                  !base::is.na(FSB_EVAL), FSB_EVAL != "X")
  if (base::nrow(edge_vmb) == 0L) {
    base::stop("Zadny predmet ochrany v pasmu statni hranice. ",
               "Zvyste near_border_m nebo zadejte habitat_codes rucne.")
  }
  freqs <- base::sort(base::table(base::as.character(edge_vmb[[habitat_col]])),
                      decreasing = TRUE)
  candidate_codes <- utils::head(base::names(freqs), max_candidates_to_check)
} else {
  candidate_codes <- base::unique(base::as.character(habitat_codes))
  if (base::any(!candidate_codes %in% protected_codes)) {
    base::warning("Nektere zadane habitaty nejsou v sites_habitats mezi predmety ochrany Sumavy.")
  }
}

candidate_rows <- base::list()
selected <- base::character()
for (code in candidate_codes) {
  m <- base::tryCatch(measure_border(code), error = function(e) {
    base::warning("Vyber ", code, ": ", base::conditionMessage(e))
    base::c(exact_m = NA_real_, near_m = NA_real_)
  })
  candidate_rows[[base::length(candidate_rows) + 1L]] <-
    dplyr::tibble(HABITAT_CODE = code, border_hsl_exact_m = m[["exact_m"]],
                  border_v_pasu_m = m[["near_m"]],
                  dotyk_presne = !base::is.na(m[["exact_m"]]) && m[["exact_m"]] > 0)
  if (base::is.null(habitat_codes) && base::isTRUE(m[["exact_m"]] > 0)) {
    selected <- base::c(selected, code)
    if (base::length(selected) >= max_habitats) break
  }
}
candidates <- dplyr::bind_rows(candidate_rows)
readr::write_csv(candidates, base::file.path(out_dir, "kandidati_statni_hranice.csv"))

if (!base::is.null(habitat_codes)) selected <- candidate_codes
if (base::is.null(habitat_codes) && base::length(selected) == 0L) {
  base::warning("Zadny kandidat nema presne mereny dotyk se statni hranici. ",
                "Vysledky nelze interpretovat jako test pohranicnich polygonu. ",
                "Zkontrolujte kandidati_statni_hranice.csv a presnost vrstev.")
  # Presto vyhodnot nejblizsi kandidat(y), aby byl zrejmy pripadny
  # nesoulad obou hranic; ve vzorci se near_border_m NEPOUZIJE.
  by_near <- candidates |>
    dplyr::arrange(dplyr::desc(.data$border_v_pasu_m)) |>
    dplyr::filter(!base::is.na(.data$border_v_pasu_m), .data$border_v_pasu_m > 0)
  selected <- utils::head(by_near$HABITAT_CODE, max_habitats)
}
if (base::length(selected) == 0L) {
  base::stop("Nepodarilo se vybrat kandidatni stanoviste. Viz CSV kandidatu.")
}
base::message("Vybrana stanoviste: ", base::paste(selected, collapse = ", "))

# 5. VLASTNI DIAGNOSTICKY VYPOCET ------------------------------
run_diagnostic <- function(code) {
  base::message("Zpracovavam ", site_code, " / ", code)
  target <- prepare_target(code)
  if (base::is.null(target)) base::stop("V EVL chybi polygon stanoviste ", code)
  target_ids <- base::unique(base::as.character(target$SEGMENT_ID))
  
  # NEW: hranice sjednocenych polygonu stanoviste.
  target_boundary <- sf::st_boundary(union_geom(target))
  new_all <- len_m(target_boundary)
  new_state_geom <- intersect_geom(target_boundary, state_local)
  new_hsl <- len_m(new_state_geom)
  new_known <- base::max(new_all - new_hsl, 0)
  near_hsl <- len_m(intersect_geom(target_boundary, state_near))
  
  natural <- vmb |>
    sf::st_filter(site_buffer) |>
    dplyr::filter(!base::is.na(FSB_EVAL),
                  !FSB_EVAL %in% base::c("X", "-", "-1"),
                  base::is.na(.data[[habitat_col]]) |
                    .data[[habitat_col]] != code) |>
    sf::st_make_valid() |>
    sf::st_collection_extract("POLYGON", warn = FALSE)
  natural_same <- natural |>
    dplyr::filter(base::as.character(SEGMENT_ID) %in% target_ids)
  natural_other <- natural |>
    dplyr::filter(!base::as.character(SEGMENT_ID) %in% target_ids)
  
  new_contact <- intersect_geom(target_boundary, union_geom(natural))
  other_geom <- union_geom(natural_other)
  new_contact_without_id <- intersect_geom(target_boundary, other_geom)
  new_contact_strict <- intersect_geom(
    target_boundary,
    if (base::length(other_geom) > 0L) sf::st_boundary(other_geom) else empty_geom()
  )
  same_contact <- intersect_geom(target_boundary, union_geom(natural_same))
  contact_non_edge <- if (base::length(new_contact_without_id) > 0L) {
    if (base::length(new_contact_strict) > 0L) {
      sf::st_difference(new_contact_without_id, sf::st_union(new_contact_strict))
    } else new_contact_without_id
  } else empty_geom()
  
  # OLD: nesjednocene obvody a puvodni dva group_by/slice(1).
  spat_old <- sf::st_cast(target, "POLYGON", warn = FALSE)
  old_all <- len_m(sf::st_cast(spat_old, "LINESTRING", warn = FALSE))
  old_state_geom <- intersect_geom(sf::st_geometry(spat_old), state_local)
  old_hsl <- len_m(old_state_geom)
  old_known <- old_all - old_hsl
  
  natural_old <- vmb |>
    sf::st_filter(site_buffer) |>
    sf::st_make_valid() |>
    dplyr::filter(
      base::as.character(sf::st_geometry_type(geometry)) %in%
        base::c("POLYGON", "MULTIPOLYGON"),
      !base::is.na(FSB_EVAL),
      !FSB_EVAL %in% base::c("X", "-", "-1"),
      !base::is.na(.data[[habitat_col]]),
      .data[[habitat_col]] != code
    ) |>
    dplyr::rename(SEGMENT_ID_buff = SEGMENT_ID) |>
    dplyr::group_by(.data$SEGMENT_ID_buff) |>
    dplyr::slice(1) |>
    dplyr::ungroup()
  
  if (base::nrow(natural_old) > 0L) {
    old_contacts_all <- base::suppressWarnings(sf::st_intersection(
      dplyr::select(spat_old, dplyr::all_of("SEGMENT_ID")),
      dplyr::select(natural_old, dplyr::all_of("SEGMENT_ID_buff"))
    )) |>
      dplyr::filter(!base::as.character(.data$SEGMENT_ID_buff) %in% target_ids) |>
      dplyr::mutate(.contact_row = dplyr::row_number())
  } else {
    old_contacts_all <- sf::st_sf(
      SEGMENT_ID = base::character(), SEGMENT_ID_buff = base::character(),
      .contact_row = base::integer(), geometry = empty_geom()
    )
  }
  old_contacts_kept <- old_contacts_all |>
    dplyr::group_by(.data$SEGMENT_ID_buff) |>
    dplyr::slice(1) |>
    dplyr::ungroup()
  old_contacts_lost <- old_contacts_all |>
    dplyr::filter(!.data$.contact_row %in% old_contacts_kept$.contact_row)
  
  old_nat <- len_m(old_contacts_kept)
  old_nat_all <- len_m(old_contacts_all)
  new_nat <- len_m(new_contact)
  new_nat_noid <- len_m(new_contact_without_id)
  new_nat_strict <- len_m(new_contact_strict)
  
  pct_old <- function(nat) {
    if (old_all <= 0 || old_known <= 0) return(NA_real_)
    base::min(100, nat / old_known * (old_known / old_all) * 100)
  }
  pct_new <- function(nat) {
    if (new_all <= 0 || new_known <= 0) return(NA_real_)
    base::min(100, base::max(0, nat / new_known * 100))
  }
  
  summary <- dplyr::tibble(
    SITECODE = site_code, HABITAT_CODE = code,
    varianta = base::c("OLD_original", "OLD_bez_slice", "NEW_original",
                       "NEW_bez_stejneho_segmentu", "NEW_jen_spolecne_hrany"),
    border_all_m = base::c(old_all, old_all, new_all, new_all, new_all),
    border_nat_m = base::c(old_nat, old_nat_all, new_nat,
                           new_nat_noid, new_nat_strict),
    border_hsl_m = base::c(old_hsl, old_hsl, new_hsl, new_hsl, new_hsl),
    border_known_m = base::c(old_known, old_known, new_known, new_known, new_known),
    podil_statni_hranice_pct = base::c(
      if (old_all > 0) old_hsl / old_all * 100 else NA_real_,
      if (old_all > 0) old_hsl / old_all * 100 else NA_real_,
      base::rep(if (new_all > 0) new_hsl / new_all * 100 else NA_real_, 3)
    ),
    mozaika_pct = base::c(pct_old(old_nat), pct_old(old_nat_all),
                          pct_new(new_nat), pct_new(new_nat_noid),
                          pct_new(new_nat_strict)),
    POZOR_statni_hranice_nepresne = near_hsl > new_hsl + 1
  )
  
  details <- dplyr::tibble(
    SITECODE = site_code, HABITAT_CODE = code,
    n_cilovych_polygonu = base::nrow(target),
    n_sousedu_se_stejnym_ID = base::nrow(natural_same),
    n_sousedu_jine_ID = base::nrow(natural_other),
    n_starych_pruniku = base::nrow(old_contacts_all),
    n_zachovanych_po_slice = base::nrow(old_contacts_kept),
    n_zahozenych_po_slice = base::nrow(old_contacts_lost),
    border_nat_ztraceny_slice_m = len_m(old_contacts_lost),
    kontakt_se_stejnym_ID_m = len_m(same_contact),
    prekryv_neni_spolecna_hrana_m = len_m(contact_non_edge),
    border_hsl_NEW_presny_m = new_hsl,
    border_v_pasu_NEW_m = near_hsl,
    border_hsl_OLD_presny_m = old_hsl,
    prirodni_kontakt_na_statni_hranici_m =
      len_m(intersect_geom(new_contact_without_id, new_state_geom)),
    neshoda_statni_hrany_m = base::max(0, near_hsl - new_hsl)
  )
  
  # QGIS: geometrie vybranych hranic. Obsahuje cely obvod stanoviste;
  # na Sumave to muze znamenat velke GPKG soubory.
  gpkg <- base::file.path(out_dir, base::paste0("hranice_", code, ".gpkg"))
  if (base::file.exists(gpkg)) base::file.remove(gpkg)
  write_layer(site_sf, gpkg, "EVL")
  write_layer(target, gpkg, "CIL_stanoviste")
  write_layer(natural_same, gpkg, "SOUSED_stejne_SEGMENT_ID")
  write_layer(target_boundary, gpkg, "HRANICE_cile")
  write_layer(new_state_geom, gpkg, "NEW_kontakt_statni_hranice")
  write_layer(old_state_geom, gpkg, "OLD_kontakt_statni_hranice")
  write_layer(new_contact, gpkg, "NEW_kontakt_puvodni")
  write_layer(new_contact_without_id, gpkg, "NEW_kontakt_bez_stejneho_ID")
  write_layer(new_contact_strict, gpkg, "NEW_pouze_spolecne_hrany")
  write_layer(same_contact, gpkg, "NEW_kontakt_stejne_ID")
  write_layer(contact_non_edge, gpkg, "NEW_kontakt_prekryv")
  write_layer(old_contacts_all, gpkg, "OLD_pruniky_pred_slice")
  write_layer(old_contacts_kept, gpkg, "OLD_pruniky_po_slice")
  write_layer(old_contacts_lost, gpkg, "OLD_pruniky_ztracene")
  write_layer(state_local, gpkg, "STATNI_hrana_okoli_EVL")
  write_layer(intersect_geom(target_boundary, state_near), gpkg,
              "KANDIDAT_hranice_do_10m")
  
  # Orientacni obrazek detailu statni hranice.
  img <- base::file.path(out_dir, base::paste0("nahled_", code, ".png"))
  grDevices::png(img, width = 1600, height = 1200, res = 160)
  base::tryCatch({
    graphics::par(mar = base::c(1, 1, 4, 1))
    graphics::plot(sf::st_geometry(site_sf), border = "grey65", col = NA,
                   main = base::paste0("Sumava / ", code, " (zelena = prirodni kontakt)"))
    graphics::plot(sf::st_geometry(target), add = TRUE, col = "#9ABDE2",
                   border = "#426D9D", lwd = 0.4)
    graphic_contact <- line_geom(new_contact_without_id)
    if (base::length(graphic_contact) > 0L) {
      graphics::plot(graphic_contact, add = TRUE, col = "#1B8F50", lwd = 1.4)
    }
    graphic_border <- line_geom(new_state_geom)
    if (base::length(graphic_border) > 0L) {
      graphics::plot(graphic_border, add = TRUE, col = "#D33C33", lwd = 2.2)
    }
    graphics::legend("bottomleft", bty = "n", cex = 0.9,
                     legend = base::c("hodnocene stanoviste", "prirodni kontakt (bez stejneho ID)",
                                      "shoda se statni hranici"),
                     col = base::c("#426D9D", "#1B8F50", "#D33C33"), lwd = 2)
  }, error = function(e) base::warning("PNG pro ", code, ": ",
                                       base::conditionMessage(e)),
  finally = grDevices::dev.off())
  
  base::list(summary = summary, details = details)
}

# 6. BEH A EXPORT ---------------------------------------------
results <- base::lapply(selected, function(code) {
  base::tryCatch(run_diagnostic(code), error = function(e) {
    base::message("CHYBA ", code, ": ", base::conditionMessage(e))
    base::list(error = base::conditionMessage(e), HABITAT_CODE = code)
  })
})
valid <- base::Filter(function(x) !"error" %in% base::names(x), results)
failed <- base::Filter(function(x) "error" %in% base::names(x), results)
if (base::length(valid) > 0L) {
  summary <- dplyr::bind_rows(base::lapply(valid, `[[`, "summary"))
  details <- dplyr::bind_rows(base::lapply(valid, `[[`, "details"))
  readr::write_csv(summary, base::file.path(out_dir, "souhrn_variant.csv"))
  readr::write_csv(details, base::file.path(out_dir, "detail_delky.csv"))
  base::print(summary)
  base::print(details)
}
log <- dplyr::bind_rows(base::lapply(results, function(x) {
  dplyr::tibble(HABITAT_CODE = if ("error" %in% base::names(x)) x$HABITAT_CODE else
    x$summary$HABITAT_CODE[[1]],
    status = if ("error" %in% base::names(x)) "error" else "ok",
    error_message = if ("error" %in% base::names(x)) x$error else NA_character_)
}))
readr::write_csv(log, base::file.path(out_dir, "diagnosticky_log.csv"))
if (base::length(failed) > 0L) base::warning("Nektere testy selhaly, viz diagnosticky_log.csv")
base::message("HOTOVO: ", out_dir)
