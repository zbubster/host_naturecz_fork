# load_stanoviste_inputs.R
#
# Vstupní adaptér mezi existující config vrstvou repozitáře (R/00_config)
# a workflow hodnocení stanovišť.
#
# Zásady:
#   - původní config skripty ani load_vmb.R se NEMĚNÍ,
#   - configy se sourcují do samostatného prostředí,
#   - load_vmb() se používá v původním rozhraní,
#   - zde se pouze sjednotí názvy sloupců potřebné novými funkcemi,
#   - výsledkem jsou přesně dva objekty: `data` a `tables`.
#
# Výstup:
#   list(
#     data = ...,
#     tables = ...,
#     manifest = ...
#   )

load_stanoviste_inputs <- function(
    repo_root = ".",
    config_env = NULL,
    keep_config_env = FALSE
) {
  
  repo_root <- base::normalizePath(
    repo_root,
    winslash = "/",
    mustWork = TRUE
  )
  
  old_wd <- base::getwd()
  base::on.exit(
    base::setwd(old_wd),
    add = TRUE
  )
  
  # Existující configy používají relativní cesty vůči kořeni repozitáře.
  base::setwd(repo_root)
  
  # ---------------------------------------------------------------------------
  # 1. Source existující config vrstvy
  # ---------------------------------------------------------------------------
  
  if (base::is.null(config_env)) {
    
    config_env <- base::new.env(
      parent = base::globalenv()
    )
    
    config_files <- base::c(
      "R/00_config/00_n2k_config.R",
      "R/00_config/02_n2k_data_druhy.R",
      "R/00_config/03_n2k_data_stanoviste.R",
      "R/00_config/load_vmb.R"
    )
    
    missing_config_files <- config_files[
      !base::file.exists(config_files)
    ]
    
    if (base::length(missing_config_files) > 0) {
      base::stop(
        "load_stanoviste_inputs(): chybí config soubory: ",
        base::paste(
          missing_config_files,
          collapse = ", "
        ),
        call. = FALSE
      )
    }
    
    base::invisible(
      base::lapply(
        config_files,
        FUN = function(file) {
          base::message(
            "Sourcing: ",
            file
          )
          
          base::sys.source(
            file,
            envir = config_env
          )
        }
      )
    )
  }
  
  # ---------------------------------------------------------------------------
  # 2. Pomocné funkce
  # ---------------------------------------------------------------------------
  
  resolve_object <- function(
    candidates,
    label
  ) {
    
    found <- candidates[
      base::vapply(
        candidates,
        FUN = function(x) {
          base::exists(
            x,
            envir = config_env,
            inherits = FALSE
          )
        },
        FUN.VALUE = base::logical(1)
      )
    ]
    
    if (base::length(found) == 0) {
      
      available <- base::ls(
        config_env,
        all.names = TRUE
      )
      
      base::stop(
        "load_stanoviste_inputs(): po načtení configů se nepodařilo najít `",
        label,
        "`. Hledané názvy: ",
        base::paste(candidates, collapse = ", "),
        ".\nDostupné objekty v config prostředí:\n",
        base::paste(
          available,
          collapse = ", "
        ),
        call. = FALSE
      )
    }
    
    selected <- found[[1]]
    
    base::list(
      name = selected,
      value = base::get(
        selected,
        envir = config_env,
        inherits = FALSE
      )
    )
  }
  
  add_alias <- function(
    x,
    target,
    candidates,
    object_name,
    required = TRUE
  ) {
    
    if (target %in% base::names(x)) {
      return(x)
    }
    
    source <- candidates[
      candidates %in% base::names(x)
    ]
    
    if (base::length(source) == 0) {
      
      if (base::isTRUE(required)) {
        base::stop(
          "load_stanoviste_inputs(): v `",
          object_name,
          "` nelze vytvořit sloupec `",
          target,
          "`. Hledány byly alternativy: ",
          base::paste(candidates, collapse = ", "),
          ".",
          call. = FALSE
        )
      }
      
      return(x)
    }
    
    x[[target]] <- x[[source[[1]]]]
    x
  }
  
  normalize_vmb_base <- function(
    x,
    object_name
  ) {
    
    if (!base::inherits(x, "sf")) {
      base::stop(
        "load_stanoviste_inputs(): `",
        object_name,
        "` musí být sf objekt.",
        call. = FALSE
      )
    }
    
    x <- add_alias(
      x = x,
      target = "REGION_ID",
      candidates = base::c(
        "REGION_ID.x",
        "REGION_ID_X",
        "REGION_ID.y",
        "REGION_ID_Y"
      ),
      object_name = object_name
    )
    
    x <- add_alias(
      x = x,
      target = "DATUM",
      candidates = base::c(
        "DATUM.x",
        "DATUM_X",
        "DATUM.y",
        "DATUM_Y"
      ),
      object_name = object_name
    )
    
    x <- add_alias(
      x = x,
      target = "OBJECTID",
      candidates = base::c(
        "OBJECTID.y",
        "OBJECTID_Y",
        "OBJECTID.x",
        "OBJECTID_X",
        "OBJECTID_1"
      ),
      object_name = object_name,
      required = FALSE
    )
    
    if ("HABITAT" %in% base::names(x)) {
      x$HABITAT <- base::as.character(
        x$HABITAT
      )
    }
    
    if ("BIOTOP" %in% base::names(x)) {
      x$BIOTOP <- base::as.character(
        x$BIOTOP
      )
    }
    
    if (
      "DATUM" %in% base::names(x) &&
      !base::inherits(x$DATUM, "Date")
    ) {
      x$DATUM <- base::as.Date(
        x$DATUM
      )
    }
    
    x
  }
  
  normalize_vmb_update <- function(
    x,
    object_name
  ) {
    
    if (!base::inherits(x, "sf")) {
      base::stop(
        "load_stanoviste_inputs(): `",
        object_name,
        "` musí být sf objekt.",
        call. = FALSE
      )
    }
    
    x <- add_alias(
      x = x,
      target = "REGION_ID",
      candidates = base::c(
        "REGION_ID.x",
        "REGION_ID_X",
        "REGION_ID.y",
        "REGION_ID_Y"
      ),
      object_name = object_name
    )
    
    x <- add_alias(
      x = x,
      target = "DATUM",
      candidates = base::c(
        "DATUM.x",
        "DATUM_X",
        "DATUM.y",
        "DATUM_Y"
      ),
      object_name = object_name
    )
    
    # Pro pravidlo X11/X12 potřebujeme rok aktualizace z novější DBF strany.
    # U joinů load_vmb() bývá typicky ROK_AKT.y.
    x <- add_alias(
      x = x,
      target = "ROK_AKT",
      candidates = base::c(
        "ROK_AKT.y",
        "ROK_AKT_Y",
        "ROK_AKT.x",
        "ROK_AKT_X"
      ),
      object_name = object_name
    )
    
    if ("BIOTOP" %in% base::names(x)) {
      x$BIOTOP <- base::as.character(
        x$BIOTOP
      )
    }
    
    if (
      "DATUM" %in% base::names(x) &&
      !base::inherits(x$DATUM, "Date")
    ) {
      x$DATUM <- base::as.Date(
        x$DATUM
      )
    }
    
    x
  }
  
  make_vmb_meta <- function(
    x,
    object_name
  ) {
    
    required <- base::c(
      "REGION_ID",
      "DATUM"
    )
    
    missing <- base::setdiff(
      required,
      base::names(x)
    )
    
    if (base::length(missing) > 0) {
      base::stop(
        "load_stanoviste_inputs(): z `",
        object_name,
        "` nelze vytvořit VMB metadata; chybí: ",
        base::paste(missing, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    
    x |>
      sf::st_drop_geometry() |>
      dplyr::transmute(
        REGION_ID = base::as.character(REGION_ID),
        DATUM = base::as.Date(DATUM)
      ) |>
      dplyr::filter(
        !base::is.na(REGION_ID)
      ) |>
      dplyr::distinct()
  }
  
  # ---------------------------------------------------------------------------
  # 3. Objekty z existujících configů
  # ---------------------------------------------------------------------------
  
  site_res <- resolve_object(
    candidates = base::c(
      "evl",
      "evl_sjtsk"
    ),
    label = "vrstva EVL"
  )
  
  minimisize_res <- resolve_object(
    candidates = base::c(
      "minimisize"
    ),
    label = "tabulka minimiareálů"
  )
  
  habitat_areas_res <- resolve_object(
    candidates = base::c(
      "habitat_areas_2022",
      "habitat_areas_a1"
    ),
    label = "tabulka celkových ploch habitatů"
  )
  
  red_list_res <- resolve_object(
    candidates = base::c(
      "red_list_species"
    ),
    label = "data druhů červeného seznamu"
  )
  
  invasive_res <- resolve_object(
    candidates = base::c(
      "invasive_species"
    ),
    label = "data invazních druhů"
  )
  
  expansive_res <- resolve_object(
    candidates = base::c(
      "expansive_species"
    ),
    label = "data expanzních druhů"
  )
  
  site <- site_res$value
  
  # Nový KLIC očekává SHAPE_AREA. Pokud ji config vrstva nemá, dopočítáme ji
  # z již načtené geometrie, nikoli z nového datového zdroje.
  if (!"SHAPE_AREA" %in% base::names(site)) {
    
    if ("SHAPE.AREA" %in% base::names(site)) {
      
      site$SHAPE_AREA <- base::as.numeric(
        site$SHAPE.AREA
      )
      
    } else {
      
      if (sf::st_is_longlat(site)) {
        base::stop(
          "load_stanoviste_inputs(): EVL nemá `SHAPE_AREA` a je v geografickém CRS.",
          call. = FALSE
        )
      }
      
      site$SHAPE_AREA <- units::drop_units(
        sf::st_area(site)
      )
    }
  }
  
  # Hranice ČR: preferujeme přímo czechia_line, pokud ji stanovistní config
  # vytvořil. Pokud je dostupný jen polygon `czechia`, odvodíme z něj hranici.
  if (
    base::exists(
      "czechia_line",
      envir = config_env,
      inherits = FALSE
    )
  ) {
    
    czechia_line_name <- "czechia_line"
    
    czechia_line <- base::get(
      czechia_line_name,
      envir = config_env,
      inherits = FALSE
    )
    
  } else if (
    base::exists(
      "czechia",
      envir = config_env,
      inherits = FALSE
    )
  ) {
    
    czechia_line_name <- "czechia -> st_boundary(st_union())"
    
    czechia <- base::get(
      "czechia",
      envir = config_env,
      inherits = FALSE
    )
    
    czechia_line <- czechia |>
      sf::st_geometry() |>
      sf::st_union() |>
      sf::st_boundary()
    
  } else {
    
    base::stop(
      "load_stanoviste_inputs(): configy nevytvořily `czechia_line` ani `czechia`.",
      call. = FALSE
    )
  }
  
  # ---------------------------------------------------------------------------
  # 4. VMB přes původní load_vmb()
  # ---------------------------------------------------------------------------
  
  if (
    !base::exists(
      "load_vmb",
      envir = config_env,
      mode = "function",
      inherits = FALSE
    )
  ) {
    base::stop(
      "load_stanoviste_inputs(): po source `load_vmb.R` není dostupná funkce `load_vmb()`.",
      call. = FALSE
    )
  }
  
  load_vmb_fun <- base::get(
    "load_vmb",
    envir = config_env,
    inherits = FALSE
  )
  
  base::message(
    "Načítám VMB0 přes původní load_vmb()..."
  )
  vmb0 <- load_vmb_fun(
    vmb_x = 0
  )
  
  base::message(
    "Načítám VMB1 přes původní load_vmb()..."
  )
  vmb1 <- load_vmb_fun(
    vmb_x = 1
  )
  
  base::message(
    "Načítám VMB2 přes původní load_vmb()..."
  )
  vmb2 <- load_vmb_fun(
    vmb_x = 2
  )
  
  required_vmb0 <- base::c(
    "vmb_shp_sjtsk_akt",
    "vmb_pb_x_akt"
  )
  
  required_vmb1 <- base::c(
    "vmb_shp_sjtsk_orig"
  )
  
  required_vmb2 <- base::c(
    "vmb_shp_sjtsk_a1",
    "vmb_pb_x_a1"
  )
  
  check_vmb_list <- function(
    x,
    required,
    version
  ) {
    
    missing <- base::setdiff(
      required,
      base::names(x)
    )
    
    if (base::length(missing) > 0) {
      base::stop(
        "load_stanoviste_inputs(): load_vmb(",
        version,
        ") nevrátil: ",
        base::paste(missing, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    
    base::invisible(TRUE)
  }
  
  check_vmb_list(
    vmb0,
    required_vmb0,
    "vmb_x = 0"
  )
  
  check_vmb_list(
    vmb1,
    required_vmb1,
    "vmb_x = 1"
  )
  
  check_vmb_list(
    vmb2,
    required_vmb2,
    "vmb_x = 2"
  )
  
  vmb_current <- normalize_vmb_base(
    vmb0$vmb_shp_sjtsk_akt,
    "VMB0$vmb_shp_sjtsk_akt"
  )
  
  vmb1_base <- normalize_vmb_base(
    vmb1$vmb_shp_sjtsk_orig,
    "VMB1$vmb_shp_sjtsk_orig"
  )
  
  vmb2_base <- normalize_vmb_base(
    vmb2$vmb_shp_sjtsk_a1,
    "VMB2$vmb_shp_sjtsk_a1"
  )
  
  vmb0_update <- normalize_vmb_update(
    vmb0$vmb_pb_x_akt,
    "VMB0$vmb_pb_x_akt"
  )
  
  vmb2_update <- normalize_vmb_update(
    vmb2$vmb_pb_x_a1,
    "VMB2$vmb_pb_x_a1"
  )
  
  # Metadata-first výběr dvojice VMB nepotřebuje znovu číst DBF.
  # REGION_ID + DATUM odvodíme z hlavních VMB objektů, které load_vmb()
  # již sestavil.
  vmb0_meta <- make_vmb_meta(
    vmb_current,
    "VMB0"
  )
  
  vmb1_meta <- make_vmb_meta(
    vmb1_base,
    "VMB1"
  )
  
  vmb2_meta <- make_vmb_meta(
    vmb2_base,
    "VMB2"
  )
  
  # ---------------------------------------------------------------------------
  # 5. Výstupní struktura pro stanoviste_eval()
  # ---------------------------------------------------------------------------
  
  data <- base::list(
    site = site,
    vmb = vmb_current,
    czechia_line = czechia_line,
    vmb1_base = vmb1_base,
    vmb2_base = vmb2_base,
    vmb2_update = vmb2_update,
    vmb0_update = vmb0_update
  )
  
  tables <- base::list(
    habitat_areas = habitat_areas_res$value,
    minimisize = minimisize_res$value,
    red_list_species = red_list_res$value,
    invasive_species = invasive_res$value,
    expansive_species = expansive_res$value,
    vmb1_meta = vmb1_meta,
    vmb2_meta = vmb2_meta,
    vmb0_meta = vmb0_meta
  )
  
  manifest <- dplyr::tibble(
    target = base::c(
      "data$site",
      "data$vmb",
      "data$czechia_line",
      "data$vmb1_base",
      "data$vmb2_base",
      "data$vmb2_update",
      "data$vmb0_update",
      "tables$habitat_areas",
      "tables$minimisize",
      "tables$red_list_species",
      "tables$invasive_species",
      "tables$expansive_species",
      "tables$vmb1_meta",
      "tables$vmb2_meta",
      "tables$vmb0_meta"
    ),
    source = base::c(
      base::paste0(
        "R/00_config: ",
        site_res$name
      ),
      "load_vmb(vmb_x = 0)$vmb_shp_sjtsk_akt",
      base::paste0(
        "R/00_config: ",
        czechia_line_name
      ),
      "load_vmb(vmb_x = 1)$vmb_shp_sjtsk_orig",
      "load_vmb(vmb_x = 2)$vmb_shp_sjtsk_a1",
      "load_vmb(vmb_x = 2)$vmb_pb_x_a1",
      "load_vmb(vmb_x = 0)$vmb_pb_x_akt",
      base::paste0(
        "R/00_config: ",
        habitat_areas_res$name
      ),
      base::paste0(
        "R/00_config: ",
        minimisize_res$name
      ),
      base::paste0(
        "R/00_config: ",
        red_list_res$name
      ),
      base::paste0(
        "R/00_config: ",
        invasive_res$name
      ),
      base::paste0(
        "R/00_config: ",
        expansive_res$name
      ),
      "odvozeno z VMB1 base: REGION_ID + DATUM",
      "odvozeno z VMB2 base: REGION_ID + DATUM",
      "odvozeno z VMB0 current: REGION_ID + DATUM"
    )
  )
  
  result <- base::list(
    data = data,
    tables = tables,
    manifest = manifest
  )
  
  if (base::isTRUE(keep_config_env)) {
    result$config_env <- config_env
  }
  
  result
}
