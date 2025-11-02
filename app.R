# app.R — DRC Provinces & Biobank Snapshot (minimal)
# ----------------------------------------------

suppressPackageStartupMessages({
  library(shiny)
  library(leaflet)
  library(sf)
  library(dplyr)
  library(readxl)
  library(RColorBrewer)
  library(htmltools)
  library(tibble)
})

options(shiny.maxRequestSize = 200 * 1024^2)

# -----------------------------------------------------------------------------
# 1) Helpers
# -----------------------------------------------------------------------------
keyify <- function(x){
  x_chr <- as.character(x)
  x_chr[is.na(x_chr)] <- ""
  x_chr <- iconv(x_chr, from = "", to = "ASCII//TRANSLIT")
  x_chr <- tolower(trimws(gsub("[^a-z0-9]+", " ", x_chr)))
  x_chr
}

list_excel <- function(dir_biobank){
  if (!dir.exists(dir_biobank)) return(character())
  list.files(dir_biobank, pattern = "\\.xlsx?$", full.names = TRUE)
}

latest_biobank <- function(dir_biobank){
  files <- list_excel(dir_biobank)
  if (!length(files)) return(NULL)
  file_path <- files[which.max(file.info(files)$mtime)]
  df <- tryCatch(readxl::read_excel(file_path, .name_repair = "minimal") |> janitor::clean_names(), error = function(e) NULL)
  list(path = file_path, data = df)
}

keyify <- function(x){
  x <- as.character(x); x[is.na(x)] <- ""
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  tolower(trimws(gsub("[^a-z0-9]+", " ", x)))
}

repair_column_names <- function(nms){
  if (is.null(nms)) return(nms)
  nms <- as.character(nms)
  blanks <- which(is.na(nms) | trimws(nms) == "")
  if (length(blanks)) {
    nms[blanks] <- paste0("unnamed_", seq_along(blanks))
  }
  make.unique(nms, sep = "_")
}

first_existing <- function(paths){
  paths <- unique(paths)
  paths[file.exists(paths)]
}

read_sf_any <- function(path){
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("gpkg", "geojson", "json", "shp")) {
    suppressMessages(sf::read_sf(path, quiet = TRUE))
  } else if (ext %in% c("rds", "rda")) {
    readRDS(path)
  } else {
    stop(sprintf("Unsupported spatial format: %s", basename(path)))
  }
}

# Load health zones (gpkg/rds/geojson), normalise names/CRS, build province layer
load_health_zones <- function(){
  candidates <- c(
    Sys.getenv("GRID3_HEALTH_ZONES_FILE"),
    file.path("data", "grid3_health_zones.gpkg"),
    file.path("data", "grid3_health_zones.rds"),
    file.path("data", "grid3_health_zones.geojson"),
    file.path("data", "health_zones_offline.geojson"),
    file.path("www", "grid3_health_zones.gpkg"),
    file.path("www", "grid3_health_zones.rds"),
    file.path("www", "grid3_health_zones.geojson")
  )
  candidates <- first_existing(candidates)
  if (!length(candidates)) {
    stop("Place a GRID3 health-zone layer in data/ or www/ (gpkg, rds, geojson).")
  }

  g <- read_sf_any(candidates[1])
  stopifnot(inherits(g, "sf"))

  nm <- names(g)
  i_prov <- which(tolower(nm) == "province")[1]
  if (is.na(i_prov)) stop("Couldn't find a 'province' column in the zones layer.")
  names(g)[i_prov] <- "province"

  i_zone <- which(tolower(nm) %in% c("zonesante", "zone_sante", "zone", "health_zone"))[1]
  if (is.na(i_zone)) stop("Couldn't find a 'zonesante' / health-zone column.")
  names(g)[i_zone] <- "zonesante"

  geom_col <- attr(g, "sf_column")
  if (!identical(geom_col, "geometry")) {
    names(g)[names(g) == geom_col] <- "geometry"
    attr(g, "sf_column") <- "geometry"
  }

  g <- sf::st_make_valid(g)
  g <- g[!sf::st_is_empty(g), ]

  if (is.na(sf::st_crs(g))) sf::st_crs(g) <- 4326
  g <- sf::st_transform(g, 4326)

  g <- mutate(g, prov_key = keyify(province), zone_key = keyify(zonesante))

  g3857 <- sf::st_transform(g, 3857)
  geom_col2 <- attr(g3857, "sf_column")
  if (!identical(geom_col2, "geometry")) {
    names(g3857)[names(g3857) == geom_col2] <- "geometry"
    attr(g3857, "sf_column") <- "geometry"
  }

  provinces <- g3857 %>%
    group_by(province, prov_key) %>%
    summarise(geometry = sf::st_union(geometry), .groups = "drop") %>%
    sf::st_as_sf() %>%
    sf::st_transform(4326) %>%
    sf::st_make_valid()

  provs <- sort(unique(g$province))
  pal <- RColorBrewer::brewer.pal(min(max(3, min(length(provs), 12)), 12), "Set3")
  cols <- setNames(rep(pal, length.out = length(provs)), provs)
  g$prov_col <- cols[match(g$province, names(cols))]

  list(zones = g, provinces = provinces, cols = cols, source = candidates[1])
}

normalise_biobank <- function(df){
  if (is.null(df)) return(tibble())
  df <- tibble::as_tibble(df)
  nms <- tolower(names(df))
  names(df) <- nms

  get_match <- function(options){
    for (opt in options) {
      idx <- which(nms == opt)
      if (length(idx)) return(names(df)[idx[1]])
    }
    NULL
  }

  rename_map <- list(
    province = get_match(c("province", "prov", "adm1", "province_name", "name_1")),
    study    = get_match(c("study", "etude", "programme", "project")),
    age      = get_match(c("age", "age_years", "age_num", "age_yrs")),
    sex      = get_match(c("sex", "sexe", "gender"))
  )

  for (nm in names(rename_map)) {
    col <- rename_map[[nm]]
    if (!is.null(col)) {
      df <- dplyr::rename(df, !!nm := dplyr::all_of(col))
    } else {
      df[[nm]] <- NA
    }
  }

  df |> dplyr::mutate(
    province = trimws(as.character(province)),
    study    = trimws(as.character(study)),
    age      = suppressWarnings(as.numeric(age)),
    sex      = toupper(trimws(as.character(sex)))
  )
}

flatten_grid3_cols <- function(x){
  stopifnot(inherits(x, "sf"))
  geom_col <- attr(x, "sf_column")
  list_cols <- names(x)[vapply(x, is.list, TRUE)]
  list_cols <- setdiff(list_cols, geom_col)
  for (nm in list_cols) {
    x[[nm]] <- vapply(
      x[[nm]],
      FUN.VALUE = character(1),
      FUN = function(v){
        if (is.null(v) || length(v) == 0) return(NA_character_)
        v <- unlist(v, use.names = FALSE)
        paste(as.character(v), collapse = "; ")
      }
    )
  }
  x
}

prepare_zone_layers <- function(sf_obj, source = NULL){
  if (is.null(sf_obj) || !inherits(sf_obj, "sf")) return(NULL)

  g <- sf_obj
  geom_col <- attr(g, "sf_column")
  if (!identical(geom_col, "geometry")) {
    names(g)[names(g) == geom_col] <- "geometry"
    attr(g, "sf_column") <- "geometry"
  }

  nm_lower <- tolower(names(g))
  find_col <- function(priority, fallback_pattern){
    idx <- which(nm_lower %in% priority)
    if (length(idx)) return(idx[1])
    if (!is.null(fallback_pattern)) {
      idx <- which(grepl(fallback_pattern, nm_lower, fixed = FALSE))
      if (length(idx)) return(idx[1])
    }
    NA_integer_
  }

  prov_idx <- find_col(
    c("province", "prov", "adm1name", "adm1", "name_1", "adm_1", "adm1_fr"),
    "prov"
  )
  zone_idx <- find_col(
    c("zonesante", "zone_sante", "zone", "health_zone", "zones", "zs", "zs_name", "name_2", "adm2name", "adm2"),
    "zone"
  )

  if (is.na(prov_idx) || is.na(zone_idx)) {
    stop("Kon kolommen voor provincie en zone niet bepalen in kaartlaag.")
  }

  prov_col <- names(g)[prov_idx]
  zone_col <- names(g)[zone_idx]

  g$province <- as.character(g[[prov_col]])
  g$zonesante <- as.character(g[[zone_col]])
  g$province[g$province %in% c("", "NA")] <- NA_character_
  g$zonesante[g$zonesante %in% c("", "NA")] <- NA_character_
  g$prov_key_default <- keyify(g$province)
  g$zone_key_default <- keyify(g$zonesante)

  g <- sf::st_make_valid(g)
  g <- g[!sf::st_is_empty(g), ]

  if (is.na(sf::st_crs(g))) sf::st_crs(g) <- 4326
  g <- sf::st_transform(g, 4326)

  g3857 <- sf::st_transform(g, 3857)
  geom_col_3857 <- attr(g3857, "sf_column")
  if (!identical(geom_col_3857, "geometry")) {
    names(g3857)[names(g3857) == geom_col_3857] <- "geometry"
    attr(g3857, "sf_column") <- "geometry"
  }

  provinces <- g3857 |>
    dplyr::mutate(
      province = g$province,
      prov_key = keyify(province)
    ) |>
    dplyr::filter(!is.na(province) & province != "") |>
    dplyr::group_by(province, prov_key) |>
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop") |>
    sf::st_as_sf() |>
    sf::st_transform(4326) |>
    sf::st_make_valid()

  provs <- sort(unique(stats::na.omit(g$province)))
  cols <- character()
  if (length(provs)) {
    pal <- RColorBrewer::brewer.pal(min(max(3, min(length(provs), 12)), 12), "Set3")
    cols <- setNames(rep(pal, length.out = length(provs)), provs)
    g$prov_col_default <- cols[match(g$province, names(cols))]
  } else {
    g$prov_col_default <- NA_character_
  }

  attr(g, "source_path") <- source

  list(
    zones = g,
    provinces = provinces,
    cols = cols,
    source = source,
    province_col = prov_col,
    zone_col = zone_col
  )
}

read_geo_path <- function(path){
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(NULL)
  }

  ext <- tolower(tools::file_ext(path))
  sf_obj <- NULL

  if (ext == "rds") {
    sf_obj <- try(readRDS(path), silent = TRUE)
  } else if (ext == "zip") {
    unzip_dir <- tempfile("grid3_zip")
    dir.create(unzip_dir, showWarnings = FALSE, recursive = TRUE)
    unzip_res <- try(utils::unzip(path, exdir = unzip_dir), silent = TRUE)
    if (inherits(unzip_res, "try-error")) {
      return(NULL)
    }
    shp_files <- list.files(unzip_dir, pattern = "\\.shp$", full.names = TRUE)
    alt_files <- c(
      list.files(unzip_dir, pattern = "\\.gpkg$", full.names = TRUE),
      list.files(unzip_dir, pattern = "\\.geojson$", full.names = TRUE),
      list.files(unzip_dir, pattern = "\\.json$", full.names = TRUE)
    )
    target_candidates <- c(shp_files, alt_files)
    target <- if (length(target_candidates)) target_candidates[1] else unzip_dir
    sf_obj <- try(sf::read_sf(target, quiet = TRUE), silent = TRUE)
    if (inherits(sf_obj, "try-error")) {
      sf_obj <- try(sf::st_read(target, quiet = TRUE, stringsAsFactors = FALSE), silent = TRUE)
    }
  } else if (ext %in% c("geojson", "json", "gpkg", "shp")) {
    sf_obj <- try(sf::read_sf(path, quiet = TRUE), silent = TRUE)
    if (inherits(sf_obj, "try-error")) {
      sf_obj <- try(sf::st_read(path, quiet = TRUE, stringsAsFactors = FALSE), silent = TRUE)
    }
  } else {
    sf_obj <- try(sf::read_sf(path, quiet = TRUE), silent = TRUE)
    if (inherits(sf_obj, "try-error")) {
      sf_obj <- try(sf::st_read(path, quiet = TRUE, stringsAsFactors = FALSE), silent = TRUE)
    }

    if (is.data.frame(df)) {
      names(df) <- repair_column_names(names(df))
    }

    df
  }

  tmp_path <- copy_with_ext(datapath, ext)
  read_geo_path(tmp_path)
}

read_default_health_zones_impl <- function(){
  candidates <- unique(c(
    Sys.getenv("GRID3_HEALTH_ZONES_FILE", unset = ""),
    file.path("data", "health_zones_offline.geojson"),
    file.path("data", "grid3_health_zones.geojson"),
    file.path("data", "grid3_health_zones.gpkg"),
    file.path("data", "grid3_health_zones.rds"),
    file.path("www", "grid3_health_zones.geojson"),
    file.path("www", "grid3_health_zones.gpkg"),
    file.path("www", "grid3_health_zones.rds")
  ))

  for (candidate in candidates) {
    sf_obj <- read_geo_path(candidate)
    if (is.null(sf_obj)) next
    sf_obj <- try(flatten_grid3_cols(sf_obj), silent = TRUE)
    if (inherits(sf_obj, "try-error")) next
    bundle <- try(prepare_zone_layers(sf_obj, source = candidate), silent = TRUE)
    if (inherits(bundle, "try-error") || is.null(bundle)) next
    return(bundle)
  }

  if (is.null(chosen)) {
    sample_df <- tibble::tribble(
      ~province,              ~study, ~age, ~sex,
      "Kasaï-Oriental",       "DA",  22,    "F",
      "Kasaï-Oriental",       "DA",  34,    "M",
      "Kinshasa",             "DP",  19,    "F",
      "Kinshasa",             "DA",  41,    "M",
      "Nord-Kivu",            "DA",  27,    "F",
      "Nord-Kivu",            "DP",  8,     "M",
      "Sud-Kivu",             "DP",  15,    "F"
    )
    return(list(path = "Sample data", data = sample_df, sample = TRUE))
  }

  chosen$sample <- FALSE
  chosen
}

biobank_summary <- function(df){
  if (is.null(df) || !nrow(df)) return(tibble())
  df |> normalise_biobank() |>
    mutate(
      province = ifelse(is.na(province) | province == "", "Unknown", province),
      study = ifelse(is.na(study) | study == "", "Unknown", study),
      prov_key = keyify(province)
    ) |>
    group_by(prov_key, province, study) |>
    summarise(records = dplyr::n(), .groups = "drop") |>
    arrange(province, study)
}

safe_total <- function(x){
  x <- suppressWarnings(as.numeric(x))
  x[is.na(x)] <- 0
  x
}

# -------- UI -----------------------------------------------------------------

ui <- fluidPage(
  tags$head(tags$style(HTML(".leaflet-container { background: #f7f7f7; }"))),
  titlePanel("DRC — Provinces, Health Zones & Biobank Snapshot"),
  fluidRow(
    column(
      width = 4,
      h4("Biobank source"),
      fileInput("upload", "Upload biobank file (Excel/CSV/RDS)",
                accept = c(".xlsx", ".xls", ".csv", ".rds")),
      verbatimTextOutput("biobank_info", placeholder = TRUE),
      h4("Biobank summary"),
      tableOutput("summary_table")
    ),
    column(
      width = 8,
      leafletOutput("map", height = "80vh")
    )
  )
)

# -----------------------------------------------------------------------------
# 4) Server
# -----------------------------------------------------------------------------
server <- function(input, output, session){

  # ---------- Helpers voor normaliseren van namen ----------
  normalize_names <- function(x){
    if (is.list(x)) {
      x <- vapply(x, function(v){
        if (length(v) == 0 || all(is.na(v))) return("")
        paste(as.character(unlist(v, use.names = FALSE)), collapse = " ")
      }, FUN.VALUE = character(1))
    }
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- stringi::stri_trans_general(x, "Latin-ASCII")
    x <- tolower(trimws(x))
    gsub("[\\s_-]+", " ", x)
  }

  used_file      <- reactiveVal("")
  chosen_file    <- reactiveVal(NULL)
  biobank_root   <- reactiveVal(NULL)
  zone_hover_id  <- reactiveVal(NULL)

  app_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)
  cache_dir <- file.path(app_dir, ".cache")
  cache_file <- file.path(cache_dir, "paths.rds")
  stored_paths <- reactiveVal(NULL)
  session$userData$bookmarking_active <- FALSE
  default_zones_sf <- reactiveVal(NULL)
  offline_notice_shown <- reactiveVal(FALSE)

  observeEvent(TRUE, {
    bundle <- read_default_health_zones()
    if (!is.null(bundle)) {
      default_zones_sf(bundle)
    }
  }, once = TRUE)

  observeEvent(TRUE, {
    paths <- NULL
    if (file.exists(cache_file)) {
      paths <- tryCatch(readRDS(cache_file), error = function(e) NULL)
    }
    if (is.list(paths)) {
      stored_paths(paths)
      if (!is.null(paths$root)) {
        updateTextInput(session, "root", value = paths$root)
      }
      if (!is.null(paths$qc_dir)) {
        updateTextInput(session, "qc-qc_dir", value = paths$qc_dir)
      }
    } else {
      stored_paths(list(root = isolate(input$root), qc_dir = NULL))
    }
  }, once = TRUE)

  observeEvent(input$root, {
    paths <- stored_paths(); if (!is.list(paths)) paths <- list()
    paths$root <- input$root
    stored_paths(paths)
  })

  output$filepick_ui <- renderUI({
    files <- list_excel(input$root)
    if (!length(files)) return(helpText("Geen Excel-bestanden in deze map."))
    nice <- basename(files)
    names(files) <- nice
    selectInput("filepick", "Kies bestand", choices = files)
  })

  observeEvent(input$filepick, {
    chosen_file(input$filepick)
  })

  observeEvent(input$refresh, {
    if (!dir.exists(input$root)) {
      showNotification("Map bestaat niet. Controleer het pad.", type = "error")
      return(NULL)
    }
    biobank_root(normalizePath(input$root, winslash = "/", mustWork = FALSE))
  }, ignoreInit = FALSE)

  raw_data <- reactive({
    if (!is.null(chosen_file())) {
      res <- read_specific_biobank(chosen_file())
    } else if (!is.null(biobank_root())) {
      res <- latest_biobank(biobank_root())
    } else {
      res <- NULL
    }
    if (is.null(res) || is.null(res$data)) {
      used_file("")
      return(NULL)
    }
    used_file(basename(res$path))
    res$data |> mutate(across(everything(), as.character))
  })

  output$used_file <- renderText({ used_file() })

  mapped_data <- reactive({
    df0 <- raw_data(); if (is.null(df0)) return(NULL)
    rename_by_regex(df0)
  })

  biobank <- reactive({
    dfm <- mapped_data(); if (is.null(dfm)) return(NULL)
    sla <- list(terrain = 7, cpltha = 7, inrb = 21) # SLA thresholds for transport segments
    df <- dfm |>
      mutate(
        date_prelev      = parse_any_date(date_prelev),
        date_env_cpltha  = parse_any_date(date_env_cpltha),
        date_rec_cpltha  = parse_any_date(date_rec_cpltha),
        date_env_inrb    = parse_any_date(date_env_inrb),
        Barcode = as.character(Barcode),
        LabID   = as.character(LabID)
      ) |>
      mutate(
        transp_terrain = safe_days(date_env_cpltha, date_prelev, max_ok = 30),
        transp_cpltha  = safe_days(date_rec_cpltha, date_env_cpltha, max_ok = 30),
        transp_inrb    = safe_days(date_env_inrb, date_rec_cpltha, max_ok = 90),
        flag_terrain = ifelse(!is.na(transp_terrain) & transp_terrain > sla$terrain, "Late", "OK"),
        flag_cpltha  = ifelse(!is.na(transp_cpltha)  & transp_cpltha  > sla$cpltha,  "Late", "OK"),
        flag_inrb    = ifelse(!is.na(transp_inrb)    & transp_inrb    > sla$inrb,    "Late", "OK")
      ) |>
      distinct(Barcode, LabID, .keep_all = TRUE) |>
      clean_age_sex() |>
      mutate(
        province_raw = province,
        zone_raw = zone,
        prov_key = normalize_names(province),
        zone_key = normalize_names(zone),
        structure_sanitaire = dplyr::na_if(trimws(as.character(structure_sanitaire)), ""),
        unite_mobile = dplyr::na_if(trimws(as.character(unite_mobile)), ""),
        structure_key = normalize_names(structure_sanitaire),
        mobile_key = normalize_names(unite_mobile),
        structure_key = dplyr::na_if(structure_key, ""),
        mobile_key = dplyr::na_if(mobile_key, "")
      )
    if (!nrow(df)) return(NULL)
    df
  })

  qc_module <- qcServer("qc", biobank = biobank)

  observeEvent(qc_module$qc_dir_input(), {
    paths <- stored_paths(); if (!is.list(paths)) paths <- list()
    paths$qc_dir <- qc_module$qc_dir_input()
    stored_paths(paths)
  })

  onStop(function(){
    paths <- stored_paths()
    if (is.list(paths)) {
      try({
        dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
        saveRDS(paths, cache_file)
      }, silent = TRUE)
    }
  })

  observe({
    df <- biobank(); if (is.null(df)) return()
    rng <- range(df$date_prelev, na.rm = TRUE)
    if (all(is.finite(rng))) {
      start <- max(rng[1], rng[2] - 180)
      end   <- rng[2]
      updateDateRangeInput(session, "daterng", start = start, end = end, min = rng[1], max = rng[2])
    }
    studies <- sort(unique(na.omit(df$study)))
    updateSelectInput(session, "study", choices = c("Alle", studies), selected = "Alle")
  })

  output$province_ui <- renderUI({
    df <- biobank(); if (is.null(df)) return(NULL)
    provs <- sort(unique(na.omit(df$province)))
    if (!length(provs)) return(helpText("Geen provincies gevonden."))
    selectizeInput("province", "Filter provincie", choices = provs, selected = provs, multiple = TRUE, options = list(plugins = list("remove_button")))
  })

  output$zone_ui <- renderUI({
    df <- biobank(); if (is.null(df)) return(NULL)
    zones <- sort(unique(na.omit(df$zone)))
    if (!length(zones)) return(helpText("Geen zones gevonden."))
    selectizeInput("zone", "Filter zone", choices = zones, selected = zones, multiple = TRUE, options = list(plugins = list("remove_button")))
  })

  output$structure_ui <- renderUI({
    df <- biobank(); if (is.null(df)) return(NULL)
    structures <- sort(unique(na.omit(df$structure_sanitaire)))
    if (!length(structures)) return(helpText("Geen structure sanitaire-waarden gevonden."))
    current <- isolate(input$structure)
    selected <- if (is.null(current) || !length(current)) structures else intersect(current, structures)
    if (!length(selected)) selected <- structures
    selectizeInput(
      "structure", "Filter structure sanitaire",
      choices = structures, selected = selected, multiple = TRUE,
      options = list(plugins = list("remove_button"))
    )
  })

  output$mobile_ui <- renderUI({
    df <- biobank(); if (is.null(df)) return(NULL)
    df_filtered <- df
    if (!is.null(input$structure) && length(input$structure)) {
      df_filtered <- df_filtered |> dplyr::filter(is.na(structure_sanitaire) | structure_sanitaire %in% input$structure)
    }
    units <- sort(unique(na.omit(df_filtered$unite_mobile)))
    if (!length(units)) return(helpText("Geen mobiele units gevonden."))
    current <- isolate(input$mobile_unit)
    selected <- if (is.null(current) || !length(current)) units else intersect(current, units)
    if (!length(selected)) selected <- units
    selectizeInput(
      "mobile_unit", "Filter unité mobile",
      choices = units, selected = selected, multiple = TRUE,
      options = list(plugins = list("remove_button"))
    )
  })

  transport_long <- reactive({
    df <- filtered()
    if (is.null(df) || !nrow(df)) return(NULL)
    df <- df |> dplyr::filter(!is.na(date_prelev))
    if (!nrow(df)) return(NULL)

    has_field_temp <- "temp_field" %in% names(df)
    has_cpltha_temp <- "temp_cpltha" %in% names(df)

    df |>
      transmute(
        sample_date = date_prelev,
        `Field → HS` = transp_terrain,
        `HS → LSD`  = transp_cpltha,
        `LSD → INRB` = transp_inrb,
        temp_field = if (has_field_temp) temp_field else NA_character_,
        temp_cpltha = if (has_cpltha_temp) temp_cpltha else NA_character_,
        flag_terrain, flag_cpltha, flag_inrb
      ) |>
      tidyr::pivot_longer(
        cols = c(`Field → HS`, `HS → LSD`, `LSD → INRB`),
        names_to = "segment",
        values_to = "days",
        values_drop_na = TRUE
      ) |>
      mutate(
        temp_code_raw = case_when(
          segment == "Field → HS" ~ temp_field,
          segment == "HS → LSD"  ~ temp_cpltha,
          TRUE ~ NA_character_
        ),
        temp_code_raw = toupper(trimws(temp_code_raw)),
        temp_code = dplyr::case_when(
          temp_code_raw %in% c("A", "AMB", "AMBANTE", "AMBIANTE", "AMBIENT", "AMBIA") ~ "Ambiante",
          temp_code_raw %in% c("F", "FR", "FRIGO", "FRIG") ~ "Frigo",
          temp_code_raw %in% c("C", "CONG", "CONGE", "CONGELATEUR", "FREEZER") ~ "Congélateur",
          is.na(temp_code_raw) | temp_code_raw == "" ~ "Onbekend",
          TRUE ~ temp_code_raw
        ),
        flag = dplyr::case_when(
          segment == "Field → HS" ~ flag_terrain,
          segment == "HS → LSD"  ~ flag_cpltha,
          TRUE ~ flag_inrb
        ),
        segment = factor(segment, levels = c("Field → HS", "HS → LSD", "LSD → INRB")),
        temp_code = factor(temp_code, levels = c("Ambiante", "Frigo", "Congélateur", "Onbekend")),
        week = lubridate::floor_date(sample_date, "week")
      )
  })

  output$summary_cards <- renderUI({
    df <- filtered()
    if (is.null(df) || !nrow(df)) {
      return(div(class = "alert alert-warning mb-0", "Geen stalen in de huidige selectie."))
    }

    n_all <- nrow(df)
    n_da <- sum(toupper(df$study) == "DA", na.rm = TRUE)
    n_dp <- sum(toupper(df$study) == "DP", na.rm = TRUE)
    n_f  <- sum(df$sex_clean == "F", na.rm = TRUE)
    pct_da <- ifelse(n_all > 0, round(100 * n_da / n_all, 1), NA_real_)
    pct_dp <- ifelse(n_all > 0, round(100 * n_dp / n_all, 1), NA_real_)
    pct_f  <- ifelse(n_all > 0, round(100 * n_f / n_all, 1), NA_real_)
    n_zones <- n_distinct(df$zone, na.rm = TRUE)
    last_sample <- suppressWarnings(max(df$date_prelev, na.rm = TRUE))

    fmt_int <- function(x) ifelse(is.na(x), "0", formatC(as.integer(x), format = "d", big.mark = " "))
    fmt_pct <- function(x) ifelse(is.na(x), "–", paste0(scales::number(x, accuracy = 0.1), "%"))
    fmt_date <- function(x) ifelse(is.na(x) || is.infinite(x), "–", format(x, "%d %b %Y"))

    stats <- list(
      list(label = "Totale stalen", value = fmt_int(n_all), sub = sprintf("Unieke zones: %s", fmt_int(n_zones))),
      list(label = "DA-stalen", value = fmt_int(n_da), sub = sprintf("%s van selectie", fmt_pct(pct_da))),
      list(label = "DP-stalen", value = fmt_int(n_dp), sub = sprintf("%s van selectie", fmt_pct(pct_dp))),
      list(label = "% vrouw", value = fmt_pct(pct_f), sub = sprintf("Vrouwen: %s", fmt_int(n_f))),
      list(label = "Laatste staal", value = fmt_date(last_sample), sub = "Op basis van date_prelev")
    )

    cards <- lapply(stats, function(x) {
      card(
        card_body(
          class = "summary-card",
          div(class = "summary-card-label", x$label),
          div(class = "summary-card-value", x$value),
          tags$small(class = "text-muted", x$sub)
        )
      )
    })

    do.call(layout_column_wrap, c(list(width = 1/3), cards))
  })

  output$empty_banner <- renderUI({
    df <- filtered()
    if (is.null(df) || !nrow(df)) {
      div(class = "alert alert-warning mb-2", "Geen stalen binnen de selectie. Vergroot het datumbereik om de kaart te vullen.")
    } else if (all(is.na(df$date_prelev))) {
      div(class = "alert alert-warning mb-2", "Alle date_prelev-waarden ontbreken binnen de selectie. Vergroot het datumbereik.")
    }
  })


  output$p_zone_container <- renderUI({
    if (isTRUE(input$p_interactive)) {
      withSpinner(plotlyOutput("p_zone_plotly", height = 650))
    } else {
      withSpinner(plotOutput("p_zone_plot", height = 650))
    }
  })

  output$p_zone_plot <- renderPlot({
    df <- filtered();
    req(!is.null(df), nrow(df) > 0, any(!is.na(df$date_prelev)))
    plot_zone_pyramids(df, bin = input$bin, min_n_per_zone = input$minN, split_study = input$split_study)
  })

  output$p_zone_plotly <- renderPlotly({
    req(isTRUE(input$p_interactive))
    df <- filtered();
    req(!is.null(df), nrow(df) > 0, any(!is.na(df$date_prelev)))
    p <- plot_zone_pyramids(df, bin = input$bin, min_n_per_zone = input$minN, split_study = input$split_study)
    plotly::ggplotly(p) # Switch to interactive mode when requested
  })

  output$p_plus_container <- renderUI({
    if (isTRUE(input$p_interactive)) {
      withSpinner(plotlyOutput("p_plus_plotly", height = 650))
    } else {
      withSpinner(plotOutput("p_plus_plot", height = 650))
    }
  })

  output$p_plus_plot <- renderPlot({
    df <- filtered();
    req(!is.null(df), nrow(df) > 0, any(!is.na(df$date_prelev)))
    plot_age_sex_pyramid_plus(df, facet_by = input$facet, bin = input$bin, split_study = input$split_study)
  })

  output$p_plus_plotly <- renderPlotly({
    req(isTRUE(input$p_interactive))
    df <- filtered();
    req(!is.null(df), nrow(df) > 0, any(!is.na(df$date_prelev)))
    p <- plot_age_sex_pyramid_plus(df, facet_by = input$facet, bin = input$bin, split_study = input$split_study)
    plotly::ggplotly(p) # Provide interactive "Plus" view on demand
  })

  output$doorlooptijd_cards <- renderUI({
    dat <- transport_long();
    if (is.null(dat) || !nrow(dat)) {
      return(helpText("Geen doorlooptijdgegevens binnen de huidige selectie."))
    }

    summ <- dat |>
      group_by(segment) |>
      summarise(
        n = dplyr::n(),
        median = stats::median(days, na.rm = TRUE),
        p95 = stats::quantile(days, 0.95, na.rm = TRUE, names = FALSE),
        .groups = "drop"
      )

    fmt_days <- function(x) {
      ifelse(is.na(x) | !is.finite(x), "–", scales::number(x, accuracy = 0.1, suffix = " d"))
    }
    fmt_int <- function(x) ifelse(is.na(x), "0", formatC(as.integer(x), format = "d", big.mark = " "))

    cards <- purrr::pmap(summ, function(segment, n, median, p95) {
      card(
        card_body(
          class = "summary-card",
          div(class = "summary-card-label", segment),
          div(class = "summary-card-value", fmt_days(median)),
          tags$small(sprintf("95e percentiel: %s · N = %s", fmt_days(p95), fmt_int(n)))
        )
      )
    })

    do.call(layout_column_wrap, c(list(width = 1/3), cards))
  })

  output$p_trans_time <- renderPlot({
    dat <- transport_long();
    validate(need(!is.null(dat) && nrow(dat) > 0, "Geen doorlooptijdgegevens."))

    weekly <- dat |>
      filter(!is.na(week)) |>
      group_by(segment, week) |>
      summarise(mean_days = mean(days, na.rm = TRUE), .groups = "drop")

    seg_cols <- setNames(c("#1b9e77", "#d95f02", "#7570b3"), levels(dat$segment))
    bg_rect <- tibble(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)

    ggplot(weekly, aes(week, mean_days, colour = segment)) +
      geom_rect(data = bg_rect,
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                inherit.aes = FALSE,
                fill = scales::alpha("#E8F6F3", 0.35),
                colour = NA) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_colour_manual(values = seg_cols, name = "Segment") +
      scale_x_date(date_labels = "%d %b", date_breaks = "2 weeks") +
      labs(x = NULL, y = "Gemiddelde dagen", title = "Gemiddelde doorlooptijd per week") +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = scales::alpha("#E8F6F3", 0.35), colour = NA),
        plot.background = element_rect(fill = "#FDFEFE", colour = NA)
      )
  })

  output$p_trans_dist <- renderPlot({
    dat <- transport_long();
    validate(need(!is.null(dat) && nrow(dat) > 0, "Geen doorlooptijdgegevens."))

    seg_cols <- setNames(c("#1b9e77", "#d95f02", "#7570b3"), levels(dat$segment))
    bg_df <- tibble(
      segment = factor(levels(dat$segment), levels = levels(dat$segment)),
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf,
      fill = scales::alpha(seg_cols[as.character(segment)], 0.25)
    )

    ggplot(dat, aes(sample_date, days, colour = segment)) +
      geom_rect(
        data = bg_df,
        mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
        inherit.aes = FALSE,
        colour = NA
      ) +
      scale_fill_identity(guide = "none") +
      geom_point(alpha = 0.55, size = 1.8) +
      facet_wrap(~segment, ncol = 1, scales = "free_y") +
      scale_colour_manual(values = seg_cols, guide = "none") +
      scale_x_date(date_labels = "%d %b", date_breaks = "2 weeks") +
      labs(x = NULL, y = "Dagen", title = "Doorlooptijd per staal over de tijd") +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(fill = "#F4F6F7", colour = NA)
      )
  })

  output$p_trans_box <- renderPlot({
    dat <- transport_long();
    validate(need(!is.null(dat) && nrow(dat) > 0, "Geen doorlooptijdgegevens."))

    seg_cols <- setNames(c("#1b9e77", "#d95f02", "#7570b3"), levels(dat$segment))
    shape_map <- c("Ambiante" = 21, "Frigo" = 24, "Congélateur" = 22, "Onbekend" = 16)
    bg_rect <- tibble(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)

    ggplot(dat, aes(segment, days, fill = segment)) +
      geom_rect(
        data = bg_rect,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        inherit.aes = FALSE,
        fill = scales::alpha("#FDF2E9", 0.4),
        colour = NA
      ) +
      geom_boxplot(width = 0.6, alpha = 0.85, colour = "grey30", outlier.shape = NA) +
      geom_jitter(aes(shape = temp_code), width = 0.1, height = 0, size = 2, alpha = 0.6) +
      scale_fill_manual(values = seg_cols, guide = "none") +
      scale_shape_manual(values = shape_map, name = "Temperatuur") +
      labs(x = NULL, y = "Dagen", title = "Verdeling per segment", subtitle = "Punten tonen temperatuurcategorie") +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 15, hjust = 1),
        panel.background = element_rect(fill = scales::alpha("#FDF2E9", 0.4), colour = NA),
        plot.background = element_rect(fill = "#FEF9F4", colour = NA)
      )
  })

  output$p_trans_flag <- renderPlot({
    dat <- transport_long()
    # Summarise transport flags to monitor SLA compliance
    validate(need(!is.null(dat) && nrow(dat) > 0, "Geen doorlooptijdgegevens."))
    flag_summary <- dat |>
      mutate(flag = factor(ifelse(is.na(flag), "OK", flag), levels = c("OK", "Late"))) |>
      count(segment, flag, name = "n") |>
      tidyr::complete(segment, flag, fill = list(n = 0))
    ggplot(flag_summary, aes(x = segment, y = n, fill = flag)) +
      geom_col(position = "stack") +
      scale_fill_manual(values = c(OK = "#1b9e77", Late = "#e74c3c"), name = "Status") +
      labs(title = "Transport SLA-status", x = NULL, y = "Aantal stalen") +
      theme_minimal(base_size = 12)
  })

  output$tbl <- renderDT({
    df <- filtered(); if (is.null(df)) df <- tibble()
    datatable(df |> select(Barcode, LabID, province, zone, study, sex_clean, age_num, date_prelev, date_env_cpltha, date_rec_cpltha, date_env_inrb, starts_with("transp_"), starts_with("flag_")), options = list(pageLength = 25, scrollX = TRUE))
  })

  filtered <- reactive({
    df <- biobank(); if (is.null(df)) return(df)
    rng <- input$daterng
    if (!is.null(rng)) df <- df |> filter(is.na(date_prelev) | (date_prelev >= rng[1] & date_prelev <= rng[2]))
    if (!is.null(input$province) && length(input$province)) df <- df |> filter(is.na(province) | province %in% input$province)
    if (!is.null(input$zone) && length(input$zone))       df <- df |> filter(is.na(zone) | zone %in% input$zone)
    if (!is.null(input$structure) && length(input$structure)) df <- df |> filter(is.na(structure_sanitaire) | structure_sanitaire %in% input$structure)
    if (!is.null(input$mobile_unit) && length(input$mobile_unit)) df <- df |> filter(is.na(unite_mobile) | unite_mobile %in% input$mobile_unit)
    if (!is.null(input$study) && input$study != "Alle")  df <- df |> filter(toupper(study) == toupper(input$study))
    if (!is.null(input$sex)) df <- df |> filter(is.na(sex_clean) | sex_clean %in% input$sex)
    if (!is.null(input$age_rng) && length(input$age_rng) == 2) {
      df <- df |> filter(is.na(age_num) | dplyr::between(age_num, input$age_rng[1], input$age_rng[2]))
    }
    df
  })

  bookmark_trigger <- debounce(reactive({ reactiveValuesToList(input) }), 1000) # Track input changes for URL bookmarking

  observe({
    bookmark_trigger()
    if (isTRUE(session$userData$bookmarking_active)) return()
    session$userData$bookmarking_active <- TRUE
    session$doBookmark() # Auto-bookmark current UI state
    session$userData$bookmarking_active <- FALSE
  })

  output$grid3_status <- renderUI({
    if (isTRUE(input$use_grid3)) {
      helpText("GRID3-laag wordt via internet geladen wanneer nodig.")
    } else {
      offline <- default_zones_sf()
      if (is.null(offline)) {
        helpText("Upload een GeoJSON/Shapefile of vink GRID3 aan.")
      } else {
        file_label <- offline$source
        if (!is.null(file_label) && nzchar(file_label)) {
          file_label <- basename(file_label)
        } else {
          file_label <- "ingebedde kaartlaag"
        }
        helpText(glue::glue("Offline kaartlaag beschikbaar: {file_label}."))
      }
    }
  })

  # ---------- Gezondheidszones kaart ----------
  zones_bundle <- reactive({
    if (isTRUE(input$use_grid3)) {
      shiny::withProgress(message = "GRID3-zones laden...", value = 0.5, {
        sf <- try(read_grid3_health_zones(), silent = TRUE)
        if (inherits(sf, "try-error")) {
          showNotification("GRID3-zones konden niet geladen worden (mogelijk ontbreekt internet of is toegang geweigerd).", type = "error")
          return(NULL)
        }
        sf <- try(flatten_grid3_cols(sf), silent = TRUE)
        if (inherits(sf, "try-error")) {
          showNotification("GRID3-kaartlaag kon niet verwerkt worden.", type = "error")
          return(NULL)
        }
        bundle <- try(prepare_zone_layers(sf, source = "GRID3"), silent = TRUE)
        if (inherits(bundle, "try-error") || is.null(bundle)) {
          msg <- if (inherits(bundle, "try-error")) conditionMessage(attr(bundle, "condition")) else "onbekende fout"
          showNotification(glue::glue("GRID3-kaartlaag kon niet voorbereid worden: {msg}"), type = "error")
          return(list(zones = sf, provinces = NULL, cols = character(), source = "GRID3"))
        }
        bundle
      })
    } else {
      sf <- read_uploaded_geo(input$zones_geo)
      if (is.null(sf)) {
        offline <- default_zones_sf()
        if (is.null(offline)) {
          showNotification("Geen offline kaartlaag gevonden. Upload een bestand of gebruik GRID3.", type = "error")
          return(NULL)
        }
        if (!isTRUE(offline_notice_shown())) {
          label <- offline$source
          if (!is.null(label) && nzchar(label)) {
            label <- basename(label)
          } else {
            label <- "ingebedde kaartlaag"
          }
          showNotification(glue::glue("Offline kaartlaag geladen: {label}."), type = "message")
          offline_notice_shown(TRUE)
        }
        offline
      } else {
        sf <- try(flatten_grid3_cols(sf), silent = TRUE)
        if (inherits(sf, "try-error")) {
          showNotification("Geüploade kaartlaag kon niet verwerkt worden.", type = "error")
          return(NULL)
        }
        label <- if (!is.null(input$zones_geo$name)) input$zones_geo$name else "upload"
        bundle <- try(prepare_zone_layers(sf, source = label), silent = TRUE)
        if (inherits(bundle, "try-error") || is.null(bundle)) {
          msg <- if (inherits(bundle, "try-error")) conditionMessage(attr(bundle, "condition")) else "onbekende fout"
          showNotification(glue::glue("Kaartlaag kon niet voorbereid worden: {msg}"), type = "error")
          return(list(zones = sf, provinces = NULL, cols = character(), source = label))
        }
        bundle
      }
    }
  })

  zones_sf <- reactive({
    bundle <- zones_bundle()
    if (is.null(bundle)) return(NULL)
    bundle$zones
  })

  observe({
    g <- zones_sf()
    bundle <- zones_bundle()
    if (is.null(g)) return()
    geom_col <- attr(g, "sf_column")

    simple_cols <- setdiff(names(g), geom_col)
    simple_cols <- simple_cols[ !vapply(g[simple_cols], is.list, TRUE) ]
    if (!length(simple_cols)) simple_cols <- setdiff(names(g), geom_col)

    cand_zone <- intersect(simple_cols, c("zone","zonesante","zs","zs_name","nom_zs","name","NAME","NOM"))
    cand_prov <- intersect(simple_cols, c("province","prov","nom_prov","adm1name","ADM1NAME","NAME_1","name_1"))

    default_zone <- NULL
    default_prov <- NULL
    if (!is.null(bundle$zone_col) && bundle$zone_col %in% simple_cols) default_zone <- bundle$zone_col
    if (!is.null(bundle$province_col) && bundle$province_col %in% simple_cols) default_prov <- bundle$province_col

    updateSelectInput(session, "zones_name_col",
      choices  = simple_cols,
      selected = if (!is.null(default_zone)) default_zone else if (length(cand_zone)) cand_zone[1] else simple_cols[1]
    )
    updateSelectInput(session, "prov_name_col",
      choices  = simple_cols,
      selected = if (!is.null(default_prov)) default_prov else if (length(cand_prov)) cand_prov[1] else simple_cols[1]
    )
  })

  zone_summary <- reactive({
    df <- filtered(); if (is.null(df) || !nrow(df)) return(NULL)
    df |>
      group_by(zone, province, zone_key, prov_key) |>
      summarise(
        n = n(),
        n_da = sum(toupper(study) == "DA", na.rm = TRUE),
        n_dp = sum(toupper(study) == "DP", na.rm = TRUE),
        n_f = sum(sex_clean == "F", na.rm = TRUE),
        pct_f = ifelse(n > 0, round(100 * n_f / n, 1), NA_real_),
        pct_da = ifelse(n > 0, round(100 * n_da / n, 1), NA_real_),
        pct_dp = ifelse(n > 0, round(100 * n_dp / n, 1), NA_real_),
        med_age = suppressWarnings(stats::median(age_num, na.rm = TRUE)),
        last_sample = suppressWarnings(max(date_prelev, na.rm = TRUE)),
        .groups = "drop"
      ) |>
      mutate(
        zone_uid = ifelse(is.na(zone_key) & is.na(prov_key), NA_character_, paste(zone_key, prov_key, sep = "__")),
        last_sample = dplyr::if_else(
          is.finite(last_sample),
          as.Date(last_sample, origin = "1970-01-01"),
          as.Date(NA)
        )
      )
  })

  mobile_units <- reactive({
    empty_units <- tibble(
      unit = character(),
      zone_label = character(),
      lat = numeric(),
      lon = numeric(),
      n = integer(),
      n_da = integer(),
      n_dp = integer(),
      last_sample = as.Date(character()),
      has_coords = logical()
    )

server <- function(input, output, session){
  hz <- load_health_zones()

  biobank_data <- reactive({
    file <- input$upload
    if (!is.null(file)) {
      load_biobank(file$datapath, original_name = file$name)
    } else {
      load_biobank()
    }
  })

  summary_data <- reactive({
    dat <- biobank_data()
    biobank_summary(dat$data)
  })

  totals_by_province <- reactive({
    summary_data() |>
      group_by(prov_key, province) |>
      summarise(total_records = sum(records), .groups = "drop")
  })

  output$biobank_info <- renderText({
    dat <- biobank_data()
    if (isTRUE(dat$sample)) {
      "Showing built-in sample biobank data."
    } else if (!is.null(dat$path)) {
      sprintf("Loaded biobank file: %s", dat$path)
    } else {
      "No biobank file available."
    }
  })

  output$summary_table <- renderTable({
    df <- summary_data()
    if (!nrow(df)) return(tibble(province = character(), study = character(), records = integer()))
    df |> select(province, study, records)
  }, striped = TRUE, bordered = TRUE, width = "100%")

  zones_map_data <- reactive({
    bundle <- zones_bundle()
    g <- zones_sf()
    s <- zone_summary()
    if (is.null(bundle) || is.null(g) || is.null(s) || !nrow(s)) return(NULL)
    req(input$zones_name_col, input$prov_name_col)
    g$zone_key <- dplyr::na_if(normalize_names(as.character(g[[input$zones_name_col]])), "")
    g$prov_key <- dplyr::na_if(normalize_names(as.character(g[[input$prov_name_col]])), "")
    g$zone_display <- dplyr::na_if(as.character(g[[input$zones_name_col]]), "")
    g$province_display <- dplyr::na_if(as.character(g[[input$prov_name_col]]), "")
    s$zone_key <- dplyr::na_if(normalize_names(as.character(s$zone)), "")
    s$prov_key <- dplyr::na_if(normalize_names(as.character(s$province)), "")

    geom_col <- attr(g, "sf_column")
    gj <- dplyr::left_join(g, s, by = c("zone_key", "prov_key"))
    if (!inherits(gj, "sf") && !is.null(geom_col) && geom_col %in% names(gj) && inherits(gj[[geom_col]], "sfc")) {
      gj <- sf::st_as_sf(gj, sf_column_name = geom_col, crs = sf::st_crs(g))
    }
    gj$zone_uid <- dplyr::coalesce(gj$zone_uid, paste(gj$zone_key, gj$prov_key, sep = "__"))
    gj$layer_id <- gj$zone_uid

    zone_lbl <- lapply(
      sprintf("<b>%s</b><br/>Province: %s<br/>Biobank records: %s",
              zones$zonesante, zones$province, safe_total(zones$total_records)),
      htmltools::HTML
    )

    prov_lbl <- lapply(
      sprintf("<b>%s</b><br/>Biobank records: %s",
              provinces$province, safe_total(provinces$total_records)),
      htmltools::HTML
    )

    provinces <- NULL
    if (!is.null(bundle$provinces) && inherits(bundle$provinces, "sf")) {
      provinces <- bundle$provinces
    }
    list(
      gj = gj,
      metric = metric_num,
      pal = pal,
      legend_title = legend_title,
      provinces = provinces,
      prov_cols = bundle$cols
    )
  })

  output$map_zones <- renderLeaflet({
    data <- zones_map_data()
    validate(need(!is.null(data), "Geen kaartgegevens beschikbaar. Upload een kaart of gebruik GRID3."))
    gj <- data$gj
    metric <- data$metric
    pal <- data$pal
    metric_title <- ifelse(is.null(data$legend_title) || is.na(data$legend_title), "", data$legend_title)
    prov_cols_lookup <- data$prov_cols
    border_cols <- rep("#444444", nrow(gj))
    if (isTRUE(input$outline_by_prov)) {
      if (!is.null(prov_cols_lookup) && length(prov_cols_lookup)) {
        border_cols <- prov_cols_lookup[match(gj$province_display, names(prov_cols_lookup))]
        border_cols[is.na(border_cols)] <- prov_outline_color(gj$prov_key[is.na(border_cols)])
      } else {
        border_cols <- prov_outline_color(gj$prov_key)
      }
    }
    border_cols[is.na(border_cols)] <- "#444444"
    fmt_int <- function(x) ifelse(is.na(x), "0", formatC(as.integer(x), format = "d", big.mark = " "))
    fmt_pct <- function(x) ifelse(is.na(x), "-", paste0(scales::number(x, accuracy = 0.1), "%"))
    fmt_date <- function(x) {
      if (inherits(x, "Date")) {
        return(ifelse(is.na(x), "-", format(x, "%d %b %Y")))
      }
      ifelse(is.na(x) | is.infinite(x), "-", format(as.Date(x, origin = "1970-01-01"), "%d %b %Y"))
    }

    map <- leaflet(gj) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        data = zones,
        group = "Health zones",
        weight = 1,
        color  = "#666666",
        fillColor = ~prov_col,
        fillOpacity = 0.6,
        label = zone_lbl,
        highlightOptions = highlightOptions(weight = 2, color = "#000000", bringToFront = TRUE)
      ) %>%
      addPolylines(
        data = provinces,
        group = "Province outlines",
        weight = 1.5,
        color = "#111111",
        opacity = 0.9,
        label = prov_lbl
      ) %>%
      addLegend(
        position = "bottomright",
        colors = unname(hz$cols),
        labels = names(hz$cols),
        title  = "Province",
        opacity = 0.9
      ) %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Carto Positron", "Esri Gray"),
        overlayGroups = c("Health zones", "Province outlines"),
        options = layersControlOptions(collapsed = FALSE)
      )

    provinces <- data$provinces
    if (isTRUE(input$outline_by_prov) && !is.null(provinces) && inherits(provinces, "sf") && nrow(provinces)) {
      prov_line_cols <- rep("#111111", nrow(provinces))
      if (!is.null(prov_cols_lookup) && length(prov_cols_lookup)) {
        prov_line_cols <- prov_cols_lookup[match(provinces$province, names(prov_cols_lookup))]
      }
      fallback_idx <- is.na(prov_line_cols)
      if (any(fallback_idx)) {
        prov_line_cols[fallback_idx] <- prov_outline_color(provinces$prov_key[fallback_idx])
      }
      prov_line_cols[is.na(prov_line_cols)] <- "#111111"
      map <- map |>
        addPolylines(
          data = provinces,
          color = prov_line_cols,
          weight = if (is.numeric(input$stroke)) input$stroke + 0.5 else 1.5,
          opacity = 0.9,
          group = "Province outlines",
          label = ~lapply(sprintf("<b>%s</b>", province), htmltools::HTML)
        )
    }

    units <- mobile_units()
    units_map <- dplyr::filter(units, !is.na(has_coords) & has_coords)
    if (nrow(units_map)) {
      map <- map |>
        addCircleMarkers(
          data = units_map,
          lng = ~lon, lat = ~lat,
          radius = ~pmax(4, sqrt(n)),
          color = "#ffffff", weight = 1,
          fillColor = "#e67e22", fillOpacity = 0.9,
          label = ~lapply(
            sprintf(
              "<b>%s</b><br/>%s<br/>Totaal: %s · DA: %s · DP: %s",
              unit,
              dplyr::coalesce(zone_label, "Zone onbekend"),
              fmt_int(n),
              fmt_int(n_da),
              fmt_int(n_dp)
            ),
            htmltools::HTML
          ),
          popup = ~lapply(
            sprintf(
              "<strong>%s</strong><br/>%s<br/>%s stalen (DA: %s · DP: %s)<br/>Laatste staal: %s",
              unit,
              dplyr::coalesce(zone_label, "Zone onbekend"),
              fmt_int(n),
              fmt_int(n_da),
              fmt_int(n_dp),
              fmt_date(last_sample)
            ),
            htmltools::HTML
          )
        )
    }

    if (!all(is.na(metric))) {
      map <- map |> addLegend(pal = pal, values = metric, title = metric_title, opacity = 0.9)
    }

    map
  })
}

# -------- run ---------------------------------------------------------------

shinyApp(ui, server)
