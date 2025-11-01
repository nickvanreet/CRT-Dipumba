# app.R — MBUJI-MAYI BIOBANK AGE–SEX (Shiny)
# -----------------------------------------------------------------------------
# New: bestandskeuze + diagnose-tab om snel te zien waarom plots leeg zijn.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(tidyverse)
  library(readxl)
  library(janitor)
  library(lubridate)
  library(glue)
  library(DT)
  library(sf)
  library(leaflet)
  library(stringi)
  library(shinycssloaders) # Added for loading spinners
  library(memoise)         # Added for memoised GRID3 downloads
  library(plotly)          # Added for optional interactive pyramids
})

# -----------------------------------------------------------------------------
# 1) Helpers
# -----------------------------------------------------------------------------
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

read_specific_biobank <- function(file_path){
  if (is.null(file_path) || !file.exists(file_path)) return(NULL)
  df <- tryCatch(readxl::read_excel(file_path, .name_repair = "minimal") |> janitor::clean_names(), error = function(e) NULL)
  list(path = file_path, data = df)
}

rename_by_regex <- function(df){
  patterns <- list(
    Barcode         = "(code.*barre|barcode)",
    LabID           = "^(numero|num[ _-]*ech|lab[_-]*id)$",
    date_prelev     = "date.*prelev",
    date_env_cpltha = "date.*envoi.*cpltha",
    date_rec_cpltha = "date.*reception.*cpltha",
    date_env_inrb   = "date.*envoi.*inrb",
    age_text        = "(^age$|age_ou_annee.*naiss|annee.*naiss)",
    sex_text        = "^sexe",
    zone            = "zone.*sante",
    province        = "^province$",
    structure_sanitaire = "(structure.*sanit|structure.*sanitaire)",
    unite_mobile    = "(unite.*mobile|mobile.*unit)",
    temp_field      = "temperature.*transport",
    temp_cpltha     = "temperature.*stockage.*cpltha",
    study           = "^(etude|study)$"
  )
  out <- df
  for (new in names(patterns)){
    pat <- patterns[[new]]
    hit <- grep(pat, names(out), ignore.case = TRUE, value = TRUE)
    if (length(hit) >= 1) {
      out <- dplyr::rename(out, !!new := dplyr::all_of(hit[1]))
    } else {
      out[[new]] <- NA_character_
    }
  }
  out
}

find_first_matching_column <- function(df, patterns){
  if (is.null(df) || !ncol(df)) return(NULL)
  nms <- names(df)
  lower <- tolower(nms)
  for (pat in patterns) {
    idx <- which(lower == pat)
    if (length(idx)) return(nms[idx[1]])
  }
  for (pat in patterns) {
    idx <- which(grepl(pat, lower, fixed = FALSE))
    if (length(idx)) return(nms[idx[1]])
  }
  NULL
}

parse_any_date <- function(x_chr){
  x_chr <- trimws(as.character(x_chr))
  x_chr[x_chr %in% c("", "NA", "N/A", "na", "NaN", "NULL")] <- NA
  out <- as.Date(rep(NA_integer_, length(x_chr)), origin = "1970-01-01")
  is_num <- grepl("^\\d+$", x_chr)
  if (any(is_num)) suppressWarnings({ out[is_num] <- as.Date(as.numeric(x_chr[is_num]), origin = "1899-12-30") })
  idx <- !is_num & !is.na(x_chr)
  if (any(idx)) {
    tmp <- sub("[ T].*$", "", x_chr[idx])
    tmp <- gsub("[.\\-]", "/", tmp)
    p <- suppressWarnings(lubridate::dmy(tmp)); miss <- is.na(p)
    if (any(miss)) p[miss] <- suppressWarnings(lubridate::ymd(tmp[miss]))
    miss <- is.na(p)
    if (any(miss)) p[miss] <- suppressWarnings(lubridate::mdy(tmp[miss]))
    out[idx] <- p
  }
  out
}

read_extractions_dir <- function(dir_extraction){
  if (!dir.exists(dir_extraction)) return(tibble())

  files <- list.files(dir_extraction, pattern = "\\.xlsx?$", full.names = TRUE)
  if (!length(files)) return(tibble())

  read_one <- function(f){
    sneak <- suppressMessages(readxl::read_excel(f, n_max = 1))
    col_types <- rep("text", ncol(sneak))
    dat <- suppressMessages(
      readxl::read_excel(f, col_types = col_types, .name_repair = "minimal") |>
        janitor::clean_names()
    ) |>
      mutate(
        source_file = basename(f),
        file_date = {
          m <- stringr::str_match(basename(f), "^(\\d{6})")
          if (!is.na(m[1, 2])) as.Date(m[1, 2], format = "%y%m%d") else as.Date(NA)
        }
      )

    date_cols <- c(
      "date_de_prelevement_jj_mm_aaaa",
      "date_envoi_vers_cpltha_jj_mm_aaaa",
      "date_de_reception_cpltha_jj_mm_aaaa",
      "date_denvoi_inrb"
    )
    for (dc in date_cols) {
      if (dc %in% names(dat)) dat[[dc]] <- parse_any_date(dat[[dc]])
    }

    if ("code_barres_kps" %in% names(dat)) {
      dat <- dat |>
        filter(!(is.na(code_barres_kps) | code_barres_kps == ""))
    }
    dat
  }

  purrr::map_dfr(files, read_one) |>
    mutate(
      volume_raw = dplyr::coalesce(
        .data[["volume_total_echantillon_sang_drs_ml"]],
        .data[["volume_total_echantillon_sang_ml"]],
        .data[["volume_ml"]],
        .data[["volume"]]
      ),
      volume_ml = suppressWarnings(as.numeric(volume_raw)),
      volume_ml = ifelse(!is.na(volume_ml) & volume_ml > 10, volume_ml / 10, volume_ml)
    )
}

join_extractions_biobank <- function(extractions, biobank){
  if (is.null(extractions) || !nrow(extractions) || is.null(biobank) || !nrow(biobank)) {
    return(tibble())
  }

  if (!"code_barres_kps" %in% names(extractions)) {
    extractions$code_barres_kps <- NA_character_
  }
  if (!"numero" %in% names(extractions)) {
    extractions$numero <- NA_character_
  }

  extractions |>
    mutate(
      numero = as.character(numero),
      code_barres_kps = as.character(code_barres_kps)
    ) |>
    left_join(
      biobank |>
        select(
          Barcode, LabID, zone, province, structure_sanitaire, unite_mobile, date_prelev,
          date_env_cpltha, date_rec_cpltha, date_env_inrb
        ),
      by = c("code_barres_kps" = "Barcode", "numero" = "LabID")
    )
}

flag_extractions <- function(extractions_joined){
  if (is.null(extractions_joined) || !nrow(extractions_joined)) {
    return(list(flagged = tibble(), dedup = tibble()))
  }

  dups_exact <- extractions_joined |>
    mutate(vol_key = round(as.numeric(volume_ml), 2)) |>
    group_by(code_barres_kps, numero, vol_key) |>
    filter(dplyr::n() > 1) |>
    ungroup()

  same_barcode_diff_num <- extractions_joined |>
    filter(!is.na(code_barres_kps)) |>
    group_by(code_barres_kps) |>
    filter(dplyr::n_distinct(numero, na.rm = TRUE) > 1) |>
    ungroup()

  reextractions <- same_barcode_diff_num |>
    group_by(code_barres_kps) |>
    filter(dplyr::n_distinct(round(as.numeric(volume_ml), 2), na.rm = TRUE) > 1) |>
    ungroup()

  same_barcode_same_vol_new_num <- same_barcode_diff_num |>
    group_by(code_barres_kps) |>
    filter(dplyr::n_distinct(round(as.numeric(volume_ml), 2), na.rm = TRUE) == 1) |>
    ungroup()

  same_numero_diff_barcode <- extractions_joined |>
    filter(!is.na(numero)) |>
    group_by(numero) |>
    filter(dplyr::n_distinct(code_barres_kps, na.rm = TRUE) > 1) |>
    ungroup()

  extractions_flagged <- extractions_joined |>
    mutate(
      vol_key = round(as.numeric(volume_ml), 2),
      flag = case_when(
        paste(code_barres_kps, numero, vol_key) %in%
          paste(dups_exact$code_barres_kps, dups_exact$numero, dups_exact$vol_key)
        ~ "EXACT_DUPLICATE",
        code_barres_kps %in% reextractions$code_barres_kps &
          numero %in% reextractions$numero
        ~ "RE_EXTRACTION_SAME_BARCODE_DIFF_NUM_DIFF_VOL",
        code_barres_kps %in% same_barcode_same_vol_new_num$code_barres_kps &
          numero %in% same_barcode_same_vol_new_num$numero
        ~ "SUSPECT_SAME_BARCODE_SAME_VOL_NEW_NUM",
        numero %in% same_numero_diff_barcode$numero &
          code_barres_kps %in% same_numero_diff_barcode$code_barres_kps
        ~ "SUSPECT_SAME_NUM_DIFF_BARCODE",
        TRUE ~ "OK"
      )
    )

  extractions_dedup <- extractions_flagged |>
    arrange(code_barres_kps, numero, desc(file_date)) |>
    group_by(code_barres_kps, numero, round(as.numeric(volume_ml), 2)) |>
    slice(1) |>
    ungroup()

  list(flagged = extractions_flagged, dedup = extractions_dedup)
}

clean_age_sex <- function(df){
  df |>
    mutate(
      age_num = readr::parse_number(age_text),
      age_num = case_when(
        is.na(age_num) ~ NA_real_,
        age_num > 1900 & !is.na(date_prelev) ~ lubridate::year(date_prelev) - age_num, # Use sample date to compute age
        age_num > 1900 ~ NA_real_,
        TRUE ~ age_num
      ),
      age_num = ifelse(dplyr::between(age_num, 0, 110), age_num, NA_real_),
      sex_clean = recode(toupper(trimws(sex_text)), "M" = "M", "F" = "F", .default = NA_character_),
      study = case_when(
        is.na(study) ~ NA_character_,
        str_detect(toupper(study), "^DA$") ~ "DA",
        str_detect(toupper(study), "^DP$") ~ "DP",
        TRUE ~ study
      )
    )
}

safe_days <- function(to, from, max_ok = 90) {
  d <- as.numeric(difftime(to, from, units = "days"))
  d[!is.finite(d) | d < 0 | d > max_ok] <- NA_real_
  d
}

make_age_bands <- function(df, bin = 5){
  max_age <- suppressWarnings(max(df$age_num, na.rm = TRUE))
  if (!is.finite(max_age)) {
    return(df |>
             mutate(age_band = factor(rep(NA_character_, nrow(df)), levels = character())))
  }

  upper <- max(bin, ceiling(max_age / bin) * bin)
  breaks <- seq(0, upper + bin, by = bin)

  df |>
    mutate(age_band = cut(age_num, breaks = breaks, right = FALSE, include.lowest = TRUE))
}

# GRID3 helpers ---------------------------------------------------------------
read_grid3_health_zones_impl <- function(
    fields = c("province", "zonesante", "zs_uid"),
    local_paths = NULL
){
  normalise_fields <- function(sf_obj) {
    nms <- names(sf_obj)
    if (!"province"  %in% nms) names(sf_obj)[match(tolower(nms), "province")]  <- "province"
    if (!"zonesante" %in% nms) names(sf_obj)[match(tolower(nms), "zonesante")] <- "zonesante"
    if (!"zs_uid"    %in% nms && "zsuid" %in% tolower(nms)) {
      names(sf_obj)[match(tolower(nms), "zsuid")] <- "zs_uid"
    }
    sf_obj
  }

  read_local_candidate <- function(path) {
    if (!nzchar(path) || !file.exists(path)) {
      return(NULL)
    }

    fmt <- tolower(tools::file_ext(path))

    sf_obj <- switch(
      fmt,
      rds = try(readRDS(path), silent = TRUE),
      {
        # allow zipped shapefiles by using the "zip://" prefix automatically
        to_read <- if (fmt == "zip") paste0("zip://", normalizePath(path)) else path
        try(sf::read_sf(to_read, quiet = TRUE), silent = TRUE)
      }
    )

    if (inherits(sf_obj, "try-error") || is.null(sf_obj)) {
      return(NULL)
    }

    if (!inherits(sf_obj, "sf")) {
      sf_obj <- try(sf::st_as_sf(sf_obj), silent = TRUE)
      if (inherits(sf_obj, "try-error") || is.null(sf_obj)) {
        return(NULL)
      }
    }

    if (!nrow(sf_obj)) {
      return(NULL)
    }

    sf_obj <- normalise_fields(sf_obj)
    keep <- unique(c(fields, attr(sf_obj, "sf_column")))
    suppressWarnings(sf_obj[, intersect(keep, names(sf_obj)), drop = FALSE])
  }

  local_defaults <- c(
    Sys.getenv("GRID3_HEALTH_ZONES_FILE", unset = ""),
    local_paths,
    file.path("data", "grid3_health_zones.rds"),
    file.path("data", "grid3_health_zones.gpkg"),
    file.path("data", "grid3_health_zones.geojson"),
    file.path("www", "grid3_health_zones.geojson"),
    file.path("www", "grid3_health_zones.gpkg"),
    file.path("www", "grid3_health_zones.rds")
  )

  for (candidate in unique(local_defaults)) {
    sf_obj <- read_local_candidate(candidate)
    if (!is.null(sf_obj)) {
      return(sf_obj)
    }
  }

  bases <- c(
    "https://services3.arcgis.com/BU6Aadhn6tbBEdyk/arcgis/rest/services/GRID3_COD_health_zones_v7_0/FeatureServer/0/query",
    "https://services3.arcgis.com/BU6Aadhn6tbBEdyk/arcgis/rest/services/GRID3_COD_health_zones_v6_0/FeatureServer/0/query",
    "https://services3.arcgis.com/BU6Aadhn6tbBEdyk/arcgis/rest/services/GRID3_COD_health_zones_v5_0/FeatureServer/0/query"
  )
  q <- function(base, fld) {
    fld <- paste(fld, collapse = ",")
    paste0(base,
           "?where=1%3D1&outFields=", utils::URLencode(fld, reserved = TRUE),
           "&outSR=4326&f=geojson")
  }
  errs <- list()
  for (b in bases) {
    url <- q(b, fields)
    sf_obj <- try(sf::read_sf(url, quiet = TRUE), silent = TRUE)
    if (!inherits(sf_obj, "try-error") && nrow(sf_obj) > 0) {
      sf_obj <- normalise_fields(sf_obj)
      return(sf_obj)
    }
    errs[[b]] <- if (inherits(sf_obj, "try-error")) as.character(sf_obj) else "no rows"
  }
  stop("Could not fetch GRID3 health zones from public endpoints.\n",
       "Some services now require an access token.\n",
       "Tried:\n- ", paste(names(errs), collapse = "\n- "),
       "\nIf blocked, download the file from GRID3’s DRC data page and read locally.")
}

# Memoise the public download to avoid repeated requests within a session
read_grid3_health_zones <- memoise::memoise(read_grid3_health_zones_impl)

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

# QC module -------------------------------------------------------------------
qcUI <- function(id){
  ns <- NS(id)
  fluidRow(
    column(
      width = 3,
      card(
        card_header("Instellingen"),
        card_body(
          textInput(
            ns("qc_dir"),
            "Map met extraction-bestanden",
            value = "C:/Users/nvanreet/ITG/THA - Digital Management System - CRT Dipumba Upload - CRT Dipumba Upload/02 - Extractions"
          ),
          actionButton(ns("qc_refresh"), "Vernieuwen"),
          div(class = "mt-2", textOutput(ns("qc_status"))),
          hr(),
          sliderInput(
            ns("qc_rng"), "Acceptabel volumebereik (mL)",
            min = 0.5, max = 3.5, value = c(1.5, 2.5), step = 0.1
          ),
          selectInput(ns("qc_prov"), "Provincie", choices = "Alle", selected = "Alle"),
          selectInput(ns("qc_zone"), "Zone de santé", choices = "Alle", selected = "Alle"),
          selectInput(ns("qc_structure"), "Structure sanitaire", choices = "Alle", selected = "Alle"),
          selectInput(ns("qc_unit"), "Unité mobile", choices = "Alle", selected = "Alle"),
          dateRangeInput(
            ns("qc_date_rng"), "Filter op datum de prélèvement",
            start = NULL, end = NULL, format = "yyyy-mm-dd", weekstart = 1
          ),
          radioButtons(
            ns("qc_agg"), "Tijdresolutie",
            choices = c("Dag" = "day", "Maand" = "month", "Jaar" = "year"),
            selected = "day", inline = TRUE
          ),
          checkboxInput(ns("qc_show_out"), "Toon enkel out-of-range", value = FALSE),
          hr(),
          downloadButton(ns("qc_dl_zone"), "Download Zone Summary (CSV)"),
          downloadButton(ns("qc_dl_out"), "Download Outliers (CSV)"),
          downloadButton(ns("qc_dl_dup"), "Download Duplicaten (CSV)"),
          downloadButton(ns("qc_dl_filt"), "Download Gefilterde Dataset (CSV)"),
          downloadButton(ns("qc_dl_agg"), "Download Tijdreeks (CSV)")
        )
      )
    ),
    column(
      width = 9,
      tabsetPanel(
        tabPanel(
          "Overzicht",
          withSpinner(plotOutput(ns("qc_hist_vol"), height = "320px")),
          withSpinner(plotOutput(ns("qc_box_vol"), height = "260px"))
        ),
        tabPanel(
          "Per Zone",
          withSpinner(plotOutput(ns("qc_trend_filedate"), height = "320px")),
          withSpinner(DTOutput(ns("qc_zone_tbl")))
        ),
        tabPanel(
          "Tijdreeksen",
          withSpinner(plotOutput(ns("qc_agg_count_plot"), height = "280px")),
          withSpinner(plotOutput(ns("qc_agg_volume_plot"), height = "280px")),
          withSpinner(DTOutput(ns("qc_agg_tbl")))
        ),
        tabPanel("Per-sample", withSpinner(DTOutput(ns("qc_per_sample_tbl")))),
        tabPanel("Outliers", withSpinner(DTOutput(ns("qc_out_tbl")))),
        tabPanel("Duplicaten", withSpinner(DTOutput(ns("qc_dup_tbl"))))
      )
    )
  )
}

qcServer <- function(id, biobank){
  moduleServer(id, function(input, output, session){
    empty_extractions <- function(){
      tibble(
        code_barres_kps = character(),
        numero = character(),
        volume_ml = numeric(),
        file_date = as.Date(character()),
        source_file = character()
      )
    }

    qc_status <- reactiveVal("Nog geen data geladen.")
    qc_raw    <- reactiveVal(empty_extractions())

    output$qc_status <- renderText({ qc_status() })

    observeEvent(input$qc_refresh, {
      dir <- normalizePath(input$qc_dir, winslash = "/", mustWork = FALSE)
      if (!dir.exists(dir)) {
        qc_status("Map bestaat niet. Controleer het pad.")
        qc_raw(empty_extractions())
        showNotification("Map bestaat niet. Controleer het pad.", type = "error")
        return()
      }

      dat <- read_extractions_dir(dir)
      if (!nrow(dat)) {
        qc_status("Geen extraction-bestanden gevonden in deze map.")
        qc_raw(empty_extractions())
        showNotification("Geen extraction-bestanden gevonden in deze map.", type = "warning")
        return()
      }

      qc_raw(dat)
      file_count <- length(unique(na.omit(dat$source_file)))
      sample_count <- nrow(dat)
      msg <- sprintf(
        "%s bestanden · %s stalen",
        scales::comma(file_count),
        scales::comma(sample_count)
      )
      qc_status(msg)
    })

    qc_joined <- reactive({
      join_extractions_biobank(qc_raw(), biobank())
    })

    qc_flagged_data <- reactive({
      flag_extractions(qc_joined())
    })

    qc_dedup <- reactive({
      res <- qc_flagged_data()
      dedup <- res$dedup
      if (is.null(dedup)) tibble() else dedup
    })

    observeEvent(qc_dedup(), {
      df <- qc_dedup()
      if (is.null(df) || !nrow(df) || !"province" %in% names(df)) {
        updateSelectInput(session, "qc_prov", choices = "Alle", selected = "Alle")
        updateSelectInput(session, "qc_zone", choices = "Alle", selected = "Alle")
        updateSelectInput(session, "qc_structure", choices = "Alle", selected = "Alle")
        updateSelectInput(session, "qc_unit", choices = "Alle", selected = "Alle")
        return()
      }

      provs <- sort(unique(na.omit(df$province)))
      sel_prov <- if (input$qc_prov %in% c("Alle", provs)) input$qc_prov else "Alle"
      updateSelectInput(session, "qc_prov", choices = c("Alle", provs), selected = sel_prov)

      structures <- if ("structure_sanitaire" %in% names(df)) sort(unique(na.omit(df$structure_sanitaire))) else character()
      sel_structure <- input$qc_structure
      if (is.null(sel_structure) || !sel_structure %in% c("Alle", structures)) sel_structure <- "Alle"
      updateSelectInput(session, "qc_structure", choices = c("Alle", structures), selected = sel_structure)

      rng <- suppressWarnings(range(df$date_prelev, na.rm = TRUE))
      if (all(is.finite(rng))) {
        updateDateRangeInput(session, "qc_date_rng", start = rng[1], end = rng[2], min = rng[1], max = rng[2])
      }
    })

    observeEvent(list(input$qc_prov, qc_dedup()), {
      df <- qc_dedup()
      if (is.null(df) || !nrow(df) || !"zone" %in% names(df)) {
        updateSelectInput(session, "qc_zone", choices = "Alle", selected = "Alle")
        return()
      }

      zones <- sort(unique(na.omit(df$zone)))
      if (!is.null(input$qc_prov) && input$qc_prov != "Alle" && "province" %in% names(df)) {
        zones <- sort(unique(na.omit(df$zone[df$province == input$qc_prov])))
      }
      sel_zone <- if (input$qc_zone %in% c("Alle", zones)) input$qc_zone else "Alle"
      updateSelectInput(session, "qc_zone", choices = c("Alle", zones), selected = sel_zone)
    }, ignoreNULL = FALSE)

    observeEvent(list(input$qc_structure, qc_dedup()), {
      df <- qc_dedup()
      if (is.null(df) || !nrow(df) || !"unite_mobile" %in% names(df)) {
        updateSelectInput(session, "qc_unit", choices = "Alle", selected = "Alle")
        return()
      }

      df_units <- df
      if (!is.null(input$qc_structure) && input$qc_structure != "Alle" && "structure_sanitaire" %in% names(df_units)) {
        df_units <- df_units |> dplyr::filter(!is.na(structure_sanitaire) & structure_sanitaire == input$qc_structure)
      }
      units <- sort(unique(na.omit(df_units$unite_mobile)))
      sel_unit <- input$qc_unit
      if (is.null(sel_unit) || !sel_unit %in% c("Alle", units)) sel_unit <- "Alle"
      updateSelectInput(session, "qc_unit", choices = c("Alle", units), selected = sel_unit)
    }, ignoreNULL = FALSE)

    qc_filtered <- reactive({
      df <- qc_dedup()
      if (is.null(df) || !nrow(df) || !"volume_ml" %in% names(df)) return(tibble())

      df <- df |> mutate(volume_num = suppressWarnings(as.numeric(volume_ml)))

      if (!is.null(input$qc_prov) && input$qc_prov != "Alle" && "province" %in% names(df)) {
        df <- df |> filter(!is.na(province) & province == input$qc_prov)
      }

      if (!is.null(input$qc_zone) && input$qc_zone != "Alle" && "zone" %in% names(df)) {
        df <- df |> filter(!is.na(zone) & zone == input$qc_zone)
      }

      if (!is.null(input$qc_structure) && input$qc_structure != "Alle" && "structure_sanitaire" %in% names(df)) {
        df <- df |> filter(!is.na(structure_sanitaire) & structure_sanitaire == input$qc_structure)
      }

      if (!is.null(input$qc_unit) && input$qc_unit != "Alle" && "unite_mobile" %in% names(df)) {
        df <- df |> filter(!is.na(unite_mobile) & unite_mobile == input$qc_unit)
      }

      if (!is.null(input$qc_date_rng) && length(input$qc_date_rng) == 2 && all(!is.na(input$qc_date_rng)) && "date_prelev" %in% names(df)) {
        d1 <- as.Date(input$qc_date_rng[1])
        d2 <- as.Date(input$qc_date_rng[2])
        df <- df |> filter(!is.na(date_prelev) & date_prelev >= d1 & date_prelev <= d2)
      }

      if (isTRUE(input$qc_show_out) && "volume_num" %in% names(df)) {
        rng <- input$qc_rng
        df <- df |> filter(!is.na(volume_num) & (volume_num < rng[1] | volume_num > rng[2]))
      }

      df
    })

    qc_agg_data <- reactive({
      df <- qc_filtered()
      if (is.null(df) || !nrow(df) || !"date_prelev" %in% names(df) || !"volume_num" %in% names(df)) return(tibble())

      df <- df |> filter(!is.na(date_prelev))
      if (!nrow(df)) return(tibble())

      agg <- input$qc_agg
      df <- df |>
        mutate(
          period = dplyr::case_when(
            agg == "day"   ~ date_prelev,
            agg == "month" ~ lubridate::floor_date(date_prelev, unit = "month"),
            agg == "year"  ~ as.Date(sprintf("%s-01-01", lubridate::year(date_prelev))),
            TRUE ~ date_prelev
          )
        )

      rng <- input$qc_rng
      df |>
        group_by(period) |>
        summarise(
          N = dplyr::n(),
          Median = suppressWarnings(stats::median(volume_num, na.rm = TRUE)),
          P10 = suppressWarnings(stats::quantile(volume_num, 0.10, na.rm = TRUE, names = FALSE)),
          P90 = suppressWarnings(stats::quantile(volume_num, 0.90, na.rm = TRUE, names = FALSE)),
          Out = sum(!is.na(volume_num) & (volume_num < rng[1] | volume_num > rng[2])),
          `% Out` = round(100 * Out / pmax(N, 1), 1),
          .groups = "drop"
        ) |>
        arrange(period)
    })

    output$qc_hist_vol <- renderPlot({
      df <- qc_filtered()
      validate(need(nrow(df) > 0, "Geen data beschikbaar."))
      rng <- input$qc_rng
      ggplot(df, aes(x = volume_num)) +
        geom_histogram(binwidth = 0.1, fill = "#2C3E50", colour = "white") +
        geom_vline(xintercept = rng, linetype = "dashed", colour = "red") +
        labs(title = "Samplevolumes", x = "Volume (mL)", y = "Frequentie") +
        theme_minimal()
    })

    output$qc_box_vol <- renderPlot({
      df <- qc_filtered()
      validate(need(nrow(df) > 0, "Geen data beschikbaar."))
      ggplot(df, aes(x = "", y = volume_num)) +
        geom_boxplot(fill = "#18BC9C", alpha = 0.7) +
        labs(title = "Boxplot samplevolumes", x = NULL, y = "Volume (mL)") +
        theme_minimal()
    })

    output$qc_trend_filedate <- renderPlot({
      df <- qc_filtered()
      validate(need(nrow(df) > 0, "Geen data beschikbaar."))
      ggplot(df, aes(x = file_date, y = volume_num)) +
        geom_point(alpha = 0.4) +
        geom_smooth(method = "loess", se = FALSE, colour = "#2980B9") +
        geom_hline(yintercept = 2, linetype = "dashed", colour = "red") +
        labs(title = "Samplevolumes doorheen extraction batches", x = "file_date", y = "Volume (mL)") +
        theme_minimal()
    })

    output$qc_zone_tbl <- renderDT({
      df <- qc_filtered()
      if (!nrow(df) || !all(c("province", "zone", "volume_num") %in% names(df))) {
        return(datatable(tibble(), options = list(pageLength = 15, scrollX = TRUE)))
      }
      rng <- input$qc_rng
      summ <- df |>
        group_by(province, zone) |>
        summarise(
          N = dplyr::n(),
          Out = sum(!is.na(volume_num) & (volume_num < rng[1] | volume_num > rng[2])),
          `% Out` = round(100 * Out / pmax(N, 1), 1),
          Median = round(stats::median(volume_num, na.rm = TRUE), 2),
          P10 = round(stats::quantile(volume_num, 0.10, na.rm = TRUE, names = FALSE), 2),
          P90 = round(stats::quantile(volume_num, 0.90, na.rm = TRUE, names = FALSE), 2),
          .groups = "drop"
        ) |>
        arrange(desc(`% Out`), desc(N))
      datatable(summ, options = list(pageLength = 15, scrollX = TRUE))
    })

    output$qc_agg_count_plot <- renderPlot({
      ad <- qc_agg_data()
      validate(need(nrow(ad) > 0, "Geen data beschikbaar."))
      ggplot(ad, aes(x = period, y = N)) +
        geom_col() +
        labs(
          title = sprintf("Aantal stalen per %s", switch(input$qc_agg, day = "dag", month = "maand", year = "jaar")),
          x = "Periode", y = "Aantal"
        ) +
        theme_minimal()
    })

    output$qc_agg_volume_plot <- renderPlot({
      ad <- qc_agg_data()
      validate(need(nrow(ad) > 0, "Geen data beschikbaar."))
      ggplot(ad, aes(x = period, y = Median)) +
        geom_line() +
        geom_point() +
        geom_ribbon(aes(ymin = P10, ymax = P90), alpha = 0.15) +
        geom_hline(yintercept = 2, linetype = "dashed") +
        labs(
          title = sprintf("Mediaan volume per %s (P10–P90)", switch(input$qc_agg, day = "dag", month = "maand", year = "jaar")),
          x = "Periode", y = "Volume (mL)"
        ) +
        theme_minimal()
    })

    output$qc_agg_tbl <- renderDT({
      ad <- qc_agg_data()
      datatable(ad, options = list(pageLength = 20, scrollX = TRUE))
    })

    output$qc_per_sample_tbl <- renderDT({
      df <- qc_filtered()
      if (!nrow(df)) df <- tibble()
      keep <- c("province", "zone", "structure_sanitaire", "unite_mobile", "numero", "code_barres_kps", "date_prelev", "date_rec_cpltha",
                "date_env_cpltha", "date_env_inrb", "volume_ml", "volume_num", "file_date", "source_file", "flag")
      datatable(df |>
                  select(any_of(keep)) |>
                  arrange(date_prelev, zone, numero),
                options = list(pageLength = 20, scrollX = TRUE), filter = "top")
    })

    output$qc_out_tbl <- renderDT({
      df <- qc_filtered()
      if (!nrow(df) || !"volume_num" %in% names(df)) df <- tibble()
      else {
        rng <- input$qc_rng
        df <- df |>
          filter(!is.na(volume_num) & (volume_num < rng[1] | volume_num > rng[2])) |>
          arrange(volume_num) |>
          mutate(reason = ifelse(volume_num < rng[1], "Low volume", "High volume")) # Reason column for QC outliers
      }
      keep <- c("province", "zone", "structure_sanitaire", "unite_mobile", "numero", "code_barres_kps", "volume_ml", "volume_num", "date_prelev", "file_date", "source_file", "reason")
      datatable(df |> select(any_of(keep)), options = list(pageLength = 20, scrollX = TRUE))
    })

    output$qc_dup_tbl <- renderDT({
      flagged <- qc_flagged_data()$flagged
      if (is.null(flagged) || !nrow(flagged) || !"flag" %in% names(flagged)) {
        flagged <- tibble()
      } else {
        flagged <- flagged |> filter(flag != "OK")
      }
      keep <- c("flag", "province", "zone", "structure_sanitaire", "unite_mobile", "numero", "code_barres_kps", "volume_ml", "volume_num", "date_prelev", "file_date", "source_file")
      datatable(flagged |> select(any_of(keep)), options = list(pageLength = 20, scrollX = TRUE))
    })

    output$qc_dl_zone <- downloadHandler(
      filename = function() paste0("zone_summary_", Sys.Date(), ".csv"),
      content = function(file){
        df <- qc_filtered()
        if (!nrow(df) || !all(c("province", "zone", "volume_num") %in% names(df))) {
          readr::write_csv(tibble(), file)
          return()
        }
        rng <- input$qc_rng
        summ <- df |>
          group_by(province, zone) |>
          summarise(
            N = dplyr::n(),
            Out = sum(!is.na(volume_num) & (volume_num < rng[1] | volume_num > rng[2])),
            `% Out` = round(100 * Out / pmax(N, 1), 1),
            Median = round(stats::median(volume_num, na.rm = TRUE), 2),
            P10 = round(stats::quantile(volume_num, 0.10, na.rm = TRUE, names = FALSE), 2),
            P90 = round(stats::quantile(volume_num, 0.90, na.rm = TRUE, names = FALSE), 2),
            .groups = "drop"
          )
        readr::write_csv(summ, file)
      }
    )

    output$qc_dl_out <- downloadHandler(
      filename = function() paste0("outliers_", Sys.Date(), ".csv"),
      content = function(file){
        df <- qc_filtered()
        if (!nrow(df) || !"volume_num" %in% names(df)) {
          readr::write_csv(tibble(), file)
          return()
        }
        rng <- input$qc_rng
        out <- df |>
          filter(!is.na(volume_num) & (volume_num < rng[1] | volume_num > rng[2])) |>
          mutate(reason = ifelse(volume_num < rng[1], "Low volume", "High volume"))
        readr::write_csv(out, file)
      }
    )

    output$qc_dl_dup <- downloadHandler(
      filename = function() paste0("duplicates_", Sys.Date(), ".csv"),
      content = function(file){
        flagged <- qc_flagged_data()$flagged
        if (is.null(flagged) || !nrow(flagged)) {
          readr::write_csv(tibble(), file)
          return()
        }
        readr::write_csv(flagged |> filter(flag != "OK"), file)
      }
    )

    output$qc_dl_filt <- downloadHandler(
      filename = function() paste0("filtered_", Sys.Date(), ".csv"),
      content = function(file){
        df <- qc_filtered()
        if (!nrow(df)) df <- tibble()
        readr::write_csv(df, file)
      }
    )

    output$qc_dl_agg <- downloadHandler(
      filename = function() paste0("tijdreeks_", input$qc_agg, "_", Sys.Date(), ".csv"),
      content = function(file){
        readr::write_csv(qc_agg_data(), file)
      }
    )

    list(
      qc_dir_input = reactive(input$qc_dir)
    )
  })
}

# -----------------------------------------------------------------------------
# 2) Plots
# -----------------------------------------------------------------------------
plot_zone_pyramids <- function(df, bin = 5, min_n_per_zone = 15, split_study = FALSE){
  need <- c("age_num","sex_clean","date_prelev","zone","province")
  if (!all(need %in% names(df))) return(ggplot() + theme_void() + ggtitle("Vereiste kolommen ontbreken"))

  dd <- df |>
    filter(!is.na(age_num), !is.na(sex_clean), !is.na(date_prelev)) |>
    make_age_bands(bin = bin) |>
    count(province, zone, age_band, sex_clean, study, name = "n") |>
    complete(province, zone, age_band, sex_clean, study, fill = list(n = 0))

  if (!nrow(dd)) return(ggplot() + theme_void() + ggtitle("Geen data na filter"))

  zone_tot <- dd |>
    group_by(province, zone) |>
    summarise(N = sum(n), .groups = "drop") |>
    filter(N >= min_n_per_zone)
  if (!nrow(zone_tot)) return(ggplot() + theme_void() + ggtitle("Geen zones met voldoende N"))

  dd <- dd |>
    semi_join(zone_tot, by = c("province","zone")) |>
    mutate(n_signed = if_else(sex_clean == "M", -n, n))

  p <- ggplot(dd, aes(x = n_signed, y = age_band, fill = sex_clean)) +
    geom_col(width = 0.9, colour = "grey40") +
    geom_vline(xintercept = 0, colour = "grey30") +
    scale_x_continuous("Aantal (links = M, rechts = F)", labels = abs) +
    scale_y_discrete("Leeftijdsband") +
    guides(fill = guide_legend(title = "Geslacht")) +
    theme_minimal(base_size = 12)

  if (split_study) p + facet_grid(study + province ~ zone, scales = "free_x", space = "free_x")
  else p + facet_grid(province ~ zone, scales = "free_x", space = "free_x")
}

plot_age_sex_pyramid_plus <- function(df, facet_by = c("zone","province"), bin = 5, split_study = FALSE){
  facet_by <- rlang::arg_match(facet_by)
  need <- c("age_num","sex_clean", facet_by)
  if (!all(need %in% names(df))) return(ggplot() + theme_void() + ggtitle("Vereiste kolommen ontbreken"))

  dd <- df |>
    filter(!is.na(age_num), !is.na(sex_clean), !is.na(date_prelev)) |>
    make_age_bands(bin = bin) |>
    count(.data[[facet_by]], age_band, sex_clean, study, name = "n") |>
    complete(.data[[facet_by]], age_band, sex_clean, study, fill = list(n = 0)) |>
    mutate(n_signed = if_else(sex_clean == "M", -n, n))

  if (!nrow(dd)) return(ggplot() + theme_void() + ggtitle("Geen data na filter"))

  p <- ggplot(dd, aes(x = n_signed, y = age_band, fill = sex_clean)) +
    geom_col(width = .9, colour = "grey40") +
    geom_vline(xintercept = 0, colour = "grey30") +
    scale_x_continuous("Aantal (links = M, rechts = F)", labels = abs) +
    scale_y_discrete("Leeftijdsband") +
    guides(fill = guide_legend(title = "Geslacht")) +
    theme_minimal(base_size = 12)

  if (split_study) p + facet_wrap(vars(study, .data[[facet_by]]))
  else p + facet_wrap(vars(.data[[facet_by]]))
}

# -----------------------------------------------------------------------------
# 3) UI
# -----------------------------------------------------------------------------
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  tags$head(tags$style(HTML(".summary-card-label {text-transform: uppercase; font-size: 0.75rem; letter-spacing: 0.05em; color: #6c757d;}\n.summary-card-value {font-size: 2rem; font-weight: 600;}\n.summary-card .card-body {display: flex; flex-direction: column; gap: 0.15rem;}\n.map-card .card-header {display: flex; justify-content: space-between; align-items: center;}\n.map-card .card-header .btn {font-size: 0.85rem;}\n.mobile-unit-item {border-bottom: 1px solid #e9ecef; padding: 0.35rem 0;}\n.mobile-unit-item:last-child {border-bottom: none;}"))),
  fluidRow(
    column(
      width = 3,
      card(
        card_header("Bron & bestand"),
        helpText("Selecteer de biobank-map, kies een Excelbestand en laad de data."),
        textInput("root", "Biobank-map", value = "C:/Users/nvanreet/ITG/THA - Digital Management System - CRT Dipumba Upload - CRT Dipumba Upload/01 - Biobanque"),
        uiOutput("filepick_ui"),
        actionButton("refresh", "Vernieuwen"),
        div(class = "mt-2", strong("Bestand:"), textOutput("used_file", inline = TRUE))
      ),
      card(
        card_header("Kaartinstellingen"),
        checkboxInput("use_grid3", "Gebruik GRID3-gezondheidszones (internet)", value = FALSE),
        uiOutput("grid3_status"),
        fileInput(
          "zones_geo",
          "Gezondheidszones (GeoJSON/Shape of .zip)",
          accept = c(".geojson", ".json", ".shp", ".zip"),
          buttonLabel = "Kies bestand"
        ),
        selectInput("zones_name_col", "Kolom met ZS-naam (in kaartlaag)", choices = NULL),
        selectInput("prov_name_col",  "Kolom met provincienaam (in kaartlaag)", choices = NULL),
        selectInput(
          "map_metric_zs", "Kleurmetriek",
          choices = c(
            "Aantal stalen" = "n",
            "DA-stalen" = "n_da",
            "DP-stalen" = "n_dp",
            "% vrouw" = "pct_f",
            "% DA" = "pct_da",
            "% DP" = "pct_dp",
            "Mediaan leeftijd" = "med_age"
          ),
          selected = "n"
        ),
        numericInput("map_bins_zs", "Kleur-bins", value = 5, min = 3, max = 9),
        checkboxInput("outline_by_prov", "Contour in kleur per provincie (Kasai-Oriental geel, Lomami rood)", value = TRUE),
        actionButton("focus_kasai", "Focus Kasai-Oriental + Lomami")
      ),
      card(
        card_header("Filters"),
        dateRangeInput("daterng", "Datumbereik (date_prelev)", start = Sys.Date() - 180, end = Sys.Date()),
        selectInput("study", "Filter studie", choices = c("Alle"), selected = "Alle"),
        uiOutput("province_ui"),
        uiOutput("zone_ui"),
        uiOutput("structure_ui"),
        uiOutput("mobile_ui"),
        checkboxGroupInput("sex", "Geslacht", choices = c("M", "F"), selected = c("M", "F"), inline = TRUE),
        sliderInput("age_rng", "Leeftijd", min = 0, max = 110, value = c(0, 80))
      ),
      card(
        card_header("Age & sex-instellingen"),
        numericInput("bin", "Leeftijdsband (jaar)", value = 5, min = 1, step = 1),
        numericInput("minN", "Min. N per zone (filter)", value = 15, min = 1, step = 1),
        checkboxInput("split_study", "Onderverdeel per studie (DA/DP)", value = FALSE),
        selectInput("facet", "Facet voor ‘Plus’ grafiek", choices = c("zone", "province"), selected = "zone"),
        checkboxInput("p_interactive", "Interactief", value = FALSE)
      ),
      card(
        card_header("Downloads"),
        downloadButton("dl_csv", "Download CSV"),
        downloadButton("dl_plot", "Download plot (PNG)"),
        downloadButton("dl_plot_pdf", "Download plot (PDF)")
      )
    ),
    column(
      width = 6,
      card(
        class = "map-card",
        card_header(
          div("Gezondheidszones & mobiele units"),
          div(
            class = "d-flex gap-2",
            actionButton("focus_kasai_top", "Kasai & Lomami", class = "btn btn-sm btn-outline-secondary"),
            actionButton("reset_map", "Reset", class = "btn btn-sm btn-outline-secondary")
          )
        ),
        div(class = "text-muted small mb-2", "Hover over een zone voor DA/DP, markers tonen mobiele units."),
        uiOutput("empty_banner"),
        withSpinner(leafletOutput("map_zones", height = "70vh"))
      )
    ),
    column(
      width = 3,
      card(
        card_header("Selectie-overzicht"),
        uiOutput("summary_cards")
      ),
      card(
        card_header("Zone detail"),
        uiOutput("zone_detail_select_ui"),
        uiOutput("zone_detail_panel")
      ),
      card(
        card_header("Mobiele units"),
        uiOutput("mobile_units_panel")
      )
    )
  ),
  fluidRow(
    column(
      width = 12,
      navset_card_pill(
        nav_panel(
          "Age & sex-piramides",
          card(
            card_body(
              uiOutput("p_zone_container")
            )
          )
        ),
        nav_panel(
          "Plus-grafiek",
          card(
            card_body(
              uiOutput("p_plus_container")
            )
          )
        ),
        nav_panel(
          "Doorlooptijden",
          card(
            card_body(
              uiOutput("doorlooptijd_cards"),
              fluidRow(
                column(6, withSpinner(plotOutput("p_trans_time", height = 350))),
                column(6, withSpinner(plotOutput("p_trans_dist", height = 350)))
              ),
              withSpinner(plotOutput("p_trans_box", height = 320)),
              withSpinner(plotOutput("p_trans_flag", height = 250))
            )
          )
        ),
        nav_panel(
          "Data",
          card(card_body(withSpinner(DTOutput("tbl"))))
        ),
        nav_panel(
          "Diagnose",
          card(
            card_body(
              fluidRow(
                column(6, h5("Kolomnamen (origineel)"), verbatimTextOutput("cols_raw")),
                column(6, h5("Kolomnamen (na mapping)"), verbatimTextOutput("cols_map"))
              ),
              hr(),
              fluidRow(
                column(6, h5("Datumbereik in data"), verbatimTextOutput("range_dates")),
                column(6, h5("Unieke waarden"), verbatimTextOutput("uniques"))
              )
            )
          )
        ),
        nav_panel(
          "Extraction QC",
          card(card_body(qcUI("qc")))
        )
      )
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

  used_file    <- reactiveVal("")
  chosen_file  <- reactiveVal(NULL)
  biobank_root <- reactiveVal(NULL)

  app_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)
  cache_dir <- file.path(app_dir, ".cache")
  cache_file <- file.path(cache_dir, "paths.rds")
  stored_paths <- reactiveVal(NULL)
  session$userData$bookmarking_active <- FALSE

  observeEvent(TRUE, {
    paths <- tryCatch(readRDS(cache_file), error = function(e) NULL)
    if (is.list(paths)) {
      stored_paths(paths)
      if (!is.null(paths$root)) updateTextInput(session, "root", value = paths$root)
      if (!is.null(paths$qc_dir)) updateTextInput(session, "qc-qc_dir", value = paths$qc_dir)
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
    df <- filtered(); if (is.null(df) || !nrow(df)) return(NULL)

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
      helpText("Upload een GeoJSON/Shapefile of vink GRID3 aan.")
    }
  })

  # ---------- Gezondheidszones kaart ----------
  zones_sf <- reactive({
    if (isTRUE(input$use_grid3)) {
      shiny::withProgress(message = "GRID3-zones laden...", value = 0.5, {
        sf <- try(read_grid3_health_zones(), silent = TRUE)
        if (inherits(sf, "try-error")) {
          showNotification("GRID3-zones konden niet geladen worden (mogelijk ontbreekt internet of is toegang geweigerd).", type = "error")
          return(NULL)
        }
        flatten_grid3_cols(sf)
      })
    } else {
      f <- input$zones_geo
      if (is.null(f)) return(NULL)
      sf <- try(sf::read_sf(f$datapath, quiet = TRUE), silent = TRUE)
      if (inherits(sf, "try-error")) {
        sf <- try(sf::st_read(f$datapath, quiet = TRUE, stringsAsFactors = FALSE), silent = TRUE)
      }
      if (!inherits(sf, "sf")) {
        showNotification("Kon kaartlaag niet lezen. Controleer het bestand.", type = "error")
        return(NULL)
      }
      flatten_grid3_cols(sf)
    }
  })

  observe({
    g <- zones_sf()
    if (is.null(g)) return()
    geom_col <- attr(g, "sf_column")

    simple_cols <- setdiff(names(g), geom_col)
    simple_cols <- simple_cols[ !vapply(g[simple_cols], is.list, TRUE) ]
    if (!length(simple_cols)) simple_cols <- setdiff(names(g), geom_col)

    cand_zone <- intersect(simple_cols, c("zone","zonesante","zs","zs_name","nom_zs","name","NAME","NOM"))
    cand_prov <- intersect(simple_cols, c("province","prov","nom_prov","adm1name","ADM1NAME","NAME_1","name_1"))

    updateSelectInput(session, "zones_name_col",
      choices  = simple_cols,
      selected = if (length(cand_zone)) cand_zone[1] else simple_cols[1]
    )
    updateSelectInput(session, "prov_name_col",
      choices  = simple_cols,
      selected = if (length(cand_prov)) cand_prov[1] else simple_cols[1]
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

    df <- filtered(); if (is.null(df) || !nrow(df)) return(empty_units)

    lat_col <- find_first_matching_column(df, c("latitude", "lat", "gps_lat", "lat_dd", "latitude_decimal", "coord_y", "y_coord"))
    lon_col <- find_first_matching_column(df, c("longitude", "lon", "gps_lon", "lon_dd", "longitude_decimal", "coord_x", "x_coord"))

    lat <- if (!is.null(lat_col)) suppressWarnings(readr::parse_number(df[[lat_col]])) else rep(NA_real_, nrow(df))
    lon <- if (!is.null(lon_col)) suppressWarnings(readr::parse_number(df[[lon_col]])) else rep(NA_real_, nrow(df))

    df_valid <- df

    unit_col <- NULL
    if ("unite_mobile" %in% names(df_valid)) {
      unit_col <- "unite_mobile"
    } else {
      unit_col <- find_first_matching_column(df_valid, c("mobile", "unite", "unit", "site", "facility", "centre"))
    }
    unit_raw <- if (!is.null(unit_col)) df_valid[[unit_col]] else df_valid$zone
    unit_clean <- stringr::str_trim(as.character(unit_raw))

    zone_name <- stringr::str_trim(as.character(df_valid$zone))
    province_name <- stringr::str_trim(as.character(df_valid$province))
    last_sample_num <- suppressWarnings(as.numeric(df_valid$date_prelev))

    tibble(
      unit = unit_clean,
      zone_name = zone_name,
      province_name = province_name,
      zone_label = dplyr::case_when(
        !is.na(zone_name) & zone_name != "" & !is.na(province_name) & province_name != "" ~ paste0(zone_name, " · ", province_name),
        !is.na(zone_name) & zone_name != "" ~ zone_name,
        !is.na(province_name) & province_name != "" ~ province_name,
        TRUE ~ NA_character_
      ),
      lat = lat,
      lon = lon,
      study = df_valid$study,
      last_sample_num = last_sample_num
    ) |>
      mutate(
        unit = dplyr::coalesce(dplyr::na_if(unit, ""), "Mobiele unit onbekend"),
        zone_label = dplyr::coalesce(dplyr::na_if(zone_label, ""), "Zone onbekend")
      ) |>
      group_by(unit, zone_label) |>
      summarise(
        lat = suppressWarnings(mean(lat, na.rm = TRUE)),
        lon = suppressWarnings(mean(lon, na.rm = TRUE)),
        n = dplyr::n(),
        n_da = sum(toupper(study) == "DA", na.rm = TRUE),
        n_dp = sum(toupper(study) == "DP", na.rm = TRUE),
        last_sample = suppressWarnings(max(last_sample_num, na.rm = TRUE)),
        .groups = "drop"
      ) |>
      mutate(
        lat = ifelse(is.finite(lat), lat, NA_real_),
        lon = ifelse(is.finite(lon), lon, NA_real_),
        n_da = dplyr::coalesce(as.integer(n_da), 0L),
        n_dp = dplyr::coalesce(as.integer(n_dp), 0L),
        last_sample = dplyr::if_else(
          is.finite(last_sample),
          as.Date(last_sample, origin = "1970-01-01"),
          as.Date(NA)
        ),
        has_coords = is.finite(lat) & is.finite(lon)
      ) |>
      arrange(desc(n))
  })

  zone_hover_id <- reactiveVal(NULL)

  observeEvent(input$map_zones_shape_mouseover, {
    ev <- input$map_zones_shape_mouseover
    if (!is.null(ev$id)) zone_hover_id(ev$id)
  })

  observeEvent(input$map_zones_shape_mouseout, {
    zone_hover_id(NULL)
  })

  observeEvent(input$map_zones_shape_click, {
    ev <- input$map_zones_shape_click
    if (is.null(ev$id)) return()
    zone_hover_id(ev$id)
    s <- zone_summary()
    if (!is.null(s) && nrow(s) && ev$id %in% s$zone_uid && !is.null(input$zone_detail_select)) {
      updateSelectInput(session, "zone_detail_select", selected = ev$id)
    }
  })

  zone_focus_id <- reactive({
    sel <- input$zone_detail_select
    if (!is.null(sel) && sel != "ALL") return(sel)
    hz <- zone_hover_id()
    if (!is.null(hz)) return(hz)
    "ALL"
  })

  zone_detail_data <- reactive({
    df <- filtered(); if (is.null(df) || !nrow(df)) return(NULL)
    s <- zone_summary()
    focus <- zone_focus_id()

    build_info <- function(total, n_da, n_dp, pct_f, med_age, last_sample, label, subtitle = NULL, pct_da = NA_real_, pct_dp = NA_real_, source = "overall"){
      if (!inherits(last_sample, "Date")) {
        if (!is.null(last_sample) && length(last_sample) && is.finite(last_sample)) {
          last_sample <- as.Date(last_sample, origin = "1970-01-01")
        } else {
          last_sample <- as.Date(NA)
        }
      }
      if (!is.finite(med_age)) med_age <- NA_real_
      list(
        title = label,
        subtitle = subtitle,
        total = total,
        n_da = n_da,
        n_dp = n_dp,
        pct_f = pct_f,
        pct_da = pct_da,
        pct_dp = pct_dp,
        med_age = med_age,
        last_sample = last_sample,
        source = source
      )
    }

    if (!is.null(s) && nrow(s) && !is.null(focus) && focus != "ALL") {
      row <- s |> filter(zone_uid == focus)
      if (nrow(row)) {
        label <- dplyr::coalesce(row$zone, "Onbekende zone")
        subtitle <- dplyr::coalesce(row$province, NA_character_)
        return(build_info(row$n, row$n_da, row$n_dp, row$pct_f, row$med_age, row$last_sample, label, subtitle, row$pct_da, row$pct_dp, source = "zone"))
      }
    }

    n_all <- nrow(df)
    n_da <- sum(toupper(df$study) == "DA", na.rm = TRUE)
    n_dp <- sum(toupper(df$study) == "DP", na.rm = TRUE)
    n_f  <- sum(df$sex_clean == "F", na.rm = TRUE)
    pct_f <- ifelse(n_all > 0, round(100 * n_f / n_all, 1), NA_real_)
    pct_da <- ifelse(n_all > 0, round(100 * n_da / n_all, 1), NA_real_)
    pct_dp <- ifelse(n_all > 0, round(100 * n_dp / n_all, 1), NA_real_)
    med_age <- suppressWarnings(stats::median(df$age_num, na.rm = TRUE))
    last_sample <- suppressWarnings(max(df$date_prelev, na.rm = TRUE))
    build_info(n_all, n_da, n_dp, pct_f, med_age, last_sample, "Alle zones", source = "overall", pct_da = pct_da, pct_dp = pct_dp)
  })

  output$zone_detail_select_ui <- renderUI({
    s <- zone_summary()
    if (is.null(s) || !nrow(s)) {
      return(helpText("Laad data en kies een zone om detail te bekijken."))
    }
    s <- s |> filter(!is.na(zone_uid))
    if (!nrow(s)) {
      return(helpText("Geen zones met sleutel beschikbaar in de kaartlaag."))
    }
    labels <- paste0(dplyr::coalesce(s$zone, "Onbekend"), " · ", dplyr::coalesce(s$province, "?"))
    choices <- setNames(c("ALL", s$zone_uid), c("Alle zones", labels))
    selected <- isolate(input$zone_detail_select)
    if (is.null(selected) || !selected %in% choices) selected <- "ALL"
    selectInput("zone_detail_select", "Zone voor detail", choices = choices, selected = selected)
  })

  output$zone_detail_panel <- renderUI({
    info <- zone_detail_data()
    if (is.null(info)) {
      return(helpText("Geen data om te tonen."))
    }

    fmt_int <- function(x) ifelse(is.na(x), "0", formatC(as.integer(x), format = "d", big.mark = " "))
    fmt_pct <- function(x) ifelse(is.na(x), "–", paste0(scales::number(x, accuracy = 0.1), "%"))
    fmt_age <- function(x) ifelse(is.na(x), "–", round(as.numeric(x), 1))
    fmt_date <- function(x) {
      if (inherits(x, "Date")) {
        return(ifelse(is.na(x), "–", format(x, "%d %b %Y")))
      }
      "–"
    }

    metrics <- tagList(
      div(class = "d-flex justify-content-between", span("Totale stalen"), strong(fmt_int(info$total))),
      div(class = "d-flex justify-content-between", span("DA"), span(sprintf("%s (%s)", fmt_int(info$n_da), fmt_pct(info$pct_da)))),
      div(class = "d-flex justify-content-between", span("DP"), span(sprintf("%s (%s)", fmt_int(info$n_dp), fmt_pct(info$pct_dp)))),
      div(class = "d-flex justify-content-between", span("% vrouw"), strong(fmt_pct(info$pct_f))),
      div(class = "d-flex justify-content-between", span("Mediaan leeftijd"), strong(fmt_age(info$med_age))),
      div(class = "d-flex justify-content-between", span("Laatste staal"), strong(fmt_date(info$last_sample)))
    )

    footnote <- if (identical(info$source, "zone")) {
      tags$small(class = "text-muted d-block mt-2", "Cijfers op basis van huidige selectie voor deze zone.")
    } else {
      tags$small(class = "text-muted d-block mt-2", "Hover over een zone of kies er één via het menu om details te zien.")
    }

    tagList(
      tags$h5(class = "mb-1", info$title),
      if (!is.null(info$subtitle) && !is.na(info$subtitle)) tags$div(class = "text-muted small mb-2", info$subtitle),
      div(class = "d-flex flex-column gap-1 small", metrics),
      footnote
    )
  })

  prov_outline_color <- function(prov_norm){
    ifelse(prov_norm %in% c("kasai oriental","kasaï oriental"), "#F1C40F",
           ifelse(prov_norm == "lomami", "#E74C3C", "#555555"))
  }

  zones_map_data <- reactive({
    g <- zones_sf()
    s <- zone_summary()
    if (is.null(g) || is.null(s) || !nrow(s)) return(NULL)
    req(input$zones_name_col, input$prov_name_col)
    g$zone_key <- normalize_names(as.character(g[[input$zones_name_col]]))
    g$prov_key <- normalize_names(as.character(g[[input$prov_name_col]]))
    s$zone_key <- normalize_names(as.character(s$zone))
    s$prov_key <- normalize_names(as.character(s$province))

    gj <- dplyr::left_join(g, s, by = c("zone_key", "prov_key"))
    gj$zone_uid <- dplyr::coalesce(gj$zone_uid, paste(gj$zone_key, gj$prov_key, sep = "__"))
    gj$layer_id <- gj$zone_uid

    metric <- switch(input$map_metric_zs,
      n       = gj$n,
      n_da    = gj$n_da,
      n_dp    = gj$n_dp,
      pct_f   = gj$pct_f,
      pct_da  = gj$pct_da,
      pct_dp  = gj$pct_dp,
      med_age = gj$med_age,
      gj$n
    )
    metric_num <- suppressWarnings(as.numeric(metric))
    pal <- colorBin("YlGnBu", domain = metric_num, bins = input$map_bins_zs, na.color = "#cccccc")

    legend_lookup <- c(
      n = "Aantal stalen",
      n_da = "DA-stalen",
      n_dp = "DP-stalen",
      pct_f = "% vrouw",
      pct_da = "% DA",
      pct_dp = "% DP",
      med_age = "Mediaan leeftijd"
    )
    legend_title <- legend_lookup[[input$map_metric_zs]]

    list(gj = gj, metric = metric_num, pal = pal, legend_title = legend_title)
  })

  output$map_zones <- renderLeaflet({
    data <- zones_map_data()
    validate(need(!is.null(data), "Geen kaartgegevens beschikbaar. Upload een kaart of gebruik GRID3."))
    gj <- data$gj
    metric <- data$metric
    pal <- data$pal
    metric_title <- ifelse(is.null(data$legend_title) || is.na(data$legend_title), "", data$legend_title)
    border_cols <- if (isTRUE(input$outline_by_prov)) prov_outline_color(gj$prov_key) else "#444444"
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
        weight = 1.2, color = border_cols, opacity = 1,
        fillOpacity = 0.85, fillColor = ~pal(metric),
        layerId = ~layer_id,
        label = ~lapply(
          sprintf(
            "<b>Zone:</b> %s<br/><b>Provincie:</b> %s<br/><b>Totaal:</b> %s<br/><b>DA:</b> %s<br/><b>DP:</b> %s<br/><b>% vrouw:</b> %s<br/><b>Mediaan leeftijd:</b> %s<br/><b>Laatste staal:</b> %s",
            ifelse(is.na(gj[[input$zones_name_col]]), "?", gj[[input$zones_name_col]]),
            ifelse(is.na(gj[[input$prov_name_col]]),  "?", gj[[input$prov_name_col]]),
            ifelse(is.na(n), 0, fmt_int(n)),
            ifelse(is.na(n_da), 0, fmt_int(n_da)),
            ifelse(is.na(n_dp), 0, fmt_int(n_dp)),
            fmt_pct(pct_f),
            ifelse(is.na(med_age), "-", round(med_age, 1)),
            fmt_date(last_sample)
          ),
          htmltools::HTML
        ),
        highlightOptions = highlightOptions(weight = 2, color = "#000000", bringToFront = TRUE)
      )

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

  output$mobile_units_panel <- renderUI({
    units <- mobile_units()
    if (is.null(units) || !nrow(units)) {
      return(helpText("Geen mobiele units met coördinaten binnen de huidige selectie."))
    }

    fmt_int <- function(x) ifelse(is.na(x), "0", formatC(as.integer(x), format = "d", big.mark = " "))
    fmt_date <- function(x) {
      if (inherits(x, "Date")) {
        return(ifelse(is.na(x), "–", format(x, "%d %b %Y")))
      }
      "–"
    }

    top_units <- head(units, 5)
    items <- purrr::map(seq_len(nrow(top_units)), function(i){
      u <- top_units[i, ]
      tags$div(
        class = "mobile-unit-item",
        tags$strong(u$unit),
        tags$br(),
        tags$span(class = "text-muted", u$zone_label),
        tags$br(),
        tags$small(
          sprintf("Stalen: %s (DA: %s · DP: %s) · Laatste staal: %s",
                  fmt_int(u$n), fmt_int(u$n_da), fmt_int(u$n_dp), fmt_date(u$last_sample))
        )
      )
    })

    extras <- if (nrow(units) > 5) {
      list(tags$small(class = "text-muted", sprintf("+%s extra mobiele units", nrow(units) - 5)))
    } else list()

    no_coord <- sum(!units$has_coords, na.rm = TRUE)
    if (no_coord > 0) {
      extras <- c(extras, list(tags$small(class = "text-muted", sprintf("%s unit(s) zonder coördinaten worden niet op de kaart getoond.", no_coord))))
    }

    do.call(tagList, c(items, extras))
  })

  focus_kasai_view <- function(){
    data <- zones_map_data()
    req(!is.null(data))
    gj <- data$gj
    if (!inherits(gj, "sf")) return()
    focus <- gj |> dplyr::filter(prov_key %in% c("kasai oriental", "lomami"))
    req(nrow(focus) > 0)
    bbox <- sf::st_bbox(focus)
    leafletProxy("map_zones") |>
      fitBounds(lng1 = bbox['xmin'], lat1 = bbox['ymin'], lng2 = bbox['xmax'], lat2 = bbox['ymax'])
  }

  observeEvent(input$focus_kasai, { focus_kasai_view() }, ignoreNULL = TRUE)
  observeEvent(input$focus_kasai_top, { focus_kasai_view() }, ignoreNULL = TRUE)

  observeEvent(input$reset_map, {
    data <- zones_map_data()
    req(!is.null(data))
    gj <- data$gj
    if (!inherits(gj, "sf")) return()
    bbox <- sf::st_bbox(gj)
    leafletProxy("map_zones") |>
      fitBounds(lng1 = bbox['xmin'], lat1 = bbox['ymin'], lng2 = bbox['xmax'], lat2 = bbox['ymax'])
  }, ignoreNULL = TRUE)


  output$cols_raw <- renderPrint({
    df0 <- raw_data(); if (is.null(df0)) return("[geen raw data]")
    names(df0)
  })

  output$cols_map <- renderPrint({
    dfm <- mapped_data(); if (is.null(dfm)) return("[geen mapped data]")
    names(dfm)
  })

  output$range_dates <- renderPrint({
    df <- biobank(); if (is.null(df)) return("[geen data]")
    range(df$date_prelev, na.rm = TRUE)
  })

  output$uniques <- renderPrint({
    df <- biobank(); if (is.null(df)) return("[geen data]")
    list(
      study    = sort(unique(na.omit(df$study)))[1:20],
      province = sort(unique(na.omit(df$province)))[1:20],
      zone     = sort(unique(na.omit(df$zone)))[1:20]
    )
  })

  output$dl_csv <- downloadHandler(
    filename = function(){ glue("biobank_filtered_{format(Sys.Date(), '%Y%m%d')}.csv") },
    content  = function(file){ readr::write_csv(filtered(), file) }
  )

  output$dl_plot <- downloadHandler(
    filename = function(){ glue("age_sex_pyramids_{format(Sys.Date(), '%Y%m%d')}.png") },
    content  = function(file){
      p <- plot_zone_pyramids(filtered(), bin = input$bin, min_n_per_zone = input$minN, split_study = input$split_study)
      ggsave(file, p, width = 14, height = 9, dpi = 300)
    }
  )

  output$dl_plot_pdf <- downloadHandler(
    filename = function(){ glue("age_sex_pyramids_{format(Sys.Date(), '%Y%m%d')}.pdf") },
    content  = function(file){
      p <- plot_zone_pyramids(filtered(), bin = input$bin, min_n_per_zone = input$minN, split_study = input$split_study)
      ggsave(file, p, width = 14, height = 9, device = grDevices::cairo_pdf) # PDF export using Cairo for crisp text
    }
  )
}


shinyApp(ui, server)
