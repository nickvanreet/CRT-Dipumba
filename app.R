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
      volume_raw = if ("volume_total_echantillon_sang_drs_ml" %in% names(.))
        volume_total_echantillon_sang_drs_ml else NA_character_,
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
          Barcode, LabID, zone, province, date_prelev,
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
        age_num > 1900 ~ lubridate::year(Sys.Date()) - age_num,
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
  max_age <- ceiling(max(df$age_num, na.rm = TRUE)/bin) * bin
  df |>
    mutate(age_band = cut(age_num, breaks = seq(0, max_age, by = bin), right = FALSE, include.lowest = TRUE))
}

# GRID3 helpers ---------------------------------------------------------------
read_grid3_health_zones <- function(fields = c("province", "zonesante", "zs_uid")){
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
    sf <- try(sf::read_sf(url, quiet = TRUE), silent = TRUE)
    if (!inherits(sf, "try-error") && nrow(sf) > 0) {
      nms <- names(sf)
      if (!"province"  %in% nms) names(sf)[match(tolower(nms), "province")]  <- "province"
      if (!"zonesante" %in% nms) names(sf)[match(tolower(nms), "zonesante")] <- "zonesante"
      if (!"zs_uid"    %in% nms && "zsuid" %in% tolower(nms)) {
        names(sf)[match(tolower(nms), "zsuid")] <- "zs_uid"
      }
      return(sf)
    }
    errs[[b]] <- if (inherits(sf, "try-error")) as.character(sf) else "no rows"
  }
  stop("Could not fetch GRID3 health zones from public endpoints.\n",
       "Some services now require an access token.\n",
       "Tried:\n- ", paste(names(errs), collapse = "\n- "),
       "\nIf blocked, download the file from GRID3’s DRC data page and read locally.")
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
  tags$head(tags$style(HTML(".summary-card-label {text-transform: uppercase; font-size: 0.75rem; letter-spacing: 0.05em; color: #6c757d;}\n.summary-card-value {font-size: 2rem; font-weight: 600;}\n.summary-card .card-body {display: flex; flex-direction: column; gap: 0.15rem;}"))),
  layout_sidebar(
    sidebar = sidebar(
      h4("MBUJI-MAYI BIOBANK AGE–SEX"),
      helpText("Selecteer de map met biobank-Excelbestanden, kies een bestand en klik Vernieuwen."),
      textInput("root", "Biobank-map", value = "C:/Users/nvanreet/ITG/THA - Digital Management System - CRT Dipumba Upload - CRT Dipumba Upload/01 - Biobanque"),
      uiOutput("filepick_ui"),
      actionButton("refresh", "Vernieuwen"),
      div(style = "margin-top:6px; font-size: 0.9rem;", strong("Bestand:"), textOutput("used_file", inline = TRUE)),
      hr(),
      dateRangeInput("daterng", "Datumbereik (date_prelev)", start = Sys.Date() - 180, end = Sys.Date()),
      numericInput("bin", "Leeftijdsband (jaar)", value = 5, min = 1, step = 1),
      numericInput("minN", "Min. N per zone (filter)", value = 15, min = 1, step = 1),
      checkboxInput("split_study", "Onderverdeel per studie (DA/DP)", value = FALSE),
      selectInput("study", "Filter studie", choices = c("Alle"), selected = "Alle"),
      selectInput("facet", "Facet voor ‘Plus’ grafiek", choices = c("zone","province"), selected = "zone"),
      uiOutput("province_ui"),
      uiOutput("zone_ui"),
      hr(),
      downloadButton("dl_csv", "Download CSV"),
      downloadButton("dl_plot", "Download plot (PNG)")
    ),
    card(
      card_header("Overzicht & Plotten"),
      uiOutput("summary_cards"),
      tabsetPanel(
        tabPanel("Zone-piramides", plotOutput("p_zone", height = 650)),
        tabPanel("Plus-grafiek", plotOutput("p_plus", height = 650)),
        tabPanel("Doorlooptijden",
                 uiOutput("doorlooptijd_cards"),
                 fluidRow(
                   column(6, plotOutput("p_trans_time", height = 350)),
                   column(6, plotOutput("p_trans_dist", height = 350))
                 ),
                 plotOutput("p_trans_box", height = 320)
        ),
        tabPanel("Kaart (gezondheidszones)",
                 fluidRow(
                   column(4,
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
                          selectInput("map_metric_zs", "Metingen",
                                      choices = c("Aantal stalen" = "n",
                                                  "% vrouw" = "pct_f",
                                                  "Mediaan leeftijd" = "med_age"),
                                      selected = "n"),
                          checkboxInput("outline_by_prov", "Contour in kleur per provincie (Kasai-Oriental geel, Lomami rood)", value = TRUE),
                          numericInput("map_bins_zs", "Kleur-bins", value = 5, min = 3, max = 9)
                   ),
                   column(8, leafletOutput("map_zones", height = 700))
                 )
        ),
        tabPanel("Data", DTOutput("tbl")),
        tabPanel("Diagnose",
                 fluidRow(
                   column(6, h5("Kolomnamen (origineel)"), verbatimTextOutput("cols_raw")),
                   column(6, h5("Kolomnamen (na mapping)"), verbatimTextOutput("cols_map"))
                 ),
                 hr(),
                 fluidRow(
                   column(6, h5("Datumbereik in data"), verbatimTextOutput("range_dates")),
                   column(6, h5("Unieke waarden"), verbatimTextOutput("uniques"))
                 )
        ),
        tabPanel("Extraction QC",
                 fluidRow(
                   column(
                     width = 3,
                     card(
                       card_header("Instellingen"),
                       card_body(
                         textInput(
                           "qc_dir",
                           "Map met extraction-bestanden",
                           value = "C:/Users/nvanreet/ITG/THA - Digital Management System - CRT Dipumba Upload - CRT Dipumba Upload/02 - Extractions"
                         ),
                         actionButton("qc_refresh", "Vernieuwen"),
                         div(class = "mt-2", textOutput("qc_status")),
                         hr(),
                         sliderInput(
                           "qc_rng", "Acceptabel volumebereik (mL)",
                           min = 0.5, max = 3.5, value = c(1.5, 2.5), step = 0.1
                         ),
                         selectInput("qc_prov", "Provincie", choices = "Alle", selected = "Alle"),
                         selectInput("qc_zone", "Zone de santé", choices = "Alle", selected = "Alle"),
                         dateRangeInput(
                           "qc_date_rng", "Filter op datum de prélèvement",
                           start = NULL, end = NULL, format = "yyyy-mm-dd", weekstart = 1
                         ),
                         radioButtons(
                           "qc_agg", "Tijdresolutie",
                           choices = c("Dag" = "day", "Maand" = "month", "Jaar" = "year"),
                           selected = "day", inline = TRUE
                         ),
                         checkboxInput("qc_show_out", "Toon enkel out-of-range", value = FALSE),
                         hr(),
                         downloadButton("qc_dl_zone", "Download Zone Summary (CSV)"),
                         downloadButton("qc_dl_out", "Download Outliers (CSV)"),
                         downloadButton("qc_dl_dup", "Download Duplicaten (CSV)"),
                         downloadButton("qc_dl_filt", "Download Gefilterde Dataset (CSV)"),
                         downloadButton("qc_dl_agg", "Download Tijdreeks (CSV)")
                       )
                     )
                   ),
                   column(
                     width = 9,
                     tabsetPanel(
                       tabPanel(
                         "Overzicht",
                         plotOutput("qc_hist_vol", height = "320px"),
                         plotOutput("qc_box_vol", height = "260px")
                       ),
                       tabPanel(
                         "Per Zone",
                         plotOutput("qc_trend_filedate", height = "320px"),
                         DTOutput("qc_zone_tbl")
                       ),
                       tabPanel(
                         "Tijdreeksen",
                         plotOutput("qc_agg_count_plot", height = "280px"),
                         plotOutput("qc_agg_volume_plot", height = "280px"),
                         DTOutput("qc_agg_tbl")
                       ),
                       tabPanel("Per-sample", DTOutput("qc_per_sample_tbl")),
                       tabPanel("Outliers", DTOutput("qc_out_tbl")),
                       tabPanel("Duplicaten", DTOutput("qc_dup_tbl"))
                     )
                   )
                 )
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
    x <- ifelse(is.na(x), "", x)
    x <- stringi::stri_trans_general(x, "Latin-ASCII")
    x <- tolower(trimws(x))
    gsub("[\\s_-]+", " ", x)
  }

  used_file    <- reactiveVal("")
  chosen_file  <- reactiveVal(NULL)
  biobank_root <- reactiveVal(NULL)

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
        transp_inrb    = safe_days(date_env_inrb, date_rec_cpltha, max_ok = 90)
      ) |>
      distinct(Barcode, LabID, .keep_all = TRUE) |>
      clean_age_sex()
    if (!nrow(df)) return(NULL)
    df
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

  observeEvent(qc_dedup(), {
    df <- qc_dedup()
    if (is.null(df) || !nrow(df) || !"province" %in% names(df)) {
      updateSelectInput(session, "qc_prov", choices = "Alle", selected = "Alle")
      updateSelectInput(session, "qc_zone", choices = "Alle", selected = "Alle")
      return()
    }

    provs <- sort(unique(na.omit(df$province)))
    sel_prov <- if (input$qc_prov %in% c("Alle", provs)) input$qc_prov else "Alle"
    updateSelectInput(session, "qc_prov", choices = c("Alle", provs), selected = sel_prov)

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

  filtered <- reactive({
    df <- biobank(); if (is.null(df)) return(df)
    rng <- input$daterng
    if (!is.null(rng)) df <- df |> filter(!is.na(date_prelev), date_prelev >= rng[1], date_prelev <= rng[2])
    if (!is.null(input$province) && length(input$province)) df <- df |> filter(is.na(province) | province %in% input$province)
    if (!is.null(input$zone) && length(input$zone))       df <- df |> filter(is.na(zone) | zone %in% input$zone)
    if (!is.null(input$study) && input$study != "Alle")  df <- df |> filter(toupper(study) == toupper(input$study))
    df
  })

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
        temp_cpltha = if (has_cpltha_temp) temp_cpltha else NA_character_
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
        segment = factor(segment, levels = c("Field → HS", "HS → LSD", "LSD → INRB")),
        temp_code = factor(temp_code, levels = c("Ambiante", "Frigo", "Congélateur", "Onbekend")),
        week = lubridate::floor_date(sample_date, "week")
      )
  })

  output$summary_cards <- renderUI({
    df <- filtered()
    n_all <- if (is.null(df)) 0L else nrow(df)
    n_prelev <- if (is.null(df)) 0L else sum(!is.na(df$date_prelev))
    n_barcode <- if (is.null(df)) 0L else n_distinct(df$Barcode, na.rm = TRUE)

    fmt <- function(x) {
      formatC(as.integer(x), format = "d", big.mark = " ")
    }

    stats <- list(
      list(label = "Records (selectie)", value = fmt(n_all)),
      list(label = "Met date_prelev", value = fmt(n_prelev)),
      list(label = "Unieke barcodes", value = fmt(n_barcode))
    )

    cards <- lapply(stats, function(x) {
      card(
        card_body(
          class = "summary-card",
          div(class = "summary-card-label", x$label),
          div(class = "summary-card-value", x$value)
        )
      )
    })

    do.call(layout_column_wrap, c(list(width = 1/3), cards))
  })

  output$p_zone <- renderPlot({
    df <- filtered(); validate(need(!is.null(df) && nrow(df) > 0, "Geen rijen binnen de selectie."))
    plot_zone_pyramids(df, bin = input$bin, min_n_per_zone = input$minN, split_study = input$split_study)
  })

  output$p_plus <- renderPlot({
    df <- filtered(); validate(need(!is.null(df) && nrow(df) > 0, "Geen rijen binnen de selectie."))
    plot_age_sex_pyramid_plus(df, facet_by = input$facet, bin = input$bin, split_study = input$split_study)
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
    fmt_int <- function(x) formatC(as.integer(x), format = "d", big.mark = " ")

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

    ggplot(weekly, aes(week, mean_days, colour = segment)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_colour_manual(values = seg_cols, name = "Segment") +
      scale_x_date(date_labels = "%d %b", date_breaks = "2 weeks") +
      labs(x = NULL, y = "Gemiddelde dagen", title = "Gemiddelde doorlooptijd per week") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$p_trans_dist <- renderPlot({
    dat <- transport_long();
    validate(need(!is.null(dat) && nrow(dat) > 0, "Geen doorlooptijdgegevens."))

    seg_cols <- setNames(c("#1b9e77", "#d95f02", "#7570b3"), levels(dat$segment))

    ggplot(dat, aes(sample_date, days, colour = segment)) +
      geom_point(alpha = 0.55, size = 1.8) +
      facet_wrap(~segment, ncol = 1, scales = "free_y") +
      scale_colour_manual(values = seg_cols, guide = "none") +
      scale_x_date(date_labels = "%d %b", date_breaks = "2 weeks") +
      labs(x = NULL, y = "Dagen", title = "Doorlooptijd per staal over de tijd") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$p_trans_box <- renderPlot({
    dat <- transport_long();
    validate(need(!is.null(dat) && nrow(dat) > 0, "Geen doorlooptijdgegevens."))

    seg_cols <- setNames(c("#1b9e77", "#d95f02", "#7570b3"), levels(dat$segment))
    shape_map <- c("Ambiante" = 21, "Frigo" = 24, "Congélateur" = 22, "Onbekend" = 16)

    ggplot(dat, aes(segment, days, fill = segment)) +
      geom_boxplot(width = 0.6, alpha = 0.85, colour = "grey30", outlier.shape = NA) +
      geom_jitter(aes(shape = temp_code), width = 0.1, height = 0, size = 2, alpha = 0.6) +
      scale_fill_manual(values = seg_cols, guide = "none") +
      scale_shape_manual(values = shape_map, name = "Temperatuur") +
      labs(x = NULL, y = "Dagen", title = "Verdeling per segment", subtitle = "Punten tonen temperatuurcategorie") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 15, hjust = 1))
  })

  output$tbl <- renderDT({
    df <- filtered(); if (is.null(df)) df <- tibble()
    datatable(df |> select(Barcode, LabID, province, zone, study, sex_clean, age_num, date_prelev, date_env_cpltha, date_rec_cpltha, date_env_inrb, starts_with("transp_")), options = list(pageLength = 25, scrollX = TRUE))
  })

  output$qc_status <- renderText({ qc_status() })

  output$qc_hist_vol <- renderPlot({
    df <- qc_filtered()
    validate(need(nrow(df) > 0 && "volume_num" %in% names(df), "Geen data beschikbaar."))
    rng <- input$qc_rng
    ggplot(df, aes(x = volume_num,
                   fill = volume_num < rng[1] | volume_num > rng[2])) +
      geom_histogram(binwidth = 0.1, colour = "black") +
      geom_vline(xintercept = 2, linetype = "dashed", colour = "red") +
      scale_fill_manual(values = c(`FALSE` = "steelblue", `TRUE` = "tomato"), guide = "none") +
      labs(
        title = "Verdeling van samplevolumes",
        x = "Volume (mL)",
        y = "Aantal",
        subtitle = sprintf("Gestreept = 2 mL | Range = %.1f–%.1f mL", rng[1], rng[2])
      ) +
      theme_minimal()
  })

  output$qc_box_vol <- renderPlot({
    df <- qc_filtered()
    validate(need(nrow(df) > 0 && "volume_num" %in% names(df), "Geen data beschikbaar."))
    rng <- input$qc_rng
    ggplot(df, aes(y = volume_num)) +
      geom_boxplot(fill = "grey90", outlier.colour = "tomato") +
      geom_jitter(height = 0, width = 0.1, alpha = 0.5) +
      geom_hline(yintercept = 2, linetype = "dashed", colour = "red") +
      geom_hline(yintercept = rng, linetype = "dotted", colour = "grey40") +
      labs(title = "Volumes met outliers", y = "Volume (mL)") +
      theme_minimal()
  })

  output$qc_trend_filedate <- renderPlot({
    df <- qc_filtered()
    validate(need(nrow(df) > 0 && "file_date" %in% names(df) && "volume_num" %in% names(df), "Geen data beschikbaar."))
    ggplot(df, aes(x = file_date, y = volume_num, colour = zone)) +
      geom_jitter(alpha = 0.4, width = 2) +
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
    keep <- c("province", "zone", "numero", "code_barres_kps", "date_prelev", "date_rec_cpltha",
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
        arrange(volume_num)
    }
    keep <- c("province", "zone", "numero", "code_barres_kps", "volume_ml", "volume_num", "date_prelev", "file_date", "source_file")
    datatable(df |> select(any_of(keep)), options = list(pageLength = 20, scrollX = TRUE))
  })

  output$qc_dup_tbl <- renderDT({
    flagged <- qc_flagged_data()$flagged
    if (is.null(flagged) || !nrow(flagged) || !"flag" %in% names(flagged)) {
      flagged <- tibble()
    } else {
      flagged <- flagged |> filter(flag != "OK")
    }
    keep <- c("flag", "province", "zone", "numero", "code_barres_kps", "volume_ml", "volume_num", "date_prelev", "file_date", "source_file")
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
        filter(!is.na(volume_num) & (volume_num < rng[1] | volume_num > rng[2]))
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
        showNotification("Kon kaartlaag niet lezen. Controleer het bestand.", type = "error")
        return(NULL)
      }
      if (inherits(sf, "sf")) flatten_grid3_cols(sf) else NULL
    }
  })

  observe({
    g <- zones_sf()
    if (is.null(g)) return()
    nms <- names(g)
    cand_zone <- intersect(nms, c("zone","zonesante","zs","zs_name","nom_zs","name","NAME","NOM"))
    cand_prov <- intersect(nms, c("province","prov","nom_prov","adm1name","ADM1NAME","NAME_1","name_1"))
    updateSelectInput(session, "zones_name_col", choices = nms, selected = ifelse(length(cand_zone), cand_zone[1], nms[1]))
    updateSelectInput(session, "prov_name_col",  choices = nms, selected = ifelse(length(cand_prov), cand_prov[1], nms[1]))
  })

  zone_summary <- reactive({
    df <- filtered(); if (is.null(df) || !nrow(df)) return(NULL)
    df |>
      group_by(zone, province) |>
      summarise(
        n = n(),
        n_f = sum(sex_clean == "F", na.rm = TRUE),
        pct_f = ifelse(n > 0, round(100 * n_f / n, 1), NA_real_),
        med_age = suppressWarnings(stats::median(age_num, na.rm = TRUE)),
        .groups = "drop"
      )
  })

  prov_outline_color <- function(prov_norm){
    ifelse(prov_norm %in% c("kasai oriental","kasaï oriental"), "#F1C40F",
           ifelse(prov_norm == "lomami", "#E74C3C", "#555555"))
  }

  output$map_zones <- renderLeaflet({
    g <- zones_sf()
    s <- zone_summary()
    validate(need(!is.null(g), "Geen gezondheidszone-kaart beschikbaar. Upload een kaart of gebruik GRID3."))
    validate(need(!is.null(s) && nrow(s) > 0, "Geen gegevens binnen de huidige filters."))

    req(input$zones_name_col, input$prov_name_col)
    g$zone_key <- normalize_names(g[[input$zones_name_col]])
    g$prov_key <- normalize_names(g[[input$prov_name_col]])
    s$zone_key <- normalize_names(s$zone)
    s$prov_key <- normalize_names(s$province)

    gj <- dplyr::left_join(g, s, by = "zone_key")

    metric <- switch(input$map_metric_zs,
                     n = gj$n,
                     pct_f = gj$pct_f,
                     med_age = gj$med_age,
                     gj$n)

    pal <- colorBin("YlGnBu", domain = metric, bins = input$map_bins_zs, na.color = "#cccccc")

    border_cols <- if (isTRUE(input$outline_by_prov)) prov_outline_color(gj$prov_key) else "#444444"

    leaflet(gj) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        weight = 1.2, color = border_cols, opacity = 1,
        fillOpacity = 0.85, fillColor = ~pal(metric),
        label = ~lapply(
          sprintf(
            "<b>Zone:</b> %s<br/><b>Provincie:</b> %s<br/><b>N:</b> %s<br/><b>%% vrouw:</b> %s<br/><b>Mediaan leeftijd:</b> %s",
            ifelse(is.na(g[[input$zones_name_col]]), "?", g[[input$zones_name_col]]),
            ifelse(is.na(g[[input$prov_name_col]]),  "?", g[[input$prov_name_col]]),
            ifelse(is.na(n), 0, n),
            ifelse(is.na(pct_f), "-", paste0(pct_f, "%")),
            ifelse(is.na(med_age), "-", round(med_age, 1))
          ),
          htmltools::HTML
        ),
        highlightOptions = highlightOptions(weight = 2, color = "#000000", bringToFront = TRUE)
      ) |>
      addLegend(pal = pal, values = metric, title = input$map_metric_zs, opacity = 0.9)
  })

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
}

shinyApp(ui, server)
