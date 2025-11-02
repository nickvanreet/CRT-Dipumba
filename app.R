# MBUJI-MAYI BIOBANK DASHBOARD
# Clean, modern biobank management dashboard
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(tidyverse)
  library(readxl)
  library(janitor)
  library(lubridate)
  library(DT)
  library(sf)
  library(leaflet)
  library(stringi)
  library(bsicons)
})

# === CORE FUNCTIONS ===

parse_any_date <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "NA", "N/A", "na", "NaN", "NULL")] <- NA

  out <- as.Date(rep(NA_integer_, length(x_chr)), origin = "1970-01-01")

  is_num <- grepl("^\\d+$", x_chr)
  if (any(is_num, na.rm = TRUE)) {
    suppressWarnings({
      out[is_num] <- as.Date(as.numeric(x_chr[is_num]), origin = "1899-12-30")
    })
  }

  idx <- !is_num & !is.na(x_chr)
  if (any(idx)) {
    tmp <- sub("[ T].*$", "", x_chr[idx])
    tmp <- gsub("[.\\-]", "/", tmp)

    p <- suppressWarnings(lubridate::dmy(tmp))
    miss <- is.na(p)
    if (any(miss)) p[miss] <- suppressWarnings(lubridate::ymd(tmp[miss]))
    miss <- is.na(p)
    if (any(miss)) p[miss] <- suppressWarnings(lubridate::mdy(tmp[miss]))

    out[idx] <- p
  }

  out
}

read_biobank_file <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch({
    readxl::read_excel(path, .name_repair = "minimal") |>
      janitor::clean_names() |>
      mutate(across(everything(), as.character))
  }, error = function(e) NULL)
}

rename_first_match <- function(df, new, pattern, ignore.case = TRUE){
  hits <- grep(pattern, names(df), ignore.case = ignore.case, value = TRUE)
  if (length(hits) >= 1) {
    dplyr::rename(df, !!new := dplyr::all_of(hits[1]))
  } else {
    df
  }
}

clean_biobank_data <- function(df) {
  # --- map likely headers -> standard names ---
  df <- df |>
    rename_first_match("barcode",   "code.*barr|barcode|code.*bar") |>
    rename_first_match("lab_id",    "num[eé]ro|id.*lab|sample.*id") |>
    rename_first_match("date_raw",  "date.*pr[eé]lev|date.*sample|^date$") |>
    rename_first_match("age",       "^age(\\b|_)|age.*(ans|years)?$") |>
    rename_first_match("sex",       "^sex|^sexe|^gender$") |>
    rename_first_match("zone",      "zone.*sant[eé]|health.*zone|^zs$") |>
    rename_first_match("province",  "^province") |>
    rename_first_match("study",     "etude|study|passif|actif|\\bdp\\b|\\bda\\b") |>
    rename_first_match("structure", "structure.*sanit|facility|centre") |>
    rename_first_match("unit",      "unit[eé].*mobile|mobile.*unit") |>
    rename_first_match("date_received_raw", "date.*recept|date.*arriv|recept.*lab|arriv[eé]e") |>
    rename_first_match("date_result_raw",   "date.*result|result.*date|date.*sortie") |>
    rename_first_match("doorlooptijd_raw",  "doorloop|turnaround|t\\.?a\\.?t") |>
    rename_first_match("drs_extract_raw",   "drs.*extract|extract.*drs|date.*drs")
  
  # --- ensure expected columns exist ---
  needed <- c("barcode","lab_id","date_raw","age","sex","zone","province","study",
              "structure","unit","date_received_raw","date_result_raw",
              "doorlooptijd_raw","drs_extract_raw")
  for (nm in needed) if (!nm %in% names(df)) df[[nm]] <- NA_character_
  
  # --- standardize values ---
  df <- df |>
    mutate(
      date_sample = parse_any_date(date_raw),
      date_received = parse_any_date(date_received_raw),
      date_result   = parse_any_date(date_result_raw),
      date_drs_extract = parse_any_date(drs_extract_raw),

      age_num = suppressWarnings(as.numeric(age)),
      age_num = dplyr::case_when(
        is.na(age_num) ~ NA_real_,
        age_num > 1900 ~ lubridate::year(Sys.Date()) - age_num,  # birth year
        dplyr::between(age_num, 0, 110) ~ age_num,
        TRUE ~ NA_real_
      ),

      doorlooptijd_num = suppressWarnings(as.numeric(doorlooptijd_raw)),
      doorlooptijd_days = dplyr::case_when(
        !is.na(doorlooptijd_num) ~ doorlooptijd_num,
        !is.na(date_result) & !is.na(date_sample) ~ as.numeric(difftime(date_result, date_sample, units = "days")),
        !is.na(date_result) & !is.na(date_received) ~ as.numeric(difftime(date_result, date_received, units = "days")),
        TRUE ~ NA_real_
      ),
      doorlooptijd_days = ifelse(is.finite(doorlooptijd_days), doorlooptijd_days, NA_real_),

      sex = toupper(trimws(sex)),
      sex = dplyr::case_when(
        sex %in% c("M","MALE","H") ~ "M",
        sex %in% c("F","FEMALE") ~ "F",
        TRUE ~ NA_character_
      ),
      
      study_raw = trimws(as.character(study)),
      study = dplyr::case_when(
        toupper(study_raw) == "DA" ~ "DA",
        toupper(study_raw) == "DP" ~ "DP",
        grepl("ACTIF",   toupper(study_raw)) ~ "DA",
        grepl("PASSIF",  toupper(study_raw)) ~ "DP",
        TRUE ~ NA_character_
      ),
      
      zone      = stringr::str_squish(as.character(zone)),
      province  = stringr::str_squish(as.character(province)),
      structure = stringr::str_squish(as.character(structure)),
      unit      = stringr::str_squish(as.character(unit))
    )

  df <- df |> select(-doorlooptijd_num, dplyr::everything())
  
  # --- if no parseable dates, return 0 rows (avoid crashes) ---
  if (all(is.na(df$date_sample))) {
    df <- dplyr::slice_head(df, n = 0)
  }
  
  dplyr::distinct(df, barcode, lab_id, .keep_all = TRUE)
}

make_age_groups <- function(age, width = 5) {
  if (all(is.na(age))) return(factor(levels = character()))
  hi <- suppressWarnings(ceiling(max(age, na.rm = TRUE) / width) * width)
  if (!is.finite(hi) || hi <= 0) hi <- width
  cut(age, breaks = seq(0, hi, by = width), right = FALSE, include.lowest = TRUE)
}

read_extractions_dir <- function(dir_extraction) {
  if (!dir.exists(dir_extraction)) return(tibble())

  files <- list.files(dir_extraction, pattern = "\\.xlsx?$", full.names = TRUE)
  if (!length(files)) return(tibble())

  read_one <- function(f) {
    sneak <- suppressMessages(readxl::read_excel(f, n_max = 1))
    col_types <- rep("text", ncol(sneak))

    suppressMessages(
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
  }

  purrr::map_dfr(files, read_one) |>
    mutate(
      date_prelev = if ("date_de_prelevement_jj_mm_aaaa" %in% names(.))
        parse_any_date(date_de_prelevement_jj_mm_aaaa) else as.Date(NA),
      date_env_cpltha = if ("date_envoi_vers_cpltha_jj_mm_aaaa" %in% names(.))
        parse_any_date(date_envoi_vers_cpltha_jj_mm_aaaa) else as.Date(NA),
      date_rec_cpltha = if ("date_de_reception_cpltha_jj_mm_aaaa" %in% names(.))
        parse_any_date(date_de_reception_cpltha_jj_mm_aaaa) else as.Date(NA),
      date_env_inrb = if ("date_denvoi_inrb" %in% names(.))
        parse_any_date(date_denvoi_inrb) else as.Date(NA),
      volume_raw = if ("volume_total_echantillon_sang_drs_ml" %in% names(.))
        volume_total_echantillon_sang_drs_ml else NA_character_,
      volume_ml = suppressWarnings(as.numeric(volume_raw)),
      volume_ml = ifelse(!is.na(volume_ml) & volume_ml > 10, volume_ml / 10, volume_ml)
    ) |>
    {
      if ("code_barres_kps" %in% names(.)) {
        dplyr::filter(., !(is.na(code_barres_kps) | code_barres_kps == ""))
      } else {
        .
      }
    }
}

join_extractions_biobank <- function(extractions, biobank) {
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
          barcode, lab_id, zone, province, date_sample,
          date_received, date_result, study
        ),
      by = c("code_barres_kps" = "barcode", "numero" = "lab_id")
    )
}

flag_extractions <- function(extractions_joined) {
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

empty_extractions <- function() {
  tibble(
    code_barres_kps = character(),
    numero = character(),
    volume_ml = numeric(),
    file_date = as.Date(character()),
    source_file = character()
  )
}

# === UI ===

ui <- page_navbar(
  title = "Mbuji-Mayi Biobank",
  theme = bs_theme(
    version = 5,
    preset = "bootstrap",
    primary = "#2C3E50",
    success = "#27AE60",
    info = "#3498DB",
    warning = "#F39C12",
    danger = "#E74C3C"
  ),
  
  sidebar = sidebar(
    width = 280,
    
    # Data source
    h5("Data Source"),
    textInput("data_dir", "Directory", 
              value = "C:/Users/nvanreet/ITG/THA - Digital Management System - CRT Dipumba Upload - CRT Dipumba Upload/01 - Biobanque"),
    uiOutput("file_selector"),
    actionButton("load_data", "Load Data", class = "btn-primary w-100 mb-3"),
    textOutput("data_status"),
    
    hr(),
    
    # Filters
    h5("Filters"),
    dateRangeInput("date_range", "Sample Date", 
                   start = Sys.Date() - 180, end = Sys.Date()),
    selectInput("filter_study", "Study", choices = c("All" = "all")),
    selectInput("filter_province", "Province", choices = c("All" = "all")),
    selectInput("filter_zone", "Zone", choices = c("All" = "all")),
    checkboxGroupInput("filter_sex", "Sex", 
                       choices = c("M", "F"), selected = c("M", "F"), inline = TRUE)
  ),
  
  # === OVERVIEW TAB ===
  nav_panel(
    title = "Overview",
    
    layout_columns(
      fill = FALSE,
      col_widths = c(3, 3, 3, 3),

      value_box(
        title = "Total Samples",
        value = textOutput("vb_total"),
        showcase = bs_icon("clipboard-data"),
        theme = "primary"
      ),
      
      value_box(
        title = "DA Samples",
        value = textOutput("vb_da"),
        showcase = bs_icon("clipboard-check"),
        theme = "info"
      ),
      
      value_box(
        title = "DP Samples", 
        value = textOutput("vb_dp"),
        showcase = bs_icon("clipboard-pulse"),
        theme = "success"
      ),
      
      value_box(
        title = "Sites Active",
        value = textOutput("vb_sites"),
        showcase = bs_icon("hospital"),
        theme = "warning"
      )
    ),

    layout_columns(
      fill = FALSE,
      col_widths = c(6, 6),

      value_box(
        title = "Median Doorlooptijd",
        value = textOutput("vb_doorlooptijd"),
        showcase = bs_icon("stopwatch"),
        theme = "secondary"
      ),

      value_box(
        title = "DRS Extractions",
        value = textOutput("vb_drs_extract"),
        showcase = bs_icon("cloud-arrow-down"),
        theme = "dark"
      )
    ),

    layout_columns(
      col_widths = c(6, 6),

      card(
        card_header("Sample Collection Over Time"),
        plotOutput("plot_timeline", height = 300)
      ),

      card(
        card_header("Study Distribution"),
        plotOutput("plot_study_dist", height = 300)
      )
    ),

    layout_columns(
      col_widths = c(6, 6),

      card(
        card_header("Doorlooptijd Distribution"),
        plotOutput("plot_doorlooptijd", height = 300)
      ),

      card(
        card_header("DRS Extraction Summary"),
        tableOutput("table_drs_extract")
      )
    ),

    layout_columns(
      col_widths = c(4, 4, 4),

      card(
        card_header("Demographics Summary"),
        tableOutput("table_demographics")
      ),
      
      card(
        card_header("Geographic Coverage"),
        tableOutput("table_geography")
      ),
      
      card(
        card_header("Top Contributing Sites"),
        tableOutput("table_top_sites")
      )
    )
  ),
  
  # === DEMOGRAPHICS TAB ===
  nav_panel(
    title = "Demographics",
    
    layout_columns(
      col_widths = c(12),
      
      card(
        card_header("Age-Sex Pyramid by Zone"),
        card_body(
          fluidRow(
            column(3,
                   numericInput("pyramid_age_width", "Age group width", 5, min = 1, max = 10),
                   numericInput("pyramid_min_n", "Min samples per zone", 15, min = 1),
                   checkboxInput("pyramid_split_study", "Split by study (DA/DP)", FALSE)
            ),
            column(9,
                   plotOutput("plot_pyramid", height = 600)
            )
          )
        )
      )
    )
  ),
  
  # === GEOGRAPHY TAB ===
  nav_panel(
    title = "Geography",

    layout_columns(
      col_widths = c(4, 8),
      
      card(
        card_header("Map Settings"),
        checkboxInput("map_use_online", "Load GRID3 online", FALSE),
        fileInput("map_upload", "Or upload shapefile", 
                  accept = c(".geojson", ".json", ".gpkg", ".shp", ".zip")),
        selectInput("map_zone_col", "Zone column", choices = NULL),
        selectInput("map_prov_col", "Province column", choices = NULL),
        selectInput("map_metric", "Color by",
                    choices = c("Sample count" = "n",
                                "DA samples" = "n_da", 
                                "DP samples" = "n_dp",
                                "% Female" = "pct_female"),
                    selected = "n"),
        actionButton("map_focus_kasai", "Focus Kasaï Region", class = "btn-sm w-100")
      ),
      
      card(
        card_header("Health Zones Map"),
        leafletOutput("map_main", height = 700)
      )
    )
  ),

  # === EXTRACTION QC TAB ===
  nav_panel(
    title = "Extraction QC",

    layout_columns(
      col_widths = c(4, 8),

      card(
        card_header("Settings"),
        textInput(
          "qc_dir", "Extractions directory",
          value = "C:/Users/nvanreet/ITG/THA - Digital Management System - CRT Dipumba Upload - CRT Dipumba Upload/02 - Extractions"
        ),
        actionButton("qc_refresh", "Load extractions", class = "btn-primary w-100"),
        div(class = "mt-2", textOutput("qc_status")),
        hr(),
        sliderInput(
          "qc_rng", "Acceptable volume range (mL)",
          min = 0.5, max = 3.5, value = c(1.5, 2.5), step = 0.1
        ),
        selectInput("qc_prov", "Province", choices = "All", selected = "All"),
        selectInput("qc_zone", "Zone", choices = "All", selected = "All"),
        dateRangeInput(
          "qc_date_rng", "Sample date filter",
          start = NULL, end = NULL, format = "yyyy-mm-dd", weekstart = 1
        ),
        radioButtons(
          "qc_agg", "Aggregation",
          choices = c("Day" = "day", "Month" = "month", "Year" = "year"),
          selected = "day", inline = TRUE
        ),
        checkboxInput("qc_show_out", "Show out-of-range only", value = FALSE),
        hr(),
        downloadButton("qc_dl_zone", "Download Zone Summary", class = "btn-sm w-100 mb-2"),
        downloadButton("qc_dl_out", "Download Outliers", class = "btn-sm w-100 mb-2"),
        downloadButton("qc_dl_dup", "Download Duplicates", class = "btn-sm w-100 mb-2"),
        downloadButton("qc_dl_filt", "Download Filtered Data", class = "btn-sm w-100 mb-2"),
        downloadButton("qc_dl_agg", "Download Aggregated Data", class = "btn-sm w-100")
      ),

      card(
        card_header("Extraction Insights"),
        navset_tab(
          nav_panel(
            "Overview",
            plotOutput("qc_hist_vol", height = 320),
            plotOutput("qc_box_vol", height = 260)
          ),
          nav_panel(
            "By Zone",
            plotOutput("qc_trend_filedate", height = 320),
            DTOutput("qc_zone_tbl")
          ),
          nav_panel(
            "Time Series",
            plotOutput("qc_agg_count_plot", height = 280),
            plotOutput("qc_agg_volume_plot", height = 280),
            DTOutput("qc_agg_tbl")
          ),
          nav_panel("Per sample", DTOutput("qc_per_sample_tbl")),
          nav_panel("Outliers", DTOutput("qc_out_tbl")),
          nav_panel("Duplicates", DTOutput("qc_dup_tbl"))
        )
      )
    )
  ),

  # === DATA TAB ===
  nav_panel(
    title = "Data",
    
    card(
      card_header(
        "Sample Records",
        class = "d-flex justify-content-between align-items-center",
        downloadButton("download_data", "Download CSV", class = "btn-sm")
      ),
      DTOutput("data_table")
    )
  ),
  
  # === DEBUG TAB ===
  nav_panel(
    title = "Debug",
    
    layout_columns(
      col_widths = c(6, 6),
      
      card(
        card_header("Raw Columns"),
        verbatimTextOutput("debug_raw_cols")
      ),
      
      card(
        card_header("Cleaned Columns"),
        verbatimTextOutput("debug_clean_cols")
      )
    ),
    
    card(
      card_header("Study Values Check"),
      verbatimTextOutput("debug_study")
    )
  )
)

# === SERVER ===

server <- function(input, output, session) {
  
  # === DATA LOADING ===
  
  raw_data <- reactiveVal(NULL)
  clean_data <- reactiveVal(NULL)

  extraction_status <- reactiveVal("No extraction data loaded.")
  extraction_raw <- reactiveVal(empty_extractions())
  
  # File selector
  output$file_selector <- renderUI({
    files <- list.files(input$data_dir, pattern = "\\.xlsx?$", full.names = TRUE)
    if (!length(files)) return(helpText("No Excel files found"))
    
    names(files) <- basename(files)
    selectInput("selected_file", "Select file", choices = files)
  })
  
  # Load data
  observeEvent(input$load_data, {
    req(input$selected_file)
    
    showNotification("Loading data...", duration = 2, type = "message")
    
    df_raw <- read_biobank_file(input$selected_file)
    if (is.null(df_raw)) {
      showNotification("Failed to load file", type = "error")
      return()
    }
    
    raw_data(df_raw)
    
    df_clean <- tryCatch(clean_biobank_data(df_raw), error = function(e) {
      showNotification(paste("Clean failed:", e$message), type = "error")
      return(tibble())
    })
    
    clean_data(df_clean)
    
    # If cleaning resulted in 0 rows, avoid crashing controls
    if (nrow(df_clean) == 0) {
      updateSelectInput(session, "filter_study", choices = c("All" = "all"))
      updateSelectInput(session, "filter_province", choices = c("All" = "all"))
      updateSelectInput(session, "filter_zone", choices = c("All" = "all"))
      updateDateRangeInput(session, "date_range", start = Sys.Date()-180, end = Sys.Date())
      showNotification("Loaded file, but 0 usable rows (no parseable dates).", type = "warning")
      return()
    }
    
    # Update filter choices
    studies   <- sort(unique(na.omit(df_clean$study)))
    provinces <- sort(unique(na.omit(df_clean$province)))
    zones     <- sort(unique(na.omit(df_clean$zone)))
    
    updateSelectInput(session, "filter_study",   choices = c("All"="all", studies))
    updateSelectInput(session, "filter_province",choices = c("All"="all", provinces))
    updateSelectInput(session, "filter_zone",    choices = c("All"="all", zones))
    
    # Update date range safely
    dr <- range(df_clean$date_sample, na.rm = TRUE)
    start <- max(dr[1], dr[2] - 180)
    updateDateRangeInput(session, "date_range",
                         start = start, end = dr[2],
                         min = dr[1],  max = dr[2])
    
    showNotification(sprintf("Loaded %s samples", scales::comma(nrow(df_clean))), type = "message")
  })
  
  output$data_status <- renderText({
    df <- clean_data()
    if (is.null(df)) return("No data loaded")
    sprintf("%s samples loaded", scales::comma(nrow(df)))
  })

  observeEvent(input$qc_refresh, {
    dir <- normalizePath(input$qc_dir, winslash = "/", mustWork = FALSE)
    if (!dir.exists(dir)) {
      extraction_status("Directory not found. Check the path.")
      extraction_raw(empty_extractions())
      showNotification("Extraction directory does not exist.", type = "error")
      return()
    }

    dat <- read_extractions_dir(dir)
    if (!nrow(dat)) {
      extraction_status("No extraction files found in this directory.")
      extraction_raw(empty_extractions())
      showNotification("No extraction files found.", type = "warning")
      return()
    }

    extraction_raw(dat)
    file_count <- length(unique(na.omit(dat$source_file)))
    sample_count <- nrow(dat)
    extraction_status(sprintf("%s files · %s samples",
                              scales::comma(file_count),
                              scales::comma(sample_count)))
  })
  
  # === FILTERED DATA ===
  
  filtered_data <- reactive({
    df <- clean_data(); req(df)
    if (!nrow(df)) return(df)

    dr <- input$date_range
    if (length(dr) == 2 && all(!is.na(dr))) {
      df <- df %>% dplyr::filter(date_sample >= dr[1], date_sample <= dr[2])
    }
    
    if (!identical(input$filter_study, "all")) {
      df <- df %>% dplyr::filter(study == input$filter_study)
    }
    if (!identical(input$filter_province, "all")) {
      df <- df %>% dplyr::filter(province == input$filter_province)
    }
    if (!identical(input$filter_zone, "all")) {
      df <- df %>% dplyr::filter(zone == input$filter_zone)
    }
    if (length(input$filter_sex)) {
      df <- df %>% dplyr::filter(sex %in% input$filter_sex)
    }

    df
  })

  extractions_joined <- reactive({
    join_extractions_biobank(extraction_raw(), clean_data())
  })

  extractions_flagged <- reactive({
    flag_extractions(extractions_joined())
  })

  extractions_dedup <- reactive({
    res <- extractions_flagged()
    if (is.null(res$dedup)) tibble() else res$dedup
  })

  observeEvent(extractions_dedup(), {
    df <- extractions_dedup()
    if (is.null(df) || !nrow(df) || !"province" %in% names(df)) {
      updateSelectInput(session, "qc_prov", choices = "All", selected = "All")
      updateSelectInput(session, "qc_zone", choices = "All", selected = "All")
      return()
    }

    provs <- sort(unique(na.omit(df$province)))
    sel_prov <- if (input$qc_prov %in% c("All", provs)) input$qc_prov else "All"
    updateSelectInput(session, "qc_prov", choices = c("All", provs), selected = sel_prov)

    rng <- suppressWarnings(range(df$date_prelev, na.rm = TRUE))
    if (all(is.finite(rng))) {
      updateDateRangeInput(session, "qc_date_rng",
                          start = rng[1], end = rng[2],
                          min = rng[1], max = rng[2])
    }
  }, ignoreNULL = FALSE)

  observeEvent(list(input$qc_prov, extractions_dedup()), {
    df <- extractions_dedup()
    if (is.null(df) || !nrow(df) || !"zone" %in% names(df)) {
      updateSelectInput(session, "qc_zone", choices = "All", selected = "All")
      return()
    }

    zones <- sort(unique(na.omit(df$zone)))
    if (!is.null(input$qc_prov) && input$qc_prov != "All" && "province" %in% names(df)) {
      zones <- sort(unique(na.omit(df$zone[df$province == input$qc_prov])))
    }
    sel_zone <- if (input$qc_zone %in% c("All", zones)) input$qc_zone else "All"
    updateSelectInput(session, "qc_zone", choices = c("All", zones), selected = sel_zone)
  }, ignoreNULL = FALSE)

  extractions_filtered <- reactive({
    df <- extractions_dedup()
    if (is.null(df) || !nrow(df)) return(tibble())

    df <- df |> mutate(
      volume_num = suppressWarnings(as.numeric(volume_ml)),
      file_date = as.Date(file_date)
    )

    if (!is.null(input$qc_prov) && input$qc_prov != "All" && "province" %in% names(df)) {
      df <- df |> filter(!is.na(province) & province == input$qc_prov)
    }

    if (!is.null(input$qc_zone) && input$qc_zone != "All" && "zone" %in% names(df)) {
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
    df <- extractions_filtered()
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
  
  # === VALUE BOXES ===
  
  output$vb_total <- renderText({
    df <- filtered_data(); req(df)
    scales::comma(nrow(df))
  })
  
  output$vb_da <- renderText({
    df <- filtered_data(); req(df)
    if (!nrow(df)) return("0 (0%)")
    n_da <- sum(df$study == "DA", na.rm = TRUE)
    pct  <- round(100 * n_da / nrow(df), 1)
    sprintf("%s (%s%%)", scales::comma(n_da), pct)
  })
  
  output$vb_dp <- renderText({
    df <- filtered_data(); req(df)
    if (!nrow(df)) return("0 (0%)")
    n_dp <- sum(df$study == "DP", na.rm = TRUE)
    pct  <- round(100 * n_dp / nrow(df), 1)
    sprintf("%s (%s%%)", scales::comma(n_dp), pct)
  })
  
  
  output$vb_sites <- renderText({
    df <- filtered_data(); req(df)
    n_structures <- df |> filter(!is.na(structure)) |> n_distinct(structure)
    n_units <- df |> filter(!is.na(unit)) |> n_distinct(unit)
    sprintf("%s structures, %s units", scales::comma(n_structures), scales::comma(n_units))
  })

  output$vb_doorlooptijd <- renderText({
    df <- filtered_data(); req(df)
    tat <- df$doorlooptijd_days
    tat <- tat[!is.na(tat) & tat >= 0]
    if (!length(tat)) return("No data")
    sprintf("%s days", scales::number(stats::median(tat), accuracy = 0.1))
  })

  output$vb_drs_extract <- renderText({
    dedup <- extractions_dedup()
    if (!is.null(dedup) && nrow(dedup)) {
      batches <- dplyr::n_distinct(dedup$file_date)
      return(sprintf("%s samples, %s batches",
                     scales::comma(nrow(dedup)),
                     scales::comma(batches)))
    }

    df <- filtered_data(); req(df)
    drs_dates <- df$date_drs_extract[!is.na(df$date_drs_extract)]
    if (!length(drs_dates)) return("No data")
    batches <- dplyr::n_distinct(drs_dates)
    sprintf("%s samples, %s batches",
            scales::comma(length(drs_dates)),
            scales::comma(batches))
  })
  
  # === OVERVIEW PLOTS ===
  
  output$plot_timeline <- renderPlot({
    df <- filtered_data()
    req(df)
    
    df |>
      count(week = floor_date(date_sample, "week"), study) |>
      ggplot(aes(week, n, fill = study)) +
      geom_col() +
      scale_fill_manual(values = c(DA = "#3498DB", DP = "#27AE60"),
                        na.value = "grey70") +
      labs(x = NULL, y = "Samples", fill = "Study") +
      theme_minimal() +
      theme(legend.position = "top")
  })
  
  output$plot_study_dist <- renderPlot({
    df <- filtered_data(); req(df)
    tab <- df |>
      mutate(study = ifelse(is.na(study), "Unknown", study)) |>
      count(study)
    if (!nrow(tab)) return(ggplot() + theme_void())
    tab <- tab |> mutate(pct = n / sum(n) * 100)
    ggplot(tab, aes(x = "", y = n, fill = study)) +
      geom_col(width = 1, color = "white", size = 2) +
      geom_text(aes(label = sprintf("%s\n%.1f%%", scales::comma(n), pct)),
                position = position_stack(vjust = 0.5),
                color = "white", fontface = "bold", size = 5) +
      coord_polar("y") +
      scale_fill_manual(values = c(DA="#3498DB", DP="#27AE60", Unknown="grey70")) +
      theme_void() + theme(legend.position = "right")
  })

  output$plot_doorlooptijd <- renderPlot({
    df <- filtered_data(); req(df)
    df_plot <- df |>
      filter(!is.na(doorlooptijd_days), doorlooptijd_days >= 0)

    if (!nrow(df_plot)) {
      return(
        ggplot() +
          annotate("text", x = 0, y = 0, label = "No doorlooptijd data", size = 6) +
          theme_void()
      )
    }

    df_plot <- df_plot |>
      mutate(reference_date = coalesce(date_drs_extract, date_result, date_sample)) |>
      mutate(period = floor_date(reference_date, "week")) |>
      filter(!is.na(period)) |>
      group_by(period) |>
      summarise(
        median_days = stats::median(doorlooptijd_days, na.rm = TRUE),
        n = dplyr::n(),
        .groups = "drop"
      )

    if (!nrow(df_plot)) {
      return(
        ggplot() +
          annotate("text", x = 0, y = 0, label = "No dated doorlooptijd records", size = 6) +
          theme_void()
      )
    }

    ggplot(df_plot, aes(period, median_days)) +
      geom_line(color = "#2C3E50", linewidth = 1) +
      geom_point(aes(size = n), color = "#3498DB", alpha = 0.8) +
      scale_y_continuous("Median doorlooptijd (days)") +
      scale_x_date(NULL) +
      scale_size_continuous("Samples", range = c(2, 8)) +
      theme_minimal() +
      theme(legend.position = "bottom")
  }, res = 96)

  output$table_drs_extract <- renderTable({
    dedup <- extractions_dedup()
    if (!is.null(dedup) && nrow(dedup)) {
      tab <- dedup |>
        mutate(file_date = as.Date(file_date)) |>
        count(file_date, sort = TRUE)

      if (!nrow(tab)) {
        return(tibble(Message = "No extraction batches available"))
      }

      return(
        tab |>
          mutate(
            file_date = format(file_date, "%Y-%m-%d"),
            n = scales::comma(n)
          ) |>
          rename(`Extraction batch` = file_date, Samples = n)
      )
    }

    df <- filtered_data(); req(df)
    tab <- df |>
      filter(!is.na(date_drs_extract)) |>
      mutate(date_drs_extract = as.Date(date_drs_extract)) |>
      count(date_drs_extract, sort = TRUE)

    if (!nrow(tab)) {
      return(tibble(Message = "No DRS extraction dates available"))
    }

    tab |>
      mutate(
        date_drs_extract = format(date_drs_extract, "%Y-%m-%d"),
        n = scales::comma(n)
      ) |>
      rename(`Extraction date` = date_drs_extract, Samples = n)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # === EXTRACTION QC OUTPUTS ===

  output$qc_status <- renderText({ extraction_status() })

  output$qc_hist_vol <- renderPlot({
    df <- extractions_filtered()
    validate(need(nrow(df) > 0 && "volume_num" %in% names(df), "No extraction data."))
    rng <- input$qc_rng
    ggplot(df, aes(x = volume_num,
                   fill = volume_num < rng[1] | volume_num > rng[2])) +
      geom_histogram(binwidth = 0.1, colour = "black") +
      geom_vline(xintercept = 2, linetype = "dashed", colour = "red") +
      scale_fill_manual(values = c(`FALSE` = "steelblue", `TRUE` = "tomato"), guide = "none") +
      labs(
        title = "Distribution of sample volumes",
        x = "Volume (mL)",
        y = "Count",
        subtitle = sprintf("Dashed = 2 mL | Range = %.1f–%.1f mL", rng[1], rng[2])
      ) +
      theme_minimal()
  })

  output$qc_box_vol <- renderPlot({
    df <- extractions_filtered()
    validate(need(nrow(df) > 0 && "volume_num" %in% names(df), "No extraction data."))
    rng <- input$qc_rng
    ggplot(df, aes(y = volume_num)) +
      geom_boxplot(fill = "grey90", outlier.colour = "tomato") +
      geom_jitter(height = 0, width = 0.1, alpha = 0.5) +
      geom_hline(yintercept = 2, linetype = "dashed", colour = "red") +
      geom_hline(yintercept = rng, linetype = "dotted", colour = "grey40") +
      labs(title = "Volumes with outliers", y = "Volume (mL)") +
      theme_minimal()
  })

  output$qc_trend_filedate <- renderPlot({
    df <- extractions_filtered()
    validate(need(nrow(df) > 0 && "file_date" %in% names(df) && "volume_num" %in% names(df), "No extraction data."))
    ggplot(df, aes(x = file_date, y = volume_num, colour = zone)) +
      geom_jitter(alpha = 0.4, width = 2) +
      geom_hline(yintercept = 2, linetype = "dashed", colour = "red") +
      labs(title = "Sample volumes by extraction batch", x = "file_date", y = "Volume (mL)") +
      theme_minimal()
  })

  output$qc_zone_tbl <- renderDT({
    df <- extractions_filtered()
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
    validate(need(nrow(ad) > 0, "No extraction data."))
    ggplot(ad, aes(x = period, y = N)) +
      geom_col() +
      labs(
        title = sprintf("Samples per %s", switch(input$qc_agg, day = "day", month = "month", year = "year")),
        x = "Period", y = "Samples"
      ) +
      theme_minimal()
  })

  output$qc_agg_volume_plot <- renderPlot({
    ad <- qc_agg_data()
    validate(need(nrow(ad) > 0, "No extraction data."))
    ggplot(ad, aes(x = period, y = Median)) +
      geom_line() +
      geom_point() +
      geom_ribbon(aes(ymin = P10, ymax = P90), alpha = 0.15) +
      geom_hline(yintercept = 2, linetype = "dashed") +
      labs(
        title = sprintf("Median volume per %s (P10–P90)", switch(input$qc_agg, day = "day", month = "month", year = "year")),
        x = "Period", y = "Volume (mL)"
      ) +
      theme_minimal()
  })

  output$qc_agg_tbl <- renderDT({
    ad <- qc_agg_data()
    datatable(ad, options = list(pageLength = 20, scrollX = TRUE))
  })

  output$qc_per_sample_tbl <- renderDT({
    df <- extractions_filtered()
    if (!nrow(df)) df <- tibble()
    keep <- c("province", "zone", "numero", "code_barres_kps", "date_prelev", "date_rec_cpltha",
              "date_env_cpltha", "date_env_inrb", "volume_ml", "volume_num", "file_date", "source_file", "flag")
    datatable(df |>
                select(any_of(keep)) |>
                arrange(date_prelev, zone, numero),
              options = list(pageLength = 20, scrollX = TRUE), filter = "top")
  })

  output$qc_out_tbl <- renderDT({
    df <- extractions_filtered()
    if (!nrow(df) || !"volume_num" %in% names(df)) {
      df <- tibble()
    } else {
      rng <- input$qc_rng
      df <- df |>
        filter(!is.na(volume_num) & (volume_num < rng[1] | volume_num > rng[2])) |>
        arrange(volume_num)
    }
    keep <- c("province", "zone", "numero", "code_barres_kps", "volume_ml", "volume_num", "date_prelev", "file_date", "source_file")
    datatable(df |> select(any_of(keep)), options = list(pageLength = 20, scrollX = TRUE))
  })

  output$qc_dup_tbl <- renderDT({
    flagged <- extractions_flagged()
    flagged <- flagged$flagged
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
    content = function(file) {
      df <- extractions_filtered()
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
    content = function(file) {
      df <- extractions_filtered()
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
    content = function(file) {
      flagged <- extractions_flagged()
      flagged <- flagged$flagged
      if (is.null(flagged) || !nrow(flagged)) {
        readr::write_csv(tibble(), file)
        return()
      }
      readr::write_csv(flagged |> filter(flag != "OK"), file)
    }
  )

  output$qc_dl_filt <- downloadHandler(
    filename = function() paste0("filtered_extractions_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- extractions_filtered()
      if (!nrow(df)) df <- tibble()
      readr::write_csv(df, file)
    }
  )

  output$qc_dl_agg <- downloadHandler(
    filename = function() paste0("extraction_timeseries_", input$qc_agg, "_", Sys.Date(), ".csv"),
    content = function(file) {
      readr::write_csv(qc_agg_data(), file)
    }
  )
  
  # === SUMMARY TABLES ===
  
  output$table_demographics <- renderTable({
    df <- filtered_data()
    req(df)
    
    tibble(
      Metric = c("Median age", "% Female", "Age range"),
      Value = c(
        sprintf("%.1f years", median(df$age_num, na.rm = TRUE)),
        sprintf("%.1f%%", mean(df$sex == "F", na.rm = TRUE) * 100),
        sprintf("%.0f - %.0f", 
                min(df$age_num, na.rm = TRUE),
                max(df$age_num, na.rm = TRUE))
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$table_geography <- renderTable({
    df <- filtered_data()
    req(df)
    
    tibble(
      Metric = c("Provinces", "Health zones", "Structures", "Mobile units"),
      Count = c(
        n_distinct(df$province, na.rm = TRUE),
        n_distinct(df$zone, na.rm = TRUE),
        n_distinct(df$structure, na.rm = TRUE),
        n_distinct(df$unit, na.rm = TRUE)
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$table_top_sites <- renderTable({
    df <- filtered_data()
    req(df)
    
    df |>
      filter(!is.na(structure)) |>
      count(structure, sort = TRUE) |>
      head(5) |>
      mutate(n = scales::comma(n)) |>
      rename(Structure = structure, Samples = n)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # === PYRAMID PLOT ===
  
  output$plot_pyramid <- renderPlot({
    df <- filtered_data()
    req(df, nrow(df) > 0)
    
    # Filter complete cases
    df_plot <- df |>
      filter(!is.na(age_num), !is.na(sex), !is.na(zone)) |>
      mutate(age_group = make_age_groups(age_num, input$pyramid_age_width))
    
    # Filter zones with min samples
    zones_keep <- df_plot |>
      count(province, zone) |>
      filter(n >= input$pyramid_min_n)
    
    if (nrow(zones_keep) == 0) {
      return(
        ggplot() + 
          annotate("text", x = 0, y = 0, 
                   label = "No zones meet minimum sample threshold",
                   size = 6) +
          theme_void()
      )
    }
    
    df_plot <- df_plot |>
      semi_join(zones_keep, by = c("province", "zone"))
    
    # Aggregate
    df_agg <- df_plot |>
      count(province, zone, age_group, sex, study) |>
      complete(province, zone, age_group, sex, study, fill = list(n = 0)) |>
      mutate(n_signed = if_else(sex == "M", -n, n))
    
    # Plot
    p <- ggplot(df_agg, aes(x = n_signed, y = age_group, fill = sex)) +
      geom_col(width = 0.9, color = "grey40") +
      geom_vline(xintercept = 0, color = "grey30") +
      scale_x_continuous("Count (M left, F right)", labels = abs) +
      scale_fill_manual(values = c(M = "#3498DB", F = "#E74C3C")) +
      labs(y = "Age group", fill = "Sex") +
      theme_minimal(base_size = 12)
    
    if (input$pyramid_split_study) {
      p + facet_grid(study + province ~ zone, scales = "free_x", space = "free_x")
    } else {
      p + facet_grid(province ~ zone, scales = "free_x", space = "free_x")
    }
  }, res = 96)
  
  # === MAP ===
  
  map_zones <- reactive({
    # Try to load shapefile
    if (input$map_use_online) {
      # Load from GRID3
      tryCatch({
        url <- "https://services3.arcgis.com/BU6Aadhn6tbBEdyk/arcgis/rest/services/GRID3_COD_health_zones_v7_0/FeatureServer/0/query?where=1%3D1&outFields=province,zonesante&outSR=4326&f=geojson"
        sf <- sf::read_sf(url, quiet = TRUE)
        # Flatten list columns
        for (col in names(sf)) {
          if (is.list(sf[[col]]) && col != attr(sf, "sf_column")) {
            sf[[col]] <- vapply(sf[[col]], function(x) paste(as.character(x), collapse = "; "), "")
          }
        }
        return(sf)
      }, error = function(e) NULL)
    }
    
    # Or from upload
    if (!is.null(input$map_upload)) {
      tryCatch({
        sf::read_sf(input$map_upload$datapath, quiet = TRUE)
      }, error = function(e) NULL)
    }
    
    NULL
  })
  
  observe({
    shp <- map_zones()
    if (is.null(shp)) return()
    
    cols <- setdiff(names(shp), attr(shp, "sf_column"))
    updateSelectInput(session, "map_zone_col", choices = cols,
                      selected = cols[grep("zone|zs", cols, ignore.case = TRUE)][1])
    updateSelectInput(session, "map_prov_col", choices = cols,
                      selected = cols[grep("prov", cols, ignore.case = TRUE)][1])
  })
  
  output$map_main <- renderLeaflet({
    shp <- map_zones()
    df <- filtered_data()
    
    if (is.null(shp) || is.null(df)) {
      return(leaflet() |> addTiles() |> 
               setView(lng = 23, lat = -6, zoom = 6))
    }
    
    req(input$map_zone_col, input$map_prov_col)
    
    # Normalize names for joining
    norm <- function(x) tolower(trimws(stringi::stri_trans_general(x, "Latin-ASCII")))
    
    shp$zone_norm <- norm(shp[[input$map_zone_col]])
    shp$prov_norm <- norm(shp[[input$map_prov_col]])
    
    df_summary <- df |>
      mutate(zone_norm = norm(zone), prov_norm = norm(province)) |>
      group_by(zone_norm, prov_norm) |>
      summarise(
        n = n(),
        n_da = sum(study == "DA", na.rm = TRUE),
        n_dp = sum(study == "DP", na.rm = TRUE),
        pct_female = mean(sex == "F", na.rm = TRUE) * 100,
        .groups = "drop"
      )
    
    shp_data <- shp |>
      left_join(df_summary, by = c("zone_norm", "prov_norm"))
    
    metric_col <- switch(input$map_metric,
                         n = shp_data$n,
                         n_da = shp_data$n_da,
                         n_dp = shp_data$n_dp,
                         pct_female = shp_data$pct_female)
    
    pal <- colorBin("YlGnBu", domain = metric_col, bins = 5, na.color = "#cccccc")
    
    leaflet(shp_data) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        fillColor = ~pal(metric_col),
        fillOpacity = 0.7,
        color = "#444",
        weight = 1,
        label = ~sprintf("%s: %s samples (DA: %s, DP: %s)",
                         shp_data[[input$map_zone_col]],
                         ifelse(is.na(n), 0, n),
                         ifelse(is.na(n_da), 0, n_da),
                         ifelse(is.na(n_dp), 0, n_dp)),
        highlightOptions = highlightOptions(weight = 2, color = "#000")
      ) |>
      addLegend(pal = pal, values = ~metric_col, title = input$map_metric)
  })
  
  observeEvent(input$map_focus_kasai, {
    shp <- map_zones()
    req(shp, input$map_prov_col)
    
    norm <- function(x) tolower(trimws(stringi::stri_trans_general(x, "Latin-ASCII")))
    shp$prov_norm <- norm(shp[[input$map_prov_col]])
    
    kasai <- shp |> filter(prov_norm %in% c("kasai oriental", "lomami"))
    
    if (nrow(kasai) > 0) {
      bbox <- sf::st_bbox(kasai)
      leafletProxy("map_main") |>
        fitBounds(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])
    }
  })
  
  # === DATA TABLE ===
  
  output$data_table <- renderDT({
    df <- filtered_data()
    req(df)
    
    df |>
      select(barcode, lab_id, date_sample, date_received, date_result,
             date_drs_extract, doorlooptijd_days,
             study, sex, age_num,
             province, zone, structure, unit) |>
      datatable(
        options = list(pageLength = 25, scrollX = TRUE),
        filter = "top",
        rownames = FALSE
      )
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      sprintf("biobank_export_%s.csv", format(Sys.Date(), "%Y%m%d"))
    },
    content = function(file) {
      write_csv(filtered_data(), file)
    }
  )
  
  # === DEBUG ===
  
  output$debug_raw_cols <- renderPrint({
    df <- raw_data()
    if (is.null(df)) return("No data")
    names(df)
  })
  
  output$debug_clean_cols <- renderPrint({
    df <- clean_data()
    if (is.null(df)) return("No data")
    names(df)
  })
  
  output$debug_study <- renderPrint({
    df <- clean_data()
    if (is.null(df)) return("No data")
    
    list(
      unique_values = sort(unique(df$study)),
      counts = table(df$study, useNA = "ifany"),
      sample_raw = head(unique(df$study_raw), 20)
    )
  })
}

shinyApp(ui, server)
