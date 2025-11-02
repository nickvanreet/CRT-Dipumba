# app.R — MBUJI-MAYI BIOBANK AGE–SEX (Shiny) - FIXED VERSION
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
  library(shinycssloaders)
  library(memoise)
})

options(shiny.maxRequestSize = 200 * 1024^2)

# -----------------------------------------------------------------------------
# 1) Helpers - FIXED
# -----------------------------------------------------------------------------

list_excel <- function(dir_biobank){
  if (!dir.exists(dir_biobank)) return(character())
  list.files(dir_biobank, pattern = "\\.xlsx?$", full.names = TRUE)
}

latest_biobank <- function(dir_biobank){
  files <- list_excel(dir_biobank)
  if (!length(files)) return(NULL)
  file_path <- files[which.max(file.info(files)$mtime)]
  df <- tryCatch(
    readxl::read_excel(file_path, .name_repair = "minimal") |> 
      janitor::clean_names(), 
    error = function(e) NULL
  )
  list(path = file_path, data = df)
}

read_specific_biobank <- function(file_path){
  if (is.null(file_path) || !file.exists(file_path)) return(NULL)
  df <- tryCatch(
    readxl::read_excel(file_path, .name_repair = "minimal") |> 
      janitor::clean_names(), 
    error = function(e) NULL
  )
  list(path = file_path, data = df)
}

# FIXED: More flexible patterns
rename_by_regex <- function(df){
  patterns <- list(
    Barcode         = "(code.*barr|barcode)",
    LabID           = "(numero|num.*ech|lab.*id)",
    date_prelev     = "date.*prelev",
    date_env_cpltha = "date.*envoi.*cpltha",
    date_rec_cpltha = "date.*(reception|recep).*cpltha",
    date_env_inrb   = "date.*envoi.*inrb",
    age_text        = "(age|annee.*naiss)",
    sex_text        = "sexe",
    zone            = "zone.*sante",
    province        = "province",
    structure_sanitaire = "structure.*sanit",
    unite_mobile    = "(unite.*mobile|mini.*unite)",
    temp_field      = "temperature.*transport",
    temp_cpltha     = "temperature.*stockage",
    study           = "etude"
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
  if (any(is_num)) {
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

# CRITICAL FIX: Study detection - exact matches FIRST, order matters!
clean_age_sex <- function(df){
  df |>
    mutate(
      age_num = readr::parse_number(age_text),
      age_num = case_when(
        is.na(age_num) ~ NA_real_,
        age_num > 1900 & !is.na(date_prelev) ~ lubridate::year(date_prelev) - age_num,
        age_num > 1900 ~ NA_real_,
        TRUE ~ age_num
      ),
      age_num = ifelse(dplyr::between(age_num, 0, 110), age_num, NA_real_),
      sex_clean = recode(toupper(trimws(sex_text)), "M" = "M", "F" = "F", .default = NA_character_),
      
      # FIXED: Check exact "DA" and "DP" FIRST before any pattern matching
      study = {
        study_chr <- trimws(as.character(study))
        study_chr <- ifelse(study_chr %in% c("", "NA", "N/A", "na"), NA_character_, study_chr)
        study_upper <- toupper(study_chr)
        
        case_when(
          is.na(study_chr) ~ NA_character_,
          study_upper == "DA" ~ "DA",
          study_upper == "DP" ~ "DP",
          grepl("DIAG.*ACTIF", study_upper) ~ "DA",
          grepl("DIAG.*PASSIF", study_upper) ~ "DP",
          TRUE ~ study_chr
        )
      }
    )
}

make_age_bands <- function(df, bin = 5){
  max_age <- suppressWarnings(max(df$age_num, na.rm = TRUE))
  if (!is.finite(max_age)) {
    return(df |> mutate(age_band = factor(rep(NA_character_, nrow(df)), levels = character())))
  }
  
  upper <- max(bin, ceiling(max_age / bin) * bin)
  breaks <- seq(0, upper + bin, by = bin)
  df |> mutate(age_band = cut(age_num, breaks = breaks, right = FALSE, include.lowest = TRUE))
}

# Map helpers
read_grid3_health_zones_impl <- function(fields = c("province", "zonesante", "zs_uid")){
  normalise_fields <- function(sf_obj) {
    nms <- names(sf_obj)
    if (!"province" %in% nms && "province" %in% tolower(nms)) {
      names(sf_obj)[which(tolower(nms) == "province")[1]] <- "province"
    }
    if (!"zonesante" %in% nms && "zonesante" %in% tolower(nms)) {
      names(sf_obj)[which(tolower(nms) == "zonesante")[1]] <- "zonesante"
    }
    sf_obj
  }
  
  # Try local first
  candidates <- c(
    file.path("data", "grid3_health_zones.rds"),
    file.path("data", "grid3_health_zones.gpkg")
  )
  
  for (path in candidates) {
    if (file.exists(path)) {
      sf_obj <- try(readRDS(path), silent = TRUE)
      if (!inherits(sf_obj, "try-error") && inherits(sf_obj, "sf")) {
        return(normalise_fields(sf_obj))
      }
    }
  }
  
  stop("Place grid3_health_zones.rds in data/ folder")
}

read_grid3_health_zones <- memoise::memoise(read_grid3_health_zones_impl)

flatten_grid3_cols <- function(x){
  stopifnot(inherits(x, "sf"))
  geom_col <- attr(x, "sf_column")
  list_cols <- names(x)[vapply(x, is.list, TRUE)]
  list_cols <- setdiff(list_cols, geom_col)
  for (nm in list_cols) {
    x[[nm]] <- vapply(x[[nm]], FUN.VALUE = character(1), FUN = function(v){
      if (is.null(v) || length(v) == 0) return(NA_character_)
      paste(as.character(unlist(v, use.names = FALSE)), collapse = "; ")
    })
  }
  x
}

# -----------------------------------------------------------------------------
# 2) Plots
# -----------------------------------------------------------------------------

plot_zone_pyramids <- function(df, bin = 5, min_n_per_zone = 15, split_study = FALSE){
  need <- c("age_num","sex_clean","date_prelev","zone","province","study")
  if (!all(need %in% names(df))) {
    return(ggplot() + theme_void() + ggtitle("Kolommen ontbreken"))
  }
  
  dd <- df |>
    filter(!is.na(age_num), !is.na(sex_clean), !is.na(date_prelev)) |>
    make_age_bands(bin = bin) |>
    count(province, zone, age_band, sex_clean, study, name = "n") |>
    complete(province, zone, age_band, sex_clean, study, fill = list(n = 0))
  
  if (!nrow(dd)) return(ggplot() + theme_void() + ggtitle("Geen data"))
  
  zone_tot <- dd |>
    group_by(province, zone) |>
    summarise(N = sum(n), .groups = "drop") |>
    filter(N >= min_n_per_zone)
  
  if (!nrow(zone_tot)) return(ggplot() + theme_void() + ggtitle("Min N niet bereikt"))
  
  dd <- dd |>
    semi_join(zone_tot, by = c("province","zone")) |>
    mutate(n_signed = if_else(sex_clean == "M", -n, n))
  
  p <- ggplot(dd, aes(x = n_signed, y = age_band, fill = sex_clean)) +
    geom_col(width = 0.9, colour = "grey40") +
    geom_vline(xintercept = 0, colour = "grey30") +
    scale_x_continuous("Aantal (M links, F rechts)", labels = abs) +
    scale_y_discrete("Leeftijd") +
    scale_fill_manual(values = c(M = "#3498db", F = "#e74c3c"), name = "Geslacht") +
    theme_minimal(base_size = 11)
  
  if (split_study) {
    p + facet_grid(study + province ~ zone, scales = "free_x")
  } else {
    p + facet_grid(province ~ zone, scales = "free_x")
  }
}

# -----------------------------------------------------------------------------
# 3) UI
# -----------------------------------------------------------------------------

ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  fluidRow(
    column(3,
      card(card_header("Data"),
        textInput("root", "Biobank map", 
          value = "C:/Users/nvanreet/ITG/THA - Digital Management System - CRT Dipumba Upload - CRT Dipumba Upload/01 - Biobanque"),
        uiOutput("filepick_ui"),
        actionButton("refresh", "Laden", class = "btn-primary w-100"),
        hr(),
        textOutput("used_file")
      ),
      card(card_header("Filters"),
        dateRangeInput("daterng", "Datum", start = Sys.Date() - 180, end = Sys.Date()),
        selectInput("study", "Studie", choices = "Alle"),
        uiOutput("province_ui"),
        uiOutput("zone_ui"),
        checkboxGroupInput("sex", "Geslacht", choices = c("M","F"), selected = c("M","F"), inline = TRUE),
        sliderInput("age_rng", "Leeftijd", 0, 110, c(0, 80))
      ),
      card(card_header("Plot"),
        numericInput("bin", "Leeftijdsband", 5, min = 1),
        numericInput("minN", "Min N/zone", 15, min = 1),
        checkboxInput("split_study", "Split DA/DP"),
        downloadButton("dl_csv", "CSV", class = "w-100 mt-2")
      )
    ),
    
    column(6,
      card(card_header("Kaart"),
        withSpinner(leafletOutput("map_zones", height = "70vh"))
      )
    ),
    
    column(3,
      card(card_header("Overzicht"),
        uiOutput("summary_cards")
      )
    )
  ),
  
  fluidRow(
    column(12,
      navset_card_pill(
        nav_panel("Piramides", withSpinner(plotOutput("p_zone", height = 600))),
        nav_panel("Data", withSpinner(DTOutput("tbl"))),
        nav_panel("Debug",
          h5("Kolommen raw"), verbatimTextOutput("cols_raw"),
          h5("Kolommen mapped"), verbatimTextOutput("cols_map"),
          h5("Study waarden"), verbatimTextOutput("study_vals")
        )
      )
    )
  )
)

# -----------------------------------------------------------------------------
# 4) Server
# -----------------------------------------------------------------------------

server <- function(input, output, session){
  
  normalize_names <- function(x){
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- stringi::stri_trans_general(x, "Latin-ASCII")
    x <- tolower(trimws(x))
    gsub("[\\s_-]+", " ", x)
  }
  
  used_file <- reactiveVal("")
  chosen_file <- reactiveVal(NULL)
  biobank_root <- reactiveVal(NULL)
  
  output$filepick_ui <- renderUI({
    files <- list_excel(input$root)
    if (!length(files)) return(helpText("Geen bestanden"))
    selectInput("filepick", "Kies", choices = setNames(files, basename(files)))
  })
  
  observeEvent(input$filepick, { chosen_file(input$filepick) })
  
  observeEvent(input$refresh, {
    if (!dir.exists(input$root)) {
      showNotification("Map niet gevonden", type = "error")
      return()
    }
    biobank_root(normalizePath(input$root, winslash = "/", mustWork = FALSE))
  }, ignoreInit = FALSE)
  
  raw_data <- reactive({
    if (!is.null(chosen_file())) {
      res <- read_specific_biobank(chosen_file())
    } else if (!is.null(biobank_root())) {
      res <- latest_biobank(biobank_root())
    } else {
      return(NULL)
    }
    
    if (is.null(res) || is.null(res$data)) {
      used_file("")
      return(NULL)
    }
    
    used_file(basename(res$path))
    res$data |> mutate(across(everything(), as.character))
  })
  
  output$used_file <- renderText({ 
    f <- used_file()
    if (nzchar(f)) paste("Bestand:", f) else "Geen bestand geladen"
  })
  
  mapped_data <- reactive({
    df0 <- raw_data()
    if (is.null(df0)) return(NULL)
    rename_by_regex(df0)
  })
  
  biobank <- reactive({
    dfm <- mapped_data()
    if (is.null(dfm)) return(NULL)
    
    df <- dfm |>
      mutate(
        date_prelev = parse_any_date(date_prelev),
        Barcode = as.character(Barcode),
        LabID = as.character(LabID)
      ) |>
      distinct(Barcode, LabID, .keep_all = TRUE) |>
      clean_age_sex() |>
      mutate(
        prov_key = normalize_names(province),
        zone_key = normalize_names(zone)
      )
    
    if (!nrow(df)) return(NULL)
    df
  })
  
  observe({
    df <- biobank()
    if (is.null(df)) return()
    
    rng <- range(df$date_prelev, na.rm = TRUE)
    if (all(is.finite(rng))) {
      updateDateRangeInput(session, "daterng", 
        start = max(rng[1], rng[2] - 180), end = rng[2], min = rng[1], max = rng[2])
    }
    
    studies <- sort(unique(na.omit(df$study)))
    updateSelectInput(session, "study", choices = c("Alle", studies), selected = "Alle")
  })
  
  output$province_ui <- renderUI({
    df <- biobank()
    if (is.null(df)) return(NULL)
    provs <- sort(unique(na.omit(df$province)))
    selectizeInput("province", "Provincie", choices = provs, selected = provs, multiple = TRUE)
  })
  
  output$zone_ui <- renderUI({
    df <- biobank()
    if (is.null(df)) return(NULL)
    zones <- sort(unique(na.omit(df$zone)))
    selectizeInput("zone", "Zone", choices = zones, selected = zones, multiple = TRUE)
  })
  
  filtered <- reactive({
    df <- biobank()
    if (is.null(df)) return(NULL)
    
    rng <- input$daterng
    if (!is.null(rng)) {
      df <- df |> filter(is.na(date_prelev) | (date_prelev >= rng[1] & date_prelev <= rng[2]))
    }
    
    if (!is.null(input$province) && length(input$province)) {
      df <- df |> filter(is.na(province) | province %in% input$province)
    }
    
    if (!is.null(input$zone) && length(input$zone)) {
      df <- df |> filter(is.na(zone) | zone %in% input$zone)
    }
    
    if (!is.null(input$study) && input$study != "Alle") {
      df <- df |> filter(toupper(study) == toupper(input$study))
    }
    
    if (!is.null(input$sex)) {
      df <- df |> filter(is.na(sex_clean) | sex_clean %in% input$sex)
    }
    
    if (!is.null(input$age_rng) && length(input$age_rng) == 2) {
      df <- df |> filter(is.na(age_num) | dplyr::between(age_num, input$age_rng[1], input$age_rng[2]))
    }
    
    df
  })
  
  output$summary_cards <- renderUI({
    df <- filtered()
    if (is.null(df) || !nrow(df)) return(div(class = "alert alert-warning", "Geen data"))
    
    n_all <- nrow(df)
    n_da <- sum(toupper(df$study) == "DA", na.rm = TRUE)
    n_dp <- sum(toupper(df$study) == "DP", na.rm = TRUE)
    
    tagList(
      div(strong("Totaal: "), scales::comma(n_all)),
      div(strong("DA: "), scales::comma(n_da), sprintf(" (%.1f%%)", 100*n_da/n_all)),
      div(strong("DP: "), scales::comma(n_dp), sprintf(" (%.1f%%)", 100*n_dp/n_all))
    )
  })
  
  zone_summary <- reactive({
    df <- filtered()
    if (is.null(df) || !nrow(df)) return(NULL)
    
    df |>
      group_by(zone, province, zone_key, prov_key) |>
      summarise(
        n = n(),
        n_da = sum(toupper(study) == "DA", na.rm = TRUE),
        n_dp = sum(toupper(study) == "DP", na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  zones_sf <- reactive({
    sf <- try(read_grid3_health_zones(), silent = TRUE)
    if (inherits(sf, "try-error")) return(NULL)
    flatten_grid3_cols(sf)
  })
  
  output$map_zones <- renderLeaflet({
    g <- zones_sf()
    s <- zone_summary()
    
    if (is.null(g)) {
      return(leaflet() |> 
        addTiles() |> 
        addControl("Plaats grid3_health_zones.rds in data/ folder", position = "topright"))
    }
    
    if (is.null(s) || !nrow(s)) {
      return(leaflet(g) |> addTiles() |> addPolygons(fillOpacity = 0.1))
    }
    
    g$zone_key <- normalize_names(g$zonesante)
    g$prov_key <- normalize_names(g$province)
    
    gj <- left_join(g, s, by = c("zone_key", "prov_key"))
    
    pal <- colorBin("YlGnBu", domain = gj$n, bins = 5, na.color = "#cccccc")
    
    leaflet(gj) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        fillColor = ~pal(n), fillOpacity = 0.7,
        color = "#444", weight = 1,
        label = ~sprintf("%s: %s DA, %s DP", 
          zonesante, 
          ifelse(is.na(n_da), 0, n_da),
          ifelse(is.na(n_dp), 0, n_dp)
        )
      ) |>
      addLegend(pal = pal, values = ~n, title = "N stalen")
  })
  
  output$p_zone <- renderPlot({
    df <- filtered()
    req(!is.null(df), nrow(df) > 0)
    plot_zone_pyramids(df, bin = input$bin, min_n_per_zone = input$minN, 
                       split_study = input$split_study)
  })
  
  output$tbl <- renderDT({
    df <- filtered()
    if (is.null(df)) df <- tibble()
    datatable(df |> select(Barcode, LabID, province, zone, study, sex_clean, age_num, date_prelev),
              options = list(pageLength = 25, scrollX = TRUE))
  })
  
  output$cols_raw <- renderPrint({
    df0 <- raw_data()
    if (is.null(df0)) return("Geen data")
    names(df0)
  })
  
  output$cols_map <- renderPrint({
    dfm <- mapped_data()
    if (is.null(dfm)) return("Geen data")
    names(dfm)
  })
  
  output$study_vals <- renderPrint({
    df <- biobank()
    if (is.null(df)) return("Geen data")
    list(
      unique_studies = sort(unique(df$study)),
      count_DA = sum(toupper(df$study) == "DA", na.rm = TRUE),
      count_DP = sum(toupper(df$study) == "DP", na.rm = TRUE),
      sample_values = head(df$study, 20)
    )
  })
  
  output$dl_csv <- downloadHandler(
    filename = function() paste0("biobank_", Sys.Date(), ".csv"),
    content = function(file) readr::write_csv(filtered(), file)
  )
}

shinyApp(ui, server)
