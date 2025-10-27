////# app.R — MBUJI-MAYI BIOBANK AGE–SEX (Shiny)
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
      layout_column_wrap(
        width = 1/3,
        card(body = div(h5("Records"), textOutput("n_all"))),
        card(body = div(h5("Met date_prelev"), textOutput("n_prelev"))),
        card(body = div(h5("Unieke Barcodes"), textOutput("n_barcode")))
      ),
      tabsetPanel(
        tabPanel("Zone-piramides", plotOutput("p_zone", height = 650)),
        tabPanel("Plus-grafiek", plotOutput("p_plus", height = 650)),
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
        transp_terrain = as.numeric(date_env_cpltha - date_prelev),
        transp_cpltha  = as.numeric(date_rec_cpltha - date_env_cpltha),
        transp_inrb    = as.numeric(date_env_inrb   - date_rec_cpltha)
      ) |>
      distinct(Barcode, LabID, .keep_all = TRUE) |>
      clean_age_sex()
    if (!nrow(df)) return(NULL)
    df
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

  filtered <- reactive({
    df <- biobank(); if (is.null(df)) return(df)
    rng <- input$daterng
    if (!is.null(rng)) df <- df |> filter(!is.na(date_prelev), date_prelev >= rng[1], date_prelev <= rng[2])
    if (!is.null(input$province) && length(input$province)) df <- df |> filter(is.na(province) | province %in% input$province)
    if (!is.null(input$zone) && length(input$zone))       df <- df |> filter(is.na(zone) | zone %in% input$zone)
    if (!is.null(input$study) && input$study != "Alle")  df <- df |> filter(toupper(study) == toupper(input$study))
    df
  })

  output$n_all     <- renderText({ df <- biobank();  if (is.null(df)) "0" else format(nrow(df), big.mark = " ") })
  output$n_prelev  <- renderText({ df <- biobank();  if (is.null(df)) "0" else format(sum(!is.na(df$date_prelev)), big.mark = " ") })
  output$n_barcode <- renderText({ df <- biobank(); if (is.null(df)) "0" else format(n_distinct(df$Barcode), big.mark = " ") })

  output$p_zone <- renderPlot({
    df <- filtered(); validate(need(!is.null(df) && nrow(df) > 0, "Geen rijen binnen de selectie."))
    plot_zone_pyramids(df, bin = input$bin, min_n_per_zone = input$minN, split_study = input$split_study)
  })

  output$p_plus <- renderPlot({
    df <- filtered(); validate(need(!is.null(df) && nrow(df) > 0, "Geen rijen binnen de selectie."))
    plot_age_sex_pyramid_plus(df, facet_by = input$facet, bin = input$bin, split_study = input$split_study)
  })

  output$tbl <- renderDT({
    df <- filtered(); if (is.null(df)) df <- tibble()
    datatable(df |> select(Barcode, LabID, province, zone, study, sex_clean, age_num, date_prelev, date_env_cpltha, date_rec_cpltha, date_env_inrb, starts_with("transp_")), options = list(pageLength = 25, scrollX = TRUE))
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
