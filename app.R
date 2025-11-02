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

# -------- helpers ------------------------------------------------------------

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

load_biobank <- function(path = NULL, original_name = NULL){
  candidate_paths <- first_existing(c(path, Sys.getenv("BIOBANK_FILE")))
  candidate_paths <- c(candidate_paths,
                       first_existing(list.files("data", pattern = "(?i)biobank.*\\.(xlsx|xls|csv|rds)$", full.names = TRUE)))
  candidate_paths <- candidate_paths[nzchar(candidate_paths)]

  loader <- function(file_path){
    ext <- tolower(tools::file_ext(file_path))
    df <- if (ext %in% c("xlsx", "xls")) {
      readxl::read_excel(file_path, .name_repair = repair_column_names)
    } else if (ext == "csv") {
      utils::read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
    } else if (ext %in% c("rds", "rda")) {
      readRDS(file_path)
    } else {
      stop(sprintf("Unsupported biobank format: %s", basename(file_path)))
    }

    if (is.data.frame(df)) {
      names(df) <- repair_column_names(names(df))
    }

    df
  }

  chosen <- NULL
  if (length(candidate_paths)) {
    for (fp in candidate_paths) {
      res <- try(loader(fp), silent = TRUE)
      if (!inherits(res, "try-error")) {
        chosen <- list(path = fp, data = res)
        break
      }
    }
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

# -------- Server -------------------------------------------------------------

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

  drc_box <- list(xmin = 12, ymin = -13, xmax = 31, ymax = 6)
  bbox <- try(as.list(sf::st_bbox(hz$zones)), silent = TRUE)
  if (inherits(bbox, "try-error") || any(!is.finite(unlist(bbox)))) bbox <- drc_box

  output$map <- renderLeaflet({
    totals <- totals_by_province()
    zones <- left_join(hz$zones, totals, by = "prov_key")
    provinces <- left_join(hz$provinces, totals, by = "prov_key")

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

    leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Carto Positron") %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Esri Gray") %>%
      fitBounds(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax) %>%
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
  })
}

# -------- run ---------------------------------------------------------------

shinyApp(ui, server)
