# ==============================================================================
# GLOBAL SPATIAL ANIMAL DISEASE RISK ANALYSIS (MCDA) - R SHINY DASHBOARD
# ==============================================================================

options(shiny.sanitize.errors = FALSE)
options(shiny.error = NULL)

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(leaflet)
  library(DT)
  library(plotly)
  library(sf)
  library(rnaturalearth)
  library(countrycode)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(classInt)
})

# --- 1. DATA INGESTION & ROBUST DATA HARMONIZATION ---
data_dir <- "D:/datathon"
if (!file.exists(file.path(data_dir, "animal_pop.csv"))) data_dir <- getwd()
proc_dir <- file.path(data_dir, "processed")
dir.create(proc_dir, showWarnings = FALSE, recursive = TRUE)

message("Loading datasets from: ", data_dir)

species_map <- c(
  "Cattle" = "Cattle", "Bovine" = "Cattle",
  "Swine" = "Swine", "Swine / pigs" = "Swine", "Pigs" = "Swine",
  "Sheep" = "Sheep", "Goats" = "Goats",
  "Birds" = "Birds", "Chickens" = "Birds", "Ducks" = "Birds", "Geese" = "Birds", "Turkeys" = "Birds", "Poultry" = "Birds",
  "Equidae" = "Equidae", "Horses" = "Equidae", "Asses" = "Equidae", "Mules and hinnies" = "Equidae",
  "Camelidae" = "Camelidae", "Camels" = "Camelidae",
  "Buffaloes" = "Buffaloes", "Buffalo" = "Buffaloes",
  "Rabbits / hares (mixed group)" = "Rabbits / hares", "Rabbits and hares" = "Rabbits / hares"
)

# 1.1 Population
pop_file <- file.path(data_dir, "animal_pop.csv")
if (!file.exists(pop_file)) stop("animal_pop.csv not found in ", data_dir)
pop_raw <- read_csv(pop_file, show_col_types = FALSE)

clean_pop <- pop_raw %>%
  filter(!is.na(`Country(ISO3)`), !is.na(`Nb live animals`)) %>%
  mutate(
    ISO3 = as.character(`Country(ISO3)`),
    Country = countrycode(ISO3, origin = "iso3c", destination = "country.name", custom_match = c("XKX" = "Kosovo")),
    RawCategory = as.character(`Animal category`),
    Species = recode(RawCategory, !!!species_map),
    Year = as.integer(Year),
    Population = as.numeric(`Nb live animals`)
  ) %>%
  filter(!is.na(Country), !is.na(Species), Population >= 0) %>%
  group_by(ISO3, Country, Species, Year) %>%
  summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop")

# 1.2 Movement Edges
edges_file <- file.path(data_dir, "edges_by_species.csv")
if (!file.exists(edges_file)) stop("edges_by_species.csv not found in ", data_dir)
edges_raw <- read_csv(edges_file, show_col_types = FALSE)
names(edges_raw) <- tolower(names(edges_raw))

clean_edges <- edges_raw %>%
  mutate(
    From_ISO = countrycode(`from`, origin = "country.name", destination = "iso3c", custom_match = c("Kosovo" = "XKX")),
    To_ISO   = countrycode(`to`, origin = "country.name", destination = "iso3c", custom_match = c("Kosovo" = "XKX")),
    From_Country = countrycode(From_ISO, origin = "iso3c", destination = "country.name"),
    To_Country   = countrycode(To_ISO, origin = "iso3c", destination = "country.name"),
    Species = recode(as.character(species), !!!species_map),
    Weight = as.numeric(weight),
    TotalHead = as.numeric(total_head),
    NYears = as.numeric(n_years)
  ) %>%
  filter(!is.na(From_ISO), !is.na(To_ISO), !is.na(Species))

# 1.3 Centrality
cent_file <- file.path(data_dir, "centrality_by_species.csv")
if (!file.exists(cent_file)) stop("centrality_by_species.csv not found in ", data_dir)
cent_raw <- read_csv(cent_file, show_col_types = FALSE)
names(cent_raw) <- tolower(names(cent_raw))

clean_cent <- cent_raw %>%
  mutate(
    ISO3 = countrycode(country, origin = "country.name", destination = "iso3c", custom_match = c("Kosovo" = "XKX")),
    Country = countrycode(ISO3, origin = "iso3c", destination = "country.name"),
    Species = recode(as.character(species), !!!species_map),
    in_degree = as.numeric(in_degree),
    out_degree = as.numeric(out_degree),
    betweenness = as.numeric(betweenness)
  ) %>%
  filter(!is.na(ISO3), !is.na(Species))

# 1.4 Disease Situation
dis_cache_file <- file.path(proc_dir, "clean_disease_agg.csv")
dis_raw_file   <- file.path(data_dir, "disease.csv")
dis_zip_file   <- file.path(data_dir, "disease.zip")

if (file.exists(dis_cache_file)) {
  clean_dis <- read_csv(dis_cache_file, show_col_types = FALSE)
} else {
  message("Aggregating disease records into clean_disease_agg.csv (cached once)...")
  dis_conn <- if (file.exists(dis_raw_file)) {
    dis_raw_file
  } else if (file.exists(dis_zip_file)) {
    unz(dis_zip_file, "disease.csv")
  } else {
    stop("Neither disease.csv nor disease.zip found in ", data_dir)
  }
  
  dis_df <- read_csv(
    dis_conn,
    col_types = cols_only(
      Year = col_integer(),
      Country = col_character(),
      Disease = col_character(),
      `Disease status` = col_character()
    )
  )
  
  clean_dis <- dis_df %>%
    mutate(
      ISO3 = countrycode(Country, origin = "country.name", destination = "iso3c", custom_match = c("Kosovo" = "XKX")),
      CountryName = countrycode(ISO3, origin = "iso3c", destination = "country.name"),
      StatusCode = case_when(
        `Disease status` == "Present" ~ 1.0,
        `Disease status` == "Suspected" ~ 0.5,
        `Disease status` == "Absent" ~ 0.0,
        TRUE ~ NA_real_
      )
    ) %>%
    filter(!is.na(ISO3), !is.na(Disease)) %>%
    group_by(ISO3, CountryName, Disease, Year) %>%
    summarise(
      DiseaseStatusMax = if (all(is.na(StatusCode))) NA_real_ else max(StatusCode, na.rm = TRUE),
      ReportCount = n(),
      .groups = "drop"
    ) %>%
    rename(Country = CountryName)
  
  write_csv(clean_dis, dis_cache_file)
}

if (!"DiseaseStatusMax" %in% names(clean_dis)) {
  clean_dis$DiseaseStatusMax <- 0.0
}

# 1.5 Spatial Polygons & Centroids
world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(iso_a3, name_long, geometry) %>%
  mutate(ISO3 = countrycode(name_long, origin = "country.name", destination = "iso3c", custom_match = c("Kosovo" = "XKX"))) %>%
  filter(!is.na(ISO3)) %>%
  st_make_valid()

suppressWarnings({
  centroids_sf <- st_point_on_surface(world_sf)
  coords_mat <- st_coordinates(centroids_sf)
  country_coords <- tibble(
    ISO3 = world_sf$ISO3,
    Lon = coords_mat[, 1],
    Lat = coords_mat[, 2]
  )
})

# Safe normalizer
safe_min_max <- function(x) {
  valid <- x[!is.na(x) & !is.infinite(x)]
  if (length(valid) == 0) return(rep(0, length(x)))
  min_v <- min(valid)
  max_v <- max(valid)
  if (abs(max_v - min_v) < .Machine$double.eps) return(rep(0.5, length(x)))
  out <- (x - min_v) / (max_v - min_v)
  out[is.na(out) | is.infinite(out)] <- 0
  return(out)
}

# Safe Fisher-Jenks Classifier
classify_risk <- function(scores) {
  cat_levels <- c("Very Low", "Low", "Moderate", "High", "Very High")
  if (length(scores) == 0 || all(is.na(scores))) {
    return(factor(rep("Unassessed", length(scores)), levels = c("Unassessed", cat_levels)))
  }
  
  valid_s <- scores[!is.na(scores) & !is.infinite(scores)]
  u_vals <- unique(valid_s)
  
  if (length(u_vals) <= 1) {
    res <- ifelse(is.na(scores), "Unassessed", "Very Low")
    return(factor(res, levels = c("Unassessed", cat_levels)))
  }
  
  brks <- NULL
  if (length(u_vals) >= 5) {
    brks <- tryCatch({
      b <- classInt::classIntervals(valid_s, n = 5, style = "jenks")$brks
      if (length(unique(b)) == 6 && all(diff(b) > 0)) b else NULL
    }, error = function(e) NULL)
  }
  
  if (is.null(brks)) {
    min_v <- min(valid_s)
    max_v <- max(valid_s)
    brks <- seq(min_v, max_v, length.out = 6)
  }
  
  brks[1] <- brks[1] - 0.0001
  brks[length(brks)] <- brks[length(brks)] + 0.0001
  
  cats <- cut(scores, breaks = brks, labels = cat_levels, include.lowest = TRUE)
  return(cats)
}

# Choices Setup
common_species <- sort(intersect(unique(clean_pop$Species), unique(clean_edges$Species)))
common_diseases <- sort(unique(clean_dis$Disease))
common_pop_years <- sort(unique(clean_pop$Year), decreasing = TRUE)
dis_years <- sort(unique(clean_dis$Year), decreasing = TRUE)

dis_period_choices <- c(
  "Full Historical Record (2005-2026)" = "all",
  "Latest Available Period (2025-2026)" = "latest",
  setNames(as.character(dis_years), paste("Specific Year:", dis_years))
)

# --- 2. USER INTERFACE (Modern Slate & Teal Aesthetic + MathJax Support) ---
ui <- page_navbar(
  title = "Global Spatial Risk Analysis of Animal Disease (MCDA)",
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#0F4C81",
    secondary = "#17A2B8",
    success = "#28A745",
    base_font = font_google("Inter"),
    heading_font = font_google("Plus Jakarta Sans")
  ),
  
  header = tags$head(
    # Inject MathJax CDN for rendering mathematical formulas properly in the Methods tab
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js?config=TeX-AMS-MML_HTMLorMML", type = "text/javascript"),
    tags$script(HTML("
      if (window.MathJax) {
        MathJax.Hub.Config({
          tex2jax: {inlineMath: [['$','$'], ['\\\\(','\\\\)']]},
          processEscapes: true
        });
      }
    "))
  ),
  
  sidebar = sidebar(
    title = "Epidemiological Controls",
    width = 340,
    selectizeInput("source_country", "Source Country of Origin (i):", choices = NULL),
    selectInput("species", "Animal Species:", choices = common_species, 
                selected = if ("Swine" %in% common_species) "Swine" else common_species[1]),
    selectInput("disease", "Target Pathogen / Disease:", choices = common_diseases, 
                selected = if ("African swine fever virus (Inf. with)" %in% common_diseases) "African swine fever virus (Inf. with)" else common_diseases[1]),
    selectInput("pop_year", "Host Population Baseline Year:", choices = common_pop_years, selected = common_pop_years[1]),
    selectInput("dis_period", "Disease Surveillance Period:", choices = dis_period_choices, selected = "all"),
    hr(),
    h6("MCDA Multi-Criteria Weights", style = "font-weight:700; color:#0F4C81;"),
    sliderInput("w_d", "Source Pathogen Pressure (w_D):", min = 0, max = 1, value = 0.50, step = 0.05),
    sliderInput("w_m", "Live Movement Pathway (w_M):", min = 0, max = 1, value = 0.50, step = 0.05),
    sliderInput("w_p", "Susceptible Host Biomass (w_P):", min = 0, max = 1, value = 0.60, step = 0.05),
    sliderInput("w_h", "Dest. Receptivity / History (w_H):", min = 0, max = 1, value = 0.40, step = 0.05),
    uiOutput("weight_validation_ui"),
    hr(),
    actionButton("run_analysis", "Run Risk Analysis", icon = icon("calculator"), 
                 class = "btn-primary w-100", style = "font-weight: 700; padding: 10px; font-size:1rem;"),
    hr(),
    downloadButton("download_csv", "Export Results Table (CSV)", class = "btn-outline-secondary w-100")
  ),
  
  nav_panel("Risk Map",
            layout_columns(
              fill = FALSE,
              value_box(title = "Assessed Destination Nations", value = textOutput("vb_count"), icon = icon("globe-americas"), theme = "primary"),
              value_box(title = "Direct Live Trade Routes", value = textOutput("vb_direct_partners"), icon = icon("ship"), theme = "info"),
              value_box(title = "Highest Destination Risk", value = textOutput("vb_top_risk"), icon = icon("triangle-exclamation"), theme = "danger")
            ),
            card(
              full_screen = TRUE,
              card_header("Global Chloropleth: Multi-Criteria Spatial Transmission Risk", style = "font-weight:700;"),
              leafletOutput("risk_map", height = "600px")
            )
  ),
  
  nav_panel("Risk Ranking",
            card(
              card_header("Destination Country Priority League Table", style = "font-weight:700;"),
              DTOutput("ranking_table")
            ),
            card(
              card_header("Top 20 Priority Destinations: Two-Stage Risk Decomposition", style = "font-weight:700;"),
              plotlyOutput("decomp_bar", height = "580px")
            )
  ),
  
  nav_panel("Movement Network",
            layout_columns(
              fill = FALSE,
              value_box(title = "Source In-Degree", value = textOutput("vb_in_deg"), icon = icon("arrow-down-left-group"), theme = "light"),
              value_box(title = "Source Out-Degree", value = textOutput("vb_out_deg"), icon = icon("arrow-up-right-from-square"), theme = "light"),
              value_box(title = "Source Betweenness Centrality", value = textOutput("vb_betweenness"), icon = icon("diagram-project"), theme = "light")
            ),
            card(
              full_screen = TRUE,
              card_header("Geographical Movement Network Map (Direct Live Animal Pathways)", style = "font-weight:700;"),
              p("Displays actual bilateral live animal export pathways originating from the source country. Line widths correspond to traded live head count, and destination markers reflect final composite risk.", 
                style = "font-size:0.9rem; color:#6c757d; margin-bottom:6px;"),
              leafletOutput("network_map", height = "620px")
            )
  ),
  
  nav_panel("Sensitivity Analysis",
            card(
              card_header("MCDA Weight Sensitivity & Ranking Stability", style = "font-weight:700;"),
              p("Assesses ranking agreement across alternative weighting models using Spearman's rank correlation (rho) and Kendall's tau relative to the baseline configuration."),
              tableOutput("sensitivity_summary"),
              hr(),
              plotlyOutput("sensitivity_scatter", height = "450px")
            )
  ),
  
  nav_panel("Methods",
            card(
              card_header("Methodological Framework & Mathematical Formulations", style = "font-weight:700;"),
              HTML("
        <div style='line-height:1.8; font-size: 1rem; color: #2C3E50;'>
          <h3 style='color:#0F4C81; font-weight:700; margin-top:10px;'>1. The Multiplicative Principle</h3>
          <p>
            Total transmission risk from source country $i$ to destination country $j$ ($\\text{Risk}_{i \\rightarrow j}$) is modeled as the product of two independent, sequential epidemiological probabilities:
          </p>
          <div style='background:#F1F5F9; border-left: 5px solid #0F4C81; padding: 14px 20px; margin: 18px 0; border-radius: 4px; font-size: 1.15rem; font-weight: bold; text-align: center;'>
            $$\\text{Risk}_{i \\rightarrow j} = \\text{Introduction Risk}_{i \\rightarrow j} \\times \\text{Establishment Risk}_j$$
          </div>
          <p>
            <b>Epidemiological Rationale:</b> This multiplicative structure enforces biological validity. If a source country has zero introduction risk 
            (pathogen is confirmed absent and no live trade movements exist along the pathway, meaning $\\text{IR}_{i \\rightarrow j} = 0$), 
            the overall transmission risk to destination $j$ mathematically evaluates to zero ($0 \\times \\text{ER}_j = 0$). 
            This prevents the model from generating false-positive risk flags for unlinked countries.
          </p>
          
          <hr style='margin: 30px 0; border-top: 1px solid #E2E8F0;'/>
          <h4 style='color:#17A2B8; font-weight:700;'>2. Stage 1: Introduction Risk ($\\text{IR}_{i \\rightarrow j}$)</h4>
          <p>Introduction Risk measures the likelihood that the pathogen is released from source country $i$ and successfully transported via live animal trade into destination country $j$.</p>
          <div style='background:#F8FAFC; border-left: 5px solid #17A2B8; padding: 12px 18px; margin: 15px 0; border-radius: 4px; font-weight: bold;'>
            $$\\text{IR}_{i \\rightarrow j} = w_D D_i + w_M M_{ij}$$
          </div>
          <ul style='padding-left: 20px;'>
            <li style='margin-bottom: 8px;'><b>Constraint:</b> $w_D + w_M = 1.0$ (User-adjustable weights between disease presence and movement volume).</li>
            <li style='margin-bottom: 8px;'><b>$D_i$ (Source Pathogen Pressure):</b> Reflects the confirmed presence or recent history of the disease in source country $i$ during the selected surveillance window. Coded on a standardized scale: <b>Present = 1.0</b>, <b>Suspected = 0.5</b>, <b>Absent/Verified Free = 0.0</b>.</li>
            <li style='margin-bottom: 8px;'><b>$M_{ij}$ (Movement Trade Intensity):</b> Quantifies bilateral live animal trade along the directed pathway $i \\rightarrow j$. It combines log-transformed live animal head counts (<code>total_head</code>) and temporal trade frequency (<code>n_years</code>) from WAHIS trade network data, normalized onto $[0, 1]$. Countries with no direct trade connection from source $i$ receive $M_{ij} = 0$.</li>
          </ul>

          <hr style='margin: 30px 0; border-top: 1px solid #E2E8F0;'/>
          <h4 style='color:#17A2B8; font-weight:700;'>3. Stage 2: Establishment Risk ($\\text{ER}_j$)</h4>
          <p>Establishment Risk measures the capacity of destination country $j$ to successfully receive, amplify, and sustain transmission of the pathogen following an introduction event.</p>
          <div style='background:#F8FAFC; border-left: 5px solid #17A2B8; padding: 12px 18px; margin: 15px 0; border-radius: 4px; font-weight: bold;'>
            $$\\text{ER}_j = w_P P_j + w_H H_j$$
          </div>
          <ul style='padding-left: 20px;'>
            <li style='margin-bottom: 8px;'><b>Constraint:</b> $w_P + w_H = 1.0$ (User-adjustable weights between host population and disease history).</li>
            <li style='margin-bottom: 8px;'><b>$P_j$ (Susceptible Host Population Biomass):</b> Min-max normalized $\\log_{10}(\\text{Nb live animals} + 1)$ for destination $j$. The logarithmic transformation compresses extreme differences between massive producer nations and smaller territories. If a census is missing for a specific target year, the model automatically rolls backward to the most recent verified WAHIS census.</li>
            <li style='margin-bottom: 8px;'><b>$H_j$ (Receptivity & Historical Endemicity):</b> Historical occurrence and reporting frequency of the target disease in destination $j$, serving as an epidemiological proxy for environmental suitability, vector competence, and local baseline receptivity.</li>
          </ul>

          <hr style='margin: 30px 0; border-top: 1px solid #E2E8F0;'/>
          <h4 style='color:#0F4C81; font-weight:700;'>4. Composite Scoring & Fisher-Jenks Risk Classification</h4>
          <p>
            Final composite scores ($\\text{Risk}_{i \\rightarrow j} \\in [0, 1]$) are grouped into five qualitative tiers using <b>Fisher-Jenks Natural Breaks</b> optimization.
          </p>
          <ul style='padding-left: 20px;'>
            <li style='margin-bottom: 8px;'><b>Classification Rationale:</b> Natural breaks algorithms iteratively identify natural groupings in epidemiological data by minimizing within-class variance and maximizing between-class divergence. This prevents the artificial skewing that occurs with equal-interval binning when dealing with zero-inflated trade and disease datasets.</li>
            <li style='margin-bottom: 8px;'><b>Tiers:</b> <i>Very Low, Low, Moderate, High, Very High</i>.</li>
          </ul>
        </div>
      ")
            )
  )
)

# --- 3. SERVER LOGIC ---
server <- function(input, output, session) {
  
  # Populate source countries safely
  observe({
    valid_sources <- sort(unique(clean_edges$From_Country))
    sel <- if ("Indonesia" %in% valid_sources) "Indonesia" else if ("Philippines" %in% valid_sources) "Philippines" else valid_sources[1]
    updateSelectizeInput(session, "source_country", choices = valid_sources, selected = sel, server = TRUE)
  })
  
  # Weight normalization labels
  output$weight_validation_ui <- renderUI({
    req(input$w_d, input$w_m, input$w_p, input$w_h)
    tot_ir <- input$w_d + input$w_m
    tot_er <- input$w_p + input$w_h
    tagList(
      div(style = "font-size:0.82rem; margin-top:4px;",
          span(class = ifelse(abs(tot_ir - 1) < 0.01, "text-success", "text-warning"),
               paste0("w_D + w_M = ", round(tot_ir, 2))), " | ",
          span(class = ifelse(abs(tot_er - 1) < 0.01, "text-success", "text-warning"),
               paste0("w_P + w_H = ", round(tot_er, 2)))
      )
    )
  })
  
  # Source Centrality Metrics
  src_cent <- reactive({
    req(input$source_country, input$species)
    clean_cent %>%
      filter(Country == input$source_country, Species == input$species)
  })
  
  output$vb_in_deg <- renderText({
    d <- src_cent()
    if (nrow(d) > 0 && !is.na(d$in_degree[1])) d$in_degree[1] else "0"
  })
  output$vb_out_deg <- renderText({
    d <- src_cent()
    if (nrow(d) > 0 && !is.na(d$out_degree[1])) d$out_degree[1] else "0"
  })
  output$vb_betweenness <- renderText({
    d <- src_cent()
    if (nrow(d) > 0 && !is.na(d$betweenness[1])) round(d$betweenness[1], 1) else "0.0"
  })
  
  # Core MCDA Engine Execution
  mcda_data <- eventReactive(input$run_analysis, {
    req(input$source_country, input$species, input$disease, input$pop_year)
    
    withProgress(message = "Executing Two-Stage Risk Pipeline...", value = 0.2, {
      
      src <- input$source_country
      sp  <- input$species
      dis <- input$disease
      pyr <- as.integer(input$pop_year)
      
      src_iso <- countrycode(src, origin = "country.name", destination = "iso3c", custom_match = c("Kosovo" = "XKX"))
      
      # 1. Weights Normalization
      wd <- input$w_d / max(0.001, input$w_d + input$w_m)
      wm <- input$w_m / max(0.001, input$w_d + input$w_m)
      wp <- input$w_p / max(0.001, input$w_p + input$w_h)
      wh <- input$w_h / max(0.001, input$w_p + input$w_h)
      
      # 2. Source Disease Pressure (D_i)
      incProgress(0.2, detail = "Evaluating Source Pathogen Pressure...")
      dis_filtered <- if (input$dis_period == "latest") {
        clean_dis %>% filter(Disease == dis, Year >= 2025)
      } else if (input$dis_period == "all") {
        clean_dis %>% filter(Disease == dis)
      } else {
        clean_dis %>% filter(Disease == dis, Year == as.integer(input$dis_period))
      }
      
      d_i_record <- dis_filtered %>% filter(ISO3 == src_iso)
      d_i <- if (nrow(d_i_record) > 0 && !is.na(d_i_record$DiseaseStatusMax[1])) {
        d_i_record$DiseaseStatusMax[1]
      } else {
        hist_rec <- clean_dis %>% filter(ISO3 == src_iso, Disease == dis)
        if (nrow(hist_rec) > 0 && any(!is.na(hist_rec$DiseaseStatusMax))) {
          max(hist_rec$DiseaseStatusMax, na.rm = TRUE)
        } else {
          0.0
        }
      }
      
      # 3. Direct Outgoing Trade Movements (M_ij)
      incProgress(0.2, detail = "Mapping live animal trade flows...")
      flow_from_src <- clean_edges %>%
        filter(From_ISO == src_iso, Species == sp)
      
      if (nrow(flow_from_src) > 0) {
        flow_from_src <- flow_from_src %>%
          mutate(
            RawFlow = log10(coalesce(TotalHead, 0) + 1) * pmax(1, coalesce(NYears, 1)),
            M_ij = safe_min_max(RawFlow)
          ) %>%
          select(To_ISO, To_Country, TotalHead, NYears, M_ij)
      } else {
        flow_from_src <- tibble(To_ISO = character(), To_Country = character(), 
                                TotalHead = numeric(), NYears = numeric(), M_ij = numeric())
      }
      
      # 4. Destination Host Biomass (P_j) with Dynamic Year Fallback
      incProgress(0.2, detail = "Evaluating destination host biomass...")
      pop_sp <- clean_pop %>% filter(Species == sp)
      pop_selected_yr <- pop_sp %>% filter(Year == pyr)
      pop_fallback    <- pop_sp %>% 
        filter(!ISO3 %in% pop_selected_yr$ISO3) %>%
        arrange(desc(Year)) %>%
        distinct(ISO3, .keep_all = TRUE)
      
      pop_dest <- bind_rows(pop_selected_yr, pop_fallback) %>%
        group_by(ISO3, Country) %>%
        summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop") %>%
        mutate(LogPop = log10(Population + 1),
               P_j = safe_min_max(LogPop))
      
      # 5. Destination Pathogen Receptivity / History (H_j)
      h_dest <- clean_dis %>%
        filter(Disease == dis) %>%
        group_by(ISO3) %>%
        summarise(RawH = if (all(is.na(DiseaseStatusMax))) 0 else max(DiseaseStatusMax, na.rm = TRUE), .groups = "drop") %>%
        mutate(H_j = safe_min_max(RawH))
      
      # 6. Destination Synthesis Matrix
      incProgress(0.1, detail = "Synthesizing spatial matrix...")
      all_destination_isos <- unique(c(pop_dest$ISO3, clean_cent$ISO3, world_sf$ISO3, flow_from_src$To_ISO))
      all_destination_isos <- all_destination_isos[!is.na(all_destination_isos) & all_destination_isos != src_iso]
      
      res_df <- tibble(ISO3 = all_destination_isos) %>%
        mutate(Country = countrycode(ISO3, origin = "iso3c", destination = "country.name", custom_match = c("XKX" = "Kosovo"))) %>%
        left_join(pop_dest %>% select(ISO3, Population, P_j), by = "ISO3") %>%
        left_join(flow_from_src %>% select(To_ISO, TotalHead, NYears, M_ij), by = c("ISO3" = "To_ISO")) %>%
        left_join(h_dest %>% select(ISO3, H_j), by = "ISO3") %>%
        mutate(
          DirectTrade = !is.na(M_ij) & (TotalHead > 0 | NYears > 0 | M_ij > 0),
          M_ij = coalesce(M_ij, 0),
          P_j  = coalesce(P_j, 0),
          H_j  = coalesce(H_j, 0),
          TotalHead = coalesce(TotalHead, 0),
          NYears    = coalesce(NYears, 0),
          
          # Two-Stage Multiplicative Formulation
          IR = (wd * d_i) + (wm * M_ij),
          ER = (wp * P_j) + (wh * H_j),
          Final_Risk = IR * ER
        ) %>%
        arrange(desc(Final_Risk)) %>%
        mutate(Rank = row_number())
      
      # 7. Safe Risk Classification
      res_df$Risk_Category <- classify_risk(res_df$Final_Risk)
      
      # Spatial Join with Global Polygons
      sf_joined <- world_sf %>%
        left_join(res_df, by = "ISO3")
      
      list(data = res_df, sf = sf_joined, source = src, src_iso = src_iso, d_i = d_i, flow = flow_from_src)
    })
  }, ignoreNULL = FALSE)
  
  # --- Value Boxes ---
  output$vb_count <- renderText({ 
    res <- mcda_data()
    req(res, res$data)
    paste(nrow(res$data), "Destinations") 
  })
  output$vb_direct_partners <- renderText({ 
    res <- mcda_data()
    req(res, res$data)
    paste(sum(res$data$DirectTrade, na.rm = TRUE), "Trade Partners") 
  })
  output$vb_top_risk <- renderText({
    res <- mcda_data()
    req(res, res$data)
    d <- res$data
    if (nrow(d) > 0 && !is.na(d$Final_Risk[1])) {
      paste0(d$Country[1], " (", round(d$Final_Risk[1], 3), ")")
    } else {
      "N/A"
    }
  })
  
  # --- Tab 1: Leaflet Map ---
  output$risk_map <- renderLeaflet({
    res <- mcda_data()
    req(res, res$sf)
    map_sf <- res$sf
    
    pal <- colorFactor(
      palette = c("#2C7BB6", "#ABD9E9", "#FFFFBF", "#FDAE61", "#D7191C"),
      levels  = c("Very Low", "Low", "Moderate", "High", "Very High"),
      na.color = "#E0E0E0"
    )
    
    popup_txt <- paste0(
      "<div style='font-size: 13px;'>",
      "<strong>Destination: </strong>", map_sf$name_long, "<br/>",
      "<strong>Global Rank: </strong>", ifelse(is.na(map_sf$Rank), "Unranked", map_sf$Rank), "<br/>",
      "<strong>Final Spatial Risk: </strong>", ifelse(is.na(map_sf$Final_Risk), "No Data", round(map_sf$Final_Risk, 3)), "<br/>",
      "<strong>Risk Category: </strong>", ifelse(is.na(map_sf$Risk_Category), "Unmonitored", as.character(map_sf$Risk_Category)), "<br/>",
      "<hr style='margin:4px 0;'/>",
      "<strong>Direct Movement Pathway: </strong>", ifelse(!is.na(map_sf$DirectTrade) & map_sf$DirectTrade, 
                                                           "<span style='color:darkgreen;font-weight:bold;'>Direct Trade Active</span>", 
                                                           "<span style='color:gray;'>No Direct Flow</span>"), "<br/>",
      "<strong>Live Animals Traded: </strong>", ifelse(is.na(map_sf$TotalHead), 0, format(map_sf$TotalHead, big.mark=",")), " head<br/>",
      "<strong>Active Trade Years: </strong>", ifelse(is.na(map_sf$NYears), 0, map_sf$NYears), " yrs<br/>",
      "<hr style='margin:4px 0;'/>",
      "<strong>Introduction Risk (IR): </strong>", round(map_sf$IR, 3), "<br/>",
      "<strong>Establishment Risk (ER): </strong>", round(map_sf$ER, 3), "<br/>",
      "<strong>Host Biomass Score: </strong>", round(map_sf$P_j, 3),
      "</div>"
    )
    
    leaflet(map_sf, options = leafletOptions(minZoom = 1.5)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(Risk_Category),
        weight = ifelse(!is.na(map_sf$DirectTrade) & map_sf$DirectTrade, 2.0, 0.5),
        color  = ifelse(!is.na(map_sf$DirectTrade) & map_sf$DirectTrade, "#0F4C81", "#FFFFFF"),
        fillOpacity = 0.82,
        highlightOptions = highlightOptions(weight = 2.5, color = "#000000", bringToFront = TRUE),
        popup = popup_txt
      ) %>%
      addLegend(pal = pal, values = ~Risk_Category, position = "bottomright",
                title = "Risk Category", na.label = "Unmonitored")
  })
  
  # --- Tab 2: Ranking Table & Plotly Decomposition ---
  output$ranking_table <- renderDT({
    res <- mcda_data()
    req(res, res$data)
    df <- res$data %>%
      select(Rank, Country, Final_Risk, Risk_Category, IR, ER, M_ij, P_j, H_j, DirectTrade) %>%
      rename(
        `Final Risk` = Final_Risk,
        `Risk Tier` = Risk_Category,
        `Intro Risk (IR)` = IR,
        `Estab Risk (ER)` = ER,
        `Movement Score` = M_ij,
        `Pop Score` = P_j,
        `Disease History` = H_j,
        `Direct Trade` = DirectTrade
      )
    
    datatable(df, options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE), rownames = FALSE) %>%
      formatRound(columns = c("Final Risk", "Intro Risk (IR)", "Estab Risk (ER)", 
                              "Movement Score", "Pop Score", "Disease History"), digits = 3)
  })
  
  output$decomp_bar <- renderPlotly({
    res <- mcda_data()
    req(res, res$data)
    
    top20 <- res$data %>%
      slice_head(n = 20) %>%
      arrange(Final_Risk)
    
    req(nrow(top20) > 0)
    top20$CountryLabel <- factor(top20$Country, levels = top20$Country)
    
    plot_ly(top20, y = ~CountryLabel, x = ~IR, type = 'bar', name = 'Introduction Risk (IR)', 
            marker = list(color = '#E65100')) %>%
      add_trace(x = ~ER, name = 'Establishment Risk (ER)', marker = list(color = '#0F4C81')) %>%
      layout(
        barmode = 'group',
        xaxis = list(title = "Stage Score (0.0 - 1.0)", range = c(0, 1.05)),
        yaxis = list(title = "", automargin = TRUE, categoryorder = "array", categoryarray = top20$Country),
        margin = list(l = 180, r = 30, t = 30, b = 60),
        legend = list(orientation = 'h', x = 0.2, y = -0.12)
      )
  })
  
  # --- Tab 3: Geographical Movement Network on Leaflet Map ---
  output$network_map <- renderLeaflet({
    res <- mcda_data()
    req(res, res$data)
    src     <- res$source
    src_iso <- res$src_iso
    df      <- res$data
    flow    <- res$flow
    
    src_coord <- country_coords %>% filter(ISO3 == src_iso)
    
    m <- leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(lng = if (nrow(src_coord) > 0) src_coord$Lon[1] else 0,
              lat = if (nrow(src_coord) > 0) src_coord$Lat[1] else 20,
              zoom = 2)
    
    if (nrow(flow) == 0 || nrow(src_coord) == 0) {
      if (nrow(src_coord) > 0) {
        m <- m %>% addCircleMarkers(
          lng = src_coord$Lon[1], lat = src_coord$Lat[1],
          radius = 12, color = "#28A745", fillColor = "#28A745", fillOpacity = 0.9,
          popup = paste0("<b>Source: ", src, "</b><br>No outgoing movements recorded for this species.")
        )
      }
      return(m)
    }
    
    flow_geo <- flow %>%
      left_join(country_coords, by = c("To_ISO" = "ISO3")) %>%
      left_join(df %>% select(ISO3, Final_Risk, Risk_Category), by = c("To_ISO" = "ISO3")) %>%
      filter(!is.na(Lon), !is.na(Lat))
    
    src_lon <- src_coord$Lon[1]
    src_lat <- src_coord$Lat[1]
    
    pal_net <- colorFactor(
      palette = c("#2C7BB6", "#ABD9E9", "#FFFFBF", "#FDAE61", "#D7191C"),
      levels  = c("Very Low", "Low", "Moderate", "High", "Very High"),
      na.color = "#E0E0E0"
    )
    
    for (k in seq_len(nrow(flow_geo))) {
      head_val <- flow_geo$TotalHead[k]
      line_w <- pmax(1.5, pmin(8, log10(head_val + 1) * 1.5))
      
      m <- m %>% addPolylines(
        lng = c(src_lon, flow_geo$Lon[k]),
        lat = c(src_lat, flow_geo$Lat[k]),
        weight = line_w,
        color = "#00D2BE",
        opacity = 0.75,
        dashArray = "4, 6",
        popup = paste0(
          "<b>Trade Route:</b> ", src, " &rarr; ", flow_geo$To_Country[k], "<br/>",
          "<b>Total Animals:</b> ", format(flow_geo$TotalHead[k], big.mark=","), " head<br/>",
          "<b>Years Active:</b> ", flow_geo$NYears[k], " years"
        )
      )
    }
    
    m <- m %>% addCircleMarkers(
      data = flow_geo,
      lng = ~Lon, lat = ~Lat,
      radius = ~pmax(6, Final_Risk * 18 + 6),
      color = "#FFFFFF", weight = 1.5,
      fillColor = ~pal_net(Risk_Category), fillOpacity = 0.95,
      popup = ~paste0(
        "<b>Destination:</b> ", To_Country, "<br/>",
        "<b>Traded Animals:</b> ", format(TotalHead, big.mark=","), " head<br/>",
        "<b>Final Spatial Risk:</b> ", round(Final_Risk, 3), "<br/>",
        "<b>Risk Category:</b> ", Risk_Category
      )
    )
    
    m <- m %>% addCircleMarkers(
      lng = src_lon, lat = src_lat,
      radius = 12, color = "#FFFFFF", weight = 2.5,
      fillColor = "#28A745", fillOpacity = 1.0,
      popup = paste0("<b>SOURCE COUNTRY: ", src, "</b><br>Out-Degree: ", nrow(flow), " destinations")
    )
    
    m
  })
  
  # --- Tab 4: Sensitivity Analysis ---
  sensitivity_data <- reactive({
    res <- mcda_data()
    req(res, res$data)
    base <- res$data
    d_i  <- res$d_i
    
    calc_scen <- function(wd, wm, wp, wh) {
      ir <- (wd * d_i) + (wm * base$M_ij)
      er <- (wp * base$P_j) + (wh * base$H_j)
      ir * er
    }
    
    scen_equal <- calc_scen(0.5, 0.5, 0.5, 0.5)
    scen_move  <- calc_scen(0.2, 0.8, 0.3, 0.7)
    scen_host  <- calc_scen(0.8, 0.2, 0.8, 0.2)
    
    safe_cor <- function(x, y, method) {
      if (sd(x, na.rm=TRUE) == 0 || sd(y, na.rm=TRUE) == 0) return(1.0)
      v <- cor(x, y, method = method, use = "complete.obs")
      if (is.na(v)) 1.0 else round(v, 3)
    }
    
    rho_equal <- safe_cor(base$Final_Risk, scen_equal, "spearman")
    tau_equal <- safe_cor(base$Final_Risk, scen_equal, "kendall")
    
    rho_move  <- safe_cor(base$Final_Risk, scen_move, "spearman")
    tau_move  <- safe_cor(base$Final_Risk, scen_move, "kendall")
    
    rho_host  <- safe_cor(base$Final_Risk, scen_host, "spearman")
    tau_host  <- safe_cor(base$Final_Risk, scen_host, "kendall")
    
    summary_tab <- tibble(
      `Weight Scenario` = c(
        "Equal Weights (w_D=0.5, w_M=0.5 | w_P=0.5, w_H=0.5)", 
        "Movement-Dominant (w_D=0.2, w_M=0.8 | w_P=0.3, w_H=0.7)", 
        "Disease/Host-Dominant (w_D=0.8, w_M=0.2 | w_P=0.8, w_H=0.2)"
      ),
      `Spearman Rank Correlation (rho)` = c(rho_equal, rho_move, rho_host),
      `Kendall Rank Correlation (tau)`  = c(tau_equal, tau_move, tau_host),
      `Ranking Stability Verdict` = ifelse(c(rho_equal, rho_move, rho_host) >= 0.80, 
                                           "Highly Stable", "Moderately Sensitive")
    )
    
    list(table = summary_tab, base = base$Final_Risk, equal = scen_equal, move = scen_move, host = scen_host)
  })
  
  output$sensitivity_summary <- renderTable({
    sensitivity_data()$table
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$sensitivity_scatter <- renderPlotly({
    s <- sensitivity_data()
    req(s$base, s$move, s$host)
    plot_ly(x = s$base, y = s$move, type = 'scatter', mode = 'markers',
            name = 'Movement-Dominant Scenario', marker = list(color = '#E65100', size = 6)) %>%
      add_trace(y = s$host, name = 'Host-Dominant Scenario', marker = list(color = '#0F4C81', size = 6)) %>%
      layout(
        xaxis = list(title = "Baseline Model Risk Score"),
        yaxis = list(title = "Alternative Scenario Risk Score"),
        margin = list(t = 20, b = 60),
        legend = list(orientation = 'h', x = 0.15, y = -0.15)
      )
  })
  
  # --- CSV Export Handler ---
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("MCDA_Risk_", input$source_country, "_", input$species, "_", input$disease, ".csv")
    },
    content = function(file) {
      write_csv(mcda_data()$data, file)
    }
  )
}

# --- 4. LAUNCH DASHBOARD ---
shinyApp(ui, server)