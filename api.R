# =============================================================
# api.R — Production-ready plumber API for your modeling stack
# =============================================================

suppressPackageStartupMessages({
  library(plumber)
  library(jsonlite)
  library(tidyverse)
  library(tidymodels)
  library(bundle)
  library(xgboost)
  library(compositions)
  library(lubridate)
  library(recipes)
  library(readr)
  library(janitor)
  library(caret) 
  library(kernlab) 
})

# ---- CORS & Error handling -----------------------------------
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
  plumber::forward()
}

as_error <- function(msg, code = 400) {
  list(error = TRUE, code = code, message = msg)
}

# ---- Variety encoders used in training -----------------------
new_productive <- c("crimson queen", "demoranville", "haines",
                    "hyred", "welker", "mullica queen",
                    "sundance", "scarlett knight")
old <- c("ben larocque", "ben lear", "ben-pil-35", "bergman",
         "gardner", "pilgrim", "howes", "wilcox")
stevens_grygleski_gh1 <- c("stevens", "grygleski", "gh1")

encode_variety <- function(v) {
  v <- tolower(trimws(v))
  dplyr::case_when(
    v %in% stevens_grygleski_gh1 ~ 1,
    v %in% old ~ 2,
    v %in% new_productive ~ 3,
    TRUE ~ NA_real_
  )
}

# ---- UI -> Training names/encodings mapper -------------------
# Accepts your UI labels and returns a data frame whose columns
# match the names and encodings used when training the model.
ui_to_model_names <- function(df) {
  
  # 1) Rename UI labels -> training variable names (case-insensitive)
  nm <- names(df)
  map <- c(
    # Fertilizer sliders (UI labels)
    "N fertilizer (lbs/acre)"  = "N_fertilizer",
    "P fertilizer (lbs/acre)"  = "P_fertilizer",
    "K fertilizer (lbs/acre)"  = "K_fertilizer",
    "Mg fertilizer (lbs/acre)" = "Mg_fertilizer",
    "S fertilizer (lbs/acre)"  = "S_fertilizer",
    "Ca fertilizer (lbs/acre)" = "Ca_fertilizer",
    "Zn fertilizer (lbs/acre)" = "Zn_fertilizer",
    "Cu fertilizer (lbs/acre)" = "Cu_fertilizer",
    "B fertilizer (lbs/acre)"  = "B_fertilizer",
    "Mn fertilizer (lbs/acre)" = "Mn_fertilizer",
    
    # Soil analysis (ppm)
    "spring water pH" = "PhEau",      # <- canonicalize to PhEau (was ph_eau before)
    "P_Sol (ppm)" = "P_Sol",
    "K_Sol (ppm)" = "K_Sol",
    "Ca_Sol (ppm)"= "Ca_Sol",
    "Al_Sol (ppm)"= "Al_Sol",
    "Mg_Sol (ppm)"= "Mg_Sol",
    "Zn_Sol (ppm)"= "Zn_Sol",
    "Cu_Sol (ppm)"= "Cu_Sol",
    "B_Sol (ppm)" = "B_Sol",
    "Mn_Sol (ppm)"= "Mn_Sol",
    "Fe_Sol (ppm)"= "Fe_Sol",
    
    # Leaf analysis (%)
    "N_Fol (%)" = "N_Fol",
    "P_Fol (%)" = "P_Fol",
    "K_Fol (%)" = "K_Fol",
    "Ca_Fol (%)"= "Ca_Fol",
    "Mg_Fol (%)"= "Mg_Fol",
    "B_Fol (%)" = "B_Fol",
    "Cu_Fol (%)"= "Cu_Fol",
    "Fe_Fol (%)"= "Fe_Fol",
    "Zn_Fol (%)"= "Zn_Fol",
    "Mn_Fol (%)"= "Mn_Fol",
    "Al_Fol (%)"= "Al_Fol",
    
    # Climate & others
    "Seasonal total precipitation(mm)"                   = "total_precip",
    "Seasonal number of freezing (min temp < 5°C) days" = "frozen",
    "Field age (year)"                                  = "Age",
    "Purety"                                            = "Pureté",
    "Regie"                                             = "Regie",
    "Soil type"                                         = "Soil_type",
    "Variety"                                           = "Variete"
  )
  
  for (i in seq_along(nm)) {
    key <- nm[i]
    hit <- names(map)[tolower(names(map)) == tolower(key)]
    if (length(hit) == 1) nm[i] <- map[[hit]]
  }
  names(df) <- nm
  
  # 2) Create helper aliases expected elsewhere (snake- and camel-case)
  if ("Regie" %in% names(df)     && !"regie"     %in% names(df)) df$regie     <- df$Regie
  if ("Soil_type" %in% names(df) && !"soil_type" %in% names(df)) df$soil_type <- df$Soil_type
  if ("Variete" %in% names(df)   && !"variete"   %in% names(df)) df$variete   <- df$Variete
  
  # 3) Accept multiple pH names; canonicalize to PhEau and mirror ph_eau
  if (!"PhEau" %in% names(df) && "ph_eau" %in% names(df)) df$PhEau <- df$ph_eau
  if (!"ph_eau" %in% names(df) && "PhEau" %in% names(df)) df$ph_eau <- df$PhEau
  
  # 4) Fertilizer name mirroring (support both training & UI spellings)
  fert_pairs <- list(
    N  = c("N_fertilizer","N_Fert"),
    P  = c("P_fertilizer","P_Fert"),
    K  = c("K_fertilizer","K_Fert"),
    Mg = c("Mg_fertilizer","Mg_Fert"),
    S  = c("S_fertilizer","So_Fert"),   # keep your So_Fert canonical too
    Ca = c("Ca_fertilizer","Ca_Fert"),
    Zn = c("Zn_fertilizer","Zn_Fert"),
    Cu = c("Cu_fertilizer","Cu_Fert"),
    B  = c("B_fertilizer","B_Fert"),
    Mn = c("Mn_fertilizer","Mn_Fert")
  )
  for (k in names(fert_pairs)) {
    a <- fert_pairs[[k]][1]; b <- fert_pairs[[k]][2]
    if (!a %in% names(df) &&  b %in% names(df)) df[[a]] <- df[[b]]
    if (!b %in% names(df) &&  a %in% names(df)) df[[b]] <- df[[a]]
  }
  
  # 5) Encode categorical values to match training
  if ("Regie" %in% names(df)) {
    df$Regie <- dplyr::case_when(
      is.character(df$Regie) & tolower(df$Regie) == "organic"      ~ 0,
      is.character(df$Regie) & tolower(df$Regie) == "conventional" ~ 1,
      TRUE ~ suppressWarnings(as.numeric(df$Regie))
    )
  }
  
  if ("Soil_type" %in% names(df)) {
    df$Soil_type <- dplyr::case_when(
      is.character(df$Soil_type) & tolower(df$Soil_type) == "sand" ~ 0,
      is.character(df$Soil_type)                                   ~ 1,
      TRUE ~ suppressWarnings(as.numeric(df$Soil_type))
    )
  }
  
  if ("Variete" %in% names(df)) {
    df$Variete <- dplyr::case_when(
      is.character(df$Variete) ~ encode_variety(df$Variete),
      TRUE ~ suppressWarnings(as.numeric(df$Variete))
    )
  }
  
  # 6) Coerce numerics where appropriate (don’t wreck character IDs)
  df <- dplyr::mutate(
    df,
    dplyr::across(
      dplyr::everything(),
      function(x) if (is.character(x)) suppressWarnings(as.numeric(x)) else x
    )
  )
  
  df
}

# ---- Minimal schema & recipe ---------------------------------
base_schema <- tibble(
  `Field name` = NA_character_,
  Regie = NA_real_,
  `N fertilizer` = NA_real_,
  `P fertilizer` = NA_real_,
  `K fertilizer` = NA_real_,
  `Mg fertilizer` = NA_real_,
  `S fertilizer` = NA_real_,
  `Ca fertilizer` = NA_real_,
  `Zn fertilizer` = NA_real_,
  `Cu fertilizer` = NA_real_,
  `B fertilizer` = NA_real_,
  `Mn fertilizer` = NA_real_,
  Soil_type = NA_real_,
  Variete = NA_real_,
  frozen = NA_real_
)

# If you saved a real recipe with your workflow, load it instead.
gaussian_recipe <-
  recipe(sqrt_yield ~ ., data = tibble(sqrt_yield = 0, Regie = 0)) %>%
  step_nzv(all_predictors()) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  prep(training = NULL, retain = TRUE)

# ---- Load models (single, robust block) ------------------------------------

# Helper to unbundle safely if needed
safe_unbundle <- function(m) {
  if (requireNamespace("bundle", quietly = TRUE)) {
    tryCatch(bundle::unbundle(m), error = function(e) m)
  } else m
}

# Helpers to pick first existing file/dir
first_existing_file <- function(...) {
  for (p in c(...)) if (file.exists(p)) return(p)
  NA_character_
}
first_existing_dir <- function(...) {
  for (p in c(...)) if (dir.exists(p)) return(p)
  NA_character_
}

# Create models container once
if (!exists("models", inherits = FALSE)) models <- list()

# --- Load Gaussian (current-year) model -------------------------------------
g_path <- first_existing_file(
  "gaussian_cranberry_model.rds",
  "data/gaussian_cranberry_model.rds",
  "models/gaussian_cranberry_model.rds",
  "gaussian_current_year.rds",
  "data/gaussian_current_year.rds",
  "models/gaussian_current_year.rds"
)

if (!is.na(g_path)) {
  try(cat("[loader] gaussian:", g_path, "\n"), silent = TRUE)
  g <- readRDS(g_path)
  models[["gaussian_current_year"]] <- safe_unbundle(g)
} else {
  try(cat("[loader] gaussian RDS NOT FOUND (checked multiple paths)\n"), silent = TRUE)
}

# --- Load next-year balance models ------------------------------------------
ny_dir <- first_existing_dir(
  "next_year_list_xgboost",
  "models/next_year_list_xgboost",
  "data/next_year_list_xgboost"
)

if (!is.na(ny_dir)) {
  files <- list.files(ny_dir, pattern = "\\.rds$", full.names = TRUE)
  for (f in files) {
    nm <- sub("\\.rds$", "", basename(f))
    m  <- safe_unbundle(readRDS(f))
    models[[nm]] <- m
  }
  try(cat("[loader] next-year models loaded:", length(files), "from", ny_dir, "\n"), silent = TRUE)
} else {
  try(cat("[loader] next-year model dir NOT FOUND\n"), silent = TRUE)
}

# --- Small helpers used later ----------------------------------------------
has_model <- function(name) isTRUE(!is.null(models[[name]]))
inv_sqrt  <- function(x) pmax(x, 0)^2

# ---- Helpers -------------------------------------------------
ensure_df <- function(payload) {
  # Accepts:
  # - a single JSON object  -> 1-row tibble
  # - an array of objects   -> rbind into tibble
  # - vectors in fields     -> recycles length-1 fields to the max length
  
  # Case 1: array of row-objects
  if (is.list(payload) && is.null(names(payload))) {
    rows <- lapply(payload, function(x) tibble::as_tibble(x, .name_repair = "check_unique"))
    return(dplyr::bind_rows(rows))
  }
  
  # Case 2: single object (possibly with vector-valued fields)
  if (is.list(payload) && !is.null(names(payload))) {
    # Determine target row count
    lens <- vapply(payload, function(x) if (is.null(x)) 1L else length(x), integer(1))
    n <- max(lens)
    # Recycle length-1 columns; error if incompatible lengths
    payload2 <- lapply(payload, function(x) {
      if (is.null(x)) return(rep(NA, n))
      if (length(x) == 1L) return(rep(x, n))
      if (length(x) == n) return(x)
      stop("Columns have incompatible lengths in JSON payload.")
    })
    return(tibble::as_tibble(payload2, .name_repair = "check_unique"))
  }
  
  stop("Payload must be a JSON object or an array of objects.")
}

harmonize_inputs <- function(df) {
  df %>% janitor::clean_names()
}

coalesce_cols <- function(df, cols) {
  for (c in cols) if (!c %in% names(df)) df[[c]] <- NA_real_
  df %>% dplyr::select(all_of(cols))
}

predict_with <- function(model_name, newdata) {
  if (!has_model(model_name)) stop(sprintf("Model '%s' not available.", model_name))
  m <- models[[model_name]]
  stats::predict(m, newdata = newdata)
}

# ---- API routes ----------------------------------------------
#* @apiTitle Cranberry Modeling API
#* @apiDescription Endpoints for current-year Gaussian model and next-year xgboost models.

#* Health check
#* @get /health
function() {
  list(status = "ok",
       time = as.character(Sys.time()),
       models_loaded = names(models))
}

#* Preflight CORS
#* @options /<path:.*>
function(res){
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
  plumber::forward()
}

#* Schema
#* @get /schema
function(){
  list(fields = names(base_schema),
       example = base_schema %>% head(1))
}

#* Predict current-year yield
#* @post /predict/current_year
#* @param body:object The JSON payload with feature values (UI labels allowed).
#* @serializer json
function(req, res, body){
  if (!has_model("gaussian_current_year")) {
    res$status <- 500; return(as_error("Gaussian model not loaded on server."))
  }
  
  # Parse & shape
  payload <- tryCatch(jsonlite::fromJSON(req$postBody, simplifyVector = FALSE),
                      error = function(e) NULL)
  if (is.null(payload)) { res$status <- 400; return(as_error("Invalid JSON body.")) }
  
  raw_df <- tryCatch(ensure_df(payload), error = function(e) NULL)
  if (is.null(raw_df)) { res$status <- 400; return(as_error("Bad JSON format.")) }
  
  # Map UI labels -> training names/encodings, coerce numerics
  X <- ui_to_model_names(raw_df) %>% dplyr::as_tibble()
  
  # Make sure the key core inputs exist (create 0 if missing)
  core_needed <- c(
    "Regie","Variete","Soil_type",
    "N_fertilizer","P_fertilizer","K_fertilizer","Mg_fertilizer","S_fertilizer",
    "Ca_fertilizer","Zn_fertilizer","Cu_fertilizer","B_fertilizer","Mn_fertilizer",
    "total_precip","frozen","age","purete"
  )
  for (v in core_needed) if (!v %in% names(X)) X[[v]] <- 0
  
  # Guarantee numeric
  X <- dplyr::mutate(X, dplyr::across(dplyr::everything(), ~ suppressWarnings(as.numeric(.))))
  
  # Try recipe; if not compatible, fall back to raw features
  X_baked <- tryCatch(bake(gaussian_recipe, new_data = X), error = function(e) X)
  
  # Predict with auto-add for any missing engineered predictors
  max_attempts <- 500
  attempt <- 0
  repeat {
    attempt <- attempt + 1
    res_try <- try(as.numeric(predict_with("gaussian_current_year", X_baked)), silent = TRUE)
    if (!inherits(res_try, "try-error")) { pred_sqrt <- res_try; break }
    if (attempt >= max_attempts) { res$status <- 400; return(as_error("Prediction failed after auto-adding many missing predictors.")) }
    msg <- as.character(res_try)
    m <- regmatches(msg, regexpr("object '([^']+)' not found", msg))
    if (length(m) == 0) { res$status <- 400; return(as_error(paste("Prediction error:", msg, "| Provided:", paste(names(X_baked), collapse=", ")))) }
    missing_name <- sub("object '([^']+)' not found", "\\1", m)
    X_baked[[missing_name]] <- 0
  }
  
  tibble::tibble(
    .row = seq_len(nrow(X)),
    sqrt_yield_pred = pred_sqrt,
    yield_pred = round(inv_sqrt(pred_sqrt),0)
  )
}

#### Chatgpt to start making change here --------------
# ---- Small helpers (must be defined BEFORE routes) --------------------------
# Null-or operator (since %||% isn't in base R)
`%or%` <- function(a, b) if (!is.null(a) && !identical(a, "")) a else b

# Predict but auto-add missing columns as 0 (workflows & xgboost)
# Also handles hardhat::forge "required column(s) missing" by creating them
predict_safely <- function(m, newdata, max_attempts = 500) {
  to_num <- function(res) {
    if (is.data.frame(res)) {
      if (".pred" %in% names(res)) return(as.numeric(res$.pred))
      return(as.numeric(unlist(res, use.names = FALSE)))
    }
    as.numeric(res)
  }
  
  predict_once <- function(obj, X) {
    # tidymodels workflow/model_fit -> new_data=
    if (inherits(obj, "workflow") || inherits(obj, "model_fit")) {
      r <- try(predict(obj, new_data = X), silent = TRUE)
      if (!inherits(r, "try-error")) return(to_num(r))
      r <- try(predict(obj, new_data = as.data.frame(X)), silent = TRUE)
      if (!inherits(r, "try-error")) return(to_num(r))
      stop(as.character(r))
    }
    # other models -> newdata=
    r <- try(stats::predict(obj, newdata = X), silent = TRUE)
    if (!inherits(r, "try-error")) return(to_num(r))
    r <- try(stats::predict(obj, newdata = as.data.frame(X)), silent = TRUE)
    if (!inherits(r, "try-error")) return(to_num(r))
    stop(as.character(r))
  }
  
  Xp <- newdata
  attempt <- 0
  repeat {
    attempt <- attempt + 1
    res_try <- try(predict_once(m, Xp), silent = TRUE)
    if (!inherits(res_try, "try-error")) return(res_try)
    
    if (attempt >= max_attempts) stop(as.character(res_try))
    msg <- as.character(res_try)
    
    # Case 1: classic "object 'foo' not found" (xgboost etc.)
    miss1 <- regmatches(msg, regexpr("object '([^']+)' not found", msg))
    if (length(miss1) > 0) {
      col_missing <- sub("object '([^']+)' not found", "\\1", miss1)
      if (!col_missing %in% names(Xp)) Xp[[col_missing]] <- 0
      next
    }
    
    # Case 2: hardhat::forge required column(s) missing
    # e.g. "The required column \"mean_temp\" is missing."
    # or  "The required columns \"A\", \"B\", ... are missing."
    if (grepl("The required column[s]? ", msg) && grepl("\"", msg)) {
      # extract all quoted names
      m_all <- gregexpr('"([^"]+)"', msg)
      cols <- unique(unlist(regmatches(msg, m_all)))
      cols <- gsub('(^"|"$)', "", cols)           # strip quotes
      # add any that aren't present
      for (cn in cols) if (!cn %in% names(Xp)) Xp[[cn]] <- 0
      next
    }
    
    # If neither pattern matched, surface the error
    stop(msg)
  }
}

# Unbundle any kind of bundled object
try_unbundle <- function(m) {
  if (inherits(m, "bundled_model") ||
      inherits(m, "bundled_workflow") ||
      inherits(m, "bundled_xgboost.Booster") ||
      inherits(m, "bundled_parsnip_model")) {
    return(tryCatch(bundle::unbundle(m), error = function(e) m))
  }
  m
}

# ---- Load models (simple & order-safe) --------------------------------------

# Helpers FIRST (so we can call them below)
safe_unbundle <- function(m) {
  if (requireNamespace("bundle", quietly = TRUE)) {
    tryCatch(bundle::unbundle(m), error = function(e) m)
  } else m
}

first_existing_file <- function(...) {
  for (p in c(...)) if (file.exists(p)) return(p)
  NA_character_
}

first_existing_dir <- function(...) {
  for (p in c(...)) if (dir.exists(p)) return(p)
  NA_character_
}

# Do NOT wipe models if it already exists; otherwise create it once
if (!exists("models", inherits = FALSE)) models <- list()

# Gaussian model (current-year)
g_path <- first_existing_file(
  "gaussian_cranberry_model.rds",
  "data/gaussian_cranberry_model.rds",
  "models/gaussian_cranberry_model.rds",
  "gaussian_current_year.rds",
  "data/gaussian_current_year.rds",
  "models/gaussian_current_year.rds"
)

if (!is.na(g_path)) {
  try(cat("[loader] gaussian:", g_path, "\n"), silent = TRUE)
  g <- readRDS(g_path)
  models[["gaussian_current_year"]] <- safe_unbundle(g)
} else {
  try(cat("[loader] gaussian RDS NOT FOUND (checked multiple paths)\n"), silent = TRUE)
}

# Next-year balance models (21 xgboost/tidymodels)
ny_dir <- first_existing_dir(
  "next_year_list_xgboost",
  "models/next_year_list_xgboost",
  "data/next_year_list_xgboost"
)

if (!is.na(ny_dir)) {
  files <- list.files(ny_dir, pattern = "\\.rds$", full.names = TRUE)
  for (f in files) {
    nm <- sub("\\.rds$", "", basename(f))
    m  <- safe_unbundle(readRDS(f))
    models[[nm]] <- m
  }
  try(cat("[loader] next-year models loaded:", length(files), "from", ny_dir, "\n"), silent = TRUE)
} else {
  try(cat("[loader] next-year model dir NOT FOUND\n"), silent = TRUE)
}


#* Predict with a next-year model
#* @param model_name:string
#* @param body:object The JSON payload with feature values (UI labels allowed).
#* @post /predict/next_year/<model_name>
#* @serializer json list(auto_unbox = TRUE, digits = 6, na = "null")
function(req, res, model_name, body){
  if (!has_model(model_name)) {
    res$status <- 404; return(as_error(sprintf("Model '%s' not found.", model_name), 404))
  }
  
  payload <- tryCatch(jsonlite::fromJSON((req$postBody %or% body), simplifyVector = FALSE),
                      error = function(e) NULL)
  if (is.null(payload)) { res$status <- 400; return(as_error("Invalid JSON body.")) }
  
  raw_df <- tryCatch(ensure_df(payload), error = function(e) NULL)
  if (is.null(raw_df)) { res$status <- 400; return(as_error("Bad JSON format.")) }
  
  X <- ui_to_model_names(raw_df) %>% dplyr::as_tibble()
  
  # So_Fert canonical + mirror
  if (!"So_Fert" %in% names(X) && "S_fertilizer" %in% names(X)) X$So_Fert <- X$S_fertilizer
  if (!"S_fertilizer" %in% names(X) && "So_Fert" %in% names(X)) X$S_fertilizer <- X$So_Fert
  
  core_needed <- c(
    "Regie","Variete","Soil_type",
    "N_Fert","P_Fert","K_Fert","Mg_Fert","So_Fert",
    "Ca_Fert","Zn_Fert","Cu_Fert","B_Fert","Mn_Fert",
    "total_precip","frozen","Age","Pureté"
  )
  for (v in core_needed) if (!v %in% names(X)) X[[v]] <- 0
  
  X <- dplyr::mutate(X, dplyr::across(dplyr::everything(), ~ suppressWarnings(as.numeric(.))))
  
  m <- models[[model_name]]
  pred_vals <- tryCatch(predict_safely(m, X),
                        error = function(e) {
                          res$status <- 400
                          return(as_error(paste("Prediction error:", as.character(e),
                                                "| Provided:", paste(names(X), collapse=", "))))
                        })
  if (is.list(pred_vals) && isTRUE(pred_vals$error)) return(pred_vals)
  
  tibble::tibble(.row = seq_len(nrow(X)), prediction = pred_vals)
}

#* Compute next-year yield from a fertilizer plan (runs all 21 balance models)
#* @param body:object The JSON payload with feature values (UI labels allowed).
#* @post /predict/next_year_yield
#* @serializer json list(auto_unbox = TRUE, digits = 6, na = "null")
function(req, res, body){
  payload <- tryCatch(jsonlite::fromJSON((req$postBody %or% body), simplifyVector = FALSE),
                      error = function(e) NULL)
  if (is.null(payload)) { res$status <- 400; return(as_error("Invalid JSON body.")) }
  
  raw_df <- tryCatch(ensure_df(payload), error = function(e) NULL)
  if (is.null(raw_df)) { res$status <- 400; return(as_error("Bad JSON format.")) }
  
  X <- ui_to_model_names(raw_df) %>% dplyr::as_tibble()
  
  # Accept So_Fert / S_fertilizer and PhEau / ph_eau
  if (!"So_Fert" %in% names(X) && "S_fertilizer" %in% names(X)) X$So_Fert <- X$S_fertilizer
  if (!"S_fertilizer" %in% names(X) && "So_Fert" %in% names(X)) X$S_fertilizer <- X$So_Fert
  if (!"PhEau" %in% names(X) && "ph_eau" %in% names(X)) X$PhEau <- X$ph_eau
  if (!"ph_eau" %in% names(X) && "PhEau" %in% names(X)) X$ph_eau <- X$PhEau
  
  core_needed <- c(
    "Regie","Variete","Soil_type",
    "N_Fert","P_Fert","K_Fert","Mg_Fert","So_Fert",
    "Ca_Fert","Zn_Fert","Cu_Fert","B_Fert","Mn_Fert",
    "total_precip","frozen","Age","Pureté","PhEau"
  )
  for (v in core_needed) if (!v %in% names(X)) X[[v]] <- 0
  
  # Pre-create all balance columns in case any model recipe expects them
  leaf_needed <- c(
    "Leaf_Fv.AlZnMnFeCuBMgCaKPN","Leaf_Al.ZnMnFeCuBMgCaKPN","Leaf_Mn.Fe",
    "Leaf_ZnCuBMgCaKPN.MnFe","Leaf_Cu.Zn","Leaf_B.ZnCu","Leaf_MgCaKPN.ZnCuB",
    "Leaf_Ca.Mg","Leaf_K.MgCa","Leaf_PN.MgCaK","Leaf_P.N"
  )
  soil_needed <- c(
    "soil_Fv.FeBMnCuZnMgKAlPCa","soil_Fe.Al","soil_FeAl.BMnCuZnMgKPCa",
    "soil_Mn.B","soil_BMn.Zn","soil_BMnZn.CuMgKPCa","soil_Cu.Mg",
    "soil_CuMg.Ca","soil_CuMgCa.KP","soil_K.P"
  )
  for (v in c(leaf_needed, soil_needed)) if (!v %in% names(X)) X[[v]] <- 0
  
  X <- dplyr::mutate(X, dplyr::across(dplyr::everything(), ~ suppressWarnings(as.numeric(.))))
  
  if (!has_model("gaussian_current_year")) {
    res$status <- 500; return(as_error("Gaussian model not loaded on server."))
  }
  m_yield <- models[["gaussian_current_year"]]
  
  # Current-year yield (to create Rendement / sqrt_yield like Shiny does)
  X_baked <- tryCatch(bake(gaussian_recipe, new_data = X), error = function(e) X)
  pred_sqrt_cur <- tryCatch(predict_safely(m_yield, X_baked),
                            error = function(e) { res$status <- 400; return(as_error(paste("Current-year yield prediction failed:", as.character(e)))) })
  if (is.list(pred_sqrt_cur) && isTRUE(pred_sqrt_cur$error)) return(pred_sqrt_cur)
  rend_cur <- pmax(pred_sqrt_cur, 0)^2
  
  X_bal <- X %>% dplyr::mutate(
    Age        = if ("Age" %in% names(.)) Age + 1 else 1,
    Rendement  = round(rend_cur, 0),
    sqrt_yield = pred_sqrt_cur
  )
  
  # Run all balance models
  bnames <- names(models)[grepl("^Model_(Leaf|soil)_.*_next(?:_next)?$", names(models))]
  if (length(bnames) == 0) {
    res$status <- 404; return(as_error("No next-year balance models loaded (expected 'Model_Leaf_*_next' or 'Model_soil_*_next').", 404))
  }
  
  bal_cols <- list()
  for (nm in bnames) {
    m <- models[[nm]]
    preds <- tryCatch(predict_safely(m, X_bal),
                      error = function(e) { res$status <- 400; return(as_error(paste0("Balance model '", nm, "' failed: ", as.character(e)))) })
    if (is.list(preds) && isTRUE(preds$error)) return(preds)
    colname <- sub("^Model_", "", nm)
    colname <- sub("_next(_next)?$", "", colname)
    bal_cols[[colname]] <- preds
  }
  BAL <- tibble::as_tibble(bal_cols)
  
  # Normalize name quirks
  if ("soil_FeAl.BMnCuZnMgKPCa_next" %in% names(BAL) && !("soil_FeAl.BMnCuZnMgKPCa" %in% names(BAL)))
    BAL[["soil_FeAl.BMnCuZnMgKPCa"]] <- BAL[["soil_FeAl.BMnCuZnMgKPCa_next"]]
  if ("soil_Mn.B_next" %in% names(BAL) && !("soil_Mn.B" %in% names(BAL)))
    BAL[["soil_Mn.B"]] <- BAL[["soil_Mn.B_next"]]
  if (!("soil_K.P" %in% names(BAL)) && "soil_K.P2" %in% names(BAL))
    BAL[["soil_K.P"]] <- BAL[["soil_K.P2"]]
  
  # Build next-year yield features
  X_next <- dplyr::bind_cols(X_bal, BAL)
  X_next_baked <- tryCatch(bake(gaussian_recipe, new_data = X_next), error = function(e) X_next)
  pred_sqrt_next <- tryCatch(predict_safely(m_yield, X_next_baked),
                             error = function(e) { res$status <- 400; return(as_error(paste("Next-year yield prediction failed:", as.character(e)))) })
  if (is.list(pred_sqrt_next) && isTRUE(pred_sqrt_next$error)) return(pred_sqrt_next)
  yield_next <- pmax(pred_sqrt_next, 0)^2
  
  # Pack balances per row as scalars (no length-1 arrays) and drop soil_K.P2
  bal_list_per_row <- split(BAL, seq_len(nrow(BAL)))
  bal_list_per_row <- lapply(bal_list_per_row, function(df1) {
    lst <- as.list(df1[1, , drop = TRUE])
    lst <- lapply(lst, function(x) unname(as.numeric(x)[1]))
    lst[["soil_K.P2"]] <- NULL
    lst
  })
  
  tibble::tibble(
    .row = seq_len(nrow(X)),
    sqrt_yield_next = pred_sqrt_next,
    yield_next = round(yield_next, 0),
    balances = I(bal_list_per_row)
  )
}

# ---- Run -----------------------------------------------------
# To run locally: R -e "pr <- plumber::plumb('api.R'); pr$run(host='0.0.0.0', port=8000)"
