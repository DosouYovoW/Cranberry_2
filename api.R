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

# ---- Encoders ------------------------------------------------
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

# ---- Schema & Recipe -----------------------------------------
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

gaussian_recipe <-
  recipe(sqrt_yield ~ ., data = tibble(sqrt_yield = 0, Regie = 0)) %>%
  step_nzv(all_predictors()) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  prep(training = NULL, retain = TRUE)

# ---- Load models ---------------------------------------------
models <- list()
if (file.exists("gaussian_cranberry_model.rds")) {
  m <- readRDS("gaussian_cranberry_model.rds")
  if (inherits(m, "bundled_model")) m <- bundle::unbundle(m)
  models[["gaussian_current_year"]] <- m
}

model_dir <- "next_year_list_xgboost"
if (dir.exists(model_dir)) {
  files <- list.files(model_dir, pattern = "\\.rds$", full.names = TRUE)
  for (f in files) {
    nm <- gsub("\\.rds$", "", basename(f))
    m <- readRDS(f)
    if (inherits(m, "bundled_model")) m <- bundle::unbundle(m)
    models[[nm]] <- m
  }
}

has_model <- function(name) isTRUE(!is.null(models[[name]]))
inv_sqrt <- function(x) pmax(x, 0)^2

# ---- Helpers -------------------------------------------------
ensure_df <- function(payload) {
  if (is.list(payload) && !is.null(names(payload))) {
    as_tibble(payload)
  } else if (is.list(payload)) {
    bind_rows(lapply(payload, as_tibble))
  } else {
    stop("Payload must be a JSON object or array of objects.")
  }
}

harmonize_inputs <- function(df) {
  df %>% janitor::clean_names()
}

coalesce_cols <- function(df, cols) {
  for (c in cols) if (!c %in% names(df)) df[[c]] <- NA_real_
  df %>% select(all_of(cols))
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
function(req, res){
  if (!has_model("gaussian_current_year")) {
    res$status <- 500; return(as_error("Gaussian model not loaded on server."))
  }
  payload <- tryCatch(fromJSON(req$postBody, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(payload)) { res$status <- 400; return(as_error("Invalid JSON body.")) }
  df <- tryCatch(ensure_df(payload), error = function(e) NULL)
  if (is.null(df)) { res$status <- 400; return(as_error("Bad JSON format.")) }
  
  df <- harmonize_inputs(df)
  needed <- c("regie","n_fertilizer","p_fertilizer","k_fertilizer","mg_fertilizer",
              "s_fertilizer","ca_fertilizer","zn_fertilizer","cu_fertilizer","b_fertilizer",
              "mn_fertilizer","soil_type","variete","frozen")
  X <- coalesce_cols(df, needed)
  
  X_baked <- tryCatch({ bake(gaussian_recipe, new_data = X) }, error = function(e) X)
  
  pred_sqrt <- as.numeric(predict_with("gaussian_current_year", X_baked))
  tibble(.row = seq_len(nrow(X)),
         sqrt_yield_pred = pred_sqrt,
         yield_pred = inv_sqrt(pred_sqrt))
}

#* List next-year models
#* @get /models/next_year
function(){
  list(models = setdiff(names(models), "gaussian_current_year"))
}

#* Predict with a next-year model
#* @param model_name:string
#* @post /predict/next_year/<model_name>
function(req, res, model_name){
  if (!has_model(model_name)) {
    res$status <- 404; return(as_error(sprintf("Model '%s' not found.", model_name), 404))
  }
  payload <- tryCatch(fromJSON(req$postBody, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(payload)) { res$status <- 400; return(as_error("Invalid JSON body.")) }
  df <- tryCatch(ensure_df(payload), error = function(e) NULL)
  if (is.null(df)) { res$status <- 400; return(as_error("Bad JSON format.")) }
  
  m <- models[[model_name]]
  preds <- as.numeric(stats::predict(m, newdata = df))
  tibble(.row = seq_len(nrow(df)), prediction = preds)
}

# ---- Run -----------------------------------------------------
# To run locally: R -e "pr <- plumber::plumb('api.R'); pr$run(host='0.0.0.0', port=8000)"
