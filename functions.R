apply_general_calculations <- function(general_vec, formulas) {
  for (name in names(formulas)) {
    general_vec[[name]] <- formulas[[name]](general_vec)
  }
  general_vec
}

apply_calculations <- function(data_vec, general_vec, formulas) {
  for (name in names(formulas)) {
    data_vec[[name]] <- formulas[[name]](data_vec, general_vec)
  }
  data_vec
}

add_totals <- function(vec, general) {
  cost_fields <- c("mort_cost", "prewean_feed_cost", "postwean_feed_cost",
                   "labour_cost", "dtc_cost", "milk_cost")
  
  vec["cost_per_calf"] <- sum(vec[cost_fields], na.rm = T)
  
  heifer_calvings <- general[["num_calvings"]] * (general[["perc_hefers"]] / 100)
  vec["total_prev"] <- heifer_calvings * (vec["perc_prev"] / 100)
  
  vec["total_cost"] <- vec["cost_per_calf"] * vec["total_prev"]
  
  return(vec)
}

# IgG Model Calculations
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

numify <- function(x, eps = 1e-6) {
  if (is.null(x)) return(NA_real_)
  x <- as.character(x); x <- trimws(gsub(",", "", x))
  out <- suppressWarnings(as.numeric(gsub("[^0-9eE\\+\\-\\.]", "", x)))
  le <- grepl("^<=\\s*[+-]?[0-9]*\\.?[0-9]+$", x)
  ge <- grepl("^>=\\s*[+-]?[0-9]*\\.?[0-9]+$", x)
  if (any(le)) out[le] <- as.numeric(sub("^<=\\s*", "", sub("\\s*$", "", x[le]))) - eps
  if (any(ge)) out[ge] <- as.numeric(sub("^>=\\s*", "", sub("\\s*$", "", x[ge]))) + eps
  out
}

predict_day1_igg <- function(pars, IgG_obs, Day_obs, draws = 4000L, include_random = TRUE) {
  IgG_obs <- numify(IgG_obs); Day_obs <- numify(Day_obs)
  if (!is.finite(IgG_obs) || IgG_obs <= 0 || !is.finite(Day_obs) || Day_obs < 1)
    stop("Invalid IgG or Day.")
  
  slope_mean <- pars$slope
  sigma      <- pars$sigma
  tau11      <- pars$tau11 %||% 0
  
  slope_sd <- if (include_random) sqrt(max(0, tau11)) else 0
  
  slope_draws <- rnorm(draws, mean = slope_mean, sd = slope_sd)
  eps_draws   <- rnorm(draws, mean = 0, sd = sigma)
  
  log_day1 <- log(IgG_obs) - slope_draws * (Day_obs - 1) + eps_draws
  igg_day1 <- exp(log_day1)
  
  tibble::tibble(
    median   = stats::median(igg_day1),
    lower_95 = stats::quantile(igg_day1, 0.025),
    upper_95 = stats::quantile(igg_day1, 0.975),
    lower_80 = stats::quantile(igg_day1, 0.10),
    upper_80 = stats::quantile(igg_day1, 0.90),
    lower_50 = stats::quantile(igg_day1, 0.25),
    upper_50 = stats::quantile(igg_day1, 0.75)
  )
}


# STP model calculations
back_predict_day1 <- function(model,
                              day,
                              colostrum,
                              stp_measure,
                              levels_override = c("1","2","3"),
                              clip_day = TRUE) {
  if (!inherits(model, "gam")) {
    stop("`model` must be an mgcv::gam. Got: ", paste(class(model), collapse = "/"))
  }
  recycled <- vctrs::vec_recycle_common(
    day         = as.numeric(day),
    colostrum   = as.character(colostrum),
    stp_measure = as.numeric(stp_measure)
  )
  day         <- recycled$day
  colostrum   <- recycled$colostrum
  stp_measure <- recycled$stp_measure
  if (isTRUE(clip_day)) {
    day <- pmin(pmax(day, 1), 7)
  }
  model_levels <- tryCatch(levels(model$model$Colostrum), error = function(e) NULL)
  levs <- if (!is.null(model_levels)) model_levels else levels_override
  
  newdat <- data.frame(
    Day         = day,
    Colostrum   = factor(colostrum, levels = levs),
    stp_current = stp_measure,
    stringsAsFactors = FALSE
  )
  if (anyNA(newdat$Day) || anyNA(newdat$stp_current) || anyNA(newdat$Colostrum)) {
    stop("Missing/invalid inputs: ensure Day, STP, and Colostrum (1/2/3) are all provided.")
  }
  pr <- mgcv::predict.gam(model, newdata = newdat, se.fit = TRUE)
  fit    <- as.numeric(pr$fit)
  se_mean <- as.numeric(pr$se.fit)

  sigma   <- sqrt(summary(model)$scale)
  se_pred <- sqrt(se_mean^2 + sigma^2)
  
  z95 <- 1.95996398454005
  z50 <- 0.674489750196082
  
  CI50_low  <- fit - z50 * se_mean; CI50_high <- fit + z50 * se_mean
  CI95_low  <- fit - z95 * se_mean; CI95_high <- fit + z95 * se_mean
  PI50_low  <- fit - z50 * se_pred; PI50_high <- fit + z50 * se_pred
  PI95_low  <- fit - z95 * se_pred; PI95_high <- fit + z95 * se_pred
  
  is_day1 <- (day <= 1)
  
  # A priori rules
  if (any(is_day1)) {
    fit[is_day1]      <- stp_measure[is_day1]
    CI50_low[is_day1] <- stp_measure[is_day1]; CI50_high[is_day1] <- stp_measure[is_day1]
    CI95_low[is_day1] <- stp_measure[is_day1]; CI95_high[is_day1] <- stp_measure[is_day1]
    PI50_low[is_day1] <- stp_measure[is_day1]; PI50_high[is_day1] <- stp_measure[is_day1]
    PI95_low[is_day1] <- stp_measure[is_day1]; PI95_high[is_day1] <- stp_measure[is_day1]
  }

  if (any(!is_day1)) {
    idx <- which(!is_day1)
    fit[idx]      <- pmax(fit[idx],      stp_measure[idx], na.rm = TRUE)
    CI50_low[idx] <- pmax(CI50_low[idx], stp_measure[idx], na.rm = TRUE)
    CI95_low[idx] <- pmax(CI95_low[idx], stp_measure[idx], na.rm = TRUE)
    PI50_low[idx] <- pmax(PI50_low[idx], stp_measure[idx], na.rm = TRUE)
    PI95_low[idx] <- pmax(PI95_low[idx], stp_measure[idx], na.rm = TRUE)
  }

  tibble::tibble(
    Day            = day,
    Colostrum      = as.character(newdat$Colostrum),
    STP_measured   = stp_measure,
    STP_day1_pred  = fit,
    CI50_low,  CI50_high,
    CI95_low,  CI95_high,
    PI50_low,  PI50_high,
    PI95_low,  PI95_high
  )
}
