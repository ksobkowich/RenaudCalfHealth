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

