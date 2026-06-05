select_carbon_species <- function(df,
                                         weights = list(
                                           size = 0.6,
                                           wood = 0.4
                                         )) {
  
  scale_01 <- function(x) {
    if (all(is.na(x))) return(rep(NA, length(x)))
    (x - min(x, na.rm = TRUE)) /
      (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  }
  
  stopifnot(abs(sum(unlist(weights)) - 1) < 1e-6)
  
  df |>
    dplyr::mutate(
      size_score =
        (scale_01(DAP_max) + scale_01(Height_max)) / 2,
      wood_score =
        scale_01(Wood_density),
      carbon_score =
        weights$size * size_score +
        weights$wood * wood_score
    ) |>
    dplyr::arrange(dplyr::desc(carbon_score))
}
