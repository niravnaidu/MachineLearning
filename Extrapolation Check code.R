extrapolation_check2 <- function(M, newdata) {
  require(flextable)
  # Original model matrix
  X <- model.matrix(M)
  if ("(Intercept)" %in% colnames(X)) {
    X <- X[, -1, drop = FALSE]
  }
  
  # New data matrix
  if (missing(newdata)) {
    NEW.X <- X
  } else {
    NEW.X <- model.matrix(delete.response(terms(M)), newdata)
    if ("(Intercept)" %in% colnames(NEW.X)) {
      NEW.X <- NEW.X[, -1, drop = FALSE]
    }
    
    # Check column alignment
    if (!all(colnames(NEW.X) %in% colnames(X))) {
      missing_cols <- setdiff(colnames(NEW.X), colnames(X))
      stop(paste("Mismatch in newdata columns:\n", paste(missing_cols, collapse = ", ")))
    }
    
    NEW.X <- NEW.X[, colnames(X), drop = FALSE]
  }
  
  # Mahalanobis calculations
  MEAN <- colMeans(X)
  COV <- cov(X)
  all.MD <- mahalanobis(X, center = MEAN, cov = COV)
  MD <- mahalanobis(NEW.X, center = MEAN, cov = COV)
  percentiles <- sapply(MD, function(x) mean(all.MD <= x))
  
  # Create data frame
  RESULTS <- data.frame(
    Observation = seq_along(percentiles),
    Percentile = round(100 * percentiles, 1),
    Status = ifelse(percentiles <= 0.95, "✓ In Range", "⚠ Extrapolated")
  )
  
  # Create flextable
  ft <- flextable(RESULTS) |>
    set_header_labels(
      Observation = "Observation #",
      Percentile = "Percentile (Compared to Training Data)",
      Status = "Extrapolation Status"
    ) |>
    bold(part = "header") |>
    color(part = "header", color = "white") |>
    bg(part = "header", bg = "#4F81BD") |>
    align(align = "center", part = "all") |>
    fontsize(size = 11, part = "all") |>
    padding(padding = 6, part = "all") |>
    bg(i = ~Status == "⚠ Extrapolated", bg = "#F4CCCC") |>
    bg(i = ~Status == "✓ In Range", bg = "#D9EAD3") |>
    bold(i = ~Status == "⚠ Extrapolated", bold = TRUE) |>
    autofit()
  
  return(ft)
}
