#' draw_weights_bayes
#'
#' @param y_obs Observed outcomes
#' @param X_obs Predictors associated with observed outcomes
#' @param beta_hat Estimated weights based on observed data
#' @param V
#' @param n1 Number of observed outcomes
#' @param q  Number of predictors (including the intercept)
#'
#' @return beta_dot
#' @export
#'
#' @examples
draw_weights_bayes <- function(y_obs, X_obs, beta_hat, V, n1, q) {

  g_dot <- rchisq(n = 1, df = n1 - q)
  sigma2_dot <- as.numeric(crossprod(x = y_obs - X_obs %*% beta_hat) / g_dot)
  z1_dot <- rnorm(n = q)
  V_half <- chol(x = V)
  # Estimated weights calculated based on random samples
  beta_dot <- beta_hat + crossprod(x = V_half, y = z1_dot) * sqrt(sigma2_dot)
  return(beta_dot)
}
