#' draw_weights_boot
#'
#' @param y_obs Observed outcomes
#' @param X_obs Predictors associated with observed outcomes
#' @param beta_hat Estimated weights based on observed data
#' @param V
#' @param n1 Number of observed outcomes
#' @param q  Number of predictors (including the intercept)
#' @param ridge Ridge penalty (a small number close to 0)
#'
#' @return beta_dot
#' @export
#'
#' @examples
draw_weights_boot <- function(y_obs, X_obs, beta_hat, V, n1, q, ridge) {

  sample_ind <- sample(x = 1:n1, size = n1, replace = TRUE)
  X_obs_dot <- X_obs[sample_ind, ]
  y_obs_dot <- y_obs[sample_ind]
  S_dot <- crossprod(x = X_obs_dot)
  S_ridge_dot <- diag(x = S_dot) * ridge
  if (length(S_ridge_dot) == 1) {
    S_ridge_dot <- matrix(S_ridge_dot)
  }
  V_dot <- solve(a = S_dot + diag(S_ridge_dot))
  # Estimated weights calculated based on random samples
  beta_dot <- V_dot %*% crossprod(x = X_obs_dot, y = y_obs_dot)

  return(beta_dot)
}
