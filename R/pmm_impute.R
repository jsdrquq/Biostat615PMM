#' pmm_impute
#'
#' @param y Vector to be imputed
#' @param ry Location of observed values -> (1) observed (TRUE) vs. missing (FALSE); (2) length(ry) == length(y)
#' @param x Numeric design matrix (no intercepts)
#' @param wy Location of values to impute -> (1) create imputations (TRUE) vs. not (FALSE); (2) length(wy) == length(y)
#' @param bayeswt Type of regression weights -> "Bayesian" (TRUE) or "Bootstrap" (FALSE)
#' @param donors The size of donor pool (usually between 3 and 10)
#' @param matchtype Type of matching distance -> {0, 1, 2}
#' @param c_matcher
#' @param ridge Ridge penalty (a small number close to 0)
#'
#' @return imputed_vals
#' @export
#'
#' @examples
pmm_impute <- function(y, ry, x, wy = NULL, bayeswt = TRUE,
                       donors = 5, matchtype = 1, c_matcher = TRUE, ridge = 1e-05) {

  if (is.null(wy)) {
    wy <- !ry   # Default: Impute the missing part only
  }

  X <- cbind(1, as.matrix(x))   # Design matrix
  X_obs <- X[ry, , drop = FALSE]   # Predictors associated with observed outcomes
  y_obs <- y[ry]   # Observed outcomes
  X_mis <- X[wy, , drop = FALSE]   # Predictors associated with missing outcomes
  n1 <- sum(ry)    # Number of observed outcomes
  n0 <- sum(wy)    # Number of missing outcomes/values to impute
  q <- ncol(X)     # Number of predictors (including the intercept)

  # Compute weights (\hat\beta)
  S <- crossprod(x = X_obs)
  S_ridge <- diag(x = S) * ridge
  if (length(S_ridge) == 1) {
    S_ridge <- matrix(S_ridge)
  }
  V <- solve(a = S + diag(S_ridge))
  beta_hat <- V %*% crossprod(x = X_obs, y = y_obs)   # Estimated weights based on observed data

  if (matchtype > 0) {
    # Compute weights calculated based on random samples (\dot\beta)
    if (bayeswt) {   # Bayesian
      beta_dot <- draw_weights_bayes(y_obs = y_obs, X_obs = X_obs, beta_hat = beta_hat,
                                     V = V, n1 = n1, q = q)
    } else {   # Bootstrap
      beta_dot <- draw_weights_boot(y_obs = y_obs, X_obs = X_obs, beta_hat = beta_hat,
                                    V = V, n1 = n1, q = q, ridge = ridge)
    }
    beta_dot[is.na(beta_dot)] <- 0
  }

  # Compute predicted outcomes
  beta_hat[is.na(beta_hat)] <- 0

  if (matchtype == 0) {
    y_obs_hat <- as.vector(X_obs %*% beta_hat)   # Predicted outcomes for the observed part
    y_mis_hat <- as.vector(X_mis %*% beta_hat)   # Predicted outcomes for the missing part
  }
  if (matchtype == 1) {
    y_obs_hat <- as.vector(X_obs %*% beta_hat)
    y_mis_hat <- as.vector(X_mis %*% beta_dot)
  }
  if (matchtype == 2) {
    y_obs_hat <- as.vector(X_obs %*% beta_dot)
    y_mis_hat <- as.vector(X_mis %*% beta_dot)
  }

  # Matching
  if (c_matcher) {
    imputed_ind <- mice:::matchindex(d = y_obs_hat, t = y_mis_hat, k = donors)   # Indices of observed data used to impute
  } else {
    imputed_ind <- naive_matcher(y_obs_hat = y_obs_hat, y_mis_hat = y_mis_hat,
                                 n1 = n1, n0 = n0, donors = donors)   # Indices of observed data used to impute
  }
  imputed_vals <- y_obs[imputed_ind]

  return(imputed_vals)
}
