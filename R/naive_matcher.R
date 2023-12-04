#' naive_matcher
#'
#' @param y_obs_hat Estimated observed outcomes using weights calculated with observed data
#' @param y_mis_hat Estimated missing outcomes using weights calculated with observed data
#' @param n1 Number of observed outcomes
#' @param n0 Number of missing outcomes/values to impute
#' @param donors The size of donor pool (usually between 3 and 10)
#'
#' @return imputed_ind
#' @export
#'
#' @examples
naive_matcher <- function(y_obs_hat, y_mis_hat, n1, n0, donors) {

  eta_dot <- abs(outer(X = y_obs_hat, Y = y_mis_hat, FUN = "-"))   # Distances (n1 * n0)
  imputed_ind <- rep(x = NA, times = n0)   # Indices of observed data used to impute
  for (i in 1:n0) {
    shuffle_ind <- sample(x = 1:n1, size = n1)
    min_ind_s <- sort(x = eta_dot[shuffle_ind, i], index.return = TRUE)$ix[1:donors]
    min_ind_o <- shuffle_ind[min_ind_s]
    imputed_ind[i] <- sample(x = min_ind_o, size = 1)
  }

  return(imputed_ind)
}
