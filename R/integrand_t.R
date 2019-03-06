#' Bla
#'
#' @param delta A number
#' @param t A number
#' @param n A number
#' @param nu A number
#' @param mu_delta A number
#' @param gamma A number
#' @param kappa A number
#'
#' @return A number
#' @export
#' @import stats
#'
#' @examples
#' integrand_t(1, 2, 3, 4, 5, 6, 7)
integrand_t <- function(delta,
                        t,
                        n,
                        nu,
                        mu_delta,
                        gamma,
                        kappa) {
  dt(
    x = t,
    df = nu,
    ncp = sqrt(n) * delta) * 1 / gamma * dt(
      x = (delta - mu_delta) / gamma,
      df = kappa)
}
