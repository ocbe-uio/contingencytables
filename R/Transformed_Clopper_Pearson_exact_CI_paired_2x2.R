#' @title The Transformed Clopper-Pearson exact confidence interval for
#' the conditional odds ratio
#' @description The Transformed Clopper-Pearson exact confidence interval for
#' the conditional odds ratio
#' @description Described in Chapter 8 "The Paired 2x2 Table"
#' @param n the observed counts (a 2x2 matrix)
#' @param alpha the nominal level, e.g. 0.05 for 95% CIs
#' @examples
#' Transformed_Clopper_Pearson_exact_CI_paired_2x2(ezra_2010)
#' @export
#' @return An object of the [contingencytables_result] class,
#' basically a subclass of [base::list()]. Use the [utils::str()] function
#' to see the specific elements returned.
Transformed_Clopper_Pearson_exact_CI_paired_2x2 <- function(n, alpha = 0.05) {
  validateArguments(mget(ls()))

  # Estimate of the conditional odds ratio (thetacondhat)
  estimate <- n[1, 2] / n[2, 1]

  # The Clopper-Pearson exact interval for mu = pi_12 / (pi_12 + pi_21)
  tmp <- ClopperPearson_exact_CI_1x2_beta_version(
    n[1, 2], n[1, 2] + n[2, 1], alpha
  )
  L_mu <- tmp$lower
  U_mu <- tmp$upper

  # Transform the confidence limits back to the conditional odds ratio scale
  L <- L_mu / (1 - L_mu)
  U <- U_mu / (1 - U_mu)

  printresults <- function() {
    sprintf(
      paste(
        "The transformed Clopper-Pearson exact CI: estimate =",
        "%6.4f (%g%% CI %6.4f to %6.4f)"
      ),
      estimate, 100 * (1 - alpha), L, U
    )
  }

  return(
    contingencytables_result(
      list("lower" = L, "upper" = U, "estimate" = estimate), printresults
    )
  )
}
