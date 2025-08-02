#' Create the plot for the Domain Expertise Collector `Shiny` app
#'
#' This function is exported only for its use in the `Shiny` app. Use
#' `collect()` to start the app.
#'
#' @param range A single string, representing the range of possible values the
#' parameter can take: '1' = '0 to 1', '2' = '0 to Inf', '3' = '-Inf to Inf'
#' @param lq A single number between 0 and 1, representing the quartile which
#' should have the value of the lower threshold
#' @param uq A single number between 0 and 1, representing the quartile which
#' should have the value of the upper threshold
#' @param lt A single number between 0 and 1, representing the lower threshold
#' @param ut A single number between 0 and 1, representing the upper threshold
#' @param start A list with 4 elements named 'shape', 'rate', 'meanlog', and
#' 'sdlog', containing starting values for the `fitdistrplus` functions for
#' Gamma and Log-Normal
#' @param type01 A single string, representing the shape for distributions in
#' range '0 to 1' with one of the thresholds equal to 0 or 1: '1' = 'Beta',
#' '2' = 'Half-Normal'
#' @param type0inf A single string, representing the shape for distributions in
#' range '0 to Inf': '1' = 'Gamma', '2' = 'Log-Normal', '3' = 'Truncated Normal'
#' @param meanbeta A single number between 0 and 1, representing the mean of the
#' Beta distribution when for a parameter in range '0 to 1', both thresholds are
#' 0 and 1
#'
#' @returns A plot
#' @export
#'
#' @examples
#' plot_prior("1", 0.01, 0.99, 0.3, 0.7)
plot_prior <- function(range, lq, uq, lt, ut, start = NULL,
                       type01 = "1", type0inf = "1", meanbeta = 0.5) {
  # Check inputs
  checkmate::assert_string(range,
    na.ok = FALSE, null.ok = FALSE, n.chars = 1
  )
  checkmate::assert_subset(range,
    empty.ok = FALSE,
    choices = c("1", "2", "3")
  )
  if (!checkmate::test_number(lq,
    lower = 0, upper = 1,
    na.ok = FALSE, null.ok = FALSE
  )) {
    return(epicprior::plot_empty_prior())
  }
  if (!checkmate::test_number(uq,
    lower = 0, upper = 1,
    na.ok = FALSE, null.ok = FALSE
  )) {
    return(epicprior::plot_empty_prior())
  }
  if (lq >= uq) {
    return(epicprior::plot_empty_prior())
  }
  if (!checkmate::test_number(lt,
    na.ok = FALSE, null.ok = FALSE
  )) {
    return(epicprior::plot_empty_prior())
  }
  if (!checkmate::test_number(ut,
    na.ok = FALSE, null.ok = FALSE
  )) {
    return(epicprior::plot_empty_prior())
  }
  if (lt >= ut) {
    return(epicprior::plot_empty_prior())
  }
  if (!checkmate::test_number(meanbeta,
    lower = 0, upper = 1,
    na.ok = FALSE, null.ok = TRUE
  )) {
    return(epicprior::plot_empty_prior())
  }
  checkmate::assert_string(type01,
    na.ok = FALSE, null.ok = FALSE, n.chars = 1
  )
  checkmate::assert_subset(type01,
    empty.ok = FALSE,
    choices = c("1", "2")
  )
  checkmate::assert_string(type0inf,
    na.ok = FALSE, null.ok = FALSE, n.chars = 1
  )
  checkmate::assert_subset(type0inf,
    empty.ok = FALSE,
    choices = c("1", "2", "3")
  )

  # lt == 0 does not work for lognormal
  if (type0inf == "2" && lt == 0) {
    return(epicprior::plot_empty_prior())
  }

  # Get params (try returns blank graph when starting value errors occur)
  par <- rlang::try_fetch({
    epicprior::get_par(
      range = range, lq = lq, uq = uq, lt = lt, ut = ut, start = start,
      type01 = type01, type0inf = type0inf, meanbeta = meanbeta
    )
  }, error = function(cnd) {
    list(dist = "empty", param = NULL)
  })


  # Plot
  if (identical(par$dist, "beta")) {
    p <- epicprior::plot_beta_prior(
      shape1 = par$param$shape1,
      shape2 = par$param$shape2,
      lq = lq,
      uq = uq
    )
  }

  if (identical(par$dist, "halfnormal")) {
    if (identical(range, "1")) {
      p <- epicprior::plot_halfnormal_prior(
        mean = par$param$mean,
        sd = par$param$sd,
        lq = lq,
        uq = uq,
        zero_one = TRUE
      )
    } else {
      p <- epicprior::plot_halfnormal_prior(
        mean = par$param$mean,
        sd = par$param$sd,
        lq = lq,
        uq = uq,
        zero_one = FALSE
      )
    }
  }

  if (identical(par$dist, "normal")) {
    p <- epicprior::plot_normal_prior(
      mean = par$param$mean,
      sd = par$param$sd,
      lq = lq,
      uq = uq
    )
  }

  if (identical(par$dist, "lognormal")) {
    p <- epicprior::plot_lognormal_prior(
      meanlog = par$param$meanlog,
      sdlog = par$param$sdlog,
      lq = lq,
      uq = uq
    )
  }

  if (identical(par$dist, "gamma")) {
    p <- epicprior::plot_gamma_prior(
      shape = par$param$shape,
      rate = par$param$rate,
      lq = lq,
      uq = uq
    )
  }

  if (identical(par$dist, "empty")) {
    p <- epicprior::plot_empty_prior()
  }

  # Return
  p
}

#' Plot Beta distributions
#'
#' This function is exported only for its use in the `Shiny` app. Use
#' `collect()` to start the app.
#'
#' @param shape1 A single postive number, representing parameter shape1 of the
#' Beta distribution
#' @param shape2 A single postive number, representing parameter shape1 of the
#' Beta distribution
#' @param lq A single number between 0 and 1, representing the quartile which
#' should have the value of the lower threshold
#' @param uq A single number between 0 and 1, representing the quartile which
#' should have the value of the upper threshold
#'
#' @returns A plot
#'
#' @export
#'
#' @examples
#' plot_beta_prior(2, 2, lq = 0.01, uq = 0.99)
plot_beta_prior <- function(shape1, shape2, lq = 0.01, uq = 0.99) {
  # Check inputs
  if (!checkmate::test_number(shape1,
    lower = 0,
    na.ok = FALSE, null.ok = FALSE
  )) {
    return(epicprior::plot_empty_prior())
  }
  if (!checkmate::test_number(shape2,
    lower = 0,
    na.ok = FALSE, null.ok = FALSE
  )) {
    return(epicprior::plot_empty_prior())
  }

  # Get quantiles
  q <- stats::qbeta(c(lq, uq), shape1, shape2)

  qr <- q %>%
    magrittr::multiply_by(100) %>%
    round(1) %>%
    stringi::stri_c(., "%") %>%
    stringi::stri_trim_both()

  # Plot
  title <- paste0(
    "Beta(",
    round(shape1, 2),
    ", ",
    round(shape2, 2),
    ")"
  )

  p <- ggplot2::ggplot() +
    ggplot2::stat_function(
      fun = stats::dbeta,
      args = list(
        shape1 = shape1,
        shape2 = shape2
      )
    ) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = q[1]), linetype = "dashed") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = q[2]), linetype = "dashed") +
    ggplot2::theme_bw() +
    ggplot2::xlim(0, 1) +
    ggplot2::labs(y = "Density", title = title) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank())

  y_max <- ggplot2::layer_scales(p)$y$range$range[2]

  p +
    ggplot2::geom_label(ggplot2::aes(
      x = q[1], y = y_max / 2,
      label = qr[1], hjust = "right"
    )) +
    ggplot2::geom_label(ggplot2::aes(
      x = q[2], y = y_max / 2,
      label = qr[2], hjust = "left"
    ))
}


#' Plot Half-Normal distributions
#'
#' This function is exported only for its use in the `Shiny` app. Use
#' `collect()` to start the app.
#' @param mean A single number, representing parameter mean of the
#' Normal distribution
#' @param sd A single postive number, representing parameter sd of the
#' Normal distribution
#' @param lq A single number between 0 and 1, representing the quartile which
#' should have the value of the lower threshold
#' @param uq A single number between 0 and 1, representing the quartile which
#' should have the value of the upper threshold
#' @param zero_one A single boolean value, indicating if the plot is used for
#' a distribution in rang '0 to 1' (TRUE) or not (FALSE; default)
#'
#' @returns A plot
#' @export
#'
#' @examples
#' plot_halfnormal_prior(0, 0.2)
plot_halfnormal_prior <- function(mean, sd, lq = 0.01, uq = 0.99,
                                  zero_one = FALSE) {
  # Check inputs
  if (!checkmate::test_number(mean, na.ok = FALSE, null.ok = FALSE)) {
    return(epicprior::plot_empty_prior())
  }
  if (!checkmate::test_number(sd, lower = 0, na.ok = FALSE, null.ok = FALSE)) {
    return(epicprior::plot_empty_prior())
  }
  checkmate::assert_flag(zero_one, na.ok = FALSE, null.ok = FALSE)

  # Get quantiles
  q <- stats::qnorm(c(lq, uq), mean, sd)

  if (zero_one) {
    qr <- q %>%
      magrittr::multiply_by(100) %>%
      round(1) %>%
      stringi::stri_c(., "%") %>%
      stringi::stri_trim_both()
  } else {
    qr <- q %>%
      round(2) %>%
      stringi::stri_trim_both()
  }

  # Plot
  if (zero_one) {
    title <- paste0(
      "Half-Normal(",
      round(mean, 2),
      ", ",
      round(sd, 2),
      ")"
    )
  } else {
    title <- paste0(
      "Truncated Normal(",
      round(mean, 2),
      ", ",
      round(sd, 2),
      ")"
    )
  }

  p <- ggplot2::ggplot() +
    ggplot2::stat_function(
      fun = stats::dnorm,
      args = list(
        mean = mean,
        sd = sd
      )
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(y = "Density", title = title) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank())

  y_max <- ggplot2::layer_scales(p)$y$range$range[2]

  if (zero_one) {
    p <- p +
      ggplot2::xlim(0, 1)
  } else {
    p <- p +
      ggplot2::xlim(0, q[2] * 1.05)
  }

  if (!zero_one || !identical(mean, 0)) {
    p <- p +
      ggplot2::geom_vline(ggplot2::aes(
        xintercept = q[1]
      ), linetype = "dashed") +
      ggplot2::geom_label(ggplot2::aes(
        x = q[1], y = y_max / 2,
        label = qr[1], hjust = "right"
      ))
  }

  if (!zero_one || !identical(mean, 1)) {
    p <- p +
      ggplot2::geom_vline(ggplot2::aes(
        xintercept = q[2]
      ), linetype = "dashed") +
      ggplot2::geom_label(ggplot2::aes(
        x = q[2], y = y_max / 2,
        label = qr[2], hjust = "left"
      ))
  }

  p
}

#' Plot Normal distributions
#'
#' This function is exported only for its use in the `Shiny` app. Use
#' `collect()` to start the app.
#'
#' @param mean A single number, representing parameter mean of the
#' Normal distribution
#' @param sd A single postive number, representing parameter sd of the
#' Normal distribution
#' @param lq A single number between 0 and 1, representing the quartile which
#' should have the value of the lower threshold
#' @param uq A single number between 0 and 1, representing the quartile which
#' should have the value of the upper threshold
#'
#' @returns A plot
#' @export
#'
#' @examples
#' plot_normal_prior(2, 0.5)
plot_normal_prior <- function(mean, sd, lq = 0.01, uq = 0.99) {
  # Check inputs
  if (!checkmate::test_number(mean, na.ok = FALSE, null.ok = FALSE)) {
    return(epicprior::plot_empty_prior())
  }
  if (!checkmate::test_number(sd, lower = 0, na.ok = FALSE, null.ok = FALSE)) {
    return(epicprior::plot_empty_prior())
  }

  # Get quantiles
  q <- stats::qnorm(c(lq, uq), mean, sd)
  q_diff_5perc <- 0.05 * (q[2] - q[1])

  qr <- q %>%
    round(2) %>%
    stringi::stri_trim_both()

  # Plot
  title <- paste0(
    "Normal(",
    round(mean, 2),
    ", ",
    round(sd, 2),
    ")"
  )

  p <- ggplot2::ggplot() +
    ggplot2::stat_function(
      fun = stats::dnorm,
      args = list(
        mean = mean,
        sd = sd
      )
    ) +
    ggplot2::xlim(q[1] - q_diff_5perc, q[2] + q_diff_5perc) +
    ggplot2::theme_bw() +
    ggplot2::labs(y = "Density", title = title) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
    ggplot2::geom_vline(ggplot2::aes(
      xintercept = q[1]
    ), linetype = "dashed") +
    ggplot2::geom_vline(ggplot2::aes(
      xintercept = q[2]
    ), linetype = "dashed")

  y_max <- ggplot2::layer_scales(p)$y$range$range[2]

  p +
    ggplot2::geom_label(ggplot2::aes(
      x = q[1], y = y_max / 2,
      label = qr[1], hjust = "right"
    )) +
    ggplot2::geom_label(ggplot2::aes(
      x = q[2], y = y_max / 2,
      label = qr[2], hjust = "left"
    ))
}

#' Plot Log-Normal distributions
#'
#' This function is exported only for its use in the `Shiny` app. Use
#' `collect()` to start the app.
#'
#' @param meanlog A single number, representing parameter meanlog of the
#' Log-Normal distribution
#' @param sdlog A single postive number, representing parameter sdlog of the
#' Log-Normal distribution
#' @param lq A single number between 0 and 1, representing the quartile which
#' should have the value of the lower threshold
#' @param uq A single number between 0 and 1, representing the quartile which
#' should have the value of the upper threshold
#'
#' @returns A plot
#' @export
#'
#' @examples
#' plot_lognormal_prior(0.5, 0.2)
plot_lognormal_prior <- function(meanlog, sdlog, lq = 0.01, uq = 0.99) {
  # Check inputs
  if (!checkmate::test_number(meanlog, na.ok = FALSE, null.ok = FALSE)) {
    return(epicprior::plot_empty_prior())
  }
  if (!checkmate::test_number(sdlog,
    lower = 0,
    na.ok = FALSE, null.ok = FALSE
  )) {
    return(epicprior::plot_empty_prior())
  }

  # Get quantiles
  q <- stats::qlnorm(c(lq, uq), meanlog, sdlog)

  qr <- q %>%
    round(2) %>%
    stringi::stri_trim_both()

  # Plot
  title <- paste0(
    "Log-Normal(",
    round(meanlog, 2),
    ", ",
    round(sdlog, 2),
    ")"
  )

  p <- ggplot2::ggplot() +
    ggplot2::stat_function(
      fun = stats::dlnorm,
      args = list(
        meanlog = meanlog,
        sdlog = sdlog
      )
    ) +
    ggplot2::xlim(0, q[2] * 1.05) +
    ggplot2::theme_bw() +
    ggplot2::labs(y = "Density", title = title) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
    ggplot2::geom_vline(ggplot2::aes(
      xintercept = q[1]
    ), linetype = "dashed") +
    ggplot2::geom_vline(ggplot2::aes(
      xintercept = q[2]
    ), linetype = "dashed")

  y_max <- ggplot2::layer_scales(p)$y$range$range[2]

  p +
    ggplot2::geom_label(ggplot2::aes(
      x = q[1], y = y_max / 2,
      label = qr[1], hjust = "right"
    )) +
    ggplot2::geom_label(ggplot2::aes(
      x = q[2], y = y_max / 2,
      label = qr[2], hjust = "left"
    ))
}

#' Plot Gamma distributions
#'
#' This function is exported only for its use in the `Shiny` app. Use
#' `collect()` to start the app.
#'
#' @param shape A single postive number, representing parameter shape of the
#' Gamma distribution
#' @param rate A single postive number, representing parameter rate of the
#' Gamma distribution
#' @param lq A single number between 0 and 1, representing the quartile which
#' should have the value of the lower threshold
#' @param uq A single number between 0 and 1, representing the quartile which
#' should have the value of the upper threshold
#'
#' @returns A plot
#' @export
#'
#' @examples
#' plot_gamma_prior(7.3, 5)
plot_gamma_prior <- function(shape, rate, lq = 0.01, uq = 0.99) {
  # Check inputs
  if (!checkmate::test_number(shape,
    lower = 0,
    na.ok = FALSE, null.ok = FALSE
  )) {
    return(epicprior::plot_empty_prior())
  }
  if (!checkmate::test_number(rate,
    lower = 0,
    na.ok = FALSE, null.ok = FALSE
  )) {
    return(epicprior::plot_empty_prior())
  }

  # Get quantiles
  q <- stats::qgamma(c(lq, uq), shape = shape, rate = rate)

  qr <- q %>%
    round(2) %>%
    stringi::stri_trim_both()

  # Plot
  title <- paste0(
    "Gamma(Shape = ",
    round(shape, 2),
    ", Rate = ",
    round(rate, 2),
    ")"
  )

  p <- ggplot2::ggplot() +
    ggplot2::stat_function(
      fun = stats::dgamma,
      args = list(
        shape = shape,
        rate = rate
      )
    ) +
    ggplot2::xlim(0, q[2] * 1.05) +
    ggplot2::theme_bw() +
    ggplot2::labs(y = "Density", title = title) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
    ggplot2::geom_vline(ggplot2::aes(
      xintercept = q[1]
    ), linetype = "dashed") +
    ggplot2::geom_vline(ggplot2::aes(
      xintercept = q[2]
    ), linetype = "dashed")

  y_max <- ggplot2::layer_scales(p)$y$range$range[2]

  p +
    ggplot2::geom_label(ggplot2::aes(
      x = q[1], y = y_max / 2,
      label = qr[1], hjust = "right"
    )) +
    ggplot2::geom_label(ggplot2::aes(
      x = q[2], y = y_max / 2,
      label = qr[2], hjust = "left"
    ))
}

#' Plot an empty canvas, used when inputs lead to errors
#'
#' This function is exported only for its use in the `Shiny` app. Use
#' `collect()` to start the app.
#'
#' @returns An empty plot
#' @export
#'
#' @examples
#' plot_empty_prior()
plot_empty_prior <- function() {
  return(ggplot2::ggplot() +
    ggplot2::theme_bw())
}
