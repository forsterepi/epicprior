#' Derive parameters for the distribution that fits with the specified
#' thresholds
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
#' @returns A list with two elements, named 'dist' and 'param', with 'dist'
#' being a single string with the name of the distribution and 'param' being a
#' named list with one element per parameter
#' @export
#'
#' @examples
#' get_par("1", 0.01, 0.99, 0.3, 0.7)
get_par <- function(range, lq, uq, lt, ut, start = NULL,
                    type01 = "1", type0inf = "1", meanbeta = 0.5) {
  # Get params
  if (identical(range, "1")) {
    out <- epicprior::get_par_01(
      lq = lq, uq = uq, lt = lt, ut = ut,
      type01 = type01, meanbeta = meanbeta
    )
  }

  if (identical(range, "2")) {
    out <- epicprior::get_par_0inf(
      lq = lq, uq = uq, lt = lt, ut = ut, start = start, type0inf = type0inf
    )
  }

  if (identical(range, "3")) {
    out <- epicprior::get_par_infinf(
      lq = lq, uq = uq, lt = lt, ut = ut
    )
  }
  # Return
  out
}

#' Derive parameters for variables in range 0 to 1
#'
#' This function is exported only for its use in the `Shiny` app. Use
#' `collect()` to start the app.
#'
#' @param lq A single number between 0 and 1, representing the quartile which
#' should have the value of the lower threshold
#' @param uq A single number between 0 and 1, representing the quartile which
#' should have the value of the upper threshold
#' @param lt A single number between 0 and 1, representing the lower threshold
#' @param ut A single number between 0 and 1, representing the upper threshold
#' @param type01 A single string, representing the shape for distributions in
#' range '0 to 1' with one of the thresholds equal to 0 or 1: '1' = 'Beta',
#' '2' = 'Half-Normal'
#' @param meanbeta A single number between 0 and 1, representing the mean of the
#' Beta distribution when for a parameter in range '0 to 1', both thresholds are
#' 0 and 1
#'
#' @returns A list with two elements, named 'dist' and 'param', with 'dist'
#' being a single string with the name of the distribution and 'param' being a
#' named list with one element per parameter
#' @export
#'
#' @examples
#' get_par_01(0.01, 0.99, 0.2, 0.4)
get_par_01 <- function(lq, uq, lt, ut, type01 = "1", meanbeta = 0.5) {
  # Check inputs
  checkmate::assert_number(lt,
    lower = 0, upper = 1,
    na.ok = FALSE, null.ok = FALSE
  )
  checkmate::assert_number(ut,
    lower = 0, upper = 1,
    na.ok = FALSE, null.ok = FALSE
  )

  # different version depending on thresholds equal to 0 or 1
  # 2 thresholds
  if (!identical(lt, 0) && !identical(ut, 1) &&
    !identical(lt, 0L) && !identical(ut, 1L)) {
    dist <- "beta"
    param <- fitdistrplus::qmedist(
      data = create_seq_from_q(lq = lq, uq = uq, lt = lt, ut = ut),
      distr = dist,
      prob = c(lq, uq)
    )$estimate %>%
      as.list()

    return(list(dist = dist, param = param))
  }

  # only ut, i.e., lt = 0
  if (identical(lt, 0) && !identical(ut, 1)) {
    if (identical(type01, "1")) {
      dist <- "beta"

      candidate_t <- data.frame(
        shape2 = seq(1, 330, by = 0.1),
        q = stats::qbeta(0.99, shape1 = 0.5, shape2 = seq(1, 330, by = 0.1))
      )
      candidate_t$diff <- abs(candidate_t$q - ut)
      s2 <- candidate_t$shape2[which(candidate_t$diff == min(candidate_t$diff))[1]]

      param <- list(shape1 = 0.5, shape2 = s2)

      return(list(dist = dist, param = param))
    }
    if (identical(type01, "2")) {
      dist <- "halfnormal"
      param <- fitdistrplus::qmedist(
        data = create_seq_from_q(lq = 0.5, uq = uq, lt = 0, ut = ut),
        distr = "norm",
        prob = c(lq, uq)
      )$estimate %>%
        as.list()
      param[["mean"]] <- 0

      return(list(dist = dist, param = param))
    }
  }

  # only lt, i.e., ut = 1
  if (!identical(lt, 0) && identical(ut, 1)) {
    if (identical(type01, "1")) {
      dist <- "beta"

      candidate_t <- data.frame(
        shape1 = seq(1, 330, by = 0.1),
        q = stats::qbeta(0.01, shape1 = seq(1, 330, by = 0.1), shape2 = 0.5)
      )
      candidate_t$diff <- abs(candidate_t$q - lt)
      s1 <- candidate_t$shape1[which(candidate_t$diff == min(candidate_t$diff))[1]]

      param <- list(shape1 = s1, shape2 = 0.5)

      return(list(dist = dist, param = param))
    }
    if (identical(type01, "2")) {
      dist <- "halfnormal"
      param <- fitdistrplus::qmedist(
        data = create_seq_from_q(lq = lq, uq = 0.5, lt = lt, ut = 1),
        distr = "norm",
        prob = c(lq, uq)
      )$estimate %>%
        as.list()
      param[["mean"]] <- 1

      return(list(dist = dist, param = param))
    }
  }

  # 0 thresholds
  if (identical(lt, 0L) && identical(ut, 1L)) {
    # Note: When both values are integerish, i.e., lt is 0 and ut is 1, then they
    # are saved as integers and indentical() needs to compare them to integers.
    # As long as 1 value is double (all other cases), 0 and 1 are doubles, too.
    dist <- "beta"
    param <- list(shape1 = meanbeta * 5, shape2 = 5 - meanbeta * 5)

    return(list(dist = dist, param = param))
  }
}

#' Derive parameters for variables in range 0 to Infinity
#'
#' This function is exported only for its use in the `Shiny` app. Use
#' `collect()` to start the app.
#'
#' @param lq A single number between 0 and 1, representing the quartile which
#' should have the value of the lower threshold
#' @param uq A single number between 0 and 1, representing the quartile which
#' should have the value of the upper threshold
#' @param lt A single number between 0 and 1, representing the lower threshold
#' @param ut A single number between 0 and 1, representing the upper threshold
#' @param start A list with 4 elements named 'shape', 'rate', 'meanlog', and
#' 'sdlog', containing starting values for the `fitdistrplus` functions for
#' Gamma and Log-Normal
#' @param type0inf A single string, representing the shape for distributions in
#' range '0 to Inf': '1' = 'Gamma', '2' = 'Log-Normal', '3' = 'Truncated Normal'
#'
#' @returns A list with two elements, named 'dist' and 'param', with 'dist'
#' being a single string with the name of the distribution and 'param' being a
#' named list with one element per parameter
#' @export
#'
#' @examples
#' get_par_0inf(0.01, 0.99, 0.5, 3,
#'   start = list(
#'     shape = 1,
#'     rate = 1,
#'     meanlog = 1,
#'     sdlog = 1
#'  ))
get_par_0inf <- function(lq, uq, lt, ut, start = NULL, type0inf = "1") {
  # Check inputs
  checkmate::assert_number(lt,
    lower = 0, na.ok = FALSE, null.ok = FALSE
  )
  checkmate::assert_number(ut,
    lower = 0, na.ok = FALSE, null.ok = FALSE
  )

  # Get params
  if (identical(type0inf, "1")) {
    s <- epicprior::create_start("gamma", start)
    dist <- "gamma"
    param <- fitdistrplus::qmedist(
      data = create_seq_from_q(lq = lq, uq = uq, lt = lt, ut = ut),
      distr = dist,
      prob = c(lq, uq),
      start = s
    )$estimate %>%
      as.list()

    return(list(dist = dist, param = param))
  }

  if (identical(type0inf, "2")) {
    s <- epicprior::create_start("lognormal", start)
    param <- fitdistrplus::qmedist(
      data = create_seq_from_q(lq = lq, uq = uq, lt = lt, ut = ut),
      distr = "lnorm",
      prob = c(lq, uq),
      start = s
    )$estimate %>%
      as.list()

    return(list(dist = "lognormal", param = param))
  }

  if (identical(type0inf, "3")) {
    param <- fitdistrplus::qmedist(
      data = create_seq_from_q(lq = lq, uq = uq, lt = lt, ut = ut),
      distr = "norm",
      prob = c(lq, uq)
    )$estimate %>%
      as.list()

    return(list(dist = "halfnormal", param = param))
  }
}

#' Derive parameters for variables in range -Infinity to Infinity
#'
#' This function is exported only for its use in the `Shiny` app. Use
#' `collect()` to start the app.
#'
#' @param lq A single number between 0 and 1, representing the quartile which
#' should have the value of the lower threshold
#' @param uq A single number between 0 and 1, representing the quartile which
#' should have the value of the upper threshold
#' @param lt A single number between 0 and 1, representing the lower threshold
#' @param ut A single number between 0 and 1, representing the upper threshold
#'
#' @returns A list with two elements, named 'dist' and 'param', with 'dist'
#' being a single string with the name of the distribution and 'param' being a
#' named list with one element per parameter
#' @export
#'
#' @examples
#' get_par_infinf(0.01, 0.99, -2, 2)
get_par_infinf <- function(lq, uq, lt, ut) {
  param <- fitdistrplus::qmedist(
    data = create_seq_from_q(lq = lq, uq = uq, lt = lt, ut = ut),
    distr = "norm",
    prob = c(lq, uq)
  )$estimate %>%
    as.list()

  return(list(dist = "normal", param = param))
}

#' Create dummy sequence of with `lt` and `ut` at `lq` and `uq`, for use in
#' `fitdistrplus` functions, which only take data, from which the qunatile
#' are concluded
#'
#' This function is exported only for its use in the `Shiny` app. Use
#' `collect()` to start the app.
#'
#' @param lq A single number between 0 and 1, representing the quartile which
#' should have the value of the lower threshold
#' @param uq A single number between 0 and 1, representing the quartile which
#' should have the value of the upper threshold
#' @param lt A single number between 0 and 1, representing the lower threshold
#' @param ut A single number between 0 and 1, representing the upper threshold
#'
#' @returns A vector of 201 sequential numbers
#' @export
#'
#' @examples
#' create_seq_from_q(0.01, 0.99, 0.2, 0.8)
create_seq_from_q <- function(lq, uq, lt, ut) {
  qdiff <- uq - lq
  nsteps <- qdiff / 0.005

  tdiff <- ut - lt
  tstep <- tdiff / nsteps

  pos_lq <- lq / 0.005

  t0 <- lt - (tstep * pos_lq)
  tend <- t0 + (tstep * 200)

  seq(from = t0, to = tend, by = tstep)
}

#' Process starting value inputs in `Shiny`
#'
#' This function is exported only for its use in the `Shiny` app. Use
#' `collect()` to start the app.
#'
#' @param dist A single string, representing the distribution, either 'gamma'
#' or 'lognormal'
#' @param start A list with 4 elements named 'shape', 'rate', 'meanlog', and
#' 'sdlog', containing starting values for the `fitdistrplus` functions for
#' Gamma and Log-Normal
#'
#' @returns A smaller version of `start` that only contains parameters of the
#' distribution named in `dist`, or `NULL`, if all relevant parameters are 0
#' @export
#'
#' @examples
#' create_start(
#'   dist = "gamma",
#'   start = list(
#'     shape = 1,
#'     rate = 1,
#'     meanlog = 1,
#'     sdlog = 1
#'    )
#'  )
create_start <- function(dist, start = NULL) {
  # Check input
  checkmate::assert_string(dist, na.ok = FALSE, null.ok = FALSE, min.chars = 1)
  checkmate::assert_subset(dist,
    empty.ok = FALSE,
    choices = c("gamma", "lognormal")
  )
  checkmate::assert_list(start,
    types = "numeric", any.missing = FALSE,
    len = 4, names = "unique"
  )
  checkmate::assert_subset(names(start),
    empty.ok = FALSE,
    choices = c("shape", "rate", "meanlog", "sdlog")
  )

  # Cut down to relevant parameters
  if (dist == "gamma") {
    s <- start[c("shape", "rate")]
  }
  if (dist == "lognormal") {
    s <- start[c("meanlog", "sdlog")]
  }

  # Check if all elements are 0L (integer!)
  if (lapply(s, identical, y = 0L) %>% unlist() %>% all()) {
    return(NULL)
  } else if (lapply(s, identical, y = 0) %>% unlist() %>% all()) {
    return(NULL)
  } else {
    return(s)
  }
}

# ## Functions using package rriskDistributions
# get_par_01_rrisk <- function(lq, uq, lt, ut, type01 = "1", meanbeta = 0.5) {
#   # Check inputs
#   checkmate::assert_number(lt,
#     lower = 0, upper = 1,
#     na.ok = FALSE, null.ok = FALSE
#   )
#   checkmate::assert_number(ut,
#     lower = 0, upper = 1,
#     na.ok = FALSE, null.ok = FALSE
#   )
#
#   # different version depending on thresholds equal to 0 or 1
#   # 2 thresholds
#   if (!identical(lt, 0) && !identical(ut, 1) &&
#     !identical(lt, 0L) && !identical(ut, 1L)) {
#     dist <- "beta"
#     param <- rriskDistributions::get.beta.par(
#       p = c(lq, uq),
#       q = c(lt, ut),
#       show.output = FALSE,
#       plot = FALSE
#     ) %>% as.list()
#
#     return(list(dist = dist, param = param))
#   }
#
#   # only ut, i.e., lt = 0
#   if (identical(lt, 0) && !identical(ut, 1)) {
#     if (identical(type01, "1")) {
#       dist <- "beta"
#
#       candidate_t <- data.frame(
#         shape2 = seq(1, 330, by = 0.1),
#         q = stats::qbeta(0.99, shape1 = 0.5, shape2 = seq(1, 330, by = 0.1))
#       )
#       candidate_t$diff <- abs(candidate_t$q - ut)
#       s2 <- candidate_t$shape2[which(candidate_t$diff == min(candidate_t$diff))[1]]
#
#       param <- list(shape1 = 0.5, shape2 = s2)
#
#       return(list(dist = dist, param = param))
#     }
#     if (identical(type01, "2")) {
#       dist <- "halfnormal"
#       param <- rriskDistributions::get.norm.par(
#         p = c(0.5, uq),
#         q = c(0, ut),
#         show.output = FALSE,
#         plot = FALSE
#       ) %>% as.list()
#       param[["mean"]] <- 0
#
#       return(list(dist = dist, param = param))
#     }
#   }
#
#   # only lt, i.e., ut = 1
#   if (!identical(lt, 0) && identical(ut, 1)) {
#     if (identical(type01, "1")) {
#       dist <- "beta"
#
#       candidate_t <- data.frame(
#         shape1 = seq(1, 330, by = 0.1),
#         q = stats::qbeta(0.01, shape1 = seq(1, 330, by = 0.1), shape2 = 0.5)
#       )
#       candidate_t$diff <- abs(candidate_t$q - lt)
#       s1 <- candidate_t$shape1[which(candidate_t$diff == min(candidate_t$diff))[1]]
#
#       param <- list(shape1 = s1, shape2 = 0.5)
#
#       return(list(dist = dist, param = param))
#     }
#     if (identical(type01, "2")) {
#       dist <- "halfnormal"
#       # rriskDistributions::get.norm.par() works fine between 0 and 1
#       param <- rriskDistributions::get.norm.par(
#         p = c(lq, 0.5),
#         q = c(lt, 1),
#         show.output = FALSE,
#         plot = FALSE
#       ) %>% as.list()
#       param[["mean"]] <- 1
#
#       return(list(dist = dist, param = param))
#     }
#   }
#
#   # 0 thresholds
#   if (identical(lt, 0L) && identical(ut, 1L)) {
#     # Note: When both values are integerish, i.e., lt is 0 and ut is 1, then they
#     # are saved as integers and indentical() needs to compare them to integers.
#     # As long as 1 value is double (all other cases), 0 and 1 are doubles, too.
#     dist <- "beta"
#     param <- list(shape1 = meanbeta * 5, shape2 = 5 - meanbeta * 5)
#
#     return(list(dist = dist, param = param))
#   }
# }
#
# get_par_0inf_rrisk <- function(lq, uq, lt, ut, type0inf = "1") {
#   # Check inputs
#   checkmate::assert_number(lt,
#     lower = 0, na.ok = FALSE, null.ok = FALSE
#   )
#   checkmate::assert_number(ut,
#     lower = 0, na.ok = FALSE, null.ok = FALSE
#   )
#
#   # Get params
#   if (identical(type0inf, "1")) {
#     param <- rriskDistributions::get.gamma.par(
#       p = c(lq, uq),
#       q = c(lt, ut),
#       show.output = FALSE,
#       plot = FALSE
#     ) %>% as.list()
#
#     return(list(dist = "gamma", param = param))
#   }
#
#   if (identical(type0inf, "2")) {
#     param <- rriskDistributions::get.lnorm.par(
#       p = c(lq, uq),
#       q = c(lt, ut),
#       show.output = FALSE,
#       plot = FALSE
#     ) %>% as.list()
#
#     return(list(dist = "lognormal", param = param))
#   }
#
#   if (identical(type0inf, "3")) {
#     # rriskDistributions::get.norm.par() has problems with SD > 20
#     # We use get.norm.sd() instead
#     param <- rriskDistributions::get.norm.sd(
#       p = c(lq, uq),
#       q = c(lt, ut),
#       plot = FALSE
#     ) %>% as.list()
#
#     return(list(dist = "halfnormal", param = param))
#   }
# }
#
# get_par_infinf_rrisk <- function(lq, uq, lt, ut) {
#   # rriskDistributions::get.norm.par() has problems with SD > 20
#   # We use get.norm.sd() instead
#   param <- rriskDistributions::get.norm.sd(
#     p = c(lq, uq),
#     q = c(lt, ut),
#     plot = FALSE
#   ) %>% as.list()
#
#   return(list(dist = "normal", param = param))
# }
