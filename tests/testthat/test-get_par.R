test_that("get_par works", {
  # get_par_01
  lq <- 0.01
  uq <- 0.99
  lt <- 0.2
  ut <- 0.5

  par <- get_par(range = "1", lq, uq, lt, ut)
  expect_identical(par$dist, "beta")
  expect_identical(qbeta(c(0.01, 0.99),
    shape1 = par$param$shape1,
    shape2 = par$param$shape2
  ) %>%
    round(1), c(lt, ut))

  lt <- 0
  par <- get_par(range = "1", lq, uq, lt, ut, type01 = "1")
  expect_identical(par$dist, "beta")
  expect_identical(qbeta(c(0.01, 0.99),
    shape1 = par$param$shape1,
    shape2 = par$param$shape2
  ) %>%
    round(2), c(lt, ut))

  par <- get_par(range = "1", lq, uq, lt, ut, type01 = "2")
  expect_identical(par$dist, "halfnormal")
  expect_identical(qnorm(c(0.5, 0.99),
    mean = par$param$mean,
    sd = par$param$sd
  ) %>%
    round(2), c(lt, ut))

  lt <- 0.2
  ut <- 1
  par <- get_par(range = "1", lq, uq, lt, ut, type01 = "1")
  expect_identical(par$dist, "beta")
  expect_identical(qbeta(c(0.01, 0.99),
    shape1 = par$param$shape1,
    shape2 = par$param$shape2
  ) %>%
    round(1), c(lt, ut))

  par <- get_par(range = "1", lq, uq, lt, ut, type01 = "2")
  expect_identical(par$dist, "halfnormal")
  expect_identical(qnorm(c(0.01, 0.5),
    mean = par$param$mean,
    sd = par$param$sd
  ) %>%
    round(2), c(lt, ut))

  lt <- 0L
  ut <- 1L
  mb <- 0.3
  par <- get_par(range = "1", lq, uq, lt, ut, meanbeta = mb)
  expect_identical(par$param$shape1 / (par$param$shape1 + par$param$shape2), mb)

  # get_par_0inf
  lq <- 0.01
  uq <- 0.99
  lt <- 1
  ut <- 2
  start <- list(
    shape = 0,
    rate = 0,
    meanlog = 0,
    sdlog = 0
  )

  par <- get_par(range = "2", lq, uq, lt, ut, start = start, type0inf = "1")
  expect_identical(par$dist, "gamma")
  expect_identical(qgamma(c(0.01, 0.99),
    shape = par$param$shape,
    rate = par$param$rate
  ) %>%
    round(2), c(lt, ut))

  par <- get_par(range = "2", lq, uq, lt, ut, start = start, type0inf = "2")
  expect_identical(par$dist, "lognormal")
  expect_identical(qlnorm(c(0.01, 0.99),
    meanlog = par$param$meanlog,
    sdlog = par$param$sdlog
  ) %>%
    round(2), c(lt, ut))

  start <- list(
    shape = 1,
    rate = 1,
    meanlog = 1,
    sdlog = 1
  )
  par <- get_par(range = "2", lq, uq, lt, ut, start = start, type0inf = "1")
  expect_identical(par$dist, "gamma")
  expect_identical(qgamma(c(0.01, 0.99),
    shape = par$param$shape,
    rate = par$param$rate
  ) %>%
    round(2), c(lt, ut))

  par <- get_par(range = "2", lq, uq, lt, ut, start = start, type0inf = "2")
  expect_identical(par$dist, "lognormal")
  expect_identical(qlnorm(c(0.01, 0.99),
    meanlog = par$param$meanlog,
    sdlog = par$param$sdlog
  ) %>%
    round(2), c(lt, ut))

  par <- get_par(range = "2", lq, uq, lt, ut, start = start, type0inf = "3")
  expect_identical(par$dist, "halfnormal")
  expect_identical(qnorm(c(0.01, 0.99),
    mean = par$param$mean,
    sd = par$param$sd
  ) %>%
    round(2), c(lt, ut))

  # get_par_infinf
  lq <- 0.01
  uq <- 0.99
  lt <- 8
  ut <- 24

  par <- get_par(range = "3", lq, uq, lt, ut)
  expect_identical(par$dist, "normal")
  expect_identical(qnorm(c(0.01, 0.99),
    mean = par$param$mean,
    sd = par$param$sd
  ) %>%
    round(2), c(lt, ut))
})

test_that("get_par_01 works", {
  lq <- 0.01
  uq <- 0.99
  lt <- 0.2
  ut <- 0.5

  par <- get_par_01(lq, uq, lt, ut)
  expect_identical(par$dist, "beta")
  expect_identical(qbeta(c(0.01, 0.99),
    shape1 = par$param$shape1,
    shape2 = par$param$shape2
  ) %>%
    round(1), c(lt, ut))

  lt <- 0
  par <- get_par_01(lq, uq, lt, ut, type01 = "1")
  expect_identical(par$dist, "beta")
  expect_identical(qbeta(c(0.01, 0.99),
    shape1 = par$param$shape1,
    shape2 = par$param$shape2
  ) %>%
    round(2), c(lt, ut))

  par <- get_par_01(lq, uq, lt, ut, type01 = "2")
  expect_identical(par$dist, "halfnormal")
  expect_identical(qnorm(c(0.5, 0.99),
    mean = par$param$mean,
    sd = par$param$sd
  ) %>%
    round(2), c(lt, ut))

  lt <- 0.2
  ut <- 1
  par <- get_par_01(lq, uq, lt, ut, type01 = "1")
  expect_identical(par$dist, "beta")
  expect_identical(qbeta(c(0.01, 0.99),
    shape1 = par$param$shape1,
    shape2 = par$param$shape2
  ) %>%
    round(1), c(lt, ut))

  par <- get_par_01(lq, uq, lt, ut, type01 = "2")
  expect_identical(par$dist, "halfnormal")
  expect_identical(qnorm(c(0.01, 0.5),
    mean = par$param$mean,
    sd = par$param$sd
  ) %>%
    round(2), c(lt, ut))

  lt <- 0L
  ut <- 1L
  mb <- 0.3
  par <- get_par_01(lq, uq, lt, ut, meanbeta = mb)
  expect_identical(par$param$shape1 / (par$param$shape1 + par$param$shape2), mb)
})

test_that("get_par_0inf works", {
  lq <- 0.01
  uq <- 0.99
  lt <- 1
  ut <- 2
  start <- list(
    shape = 0,
    rate = 0,
    meanlog = 0,
    sdlog = 0
  )

  par <- get_par_0inf(lq, uq, lt, ut, start = start, type0inf = "1")
  expect_identical(par$dist, "gamma")
  expect_identical(qgamma(c(0.01, 0.99),
    shape = par$param$shape,
    rate = par$param$rate
  ) %>%
    round(2), c(lt, ut))

  par <- get_par_0inf(lq, uq, lt, ut, start = start, type0inf = "2")
  expect_identical(par$dist, "lognormal")
  expect_identical(qlnorm(c(0.01, 0.99),
    meanlog = par$param$meanlog,
    sdlog = par$param$sdlog
  ) %>%
    round(2), c(lt, ut))

  start <- list(
    shape = 1,
    rate = 1,
    meanlog = 1,
    sdlog = 1
  )
  par <- get_par_0inf(lq, uq, lt, ut, start = start, type0inf = "1")
  expect_identical(par$dist, "gamma")
  expect_identical(qgamma(c(0.01, 0.99),
    shape = par$param$shape,
    rate = par$param$rate
  ) %>%
    round(2), c(lt, ut))

  par <- get_par_0inf(lq, uq, lt, ut, start = start, type0inf = "2")
  expect_identical(par$dist, "lognormal")
  expect_identical(qlnorm(c(0.01, 0.99),
    meanlog = par$param$meanlog,
    sdlog = par$param$sdlog
  ) %>%
    round(2), c(lt, ut))

  par <- get_par_0inf(lq, uq, lt, ut, start = start, type0inf = "3")
  expect_identical(par$dist, "halfnormal")
  expect_identical(qnorm(c(0.01, 0.99),
    mean = par$param$mean,
    sd = par$param$sd
  ) %>%
    round(2), c(lt, ut))
})

test_that("get_par_infinf works", {
  lq <- 0.01
  uq <- 0.99
  lt <- 8
  ut <- 24

  par <- get_par_infinf(lq, uq, lt, ut)
  expect_identical(par$dist, "normal")
  expect_identical(qnorm(c(0.01, 0.99),
    mean = par$param$mean,
    sd = par$param$sd
  ) %>%
    round(2), c(lt, ut))
})

test_that("create_seq_from_q works", {
  lt <- 0.4
  ut <- 0.9
  x <- quantile(
    create_seq_from_q(0.01, 0.99, lt, ut), c(0.01, 0.99)
  ) %>%
    magrittr::set_names(NULL)
  expect_identical(round(x, 5), round(c(lt, ut), 5))

  lt <- 0
  ut <- 300
  x <- quantile(
    create_seq_from_q(0.01, 0.99, lt, ut), c(0.01, 0.99)
  ) %>%
    magrittr::set_names(NULL)
  expect_identical(round(x, 5), round(c(lt, ut), 5))

  lt <- 0
  ut <- 1
  x <- quantile(
    create_seq_from_q(0.01, 0.99, lt, ut), c(0.01, 0.99)
  ) %>%
    magrittr::set_names(NULL)
  expect_identical(round(x, 5), round(c(lt, ut), 5))

  lt <- -3242
  ut <- 12
  x <- quantile(
    create_seq_from_q(0.01, 0.99, lt, ut), c(0.01, 0.99)
  ) %>%
    magrittr::set_names(NULL)
  expect_identical(round(x, 5), round(c(lt, ut), 5))
})

test_that("create_start works", {
  start <- list(
    shape = 0,
    rate = 0,
    meanlog = 0,
    sdlog = 0
  )

  expect_null(create_start("gamma", start))
  expect_null(create_start("lognormal", start))

  start <- list(
    shape = 1,
    rate = 1,
    meanlog = 1,
    sdlog = 1
  )

  expect_identical(create_start("gamma", start), list(shape = 1, rate = 1))
  expect_identical(
    create_start("lognormal", start),
    list(meanlog = 1, sdlog = 1)
  )
})
