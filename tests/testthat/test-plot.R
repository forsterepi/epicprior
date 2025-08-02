test_that("plot_prior works", {
  # get_par_01
  lq <- 0.01
  uq <- 0.99
  lt <- 0.2
  ut <- 0.5

  expect_no_error(plot_prior(range = "1", lq, uq, lt, ut))

  lt <- 0
  expect_no_error(plot_prior(range = "1", lq, uq, lt, ut, type01 = "1"))

  expect_no_error(plot_prior(range = "1", lq, uq, lt, ut, type01 = "2"))

  lt <- 0.2
  ut <- 1
  expect_no_error(plot_prior(range = "1", lq, uq, lt, ut, type01 = "1"))
  expect_no_error(plot_prior(range = "1", lq, uq, lt, ut, type01 = "2"))

  lt <- 0L
  ut <- 1L
  mb <- 0.3
  expect_no_error(plot_prior(range = "1", lq, uq, lt, ut, meanbeta = mb))

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

  expect_no_error(plot_prior(
    range = "2", lq, uq, lt, ut,
    start = start, type0inf = "1"
  ))
  expect_no_error(plot_prior(
    range = "2", lq, uq, lt, ut,
    start = start, type0inf = "2"
  ))

  start <- list(
    shape = 1,
    rate = 1,
    meanlog = 1,
    sdlog = 1
  )
  expect_no_error(plot_prior(
    range = "2", lq, uq, lt, ut,
    start = start, type0inf = "1"
  ))
  expect_no_error(plot_prior(
    range = "2", lq, uq, lt, ut,
    start = start, type0inf = "2"
  ))

  expect_no_error(plot_prior(
    range = "2", lq, uq, lt, ut,
    start = start, type0inf = "3"
  ))

  # get_par_infinf
  lq <- 0.01
  uq <- 0.99
  lt <- 8
  ut <- 24

  expect_no_error(plot_prior(range = "3", lq, uq, lt, ut))
})

test_that("plot_beta_prior works", {
  expect_no_error(plot_beta_prior(6.3, 6.3))
})

test_that("plot_halfnormal_prior works", {
  expect_no_error(plot_halfnormal_prior(0, 0.2, zero_one = TRUE))
  expect_no_error(plot_halfnormal_prior(1.1, 0.4, zero_one = FALSE))
})

test_that("plot_normal_prior works", {
  expect_no_error(plot_normal_prior(2, 0.43))
})

test_that("plot_lognormal_prior works", {
  expect_no_error(plot_lognormal_prior(0.35, 0.15))
})

test_that("plot_gamma_prior works", {
  expect_no_error(plot_gamma_prior(45.67, 31.42))
})

test_that("plot_empty_prior works", {
  expect_no_error(plot_empty_prior())
})
