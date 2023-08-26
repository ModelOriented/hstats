test_that("losses are identical to stats package for specific case", {
  y <- 1:10
  p <- y^2
  expect_equal(loss_squared_error(y, p), gaussian()$dev.resid(y, p, 1))
  expect_equal(loss_gamma(y, p), Gamma()$dev.resid(y, p, 1))
  
  y <- 0:1
  p <- c(0.1, 0.9)
  expect_equal(loss_poisson(y, p), poisson()$dev.resid(y, p, 1))
  expect_equal(loss_logloss(y, p), binomial()$dev.resid(y, p, 1) / 2)
})

test_that("loss_absolute_error() works for specific case", {
  y <- 1:10
  p <- y^2
  expect_equal(loss_absolute_error(y, p), abs(y - p))
})

test_that("loss_mlogloss() is consistent with loss_logloss()", {
  y <- 0:1
  p <- c(0.1, 0.8)
  expect_equal(loss_logloss(y, p), loss_mlogloss(cbind(y, 1 - y), cbind(p, 1 - p)))
})

test_that("The 0/0 case is okay with Poisson and logloss", {
  y <- 0:1
  p <- 0:1
  expect_equal(loss_poisson(y, p), c(0, 0))
  expect_equal(loss_logloss(y, p), c(0, 0))
  expect_equal(loss_mlogloss(cbind(y, 1 - y), cbind(p, 1 - p)), c(0, 0))
})

test_that("Some errors are thrown", {
  expect_error(loss_poisson(-1:1, 1:2))
  expect_error(loss_poisson(0:1, -1:0))
  expect_error(loss_gamma(0:1, 1:2))
  expect_error(loss_gamma(1:2, -1:0))
  expect_error(loss_logloss(-1:0, 0:1))
  expect_error(loss_logloss(0:1, -1:0))
  expect_error(loss_mlogloss(cbind(-1:0), 0:1))
  expect_error(loss_mlogloss(0:1, cbind(-1:0)))
  
  expect_error(check_dim(cbind(1:3), cbind(1:3, 1:3)))
})

test_that("xlogy is ok", {
  expect_equal(xlogy(1:3, 1:3), (1:3) * log(1:3))
  expect_equal(xlogy(0:2, 0:2), c(0, 0, 2 * log(2)))
  expect_equal(
    xlogy(cbind(1:3, 0:2), cbind(1:3, 0:2)), 
    cbind(xlogy(1:3, 1:3), xlogy(0:2, 0:2))
  )
})

test_that("get_loss_fun() works", {
  expect_error(get_loss_fun("no_loss"))
  expect_equal(get_loss_fun("poisson"), loss_poisson)
})
