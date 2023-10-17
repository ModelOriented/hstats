test_that("postprocess() works for matrix input", {
  num <- cbind(a = 1:3, b = c(1, 1, 1))
  denom <- cbind(a = 1:3, b = 1:3)
  
  expect_equal(postprocess(num = num, sort = FALSE), num)
  expect_equal(postprocess(num = num, denom = denom, sort = FALSE), num / denom)
  expect_equal(postprocess(num = num), num[3:1, ])
  expect_equal(postprocess(num = num, squared = FALSE), sqrt(num[3:1, ]))
  
  expect_equal(postprocess(num = num, denom = 2, sort = FALSE), num / 2)
  expect_equal(
    postprocess(num = num, denom = 1:2, sort = FALSE), 
    num / cbind(c(1, 1, 1), c(2, 2, 2))
  )
  
  expect_equal(postprocess(num = cbind(0:1, 0:1), zero = FALSE), rbind(c(1, 1)))
  expect_null(postprocess(num = cbind(0, 0), zero = FALSE))
})

test_that("postprocess() works for vector input", {
  num <- 1:3
  denom <- c(2, 4, 6)
  
  expect_equal(postprocess(num = num), 3:1)
  expect_equal(postprocess(num = num, denom = denom), num / denom)
  expect_equal(postprocess(num = num, sort = FALSE), num)
  expect_equal(postprocess(num = num, squared = FALSE), sqrt(num[3:1]))
  
  expect_equal(postprocess(num = 0:1, denom = c(2, 2), zero = FALSE), 0.5)
  expect_null(postprocess(num = 0, zero = FALSE))
})


test_that(".zap_small() works for vector input", {
  expect_equal(.zap_small(1:3), 1:3)
  expect_equal(.zap_small(c(1:3, NA)), c(1:3, 0))
  expect_equal(.zap_small(c(0.001, 1), eps = 0.01), c(0, 1))
})

test_that(".zap_small() works for matrix input", {
  expect_equal(
    .zap_small(cbind(c(0.001, 1), c(0, 0)), eps = 0.01), 
    cbind(c(0, 1), c(0, 0))
  )
})

fit <- lm(cbind(up = uptake, up2 = 2 * uptake) ~ Type * Treatment * conc, data = CO2)
H <- hstats(fit, X = CO2[2:4], verbose = FALSE)
s <- h2_pairwise(H)

test_that("print() method does not give error", {
  capture_output(expect_no_error(print(s)))
})

test_that("dim() is correct", {
  expect_equal(dim(s), c(3L, 2L))
})

test_that("dimnames() is correct", {
  expect_equal(dimnames(s), list(rownames(s$M), colnames(s$M)))
})

test_that("subsetting works", {
  expect_equal(dim(s[, "up2"]), c(3L, 1L))
  expect_equal(dim(s[1, "up2"]), c(1L, 1L))
  expect_equal(dim(s[1:2, ]), c(2L, 2L))
})

fit <- lm(uptake ~ Type * Treatment * conc, data = CO2)
set.seed(1L)
s <- perm_importance(fit, X = CO2[2:4], y = CO2$uptake)

test_that("print() method does not give error", {
  capture_output(expect_no_error(print(s)))
})

test_that("dim() is correct", {
  expect_equal(dim(s), c(3L, 1L))
})

test_that("rownames() is correct", {
  expect_equal(rownames(s[1L, ]), rownames(s$M[1L, , drop = FALSE]))
})
