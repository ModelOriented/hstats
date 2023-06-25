test_that("align_pred() works", {
  expect_error(align_pred("a"))
  
  x <- array(1:4, dim = c(2L, 2L))
  
  expect_equal(align_pred(x), cbind(1:2, 3:4))
  expect_equal(align_pred(1:4), cbind(1:4))
})

test_that("rowmean() works for vector input", {
  x <- 6:1
  out <- rowmean(x, ngroups = 2L)
  expect_true(is.matrix(out))
  expect_equal(as.vector(out), c(5, 2))
  expect_equal(as.vector(rowmean(x, ngroups = 3L)), c(5.5, 3.5, 1.5))
  
  # Constant weights have no effect
  expect_equal(rowmean(x, ngroups = 2L, w = c(1, 1, 1)), out)
  expect_equal(rowmean(x, ngroups = 2L, w = c(4, 4, 4)), out)
  
  # Non-constant weights
  a <- weighted.mean(6:4, 1:3)
  b <- weighted.mean(3:1, 1:3)
  expect_equal(as.vector(rowmean(x, ngroups = 2L, w = 1:3)), c(a, b))
})

test_that("rowmean() works for matrix input", {
  x <- cbind(x = 6:1, z = 1:6)
  out <- rowmean(x, ngroups = 2L)
  expect_true(is.matrix(out))
  expect_equal(out, cbind(x = c(5, 2), z = c(2, 5)))
  expect_equal(
    rowmean(x, ngroups = 3L), 
    cbind(x = c(5.5, 3.5, 1.5), z = c(1.5, 3.5, 5.5))
  )
  
  # Constant weights have no effect
  expect_equal(rowmean(x, ngroups = 2L, w = c(1, 1, 1)), out)
  expect_equal(rowmean(x, ngroups = 2L, w = c(4, 4, 4)), out)
  
  # Non-constant weights
  xpected <- cbind(
    x = c(weighted.mean(6:4, 1:3), weighted.mean(3:1, 1:3)),
    z = c(weighted.mean(1:3, 1:3), weighted.mean(4:6, 1:3))
  )
  
  expect_equal(rowmean(x, ngroups = 2L, w = 1:3), xpected)
})