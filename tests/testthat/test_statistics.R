
test_that("poor_man_stack() works (test could be improved", {
  y <- c("a", "b", "c")
  z <- c("aa", "bb", "cc")
  X <- data.frame(x = 1:3, y = y, z = z)
  out <- poor_man_stack(X, to_stack = c("y", "z"))
  xpected <- data.frame(
    x = rep(1:3, times = 2L), 
    varying_ = factor(rep(c("y", "z"), each = 3L)),
    value_ = c(y, z)
  )
  expect_equal(out, xpected)
  
  expect_error(poor_man_stack(cbind(a = 1:3, b = 2:4), to_stack = "b"))
})

test_that("mat2df() works (test could be improved)", {
  mat <- cbind(y = 1:2, z = c(0.5, 0.5))
  rownames(mat) <- letters[seq_len(nrow(mat))]
  out <- mat2df(mat)
  rownames(out) <- NULL
  xpected <- data.frame(
    id_ = "Overall", 
    variable_ = factor(c("a", "b", "a", "b")),
    varying_ = factor(c("y", "y", "z", "z")),
    value_ = c(1, 2, 0.5, 0.5),
    stringsAsFactors = FALSE
  )
  expect_equal(out, xpected)
  
  mat_no_names <- mat
  colnames(mat_no_names) <- NULL
  expect_equal(unique(mat2df(mat_no_names)$varying_), factor(c("y1", "y2")))
  
  expect_error(mat2df(head(iris)))
  expect_error(mat2df(1:4))
})

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
imp <- perm_importance(
  fit, CO2, v = c("Type", "Treatment", "conc"), y = "uptake", verbose = FALSE
)

test_that("print() method does not give error", {
  capture_output(expect_no_error(print(s)))
  capture_output(expect_no_error(print(s)))
})

test_that("dim() is correct", {
  expect_equal(dim(s), c(3L, 2L))
  expect_equal(dim(imp), c(3L, 2L))
})

test_that("dimnames() is correct", {
  expect_equal(dimnames(s), list(rownames(s$M), colnames(s$M)))
  expect_equal(dimnames(imp), list(rownames(imp$SE), colnames(imp$SE)))
})

test_that("dimnames() (replacement) works", {
  s2 <- s
  colnames(s2) <- c("y", "x")
  rownames(s2) <- c("A", "B", "C")
  expect_equal(colnames(s2), c("y", "x"))
  expect_equal(rownames(s2), c("A", "B", "C"))
  
  imp2 <- imp
  dimnames(imp2) <- list(c("A", "B", "C"), c("y", "x"))
  expect_equal(colnames(imp2), c("y", "x"))
  expect_equal(rownames(imp2), c("A", "B", "C"))
})

test_that("subsetting works", {
  expect_equal(dim(s[, "up2"]), c(3L, 1L))
  expect_equal(dim(s[1, "up2"]), c(1L, 1L))
  expect_equal(dim(s[1:2, ]), c(2L, 2L))
})

fit <- lm(uptake ~ Type * Treatment * conc, data = CO2)
set.seed(1L)
s <- perm_importance(fit, X = CO2[2:4], y = CO2$uptake, verbose = FALSE)

test_that("print() method does not give error", {
  capture_output(expect_no_error(print(s)))
})

test_that("dim() is correct", {
  expect_equal(dim(s), c(3L, 1L))
})

test_that("rownames() is correct", {
  expect_equal(rownames(s[1L, ]), rownames(s$M[1L, , drop = FALSE]))
})
