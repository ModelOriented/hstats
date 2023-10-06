
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

