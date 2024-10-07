test_that("ntickets return correct nd", {
  l <- ntickets(N = 200, gamma = 0.02, p = 0.95)
  expect_equal(l$nd, 205)  # Test that the mu component is correct
})

test_that("ntickets return correct nc", {
  l <- ntickets(N = 200, gamma = 0.02, p = 0.95)
  expect_equal(l$nc, 204.31784)  # Test that the mu component is correct
})

test_that("ntickets return correct N", {
  l <- ntickets(N = 200, gamma = 0.02, p = 0.95)
  expect_equal(l$N, 200)  # Test that the mu component is correct
})

test_that("ntickets return correct gamma", {
  l <- ntickets(N = 200, gamma = 0.02, p = 0.95)
  expect_equal(l$gamma, 0.02)  # Test that the mu component is correct
})

test_that("ntickets return correct p", {
  l <- ntickets(N = 200, gamma = 0.02, p = 0.95)
  expect_equal(l$p, 0.95)  # Test that the mu component is correct
})
