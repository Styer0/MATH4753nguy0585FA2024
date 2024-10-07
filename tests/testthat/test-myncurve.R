test_that("myncurve returns correct mu", {
  l <- myncurve(mu = 5, sigma = 10, a = 3)
  expect_equal(l$mu, 5)  # Test that the mu component is correct
})

test_that("myncurve returns correct sigma", {
  l <- myncurve(mu = 5, sigma = 10, a = 3)
  expect_equal(l$sigma, 10)  # Test that the sigma component is correct
})

test_that("myncurve returns correct area", {
  l <- myncurve(mu = 5, sigma = 10, a = 3)
  expected_area <- pnorm(3, mean = 5, sd = 10)  # Calculate the expected area
  expect_equal(l$area, expected_area)  # Test that the area component is correct
})
