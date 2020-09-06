test_that("joinvo works", {
  expect_silent(joinvo(longitudinal_HFS,"HFS","Time"))
  expect_silent(joinvo(example_benvo,"FFR","Distance"))
})
