test_that("joinvo works", {
  expect_silent(joinvo(longitudinal_HFS,"HFS","Time"))
  expect_silent(joinvo(FFbenvo,"FFR","Distance"))
})
