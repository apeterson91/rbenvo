test_that("joinvo works", {
  expect_silent(joinvo(longitudinal_HFS,"HFS","Time"))
  expect_silent(joinvo(FFbenvo,"FFR","Distance"))
  expect_silent(joinvo(FFbenvo,"FFR","Distance",NA_to_zero = TRUE))
  expect_equivalent(unique(joinvo(FFbenvo,"FFR","Distance")$id),FFbenvo$subject_data$id)
})
