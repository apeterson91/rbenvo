test_that("accurate plot works", {
  expect_silent(plot(FFbenvo))
  expect_silent(plot(longitudinal_HFS))
  expect_silent(plot(longitudinal_HFS,term="HFS",component="Time"))
})

test_that("inaccurate plot errors correctly",{
  expect_error(plot(FFbenvo,term = "HFS"),regexp = "Term")
  expect_error(plot(FFbenvo,component = "Time"))
})
