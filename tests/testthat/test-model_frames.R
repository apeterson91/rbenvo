test_that("model frames evaluate correctly", {
  expect_type(subject_design(FFbenvo,BMI ~ sex),"list")
  expect_equivalent(FFbenvo$subject_data$BMI,subject_design(FFbenvo,BMI ~ sex)$y) ## because already sorted
  expect_type(longitudinal_design(longitudinal_HFS,BMI ~ sex + (1|id)),"list")
  expect_equivalent(longitudinal_HFS$subject_data$BMI,longitudinal_design(longitudinal_HFS,BMI ~ sex + (1|id))$y)
  expect_type(longitudinal_design(longitudinal_HFS,BMI ~ sex + (measurement|id)),"list")
})
