test_that("activate function appropriately switches df", {
  expect_equal("FFR",FFbenvo %>% activate(FFR) %>% active(.))
  expect_equal("subject",FFbenvo %>%
                 activate(FFR) %>%
                 activate(subject) %>% active(.))
})
