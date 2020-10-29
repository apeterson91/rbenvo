test_that("dplyr verbs function", {
  expect_equal(1,FFbenvo %>% select(id) %>% head() %>% ncol())
  expect_equal(2,FFbenvo %>% activate(FFR) %>% select(id,Distance) %>% head() %>% ncol())
  expect_error(FFbenvo %>% activate(FFR) %>% select(id) %>% head() %>% ncol(),regexp="Distance")
  expect_equal(3,FFbenvo %>% mutate(id=id+1) %>% head() %>% ncol())
  expect_equal(1,FFbenvo %>% filter(id==1) %>% head() %>% nrow())
  expect_equal(6,FFbenvo %>% distinct() %>% head() %>% nrow())
  expect_silent(FFbenvo %>% arrange(id))
  expect_error(FFbenvo %>% group_by(id),regexp="not currently implimented")
  expect_error(FFbenvo %>% ungroup(),regexp="not currently implimented")
})

