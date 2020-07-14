M <- matrix(1,nrow(joinvo(longitudinal_HFS,"HFS")),ncol=1)
test_that("Aggrenvo works", {
  expect_equivalent(11,aggrenvo(longitudinal_HFS,M,"HFS","Distance")[1,1])
  expect_error(aggrenvo(longitudinal_HFS,M[1:5,],"HFS","Distance"))
})

M_ <- aggrenvo(longitudinal_HFS,M,"HFS","Distance")
test_that("bw_construction works",{
  expect_equivalent(10,bwinvo(longitudinal_HFS,M_)$between[1,1])
})
