test_that("Benvo rejects inappropriate structures", {
  ## No common ID
  expect_error(benvo(subject_data = data.frame(a=1,b=2),
                     bef_data =list(FFR=data.frame(c=1,d=2))),regexp = "common")
  ## No BEF data
  expect_error(benvo(subject_data = data.frame(a=1,b=2),
                     bef_data = list()))
  ## wrong subject data type
  expect_error(benvo(subject_data = 7,
                     bef_data =list(data.frame(c=1,b=2))))

})

test_that("Benvo accepts appropriate structures",{
  expect_silent(benvo(subject_data = FFR_subjects,
                      bef_data = list(FFR=FFR_distances)))
  expect_message(benvo(subject_data = FFR_subjects,
                      bef_data = list(FFR_distances),by = "id"),regexp = "generic names")
  expect_silent(benvo(subject_data = FFR_subjects,
                       bef_data = list(FFR=FFR_distances),by = "id"))
  expect_silent(benvo(subject_data = HFS_subjects,
                      bef_data = list(HFS=HFS_distances_times)))
  expect_silent(benvo(subject_data = HFS_subjects,
                      bef_data = list(HFS=HFS_distances_times),by=c("id","measurement")))
})


test_that("Benvo correctly identifies id",{
  expect_equal(c("id","measurement"),attr(benvo(subject_data = HFS_subjects,
                                           bef_data = list(HFS=HFS_distances_times)),'id'))
  expect_equal("cid",attr(benvo(subject_data = data.frame(cid = c("a","b"),
                                                             BMI = rnorm(2)),
                              bef_data = list(data.frame(cid=c("a","a","b","b"),
                                                         Distance=rnorm(4)))),
                             "id"))
  expect_warning(benvo(subject_data = data.frame(cid = factor(c("a","b")),
                                                 BMI = rnorm(2)),
                              bef_data = list(data.frame(cid=factor(c("a","a","b","b")),
                                                         Distance=rnorm(4)))),"data column")
})

test_that("Benvo simple methods work",{
  expect_output(print(FFbenvo),"tibble")
  expect_output(summary(FFbenvo),"Observations")
  expect_output(summary(longitudinal_HFS),
                regexp = "Subjects:")
  expect_invisible(summary(FFbenvo))
  expect_invisible(summary(longitudinal_HFS))

})
