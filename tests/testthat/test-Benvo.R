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

  expect_error(new("Benvo",
                    subject_data=FFR_subjects %>% dplyr::select(-id),
                    bef_data=list(FFR=FFR_distances),
                   bef_names = "FFR",components="Distance",
                    longitudinal=F),regexp = "id")

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
  expect_silent(new("Benvo",
                    subject_data=FFR_subjects,
                    bef_data=list(FFR=FFR_distances),
                    longitudinal=F,id=c("id"),
                    bef_names= "FFR",components="Distance"))
})


test_that("Benvo correctly identifies id",{
  expect_equal(c("id","measurement"),benvo(subject_data = HFS_subjects,
                                           bef_data = list(HFS=HFS_distances_times))@id)
  expect_equal(c("cid"),benvo(subject_data = data.frame(cid = c("a","b"),BMI = rnorm(2)),
                              bef_data = list(data.frame(cid=c("a","a","b","b"),Distance=rnorm(4))))@id)
  expect_warning(benvo(subject_data = data.frame(cid = factor(c("a","b")),
                                                 BMI = rnorm(2)),
                              bef_data = list(data.frame(cid=factor(c("a","a","b","b")),
                                                         Distance=rnorm(4))))@id,"data column")
})

test_that("Benvo simple methods work",{
  expect_output(print(example_benvo),regexp = "Observations:")
  expect_output(print(longitudinal_HFS),regexp = "Subjects:")
  expect_invisible(bef_summary(example_benvo))
  expect_invisible(bef_summary(longitudinal_HFS))

})
