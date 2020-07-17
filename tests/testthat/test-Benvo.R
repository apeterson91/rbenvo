test_that("Benvo rejects inappropriate structures", {
  expect_error(benvo(subject_data = data.frame(a=1,b=2),
                     bef_data =list(data.frame(c=1,d=2)),
                     bef_names="FFR",joining_id="b",
                     distance_col = "d",
                     exposed_time_col = NA))
  expect_error(benvo(subject_data = data.frame(a=1,b=2),
                     bef_data =list(data.frame(c=1,d=2)),
                     bef_names="FFR",joining_id="b",
                     distance_col = "d",
                     exposed_time_col = c(NA,NA)))
  expect_error(benvo(subject_data = data.frame(a=1,b=2),
                     bef_data =list(data.frame(c=1,b=2)),
                     bef_names="FFR",joining_id="b",
                     distance_col = "d",
                     exposed_time_col = c(NA,NA)))
  expect_error(benvo(subject_data = data.frame(a=1,b=2),
                     bef_data =list(data.frame(c=1,b=2),data.frame(foo=1,bar=2,b=1)),
                     bef_names="FFR",joining_id="b",
                     distance_col = "d",
                     exposed_time_col = NA),"There must be a BEF")

  expect_error(benvo(subject_data = data.frame(a=1,b=2),
                     bef_data =list(data.frame(c=1,b=2)),
                     bef_names="FFR",joining_id="b",
                     distance_col = "d",
                     exposed_time_col = c(NA,"c")),regexp = "There must be an element")
  expect_error(benvo(subject_data = data.frame(a=1,b=2,c=1,e=3),
                     bef_data =list(data.frame(c=1,b=2,a=1,e=3)),
                     bef_names="FFR",joining_id=c("b","c","e"),
                     distance_col = "d",
                     exposed_time_col = c(NA,NA)),regexp = "joining ID")
  expect_error(new("Benvo",
                    subject_data=FFR_subjects %>% dplyr::select(-id),
                    bef_data=list(FFR_distances),
                    longitudinal=F,
                    bef_names= "FFR",
                    components="Distance"))

})

test_that("Benvo accepts appropriate structures",{
  expect_silent(benvo(subject_data = FFR_subjects,
                      bef_data = list(FFR_distances),
                      bef_names = "FFR",
                      joining_id = "id",
                      distance_col = "Distance"))
  expect_silent(benvo(subject_data = FFR_subjects,
                      bef_data = list(FFR_distances),
                      bef_names = "Fast Food",
                      joining_id = "id",
                      distance_col = "Distance",exposed_time_col = NA))
  expect_silent(benvo(subject_data = HFS_subjects,
                      bef_data = list(HFS_distances_times),
                      bef_names = "HFS",
                      distance_col = "Distance",
                      exposed_time_col = "Time",joining_id = "id"))
  expect_silent(new("Benvo",
                    subject_data=FFR_subjects %>% dplyr::rename(ID=id),
                    bef_data=list(FFR_distances %>% dplyr::rename(ID=id)),
                    longitudinal=F,
                    bef_names= "FFR",
                    components="Distance"))
})
