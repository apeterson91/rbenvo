## code to prepare `longitudinal_HFS` dataset goes here


set.seed(3431)
num_subj <- 3E2
num_visits <- rpois(num_subj,.9)+1
visit_num <- purrr::map_dfr(1:num_subj,function(x) dplyr::tibble(id = x,measurement = 1:num_visits[x]))
num_obs <- sum(num_visits)
Z <- rbinom(num_subj,1,.5)
sjdf <- dplyr::tibble(id=1:num_subj,
                      sex = Z,
                      subj_effect = rnorm(num_subj,mean = 0,sd = 1.2))
has_exp <- rbinom(num_obs,size = 1,prob = .95)
cnt <- rpois(num_obs,10)*has_exp
ldists <- lapply(cnt,function(x) runif(x) )
ltime <- lapply(cnt,function(x) 5*runif(x) )
f <- function(x,y) -.1*pweibull(x,shape=5,scale=.6,lower.tail = F)*pweibull(y,shape=1,scale=1.3,lower.tail=T)
exposure <- purrr::map2_dbl(ldists,ltime,function(x,y) sum(f(x,y)))
visit_num <- visit_num %>% dplyr::mutate(exposure  = exposure)

sjdf <- dplyr::left_join(sjdf,visit_num)


sjdf$BMI <- 26 +  sjdf$sex* -2.2 +  sjdf$exposure  + sjdf$subj_effect + rnorm(num_obs)


HFS_subjects <- sjdf

HFS_distances_times <-purrr::map_dfr(1:length(ldists),function(x) dplyr::tibble(ix=x,Distance=ldists[[x]],Time=ltime[[x]])) %>%
  dplyr::right_join(visit_num %>% dplyr::mutate(ix=1:dplyr::n())) %>%
  dplyr::filter(!is.na(Distance)) %>%
  dplyr::select(id,measurement,Distance,Time)

longitudinal_HFS <- benvo(subject_data = sjdf,
                          bef_data = list(HFS_distances_times),
                          bef_names = "HFS",
                          distance_col = "Distance",
                          joining_id = c("id","measurement"),
                          exposed_time_col = "Time")

usethis::use_data(HFS_subjects, overwrite = TRUE)
usethis::use_data(HFS_distances_times, overwrite = TRUE)
usethis::use_data(longitudinal_HFS, overwrite = TRUE)
