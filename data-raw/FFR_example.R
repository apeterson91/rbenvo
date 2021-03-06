## code to prepare `FFR_subjects` and `FFR_distances` datasets

set.seed(3431)
num_subj <- 1E3
Z <- rbinom(num_subj,1,.5)
has_exp <- rbinom(num_subj,size = 1,prob = .95)
cnt <- rpois(num_subj,10)*has_exp
ldists <- lapply(cnt,function(x) runif(x) )
f <- function(x) 3*pweibull(x,shape=5,scale=.6,lower.tail = F)
exposure <- sapply(ldists,function(x) sum(f(x)))


y <- 26 + Z * -2.2 +  exposure  +  rnorm(num_subj)



FFR_subjects <- dplyr::tibble(id=1:num_subj,
                              BMI = y,
                              sex = Z)

FFR_distances <- purrr::map2_dfr(1:length(ldists),ldists,function(x,y) dplyr::tibble(id=x,Distance=y))

usethis::use_data(FFR_subjects, overwrite = TRUE)
usethis::use_data(FFR_distances, overwrite = TRUE)

FFbenvo <- rbenvo::benvo(subject_data = FFR_subjects,
                         sub_bef_data = list(FFR=FFR_distances))

usethis::use_data(FFbenvo, overwrite = TRUE)
