


#' Small dataset for use in \pkg{benvo} examples and vignettes.
#'
#' @name FFR_subjects
#'
#' @format  A data frame with 1000 rows and 3 columns
#' \describe{
#' \item{\code{id}}{:The subject unique identifier}
#' \item{\code{sex}}{:The measurement unique identifier}
#' \item{\code{BMI}}{:The Built Environment Unique identifier}
#' }
#' 
#' 
"FFR_subjects"


#' Small dataset for use in \pkg{benvo} examples and vignettes.
#'
#' @name FFR_distances
#' @format  A data frame with 9501 rows and 2 columns
#' \describe{
#' \item{ \code{id}}{: The subject unique identifier}
#' \item{\code{Distance}}{:The simulated distance between a hypothetical subject and fast food restaurant.}
#' }
#' 
#' 
"FFR_distances"


#' Small benvo for use in \pkg{benvo} examples and vignettes.
#'
#' @name example_benvo 
#'
#' @format  A benvo with 1000 subjects and nearby simulated FFRs
#' \describe{
#' \item{\code{FFR_subjects }}{:see FFR_subjects dataset}
#' \item{\code{FFR_distances}}{:see FFR_distances dataset}
#' }
#' 
#' 
"example_benvo"

#' Longitudinal Dataset for use in \pkg{benvo} examples and vignettes.
#'
#' @name HFS_subjects
#' 
#' @format A data frame with 596 rows and 4 columns
#' \describe{
#' \item{\code{id}}{:The subject unique identifier}
#' \item{\code{measurement}}{:The subject repeat measurement id}
#' \item{\code{sex}}{:The measurement unique identifier}
#' \item{\code{BMI}}{:The Built Environment Unique identifier}
#' \item{\code{exposure}}{:The hypothetical Healthy Food Store exposure effect }
#' }
"HFS_subjects"


#' Small dataset for use in \pkg{benvo} examples and vignettes.
#'
#' @name HFS_distances_times 
#' @format  A data frame with 5709 rows and 3 columns
#' \describe{
#' \item{ \code{id}}{: The subject unique identifier}
#' \item{\code{measurement}}{:The subject repeat measurement id}
#' \item{\code{Distance}}{:The simulated distance between a hypothetical subject and fast food restaurant.}
#' \item{\code{Time}}{:The simulated time between a hypothetical subject and fast food restaurant.}
#' }
#' 
#' 
"HFS_distances_times"

#' Small benvo for use in \pkg{benvo} longitudinal examples and vignettes.
#'
#' @name longitudinal_HFS 
#'
#' @details A hypothetical example showing how exposure to Healthy Food Stores (HFS) over time may decrease BMI
#'
#' @format  A benvo with 1000 subjects and nearby simulated FFRs
#' \describe{
#' \item{\code{HFS_subjects }}{:see HFS_subjects dataset}
#' \item{\code{HFS_subjects}}{:see HFS_distances dataset}
#' }
#' 
"longitudinal_HFS"
