#' @title Benvo data objects
#'
#' @description
#' A custom data structure for handling point pattern built environment data.
#' Consists of two types of dataframes (1) subject and (2) subject - BEF with a joining id(s).
#' Whether using the \code{\link[rbenvo]{benvo}} constructor helper function or the raw
#' \code{\link[rbenvo]{Benvo}} class constructor, certain constraints are maintained within the class.
#' Namely,(1) there must be a bef name and component for each bef data frame.
#' (2) Both bef_data and subject data must inherit
#' from class data.frame. (3) Components must be one of c("Distance","Time","Distance-Time")
#" and (4) There must be a common id column between the subject data and all bef_data data frames that includes a numeric "ID" column.
#'
#'
#' @slot subject_data
#'      data.frame containing subject level covariates.
#' @slot bef_data
#'		list containing subject - bef distance/time data frames
#' @slot longitudinal
#'      boolean value indicating whether the Benvo holds longitudinal data
#' @slot bef_names
#'      character vector containing the names of the BEFs
#' @slot components
#'	    character vector containing the Distances and/or Time data measured for each BEF
#' @slot id
#'		character vector of length 1 or 2 that determines how subjects are joined. Note that
#'      for repeated measures data it is understood that id[1] is the subject id and id[2] is the measurement/visit id
#'
#' @export
#'
Benvo <- setClass("Benvo",
                  slots = list(
                    subject_data = "data.frame",
                    bef_data = "list",
				    longitudinal = "logical",
					bef_names = "character",
					components = "character",
					id = "character"
                  )
				)

setValidity("Benvo",function(object){
				if(length(object@bef_names) != length(length(object@components)))
					"@bef_names and @components must be the same length"
				if(!(all(object@components  %in% c("Distance","Time","Distance-Time")) ))
					"Components must be one of c('Distance','Time','Distance-Time')"
				if(!is.data.frame(object@subject_data))
					"@subject_data must inherit from class data.frame"
				if(!all(sapply(object@bef_data,is.data.frame)))
					"All members of @bef_data must inherit from class data.frame"
				if(!length(get_common_ids(object@subject_data,object@bef_data)))
					"There must be a common id between the subject_data and all bef_data data frames"
				if(!all(object@id %in% get_common_ids(object@subject_data,object@bef_data)))
						" An id must be common between all subject and bef data frames"
				if(length(object@id)==0)
					"An id for relating the subject-bef dataframes must be supplied"
})
