#' @title Benvo data objects
#'
#' @description
#' A custom data structure for handling point pattern built environment data. 
#' Consists of two dataframes (1) subject and (2) subject - BEF with a joining id(s).
#' 
#' 
#' @slot subject_data
#'      data.frame containing subject level covariates.
#' @slot bef_data 
#'		data.frame containing subject - bef distance/time data   
#' @slot longitudinal 
#'      boolean value indicating whether the Benvo holds longitudinal data
#' @slot bef_names 
#'      character vector containing the names of the BEFs
#' @slot components
#'	    character vector containing the Space and/or Time data measured for each BEF
#'
#' @export
#'
Benvo <- setClass("Benvo",
                  slots = list(
                    subject_data = "data.frame",
                    bef_data = "list",
				    longitudinal = "logical",
					bef_names = "character",
					components = "character"
                  )
				)

setValidity("Benvo",function(object){
				if(length(object@bef_names) != length(length(object@components))){
					"@bef_names and @components must be the same length"
				}
				if(!all(sapply(bef_data,is.data.frame)))
					"All members of @bef_data must inherit from class data.frame"
				if(length(intersect(colnames(subject_data),Reduce(lapply(bef_data,colnames),union))==0))
					"There must be a common id between the subject_data and all bef_data data frames"
				})
