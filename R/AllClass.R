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
#' @slot id 
#'      character vector containing strings in both data frames used to join data
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
				    id = "character",
					bef_names = "character",
					components = "character"
                  )
				)
