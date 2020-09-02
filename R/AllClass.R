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
				if(!(all(object@components  %in% c("Distance","Time","Distance-Time")) )){
					"Components must be one of c('Distance','Time','Distance-Time')"
				}
				if(!is.data.frame(object@subject_data)){
					"@subject_data must be a data.frame"
				}
				if(!all(sapply(object@bef_data,is.data.frame)))
					"All members of @bef_data must inherit from class data.frame"
				if(length(intersect(colnames(object@subject_data),Reduce(intersect,lapply(object@bef_data,colnames))))==0){
					"There must be a common id between the subject_data and all bef_data data frames"
				}
				if(object@longitudinal){
					if(!all((c("ID","Measurement") %in% intersect(colnames(object@subject_data),Reduce(intersect,lapply(object@bef_data,colnames)))) )){
						"'ID' and 'Measurement' columns must be included in any longitudinal benvo"
					}else if(!is.numeric(object@subject_data$ID) ||
				     !is.numeric(object@subject_data$Measurement) ||
				     !all(sapply(object@bef_data,function(x) is.numeric(x$ID) & is.numeric(x$Measurement) ))){
				    "'ID' and 'Measurement' Columns must be numeric in all benvo data frames"
				  }
				}else{
					if(!(c("ID") %in% intersect(colnames(object@subject_data),Reduce(intersect,lapply(object@bef_data,colnames))) )){
						" An 'ID'  column must be included in any non-longitudinal benvo"
					}else if(!is.numeric(object@subject_data$ID) ||
				     !all(sapply(object@bef_data,function(x) is.numeric(x$ID))) ){
				    "'ID' and 'Measurement' Columns must be numeric in all benvo data frames"
				  }
				}
})
