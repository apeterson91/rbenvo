#' Create a benvo object
#'
#' @export
#'
#' @param subject_data
#'      data.frame containing subject level covariates.
#' @param sub_bef_data named list of data frames that contain subject-bef relevant data.
#' NULL by default which returns a "base benvo" Which can be built upon/added to.
#' @param by optional key to link subject - sub_bef data. Will use the intersection of column names if not specified directly.
#' @param ... optional arguments for specifying date-time columns see \code{\link{set_datetime_cols}}
#' @importFrom dplyr tibble
#' @return benvo object
#' @details benvo is a constructor function which creates benvo objects.
#' In particular, note that the \code{benvo} function will explicitly check the data you provide,
#' to ensure benvo methods can be performed without error.
#' @seealso  \href{https://apeterson91.github.io/rbenvo/articles/Introduction.html}{Introductory} and more \href{https://apeterson91.github.io/rbenvo/articles/Building_Benvos.html}{Specialized} vignettes. 
#'
benvo <- function(subject_data,
				  sub_bef_data = NULL,
				  by = NULL,...){

	if(is.null(sub_bef_data)){
		if(is.null(by))
			stop("by and `sub_bef_data` cannot both be NULL")
		return(base_benvo(subject_data,by=by,...))
	}
	check_dfs(subject_data,sub_bef_data)
	bef_names <- get_bef_names(sub_bef_data)
	ids <- get_ids(subject_data,sub_bef_data, by)

	components <- sapply(sub_bef_data,extract_check_components)
	bef_sf_attr <- sapply(sub_bef_data,function(x) inherits(x,'sf'))
	datetime_cols <- set_datetime_cols(...)
	dt_checks <- check_datetime_cols(datetime_cols,subject_data,sub_bef_data)

	out <- list(subject_data = subject_data,
				sub_bef_data = sub_bef_data,
				bef_data = NULL)


	structure(out,
			  bef_sf = bef_sf_attr,
			  subject_sf = inherits(subject_data,'sf'),
			  bef_dt = FALSE,
			  subject_dt = dt_checks,
			  longitudinal = (length(ids)>1),
			  bef_names = bef_names,
			  bef_ids = NULL,
			  components = components,
			  measurement_date = datetime_cols[1],
			  start_date_col = datetime_cols[2],
			  stop_date_col = datetime_cols[3],
			  active = "subject",
			  base = FALSE,
			  id = ids,
			  class = "benvo")
}

#' Set DateTime Columns
#'
#' When exposure time and lag exposure time need to be calculated
#' the measurement date, and start/stop date columns can be provided to the benvo
#' and base_benvo functions as optional arguments. Note that these columns will be converted to \code{\link[lubridate]{Date}}
#' objects if they're not already.
#' @export
#' @param measurement_date column string for the date at which a subject was measured
#' @param start_date_col column string for the date at which a subject/bef moved to their corresponding location
#' @param stop_date_col column string for the date at which a subject/bef stopped having exposure at the corresponding location.
#'
set_datetime_cols <- function(measurement_date = NULL,
							  start_date_col = NULL,
							  stop_date_col = NULL){

	out <- c("measurement_date"=measurement_date,
			 "start_date_col" = start_date_col,
			 "stop_date_col" = stop_date_col)
	return(out)
}

## Internal ------------------------------------------------------


check_datetime_cols <- function(datetime_cols,subject_data,bef_data = NULL){

	if(all(is.null(datetime_cols)))
		return(FALSE)

	subject <- all(datetime_cols %in% colnames(subject_data))

	if(!is.null(bef_data))
		bef <- lapply(bef_data,function(x) all(datetime_cols %in% colnames(x)))
	else
		return(subject)

	return(list(subject=subject,bef=bef))
}


check_dfs <- function(subject_data,sub_bef_data){
	stopifnot(is.data.frame(subject_data))
	if(!is.list(sub_bef_data))
	if(!all(sapply(sub_bef_data,is.data.frame)))
		stop("All entries in bef_data must be data.frames")
}

get_common_ids <- function(subject_data,sub_bef_data){
	scnames <- colnames(subject_data)
	bcnames <- Reduce(intersect,lapply(sub_bef_data,colnames))
	by <- intersect(scnames,bcnames)
	return(by)
}

get_bef_names <- function(sub_bef_data){

	nms <- names(sub_bef_data)
	if(is.null(nms)){
		message("No BEF Names assigned, assigning generic names: `BEF_1`,...")
		nms <- paste0("BEF_",1:length(sub_bef_data))
	}
	return(nms)
}

get_ids <- function(subject_data,sub_bef_data,by){


	ids <- get_common_ids(subject_data,sub_bef_data)
	if(!length(ids))
		stop("There must be at least one ID common between subject and BEF data")
	if(length(ids)>2 || length(ids)<0)
		stop("Benvos are currently limited to having at most 2 common IDs between subject and BEF data")
	if(length(ids)==2){
	  if(length(unique(subject_data[,ids[1],drop=TRUE])) >length(unique(subject_data[,ids[2],drop=TRUE])))
		  ids <- ids
		else
		  ids <- c(ids[2],ids[1])
	}
	ids <- check_by(ids,by)
	check_ids(ids,subject_data,sub_bef_data)
	return(ids)
}

check_by <- function(ids,by){
	if(!is.null(by)){
		if(!length(intersect(ids,by)) == length(union(ids,by)) )
			stop("argument by=", by, "is not a member of the common columns between all bef data and subject data")
		ids <- by
		return(by)
	}
	message("Will join dfs in benvo using: " , ids)
	return(ids)
}

extract_check_components <- function(dt){
		nms <- colnames(dt)
		rslt <- c("Distance","Time") %in% nms
		if(all(rslt==FALSE))
			stop("There must be one column labeled `Distance` or `Time` in each bef_data dataframe")
		if(all(rslt==TRUE))
			return(c("Distance-Time"))
		if(rslt[1] == TRUE)
			return("Distance")
		if(rslt[2]==TRUE)
			return("Time")
}

# Warns users if id columns are not integer or character, OR not consistently typed across bef_data.
check_ids <- function(ids,sdf,bdf){

	types <- sapply(ids,function(x) class(sdf[,x,drop=TRUE]))
	types_2 <- sapply(ids,function(x) lapply(bdf,function(y) class(y[,x,drop=TRUE])))
	check_type <- function(types,id, dftype){
		if(!(all(types=="integer") || (all(types=="character"))) ){
			st <- glue::glue("Your {dftype} data column {id} is of type {types}.
							 This may lead to erroneous behavior depending on how it is coerced in joins.
							 Change your id to integer or character for better behavior.")
			warning(st)
		}
	}
	check_type(types,ids,"subject")
	types_2 <- Reduce(cbind,types_2)
	if(is.matrix(types_2)){
		t22 <- types_2[,2]
		t21 <- types_2[,1]
		check_type(t22,ids[2],"bef")
		check_type(t21,ids[1],"bef")
	}else
		check_type(types_2,ids,"bef")
}

