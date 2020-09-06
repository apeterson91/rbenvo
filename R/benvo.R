#' Create a benvo object
#'
#' @export
#'
#' @param subject_data
#'      data.frame containing subject level covariates.
#' @param bef_data named list of BEF data frames
#' @param by optional key
#' @importFrom methods new
#' @seealso \code{\link[rbenvo]{Benvo}}
#' @details benvo is a helper constructor function which creates nicely formatted Benvo objects.
#' In particular, note that the \code{benvo} function will explicitly alter the data you provide, creating a new
#' numeric joining ID to enable easy aggregation and use with other methods. This alteration will not occur
#' if calling the raw constructor function \code{\link[rbenvo]{Benvo}}, though it will check for it.
#'
benvo <- function(subject_data,
				  bef_data,
				  by = NULL){

	subject_data <- as.data.frame(subject_data)
	## --  Checks
	bef_names <- get_bef_names(bef_data)
	ids <- check_by(subject_data,bef_data)
	if(!is.null(by)){
		if(!length(intersect(ids,by)) || !(length(by) %in% c(1,2)))
			stop("argument by=", by, "is not a member of the common columns between all bef data and subject data")
		ids <- by
	}
	check_ids(ids,subject_data,bef_data)
	## ------

	components <- sapply(bef_data,extract_components)


	bdf <- new("Benvo",
			   subject_data = subject_data,
			   bef_data = bef_data,
			   longitudinal = (length(ids)>1),
			   bef_names = bef_names,
			   components = components,
				id = ids)
}


## Internal ------------------------------------------------------
get_common_ids <- function(subject_data,bef_data){
	scnames <- colnames(subject_data)
	bcnames <- Reduce(intersect,lapply(bef_data,colnames))
	by <- intersect(scnames,bcnames)
	return(by)
}

get_bef_names <- function(bef_data){

	nms <- names(bef_data)
	if(is.null(nms)){
		message("No BEF Names assigned, assigning generic names: `BEF_1`,...")
		nms <- paste0("BEF_",1:length(bef_data))
	}
	return(nms)
}

check_by <- function(subject_data,bef_data){


	by <- get_common_ids(subject_data,bef_data)
	if(!length(by))
		stop("There must be at least one ID common between subject and BEF data")
	if(length(by)>2)
		stop("Benvos are currently limited to having at most 2 common IDs between subject and BEF data")
	if(length(by)==2){
	  if(length(unique(subject_data[,by[1]])) >length(unique(subject_data[,by[2]])))
		  by <- by
		else
		  by <- c(by[2],by[1])
	}
	return(by)
}

extract_components <- function(dt){
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

	types <- sapply(ids,function(x) class(sdf[,x]))
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

