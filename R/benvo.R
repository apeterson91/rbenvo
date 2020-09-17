#' Create a benvo object
#'
#' @export
#'
#' @param subject_data
#'      data.frame containing subject level covariates.
#' @param bef_data named list of BEF data frames
#' @param by optional key
#' @importFrom dplyr tibble
#' @details benvo is a constructor function which creates benvo objects.
#' In particular, note that the \code{benvo} function will explicitly check the data you provide,
#' to ensure benvo methods can be performed without error.
#'
benvo <- function(subject_data,
				  bef_data,
				  by = NULL){

	check_dfs(subject_data,bef_data)
	bef_names <- get_bef_names(bef_data)
	ids <- get_ids(subject_data,bef_data, by)

	components <- sapply(bef_data,extract_components)
	bef_sf_attr <- sapply(bef_data,function(x) inherits(x,'sf'))
	
	out <- list(subject_data = subject_data,
				bef_data = bef_data)


	structure(out,
			  bef_sf = bef_sf_attr,
			  subject_sf = inherits(subject_data,'sf'),
			  longitudinal = (length(ids)>1),
			  bef_names = bef_names,
			  components = components,
			  active = "subject",
			  id = ids,
			  class = "benvo")
}


## Internal ------------------------------------------------------
check_dfs <- function(subject_data,bef_data){
	stopifnot(is.data.frame(subject_data))
	if(!is.list(bef_data))
	if(!all(sapply(bef_data,is.data.frame)))
		stop("All entries in bef_data must be data.frames")
}
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

get_ids <- function(subject_data,bef_data,by){


	ids <- get_common_ids(subject_data,bef_data)
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
	check_ids(ids,subject_data,bef_data)
	return(ids)
}

check_by <- function(ids,by){
	if(!is.null(by)){
		if(!length(intersect(ids,by)) == length(union(ids,by)) )
			stop("argument by=", by, "is not a member of the common columns between all bef data and subject data")
		ids <- by
		return(by)
	}
	return(ids)
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

