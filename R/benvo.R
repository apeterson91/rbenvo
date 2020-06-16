#' Create a benvo object
#'
#' @export
#'
#' @param subject_data
#'      data.frame containing subject level covariates.
#' @param bef_data list of BEF data frames
#' @param bef_names character vector of data frames
#' @param joining_id character vector containing strings in both subject and bef data frames used to join data
#' @param distance_col character vector containing strings
#' in bef_data frames where Distance measures are stored. NULL if there is no distance measure.
#' @param time_col character vector containing strings
#' in bef_data frames where Distance measures are stored. NULL if there is no time measure.
#'
benvo <- function(subject_data,
				  bef_data,
				  bef_names,
				  joining_id,
				  distance_col = NULL,
				  exposed_time_col = NULL){

	## --  Checks
	check_bef_data(bef_names,bef_data)
	stopifnot(joining_id %in% colnames(subject_data))
	stopifnot(joining_id %in% Reduce(intersect,lapply(bef_data,colnames) ))

	Q <- length(bef_names)
	check_col(distance_col,Q)
	check_col(exposed_time_col,Q)
	## ------

	## Processing / Standardize Distance/Time Columns
	sapply(1:Q,function(x){
			   if(!is.null(distance_col[x]))
				   colnames(bef_data[[x]][,distance_col,drop = F]) <- "Distance"
			   if(!is.null(exposed_time_col[x]))
				   colnames(bef_data[[x]][,exposed_time_col, drop = F]) <- "Time"
				  } )
	components <- vector(mode="character",length = Q)
	for(i in 1:Q){
	  if(!is.null(distance_col[i])){
	    components[i] <- "Space"
	    if(!is.null(exposed_time_col[i]))
	      components[i] <- "Space-Time"
	  }else if(!is.null(exposed_time_col[i])){
	    components[i] <- "Time"
	  }else{
	    stop("Each BEF must either have Distance or Time measures associated with it")
	  }
		}
	## ------

	bdf <- new("Benvo",subject_data = subject_data,
					  bef_data = bef_data,
					  id = joining_id,
					  bef_names = bef_names,
					  components = components)
}

check_bef_data <- function(bef_names,bef_data){

	P <- length(bef_names)
	Q <- length(bef_data)
	if(P!=Q)
		stop("There must be a BEF for each
			 data.frame in bef_data")
	stopifnot(is.list(bef_data))
	stopifnot(all(sapply(bef_data,is.data.frame)))
}

check_col<- function(col,Q){
	if(!is.null(col))
		if(length(col)!=Q)
			stop("There must be an element in distance_col
				 for each entry in the bef_data list.
				 Leave NULL for those bef_data
				 without distance measures")
}
