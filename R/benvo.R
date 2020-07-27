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
#' in bef_data frames where Distance measures are stored. NA if there is no distance measure.
#' @param exposed_time_col character vector containing strings
#' in bef_data frames where Time measures are stored. NA if there is no time measure.
#' @importFrom methods new
#' @seealso \code{\link[rbenvo]{Benvo}} 
#' @details benvo is a helper constructor function which creates nicely formatted Benvo objects. 
#' In particular, note that the \code{benvo} function will explicitly alter the data you provide, creating a new
#' numeric joining ID to enable easy aggregation and use with other methods. This alteration will not occur 
#' if calling the raw constructor function \code{\link[rbenvo]{Benvo}}, though it will check for it.
#'
benvo <- function(subject_data,
				  bef_data,
				  bef_names,
				  joining_id,
				  distance_col = rep(NA,length(bef_data)),
				  exposed_time_col = rep(NA,length(bef_data))){

	subject_data <- as.data.frame(subject_data)
	## --  Checks
	if(!all(sapply(bef_data,is.data.frame)))
	  stop("All components of bef_data must be a data.frame")
	check_bef_data(bef_names,bef_data)
	stopifnot(joining_id %in% colnames(subject_data))
	stopifnot(joining_id %in% Reduce(intersect,lapply(bef_data,colnames) ))

	Q <- length(bef_names)
	check_col(distance_col,Q)
	check_col(exposed_time_col,Q)
	## ------
	if(length(joining_id)==1){
		ID_names <- c("ID")
		subject_data$ID <- as.numeric(factor(subject_data[,joining_id]))
		if(joining_id !="ID")
			subject_data <- subject_data[,!(names(subject_data) %in% joining_id)]
	}
	else if(length(joining_id)==2){
		ID_names <- c("ID","Measurement")
		subject_data$ID <- as.numeric(factor(subject_data[,joining_id[1]]))
		subject_data$Measurement <- as.numeric(factor(subject_data[,joining_id[2]]))
		if(all(joining_id != c("ID","Measurement")))
			subject_data <- subject_data[,!(names(subject_data) %in% joining_id)]
	}
	else
		stop("joining ID can only have 1 or 2 names 
			 for cross sectional or longitudinal data respectively")

	## Processing / Standardize Distance/Time Columns
	components <- vector(mode="character",length = Q)


	for(i in 1:Q){
	  col_names <- ID_names
	  cols_to_keep <- c(joining_id)
	  if(!is.na(distance_col[i])){
	    col_names <- c(col_names,"Distance")
	    cols_to_keep <- c(cols_to_keep,distance_col[i])
	    components[i] <- c("Distance")
	    if(!is.na(exposed_time_col[i])){
	      col_names <- c(col_names,"Time")
	      cols_to_keep <- c(cols_to_keep,exposed_time_col[i])
	      components <- c("Distance-Time")
	    }
	  }
	  else if(!is.na(exposed_time_col[i])){
	    components <- c("Time")
	    cols_to_keep <- c(cols_to_keep,exposed_time_col[i])
	    col_names <- c(col_names,"Time")
	  }
	  bef_data[[i]] <- bef_data[[i]][,cols_to_keep]
	  colnames(bef_data[[i]]) <- col_names
	}

	## ------

	bdf <- new("Benvo",subject_data = subject_data,
					  bef_data = bef_data,
					  longitudinal = (length(joining_id)>1),
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

check_col <- function(col,Q){
		if(any(!is.na(col))){
			if(length(col)!=Q){
				stop("There must be an element in distance_col
					 for each entry in the bef_data list.
					 Leave NA for those bef_data
					 without distance measures",.call=F)
			}
		}
	}
