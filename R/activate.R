#' Determine the context of subsequent manipulation
#'
#' A benvo is a essentially a small relational database
#' with a specific one-to-many structure between the subject table and each BEF tables.
#' In order to know which data frame is of interest for displaying/manipulating at any given time
#' use the activate function (akin to \code{\link[tidygraph]{activate}}) to do so.
#'
#' @export
#' @param x benvo object
#' @param what name of df to activate
#' @return a benvo 
#'
activate <- function(x,what) UseMethod("activate")



#' @export 
#' @importFrom rlang quo_text enquo
activate.benvo <- function(x,what){
	active(x) <- quo_text(enquo(what))
	x
}


#' @rdname activate
#' @export
active <- function(x)
	return(attr(x,'active'))

`active<-` <- function(x, value) {
	value <- gsub('"', '', value)
	check_activate(x,value)
	attr(x, 'active') <- value
	x
}

get_active <- function(x){
	if(active(x)=="subject")
		return(x$subject_data)
	else
		return(x$bef_data[[active(x)]])
}

# Internal -------------------------

check_activate <- function(x,value){
	if(!(value %in% active_names(x)))
		stop(paste0(value," is not a table in this benvo. For a list of possible tables use `bef_names()` "))
}

active_names <- function(x){
	c("subject",bef_names(x))
}
