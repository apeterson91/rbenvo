#' Benvo Methods
#'
#'
#' @name benvo-methods
#' @aliases head tail
#'
#' @importFrom utils head tail
#' @param x a benvo object
#'
#'
NULL 


#' @rdname benvo-methods
#' @export
bef_names <- function(x) UseMethod("bef_names")

#' @describeIn bef_names BEF name attribute
#' @export
bef_names.benvo <- function(x) return(attr(x,'bef_names'))

#' @export
#' @rdname benvo-methods
components <- function(x) UseMethod("components")

#' @describeIn components BEF components attribute
#' @export
components.benvo <- function(x){
	return(attr(x,'components'))
}

#' @rdname benvo-methods
#' @param term bef_name string
#' @export
component_lookup <- function(x,term) UseMethod("component_lookup")

#' @describeIn component_lookup lookups component based on bef name
#' @export
component_lookup.benvo <- function(x,term){

	ix <- which(bef_names(x) == term)

	component <- attr(x,'components')[ix]

	out <- translate_component_to_cols(component) 

	return(out)
}

#' @rdname benvo-methods
#' @export
subject_has_sf <- function(x){
	if(attr(x,"subject_sf"))
		return(TRUE)
	else
		return(FALSE)
}

#' @rdname benvo-methods
#' @export
bef_has_sf <- function(x,term){
	ix <- term_check(x,term)
	if(attr(x,"bef_sf")[ix])
		return(TRUE)
	else
		return(FALSE)
}

#' @rdname benvo-methods
num_BEF <- function(x) UseMethod("num_BEF")

#' @rdname benvo-methods
#' @export
#' @param ... optional arguments 
head.benvo <- function(x,...){
	if(active(x) == 'subject')
		return(head(x$subject_data,...))
	else
		return(head(x$sub_bef_data[[active(x)]],...))
}

#' @rdname benvo-methods
#' @export
tail.benvo <- function(x,...){
	if(active(x) == 'subject')
		return(tail(x$subject_data,...))
	else
		return(tail(x$sub_bef_data[[active(x)]],...))
}

#' @export
#' @describeIn num_BEF number of BEF data frames
num_BEF.benvo <- function(x){
	return(length(x$sub_bef_data))
}

#' Is a longitudinal benvo
#'
#' @keywords internal
#' @export
#'
is.longitudinal <- function(object) UseMethod("is.longitudinal")

#'
#' @describeIn is.longitudinal returns true if longitudinal
#' @export
#'
is.longitudinal.benvo <- function(object) return(attr(object,"longitudinal"))

#' @rdname benvo-methods
#' @export
get_id <- function(x) UseMethod("get_id")

#' @describeIn get_id retrieve benvo joining id
#' @export
get_id.benvo <- function(x)  return(attr(x,"id"))

#' @export
#' @rdname benvo-methods
has_subject_dt <- function(x) UseMethod("has_subject_dt")

#' @export
#' @describeIn  has_subject_dt
has_subject_dt.benvo <- function(x) return(attr(x,"subject_dt"))

#' @export
#' @rdname benvo-methods
has_bef_dt <- function(x,term) UseMethod("has_bef_dt")

#' @export
#' @describeIn has_bef_dt does bef has datetime info
has_bef_dt <- function(x,term){

	ix <- term_check(x,term)
	return(attr(x,"bef_dt")[ix])
}

#' @rdname benvo-methods
#' @export
is.benvo <- function(x) inherits(x,"benvo")



## Internal ----------------------------

get_date_cols <- function(x) return(sapply(c("measurement_date","start_date_col","stop_date_col"),function(y) attr(x,y)))



bef_id_lookup <- function(x,term) return(attr(x,"bef_id")[term])

create_unique_ID_mat <- function(id_one,id_two = NULL){
	tmp <- paste0(id_one,"_",id_two)
	lvls <- unique(tmp)
	new_id <- factor(tmp,levels=lvls)
	Matrix::fac2sparse(new_id)
}

.printfr <- function(x, digits, ...) {
  print(format(round(x, digits), nsmall = digits), quote = FALSE, ...)
}

term_check <- function(x,term){
	stopifnot(length(term)==1)
	nms <- bef_names(x)
	ix <- which(nms == term )
	if(!length(ix))
		stop("Term is not a member of this benvo")
	return(ix)
}

sf_check <-function(x){
	inherits(x,'sf')
}

component_check <- function(x, term, component){

	if(!all((translate_component_to_cols(component) %in% component_lookup(x,term)))){
		stop("Component:", component, "Not associated with ", term)
	}
}

translate_component_to_cols <-  function(component){
	out <- switch(component,
				  "Distance" = "Distance",
				  "Time" = "Time",
				  "Distance-Time" = c("Distance","Time"),
					stop("Incorrect component value"))
	return(out)
}


