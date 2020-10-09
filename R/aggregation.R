#' Join BEF and subject data within a benvo
#'
#' @export
#' @details Joins the subject dataframe within a benvo to the supplied BEF dataframe keeping the selected component
#' @param x benvo object
#' @param term string of bef name to join on in sub_bef_data
#' @param component one of c("Distance","Time","Distance-Time") indicating which column(s) of the bef dataset should be returned
#' @param NA_to_zero replaces NA values with zeros - potentially useful when constructing design matrices
#'
joinvo <- function(x,term,component = "Distance",NA_to_zero = F) UseMethod("joinvo")

#'
#' @export
#' @importFrom stats quantile median
#' @describeIn joinvo method
#'
joinvo.benvo <- function(x,term,component = "Distance",NA_to_zero = F){


	Distance <- Time <- NULL

	ix <- term_check(x,term)
	component_check(x,term,component)
	id <- get_id(x)


	if(subject_has_sf(x))
		sdf <- sf::st_drop_geometry(x$subject_data[,id,drop=F])
	else
		sdf <- x$subject_data[,id,drop=F]

	jdf <- dplyr::right_join(x$sub_bef_data[[ix]],sdf, by=id) %>% dplyr::arrange_at(id)

	if(NA_to_zero){
		col <- translate_component_to_cols(component)
		jdf <- jdf %>% dplyr::mutate_at(col,function(x) tidyr::replace_na(x,0))
	}

	return(jdf)
}





#' Aggregate Matrix to Subject or Subject - Measurement Level
#'
#' @param x benvo object
#' @param M matrix to aggregate
#' @param stap_term relevant stap term
#' @param component one of c("Distance","Time","Distance-Time") indicating which column(s) of the bef dataset should be returned
#' @export
#'
aggrenvo <- function(x,M,stap_term,component) UseMethod("aggrenvo")


#'
#' @export
#' @describeIn aggrenvo method
#'
aggrenvo.benvo <- function(x,M,stap_term,component){

	. <- NULL
	id <- get_id(x)
	jndf <- joinvo(x,stap_term,component,NA_to_zero = F) 

	if(component=="Distance-Time")
		component_ <- c("Distance") ## Fine to use just one since zero exposure variable will equate to zero exposure in the other
	else
		component_ <- component

	if(subject_has_sf(x))
	  sdf <- sf::st_drop_geometry(x$subject_data)
	else
	  sdf <- x$subject_data

	if(is.longitudinal(x)){
		AggMat <- create_unique_ID_mat(jndf[,id[1],drop=TRUE],jndf[,id[2],drop=TRUE])
		IDMat <- Matrix::t(create_unique_ID_mat(sdf[,id[1],drop=TRUE],sdf[,id[2],drop=TRUE]))
	}else{
		IDMat <- Matrix::t(create_unique_ID_mat(sdf %>% dplyr::arrange_at(id) %>% dplyr::select_at(id) %>% dplyr::pull(name = id)))
		AggMat <- create_unique_ID_mat(jndf[,id,drop=TRUE])
	}
	zeromat <- jndf %>% dplyr::group_by_at(id)  %>%
			dplyr::summarise_at(component_,function(x) 1*all(!is.na(x))) %>%
			dplyr::pull(component_) %>%
			diag(.) %>%
			Matrix::Matrix()

	AggMat <- IDMat %*% zeromat %*% AggMat
	stopifnot(nrow(M) == ncol(AggMat))
	stopifnot(nrow(x$subject_data) == nrow(AggMat))
	X <- as.matrix((AggMat %*% M))
	return(X)
}


#' Between - Within Decomposition
#'
#' @export
#' @param x benvo object
#' @param M matrix to construct between/within measures
#' @keywords internal
#'
bwinvo <- function(x,M) UseMethod("bwinvo")


#' Between - Within Construction
#'
#' @describeIn bwinvo between within decomposition of longitudinal matrix M according to benvo subject-bef data
#' @export
#'
bwinvo.benvo <- function(x,M){

	stopifnot(is.longitudinal(x))
	id <- get_id(x)
	smat <- create_unique_ID_mat(x$subject_data[,id[1],drop=TRUE])
	if(nrow(M)!=ncol(smat))
		stop("rows in M are inappropriate")
	num <- apply(smat,1,sum)
	Xb <- apply((smat %*% M) ,2, function(x) x/num)
	Xb <- as.matrix(Matrix::t(smat) %*% Xb)
	Xw <- as.matrix(M - Xb)
	return(list(between = Xb, within = Xw))
}
