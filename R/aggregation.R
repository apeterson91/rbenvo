
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
	jndf <- joinvo(x,stap_term,component,NA_to_zero = F)
	id <- get_id(x)

	if(component=="Distance-Time")
		component_ <- c("Distance") ## Fine to use just one since zero exposure variable will equate to zero exposure in the other
	else
		component_ <- component


	if(is.longitudinal(x)){
		AggMat <- create_unique_ID_mat(jndf[,id[1],drop=TRUE],jndf[,id[2],drop=TRUE])
		IDMat <- Matrix::t(create_unique_ID_mat(x$subject_data[,id[1],drop=TRUE],x$subject_data[,id[2],drop=TRUE]))
	}else{
		IDMat <- Matrix::t(create_unique_ID_mat(x %>% dplyr::select_at(id) %>% dplyr::pull(name = id)))
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
	smat <- create_unique_ID_mat(x$subject_data[,id[1],drop=TRUE],x$subject_data[,id[2],drop=TRUE])
	if(nrow(M)!=ncol(smat))
		stop("rows in M are inappropriate")
	num <- apply(smat,1,sum)
	Xb <- apply((smat %*% M) ,2, function(x) x/num)
	Xb <- as.matrix(Matrix::t(smat) %*% Xb)
	Xw <- as.matrix(M - Xb)
	return(list(between = Xb, within = Xw))
}
