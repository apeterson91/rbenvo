
#' @importFrom dplyr group_by
#' @export
group_by.benvo <- function(.data,...,.add = FALSE, .drop = dplyr::group_by_drop_default(.data)){
	stop("group_by is not currently implimented for benvos")
}

#' @importFrom dplyr group_by
#' @export
ungroup <- function(x,...){
	stop("ungroup is not currently implimented for benvos")
}

#' @export
dplyr::group_by

#' @export
dplyr::ungroup
