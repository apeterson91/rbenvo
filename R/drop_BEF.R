#' Drop Built Environment Feature from Benvo
#'
#' Remove the active BEF data table and corresponding sub-bef data from the benvo
#' @param x benvo or base benvo
#' @export
#' @return benvo without the active bef data
#'
drop_BEF <- function(x) UseMethod("drop_BEF")


#' @export
#" @describeIn drop_BEF drop BEF tables
drop_BEF.benvo <- function(x){


   if(active(x) == 'subject')
	   stop("Cannot drop subject data")
	else{
		if(bef_is_active(x)){
			one <- no_pre_bef(active(x))
			two <- active(x)
		}
		else{
			one <- pre_bef(active(x))
			two <- active(x)
		}
		x$bef_data[[one]] <- NULL
		x$sub_bef_data[[two]] <- NULL
		nms <- setdiff(names(attr(x,'bef_names')),active(x))
		attr(x,'bef_names') <- attr(x,'bef_names')[nms]
		nms <- setdiff(names(attr(x,'bef_sf')),active(x))
		attr(x,'bef_sf') <- attr(x,'bef_sf')[nms]
		nms <- setdiff(names(attr(x,'bef_dt')),active(x))
		attr(x,'bef_dt') <- attr(x,'bef_dt')[nms]
	}
	active(x) <- 'subject'

	return(x)

}
