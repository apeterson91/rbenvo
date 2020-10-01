
#' Create California Benvo
#'
#' This function exists primarily to save the hassle of having an sf object stored as an
#' R data object, as it introduces non-ascii characters into the package.
#' With this function, the appropriate benvo is returned.
#' @export
#' @return a benvo with the Los Angeles data converted to sf objects.
#' @seealso The building benvos vignette
#'
create_CA_benvo <- function(){

	Distance <- cdscode <- osm_id <- NULL
	LA_schools <- sf::st_as_sf(rbenvo::LA_schools,coords=c("Longitude","Latitude"),crs=4326)
	FFR <- sf::st_as_sf(rbenvo::LA_FF,coords=c("Latitude","Longitude"),crs=4326)
	benvo(LA_schools,by='cdscode') %>%
	  add_BEF(FFR,bef_id = 'osm_id') %>%
	  activate(FFR) %>%
	  mutate(Distance = as.numeric(Distance/1E3) ) %>%
	  filter(Distance<=10) %>%
	  distinct(cdscode,osm_id,Distance)-> bdf
  return(bdf)
}

