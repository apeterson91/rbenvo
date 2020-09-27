LA_schools <- sf::st_as_sf(LA_schools,coords=c("Longitude","Latitude"),crs=4326)
FFR <- sf::st_as_sf(head(LA_FF),coords=c("Latitude","Longitude"),crs=4326)


test_that("building BEF works", {
  expect_equal(1,num_BEF(benvo(LA_schools,by='cdscode') %>%
                  add_BEF(FFR,bef_id = 'osm_id')))
  expect_equal(0,num_BEF(benvo(LA_schools,by='cdscode') %>%
                           add_BEF(FFR,bef_id = 'osm_id') %>%
                           activate(FFR) %>%
                           drop_BEF()))
})
