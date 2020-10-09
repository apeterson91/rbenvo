## code to prepare `FFRcoords` dataset goes here



# Libraries ---------------------------------------------------------------


library(tidyverse)
library(sf)
library(osmdata)
library(ggmap)


# School Data -------------------------------------------------------------


temp <- tempfile()
download.file("https://www.cde.ca.gov/SchoolDirectory/report?rid=dl1&tp=xlsx&ict=Y",temp)
schls <- readxl::read_xlsx(temp,skip=5) %>%
  filter(StatusType=="Active") %>%
  mutate(across(contains("tude"),as.numeric)) %>%
  filter(!is.na(Latitude),!is.na(Longitude))
unlink(temp)
temp <- tempfile()
download.file("http://www3.cde.ca.gov/pftresearchfiles/pftresearchfiles19.zip",temp)
census <- read_csv(unz(temp,"2018_19_Entities.txt"))
school_data <- read_csv(unz(temp,"2018_19_ResearchFile.txt")) %>%
  mutate(cdscode = stringr::str_c(CO,DIST,SCHL))
unlink(temp)

school_data %>%
  filter(Table_Number==1,Level_Number==1,Line_Number==2,Report_Number==0) %>%
  mutate(across(contains("Perc"),as.numeric)) %>%
  filter(across(contains("Perc"),function(x) !is.na(x))) %>%
  mutate(Charter = factor((ChrtNum=="0000")*1,labels=c("Charter","Traditional")),
         Percent5 = Perc5b + Perc5c,
         Percent7 = Perc7b + Perc7c,
         Percent9 = Perc9b + Perc9c) %>%
  select(cdscode,
         contains("NoStud"),
         matches("Percent[5-9]"),
         Charter) %>%
  gather(contains("No"),key="Grade",value="N") %>%
  filter(N>0) %>% arrange(cdscode) %>%
  gather(contains("Perc"),key="PcntGrade",value="Percent") %>%
  mutate(Grade = factor(stringr::str_extract(Grade,"[5-9]")),
         PcntGrade = factor(stringr::str_extract(Grade,"[5-9]")),
         Percent = as.numeric(Percent)) %>%
  filter(N>0,Percent>0,!is.na(Percent),Grade==PcntGrade) %>%
  mutate(NoStud_OverweightObese = round((N*Percent)/100),
         NoStud_NotOverweightObese = N- NoStud_OverweightObese) %>%
  select(-PcntGrade) %>%
  arrange(cdscode) %>%
  inner_join(schls %>%
               select(CDSCode,City,County,Latitude,Longitude),
             by=c("cdscode"="CDSCode")) -> sdf



LA_schools <- sdf %>% filter(City=="Los Angeles") %>% select(-County,-City)
usethis::use_data(LA_schools, overwrite = TRUE)


# Restaurants -------------------------------------------------------------



q <- opq(bbox = 'Los Angeles, California') %>%
  add_osm_feature(key = 'amenity',value = "fast_food") %>%
  osmdata_sf()


## For ease of storing sf objects in package
LA_FF <- q$osm_points %>%
  dplyr::select(name,geometry,osm_id) %>%
  mutate(Latitude = unlist(map(.$geometry,1)),
         Longitude = unlist(map(.$geometry,2))) %>%
   as_tibble() %>% select(-geometry) %>%
  dplyr::mutate(Name = stringi::stri_trans_general(name,'latin'))


usethis::use_data(LA_FF,overwrite = TRUE)
# usethis::use_data(LA_restaurants,overwrite=TRUE)

# lamap <- ggmap::get_map(location = "Los Angeles, CA",source = "google")
# fs <- rbind(sf::st_transform(sdf %>%
#                                filter(City=="Los Angeles") %>%
#                                select(geometry) %>%
#                                mutate(Class="Schools"),
#                              "EPSG:3857"),
#             sf::st_transform(RES %>% select(geometry) %>% mutate(Class="Restaurants"),
#                              "EPSG:3857"))
#
# # Define a function to fix the bbox to be in EPSG:3857
# ggmap_bbox <- function(map) {
#   if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
#   # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector,
#   # and set the names to what sf::st_bbox expects:
#   map_bbox <- setNames(unlist(attr(map, "bb")),
#                        c("ymin", "xmin", "ymax", "xmax"))
#
#   # Coonvert the bbox to an sf polygon, transform it to 3857,
#   # and convert back to a bbox (convoluted, but it works)
#   bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
#
#   # Overwrite the bbox of the ggmap object with the transformed coordinates
#   attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
#   attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
#   attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
#   attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
#   map
# }
#
# ggmap(ggmap_bbox(lamap)) +
#   xlab("") + ylab("")  +
#   theme(axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank()) +
#   coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
#   geom_sf(data=fs %>% mutate(Class=factor(Class)),
#           inherit.aes = FALSE,aes(color=Class)) +
#   scale_colour_manual(values=c("grey","black"))
#
# LA_schools %>%
#   st_distance(.,LA_restaurants) -> dists
#
# colnames(dists) <- stringr::str_c("osm_id_",LA_restaurants$osm_id)
# as_tibble(dists) %>%
#   mutate(cdscode=LA_schools %>%
#            pull(cdscode)) %>%
#   gather(contains("osm_id"),key="osm_id",value="Distance") %>%
#   mutate(osm_id = stringr::str_replace(osm_id,"osm_id_",""),
#          Distance = as.numeric(Distance)/1000) %>%
#   filter(Distance <= 10) -> LA_res_distances
#
# CA_bdf <- rbenvo::benvo(subject_data = LA_schools %>% dplyr::mutate(NoStud5_Obese = as.integer(round((Perc5c * NoStud5)/100)),
#                                                                     NoStud5_NotObese = NoStud5 - NoStud5_Obese),
#                      bef_data = list(RES=LA_res_distances),by='cdscode')
#
# usethis::use_data(CA_bdf,overwrite = TRUE)
#
# fit <- rsstap::sstap_glm(cbind(NoStud5_Obese,NoStud5_NotObese) ~ Charter + sap(RES),
#                  benvo = CA_RES,family = binomial())
#
# usethis::use_data(CA_RES, overwrite = TRUE)
