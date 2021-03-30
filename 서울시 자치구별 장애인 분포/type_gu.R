setwd("C:/R/BigDataCampus data")

library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(dplyr)

type_gu <- read.csv("type_gu.csv", header = T, stringsAsFactors = F)
View(type_gu)
str(type_gu)
seoul_id <- read.csv("seoul_id.csv", stringsAsFactors = F)
View(seoul_id)
str(seoul_id)

seoul_id <- rename(seoul_id, 자치구=시군구명)
type_gu_code <- inner_join(type_gu, seoul_id, by = "자치구")
View(type_gu_code)
str(type_gu_code)

map <- shapefile("C:/R/BigDataCampus data/SIG_201703/TL_SCCO_SIG.shp")
class(map)
map <- spTransform(map, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
new_map <- fortify(map, region = 'SIG_CD')
View(new_map)
new_map$id <- as.numeric(new_map$id)
seoul_map <- new_map[new_map$id <= 11740,]
type_gu_code_seoul_map <- merge(seoul_map, type_gu_code, by = 'id')
str(type_gu_code_seoul_map)

ggplot() +
  geom_polygon(data = type_gu_code_seoul_map, aes(x=long, y=lat, group=group), fill = 'white', color='black')
ggplot() + 
  geom_polygon(data = type_gu_code_seoul_map, aes(x=long, y=lat, group = group, fill = 합계)) +
  scale_fill_gradient(low = "#cee8f5", high = "#03a9fc", space = "Lab", guide = "colourbar") +
  theme_bw() +
  labs(title = "서울시 장애인 자치구별 분포") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 20, hjust = 0.5))
?geom_polygon
