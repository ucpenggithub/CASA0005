# 定位文件夹
setwd('C:/Users/Scott Shaw/Desktop/CASA0005/Week3')

#读取数据 here（前置文件夹，需要读取的文件）
library(sf)
library(here)
st_layers(here("prac3_data", "gadm36_AUS.gpkg"))

#读取整个AUS的包
library(sf)
Ausoutline <- st_read(here("prac3_data", "gadm36_AUS.gpkg"), 
                      layer='gadm36_AUS_0')
#检测坐标系 
print(Ausoutline)

#世界大地测量系统84（WGS84 - 最后一次修订于1984年）是最常见的全球投影系统之一，几乎用于所有GPS设备。但是，有许多版本的 WGS84，一些是地理参考系，一些是投影坐标参考系。
#3D 地球仪 GPS 设备和点数据集通常使用地理版本（EPSG 代码 4326），通常简称为 WGS84 谷歌地球使用地理版本，因为它是一个3D地球仪（EPSG代码4326）
#Google Maps / carto / bing / Open Street地图使用Web Mercator，Google Web Mercator，球形Mercator，WGS 84 Web Mercator或WGS 84 / pseudo-Mercator，这是一个预测的坐标参考系统（EPSG代码3857）。它是Mercator地图投影的变体。

library(sf)
st_crs(Ausoutline)$proj4string

#WGS84世界测量系统的EPSG代码（通常是大多数空间数据的默认CR）为4326  -  http://epsg.io/4326

#如果没有空间参考系统，我们可以使用st_set_crs(., 4326)
Ausoutline <- Ausoutline %>%
  st_set_crs(., 4326)

#earily way 
Ausoutline <- st_read(here("prac3_data", "gadm36_AUS.gpkg"), 
                      layer='gadm36_AUS_0') %>% 
  st_set_crs(4326)

#对于SF对象，就像我们在澳大利亚的轮廓一样，它是使用ST_Transform进行的 我们将从WGS84更改为GDA94，这是澳大利亚本地CR，具有EPSG代码3112
AusoutlinePROJECTED <- Ausoutline %>%
  st_transform(.,3112)

#sp transform to sf 
#From sf to sp
AusoutlineSP <- Ausoutline %>%
  as(., "Spatial")

#From sp to sf
AusoutlineSF <- AusoutlineSP %>%
  st_as_sf()

library(raster)
jan<-raster(here("prac3_data", "wc2.1_5m_tavg_01.tif"))
# have a look at the raster layer jan
jan
plot(jan)

# 将他换成proj 4 
newproj<-"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# get the jan raster and give it the new proj4 给他一个新的raster 成为一个新的proj4
pr1 <- jan %>%
 projectRaster(., crs=newproj)
plot(pr1)

pr1 <- pr1 %>%
  projectRaster(., crs="+init=epsg:4326")
plot(pr1)

# look in our folder, find the files that end with .tif and 
library(fs)
dir_info("prac3_data/") 

library(tidyverse)
listfiles<-dir_info("prac3_data/") %>%
  filter(str_detect(path, ".tif")) %>%
  dplyr::select(path)%>%
  pull()

#have a look at the file names 
listfiles

worldclimtemp <- listfiles %>%
  stack()

#have a look at the raster stack
worldclimtemp

# access the january layer
worldclimtemp[[1]]

month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

names(worldclimtemp) <- month

worldclimtemp$Jan

site <- c("Brisbane", "Melbourne", "Perth", "Sydney", "Broome", "Darwin", "Orange", 
          "Bunbury", "Cairns", "Adelaide", "Gold Coast", "Canberra", "Newcastle", 
          "Wollongong", "Logan City" )
lon <- c(153.03, 144.96, 115.86, 151.21, 122.23, 130.84, 149.10, 115.64, 145.77, 
         138.6, 153.43, 149.13, 151.78, 150.89, 153.12)
lat <- c(-27.47, -37.91, -31.95, -33.87, 17.96, -12.46, -33.28, -33.33, -16.92, 
         -34.93, -28, -35.28, -32.93, -34.42, -27.64)
#Put all of this inforamtion into one list 
samples <- data.frame(site, lon, lat, row.names="site")
# Extract the data from the Rasterstack for all points 
AUcitytemp<- raster::extract(worldclimtemp, samples)

Aucitytemp2 <- AUcitytemp %>% 
  as_tibble()%>% 
  add_column(Site = site, .before = "Jan")

Perthtemp <- Aucitytemp2 %>%
  filter(site=="Perth") 

Perthtemp <- Aucitytemp2[3,] 
#做直方图 
hist(as.numeric(Perthtemp))

#define where you want the breaks in the historgram
userbreak<-c(8,10,12,14,16,18,20,22,24,26)
hist(as.numeric(Perthtemp), 
     breaks=userbreak, 
     col="red", 
     main="Histogram of Perth Temperature", 
     xlab="Temperature", 
     ylab="Frequency")

histinfo <- Perthtemp %>%
  as.numeric()%>%
  hist(.)

histinfo

plot(Ausoutline$geom)

#简化shp图
AusoutSIMPLE <- Ausoutline %>%
  st_simplify(., dTolerance = 1000) %>%
  st_geometry()%>%
  plot()
print(Ausoutline)

crs(worldclimtemp)

Austemp <- Ausoutline %>%
  # now crop our temp data to the extent
  crop(worldclimtemp,.)

# plot the output
plot(Austemp)

exactAus <- Austemp %>%
  mask(.,Ausoutline, na.rm=TRUE)

hist(exactAus[[3]], col="red", main ="March temperature")