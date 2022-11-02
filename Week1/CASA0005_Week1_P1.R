library(sf)
# change this to your file path!!!
shape <- st_read('C:/Users/Scott Shaw/Desktop/CASA0005/Week1/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp')
summary(shape)
plot(shape)
library(sf)
shape %>% 
  st_geometry() %>%
  plot()
library(tidyverse)
#this needs to be your file path again
mycsv <-  read_csv('C:/Users/Scott Shaw/Desktop/CASA0005/Week1/fly-tipping-borough-new.csv',skip=1)
mycsv
shape <- shape%>%
  merge(.,
        mycsv,
        by.x="GSS_CODE", 
        by.y="Row Labels")
shape%>%
  head(., n=10)
library(tmap)
tmap_mode("plot")
# change the fill to your column name if different
shape %>%
  qtm(.,fill = "2019_20")