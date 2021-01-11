library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)


BeijingBoroughs <- st_read(here::here("Beijing.shp"))

library(stringr)

BeijingMap <- BeijingBoroughs %>%
  dplyr::filter(str_detect(GSS_CODE, "^1101"))%>%
  st_transform(., 27700)
qtm(BeijingMap)


summary(BeijingMap)


hospitals <- st_read(here::here("hospital.shp")) %>%
  st_transform(.,27700)
summary(hospitals)


#plot the blue plaques in the city
tmap_mode("view")


tm_shape(BeijingMap) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(hospitals) +
  tm_dots(col = "red")

library(tidyverse)

library(sf)
hospitals <- distinct(hospitals)

hospitalsSub <- hospitals[BeijingMap,]
#check to see that they've been removed
tmap_mode("view")
tm_shape(BeijingMap) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(hospitalsSub) +
  tm_dots(col = "red")




#Select Haidian district
Haidian <- BeijingMap %>%
  filter(., name=="Haidian")

#Check to see that the correct borough has been pulled out
tm_shape(Haidian) +
  tm_polygons(col = NA, alpha = 0.5)


#Check to see that the correct borough has been pulled out
tm_shape(Haidian) +
  tm_polygons(col = NA, alpha = 0.5)

#clip the data to our single borough
HospitalsSub <- hospitals[Haidian,]
#check that it's worked
tmap_mode("view")

tm_shape(Haidian) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(HospitalsSub) +
  tm_dots(col = "red")

#now set a window as the borough boundary
window <- as.owin(Haidian)
plot(window)


HospitalsSub<- HospitalsSub %>%
  as(., 'Spatial')

HospitalsSub.ppp <- ppp(x=HospitalsSub@coords[,1],
                        y=HospitalsSub@coords[,2],
                        window=window)
HospitalsSub.ppp %>%
  plot(.,pch=16,cex=0.5,
       main="Hospitals in Haidian district")



#Kernel Density Estimation
HospitalsSub.ppp %>%
  density(., sigma=800) %>%
  plot()


#Quadrat Analysis  ¡®complete spatial randomness¡¯ ¡ª CSR
plot(HospitalsSub.ppp,
     pch=16,
     cex=0.5, 
     main="Hospitals in Haidian district")

#now count the points in that fall in a 6 x 6
#grid overlaid across the windowBluePlaquesSub.ppp2<-BluePlaquesSub.ppp %>%
HospitalsSub.ppp %>%
  quadratcount(.,nx = 6, ny = 6)%>%
  plot(., add=T, col="red")

#Complete Spatial Random
#run the quadrant count
Qcount <- HospitalsSub.ppp %>%
  quadratcount(.,nx = 6, ny = 6) %>%
  as.data.frame() %>%
  dplyr::count(Var1=Freq)%>%
  dplyr::rename(Freqquadratcount=n)

Qcount %>% 
  summarise_all(class)

#Poisson distribution
sums <- Qcount %>%
  #calculate the total blue plaques (Var * Freq)
  mutate(total = Var1 * Freqquadratcount) %>%
  dplyr::summarise(across(everything(), sum))%>%
  dplyr::select(-Var1) 


lambda<- Qcount%>%
  #calculate lambda
  mutate(total = Var1 * Freqquadratcount)%>%
  dplyr::summarise(across(everything(), sum)) %>%
  mutate(lambda=total/Freqquadratcount) %>%
  dplyr::select(lambda)%>%
  pull(lambda)


QCountTable <- Qcount %>%
  mutate(Pr=((lambda^Var1)*exp(-lambda))/factorial(Var1))%>%
  #now calculate the expected counts based on our total number of hospitals
  #and save them to the table
  mutate(Expected= (round(Pr * sums$Freqquadratcount, 0)))


#Compare the frequency distributions of the observed and expected point patterns
plot(c(1,5),c(0,14), type="n",
     xlab="Number of Hospitals (Red=Observed,Blue=Expected)", 
     ylab="Frequency of Occurances")
points(QCountTable$Freqquadratcount, 
       col="Red", 
       type="o", 
       lwd=3)
points(QCountTable$Expected, col="Blue", 
       type="o", 
       lwd=3)


teststats <- quadrat.test(HospitalsSub.ppp, nx = 6, ny = 6)

plot(HospitalsSub.ppp,pch=16,cex=0.5, main="Hospitals in Haidian")
plot(teststats, add=T, col = "red")
#uhhhh


#Ripley¡¯s K
K <- HospitalsSub.ppp %>%
  Kest(., correction="border") %>%
  plot()
#the Kpois(r) line in Red is the theoretical value of K for each distance window (r) under a Poisson assumption of Complete Spatial Randomness. 
#The Black line is the estimated values of K accounting for the effects of the edge of the study area.








#DBSCAN
library(raster)
library(fpc)
#first check the coordinate reference system of the Haidian spatial polygon:
st_geometry(BeijingMap)


#first extract the points from the spatial points data frame
HospitalSubPoints <- HospitalsSub %>%
  coordinates(.)%>%
  as.data.frame()

#now run the dbscan analysis
db <- HospitalSubPoints %>%
  fpc::dbscan(.,eps = 1000, MinPts = 4)

#now plot the results
plot(db, HospitalSubPoints, main = "DBSCAN Output", frame = F)
plot(BeijingMap$geometry, add=T)

# used to find suitable eps value based on the knee in plot
# k is no of nearest neighbors used, use min points
library(dbscan)

HospitalSubPoints%>%
  dbscan::kNNdistplot(.,k=4)


library(ggplot2)
db
#db$cluster -----3
#read the ward data in
BeijingWards <- st_read(here::here("beijing_region.shp"))

HospitalSubPoints<- HospitalSubPoints %>%
  mutate(dbcluster=db$cluster)


chulls <- HospitalSubPoints %>%
  group_by(dbcluster) %>%
  dplyr::mutate(hull = 1:n(),
                hull = factor(hull, chull(coords.x1, coords.x2)))%>%
  arrange(hull)


chulls <- chulls %>%
  filter(dbcluster >=1)

dbplot <- ggplot(data=HospitalSubPoints, 
                 aes(coords.x1,coords.x2, colour=dbcluster, fill=dbcluster)) 
#add the points in
dbplot <- dbplot + geom_point()
#now the convex hulls
dbplot <- dbplot + geom_polygon(data = chulls, 
                                aes(coords.x1,coords.x2, group=dbcluster), 
                                alpha = 0.5) 
#now plot, setting the coordinates to scale correctly and as a black and white plot 
#(just for the hell of it)...
dbplot + theme_bw() + coord_equal()




