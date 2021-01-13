/* Install the required libraries*/

library(rgdal)
library(gdalUtils)
library(raster)

gdalinfo("C:/Users/Karthik/Desktop/SURFACE REFLECTANCE/MOD09A1.A2019145.h25v07.006.2019154041949.hdf")
sds<-get_subdatasets("C:/Users/Karthik/Desktop/SURFACE REFLECTANCE/MOD09A1.A2019145.h25v07.006.2019154041949.hdf")
sds

gdal_translate(sds[1], dst_dataset = "RED.tif")
gdal_translate(sds[2], dst_dataset = "NIR1.tif")
gdal_translate(sds[3], dst_dataset = "BLUE.tif")
gdal_translate(sds[4], dst_dataset = "GREEN.tif")
gdal_translate(sds[5], dst_dataset = "NIR2.tif")
gdal_translate(sds[6], dst_dataset = "SWIR1.tif")
gdal_translate(sds[7], dst_dataset = "SWIR2.tif")

B1<readGDAL("RED.tif")
B2<readGDAL("NIR1.tif")
B3<readGDAL("BLUE.tif")
B4<readGDAL("GREEN.tif")
B5<readGDAL("NIR2.tif")
B6<readGDAL("SWIR1.tif")
B7<readGDAL("SWIR2.tif")

rast.B1<-raster(B1)
rast.B2<-raster(B2)
rast.B3<-raster(B3)
rast.B4<-raster(B4)
rast.B5<-raster(B5)
rast.B6<-raster(B6)
rast.B7<-raster(B7)


NDVI<-((rast.B2-rast.B1)/(rast.B2+rast.B1))
breaks_for_NDVI<-seq(-1,1, by = 0.2)
NDVI=calc(NDVI,function(x){x[x< -1]<- NA;return(x)})
NDVI=calc(NDVI,function(x){x[x>1]<- NA;return(x)})
cols_for_NDVI <- colorRampPalette(c("blue","red", "yellow","darkgreen"))(length(breaks_for_NDVI)-1)
plot(NDVI,col=cols_for_NDVI,breaks=breaks_for_NDVI)

VEGCOVER<-calc(NDVI,function(x){x[x< 0.4]<- NA;return(x)})
breaks_for_VEGCOVER<-seq(-1,1, by = 0.2)
NDVI=calc(NDVI,function(x){x[x< -1]<- NA;return(x)})
NDVI=calc(NDVI,function(x){x[x>1]<- NA;return(x)})
cols_for_VEGCOVER <- colorRampPalette(c("blue", "yellow","darkgreen"))(length(breaks_for_VEGCOVER)+1)
plot(VEGCOVER,col=cols_for_VEGCOVER,breaks=breaks_for_VEGCOVER)

EVI<-(2.5*(rast.B2-rast.B1))/(rast.B2+6*rast.B1-7.5*rast.B3+1)
EVI=calc(EVI,function(x){x[x<0]<- NA;return(x)})
EVI=calc(EVI,function(x){x[x>1]<- NA;return(x)})
plot(EVI)

NDWI1<-(rast.B2-rast.B6)/(rast.B2+rast.B6)
breaks_for_NDWI1<-seq(-1,1,by=0.2)
cols_for_NDWI1 <- colorRampPalette(c("red","palegoldenrod", "blue"))(length(breaks_for_NDWI1))
NDWI1=calc(NDWI1,function(x){x[x>1]<- NA;return(x)})
NDWI1=calc(NDWI1,function(x){x[x<(-1)]<- NA;return(x)})
NDWI1=calc(NDWI1,function(x){x[x< (-0.3)]<-NA;return(x)})
plot(NDWI1,col=cols_for_NDWI1,breaks=breaks_for_NDWI1)

NDWI2<-(rast.B4-rast.B2)/(rast.B4+rast.B2)
breaks_for_NDWI2<-seq(-1,1,by=0.2)
cols_for_NDWI2 <- colorRampPalette(c("red","yellow","white","blue"))(length(breaks_for_NDWI2))
NDWI2=calc(NDWI2,function(x){x[x>1]<- NA;return(x)})
NDWI2=calc(NDWI2,function(x){x[x<(-1)]<- NA;return(x)})
plot(NDWI2,col=cols_for_NDWI2,breaks=breaks_for_NDWI2)

df<-data.frame("Band"=c(1,2,3,4,5,6,7),"Wavelength"=c("620-670","841-876","459-479","545-565","1230-1250","1628-1652","2105-2155"),"Resolution"=c(250,250,500,500,500,500,500),"BandName"=c("Red","NIR1","Blue","Green","NIR2","SWIR1","SWIR2"))
df

