# Open NO2 OMI OMNO2d data
# 04/1/2017
# Ferchu fgarciaferreyra@conae.gov.ar

# Necesario!
library(RNetCDF)
library(maptools)
library(sp)
library(raster)
library(fields)

# para verificar:
#library(rgeos)

data <- open.nc("/home/octave/Documentos/Fernanda/OMINO2d/OMI-Aura_L3-OMNO2d_2016m0319_v003-2016m0825t091247.he5.nc", write=FALSE)
dataread <- read.nc(data, unpack=TRUE)
ColumnAmountNO2 <- var.get.nc(data, "ColumnAmountNO2") # "NO2 vertical column density" molec/cm2
ColumnAmountNO2CloudScreened <- var.get.nc(data,"ColumnAmountNO2CloudScreened")  # "NO2 vertical column density, screened for CloudFraction < 30%" molec/cm2
ColumnAmountNO2Trop <- var.get.nc(data,"ColumnAmountNO2Trop") # "NO2 tropospheric column density" molec/cm2
ColumnAmountNO2TropCloudScreened <- var.get.nc(data,"ColumnAmountNO2TropCloudScreened") # "NO2 tropospheric column density, screened for CloudFraction < 30%" - molec/cm2
Weight <- var.get.nc(data,"Weight") # "Weight Factor used for each Grid Cell" - no units
lon <- var.get.nc(data,"lon")
lat <- var.get.nc(data,'lat')

dim_ColumnAmountNO2 <- dim(ColumnAmountNO2) # tamaño de la matriz de datos: 1440 720
#missing_value <- att.get.nc(data,"ColumnAmountNO2","missing_value")

#Lectura de shapefiles:
shape_world <- readShapePoly("/home/octave/Documentos/Fernanda/Vectores/WorldCountries.shp")

xmin <- min(lon)
xmax <- max(lon)
ymin <- min(lat)
ymax <- max(lat)
nx <- xmax*2/length(lon)
ny <- ymax*2/length(lat)
coords_world <- raster(xmn=xmin, xmx=xmax, ymn=ymin, ymx=ymax, ncols=length(lon), nrows=length(lat))

hist_NO2 <- hist(ColumnAmountNO2, breaks=100, xlab="molec/cm2")
hist_NO2CloudScreened <- hist(ColumnAmountNO2CloudScreened, breaks=100, xlab="molec/cm2")
hist_NO2Trop <- hist(ColumnAmountNO2Trop, breaks=100, xlab="molec/cm2")
hist_NO2TropCloudScreened <- hist(ColumnAmountNO2TropCloudScreened, breaks=100, xlab="molec/cm2")
hist_Weight <- hist(Weight, breaks=100)

# Mapa del mundo Column Amount NO2
data <- c(ColumnAmountNO2[,dim_ColumnAmountNO2[2]:1]) # vectoriza primero por columna y después por fila
data_raster <- setValues(coords_world, data) #pone los valores de la matriz en las coordenadas definidas
breakpoints <- c(0,1e+15,2e+15,3e+15,4e+15,5e+15,6e+15,7e+15,9e+15,12e+15,15e+15,18e+15,20e+15)
image(data_raster, col=rainbow(12, start=.4, end=.2), breaks=breakpoints,
      main = "NO2 OMI 19-03-2016", ylim=c(-90, 90),
      xlab="",ylab="")
plot(shape_world, add=TRUE)
image.plot(zlim=c(0,2e+16), legend.only=TRUE, nlevel=50, breaks=breakpoints,
           horizontal=T, col=rainbow(12, start=0.4, end=.2))

# Mapa del mundo ColumnAmountNO2CloudScreened
data <- c(ColumnAmountNO2CloudScreened[,dim_ColumnAmountNO2[2]:1]) # vectoriza primero por columna y después por fila
data_raster <- setValues(coords_world, data) #pone los valores de la matriz en las coordenadas definidas
breakpoints <- c(0,1e+15,2e+15,3e+15,4e+15,5e+15,6e+15,7e+15,9e+15,12e+15,15e+15,18e+15,20e+15)
image(data_raster, col=rainbow(12, start=.4, end=.2), breaks=breakpoints,
      main = "NO2 CloudScreened OMI 19-03-2016", ylim=c(-90, 90),
      xlab="",ylab="")
plot(shape_world, add=TRUE)
image.plot(zlim=c(0,2e+16), legend.only=TRUE, nlevel=50, breaks=breakpoints,
           horizontal=T, col=rainbow(12, start=0.4, end=.2))

# Mapa del mundo ColumnAmountNO2Trop
data <- c(ColumnAmountNO2Trop[,dim_ColumnAmountNO2[2]:1]) # vectoriza primero por columna y después por fila
data_raster <- setValues(coords_world, data) #pone los valores de la matriz en las coordenadas definidas
breakpoints <- c(0,1e+15,2e+15,3e+15,4e+15,5e+15,6e+15,7e+15,9e+15,12e+15,15e+15,18e+15,20e+15)
image(data_raster, col=rainbow(12, start=.4, end=.2), breaks=breakpoints,
      main = "NO2 Troposphere OMI 19-03-2016", ylim=c(-90, 90),
      xlab="",ylab="")
plot(shape_world, add=TRUE)
image.plot(zlim=c(0,2e+16), legend.only=TRUE, nlevel=50, breaks=breakpoints,
           horizontal=T, col=rainbow(12, start=0.4, end=.2))

# Mapa del mundo ColumnAmountNO2TropCloudScreened
data <- c(ColumnAmountNO2TropCloudScreened[,dim_ColumnAmountNO2[2]:1]) # vectoriza primero por columna y después por fila
data_raster <- setValues(coords_world, data) #pone los valores de la matriz en las coordenadas definidas
breakpoints <- c(0,1e+15,2e+15,3e+15,4e+15,5e+15,6e+15,7e+15,9e+15,12e+15,15e+15,18e+15,20e+15)
image(data_raster, col=rainbow(12, start=.4, end=.2), breaks=breakpoints,
      main = "NO2 Troposphere - Cloud Screened OMI 19-03-2016", ylim=c(-90, 90),
      xlab="",ylab="")
plot(shape_world, add=TRUE)
image.plot(zlim=c(0,2e+16), legend.only=TRUE, nlevel=50, breaks=breakpoints,
           horizontal=T, col=rainbow(12, start=.4, end=.2))

# Mapa del mundo Weight
data <- c(Weight[,dim_ColumnAmountNO2[2]:1]) # vectoriza primero por columna y después por fila
data_raster <- setValues(coords_world, data) #pone los valores de la matriz en las coordenadas definidas
breakpoints <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
image(data_raster, col=rainbow(10, start=.4, end=.2), breaks=breakpoints, 
      main = "Weight OMI 19-03-2016", ylim=c(-90, 90),
      xlab="",ylab="")
plot(shape_world, add=TRUE)
image.plot(zlim=c(0,1), legend.only=TRUE, nlevel=50, breaks=breakpoints,
           horizontal=T, col=rainbow(10, start=.4, end=.2))

###### RECORTE PARA ARGENTINA

#Lectura de shapefiles:
shape_arg <- readShapePoly("/home/octave/Documentos/Fernanda/Vectores/Argentina.shp")

xmin_arg <- -75.125
xmax_arg <- -51.875
ymin_arg <- -57.125
ymax_arg <- -20.125

i_xmin_arg <- which(lon == xmin_arg)
i_xmax_arg <- which(lon == xmax_arg)
i_ymin_arg <- which(lat == ymin_arg)
i_ymax_arg <- which(lat == ymax_arg)

lon_arg <- lon[i_xmin_arg:i_xmax_arg]
lat_arg <- lat[i_ymin_arg:i_ymax_arg]
i_coords_arg <- c(dim(lon_arg),dim(lat_arg))

coords_arg <- raster(xmn=xmin_arg, xmx=xmax_arg, ymn=ymin_arg, ymx=ymax_arg, ncols=i_coords_arg[1], nrows=i_coords_arg[2])

ColNO2_Arg_raster <- raster(ColumnAmountNO2[i_xmin_arg:i_xmax_arg,i_ymin_arg:i_ymax_arg])
ColNO2CloudScreened_Arg_raster <- raster(ColumnAmountNO2CloudScreened[i_xmin_arg:i_xmax_arg,i_ymin_arg:i_ymax_arg])
ColNO2Trop_Arg_raster <- raster(ColumnAmountNO2Trop[i_xmin_arg:i_xmax_arg,i_ymin_arg:i_ymax_arg])
ColNO2TropCloudScreened_Arg_raster <- raster(ColumnAmountNO2TropCloudScreened[i_xmin_arg:i_xmax_arg,i_ymin_arg:i_ymax_arg])

hist_NO2_Arg <- hist(ColNO2_Arg_raster,breaks=100, main="Histograma Argentina NO2 OMI 19-03-2016", xlab="molec/cm2")
hist_NO2CloudScreened_Arg <- hist(ColNO2CloudScreened_Arg_raster, breaks=100, main="Histograma Argentina NO2 CloudScreened OMI 19-03-2016", xlab="molec/cm2")
hist_NO2Trop_Arg <- hist(ColNO2Trop_Arg_raster, breaks=100, main="Histograma Argentina NO2 Troposphere OMI 19-03-2016", xlab="molec/cm2")
hist_NO2TropCloudScreened_Arg <- hist(ColNO2TropCloudScreened_Arg_raster, breaks=100, main="Histograma Argentina NO2 Trop CloudScreened OMI 19-03-2016", xlab="molec/cm2")

# Mapa de Argentina NO2
data_arg <- c(ColNO2_Arg_raster[,i_coords_arg[2]:1]) # vectoriza primero por columna y después por fila
data <- setValues(coords_arg, data_arg) #pone los valores de la matriz en las coordenadas definidas
breakpoints <- c(0,1e+15,2e+15,3e+15,4e+15,5e+15,6e+15,7e+15,9e+15,12e+15,15e+15,18e+15,20e+15)
image(data, breaks=breakpoints, col=rainbow(12, start=.4, end=.2),
      main = "Argentina NO2 OMI 19-03-2016",
      xlab=" ", ylab=" ")
plot(shape_arg, add=TRUE)
image.plot(zlim=c(0,2e+16), nlevel=50, legend.only=TRUE, 
           horizontal=T, col=rainbow(12, start=.4, end=.2)) #col=rainbow(50, start=2/6, end=0))

# Mapa de Argentina NO2CloudScreened
data_arg <- c(ColNO2CloudScreened_Arg_raster[,i_coords_arg[2]:1]) # vectoriza primero por columna y después por fila
data <- setValues(coords_arg, data_arg) #pone los valores de la matriz en las coordenadas definidas
breakpoints <- c(0,1e+15,2e+15,3e+15,4e+15,5e+15,6e+15,7e+15,9e+15,12e+15,15e+15,18e+15,20e+15)
image(data, breaks=breakpoints, col=rainbow(12, start=.4, end=.2),
      main = "Argentina NO2 Cloud Screened OMI 19-03-2016",
      xlab=" ", ylab=" ")
plot(shape_arg, add=TRUE)
image.plot(zlim=c(0,2e+16), nlevel=50, legend.only=TRUE, 
           horizontal=T, col=rainbow(12, start=.4, end=.2)) #col=rainbow(50, start=2/6, end=0))

# Mapa de Argentina NO2Trop
data_arg <- c(ColNO2Trop_Arg_raster[,i_coords_arg[2]:1]) # vectoriza primero por columna y después por fila
data <- setValues(coords_arg, data_arg) #pone los valores de la matriz en las coordenadas definidas
breakpoints <- c(0,1e+15,2e+15,3e+15,4e+15,5e+15,6e+15,7e+15,9e+15,12e+15,15e+15,18e+15,20e+15)
image(data, breaks=breakpoints, col=rainbow(12, start=.4, end=.2),
      main = "Argentina NO2 Troposphere OMI 19-03-2016",
      xlab=" ", ylab=" ")
plot(shape_arg, add=TRUE)
image.plot(zlim=c(0,2e+16), nlevel=50, legend.only=TRUE, 
           horizontal=T, col=rainbow(12, start=.4, end=.2)) #col=rainbow(50, start=2/6, end=0))

# Mapa de Argentina NO2TropCloudScreened
data_arg <- c(ColNO2TropCloudScreened_Arg_raster[,i_coords_arg[2]:1]) # vectoriza primero por columna y después por fila
data <- setValues(coords_arg, data_arg) #pone los valores de la matriz en las coordenadas definidas
breakpoints <- c(0,1e+15,2e+15,3e+15,4e+15,5e+15,6e+15,7e+15,9e+15,12e+15,15e+15,18e+15,20e+15)
image(data, breaks=breakpoints, col=rainbow(12, start=.4, end=.2),
      main = "Argentina NO2 Troposphere CloudScreened OMI 19-03-2016",
      xlab=" ", ylab=" ")
plot(shape_arg, add=TRUE)
image.plot(zlim=c(0,2e+16), nlevel=50, legend.only=TRUE, 
           horizontal=T, col=rainbow(12, start=.4, end=.2)) #col=rainbow(50, start=2/6, end=0))
