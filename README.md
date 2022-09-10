# map_transect
```
library(raster)
library(rayshader)
library(sf)
library(tidyverse)

# To install the latest version from Github:
# install.packages("devtools")
#devtools::install_github("tylermorganwall/rayshader")

# Set working directory
setwd("C:/Users/Charlie/Desktop/Projects/CA")

#Load image
topo_map = raster::brick("Montara_map.tif")
dim(topo_map) #Dimensions of raster
ex<-(extent(topo_map))
mean_lat<-ex@ymin+(ex@ymax-ex@ymin)/2
asp = 1/cospi(mean_lat/180) #Determines aspect ratio from latitude of image
topo_map<-aggregate(topo_map,fact=1) #Larger numbers reduce raster size by more

#Splits image up into rgb
names(topo_map) = c("r","g","b")
topo_map_r = rayshader::raster_to_matrix(topo_map$r)
topo_map_g = rayshader::raster_to_matrix(topo_map$g)
topo_map_b = rayshader::raster_to_matrix(topo_map$b)

topo_map_array = array(0,dim=c(nrow(topo_map_r),ncol(topo_map_r),3))

topo_map_array[,,1] = topo_map_r/255 #Red 
topo_map_array[,,2] = topo_map_g/255 #Blue 
topo_map_array[,,3] = topo_map_b/255 #Green 
topo_map_array = aperm(topo_map_array, c(2,1,3))

plot_map(topo_map_array)

#Load elevation data
elevation1 = raster::raster("Montara_dem2.tif")
elevation=elevation1+0
res(elevation) #Resolution of a pixel
extent(elevation) #Extent of raster
dim(elevation) #Dimensions of raster

height_shade(raster_to_matrix(elevation)) %>%
  plot_map()

#Check CRS
raster::crs(topo_map)
raster::crs(elevation)

#Extent
# extent format (xmin,xmax,ymin,ymax)
xmin=-122.43446
xmax=-122.41273
ymin=37.50026
ymax=37.624783

#Loop
steps=450
num<-c(1:steps, steps:1) #Sequence for frames

zscale1=10 #Smaller value=more vertical exaggeration

xdim=0.02173
ydim=0.01614

seq_xmin<-seq(from=xmin, to=(xmax-xdim), length.out=steps)
seq_xmax<-seq(from=xmin+xdim, to=xmax, length.out=steps)

seq_ymin<-seq(from=ymin, to=(ymax-ydim), length.out=steps)
seq_ymax<-seq(from=ymin+ydim, to=ymax, length.out=steps)


for(i in 1:steps) 
{
  #Extent
  # extent format (xmin,xmax,ymin,ymax)
  e <- as(extent(-122.43446, (-122.43446+xdim), seq_ymin[i], seq_ymax[i]), 'SpatialPolygons')
  raster::crs(e) <- "EPSG:4326"
  
  #Crop elevation
  el_cropped <- crop(elevation, e)
  
  #Crop map
  topo_cropped <- crop(topo_map, e)
  
  
  #Splits image up into rgb
  names(topo_cropped) = c("r","g","b")
  topo_cropped_r = rayshader::raster_to_matrix(topo_cropped$r)
  topo_cropped_g = rayshader::raster_to_matrix(topo_cropped$g)
  topo_cropped_b = rayshader::raster_to_matrix(topo_cropped$b)
  
  topo_cropped_array = array(0,dim=c(nrow(topo_cropped_r),ncol(topo_cropped_r),3))
  
  topo_cropped_array[,,1] = topo_cropped_r/255 #Red 
  topo_cropped_array[,,2] = topo_cropped_g/255 #Blue 
  topo_cropped_array[,,3] = topo_cropped_b/255 #Green 
  topo_cropped_array = aperm(topo_cropped_array, c(2,1,3))
  
  
  # Changes name
  elevation_final<-el_cropped
  
  #Raster to matrix conversion of elevation data
  el_matrix = rayshader::raster_to_matrix(elevation_final)
  
  #Plot maps
  #Reduce the size of the elevation data, for speed
  small_el_matrix = resize_matrix(el_matrix, scale = 1)
  
  #Render
  zscale1=2
  ambient_layer = ambient_shade(small_el_matrix, zscale = zscale1, multicore = TRUE, maxsearch = 200)
  ray_layer = ray_shade(small_el_matrix, zscale = zscale1, sunaltitude=35, sunangle=330, multicore = TRUE)
 
  zscale2=5
  #Plot in 3D
  (topo_cropped_array) %>%
    add_shadow(ray_layer,0.3) %>%
    add_shadow(ambient_layer,0.1) %>%
    plot_3d(small_el_matrix,asp=1.3, shadow=FALSE, solid=FALSE, zscale=zscale2, dirt=FALSE, soliddepth=-100, shadowdepth=-100, windowsize = c(1000, 800))

  Sys.sleep(0.2)
  render_snapshot(filename = sprintf("scroll_3d%i.png", i))
  
  rgl::rgl.close()
}

frame_num<-c(1:steps) #Sequence for frames

#Read sequence and create video
av::av_encode_video(sprintf("scroll_3d%i.png", frame_num), vfilter = "scale=trunc(iw/2)*2:trunc(ih/2)*2", framerate = 30, output = "scroll30_2.mp4")
````
