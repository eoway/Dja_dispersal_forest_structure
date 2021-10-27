#~ source('/Users/aferraz/code/r_ubuntu/R/gedi_clipArea.r')
rm(list = ls()) ### remove variables from environment

### Clipping GEDI shots according to a Region of Interest (ROI) defined by a shapefile 
### It requires a shapefile associated with the GEDI shots .h5 files (see "gedi_read_h5_write_shp.r") 
library(tools)
library(rGEDI)
library(rgdal)
library(sf)

### shapefile representing the ROI
#shp.f<-"G:/My Drive/Projects/NASA_Biodiversity_20-BIODIV20-0044/Box/Data/Remote_Sensing_Data/Forest_Structure/Dja_reserve_focus_area_shots/Shots/DjaFaunalReserveOSM.shp"
#shp.f<-"G:/My Drive/Projects/NASA_Biodiversity_20-BIODIV20-0044/Box/Data/Remote_Sensing_Data/Forest_Structure/Dja_reserve_focus_area_shots/Shots/Dja_monodominant.shp"
#shp.f<-"G:/My Drive/Projects/NASA_Biodiversity_20-BIODIV20-0044/Box/Data/Remote_Sensing_Data/Forest_Structure/Dja_reserve_focus_area_shots/Shots/Dja_swamp_forest.shp"
shp.f<-"G:/My Drive/Projects/NASA_Biodiversity_20-BIODIV20-0044/Box/Data/Remote_Sensing_Data/Forest_Structure/Dja_reserve_focus_area_shots/Shots/Dja_mixed_spp_IRD.shp"

# use Dja_swamp_forest.shp
# use Dja_monodominant.shp
# use Dja_mixed_spp_IRD.shp

shp<-readOGR(dsn=shp.f,layer=basename(file_path_sans_ext(shp.f)))
# #Study area boundary box coordinates
# ul_lat<- shp@bbox[2,2] #ymax
# lr_lat<- shp@bbox[2,1] #ymin
# ul_lon<- shp@bbox[1,1] #xmin
# lr_lon<- shp@bbox[1,2] #xmax

# directory with GEDI .h5 files and associated shapefile
download.dir="G:/My Drive/Projects/NASA_Biodiversity_20-BIODIV20-0044/Box/Data/Remote_Sensing_Data/Forest_Structure/Dja_reserve_focus_area_shots/Shots"
# directory to save the clipped GEDI .h5 shots
#out.dir="C:/Users/elsa-admin/Desktop/temp_files/Dja_Reserve_Shots"
#out.dir="C:/Users/elsa-admin/Desktop/temp_files/Dja_monodominant_shots"
#out.dir="C:/Users/elsa-admin/Desktop/temp_files/Dja_swamp_forest_shots"
out.dir="C:/Users/elsa-admin/Desktop/temp_files/Dja_mixed_spp_IRD_shots"

#### find GEDI .h5 files and associated shapefile
#files.GEDI01_B<-list.files(download.dir,pattern="*GEDI01_B_2*",recursive = T,full.names=T);files.GEDI01_B<-files.GEDI01_B[grepl('.h5$',files.GEDI01_B)]
files.GEDI02_A<-list.files(download.dir,pattern="*GEDI02_A_2*",recursive = T,full.names=T);files.GEDI02_A<-files.GEDI02_A[grepl('.h5$',files.GEDI02_A)]
files.shp<-list.files(download.dir,pattern="*.shp*",recursive = T,full.names=T);files.shp<-files.shp[grepl('.shp$',files.shp)]

for (ii in 1:length(files.GEDI02_A))
{
  
  output.name=paste(out.dir,"/",basename(file_path_sans_ext(files.GEDI02_A[ii])),".h5",sep="")
  
  if (!file.exists(output.name)) # check if exist
  {
  print(paste("processing: ",output.name ))
  gedilevel2a<-readLevel2A(level2Apath = files.GEDI02_A[ii]) # Reading GEDI data
  files.shp.2a<-files.shp[grep(basename(file_path_sans_ext(files.GEDI02_A[ii])),files.shp)]
  
  if (length(files.shp.2a)!=1){stop("the shapefile for the h5 does not exist: please run gedi_read_h5_write_shp.r")}
  shp.orbit<-readOGR(dsn=files.shp.2a,layer=basename(file_path_sans_ext(files.shp.2a)))
 
  out <- st_intersects(st_as_sf(shp.orbit), st_as_sf(shp),sparse = FALSE)
  output.name=paste(out.dir,"/",basename(file_path_sans_ext(files.GEDI02_A[ii])),".h5",sep="")
  
  if (sum(out)>0) ##### check if GEDI shots covers the ROI
  {
    print(paste("clipping:", output.name))
    g2b.clip <-clipLevel2AGeometry(gedilevel2a,shp,output.name)

    close(gedilevel2a)
      for (i in 1:length(g2b.clip))
      {
      close(g2b.clip[[i]])
      }
    rm (gedilevel2a);rm(g2b.clip)
  }else{print("existing file or does not cover the ROI")}


  ####### change name to remove "_" added at the end of the name of the clipped GEDI
  change.name<-list.files(out.dir,pattern="*_.h5$",recursive = T,full.names=T)
  for (j in change.name)
  {
    file.rename(j,paste0(substr(change.name,1,nchar(change.name)-4),".h5"))
  }
}
}
