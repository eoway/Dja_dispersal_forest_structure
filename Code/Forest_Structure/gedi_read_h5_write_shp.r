rm(list = ls()) ### remove variables from environment

library(rhdf5)
library(rgdal)
library(tools)
download.dir<-"C:/Users/aferraz/Documents/data/GEDI_CongoBasin/DRC/h5/6"
files.GEDI02_A<-list.files(download.dir,pattern="*GEDI02_A*",recursive = F,full.names=T);files.GEDI02_A<-files.GEDI02_A[grepl('.h5$',files.GEDI02_A)]

for (ii in 1:length(files.GEDI02_A))
#for (i in 20:20)
{
  out.shp.f<-paste0(file_path_sans_ext(files.GEDI02_A[ii]),".shp")
  if (!file.exists(out.shp.f))
  {
  print(paste0("processing: ",out.shp.f))
  df<-data.frame(lat_lowestmode=numeric(0),lon_lowestmode=numeric(0),elev_lowestmode=numeric(0),rh98=numeric(0),shot_number=character(),quality_flag=numeric(0),sensitivity=numeric(0),beamId=character(0),beamType=character(0),file=character(0))
  beam_<-c('BEAM0000','BEAM0001','BEAM0010','BEAM0011','BEAM0101','BEAM0110','BEAM1000','BEAM1011')
  tbeam_<-c('coverage','coverage','coverage','coverage','full','full','full','full')
  
  ############################################## check if every beam is in the orbit
  a<-h5ls(files.GEDI02_A[ii])
  df.beam<-data.frame(group=character(0),tbeam_=character(0),dim=character(0))
  for (h in 1:length(beam_))
  {
    dim<-as.numeric(subset(a,(a$group==paste0("/",beam_[h]) & a$name=="beam"))$dim)
    df.beam<-rbind(df.beam,data.frame(group=beam_[h],tbeam_=tbeam_[h],dim=dim))
  }
  df.beam<-subset(df.beam,df.beam$dim!=0)
  ############################################# check if every beam is in the orbit
  GEDI02_A.o<-H5Fopen(files.GEDI02_A[ii])
  for (j in df.beam$group)
  {
    GEDI02_A<-h5read(GEDI02_A.o,j,bit64conversion='bit64')
    lat_lowestmode<-GEDI02_A$lat_lowestmode;lon_lowestmode<-GEDI02_A$lon_lowestmode
    elev_lowestmode=GEDI02_A$elev_lowestmode;rh98=GEDI02_A$rh[99,]
    shot_number=as.character(GEDI02_A$shot_number);quality_flag=as.numeric(GEDI02_A$quality_flag);sensitivity=GEDI02_A$sensitivity
    beamId=rep(j,length(lat_lowestmode));beamType=rep(df.beam[df.beam$group==j,]$tbeam_,length(lat_lowestmode))
    df<-rbind(df,data.frame(lat_lowestmode=lat_lowestmode,lon_lowestmode=lon_lowestmode,elev_lowestmode=elev_lowestmode,rh98=rh98,shot_number=shot_number,quality_flag=quality_flag,sensitivity=sensitivity,beamId=beamId,beamType=beamType))
    rm(GEDI02_A)
  }
  h5closeAll()
  df<-df[complete.cases(df), ]
  coordinates(df)<-~lon_lowestmode+lat_lowestmode
  proj4string(df)<- CRS("+proj=longlat +datum=WGS84")
  writeOGR(df,dsn=out.shp.f,layer=basename(file_path_sans_ext(out.shp.f)),driver="ESRI Shapefile",overwrite_layer = T)
  ##h5write(B, "ex_hdf5file.h5","B")
  }
  
}
