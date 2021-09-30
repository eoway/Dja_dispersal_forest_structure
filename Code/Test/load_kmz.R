
#Test change to file
# adding

#------------------------------------------------------------------------------------------------#
# Set output dir for downloading and reading files
# outdir="G:/My Drive/GEDI/Southern_Sierra"

library(raster); library(rgdal); library(sp)
library(reshape2); library(dplyr); library(rgeos)
library(GISTools); library(sf); library(SDMTools)
library(ggplot2); library(RColorBrewer); library(grid)
library(rasterVis); library(viridis); library(stringr)
library(cowplot); library(ggplotify); library(purrr)
library(fasterize)

ctaLines <- readOGR( dsn = paste(getwd()
                                  , "Constrained_MAXI.kml"
                                  , sep = "/"), 
                     stringsAsFactors = FALSE)
