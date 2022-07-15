getwd()
#------------------------------------------------------------------------------------------------#
# set working directory
#------------------------------------------------------------------------------------------------#
setwd("C:/Users/Etudiants/Documents/Antoine_Tekam/Base de données")

# Antoine: make sure you update the working directory above and change the file paths below
#------------------------------------------------------------------------------------------------#

library(raster) # Manipulating rasters
library(rgdal) # GIS data
library(tidyverse) # has dplyr
library(colorRamps)
r2 <- matlab.like2(200)

require(sf) # GIS data



# load excel data sheet (charger le fichier excel)
dat <- read.csv("crown_df_idtax_AT_phenology.csv")
head(dat)

# load crown shapefile (charger le shapefile)
crowns <- readOGR(dsn="C:/Users/Etudiants/Documents/Antoine_Tekam/Base de données/Data", layer="Crowns_Bouamir_clean_12052022_version_finale")
head(crowns)
#phen_crowns <- subset(crowns, Point_FID != "NA")
#crown_test <- subset(crowns, Point_FID == 63)
#final_trees <- subset(crowns, Point_FID %in% c(63, 219, 111))
plot(crowns)

# add tax_fam and phenology columns to shapefile (ajouter les colonnes tax_fam et phénologie au shapefile )
head(crowns)
head(dat)

# check to see if two columns are identical (vérifier si deux colonnes sont identiques)
identical(crowns$Gns_spc,dat$Gns_spc) # false

# find the rows that are not identical (chercher les lignes qui ne sont pas identiques )
ifelse(crowns$Gns_spc==dat$Gns_spc,"Yes","No") #546; 577

crowns$Gns_spc[546]
dat$Gns_spc[546]

crowns$Gns_spc[577]
dat$Gns_spc[577]

crowns$Gns_spc[581]
dat$Gns_spc[581]

crowns$Gns_spc[621]
dat$Gns_spc[621]

crowns$Gns_spc[629]
dat$Gns_spc[629]
# correct the species names (corriger le nom des espèces)
crowns$Gns_spc <- sub("Anikia afinis", "Annickia affinis", crowns$Gns_spc)
crowns$Gns_spc[546]
crowns$Gns_spc[577]

identical(crowns$Gns_spc,dat$Gns_spc) # false
ifelse(crowns$Gns_spc==dat$Gns_spc,"Yes","No") #546; 577

sum(ifelse(crowns$Gns_spc==dat$Gns_spc,0,1), na.rm=T)

crowns$phenology <- dat$phenology
crowns$tax_fam <- dat$tax_fam



# nombre espece
length(unique(as.character(crowns$Gns_spc)))
# nombre de couronnes par espece
sort(table(crowns$Gns_spc), decreasing=T)

# pour avoir par exemple les 10 espèces avec plus d'effectifs 
sort(table(crowns$Gns_spc), decreasing=T)[1:10]

# famille
length(unique(as.character(crowns$tax_fam)))


# load RGB mosaics (charger les mosaïques RGB et ne pas oublier l'extention)
chm_2022 <- raster("D:/Bouamir Mars 2022/CHM_1m.tif")
rgb_2022_03_26 <- brick("D:/Bouamir Mars 2022/Bouamir_20220326_Pheno_Mosa.tif")

# plot mosaic data
plot(chm_2022) # plot each layer (n=4) separately
plot(chm_2022, col=r2)
plot(crowns, add=T, col="green")


# look at projection and crs (coordinate reference system)
projection(crowns)
crs(crowns) #longlat / longitude latitude 

projection(chm_2022)
crs(chm_2022) # UTM Zone 33

projection(rgb_2022_03_26)
crs(rgb_2022_03_26) # UTM Zone 33


# set final_trees crs to be same as raster data (mosaic)
new_crs <- crs(chm_2022) # could replace chm_2022 with EVI data
crownsUTM <- spTransform(crowns, CRS=new_crs)
projection(crownsUTM) #make sure the new final_trees projection is now UTM
projection(chm_2022)

# plot 
plot(chm_2022, col=r2)
plot(crownsUTM, add=T, border="red")



# plot(mosaic_2020_5_27) # plot each layer (n=4) separately
# plotRGB(mosaic_2020_5_27, r=4,g=2,b=1, stretch = "lin") # plot a composite of multiple bands
plot(rgb_2022_03_26)
plotRGB(rgb_2022_03_26, r=3,g=2,b=1, stretch = "lin")
plot(crownsUTM, add=T, border="red")



# # load EVI mosaics (each image is one date from the year 2021)
# # you'll stack all dates to get multiple dates from 2021, roughly 1-2 per month
# # load every single file in the path directory that's a tif file (all your EVI mosaic images) - call it rastlist
# rastlist <- list.files(path = "/path/to/wd", pattern='.TIF$',all.files=TRUE, full.names=FALSE)
# # stack every file from rastlist 
# evi_stack <- stack(rastlist)

# # charger les mosaïques EVI (chaque image est une date de l'année 2022)
# # Empilerez toutes les dates pour obtenir plusieurs dates de 2022, environ 1 à 2 par mois
# # charger tous les fichiers du répertoire path qui sont des fichiers tif (toutes vos images de mosaïque EVI) - appelez-les rastlist.
# rastlist <- list.files(path = "/path/to/wd", pattern='.TIF$',all.files=TRUE, full.names=FALSE)
# # empiler chaque fichier de rastlist 
# evi_stack <- stack(rastlist)


# examine data
res(rgb_2022_03_26)
head(crownsUTM)
dim(crownsUTM)


# # extract crown info for each band separately (this takes a minute)
# mosaic_crowns_b1_1 <- raster::extract(mosaic_2020_5_27[[1]], final_treesUTM) #B
# mosaic_crowns_b2_1 <- raster::extract(mosaic_2020_5_27[[2]], final_treesUTM) #G
# mosaic_crowns_b3_1 <- raster::extract(mosaic_2020_5_27[[3]], final_treesUTM) #R
# # mosaic_crowns_b4_1 <- raster::extract(mosaic_2020_5_27[[4]], final_treesUTM) #NIR
# class(mosaic_crowns_b1_1) #object of class list - need to convert to data frame
                            #objet de la classe liste - doit être converti en cadre de données

#mosaic_crowns_b1_1 <- raster::extract(mosaic_2020_5_27[[1]], final_treesUTM)

## extraire les informations de couronne pour chaque bande séparément 
mosaic_crowns_b1_1 <- raster::extract(rgb_2022_03_26[[1]], crownsUTM) #B
mosaic_crowns_b2_1 <- raster::extract(rgb_2022_03_26[[2]], crownsUTM) #G
mosaic_crowns_b3_1 <- raster::extract(rgb_2022_03_26[[3]], crownsUTM) #R

mosaic_crowns_b4_1 <- raster::extract(mosaic_2022_03_26 [[4]], final_treesUTM) #NIR

# first, examine list (examiner d'abord la liste) à faire pour toutes les autres bandes
str(mosaic_crowns_b1_1) # list of 285 objects (matrices)
                        # liste de 285 objets (matrices)
head(mosaic_crowns_b1_1) # not particularly useful for lists, more useful for data frames
                         # pas particulièrement utile pour les listes, mais plus utile pour les cadres de données
dim(mosaic_crowns_b1_1) # doesn't work on lists to get dimensions, so use length() instead in next line
                        # ne fonctionne pas sur les listes pour obtenir les dimensions, donc utilisez length() à la place dans la ligne suivante
length(mosaic_crowns_b1_1) # good, same number of list objects as crown polygons (n=285)
                           # bon, même nombre d'objets de liste que de polygones de couronne (n=285)
summary(mosaic_crowns_b1_1) # summarizes each of the 285 objects (length, class, and mode)
                            # résume chacun des 285 objets (longueur, classe, et mode)
# in the summary output, length tells you how many elements are in each list # Dans la sortie du résumé, la longueur vous indique le nombre d'éléments de chaque liste.
# notice each object/vector within the list is of a different length  Notez que chaque objet/vecteur de la liste a une longueur différente. 
# each object/vector consists of pixel values extracted for each crown (n=285)  Chaque objet/vecteur est constitué de valeurs de pixels extraites pour chaque couronne (n=285).


#------------------------------------------------------------------------------------------------#
# convert list to data frame 
#------------------------------------------------------------------------------------------------#
# the code below converts each list object (n=285) to a column and sets the number of rows to the maximum list object/vector length (595)
#le code ci-dessous convertit chaque objet de liste (n=285) en une colonne et fixe le nombre de lignes à la longueur maximale de l'objet de liste/vecteur (595)

mosaic_crowns_b1_1_df <- data.frame(lapply(mosaic_crowns_b1_1, "length<-", max(lengths(mosaic_crowns_b1_1))))
colnames(mosaic_crowns_b1_1_df) <- paste0("Crown_",1:ncol(mosaic_crowns_b1_1_df))
# rename the columns (à faire pour toutes les autres bandes)
dim(mosaic_crowns_b1_1_df)
colnames(mosaic_crowns_b1_1_df) <- paste0("Crown_",1:ncol(mosaic_crowns_b1_1_df))
head(mosaic_crowns_b1_1_df)
summary(mosaic_crowns_b1_1_df[,1:5]) # notice the different number of NAs for each ROI; each column is 595 rows, but not all polygons had 595 pixels within them; NAs used to fill in the No Data rows 
                                     # Notez le nombre différent de NA pour chaque ROI ; chaque colonne comporte 595 lignes, mais tous les polygones n'ont pas 595 pixels ; les NA sont utilisés pour remplir les lignes "No Data". 

# do the same for the extracted lists from bands 2-4 # à faire de même pour les listes extraites des bandes 2-4 )
mosaic_crowns_b2_1_df <- data.frame(lapply(mosaic_crowns_b2_1, "length<-", max(lengths(mosaic_crowns_b2_1))))
colnames(mosaic_crowns_b2_1_df) <- paste0("Crown_",1:ncol(mosaic_crowns_b2_1_df))

mosaic_crowns_b3_1_df <- data.frame(lapply(mosaic_crowns_b3_1, "length<-", max(lengths(mosaic_crowns_b3_1))))
colnames(mosaic_crowns_b3_1_df) <- paste0("Crown_",1:ncol(mosaic_crowns_b3_1_df))

# mosaic_crowns_b4_1_df <- data.frame(lapply(mosaic_crowns_b4_1, "length<-", max(lengths(mosaic_crowns_b4_1))))
# colnames(mosaic_crowns_b4_1_df) <- paste0("Crown_",1:ncol(mosaic_crowns_b4_1_df))

#------------------------------------------------------------------------------------------------#
# add species, phenology type and any other columns of interest to data frame here (ajouter ici, l'espèce, le type de phénologie et toute autre colonne d'intérêt au cadre de données)
#------------------------------------------------------------------------------------------------#
# à faire pour toutes les bandes
mosaic_crowns_b1_1_df$GNs_spc <- crownsUTM$Gns_spc
mosaic_crowns_b1_1_df$phenology <- crownsUTM$phenology
mosaic_crowns_b1_1_df$tax_fam <- crownsUTM$tax_fam
#------------------------------------------------------------------------------------------------#



#------------------------------------------------------------------------------------------------#
# use dplyr::summarize to calculate the mean band value for each crown ( utiliser dplyr::summarize pour calculer la valeur moyenne de la bande pour chaque couronne)
#------------------------------------------------------------------------------------------------#
# use gather() to reformat data to use summarize function (utiliser la fonction gather() pour reformater les données afin d'utiliser la fonction summarize)
                                                          # à faire pour les autres bandes
mosaic_crowns_b1_1_df_long <- gather(mosaic_crowns_b1_1_df, "Crown", "B1_B")
head(mosaic_crowns_b1_1_df_long)
table(mosaic_crowns_b1_1_df_long$Crown)
# the above code converts the data from 'wide' format to 'long' format (le code ci-dessus convertit les données du format 'large' au format 'long')
mosaic_crowns_b2_1_df_long <- gather(mosaic_crowns_b2_1_df, "Crown", "B2_V")
head(mosaic_crowns_b2_1_df_long)
table(mosaic_crowns_b2_1_df_long$Crown)

mosaic_crowns_b3_1_df_long <- gather(mosaic_crowns_b3_1_df, "Crown", "B3_R")
head(mosaic_crowns_b3_1_df_long)
table(mosaic_crowns_b3_1_df_long$Crown)
#mosaic_crowns_b4_1_df_long <- gather(mosaic_crowns_b4_1_df, "Crown", "B4")

head(mosaic_crowns_b2_1_df_long)
#------------------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------------------#
# Combine all data into a single data frame (Combiner toutes les données dans un seul cadre de données)
#------------------------------------------------------------------------------------------------#
mosaic_crowns_all_1 <- cbind(mosaic_crowns_b1_1_df_long, mosaic_crowns_b2_1_df_long$B2, mosaic_crowns_b3_1_df_long$B3)
#mosaic_crowns_all_1 <- cbind(mosaic_crowns_b1_1_df_long, mosaic_crowns_b2_1_df_long$B2, mosaic_crowns_b3_1_df_long$B3, mosaic_crowns_b4_1_df_long$B4)
head(mosaic_crowns_all_1)
colnames(mosaic_crowns_all_1) <- c("Crown","B1_B","B2_V","B3_R")
#colnames(mosaic_crowns_all_1) <- c("Crown","B1","B2","B3","B4")
head(mosaic_crowns_all_1)

write.csv(mosaic_crowns_all_1, "C:/Users/elsao/Desktop/Cameroon_Data/Pierre_Antoine_Crowns_Data/rgb_2022_03_26_crowns.csv")

write.csv(mosaic_crowns_all_1, "D:/Base de données/Data/rgb_2022_03_26_crowns.csv")

#-------------------------------------------------------------------------------------------------#
# Start here
#-------------------------------------------------------------------------------------------------#

# dat <- read.csv("D:/Base de données/Data/rgb_2022_03_26_crowns.csv")
# 
# n = 100000
# dat_samp <- dat[sample(nrow(dat), n), ]
# dim(dat); dim(dat_samp)
# write.csv(dat_samp, "D:/Base de données/Data/rgb_2022_03_26_crowns_samp100000.csv")

dat <- read.csv("D:/Base de données/Data/rgb_2022_03_26_crowns_samp100000.csv")


# use summarize() to summarize all pixels within each ROI (utiliser summarize() pour résumer tous les pixels dans chaque ROI.)
Crown_dat_1 <- dat %>% group_by(Crown) %>% summarize(n=n(),
                                                     n_NAs     = sum(is.na(B1_B)),
                                                     n_pixels  = n-n_NAs,
                                                     mean_B1 = mean(B1_B, na.rm=T),
                                                     sd_B1   = sd(B1_B, na.rm=T), # deviation standard
                                                     se_B1   = sd_B1/mean_B1, # l'error standard
                                                     B1_CI   = se_B1/sqrt(n_pixels)*1.96, # l'interval de confiance 95%
                                                     mean_B2 = mean(B2_V, na.rm=T),
                                                     sd_B2   = sd(B2_V, na.rm=T),
                                                     mean_B3 = mean(B3_R, na.rm=T),
                                                     sd_B3   = sd(B3_R, na.rm=T))
Crown_dat_1
Crown_dat_1$date = "2022_03_26"

length(unique(Crown_dat_1$Crown))
dat_sub <- Crown_dat_1[1:20,]

ggplot(dat_sub, aes(Crown, mean_B1)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = mean_B1 - B1_CI, ymax = mean_B1 + B1_CI), width=0.3) + 
  theme_classic()

# check number of pixels in summarized data against Crown_dat_1 (vérifier le nombre de pixels dans les données résumées par rapport à Crown_dat_1)
Crown_dat_1$n_pixels
summary(Crown_dat_1) # same number of pixels, but in different order because Crown_dat_1 lists 1, then 10...
                     # même nombre de pixels, mais dans un ordre différent car Crown_dat_1 liste 1, puis 10...
#------------------------------------------------------------------------------------------------#


#------------------------------------------------------------------------------------------------#
# summarize again to get single EVI value per crown per month ( résumer à nouveau pour obtenir une seule valeur EVI par couronne et par mois)
#------------------------------------------------------------------------------------------------#
Crown_dat_reformat <- gather(Crown_dat_1, "band", "EVI", -c(Crown, n, n_NAs, n_pixels)) # where columns 5:8 are your band values - in this case bands, will be dates
                                                                                        # où les colonnes 5:8 sont les valeurs de vos bandes - dans ce cas, les bandes seront des dates.
head(Crown_dat_reformat)

#Crown_dat_reformat$month <- # pull month value from date value (tirer la valeur du mois de la valeur de la date)
monthly_dat <- Crown_dat_reformat %>% group_by(month) %>% summarize(n=n(), 
                                                                    mean_EVI = mean(EVI, na.rm=T))

# from here plot (ggplot - geom_point and geom_line): (à partir d'ici tracer (ggplot - geom_point et geom_line)
# y-axis = EVI
# x-axis = month
# group / color / fill = Crown 

# can add precip data to this (voir si on  peut ajouter des données de précipitation)
#------------------------------------------------------------------------------------------------#
