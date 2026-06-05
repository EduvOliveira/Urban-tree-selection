#' ---
# Title: Functional and climate-based selection of # tree  species for resilient urban afforestation 
# By Eduardo V. S. Oliveira
# June 5, 2026
#' ---

## Habitat suitability models of the natives species for urban planting

##################################################
# To run the models, ensure that:
# (1) The "maxent.jar" file is in the "java"      # folder of the "dismo" package.
# (2) Java is installed on the computer.
##################################################

if(!require(raster)) install.packages("raster")
if(!require(sp)) install.packages("sp")
if(!require(virtualspecies)) install.packages("virtualspecies")
if(!require(openxlsx)) install.packages("openxlsx")
if(!require(dplyr)) install.packages("dplyr")
if(!require(rJava)) install.packages("rJava")
if(!require(SSDM)) install.packages("SSDM")

# Load libraries

library(raster)
library(sp)
library(virtualspecies)
library(openxlsx)
library(dplyr)
library(rJava)
library(SSDM)

# Checking Collinearity

setwd("insert_the_path") 

lst <- list.files(path=".",pattern='tif$',full.names = T) 
preds<-stack(lst)
names(preds)

var<-removeCollinearity(preds, multicollinearity.cutoff = 0.70, select.variables = TRUE, sample.points = FALSE, plot = TRUE)

write.xlsx(as.data.frame(var),"preds_indep.xlsx")

# Load data

var_path<-"insert_the_path"

envi<-load_var(path = var_path, files = c("bio2.tif","bio4.tif","bio11.tif","bio15.tif","bio17.tif","bio18.tif","bio19.tif","clay.tif","ocd.tif","elev.tif","ph.tif","textu.tif","silt.tif"), format = ".tif")

path.occ<-setwd("insert_the_path") 

occ<-load_occ(path = path.occ, envi, file = "SPP.txt", sep = '\t', Xcol = 'LONGITUDE', Ycol = 'LATITUDE', Spcol = 'SPECIES')

# Plotting occurrence points

pol<-shapefile(choose.files()) 

plot(pol)
points(occ$lon, occ$lat, col='red', pch=20, cex=0.75) 

plot(envi$bio12)
points(occ$lon, occ$lat, col='red', pch=20, cex=0.5) 

# Running the models

my_path<-"insert_the_path"

setwd(paste(my_path)) 

dir.create("PROJ_actual")

path.model<-setwd(paste(my_path,"/PROJ_actual", sep="")) 

t1=Sys.time()

SSDM <- SSDM::stack_modelling('MAXENT', occ, envi, name = "modelo1", save = TRUE,path = path.model,rep = 100, rep = 100,cv = "k-fold", cv.param = c(4, 5),uncertainty = FALSE, ensemble.metric = "AUC",ensemble.thresh = 0.7,bin.thresh = "SES")

t2=Sys.time()
t2-t1 

# Accessing the results

plot(SSDM)

class(SSDM)

# Projecting to future climate scenarios

names(envi)

setwd(paste(my_path,"/SSP245_2050", sep="")) #Change the folder according to each scenario  

var_proj<-"insert_the_path"

envi_proj<-load_var(path = var_proj, files = c("bio2.tif","bio4.tif","bio11.tif","bio15.tif","bio17.tif","bio18.tif","bio19.tif","clay.tif","ocd.tif","elev.tif","ph.tif","textu.tif","silt.tif"), format = ".tif")

path.model<-setwd(paste(my_path,"/PROJ_future", sep="")) 

r_proj<-SSDM::project(obj=SSDM, Env=envi_proj, output.format = 'rasters',uncertainty = FALSE,SDM.projections = TRUE)

# Accessing the results

plot(r_proj$diversity.map)

plot(r_proj$esdms$Byrsonima_gardneriana.Ensemble.SDM$binary)


##THE END##

rm(list=ls()) 
