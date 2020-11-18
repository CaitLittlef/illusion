###########
## SETUP ##
###########

#####################################
## Set working directory 
setwd("C:/Users/clittlef/Google Drive/2Illusion/illusion") # If working with/within drive
wd <- "C:/Users/clittlef/Google Drive/2Illusion/illusion/"
data.dir <- "C:/Users/clittlef/Google Drive/2Illusion/illusion/data/"
out.dir <- "C:/Users/clittlef/Google Drive/2Illusion/illusion/out/"



#####################################
# Install packages if not already installed
required.packages <- c("plyr", "ggplot2", "gridExtra", "raster", "sf", "rgdal", "dplyr",
                       "tidyverse", "maptools", "rgeos", 
                       "partykit", "vcd", "maps", "mgcv", "tmap",
                       "MASS", "pROC", "ResourceSelection", "caret", "broom", "boot",
                       "dismo", "gbm", "usdm", "pscl", "randomForest", "pdp", "classInt", "plotmo",
                       "ggspatial", "lmtest",  "dynatopmodel", "spatialEco")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) install.packages(new.packages)
rm(required.packages, new.packages)

# Libraries
# library(plyr)
library(ggplot2)
library(gridExtra)
library(raster)
# library(sp)
library(sf)
library(rgdal)
library(dplyr)
library(tidyverse)
library(maptools)
library(rgeos)
library(partykit)
library(vcd)
library(maps)
library(mgcv)
library(tmap)
library(MASS)
library(pROC)
library(ResourceSelection)
library(caret)
library(broom)
library(boot)
library(dismo)
library(gbm)
library(usdm)
library(pscl)
library(randomForest)
library(pdp)
library(classInt)
library(plotmo)
library(ggspatial)
library(lmtest)
library(dynatopmodel)
library(spatialEco)
library(RColorBrewer)

# rm(GCtorture)

#####################################
# Turn off scientific notation
options(scipen=999) 


#####################################
# Grab date for saving files
currentDate <- Sys.Date()



palette <- c("aquamarine4", "coral2", "aquamarine2")
palette <- colorRampPalette(c("aquamarine2", "coral2"))


display.brewer.all(colorblindFriendly = TRUE)
display.brewer.pal(8, "Dark2")
palette <- brewer.pal(8, "Dark2")

#  teal      orange    purple    pink      green     yellow    brown     grey      
#"#1B9E77" "#D95F02" "#7570B3" "#E7298A" "#66A61E" "#E6AB02" "#A6761D" "#666666"