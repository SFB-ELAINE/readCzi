# Testscript for using the R package readCzi +++++++++++++++++++++++++++++++
# Author: Kai Budde
# Created: 2021/04/08
# Last changed: 2021/04/08


# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()

# Load packages ############################################################

list.of.packages <- c("BiocManager", "reticulate")
#list.of.packages <- c("tiff", "dplyr", "devtools", "BiocManager", "xlsx", "zis", "reticulate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

if(!("EBImage" %in% utils::installed.packages())){
  print("Installing EBImage.")
  BiocManager::install("EBImage")
  #BiocManager::install("MaxContrastProjection")
}

# Install the R package for reading czi images
if(!("readCzi" %in% installed.packages()[,"Package"])){
  print("Installing readCzi.")
  devtools::install_github("SFB-ELAINE/readCzi", ref = "v0.1.2")
}

require(devtools)
require(EBImage)
require(reticulate)
require(readCzi)

# Read in Python package for reading czi files
# (Users will be asked to install miniconda
# when starting for the first time)
reticulate::py_install("czifile")



# Please adapt the following parameters ####################################
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
input_file <- "tests/image1.czi"
#input_folder <- "tests/"

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



### Test with files --------------------------------------------------------

# Test script for reading czi-file -----------------------------------------
image_data <- readCzi(input_file <- input_file)

# Test script for reading metadata of czi-file -----------------------------
df_metadata <- readCziMetadata(input_file <- input_file)

# Test script for converting a czi file into a tif file --------------------
convertCziToTif(input_file <- input_file)

### Test with files in folder ----------------------------------------------

# czi_files <- list.files(path = input_folder, pattern = "\\.czi", full.names = TRUE)
#
# for(i in 1:length(czi_files)){
#   convertCziToTif(input_file = czi_files[i])
#
#   if(i == 1){
#     df_metadata <- readCziMetadata(input_file <- czi_files[i])
#   }else{
#     df_dummy <- readCziMetadata(input_file <- czi_files[i])
#     df_metadata <- rbind(df_metadata, df_dummy)
#   }
# }
#
#save(df_metadata, file = paste(input_folder, "output/df_metadata.Rda", sep=""))
#write.csv2(x = df_metadata, file = paste(input_folder, "output/df_metadata_de.csv", sep=""))


