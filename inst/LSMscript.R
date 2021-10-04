# Script for working with LSM images  ++++++++++++++++++++++++++++++++++++++
# Author: Kai Budde
# Created: 2021/09/09
# Last changed: 2021/09/30


# ATTENTION: It is better to use/import small(er) czi files. So please do
# not save the images as scenes, whenever possible. Otherwise the allocation
# of enough disk/memory space could be a problem!


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

require(devtools)
require(EBImage)
require(reticulate)

#require(tiff)
#require(dplyr)
#require(xlsx)

# Read in Python package for reading czi files
# (Users will be asked to install miniconda
# when starting for the first time)
reticulate::py_install("czifile")

# Install the R package for producing stacks of the images
#if(!("stackImages" %in% installed.packages()[,"Package"])){
#  if(!installed.packages()[,"Version"][installed.packages()[,"Package"] == "stackImages"] == "0.1.4"){
#    devtools::install_github("SFB-ELAINE/stackImages", ref = "v0.1.4")
#  }
#}
#require(stackImages)

# Check package
#check()

# Document package
#document()

# Load package to use it
load_all()

# Please adapt the following parameters ####################################
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
input_folder <-  "E:/LSM"
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

### Test small functions ---------------------------------------------------

# # Test script for reading czi-file -----------------------------------------
# image_data <- readCzi(input_file <- input_file)
#
# # Test script for reading metadata of czi-file -----------------------------
#df_metadata <- readCziMetadata(input_file <- input_file)
#
# # Test script for converting a czi file into a tif file --------------------
# convertCziToTif(input_file <- input_file, convert_all_slices = FALSE)

### Files in folder ----------------------------------------------
czi_files <- list.files(path = input_folder, pattern = "\\.czi", full.names = TRUE)

for(file_number in 1:length(czi_files)){
  convertCziToTif(input_file = czi_files[file_number], convert_all_slices = FALSE, stack_image = TRUE, stack_method = "average")

  if(file_number == 1){
    df_metadata <- readCziMetadata(input_file <- czi_files[file_number])
  }else{
    df_dummy <- readCziMetadata(input_file <- czi_files[file_number])
    df_metadata <- rbind(df_metadata, df_dummy)
  }
}

save(df_metadata, file = paste(input_folder, "/output/df_metadata.Rda", sep=""))
write.csv2(x = df_metadata, file = paste(input_folder, "/output/df_metadata_de.csv", sep=""))

### Convert cilia images from directory ------------------------------------

czi_files <- list.files(path = input_folder, pattern = "\\.czi", full.names = TRUE)

for(file_number in 1:length(czi_files)){
  convertCziToTif(input_file = czi_files[file_number], convert_all_slices = FALSE,
                  stack_method = "maxprojection", change_layers = "red<->green")

  if(file_number == 1){
    df_metadata <- readCziMetadata(input_file <- czi_files[file_number])
  }else{
    df_dummy <- readCziMetadata(input_file <- czi_files[file_number])
    df_metadata <- rbind(df_metadata, df_dummy)
  }
}

save(df_metadata, file = paste(input_folder, "/output/df_metadata.Rda", sep=""))
write.csv2(x = df_metadata, file = paste(input_folder, "/output/df_metadata_de.csv", sep=""))


