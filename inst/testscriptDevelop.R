# Testscript for using the R package readCzi for development  ++++++++++++++
# Author: Kai Budde
# Created: 2021/03/05
# Last changed: 2022/01/24


# ATTENTION: It is better to use/import small(er) czi files. So please do
# not save the images as scenes, whenever possible. Otherwise the allocation
# of enough disk/memory space could be a problem!


# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()

# Load packages ############################################################

list.of.packages <- c("BiocManager", "reticulate", "XML")
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
require(XML)

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
# check()

# Document package
document()

# Load package to use it
load_all()

# Please adapt the following parameters ####################################
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#input_file <- "examples/AxioImager_Test.czi"
#input_file <- "examples/Apotome_Test.czi"
#input_file <- "examples/LSM_twoChannels.czi"
#input_file <- "examples/LSM_threeChannels.czi"
#input_file <- "examples/LSM_CellBiology.czi"
input_file <- "examples/V5_BetaCatenin_385Hochst_475ActinAF488_555BetaCateninAF555_20x_30min_Stim-Scene-05-P5.czi"
#input_folder <-  "E:/LSM/"

# input_file <- "Y:/20210624_AuswertungBilderRPaket_ALT/190815/190815_EV38_2_Kollagen mit Asc u Dexa_63x_zstack_6.czi"
# convertCziToTif(input_file = input_file, convert_all_slices = T, stack_image = TRUE, stack_method = "maxprojection")
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

### Test small functions ---------------------------------------------------

# Test script for reading czi-file -----------------------------------------
#image_data <- readCzi(input_file = input_file)

# Test script for reading metadata of czi-file -----------------------------
df_metadata <- readCziMetadata(input_file = input_file)
directory_of_file <- dirname(input_file)
dir.create(paste(directory_of_file, "/output", sep=""), showWarnings = FALSE)
file_name <- gsub(pattern = "\\.czi", replacement = "", x = basename(input_file))
write.csv2(x = df_metadata, file = paste(directory_of_file, "/output/", file_name, "_df_metadata_de.csv", sep=""),
           row.names=FALSE)
write.csv(x = df_metadata, file = paste(directory_of_file, "/output/", file_name, "_df_metadata_en.csv", sep=""),
           row.names=FALSE)

# Test script for converting a czi file into a tif file --------------------
convertCziToTif(input_file = input_file)

# ### Test with files in folder ----------------------------------------------
#
# czi_files <- list.files(path = input_folder, pattern = "\\.czi", full.names = TRUE)
#
# for(file_number in 1:length(czi_files)){
#   convertCziToTif(input_file = czi_files[file_number])
#
#   if(file_number == 1){
#     df_metadata <- readCziMetadata(input_file <- czi_files[file_number])
#   }else{
#     df_dummy <- readCziMetadata(input_file <- czi_files[file_number])
#     df_metadata <- rbind(df_metadata, df_dummy)
#   }
# }
#
# #save(df_metadata, file = paste(input_folder, "/output/df_metadata.Rda", sep=""))
# write.csv2(x = df_metadata, file = paste(input_folder, "/output/df_metadata_de.csv", sep=""),
#            row.names = FALSE)

### Convert cilia images from directory ------------------------------------

# czi_files <- list.files(path = input_folder, pattern = "\\.czi", full.names = TRUE)
#
# for(file_number in 1:length(czi_files)){
#   convertCziToTif(input_file = czi_files[file_number], convert_all_slices = FALSE,
#                   stack_method = "maxprojection", change_layers = "red<->green")
#
#   if(file_number == 1){
#     df_metadata <- readCziMetadata(input_file <- czi_files[file_number])
#   }else{
#     df_dummy <- readCziMetadata(input_file <- czi_files[file_number])
#     df_metadata <- rbind(df_metadata, df_dummy)
#   }
# }
#
# save(df_metadata, file = paste(input_folder, "/output/df_metadata.Rda", sep=""))
# write.csv2(x = df_metadata, file = paste(input_folder, "/output/df_metadata_de.csv", sep=""))

