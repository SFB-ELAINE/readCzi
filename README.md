# readCzi
readCZI is an R package to read czi image files (images from ZEISS microscopes), stack them if needed, convert them to tifs (also normalized and histogram equalized), and store the metadata.

## Examples
The following examples (script and results) show the capabilities of the package.


```R

# Using the R package readCzi to convert a czi file ++++++++++++++++++++++++
# Author: Kai Budde
# Created: 2021/10/08
# Last changed: 2021/10/08


# ATTENTION: It is better to use/import small(er) czi files. So please do
# not save the images as scenes, whenever possible. Otherwise the allocation
# of enough disk/memory space could be a problem!


# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()

# Load packages ############################################################

list.of.packages <- c("BiocManager", "reticulate", "XML", "devtools")
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

# Read in Python package for reading czi files
# (Users will be asked to install miniconda
# when starting for the first time)
reticulate::py_install("czifile")

# Install readCzi from GitHub ##############################################
devtools::install_github(repo = "https://github.com/SFB-ELAINE/readCzi")
require(readCzi)

# Using the readCzi package for plain conversion ###########################

# Input file ---------------------------------------------------------------
input_file <- "examples/AxioImager_Test.czi"

# Writing dataframe with metadata and converting czi to tif ----------------

# Reading metadata of czi-file ---------------------------------------------
# Test script for reading metadata of czi-file -----------------------------
df_metadata <- readCziMetadata(input_file = input_file)
directory_of_file <- dirname(input_file)
dir.create(paste(directory_of_file, "/output", sep=""), showWarnings = FALSE)
file_name <- gsub(pattern = "\\.czi", replacement = "", x = basename(input_file))
write.csv2(x = df_metadata, file = paste(directory_of_file, "/output/", file_name, "_df_metadata_de.csv", sep=""),
           row.names=FALSE)
write.csv(x = df_metadata, file = paste(directory_of_file, "/output/", file_name, "_df_metadata_en.csv", sep=""),
           row.names=FALSE)
           
# Converting a czi file into a tif file ------------------------------------
convertCziToTif(input_file = input_file)

# Reading czi-file ---------------------------------------------------------
#image_data <- readCzi(input_file = input_file)


# Using the readCzi package for converting mulptiple z-stack layers ########

# Input file ---------------------------------------------------------------
input_file <- "examples/LSM_twoChannels.czi"


# Converting every layer of a z-stack czi file into a tif file -------------
convertCziToTif(input_file = input_file,
                convert_all_slices = TRUE,
                stack_image = TRUE,
                stack_method = "average",
                higher_contrast_slices = FALSE,
                higher_contrast_stack = TRUE,
                normalize_stack = TRUE,
                change_layers = "none")


```

The results are the following:

**1. AxioImager_Test.czi**

  * Original image:
    ![Original converted AxioImager image](https://github.com/SFB-ELAINE/readCzi/blob/main/examples/output_readme/output_AxioImager_Test/AxioImager_Test_small.jpg?raw=true)
  
  * Histogram equalized image (using the clahe (Contrast Limited Adaptive Histogram Equalization) method of the [EBImage R package](https://rdrr.io/bioc/EBImage/man/clahe.html)):
    ![Histogram equalized AxioImager image](https://github.com/SFB-ELAINE/readCzi/blob/main/examples/output_readme/output_AxioImager_Test/AxioImager_Test_histogram_equalized_small.jpg?raw=true)
  
  * Normalized image (normalizing each channel separately):
    ![Normalized AxioImager image](https://github.com/SFB-ELAINE/readCzi/blob/main/examples/output_readme/output_AxioImager_Test/AxioImager_Test_normalized_small.jpg?raw=true)
  
**2. LSM_Test_twoChannels.czi**

  * Original images (layers + z-stack (average method)):
  
    * Z-stack (average method):
    ![Original converted LSM image (z-stack)](https://github.com/SFB-ELAINE/readCzi/blob/main/examples/output_readme/output_LSM_twoChannels/LSM_twoChannels_zstack_small.jpg?raw=true)
    
    * Layer 1:
    ![Original converted LSM image (layer 1)](https://github.com/SFB-ELAINE/readCzi/blob/main/examples/output_readme/output_LSM_twoChannels/LSM_twoChannels_z1_small.jpg?raw=true)
    
    * Layer 2:
    ![Original converted LSM image (layer 2)](https://github.com/SFB-ELAINE/readCzi/blob/main/examples/output_readme/output_LSM_twoChannels/LSM_twoChannels_z2_small.jpg?raw=true)
    
    * Layer 3:
    ![Original converted LSM image (layer 3)](https://github.com/SFB-ELAINE/readCzi/blob/main/examples/output_readme/output_LSM_twoChannels/LSM_twoChannels_z3_small.jpg?raw=true)
    
    * Layer 4:
    ![Original converted LSM image (layer 4)](https://github.com/SFB-ELAINE/readCzi/blob/main/examples/output_readme/output_LSM_twoChannels/LSM_twoChannels_z4_small.jpg?raw=true)
    
    * Layer 5:
    ![Original converted LSM image (layer 5)](https://github.com/SFB-ELAINE/readCzi/blob/main/examples/output_readme/output_LSM_twoChannels/LSM_twoChannels_z5_small.jpg?raw=true)
    
    * Layer 6:
    ![Original converted LSM image (layer 6)](https://github.com/SFB-ELAINE/readCzi/blob/main/examples/output_readme/output_LSM_twoChannels/LSM_twoChannels_z6_small.jpg?raw=true)
    
    * Layer 7:
    ![Original converted LSM image (layer 7)](https://github.com/SFB-ELAINE/readCzi/blob/main/examples/output_readme/output_LSM_twoChannels/LSM_twoChannels_z7_small.jpg?raw=true)
    
    * Layer 8:
    ![Original converted LSM image (layer 8)](https://github.com/SFB-ELAINE/readCzi/blob/main/examples/output_readme/output_LSM_twoChannels/LSM_twoChannels_z8_small.jpg?raw=true)
    
    * Layer 9:
    ![Original converted LSM image (layer 9)](https://github.com/SFB-ELAINE/readCzi/blob/main/examples/output_readme/output_LSM_twoChannels/LSM_twoChannels_z9_small.jpg?raw=true)
    
    * Layer 10:
    ![Original converted LSM image (layer 10)](https://github.com/SFB-ELAINE/readCzi/blob/main/examples/output_readme/output_LSM_twoChannels/LSM_twoChannels_z10_small.jpg?raw=true)
    
    * Layer 11:
    ![Original converted LSM image (layer 11)](https://github.com/SFB-ELAINE/readCzi/blob/main/examples/output_readme/output_LSM_twoChannels/LSM_twoChannels_z11_small.jpg?raw=true)
    
    * Layer 12:
    ![Original converted LSM image (layer 12)](https://github.com/SFB-ELAINE/readCzi/blob/main/examples/output_readme/output_LSM_twoChannels/LSM_twoChannels_z12_small.jpg?raw=true)
    
    * Layer 13:
    ![Original converted LSM image (layer 13)](https://github.com/SFB-ELAINE/readCzi/blob/main/examples/output_readme/output_LSM_twoChannels/LSM_twoChannels_z13_small.jpg?raw=true)

  * Histogram equalized z-stack image (using the clahe method of the [EBImage R package](https://rdrr.io/bioc/EBImage/man/clahe.html)):
    ![Histogram equalized LSM z-stack image](https://github.com/SFB-ELAINE/readCzi/blob/main/examples/output_readme/output_LSM_twoChannels/LSM_twoChannels_zstack_histogram_equalized_small.jpg?raw=true)
  
  * Normalized image (normalizing each channel separately):
    ![Normalized LSM z-stack image](https://github.com/SFB-ELAINE/readCzi/blob/main/examples/output_readme/output_LSM_twoChannels/LSM_twoChannels_zstack_normalized_small.jpg?raw=true)

(The jpgs were created with GIMP.)
