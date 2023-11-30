# readCzi
**readCzi** is an R package to read CZI image files (images from ZEISS microscopes), stack them if needed (projection), convert the files to tifs (also normalized and histogram equalized), and store the metadata a CSV files.

## Readable image/microscopy formats

The R package **readCzi** has been checked with image data from
 * ZEISS Apotome,
 * ZEISS AxioImager,
 * ZEISS laser scanning microscopes (1 to 3 channels, 1 to 3 tracks, 1 to n z layers).
 Please contact Kai for further information and sample data.

## Example
The following example (script and results) shows the capabilities of the package.

```R
# Testscript for using the R package readCzi +++++++++++++++++++++++++++++++
# Author: Kai Budde-Sagert
# Created: 2021/04/08
# Last changed: 2023/11/29


# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()

# Load packages ############################################################
groundhog.day <- "2023-01-01"
if(!any(grepl(pattern = "groundhog", x = installed.packages(),ignore.case = TRUE))){
  install.packages("groundhog")
}

# Load packages
library(groundhog)
pkgs <- c("BiocManager", "devtools", "reticulate", "tibble", "xml2")
groundhog.library(pkgs, groundhog.day)

if(!("EBImage" %in% utils::installed.packages())){
  print("Installing EBImage.")
  BiocManager::install("EBImage")
}

require(EBImage)

# Install Python package for reading CZI files
# (Users will be asked to install miniconda when starting for the first time)
if(! "czifile" %in% reticulate::py_list_packages()$package){
  reticulate::py_install("czifile")
}

# Install this R package for reading CZI images in R
if(!("readCzi" %in% installed.packages()[,"Package"])){
  devtools::install_github("SFB-ELAINE/readCzi")
}
require(readCzi)

# Please adapt the following parameters ####################################
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Input file
input_file <- system.file("extdata", "LSM_threeChannels.czi",
                          package = "readCzi", mustWork = TRUE)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

## 1. Read czi file and store as array -------------------------------------
image_data <- readCzi(input_file = input_file)

## 2. Read metadata of czi file and save as CSV ----------------------------
##    (in German and English format)
df_metadata <- readCziMetadata(input_file = input_file,
                               save_metadata = TRUE)

file_path_de <- file.path(dirname(input_file), "output",
                          paste0(gsub("\\.czi", "", basename(input_file)),
                          "_df_metadata_de.csv"))
file_path_en <- file.path(dirname(input_file), "output",
                          paste0(gsub("\\.czi", "", basename(input_file)),
                          "_df_metadata_en.csv"))

dir.create(path = file.path(dirname(input_file), "output"),
showWarnings = FALSE)
readr::write_csv(x = df_metadata, file = file_path_en)
readr::write_csv2(x = df_metadata, file = file_path_de)

## 3. Convert czi file to tifs ---------------------------------------------
convertCziToTif(input_file = input_file,
                convert_all_slices = TRUE)

```

The results are the following:

**1. AxioImager_Test.czi**

  * Original image:
    ![Original converted AxioImager image](https://github.com/SFB-ELAINE/readCzi/blob/main/examples/output_readme/output_AxioImager_Test/AxioImager_Test_small.jpg?raw=true)
  
  * Histogram equalized image (using the clahe (Contrast Limited Adaptive Histogram Equalization) method of the [EBImage R package](https://rdrr.io/bioc/EBImage/man/clahe.html)):
    ![Histogram equalized AxioImager image](https://github.com/SFB-ELAINE/readCzi/blob/main/examples/output_readme/output_AxioImager_Test/AxioImager_Test_histogram_equalized_small.jpg?raw=true)
  
  * Normalized image (normalizing each channel separately):
    ![Normalized AxioImager image](https://github.com/SFB-ELAINE/readCzi/blob/main/examples/output_readme/output_AxioImager_Test/AxioImager_Test_normalized_small.jpg?raw=true)
  
**2. LSM_threeChannels.czi**

  * Original images (z-stack layers + projection (average method)):
  
    * Layer 1:
    ![Original converted LSM image (z-stack layer 1)](https://github.com/SFB-ELAINE/readCzi/blob/main/docs/LSM_threeChannels_z1_small.jpg?raw=true)
    * Layer 2:
    ![Original converted LSM image (z-stack layer 2)](https://github.com/SFB-ELAINE/readCzi/blob/main/docs/LSM_threeChannels_z2_small.jpg?raw=true)
    * Layer 3:
    ![Original converted LSM image (z-stack layer 3)](https://github.com/SFB-ELAINE/readCzi/blob/main/docs/LSM_threeChannels_z3_small.jpg?raw=true)
    * Layer 4:
    ![Original converted LSM image (z-stack layer 4)](https://github.com/SFB-ELAINE/readCzi/blob/main/docs/LSM_threeChannels_z4_small.jpg?raw=true)
    * Layer 5:
    ![Original converted LSM image (z-stack layer 5)](https://github.com/SFB-ELAINE/readCzi/blob/main/docs/LSM_threeChannels_z5_small.jpg?raw=true)
    * Layer 6:
    ![Original converted LSM image (z-stack layer 6)](https://github.com/SFB-ELAINE/readCzi/blob/main/docs/LSM_threeChannels_z6_small.jpg?raw=true)
    * Layer 7:
    ![Original converted LSM image (z-stack layer 7)](https://github.com/SFB-ELAINE/readCzi/blob/main/docs/LSM_threeChannels_z7_small.jpg?raw=true)
    * ...
    * Layer 17:
    ![Original converted LSM image (z-stack layer 17)](https://github.com/SFB-ELAINE/readCzi/blob/main/docs/LSM_threeChannels_z17_small.jpg?raw=true)
    * Layer 18:
    ![Original converted LSM image (z-stack layer 18)](https://github.com/SFB-ELAINE/readCzi/blob/main/docs/LSM_threeChannels_z18_small.jpg?raw=true)
    * Layer 19:
    ![Original converted LSM image (z-stack layer 19)](https://github.com/SFB-ELAINE/readCzi/blob/main/docs/LSM_threeChannels_z19_small.jpg?raw=true)
    * Layer 20:
    ![Original converted LSM image (z-stack layer 20)](https://github.com/SFB-ELAINE/readCzi/blob/main/docs/LSM_threeChannels_z20_small.jpg?raw=true)
    * Layer 21:
    ![Original converted LSM image (z-stack layer 21)](https://github.com/SFB-ELAINE/readCzi/blob/main/docs/LSM_threeChannels_z21_small.jpg?raw=true)
    * Layer 22:
    ![Original converted LSM image (z-stack layer 22)](https://github.com/SFB-ELAINE/readCzi/blob/main/docs/LSM_threeChannels_z22_small.jpg?raw=true)
    * Layer 23:
    ![Original converted LSM image (z-stack layer 23)](https://github.com/SFB-ELAINE/readCzi/blob/main/docs/LSM_threeChannels_z23_small.jpg?raw=true)
    * Layer 24:
    ![Original converted LSM image (z-stack layer 24)](https://github.com/SFB-ELAINE/readCzi/blob/main/docs/LSM_threeChannels_z24_small.jpg?raw=true)
    * Layer 25:
    ![Original converted LSM image (z-stack layer 25)](https://github.com/SFB-ELAINE/readCzi/blob/main/docs/LSM_threeChannels_z25_small.jpg?raw=true)
    * Z-stack projection (average method):
    ![Original converted LSM image (z-stack)](https://github.com/SFB-ELAINE/readCzi/blob/main/docs/LSM_threeChannels_zstack_small.jpg?raw=true)
    * Histogram equalized z-stack projection (using the clahe method of the [EBImage R package](https://rdrr.io/bioc/EBImage/man/clahe.html)):
    ![Histogram equalized LSM z-stack image](https://github.com/SFB-ELAINE/readCzi/blob/main/docs/LSM_threeChannels_zstack_histogram_equalized_small.jpg?raw=true)
    * Normalized histogram equalized z-stack projection (normalizing each channel separately):
    ![Normalized LSM z-stack image](https://github.com/SFB-ELAINE/readCzi/blob/main/docs/LSM_threeChannels_zstack_histogram_equalized_normalized_small.jpg?raw=true)

(The jpgs were created with the R package 'magick'. Please see ´inst/testscript.R´)
