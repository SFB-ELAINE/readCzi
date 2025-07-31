# Testscript for using the R package readCzi +++++++++++++++++++++++++++++++
# Author: Kai Budde-Sagert
# Created: 2021/04/08
# Last changed: 2025//17


# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()

# Load packages ############################################################
groundhog.day <- "2024-12-01"
if(!any(grepl(pattern = "groundhog", x = installed.packages(), ignore.case = TRUE))){
  install.packages("groundhog")
}

# Load packages
library(groundhog)
pkgs <- c("BiocManager", "devtools", "magick", "reticulate",
          "tibble", "xml2")
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

# Install this R package for reading CZI images
devtools::install_github("SFB-ELAINE/readCzi", upgrade = "ask")
require(readCzi)
# Alternatively:
# devtools::load_all()
# devtools::document()
# devtools::check()

# Please adapt the following parameters ####################################
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Input file
input_file <- system.file("extdata", "LSM_threeChannels.czi",
                          package = "readCzi", mustWork = TRUE)

# Examples for testing
# input_file <- system.file("examplesForTesting", "Apotome_Test.czi",
#                           package = "readCzi", mustWork = TRUE)
# input_file <- system.file("examplesForTesting", "AxioImager_Test.czi",
#                           package = "readCzi", mustWork = TRUE)
# input_file <- system.file("examplesForTesting", "AxioImager_Test2.czi",
#                           package = "readCzi", mustWork = TRUE)
# input_file <- system.file("examplesForTesting", "LSM_oneChannel.czi",
#                           package = "readCzi", mustWork = TRUE)
# input_file <- system.file("examplesForTesting", "LSM_twoChannels.czi",
#                           package = "readCzi", mustWork = TRUE)
# input_file <- system.file("examplesForTesting", "LSM_threeChannels.czi",
#                           package = "readCzi", mustWork = TRUE)
# input_file <- system.file("examplesForTesting", "LSM_twoTracksThreeChannels.czi",
#                           package = "readCzi", mustWork = TRUE)
# input_file <- system.file("examplesForTesting", "LSM_withTransmission.czi",
#                           package = "readCzi", mustWork = TRUE)


# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

## 1. Read czi file and store as array -------------------------------------
image_data <- readCzi(input_file = input_file)

## 2. Read metadata of czi file and save as CSV ----------------------------
##    (in German and English format)
df_metadata <- readCziMetadata(input_file = input_file,
                               save_metadata = TRUE)

file_path_de <- file.path(dirname(input_file), "output",
                          paste0(gsub("\\.czi", "", basename(input_file)), "_df_metadata_de.csv"))
file_path_en <- file.path(dirname(input_file), "output",
                          paste0(gsub("\\.czi", "", basename(input_file)), "_df_metadata_en.csv"))

dir.create(path = file.path(dirname(input_file), "output"), showWarnings = FALSE)
readr::write_csv(x = df_metadata, file = file_path_en)
readr::write_csv2(x = df_metadata, file = file_path_de)

## 3. Convert czi file to tifs ---------------------------------------------
# convertCziToTif(input_file = input_file) # Use this for examples for testing

convertCziToTif(input_file = input_file,
                convert_all_slices = TRUE)
print(paste0("The results can be found here: ",
             file.path(dirname(input_file),"output")))

## 4. Copy results to doc directory for README -----------------------------
image_files <- list.files(path = system.file("extdata", "output",
                                             package = "readCzi",
                                             mustWork = TRUE),
                          pattern = "tif", full.names = TRUE)

for(i in seq_along(image_files)){
  image_file <- magick::image_read(image_files[i])
  resized_image <- magick::image_scale(image = image_file, geometry = "800x800")
  image_path <- gsub(pattern = "\\.tif", replacement = "_small.jpg", x = basename(image_files[i]))
  image_path <-  file.path(
    system.file("docs", package = "readCzi", mustWork = TRUE),
    image_path)
  magick::image_write(image = resized_image,
                      path = image_path, format = "jpg")
}

rm(list = c("image_files", "image_file", "resized_image", "image_path", "i"))
