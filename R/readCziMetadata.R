#' @title readCziMetadata
#' @description Reads and saves key values from czi metadata as well as
#' entire metadata as txt
#' @details This functions saves the metadata of a czi file as txt and also
#' extract the most important information and saves it in a data frame.
#' @aliases readcziMetadata readCzimetadata readCZIMetadata
#' @author Kai Budde
#' @export readCziMetadata
#' @param input_file A character (path to czi file to be read)
#' @return A data frame with key information from the metadata is returned.

readCziMetadata <- function(input_file = NULL) {

  if(is.null(input_file)){
    print("Please call function with input file.")
    return()
  }

  # Get the directory and file names (without the path and ".czi" ending)
  directory_of_file <- dirname(input_file)
  file_name <- basename(input_file)
  image_name_wo_czi <- gsub("\\.czi", "", file_name)

  # Import module for czi files
  zis <- reticulate::import("czifile")

  czi_class <- zis$CziFile(input_file)
  metadata <- czi_class$metadata(czi_class)

  # Save metadata in txt file
  utils::write.table(metadata, file = paste(directory_of_file, "/",
                                            image_name_wo_czi,
                                            "_metadata.txt", sep = ""),
                     sep = "", row.names = FALSE,
                     col.names = FALSE, quote = FALSE)

  # Save the dimension information
  # C: Channel in a Multi-Channel data set
  # X: Pixel index / offset in the X direction. Used for tiled images (width)
  # Y: Pixel index / offset in the y direction. Used for tiled images (height)
  # Z: Slice (depth)
  # T: Time
  # R: Rotation,
  # S: Scene (contiguous regions of interest in a mosaic image)
  # I: Illumination (direction)
  # B: (Acquisition) Block index in segmented experiments.
  # M: Mosaic (index of tile for compositing a scene)
  # H: Phase e.g., (Airy detector fibers)
  # V: View (e.g., for SPIM)
  # 0: Sample (e.g., RGBA)


  # Compression:
  # 0: Uncompressed
  # 1: JpgFile
  # 2: LZW
  # 4: JpegXrFile
  # 100 and up: camera/system specific specific RAW data



  # General microscopy information #########################################

  # Main information is found below <Information>

  # Microscope name
  if(grepl(pattern = "<Microscopes>.+Name=.+</Microscopes>", x = metadata, ignore.case = TRUE)){
    microscope_name <- gsub(pattern = ".+<Microscopes>.+<Microscope.+Name=\"(.+)\".+</Microscopes>.+",
                            replacement = "\\1", x = metadata)
  }else{
    microscope_name <- NA
  }

  # Acquisition start time
  if(grepl(pattern = "<StartTime>", x = metadata, ignore.case = TRUE)){
    acquisition_time <- gsub(pattern = ".+<StartTime>(.+)</StartTime>.+",
                       replacement = "\\1", x = metadata)

    acquisition_date <- gsub(pattern = "(.+)T.+",
                             replacement = "\\1",
                             x = acquisition_time)
    acquisition_time <- gsub(pattern = ".+T(.+)Z",
                             replacement = "\\1",
                             x = acquisition_time)
  }else{
    acquisition_date <- NA
    acquisition_time <- NA
  }

  # Device Mode (LSM only)
  if(grepl(pattern = "<System>", x = metadata, ignore.case = TRUE)){
    microscopy_system <- gsub(pattern = ".+<System>(.+)</System.+",
                              replacement = "\\1", x = metadata)
  }else{
    microscopy_system <- NA
  }

  # Image information ######################################################

  # Magnification of objective
  if(grepl(pattern = "<NominalMagnification>", x = metadata, ignore.case = TRUE)){
    objective_magnification <- gsub(pattern = ".+<NominalMagnification>(.+)</NominalMagnification>.+",
                                    replacement = "\\1", x = metadata)
  }else{
    objective_magnification <- NA
  }

  # Scaling (in \mu m or m)
  if(grepl(pattern = "scaling_x", x = metadata, ignore.case = TRUE)){
    scaling_x <- gsub(pattern = ".+<ScalingX>(.+)</ScalingX>.+",
                      replacement = "\\1", x = metadata)
    scaling_x <- tolower(scaling_x)
    scaling_x <- as.numeric(scaling_x)
    scaling_x <- scaling_x*1e6
  }else if(grepl(pattern = "<Distance Id=\"X\">", x = metadata, ignore.case = TRUE)){
    scaling_x <- gsub(pattern = ".+<Distance Id=\"X\">.+<Value>(.{1,35})</Value>.+</Distance>.+",
                      replacement = "\\1", x = metadata)
    scaling_x <- tolower(scaling_x)
    scaling_x <- as.numeric(scaling_x)
  }else{
    scaling_x <- NA
  }

  if(grepl(pattern = "ScalingY", x = metadata, ignore.case = TRUE)){
    scaling_y <- gsub(pattern = ".+<ScalingY>(.+)</ScalingY>.+",
                      replacement = "\\1", x = metadata)
    scaling_y <- tolower(scaling_y)
    scaling_y <- as.numeric(scaling_y)
    scaling_y <- scaling_y*1e6
  }else if(grepl(pattern = "<Distance Id=\"Y\">", x = metadata, ignore.case = TRUE)){
    scaling_y <- gsub(pattern = ".+<Distance Id=\"Y\">.+<Value>(.{1,35})</Value>.+</Distance>.+",
                      replacement = "\\1", x = metadata)
    scaling_y <- tolower(scaling_y)
    scaling_y <- as.numeric(scaling_y)
  }else{
    scaling_y <- NA
  }

  if(grepl(pattern = "ScalingZ", x = metadata, ignore.case = TRUE)){
    scaling_z <- gsub(pattern = ".+<ScalingZ>(.+)</ScalingZ>.+",
                      replacement = "\\1", x = metadata)
    scaling_z <- tolower(scaling_z)
    scaling_z <- as.numeric(scaling_z)
    scaling_z <- scaling_z*1e6
  }else if(grepl(pattern = "<Distance Id=\"Z\">", x = metadata, ignore.case = TRUE)){
    scaling_z <- gsub(pattern = ".+<Distance Id=\"Z\">.+<Value>(.{1,35})</Value>.+</Distance>.+",
                      replacement = "\\1", x = metadata)
    scaling_z <- tolower(scaling_z)
    scaling_z <- as.numeric(scaling_z)
  }else{
    scaling_z <- NA
  }


  # Image dimensions
  dim_x <- gsub(pattern = ".+<SizeX>(.+)</SizeX>.+",
                replacement = "\\1", x = metadata)
  dim_x <- tolower(dim_x)
  dim_x <- as.numeric(dim_x)

  dim_y <- gsub(pattern = ".+<SizeY>(.+)</SizeY>.+",
                replacement = "\\1", x = metadata)
  dim_y <- tolower(dim_y)
  dim_y <- as.numeric(dim_y)

  if(grepl(pattern = "SizeZ", x = metadata, ignore.case = TRUE)){
    dim_z <- gsub(pattern = ".+<SizeZ>(.+)</SizeZ>.+",
                  replacement = "\\1", x = metadata)
    dim_z <- tolower(dim_z)
    dim_z <- as.numeric(dim_z)
  }else{
    dim_z <- 1
  }

  if(grepl(pattern = "SizeC", x = metadata, ignore.case = TRUE)){
    number_of_channels <- gsub(pattern = ".+<SizeC>(.+)</SizeC>.+",
                               replacement = "\\1", x = metadata)
    number_of_channels <- tolower(number_of_channels)
    number_of_channels <- as.numeric(number_of_channels)
  }else{
    number_of_channels <- 1
  }

  if(grepl(pattern = "<PixelType>", x = metadata, ignore.case = TRUE)){
    pixel_type <- gsub(pattern = ".+<PixelType>(.+)</PixelType>.+",
                       replacement = "\\1", x = metadata)
  }else{
    pixel_type <- NA
  }

  if(tolower(pixel_type) == "bgr24"){
    color_axis <- "0"
  }else{
    color_axis <- "C"
  }


  # Microscope-specific information ########################################

  if(color_axis == "0"){
    # Type of microscope: AxioImager
    df_metadata <- readCziMetadata_AxioImager(metadata)
  }else if(color_axis == "C" &&
           grepl(pattern = "<AcquisitionMode>WideField</AcquisitionMode>",
                 x = metadata, ignore.case = TRUE)){
    df_metadata <- readCziMetadata_Apotome(metadata, number_of_channels)
  }else if(color_axis == "C" &&
           grepl(pattern = "<AcquisitionMode>LaserScanningConfocalMicroscopy</AcquisitionMode>",
                 x = metadata, ignore.case = TRUE)){
    df_metadata <- readCziMetadata_LSM(metadata, number_of_channels)
  }else{
    print("Microscope could not be identified.")
    return()
  }

  # Put information into a data frame
  df_metadata$fileName <- file_name
  df_metadata$acquisition_date <- acquisition_date
  df_metadata$acquisition_time <- acquisition_time
  df_metadata$microscope_name <- microscope_name
  df_metadata$microscopy_system <- microscopy_system
  df_metadata$color_system <- ifelse(color_axis == "0", "RGBA", "Channels")
  df_metadata$number_of_channels <- number_of_channels
  df_metadata$objective_magnification <- objective_magnification
  df_metadata$dim_x <- dim_x
  df_metadata$dim_y <- dim_y
  df_metadata$dim_z <- dim_z
  df_metadata$scaling_x <- scaling_x
  df_metadata$scaling_y <- scaling_y
  df_metadata$scaling_z <- scaling_z

  return(df_metadata)
}
