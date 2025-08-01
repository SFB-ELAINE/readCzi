#' @title readCziMetadata
#' @description Reads and saves key values from czi metadata as well as
#' entire metadata as txt
#' @details This functions saves the metadata of a czi file as txt and also
#' extract the most important information and saves it in a data frame.
#' @aliases readcziMetadata readCzimetadata readCZIMetadata
#' @author Kai Budde-Sagert
#' @export readCziMetadata
#' @param input_file A character (path to czi file to be read)
#' @param save_metadata A Boolean (save metadata as txt file if TRUE)
#' @returns A tibble with key information from the metadata.

readCziMetadata <- function(input_file = NULL,
                            save_metadata = TRUE) {

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
  metadata_XML <- xml2::read_xml(metadata)
  metadata_XML_list <- xml2::as_list(x = metadata_XML)

  # Save metadata in txt file
  if(save_metadata){
    utils::write.table(metadata, file = paste(directory_of_file, "/",
                                              image_name_wo_czi,
                                              "_metadata.txt", sep = ""),
                       sep = "", row.names = FALSE,
                       col.names = FALSE, quote = FALSE)
  }

  # Save the dimension information
  # C: Channel in a multi channel data set
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

  # Microscope system
  if(grepl(pattern = "Microscopes", x = metadata_XML_list)){
    if(grepl(pattern = "System", x = metadata_XML_list)){
      microscopy_system <- unlist(
        metadata_XML_list$ImageDocument$Metadata$Information$Instrument$
          Microscopes$Microscope$System)
    }else{
      microscopy_system <- xml2::xml_find_all(x = metadata_XML, xpath = "//Microscope")
      microscopy_system <- unique(xml2::xml_attr(x = microscopy_system, attr = "Name"))
    }

  }else{
    microscopy_system <- NA
  }

  # Acquisition start time
  if(grepl(pattern = "CreationDate", x = metadata_XML_list)){
    acquisition_time <- unlist(metadata_XML_list$ImageDocument$
                                 Metadata$Information$Document$CreationDate)
    acquisition_date <- gsub(pattern = "(.+)T.+",
                             replacement = "\\1",
                             x = acquisition_time)
    acquisition_time <- gsub(pattern = ".+T(.+)",
                             replacement = "\\1",
                             x = acquisition_time)
    if(length(unlist(strsplit(x = acquisition_time, split = ""))) > 8){
      acquisition_time <- sub(
        pattern = "(^[[:digit:]]{0,2}:[[:digit:]]{0,2}:[[:digit:]]{0,2}).+",
        replacement = "\\1", x = acquisition_time)
    }
  }else{
    acquisition_date <- NA
    acquisition_time <- NA
  }

  # Image information ######################################################

  # Objective
  if(grepl(pattern = "Objectives", x = metadata_XML_list)){
    objective <- unlist(metadata_XML_list$ImageDocument$Metadata$
                          Information$Instrument$Objectives$Objective$
                          Manufacturer$Model)
    if(is.null(objective)){
      objective <- attr(x = metadata_XML_list$ImageDocument$Metadata$
                          Information$Instrument$Objectives$Objective,
                        which = "Name")
    }

  }else{
    objective <- NA
  }

  # Magnification of objective
  if(grepl(pattern = "NominalMagnification", x = metadata_XML_list)){
    objective_magnification <- as.numeric(unlist(
      metadata_XML_list$ImageDocument$Metadata$Information$Instrument$
        Objectives$Objective$NominalMagnification))
  }else{
    objective_magnification <- NA
  }

  # Scaling (in um)
  if(grepl(pattern = "scalingX", x = metadata, ignore.case = TRUE)){
    scaling_x_in_um <- gsub(pattern = ".+<ScalingX>(.+)</ScalingX>.+",
                            replacement = "\\1", x = metadata)
    scaling_x_in_um <- tolower(scaling_x_in_um)
    scaling_x_in_um <- as.numeric(scaling_x_in_um)
  }else if(grepl(pattern = "<Distance Id=\"X\">", x = metadata, ignore.case = TRUE)){
    scaling_x_in_um <- xml2::xml_find_all(x = metadata_XML, xpath = '//Distance[@Id="X"]/Value/text()')
    scaling_x_in_um <- xml2::xml_double(scaling_x_in_um)
  }else{
    scaling_x_in_um <- NA
  }
  if(scaling_x_in_um < 1e-3){
    # Assuming that it is given in m -> recalculate to um
    scaling_x_in_um <- scaling_x_in_um * 1e6
  }

  if(grepl(pattern = "ScalingY", x = metadata, ignore.case = TRUE)){
    scaling_y_in_um <- gsub(pattern = ".+<ScalingY>(.+)</ScalingY>.+",
                            replacement = "\\1", x = metadata)
    scaling_y_in_um <- tolower(scaling_y_in_um)
    scaling_y_in_um <- as.numeric(scaling_y_in_um)
  }else if(grepl(pattern = "<Distance Id=\"Y\">", x = metadata, ignore.case = TRUE)){
    scaling_y_in_um <- xml2::xml_find_all(x = metadata_XML, xpath = '//Distance[@Id="Y"]/Value/text()')
    scaling_y_in_um <- xml2::xml_double(scaling_y_in_um)
  }else{
    scaling_y_in_um <- NA
  }
  if(scaling_y_in_um < 1e-3){
    # Assuming that it is given in m -> recalculate to um
    scaling_y_in_um <- scaling_y_in_um * 1e6
  }

  if(grepl(pattern = "ScalingZ", x = metadata, ignore.case = TRUE)){
    scaling_z_in_um <- gsub(pattern = ".+<ScalingZ>(.+)</ScalingZ>.+",
                            replacement = "\\1", x = metadata)
    scaling_z_in_um <- tolower(scaling_z_in_um)
    scaling_z_in_um <- as.numeric(scaling_z_in_um)
  }else if(grepl(pattern = "<Distance Id=\"Z\">", x = metadata, ignore.case = TRUE)){
    scaling_z_in_um <- xml2::xml_find_all(x = metadata_XML, xpath = '//Distance[@Id="Z"]/Value/text()')
    scaling_z_in_um <- xml2::xml_double(scaling_z_in_um)
  }else{
    scaling_z_in_um <- 0
  }
  if(scaling_z_in_um < 1e-3){
    # Assuming that it is given in m -> recalculate to um
    scaling_z_in_um <- scaling_z_in_um * 1e6
  }


  # Image dimensions
  if(grepl(pattern = "SizeX", x = metadata_XML_list)){
    dim_x <- unlist(metadata_XML_list$ImageDocument$Metadata$Information$Image$SizeX)
    dim_x <- as.numeric(dim_x)
  }else{
    dim_x <- NA
  }
  if(grepl(pattern = "SizeY", x = metadata_XML_list)){
    dim_y <- unlist(metadata_XML_list$ImageDocument$Metadata$Information$Image$SizeY)
    dim_y <- as.numeric(dim_y)
  }else{
    dim_y <- NA
  }
  if(grepl(pattern = "SizeZ", x = metadata_XML_list)){
    dim_z <- unlist(metadata_XML_list$ImageDocument$Metadata$Information$Image$SizeZ)
    dim_z <- as.numeric(dim_z)
  }else{
    dim_z <- 1
  }
  if(grepl(pattern = "SizeC", x = metadata_XML_list)){
    number_of_channels <- unlist(metadata_XML_list$ImageDocument$Metadata$Information$Image$SizeC)
    number_of_channels <- as.numeric(number_of_channels)
  }else{
    number_of_channels <- 1
  }
  if(grepl(pattern = "Track", x = metadata_XML_list)){
    look_for <- paste(".//Tracks/Track", sep="")
    tracks_information <- xml2::xml_find_all(x = metadata_XML, xpath = look_for)
    tracks_ids <- unique(xml2::xml_attr(x = tracks_information, attr = "Id"))

    number_of_tracks <- length(tracks_ids)
  }else{
    number_of_tracks <- 0
  }

  if(grepl(pattern = "PixelType", x = metadata_XML_list)){
    pixel_type <- unlist(metadata_XML_list$ImageDocument$Metadata$Information$Image$PixelType)
  }else{
    pixel_type <- NA
  }

  if(tolower(pixel_type) == "bgr24" | tolower(pixel_type) == "bgr48"){
    color_axis <- "0"
  }else{
    color_axis <- "C"
  }


  # Microscope-specific information ########################################

  if(color_axis == "0"){
    # Type of microscope: AxioImager or Axio Image.M2
    df_metadata <- readCziMetadata_AxioImager(metadata,
                                              number_of_channels,
                                              number_of_tracks)

  }else if(# Type of microscope: LSM (in wide field acquisition mode)
    color_axis == "C" &&
    (grepl(pattern = "WideField", x = metadata_XML_list) ||
     as.logical(xml2::xml_attr(
       xml2::xml_find_first(metadata_XML, xpath = "//ApoTomeAcquisitionSetup"),
       "IsActivated"))
    )){
    df_metadata <- readCziMetadata_Apotome(metadata,
                                           number_of_channels,
                                           number_of_tracks)
  }else if(# Type of microscope: LSM (in confocal acquisition mode)
    color_axis == "C" &&
    grepl(pattern = "LaserScanningConfocalMicroscopy", x = metadata_XML_list)){
    df_metadata <- readCziMetadata_LSM(metadata,
                                       number_of_channels,
                                       number_of_tracks)

  }else{
    print("Microscope could not be identified.")
    return()
  }

  # Put information into a data frame
  df_metadata$fileName <- file_name
  df_metadata$acquisition_date <- acquisition_date
  df_metadata$acquisition_time <- acquisition_time
  df_metadata$microscopy_system <- microscopy_system
  df_metadata$color_system <- ifelse(color_axis == "0", "RGBA", "Channels")
  df_metadata$number_of_channels <- number_of_channels
  df_metadata$number_of_tracks <- number_of_tracks
  df_metadata$objective <- objective
  df_metadata$objective_magnification <- objective_magnification
  df_metadata$dim_x <- dim_x
  df_metadata$dim_y <- dim_y
  df_metadata$dim_z <- dim_z
  df_metadata$scaling_x_in_um <- scaling_x_in_um
  df_metadata$scaling_y_in_um <- scaling_y_in_um
  df_metadata$scaling_z_in_um <- scaling_z_in_um

  return(df_metadata)
}
