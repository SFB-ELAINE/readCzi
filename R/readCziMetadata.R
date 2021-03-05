#' @title readCziMetadata
#' @description Reads and saves key values from czi metadata as well as
#' entire metadata as txt
#' @details This functions saves the metadata of a czi file as txt and also
#' extract the most important information and saves it in a data frame.
#' @aliases readcziMetadata readCzimetadata readCZIMetadata
#' @author Kai Budde
#' @export readCziMetadata
#' @param input_file A character (path to czi file to be read)

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

  # Device Mode
  system <- gsub(pattern = ".+<System>(.+)</System.+", replacement = "\\1", x = metadata)

  # Image information

  # Scaling (in \mu m)
  scaling_x <- gsub(pattern = ".+<ScalingX>(.+)</ScalingX>.+", replacement = "\\1", x = metadata)
  scaling_x <- tolower(scaling_x)
  scaling_x <- as.numeric(scaling_x)
  scaling_x <- scaling_x*1e6

  scaling_y <- gsub(pattern = ".+<ScalingY>(.+)</ScalingY>.+", replacement = "\\1", x = metadata)
  scaling_y <- tolower(scaling_y)
  scaling_y <- as.numeric(scaling_y)
  scaling_y <- scaling_y*1e6

  scaling_z <- gsub(pattern = ".+<ScalingZ>(.+)</ScalingZ>.+", replacement = "\\1", x = metadata)
  scaling_z <- tolower(scaling_z)
  scaling_z <- as.numeric(scaling_z)
  scaling_z <- scaling_z*1e6

  dim_x <- gsub(pattern = ".+<SizeX>(.+)</SizeX>.+", replacement = "\\1", x = metadata)
  dim_x <- tolower(dim_x)
  dim_x <- as.numeric(dim_x)

  dim_y <- gsub(pattern = ".+<SizeY>(.+)</SizeY>.+", replacement = "\\1", x = metadata)
  dim_y <- tolower(dim_y)
  dim_y <- as.numeric(dim_y)

  dim_z <- gsub(pattern = ".+<SizeZ>(.+)</SizeZ>.+", replacement = "\\1", x = metadata)
  dim_z <- tolower(dim_z)
  dim_z <- as.numeric(dim_z)

  number_of_channels <- gsub(pattern = ".+<SizeC>(.+)</SizeC>.+", replacement = "\\1", x = metadata)
  number_of_channels <- tolower(number_of_channels)
  number_of_channels <- as.numeric(number_of_channels)

  # averaging and mode of acquisition
  averaging <- gsub(pattern = ".+<Averaging>(.+)</Averaging>.+", replacement = "\\1", x = metadata)
  scanning_mode <- gsub(pattern = ".+<ScanningMode>(.+)</ScanningMode>.+", replacement = "\\1", x = metadata)

  # Laser
  laser_wavelengths <- gsub(
    pattern =  paste(".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
                     ".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
                     ".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
                     sep=""),
    replacement = "\\1,\\2,\\3",
    x = metadata)
  laser_wavelengths <- as.numeric(strsplit(laser_wavelengths, split = ",")[[1]])

  laser_power <-  gsub(pattern = paste(".+<Power>(.+)</Power>.+",
                                       ".+<Power>(.+)</Power>.+",
                                       ".+<Power>(.+)</Power>.+",
                                       sep=""),
                       replacement = "\\1, \\2, \\3", x = metadata)
  laser_power <- unlist(strsplit(x = laser_power, split = ","))
  laser_power <- as.numeric(laser_power)

  laser_attenuation <-  gsub(pattern = paste(".+<Attenuation>(.+)</Attenuation>.+",
                                             ".+<Attenuation>(.+)</Attenuation>.+",
                                             ".+<Attenuation>(.+)</Attenuation>.+",
                                             sep=""),
                             replacement = "\\1, \\2, \\3", x = metadata)
  laser_attenuation <- unlist(strsplit(x = laser_attenuation, split = ","))
  laser_attenuation <- as.numeric(laser_attenuation)


  laser_transmission <-  gsub(pattern = paste(".+<Transmission>(.+)</Transmission>.+",
                                              ".+<Transmission>(.+)</Transmission>.+",
                                              ".+<Transmission>(.+)</Transmission>.+",
                                              sep=""),
                              replacement = "\\1, \\2, \\3", x = metadata)
  laser_transmission <- unlist(strsplit(x = laser_transmission, split = ","))
  laser_transmission <- as.numeric(laser_transmission)


  # Detector

  # Pixel time in \mu s
  pixel_time <- gsub(pattern = paste(".+<PixelTime>(.+)</PixelTime>.+",
                                     ".+<PixelTime>(.+)</PixelTime>.+",
                                     ".+<PixelTime>(.+)</PixelTime>.+",
                                     sep=""),
                     replacement = "\\1, \\2, \\3", x = metadata)
  pixel_time <- unlist(strsplit(x = pixel_time, split = ","))
  pixel_time <- as.numeric(pixel_time)
  pixel_time <- pixel_time*1e6


  photon_conversion_factor <- gsub(pattern = paste(".+<PhotonConversionFactor>(.+)</PhotonConversionFactor>.+",
                                                   ".+<PhotonConversionFactor>(.+)</PhotonConversionFactor>.+",
                                                   ".+<PhotonConversionFactor>(.+)</PhotonConversionFactor>.+",
                                                   sep=""),
                                   replacement = "\\1, \\2, \\3", x = metadata)
  photon_conversion_factor <- unlist(strsplit(x = photon_conversion_factor, split = ","))
  photon_conversion_factor <- as.numeric(photon_conversion_factor)

  detector_gain <- gsub(pattern = paste(".+<Voltage>(.+)</Voltage>.+",
                                        ".+<Voltage>(.+)</Voltage>.+",
                                        ".+<Voltage>(.+)</Voltage>.+",
                                        sep=""),
                        replacement = "\\1, \\2, \\3", x = metadata)
  detector_gain <- unlist(strsplit(x = detector_gain, split = ","))
  detector_gain <- as.numeric(detector_gain)

  amplifier_gain <- gsub(pattern = paste(".+<AmplifierGain>(.+)</AmplifierGain>.+",
                                         ".+<AmplifierGain>(.+)</AmplifierGain>.+",
                                         ".+<AmplifierGain>(.+)</AmplifierGain>.+",
                                         sep=""),
                         replacement = "\\1, \\2, \\3", x = metadata)
  amplifier_gain <- unlist(strsplit(x = amplifier_gain, split = ","))
  amplifier_gain <- as.numeric(amplifier_gain)

  amplifier_offset <- gsub(pattern = paste(".+<AmplifierOffset>(.+)</AmplifierOffset>.+",
                                           ".+<AmplifierOffset>(.+)</AmplifierOffset>.+",
                                           ".+<AmplifierOffset>(.+)</AmplifierOffset>.+",
                                           sep=""),
                           replacement = "\\1, \\2, \\3", x = metadata)
  amplifier_offset <- unlist(strsplit(x = amplifier_offset, split = ","))
  amplifier_offset <- as.numeric(amplifier_offset)

  # pinhole diameter in \mu m
  pinhole_diameter <- gsub(pattern = paste(".+<PinholeDiameter>(.+)</PinholeDiameter>.+",
                                           ".+<PinholeDiameter>(.+)</PinholeDiameter>.+",
                                           ".+<PinholeDiameter>(.+)</PinholeDiameter>.+",
                                           sep=""),
                           replacement = "\\1, \\2, \\3", x = metadata)
  pinhole_diameter <- unlist(strsplit(x = pinhole_diameter, split = ","))
  pinhole_diameter <- as.numeric(pinhole_diameter)
  pinhole_diameter <- pinhole_diameter*1e6

  detection_range_wavelength <- gsub(pattern = paste(".+<Ranges>(.+)</Ranges>.+",
                                                     ".+<Ranges>(.+)</Ranges>.+",
                                                     ".+<Ranges>(.+)</Ranges>.+",
                                                     sep=""),
                                     replacement = "\\1, \\2, \\3", x = metadata)
  detection_range_wavelength <- unlist(strsplit(x = detection_range_wavelength, split = ","))
  detection_range_wavelength <- unlist(strsplit(x = detection_range_wavelength, split = "-"))
  detection_range_wavelength <- as.numeric(detection_range_wavelength)

  # Find red, green, and blue channel ID
  red_id <- which(laser_wavelengths == max(laser_wavelengths))
  blue_id <- which(laser_wavelengths == min(laser_wavelengths))

  if(length(laser_wavelengths) == 3){
    green_id <- c(1:3)[!(c(1:3) %in% red_id | c(1:3) %in% blue_id)]
  }else{
    print("There are more or fewer than 3 wavelengths.")
    return()
  }

  rgb_layers <- c(red_id, green_id, blue_id)

  # Save metadata in txt file
  utils::write.table(metadata, file = paste(directory_of_file, "/", image_name_wo_czi,
                                            "_metadata.txt", sep = ""),
                     sep = "", row.names = FALSE, col.names = FALSE, quote = FALSE)

  # Put information into a data frame
  df_metadata <- data.frame(
    "fileName" = file_name,
    "Microscopy_system" = system,
    "dim_x" = dim_x,
    "dim_y" = dim_y,
    "dim_z" = dim_z,
    "number_of_channels" = number_of_channels,
    "scaling_x" = scaling_x,
    "scaling_y" = scaling_y,
    "scaling_z" = scaling_z,
    "scanning_mode" = scanning_mode,
    "averaging" = averaging,
    "laser_wavelengths_red" = laser_wavelengths[rgb_layers[1]],
    "laser_wavelengths_green" = laser_wavelengths[rgb_layers[2]],
    "laser_wavelengths_blue" = laser_wavelengths[rgb_layers[3]],
    "laser_power_red" = laser_power[rgb_layers[1]],
    "laser_power_green" = laser_power[rgb_layers[2]],
    "laser_power_blue" = laser_power[rgb_layers[3]],
    "laser_attenuation_red" = laser_attenuation[rgb_layers[1]],
    "laser_attenuation_green" = laser_attenuation[rgb_layers[2]],
    "laser_attenuation_blue" = laser_attenuation[rgb_layers[3]],
    "laser_transmission_red" = laser_transmission[rgb_layers[1]],
    "laser_transmission_green" = laser_transmission[rgb_layers[2]],
    "laser_transmission_blue" = laser_transmission[rgb_layers[3]],
    "pixel_time_red" = pixel_time[rgb_layers[1]],
    "pixel_time_green" = pixel_time[rgb_layers[2]],
    "pixel_time_blue" = pixel_time[rgb_layers[3]],
    "photon_conversion_factor_red" = photon_conversion_factor[rgb_layers[1]],
    "photon_conversion_factor_green" = photon_conversion_factor[rgb_layers[2]],
    "photon_conversion_factor_blue" = photon_conversion_factor[rgb_layers[3]],
    "detector_gain_red" = detector_gain[rgb_layers[1]],
    "detector_gain_green" = detector_gain[rgb_layers[2]],
    "detector_gain_blue" = detector_gain[rgb_layers[3]],
    "amplifier_gain_red" = amplifier_gain[rgb_layers[1]],
    "amplifier_gain_green" = amplifier_gain[rgb_layers[2]],
    "amplifier_gain_blue" = amplifier_gain[rgb_layers[3]],
    "amplifier_offset_red" = amplifier_offset[rgb_layers[1]],
    "amplifier_offset_green" = amplifier_offset[rgb_layers[2]],
    "amplifier_offset_blue" = amplifier_offset[rgb_layers[3]],
    "pinhole_diameter_red" = pinhole_diameter[rgb_layers[1]],
    "pinhole_diameter_green" = pinhole_diameter[rgb_layers[2]],
    "pinhole_diameter_blue" = pinhole_diameter[rgb_layers[3]],
    "detection_range_wavelength_red_lower" = detection_range_wavelength[2*rgb_layers[1]-1],
    "detection_range_wavelength_red_upper" = detection_range_wavelength[2*rgb_layers[1]],
    "detection_range_wavelength_green_lower" = detection_range_wavelength[2*rgb_layers[2]-1],
    "detection_range_wavelength_green_upper" = detection_range_wavelength[2*rgb_layers[2]],
    "detection_range_wavelength_blue_lower" = detection_range_wavelength[2*rgb_layers[3]-1],
    "detection_range_wavelength_blue_upper" = detection_range_wavelength[2*rgb_layers[3]])

  return(df_metadata)
}
