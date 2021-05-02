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
  # Microscope name
  if(grepl(pattern = "<Microscopes>.+Name=.+</Microscopes>", x = metadata, ignore.case = TRUE)){
    microscope_name <- gsub(pattern = ".+<Microscopes>.+<Microscope.+Name=\"(.+)\".+</Microscopes>.+",
                   replacement = "\\1", x = metadata)
  }else{
    microscope_name <- NA
  }


  # Device Mode
  if(grepl(pattern = "<System>", x = metadata, ignore.case = TRUE)){
    system <- gsub(pattern = ".+<System>(.+)</System.+",
                   replacement = "\\1", x = metadata)
  }else{
    system <- NA
  }

  # Image information ######################################################

  # Magnification of objective
  if(grepl(pattern = "<NominalMagnification>", x = metadata, ignore.case = TRUE)){
    objective_magnification <- gsub(pattern = ".+<NominalMagnification>(.+)</NominalMagnification>.+",
                            replacement = "\\1", x = metadata)
  }else{
    objective_magnification <- NA
  }

  # Scaling (in \mu m)
  if(grepl(pattern = "scaling_x", x = metadata, ignore.case = TRUE)){
    scaling_x <- gsub(pattern = ".+<ScalingX>(.+)</ScalingX>.+",
                      replacement = "\\1", x = metadata)
    scaling_x <- tolower(scaling_x)
    scaling_x <- as.numeric(scaling_x)
    scaling_x <- scaling_x*1e6
  }else{
    scaling_x <- NA
  }

  if(grepl(pattern = "ScalingY", x = metadata, ignore.case = TRUE)){
    scaling_y <- gsub(pattern = ".+<ScalingY>(.+)</ScalingY>.+",
                      replacement = "\\1", x = metadata)
    scaling_y <- tolower(scaling_y)
    scaling_y <- as.numeric(scaling_y)
    scaling_y <- scaling_y*1e6
  }else{
    scaling_y <- NA
  }

  if(grepl(pattern = "ScalingZ", x = metadata, ignore.case = TRUE)){
    scaling_z <- gsub(pattern = ".+<ScalingZ>(.+)</ScalingZ>.+",
                      replacement = "\\1", x = metadata)
    scaling_z <- tolower(scaling_z)
    scaling_z <- as.numeric(scaling_z)
    scaling_z <- scaling_z*1e6
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

  # averaging and mode of acquisition
  if(grepl(pattern = "<Averaging>", x = metadata, ignore.case = TRUE)){
    averaging <- gsub(pattern = ".+<Averaging>(.+)</Averaging>.+",
                      replacement = "\\1", x = metadata)
  }else{
    averaging <- NA
  }
  if(grepl(pattern = "<ScanningMode>", x = metadata, ignore.case = TRUE)){
    scanning_mode <- gsub(pattern = ".+<ScanningMode>(.+)</ScanningMode>.+",
                          replacement = "\\1", x = metadata)
  }else{
    scanning_mode <- NA
  }

  # Get information of brightfield microscopes #############################

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

  # Get information of laser scanning microscopes ##########################

  # Get information depending on number of channels
  if(color_axis == "C"){

    if(number_of_channels == 3){
      # Laser
      emission_wavelengths <- gsub(
        pattern =  paste(".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
                         ".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
                         ".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
                         sep=""),
        replacement = "\\1,\\2,\\3",
        x = metadata)
      laser_power <-  gsub(pattern = paste(".+<Power>(.+)</Power>.+",
                                           ".+<Power>(.+)</Power>.+",
                                           ".+<Power>(.+)</Power>.+",
                                           sep=""),
                           replacement = "\\1, \\2, \\3", x = metadata)
      laser_attenuation <-  gsub(
        pattern = paste(".+<Attenuation>(.+)</Attenuation>.+",
                        ".+<Attenuation>(.+)</Attenuation>.+",
                        ".+<Attenuation>(.+)</Attenuation>.+",
                        sep=""),
        replacement = "\\1, \\2, \\3", x = metadata)
      laser_transmission <-  gsub(
        pattern = paste(".+<Transmission>(.+)</Transmission>.+",
                        ".+<Transmission>(.+)</Transmission>.+",
                        ".+<Transmission>(.+)</Transmission>.+",
                        sep=""),
        replacement = "\\1, \\2, \\3", x = metadata)

      # Detector

      # Pixel time in \mu s
      pixel_time <- gsub(pattern = paste(".+<PixelTime>(.+)</PixelTime>.+",
                                         ".+<PixelTime>(.+)</PixelTime>.+",
                                         ".+<PixelTime>(.+)</PixelTime>.+",
                                         sep=""),
                         replacement = "\\1, \\2, \\3", x = metadata)
      photon_conversion_factor <- gsub(
        pattern = paste(".+<PhotonConversionFactor>(.+)</PhotonConversionFactor>.+",
                        ".+<PhotonConversionFactor>(.+)</PhotonConversionFactor>.+",
                        ".+<PhotonConversionFactor>(.+)</PhotonConversionFactor>.+",
                        sep=""),
        replacement = "\\1, \\2, \\3", x = metadata)
      detector_gain <- gsub(
        pattern = paste(".+<Voltage>(.+)</Voltage>.+",
                        ".+<Voltage>(.+)</Voltage>.+",
                        ".+<Voltage>(.+)</Voltage>.+",
                        sep=""),
        replacement = "\\1, \\2, \\3", x = metadata)
      amplifier_gain <- gsub(
        pattern = paste(".+<AmplifierGain>(.+)</AmplifierGain>.+",
                        ".+<AmplifierGain>(.+)</AmplifierGain>.+",
                        ".+<AmplifierGain>(.+)</AmplifierGain>.+",
                        sep=""),
        replacement = "\\1, \\2, \\3", x = metadata)
      amplifier_offset <- gsub(
        pattern = paste(".+<AmplifierOffset>(.+)</AmplifierOffset>.+",
                        ".+<AmplifierOffset>(.+)</AmplifierOffset>.+",
                        ".+<AmplifierOffset>(.+)</AmplifierOffset>.+",
                        sep=""),
        replacement = "\\1, \\2, \\3", x = metadata)

      # pinhole diameter in \mu m
      pinhole_diameter <- gsub(
        pattern = paste(".+<PinholeDiameter>(.+)</PinholeDiameter>.+",
                        ".+<PinholeDiameter>(.+)</PinholeDiameter>.+",
                        ".+<PinholeDiameter>(.+)</PinholeDiameter>.+",
                        sep=""),
        replacement = "\\1, \\2, \\3", x = metadata)

      detection_range_wavelength <- gsub(
        pattern = paste(".+<Ranges>(.+)</Ranges>.+",
                        ".+<Ranges>(.+)</Ranges>.+",
                        ".+<Ranges>(.+)</Ranges>.+",
                        sep=""),
        replacement = "\\1, \\2, \\3", x = metadata)

    }else if(number_of_channels == 2){
      # Laser
      emission_wavelengths <- gsub(
        pattern =  paste(".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
                         ".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
                         sep=""),
        replacement = "\\1,\\2",
        x = metadata)
      laser_power <-  gsub(pattern = paste(".+<Power>(.+)</Power>.+",
                                           ".+<Power>(.+)</Power>.+",
                                           sep=""),
                           replacement = "\\1, \\2", x = metadata)
      laser_attenuation <-  gsub(
        pattern = paste(".+<Attenuation>(.+)</Attenuation>.+",
                        ".+<Attenuation>(.+)</Attenuation>.+",
                        sep=""),
        replacement = "\\1, \\2", x = metadata)
      laser_transmission <-  gsub(
        pattern = paste(".+<Transmission>(.+)</Transmission>.+",
                        ".+<Transmission>(.+)</Transmission>.+",
                        sep=""),
        replacement = "\\1, \\2", x = metadata)

      # Detector

      # Pixel time in \mu s
      pixel_time <- gsub(
        pattern = paste(".+<PixelTime>(.+)</PixelTime>.+",
                        ".+<PixelTime>(.+)</PixelTime>.+",
                        sep=""),
        replacement = "\\1, \\2", x = metadata)
      photon_conversion_factor <- gsub(pattern = paste(".+<PhotonConversionFactor>(.+)</PhotonConversionFactor>.+",
                                                       ".+<PhotonConversionFactor>(.+)</PhotonConversionFactor>.+",
                                                       sep=""),
                                       replacement = "\\1, \\2", x = metadata)
      detector_gain <- gsub(
        pattern = paste(".+<Voltage>(.+)</Voltage>.+",
                        ".+<Voltage>(.+)</Voltage>.+",
                        sep=""),
        replacement = "\\1, \\2", x = metadata)
      amplifier_gain <- gsub(
        pattern = paste(".+<AmplifierGain>(.+)</AmplifierGain>.+",
                        ".+<AmplifierGain>(.+)</AmplifierGain>.+",
                        sep=""),
        replacement = "\\1, \\2", x = metadata)
      amplifier_offset <- gsub(
        pattern = paste(".+<AmplifierOffset>(.+)</AmplifierOffset>.+",
                        ".+<AmplifierOffset>(.+)</AmplifierOffset>.+",
                        sep=""),
        replacement = "\\1, \\2", x = metadata)

      # pinhole diameter in \mu m
      pinhole_diameter <- gsub(
        pattern = paste(".+<PinholeDiameter>(.+)</PinholeDiameter>.+",
                        ".+<PinholeDiameter>(.+)</PinholeDiameter>.+",
                        sep=""),
        replacement = "\\1, \\2", x = metadata)

      detection_range_wavelength <- gsub(
        pattern = paste(".+<Ranges>(.+)</Ranges>.+",
                        ".+<Ranges>(.+)</Ranges>.+",
                        sep=""),
        replacement = "\\1, \\2", x = metadata)

    }else if(number_of_channels == 1){
      # Laser
      emission_wavelengths <- gsub(
        pattern =  paste(".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
                         sep=""),
        replacement = "\\1",
        x = metadata)
      laser_power <-  gsub(
        pattern = paste(".+<Power>(.+)</Power>.+",
                        sep=""),
        replacement = "\\1", x = metadata)
      laser_attenuation <-  gsub(
        pattern = paste(".+<Attenuation>(.+)</Attenuation>.+",
                        sep=""),
        replacement = "\\1", x = metadata)
      laser_transmission <-  gsub(
        pattern = paste(".+<Transmission>(.+)</Transmission>.+",
                        sep=""),
        replacement = "\\1", x = metadata)
      # Detector

      # Pixel time in \mu s
      pixel_time <- gsub(
        pattern = paste(".+<PixelTime>(.+)</PixelTime>.+",
                        sep=""),
        replacement = "\\1", x = metadata)
      photon_conversion_factor <- gsub(
        pattern = paste(".+<PhotonConversionFactor>(.+)</PhotonConversionFactor>.+",
                        sep=""),
        replacement = "\\1", x = metadata)
      detector_gain <- gsub(
        pattern = paste(".+<Voltage>(.+)</Voltage>.+",
                        sep=""),
        replacement = "\\1", x = metadata)
      amplifier_gain <- gsub(
        pattern = paste(".+<AmplifierGain>(.+)</AmplifierGain>.+",
                        sep=""),
        replacement = "\\1", x = metadata)
      amplifier_offset <- gsub(
        pattern = paste(".+<AmplifierOffset>(.+)</AmplifierOffset>.+",
                        sep=""),
        replacement = "\\1", x = metadata)

      # pinhole diameter in \mu m
      pinhole_diameter <- gsub(
        pattern = paste(".+<PinholeDiameter>(.+)</PinholeDiameter>.+",
                        sep=""),
        replacement = "\\1", x = metadata)

      detection_range_wavelength <- gsub(
        pattern = paste(".+<Ranges>(.+)</Ranges>.+",
                        sep=""),
        replacement = "\\1", x = metadata)

    }

    emission_wavelengths <- as.numeric(unlist(strsplit(emission_wavelengths, split = ",")))
    laser_power <- as.numeric(unlist(strsplit(x = laser_power, split = ",")))
    laser_attenuation <- as.numeric(unlist(strsplit(x = laser_attenuation, split = ",")))
    laser_transmission <- as.numeric(unlist(strsplit(x = laser_transmission, split = ",")))

    pixel_time <- as.numeric(unlist(strsplit(x = pixel_time, split = ",")))
    pixel_time <- pixel_time*1e6
    photon_conversion_factor <- as.numeric(unlist(strsplit(x = photon_conversion_factor, split = ",")))
    detector_gain <- as.numeric(unlist(strsplit(x = detector_gain, split = ",")))
    amplifier_gain <- as.numeric(unlist(strsplit(x = amplifier_gain, split = ",")))
    amplifier_offset <- as.numeric(unlist(strsplit(x = amplifier_offset, split = ",")))

    pinhole_diameter <- as.numeric(unlist(strsplit(x = pinhole_diameter, split = ",")))
    pinhole_diameter <- pinhole_diameter*1e6

    detection_range_wavelength <- unlist(strsplit(x = detection_range_wavelength, split = ","))
    detection_range_wavelength <- unlist(strsplit(x = detection_range_wavelength, split = "-"))
    detection_range_wavelength <- as.numeric(detection_range_wavelength)

    # Find red, green, and blue channel ID

    # Upper and lower limits of wavelengths to determine the colors
    # red: > 600nm
    # green: >= 500nm and <= 600nm
    # blue: < 500nm

    upper_limit_blue <- 500
    lower_limit_red <- 600

    test_red <- any(emission_wavelengths > lower_limit_red)
    red_id <- ifelse(test = test_red,
                     yes = which(emission_wavelengths > lower_limit_red),
                     no = 0)
    if(length(red_id) > 1){print("More than one red channel!?")}

    test_green <- all( (upper_limit_blue) <= emission_wavelengths &
                         (emission_wavelengths <= lower_limit_red))
    green_id <- ifelse(test = test_green,
                       yes = which((upper_limit_blue <= emission_wavelengths) &
                                     (emission_wavelengths <= lower_limit_red)),
                       no = 0)
    if(length(green_id) > 1){print("More than one green channel!?")}

    test_blue <- any(emission_wavelengths < upper_limit_blue)
    blue_id <- ifelse(test = test_blue,
                      yes = which(upper_limit_blue > emission_wavelengths),
                      no = 0)
    if(length(blue_id) > 1){print("More than one blue channel!?")}

    rgb_layers <- c(red_id, green_id, blue_id)

  }


  # Save metadata in txt file
  utils::write.table(metadata, file = paste(directory_of_file, "/",
                                            image_name_wo_czi,
                                            "_metadata.txt", sep = ""),
                     sep = "", row.names = FALSE,
                     col.names = FALSE, quote = FALSE)

  # Put information into a data frame
  df_metadata <- data.frame(
    "fileName" = file_name,
    "microscope_name" = microscope_name,
    "microscopy_system" = system,
    "color_system" <- ifelse(color_axis == "0", "RGBA", "Channels"),
    "objective_magnification" = objective_magnification,
    "dim_x" = dim_x,
    "dim_y" = dim_y,
    "dim_z" = dim_z,
    "number_of_channels" = number_of_channels,
    "scaling_x" = scaling_x,
    "scaling_y" = scaling_y,
    "scaling_z" = scaling_z,
    "scanning_mode" = scanning_mode,
    "averaging" = averaging,
    "emission_wavelengths_red" =
      ifelse(color_axis == "0" || length(emission_wavelengths[rgb_layers[1]])==0, NA,
             emission_wavelengths[rgb_layers[1]]),
    "emission_wavelengths_green" =
      ifelse(color_axis == "0" || length(emission_wavelengths[rgb_layers[2]])==0, NA,
             emission_wavelengths[rgb_layers[2]]),
    "emission_wavelengths_blue" =
      ifelse(color_axis == "0" || length(emission_wavelengths[rgb_layers[3]])==0, NA,
             emission_wavelengths[rgb_layers[3]]),
    "laser_power_red" =
      ifelse(color_axis == "0" || length(laser_power[rgb_layers[1]])==0, NA,
             laser_power[rgb_layers[1]]),
    "laser_power_green" =
      ifelse(color_axis == "0" || length(laser_power[rgb_layers[2]])==0, NA,
             laser_power[rgb_layers[2]]),
    "laser_power_blue" =
      ifelse(color_axis == "0" || length(laser_power[rgb_layers[3]])==0, NA,
             laser_power[rgb_layers[3]]),
    "laser_attenuation_red" =
      ifelse(color_axis == "0" || length(laser_attenuation[rgb_layers[1]])==0,
             NA, laser_attenuation[rgb_layers[1]]),
    "laser_attenuation_green" =
      ifelse(color_axis == "0" || length(laser_attenuation[rgb_layers[2]])==0,
             NA, laser_attenuation[rgb_layers[2]]),
    "laser_attenuation_blue" =
      ifelse(color_axis == "0" || length(laser_attenuation[rgb_layers[3]])==0,
             NA, laser_attenuation[rgb_layers[3]]),
    "laser_transmission_red" =
      ifelse(color_axis == "0" || length(laser_transmission[rgb_layers[1]])==0,
             NA, laser_transmission[rgb_layers[1]]),
    "laser_transmission_green" =
      ifelse(color_axis == "0" || length(laser_transmission[rgb_layers[2]])==0,
             NA, laser_transmission[rgb_layers[2]]),
    "laser_transmission_blue" =
      ifelse(color_axis == "0" || length(laser_transmission[rgb_layers[3]])==0,
             NA, laser_transmission[rgb_layers[3]]),
    "pixel_time_red" =
      ifelse(color_axis == "0" || length(pixel_time[rgb_layers[1]])==0, NA,
             pixel_time[rgb_layers[1]]),
    "pixel_time_green" =
      ifelse(color_axis == "0" || length(pixel_time[rgb_layers[2]])==0, NA,
             pixel_time[rgb_layers[2]]),
    "pixel_time_blue" =
      ifelse(color_axis == "0" || length(pixel_time[rgb_layers[3]])==0, NA,
             pixel_time[rgb_layers[3]]),
    "photon_conversion_factor_red" =
      ifelse(color_axis == "0" || length(photon_conversion_factor[rgb_layers[1]])==0, NA,
             photon_conversion_factor[rgb_layers[1]]),
    "photon_conversion_factor_green" =
      ifelse(color_axis == "0" || length(photon_conversion_factor[rgb_layers[2]])==0, NA,
             photon_conversion_factor[rgb_layers[2]]),
    "photon_conversion_factor_blue" =
      ifelse(color_axis == "0" || length(photon_conversion_factor[rgb_layers[3]])==0, NA,
             photon_conversion_factor[rgb_layers[3]]),
    "detector_gain_red" =
      ifelse(color_axis == "0" || length(detector_gain[rgb_layers[1]])==0, NA,
             detector_gain[rgb_layers[1]]),
    "detector_gain_green" =
      ifelse(color_axis == "0" || length(detector_gain[rgb_layers[2]])==0, NA,
             detector_gain[rgb_layers[2]]),
    "detector_gain_blue" =
      ifelse(color_axis == "0" || length(detector_gain[rgb_layers[3]])==0, NA,
             detector_gain[rgb_layers[3]]),
    "amplifier_gain_red" =
      ifelse(color_axis == "0" || length(amplifier_gain[rgb_layers[1]])==0, NA,
             amplifier_gain[rgb_layers[1]]),
    "amplifier_gain_green" =
      ifelse(color_axis == "0" || length(amplifier_gain[rgb_layers[2]])==0, NA,
             amplifier_gain[rgb_layers[2]]),
    "amplifier_gain_blue" =
      ifelse(color_axis == "0" || length(amplifier_gain[rgb_layers[3]])==0, NA,
             amplifier_gain[rgb_layers[3]]),
    "amplifier_offset_red" =
      ifelse(color_axis == "0" || length(amplifier_offset[rgb_layers[1]])==0, NA,
             amplifier_offset[rgb_layers[1]]),
    "amplifier_offset_green" =
      ifelse(color_axis == "0" || length(amplifier_offset[rgb_layers[2]])==0, NA,
             amplifier_offset[rgb_layers[2]]),
    "amplifier_offset_blue" =
      ifelse(color_axis == "0" || length(amplifier_offset[rgb_layers[3]])==0, NA,
             amplifier_offset[rgb_layers[3]]),
    "pinhole_diameter_red" =
      ifelse(color_axis == "0" || length(pinhole_diameter[rgb_layers[1]])==0, NA,
             pinhole_diameter[rgb_layers[1]]),
    "pinhole_diameter_green" =
      ifelse(color_axis == "0" || length(pinhole_diameter[rgb_layers[2]])==0, NA,
             pinhole_diameter[rgb_layers[2]]),
    "pinhole_diameter_blue" =
      ifelse(color_axis == "0" || length(pinhole_diameter[rgb_layers[3]])==0, NA,
             pinhole_diameter[rgb_layers[3]]),
    "detection_range_wavelength_red_lower" =
      ifelse(color_axis == "0" || length(rgb_layers[rgb_layers[1]])==0, NA,
             detection_range_wavelength[2*rgb_layers[1]-1]),
    "detection_range_wavelength_red_upper" =
      ifelse(color_axis == "0" || length(rgb_layers[rgb_layers[1]])==0, NA,
             detection_range_wavelength[2*rgb_layers[1]]),
    "detection_range_wavelength_green_lower" =
      ifelse(color_axis == "0" || length(rgb_layers[rgb_layers[2]])==0, NA,
             detection_range_wavelength[2*rgb_layers[2]-1]),
    "detection_range_wavelength_green_upper" =
      ifelse(color_axis == "0" || length(rgb_layers[rgb_layers[2]])==0, NA,
             detection_range_wavelength[2*rgb_layers[2]]),
    "detection_range_wavelength_blue_lower" =
      ifelse(color_axis == "0" || length(rgb_layers[rgb_layers[3]])==0, NA,
             detection_range_wavelength[2*rgb_layers[3]-1]),
    "detection_range_wavelength_blue_upper" =
      ifelse(color_axis == "0" || length(rgb_layers[rgb_layers[3]])==0, NA,
             detection_range_wavelength[2*rgb_layers[3]])
  )

  return(df_metadata)
}
