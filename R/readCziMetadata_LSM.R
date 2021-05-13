#' Check for apotome-specific values in metadata
#' @param metadata A character (loaded metadata in a string)
#' @param number_of_channels A number
#' @keywords internal

readCziMetadata_LSM <- function(metadata = metadata,
                                number_of_channels = number_of_channels) {
  # averaging and mode of acquisition
  if (grepl(pattern = "<Averaging>",
            x = metadata,
            ignore.case = TRUE)) {
    averaging <- gsub(pattern = ".+<Averaging>(.+)</Averaging>.+",
                      replacement = "\\1",
                      x = metadata)
  } else{
    averaging <- NA
  }
  if (grepl(pattern = "<ScanningMode>",
            x = metadata,
            ignore.case = TRUE)) {
    scanning_mode <-
      gsub(pattern = ".+<ScanningMode>(.+)</ScanningMode>.+",
           replacement = "\\1",
           x = metadata)
  } else{
    scanning_mode <- NA
  }

  # Get information of laser scanning microscopes ##########################

  # Get information depending on number of channels

  if (number_of_channels == 3) {

    laser_wavelengths <- gsub(
      pattern =  paste(
        ".+<Wavelength>(.+)</Wavelength>.+",
        ".+<Wavelength>(.+)</Wavelength>.+",
        ".+<Wavelength>(.+)</Wavelength>.+",
        sep = ""
      ),
      replacement = "\\1,\\2,\\3",
      x = metadata
    )

    emission_wavelengths <- gsub(
      pattern =  paste(
        ".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
        ".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
        ".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
        sep = ""
      ),
      replacement = "\\1,\\2,\\3",
      x = metadata
    )

    laser_power <-  gsub(
      pattern = paste(
        ".+<Power>(.+)</Power>.+",
        ".+<Power>(.+)</Power>.+",
        ".+<Power>(.+)</Power>.+",
        sep = ""
      ),
      replacement = "\\1, \\2, \\3",
      x = metadata
    )

    laser_attenuation <-  gsub(
      pattern = paste(
        ".+<Attenuation>(.+)</Attenuation>.+",
        ".+<Attenuation>(.+)</Attenuation>.+",
        ".+<Attenuation>(.+)</Attenuation>.+",
        sep = ""
      ),
      replacement = "\\1, \\2, \\3",
      x = metadata
    )

    laser_transmission <-  gsub(
      pattern = paste(
        ".+<Transmission>(.+)</Transmission>.+",
        ".+<Transmission>(.+)</Transmission>.+",
        ".+<Transmission>(.+)</Transmission>.+",
        sep = ""
      ),
      replacement = "\\1, \\2, \\3",
      x = metadata
    )

    # Detector

    # Pixel time in \mu s
    pixel_time <-
      gsub(
        pattern = paste(
          ".+<PixelTime>(.+)</PixelTime>.+",
          ".+<PixelTime>(.+)</PixelTime>.+",
          ".+<PixelTime>(.+)</PixelTime>.+",
          sep = ""
        ),
        replacement = "\\1, \\2, \\3",
        x = metadata
      )

    photon_conversion_factor <- gsub(
      pattern = paste(
        ".+<PhotonConversionFactor>(.+)</PhotonConversionFactor>.+",
        ".+<PhotonConversionFactor>(.+)</PhotonConversionFactor>.+",
        ".+<PhotonConversionFactor>(.+)</PhotonConversionFactor>.+",
        sep = ""
      ),
      replacement = "\\1, \\2, \\3",
      x = metadata
    )

    detector_gain <- gsub(
      pattern = paste(
        ".+<Voltage>(.+)</Voltage>.+",
        ".+<Voltage>(.+)</Voltage>.+",
        ".+<Voltage>(.+)</Voltage>.+",
        sep = ""
      ),
      replacement = "\\1, \\2, \\3",
      x = metadata
    )

    amplifier_gain <- gsub(
      pattern = paste(
        ".+<AmplifierGain>(.+)</AmplifierGain>.+",
        ".+<AmplifierGain>(.+)</AmplifierGain>.+",
        ".+<AmplifierGain>(.+)</AmplifierGain>.+",
        sep = ""
      ),
      replacement = "\\1, \\2, \\3",
      x = metadata
    )

    amplifier_offset <- gsub(
      pattern = paste(
        ".+<AmplifierOffset>(.+)</AmplifierOffset>.+",
        ".+<AmplifierOffset>(.+)</AmplifierOffset>.+",
        ".+<AmplifierOffset>(.+)</AmplifierOffset>.+",
        sep = ""
      ),
      replacement = "\\1, \\2, \\3",
      x = metadata
    )

    # pinhole diameter in \mu m
    pinhole_diameter <- gsub(
      pattern = paste(
        ".+<PinholeDiameter>(.+)</PinholeDiameter>.+",
        ".+<PinholeDiameter>(.+)</PinholeDiameter>.+",
        ".+<PinholeDiameter>(.+)</PinholeDiameter>.+",
        sep = ""
      ),
      replacement = "\\1, \\2, \\3",
      x = metadata
    )

    detection_range_wavelength <- gsub(
      pattern = paste(
        ".+<Ranges>(.+)</Ranges>.+",
        ".+<Ranges>(.+)</Ranges>.+",
        ".+<Ranges>(.+)</Ranges>.+",
        sep = ""
      ),
      replacement = "\\1, \\2, \\3",
      x = metadata
    )

  } else if (number_of_channels == 2) {

    laser_wavelengths <- gsub(
      pattern =  paste(
        ".+<Wavelength>(.+)</Wavelength>.+",
        ".+<Wavelength>(.+)</Wavelength>.+",
        sep = ""
      ),
      replacement = "\\1,\\2",
      x = metadata
    )

    emission_wavelengths <- gsub(
      pattern =  paste(
        ".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
        ".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
        sep = ""
      ),
      replacement = "\\1,\\2",
      x = metadata
    )

    laser_power <-  gsub(
      pattern = paste(
        ".+<Power>(.+)</Power>.+",
        ".+<Power>(.+)</Power>.+",
        sep = ""
      ),
      replacement = "\\1, \\2",
      x = metadata
    )

    laser_attenuation <-  gsub(
      pattern = paste(
        ".+<Attenuation>(.+)</Attenuation>.+",
        ".+<Attenuation>(.+)</Attenuation>.+",
        sep = ""
      ),
      replacement = "\\1, \\2",
      x = metadata
    )

    laser_transmission <-  gsub(
      pattern = paste(
        ".+<Transmission>(.+)</Transmission>.+",
        ".+<Transmission>(.+)</Transmission>.+",
        sep = ""
      ),
      replacement = "\\1, \\2",
      x = metadata
    )

    # Detector

    # Pixel time in \mu s
    pixel_time <- gsub(
      pattern = paste(
        ".+<PixelTime>(.+)</PixelTime>.+",
        ".+<PixelTime>(.+)</PixelTime>.+",
        sep = ""
      ),
      replacement = "\\1, \\2",
      x = metadata
    )

    photon_conversion_factor <-
      gsub(
        pattern = paste(
          ".+<PhotonConversionFactor>(.+)</PhotonConversionFactor>.+",
          ".+<PhotonConversionFactor>(.+)</PhotonConversionFactor>.+",
          sep = ""
        ),
        replacement = "\\1, \\2",
        x = metadata
      )

    detector_gain <- gsub(
      pattern = paste(
        ".+<Voltage>(.+)</Voltage>.+",
        ".+<Voltage>(.+)</Voltage>.+",
        sep = ""
      ),
      replacement = "\\1, \\2",
      x = metadata
    )

    amplifier_gain <- gsub(
      pattern = paste(
        ".+<AmplifierGain>(.+)</AmplifierGain>.+",
        ".+<AmplifierGain>(.+)</AmplifierGain>.+",
        sep = ""
      ),
      replacement = "\\1, \\2",
      x = metadata
    )

    amplifier_offset <- gsub(
      pattern = paste(
        ".+<AmplifierOffset>(.+)</AmplifierOffset>.+",
        ".+<AmplifierOffset>(.+)</AmplifierOffset>.+",
        sep = ""
      ),
      replacement = "\\1, \\2",
      x = metadata
    )

    # pinhole diameter in \mu m
    pinhole_diameter <- gsub(
      pattern = paste(
        ".+<PinholeDiameter>(.+)</PinholeDiameter>.+",
        ".+<PinholeDiameter>(.+)</PinholeDiameter>.+",
        sep = ""
      ),
      replacement = "\\1, \\2",
      x = metadata
    )

    detection_range_wavelength <- gsub(
      pattern = paste(
        ".+<Ranges>(.+)</Ranges>.+",
        ".+<Ranges>(.+)</Ranges>.+",
        sep = ""
      ),
      replacement = "\\1, \\2",
      x = metadata
    )

  } else if (number_of_channels == 1) {

    laser_wavelengths <- gsub(
      pattern =  paste(
        ".+<Wavelength>(.+)</Wavelength>.+",
        sep = ""
      ),
      replacement = "\\1",
      x = metadata
    )

    emission_wavelengths <- gsub(
      pattern =  paste(
        ".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
        sep = ""
      ),
      replacement = "\\1",
      x = metadata
    )

    laser_power <-  gsub(
      pattern = paste(".+<Power>(.+)</Power>.+",
                      sep = ""),
      replacement = "\\1",
      x = metadata
    )

    laser_attenuation <-  gsub(
      pattern = paste(".+<Attenuation>(.+)</Attenuation>.+",
                      sep = ""),
      replacement = "\\1",
      x = metadata
    )

    laser_transmission <-  gsub(
      pattern = paste(".+<Transmission>(.+)</Transmission>.+",
                      sep = ""),
      replacement = "\\1",
      x = metadata
    )

    # Detector

    # Pixel time in \mu s
    pixel_time <- gsub(
      pattern = paste(".+<PixelTime>(.+)</PixelTime>.+",
                      sep = ""),
      replacement = "\\1",
      x = metadata
    )

    photon_conversion_factor <- gsub(
      pattern = paste(
        ".+<PhotonConversionFactor>(.+)</PhotonConversionFactor>.+",
        sep = ""
      ),
      replacement = "\\1",
      x = metadata
    )

    detector_gain <- gsub(
      pattern = paste(".+<Voltage>(.+)</Voltage>.+",
                      sep = ""),
      replacement = "\\1",
      x = metadata
    )

    amplifier_gain <- gsub(
      pattern = paste(".+<AmplifierGain>(.+)</AmplifierGain>.+",
                      sep = ""),
      replacement = "\\1",
      x = metadata
    )

    amplifier_offset <- gsub(
      pattern = paste(".+<AmplifierOffset>(.+)</AmplifierOffset>.+",
                      sep = ""),
      replacement = "\\1",
      x = metadata
    )

    # pinhole diameter in \mu m
    pinhole_diameter <- gsub(
      pattern = paste(".+<PinholeDiameter>(.+)</PinholeDiameter>.+",
                      sep = ""),
      replacement = "\\1",
      x = metadata
    )

    detection_range_wavelength <- gsub(
      pattern = paste(".+<Ranges>(.+)</Ranges>.+",
                      sep = ""),
      replacement = "\\1",
      x = metadata
    )

  }

  laser_wavelengths <- as.numeric(unlist(strsplit(laser_wavelengths, split = ",")))
  emission_wavelengths <-
    as.numeric(unlist(strsplit(emission_wavelengths, split = ",")))
  laser_power <-
    as.numeric(unlist(strsplit(x = laser_power, split = ",")))
  laser_attenuation <-
    as.numeric(unlist(strsplit(x = laser_attenuation, split = ",")))
  laser_transmission <-
    as.numeric(unlist(strsplit(x = laser_transmission, split = ",")))

  pixel_time <-
    as.numeric(unlist(strsplit(x = pixel_time, split = ",")))
  pixel_time <- pixel_time * 1e6
  photon_conversion_factor <-
    as.numeric(unlist(strsplit(x = photon_conversion_factor, split = ",")))
  detector_gain <-
    as.numeric(unlist(strsplit(x = detector_gain, split = ",")))
  amplifier_gain <-
    as.numeric(unlist(strsplit(x = amplifier_gain, split = ",")))
  amplifier_offset <-
    as.numeric(unlist(strsplit(x = amplifier_offset, split = ",")))

  pinhole_diameter <-
    as.numeric(unlist(strsplit(x = pinhole_diameter, split = ",")))
  pinhole_diameter <- pinhole_diameter * 1e6

  detection_range_wavelength <-
    unlist(strsplit(x = detection_range_wavelength, split = ","))
  detection_range_wavelength <-
    unlist(strsplit(x = detection_range_wavelength, split = "-"))
  detection_range_wavelength <-
    as.numeric(detection_range_wavelength)

  # Put information into a data frame
  df_metadata <- data.frame(
    "fileName" = NA,
    "acquisition_date" = NA,
    "acquisition_time" = NA,
    "microscope_name" = NA,
    "microscopy_system" = NA,
    "color_system" = NA,
    "number_of_channels" = NA,
    "objective_magnification" = NA,
    "dim_x" = NA,
    "dim_y" = NA,
    "dim_z" = NA,
    "scaling_x" = NA,
    "scaling_y" = NA,
    "scaling_z" = NA,
    "scanning_mode" = scanning_mode,
    "averaging" = averaging,
    "laser_wavelength_1" = laser_wavelengths[1],
    "laser_wavelength_2" = laser_wavelengths[2],
    "laser_wavelength_3" = laser_wavelengths[3],
    "emission_wavelength_1" = emission_wavelengths[1],
    "emission_wavelength_2" = emission_wavelengths[2],
    "emission_wavelength_3" = emission_wavelengths[3],
    "laser_power_1" = laser_power[1],
    "laser_power_2" = laser_power[2],
    "laser_power_3" = laser_power[3],
    "laser_attenuation_1" = laser_attenuation[1],
    "laser_attenuation_2" = laser_attenuation[2],
    "laser_attenuation_3" = laser_attenuation[3],
    "laser_transmission_1" = laser_transmission[1],
    "laser_transmission_2" = laser_transmission[2],
    "laser_transmission_3" = laser_transmission[3],
    "pixel_time_1" = pixel_time[1],
    "pixel_time_2" = pixel_time[2],
    "pixel_time_3" = pixel_time[3],
    "photon_conversion_factor_1" = photon_conversion_factor[1],
    "photon_conversion_factor_2" = photon_conversion_factor[2],
    "photon_conversion_factor_3" = photon_conversion_factor[3],
    "detector_gain_1" = detector_gain[1],
    "detector_gain_2" = detector_gain[2],
    "detector_gain_3" = detector_gain[3],
    "amplifier_gain_1" = amplifier_gain[1],
    "amplifier_gain_2" = amplifier_gain[2],
    "amplifier_gain_3" = amplifier_gain[3],
    "amplifier_offset_1" = amplifier_offset[1],
    "amplifier_offset_2" = amplifier_offset[2],
    "amplifier_offset_3" = amplifier_offset[3],
    "pinhole_diameter_1" = pinhole_diameter[1],
    "pinhole_diameter_2" = pinhole_diameter[2],
    "pinhole_diameter_3" = pinhole_diameter[3],
    "detection_range_wavelength_1_lower" = detection_range_wavelength[1],
    "detection_range_wavelength_1_upper" = detection_range_wavelength[2],
    "detection_range_wavelength_2_lower" = detection_range_wavelength[3],
    "detection_range_wavelength_2_upper" = detection_range_wavelength[4],
    "detection_range_wavelength_3_lower" = detection_range_wavelength[5],
    "detection_range_wavelength_3_upper" = detection_range_wavelength[6]
  )

  return(df_metadata)

}
