#' Check for apotome-specific values in metadata
#' @param metadata A character (loaded metadata in a string)
#' @param number_of_channels A number
#' @keywords internal

readCziMetadata_LSM <- function(metadata = metadata,
                                number_of_channels = number_of_channels) {

  metadata_XML <- xml2::read_xml(x = metadata)

  # Get information of laser scanning microscopes ##########################

  # Channel names
  channel_information <- look_for <- paste(".//Channel[@Name='",channel_names[current_channel], "']/Fluor", sep="")
  channel_names <- unique(xml2::xml_attr(x = channel_information, attr = "Name"))
  if(number_of_channels != length(channel_names)){
    print("The number of channels does not correspond to the number of channel names in the metadata.")
  }

  # Empty vectors
  fluorophores <- rep(x = NA, number_of_channels)
  detection_wavelength_start <- rep(x = NA, number_of_channels)
  detection_wavelength_end <- rep(x = NA, number_of_channels)
  excitation_wavelengths <- rep(x = NA, number_of_channels)
  emission_wavelengths <- rep(x = NA, number_of_channels)
  laser_scan_pixel_times <- rep(x = NA, number_of_channels)
  averaging <- rep(x = NA, number_of_channels)
  laser_scan_zoom_x <- rep(x = NA, number_of_channels)
  laser_scan_zoom_y <- rep(x = NA, number_of_channels)
  photon_conversion_factors <- rep(x = NA, number_of_channels)
  detector_gain <- rep(x = NA, number_of_channels)
  digital_gain <- rep(x = NA, number_of_channels)
  amplifier_offset <- rep(x = NA, number_of_channels)

  laser_wavelength <- rep(x = NA, number_of_channels)
  laser_transmission <- rep(x = NA, number_of_channels)
  laser_name <- rep(x = NA, number_of_channels)
  laser_power <- rep(x = NA, number_of_channels)


  # Go through each channel and get information
  for(i in 1:number_of_channels){

    # Filter for channel name ----------------------------------------------
    look_for <- paste(".//Dimensions/Channels/Channel[@Name='", channel_names[i], "']", sep="")
    channel_information <- xml2::xml_find_all(x = metadata_XML, xpath = look_for)
    channel_information <- xml2::as_list(channel_information)

    # Fluorphore
    fluorophores[i] <- unlist(channel_information[[1]]$Fluor)

    # Detection wavelengths
    detection_wavelength_start[i] <- as.numeric(unlist(
      strsplit(
        x = unlist(channel_information[[1]]$DetectionWavelength$Ranges),
        split = "-"))[1])
    detection_wavelength_end[i] <- as.numeric(unlist(
      strsplit(
        x = unlist(channel_information[[1]]$DetectionWavelength$Ranges),
        split = "-"))[2])

    # Excitation and emission wavelengths
    if(grepl(pattern = "ExcitationWavelength", x = channel_information, ignore.case = TRUE)){
      excitation_wavelengths[i] <- as.numeric(unlist(channel_information[[1]]$ExcitationWavelength))
    }
    if(grepl(pattern = "EmissionWavelength", x = channel_information, ignore.case = TRUE)){
      emission_wavelengths[i] <- as.numeric(unlist(channel_information[[1]]$EmissionWavelength))
    }

    # Laser scan pixel times
    if(grepl(pattern = "PixelTime", x = channel_information, ignore.case = TRUE)){
      laser_scan_pixel_times[i] <- as.numeric(unlist(channel_information[[1]]$LaserScanInfo$PixelTime))
    }

    # Laser scan averaging
    if(grepl(pattern = "Averaging", x = channel_information, ignore.case = TRUE)){
      averaging[i] <- as.numeric(unlist(channel_information[[1]]$LaserScanInfo$Averaging))
    }

    # Laser scan zoom
    if(grepl(pattern = "ZoomX", x = channel_information, ignore.case = TRUE)){
      laser_scan_zoom_x[i] <- as.numeric(unlist(channel_information[[1]]$LaserScanInfo$ZoomX))
    }
    if(grepl(pattern = "ZoomY", x = channel_information, ignore.case = TRUE)){
      laser_scan_zoom_y[i] <- as.numeric(unlist(channel_information[[1]]$LaserScanInfo$ZoomY))
    }

    # Detector settings

    if(grepl(pattern = "PhotonConversionFactor", x = channel_information, ignore.case = TRUE)){
      photon_conversion_factors[i] <- as.numeric(unlist(channel_information[[1]]$DetectorSettings$PhotonConversionFactor))
    }
    if(grepl(pattern = "Gain", x = channel_information, ignore.case = TRUE)){
      detector_gain[i] <- as.numeric(unlist(channel_information[[1]]$DetectorSettings$Gain))
    }
    if(grepl(pattern = "DigitalGain", x = channel_information, ignore.case = TRUE)){
      digital_gain[i] <- as.numeric(unlist(channel_information[[1]]$DetectorSettings$DigitalGain))
    }
    if(grepl(pattern = "Offset", x = channel_information, ignore.case = TRUE)){
      amplifier_offset[i] <- as.numeric(unlist(channel_information[[1]]$DetectorSettings$Offset))
    }
  }
  rm(i)

  # Sorting the channels information regarding wavelength ------------------
  channel_order <- order(detection_wavelength_start)

  fluorophores_copy <- fluorophores
  detection_wavelength_start_copy <- detection_wavelength_start
  detection_wavelength_end_copy <- detection_wavelength_end
  excitation_wavelengths_copy <- excitation_wavelengths
  emission_wavelengths_copy <- emission_wavelengths
  laser_scan_pixel_times_copy <- laser_scan_pixel_times
  averaging_copy <- averaging
  laser_scan_zoom_x_copy <- laser_scan_zoom_x
  laser_scan_zoom_y_copy <- laser_scan_zoom_y
  photon_conversion_factors_copy <- photon_conversion_factors
  detector_gain_copy <- detector_gain
  digital_gain_copy <- digital_gain
  amplifier_offset_copy <- amplifier_offset

  for(i in 1:number_of_channels){
    fluorophores[i] <- fluorophores_copy[channel_order[i]]
    detection_wavelength_start[i] <- detection_wavelength_start_copy[channel_order[i]]
    detection_wavelength_end[i] <- detection_wavelength_end_copy[channel_order[i]]
    excitation_wavelengths[i] <- excitation_wavelengths_copy[channel_order[i]]
    emission_wavelengths[i] <- emission_wavelengths_copy[channel_order[i]]
    laser_scan_pixel_times[i] <- laser_scan_pixel_times_copy[channel_order[i]]
    averaging[i] <- averaging_copy[channel_order[i]]
    laser_scan_zoom_x[i] <- laser_scan_zoom_x_copy[channel_order[i]]
    laser_scan_zoom_y[i] <- laser_scan_zoom_y_copy[channel_order[i]]
    photon_conversion_factors[i] <- photon_conversion_factors_copy[channel_order[i]]
    detector_gain[i] <- detector_gain_copy[channel_order[i]]
    digital_gain[i] <- digital_gain_copy[channel_order[i]]
    amplifier_offset[i] <- amplifier_offset_copy[channel_order[i]]
  }
  rm(i)

  # Filter for experiment --------------------------------------------------

  for(i in 1:number_of_channels){
    look_for <- paste(".//Experiment", sep="")
    experiment_information <- xml2::xml_find_all(x = metadata_XML, xpath = look_for)
    experiment_information <- xml2::as_list(experiment_information)

    if(grepl(pattern = "LaserName", x = experiment_information, ignore.case = TRUE)){
      laser_name[i] <- unlist(experiment_information[[1]]$ExperimentBlocks$AcquisitionBlock$Lasers[i]$Laser$LaserName)
    }
    if(grepl(pattern = "LaserPower", x = experiment_information, ignore.case = TRUE)){
      laser_power[i] <- unlist(experiment_information[[1]]$ExperimentBlocks$AcquisitionBlock$Lasers[i]$Laser$LaserPower)
    }

    look_for <- paste(".//Attenuators/Attenuator[Laser='", laser_name[i], "']", sep="")
    laser_information <- xml2::xml_find_all(x = metadata_XML, xpath = look_for)
    laser_information <- xml2::as_list(laser_information)

    if(grepl(pattern = "Wavelength", x = laser_information, ignore.case = TRUE)){

      laser_wavelength[i] <- as.numeric(unlist(laser_information[[1]]$Wavelength))
    }

    if(grepl(pattern = "Transmission", x = laser_information, ignore.case = TRUE)){
      laser_transmission[i] <- as.numeric(unlist(laser_information[[1]]$Transmission))
    }
  }

  rm(i)

  # Sort experiment information --------------------------------------------
  experiment_order <- order(laser_wavelength)

  laser_name_copy <- laser_name
  laser_power_copy <- laser_power
  laser_wavelength_copy <- laser_wavelength
  laser_transmission_copy <- laser_transmission

  for(i in 1:number_of_channels){
    laser_name[i] <- laser_name_copy[experiment_order[i]]
    laser_power[i] <- laser_power_copy[experiment_order[i]]
    laser_wavelength[i] <- laser_wavelength_copy[experiment_order[i]]
    laser_transmission[i] <- laser_transmission_copy[experiment_order[i]]
  }

  # for(current_channel in 0:(number_of_channels-1)){
  #
  #   if(grepl(pattern = "Channel", x = metadata_XML)){
  #     averaging_1 <- metadata_XML$Metadata$Information$Instrument$Objectives$Objective$Manufacturer$Model
  #   }else{
  #     averaging_1 <- NA
  #   }
  #
  #   channel_name_1 <- gsub(
  #         pattern = paste(rep(".+<Channel [[:print:]]+ Name=\"([[:print:]]+)\">.+",number_of_channels), collapse=""),
  #         replacement = paste("\\", 1:3, sep="", collapse=","),
  #         x = metadata)
  #
  #   channel_names <- unlist(strsplit(x = channel_names, split = ","))
  #
  #   metadata_block <- sub(
  #     pattern = paste(".+<Channel [[:print:]]+ Name=\"",
  #                     channel_names[current_channel],
  #                     "\">(.+)</Channel>?.+", sep=""),
  #     replacement = "\\1", x = metadata)
  #
  #
  #   metadata_block <- gsub(
  #     pattern = ".+(<LightSource Id=\"LightSource:0\" />.+)</LightSourceSettings>.+(<LightSourceSettings>)*?.+", replacement = "\\1", x = metadata)
  # }
  # rm(current_channel)
  #
  #
  # # Channel-specific information
  #
  # # averaging and mode of acquisition
  # if(grepl(pattern = "Averaging", x = metadata_XML)){
  #   averaging_1 <- metadata_XML$Metadata$Information$Instrument$Objectives$Objective$Manufacturer$Model
  # }else{
  #   averaging_1 <- NA
  # }
  #
  #
  # if (grepl(pattern = "<Averaging>",
  #           x = metadata,
  #           ignore.case = TRUE)) {
  #   averaging <- gsub(pattern = ".+<Averaging>(.+)</Averaging>.+",
  #                     replacement = "\\1",
  #                     x = metadata)
  # } else{
  #   averaging <- NA
  # }
  # if (grepl(pattern = "<ScanningMode>",
  #           x = metadata,
  #           ignore.case = TRUE)) {
  #   scanning_mode <-
  #     gsub(pattern = ".+<ScanningMode>(.+)</ScanningMode>.+",
  #          replacement = "\\1",
  #          x = metadata)
  # } else{
  #   scanning_mode <- NA
  # }
  #
  # # Get information depending on number of channels
  #
  # if (number_of_channels == 3) {
  #
  #   fluor <- gsub(
  #     pattern =  paste(
  #       ".+<Fluor>(.+)</Fluor>.+",
  #       ".+<Fluor>(.+)</Fluor>.+",
  #       ".+<Fluor>(.+)</Fluor>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1,\\2,\\3",
  #     x = metadata
  #   )
  #
  #   detection_wavelength_start <- gsub(
  #     pattern =  paste(
  #       ".+<Ranges>(.+)-.+</Ranges>.+",
  #       ".+<Ranges>(.+)-.+</Ranges>.+",
  #       ".+<Ranges>(.+)-.+</Ranges>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1,\\2,\\3",
  #     x = metadata
  #   )
  #
  #   detection_wavelength_end <- gsub(
  #     pattern =  paste(
  #       ".+<Ranges>.+-(.+)</Ranges>.+",
  #       ".+<Ranges>.+-(.+)</Ranges>.+",
  #       ".+<Ranges>.+-(.+)</Ranges>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1,\\2,\\3",
  #     x = metadata
  #   )
  #
  #   laser_wavelength <- gsub(
  #     pattern =  paste(
  #       ".+<Attenuator>.+<Wavelength>(.+)</Wavelength>.+</Attenuator>.+",
  #       ".+<Attenuator>.+<Wavelength>(.+)</Wavelength>.+</Attenuator>.+",
  #       ".+<Attenuator>.+<Wavelength>(.+)</Wavelength>.+</Attenuator>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1,\\2,\\3",
  #     x = metadata
  #   )
  #
  #   laser_name <- gsub(
  #     pattern = paste(
  #       ".+<Attenuator>.+<Laser>(.+)</Laser>.+</Attenuator>.+",
  #       ".+<Attenuator>.+<Laser>(.+)</Laser>.+</Attenuator>.+",
  #       ".+<Attenuator>.+<Laser>(.+)</Laser>.+</Attenuator>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1, \\2, \\3",
  #     x = metadata
  #   )
  #
  #
  #   laser_name_2 <- gsub(
  #     pattern = paste(
  #       ".+<LaserName>(.+)</LaserName>.+",
  #       ".+<LaserName>(.+)</LaserName>.+",
  #       ".+<LaserName>(.+)</LaserName>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1, \\2, \\3",
  #     x = metadata
  #   )
  #
  #   # Light source 0 (=laser_name_2[1])
  #   metadata_cropped <- gsub(pattern = ".+(<LightSource Id=\"LightSource:0\" />.+)</LightSourceSettings>.+(<LightSourceSettings>)*?.+", replacement = "\\1", x = metadata)
  #
  #   # Light source 1 (=laser_name_2[2])
  #   # Light source 2 (=laser_name_2[3])
  #   laser_power <-  gsub(
  #     pattern = paste(
  #       ".+<LaserPower>(.+)</LaserPower>.+",
  #       ".+<LaserPower>(.+)</LaserPower>.+",
  #       ".+<LaserPower>(.+)</LaserPower>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1, \\2, \\3",
  #     x = metadata
  #   )
  #
  #   laser_attenuation <-  gsub(
  #     pattern = paste(
  #       ".+<Attenuation>(.+)</Attenuation>.+",
  #       ".+<Attenuation>(.+)</Attenuation>.+",
  #       ".+<Attenuation>(.+)</Attenuation>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1, \\2, \\3",
  #     x = metadata
  #   )
  #
  #   laser_transmission <-  gsub(
  #     pattern = paste(
  #       ".+<Transmission>(.+)</Transmission>.+",
  #       ".+<Transmission>(.+)</Transmission>.+",
  #       ".+<Transmission>(.+)</Transmission>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1, \\2, \\3",
  #     x = metadata
  #   )
  #
  #   for(max_index in 3:1){
  #     pattern <- paste(rep(".+<EmissionWavelength>(.+)</EmissionWavelength>.+", max_index), collapse = "")
  #
  #     if(grepl(pattern = pattern, x = metadata, ignore.case = TRUE, )){
  #       break
  #     }
  #   }
  #
  #   if(max_index == 3){
  #     emission_wavelengths <- gsub(
  #       pattern =  paste(
  #         ".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
  #         ".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
  #         ".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
  #         sep = ""
  #       ),
  #       replacement = "\\1,\\2,\\3",
  #       x = metadata
  #     )
  #   }else if(max_index == 2){
  #     emission_wavelengths <- gsub(
  #       pattern =  paste(
  #         ".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
  #         ".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
  #         sep = ""
  #       ),
  #       replacement = "\\1,\\2",
  #       x = metadata
  #     )
  #   }
  #
  #
  #
  #   # Detector
  #
  #   # Pixel time in \mu s
  #   pixel_time <-
  #     gsub(
  #       pattern = paste(
  #         ".+<PixelTime>(.+)</PixelTime>.+",
  #         ".+<PixelTime>(.+)</PixelTime>.+",
  #         ".+<PixelTime>(.+)</PixelTime>.+",
  #         sep = ""
  #       ),
  #       replacement = "\\1, \\2, \\3",
  #       x = metadata
  #     )
  #
  #   photon_conversion_factor <- gsub(
  #     pattern = paste(
  #       ".+<PhotonConversionFactor>(.+)</PhotonConversionFactor>.+",
  #       ".+<PhotonConversionFactor>(.+)</PhotonConversionFactor>.+",
  #       ".+<PhotonConversionFactor>(.+)</PhotonConversionFactor>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1, \\2, \\3",
  #     x = metadata
  #   )
  #
  #   detector_gain <- gsub(
  #     pattern = paste(
  #       ".+<Voltage>(.+)</Voltage>.+",
  #       ".+<Voltage>(.+)</Voltage>.+",
  #       ".+<Voltage>(.+)</Voltage>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1, \\2, \\3",
  #     x = metadata
  #   )
  #
  #   digital_gain <- gsub(
  #     pattern = paste(
  #       ".+<AmplifierGain>(.+)</AmplifierGain>.+",
  #       ".+<AmplifierGain>(.+)</AmplifierGain>.+",
  #       ".+<AmplifierGain>(.+)</AmplifierGain>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1, \\2, \\3",
  #     x = metadata
  #   )
  #
  #   amplifier_offset <- gsub(
  #     pattern = paste(
  #       ".+<AmplifierOffset>(.+)</AmplifierOffset>.+",
  #       ".+<AmplifierOffset>(.+)</AmplifierOffset>.+",
  #       ".+<AmplifierOffset>(.+)</AmplifierOffset>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1, \\2, \\3",
  #     x = metadata
  #   )
  #
  #   # pinhole diameter in \mu m
  #   pinhole_diameter <- gsub(
  #     pattern = paste(
  #       ".+<PinholeDiameter>(.+)</PinholeDiameter>.+",
  #       ".+<PinholeDiameter>(.+)</PinholeDiameter>.+",
  #       ".+<PinholeDiameter>(.+)</PinholeDiameter>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1, \\2, \\3",
  #     x = metadata
  #   )
  #
  #   detection_range_wavelength <- gsub(
  #     pattern = paste(
  #       ".+<Ranges>(.+)</Ranges>.+",
  #       ".+<Ranges>(.+)</Ranges>.+",
  #       ".+<Ranges>(.+)</Ranges>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1, \\2, \\3",
  #     x = metadata
  #   )
  #
  # } else if (number_of_channels == 2) {
  #
  #   fluor <- gsub(
  #     pattern =  paste(
  #       ".+<Fluor>(.+)</Fluor>.+",
  #       ".+<Fluor>(.+)</Fluor>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1,\\2",
  #     x = metadata
  #   )
  #
  #   detection_wavelength_start <- gsub(
  #     pattern =  paste(
  #       ".+<Ranges>(.+)-.+</Ranges>.+",
  #       ".+<Ranges>(.+)-.+</Ranges>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1,\\2",
  #     x = metadata
  #   )
  #
  #   detection_wavelength_end <- gsub(
  #     pattern =  paste(
  #       ".+<Ranges>.+-(.+)</Ranges>.+",
  #       ".+<Ranges>.+-(.+)</Ranges>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1,\\2",
  #     x = metadata
  #   )
  #
  #   laser_wavelength <- gsub(
  #     pattern =  paste(
  #       ".+<Attenuator>.+<Wavelength>(.+)</Wavelength>.+</Attenuator>.+",
  #       ".+<Attenuator>.+<Wavelength>(.+)</Wavelength>.+</Attenuator>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1,\\2",
  #     x = metadata
  #   )
  #
  #   emission_wavelengths <- gsub(
  #     pattern =  paste(
  #       ".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
  #       ".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1,\\2",
  #     x = metadata
  #   )
  #
  #   laser_name <- gsub(
  #     pattern = paste(
  #       ".+<LaserName>(.+)</LaserName>.+",
  #       ".+<LaserName>(.+)</LaserName>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1, \\2",
  #     x = metadata
  #   )
  #
  #   laser_power <-  gsub(
  #     pattern = paste(
  #       ".+<LaserPower>(.+)</LaserPower>.+",
  #       ".+<LaserPower>(.+)</LaserPower>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1, \\2",
  #     x = metadata
  #   )
  #
  #   laser_attenuation <-  gsub(
  #     pattern = paste(
  #       ".+<Attenuation>(.+)</Attenuation>.+",
  #       ".+<Attenuation>(.+)</Attenuation>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1, \\2",
  #     x = metadata
  #   )
  #
  #   laser_transmission <-  gsub(
  #     pattern = paste(
  #       ".+<Transmission>(.+)</Transmission>.+",
  #       ".+<Transmission>(.+)</Transmission>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1, \\2",
  #     x = metadata
  #   )
  #
  #   # Detector
  #
  #   # Pixel time in \mu s
  #   pixel_time <- gsub(
  #     pattern = paste(
  #       ".+<PixelTime>(.+)</PixelTime>.+",
  #       ".+<PixelTime>(.+)</PixelTime>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1, \\2",
  #     x = metadata
  #   )
  #
  #   photon_conversion_factor <-
  #     gsub(
  #       pattern = paste(
  #         ".+<PhotonConversionFactor>(.+)</PhotonConversionFactor>.+",
  #         ".+<PhotonConversionFactor>(.+)</PhotonConversionFactor>.+",
  #         sep = ""
  #       ),
  #       replacement = "\\1, \\2",
  #       x = metadata
  #     )
  #
  #   detector_gain <- gsub(
  #     pattern = paste(
  #       ".+<Voltage>(.+)</Voltage>.+",
  #       ".+<Voltage>(.+)</Voltage>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1, \\2",
  #     x = metadata
  #   )
  #
  #   digital_gain <- gsub(
  #     pattern = paste(
  #       ".+<AmplifierGain>(.+)</AmplifierGain>.+",
  #       ".+<AmplifierGain>(.+)</AmplifierGain>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1, \\2",
  #     x = metadata
  #   )
  #
  #   amplifier_offset <- gsub(
  #     pattern = paste(
  #       ".+<AmplifierOffset>(.+)</AmplifierOffset>.+",
  #       ".+<AmplifierOffset>(.+)</AmplifierOffset>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1, \\2",
  #     x = metadata
  #   )
  #
  #   # pinhole diameter in \mu m
  #   pinhole_diameter <- gsub(
  #     pattern = paste(
  #       ".+<PinholeDiameter>(.+)</PinholeDiameter>.+",
  #       ".+<PinholeDiameter>(.+)</PinholeDiameter>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1, \\2",
  #     x = metadata
  #   )
  #
  #   detection_range_wavelength <- gsub(
  #     pattern = paste(
  #       ".+<Ranges>(.+)</Ranges>.+",
  #       ".+<Ranges>(.+)</Ranges>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1, \\2",
  #     x = metadata
  #   )
  #
  # } else if (number_of_channels == 1) {
  #
  #   fluor <- gsub(
  #     pattern =  paste(
  #       ".+<Fluor>(.+)</Fluor>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1",
  #     x = metadata
  #   )
  #
  #   detection_wavelength_start <- gsub(
  #     pattern =  paste(
  #       ".+<Ranges>(.+)-.+</Ranges>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1",
  #     x = metadata
  #   )
  #
  #   detection_wavelength_end <- gsub(
  #     pattern =  paste(
  #       ".+<Ranges>.+-(.+)</Ranges>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1",
  #     x = metadata
  #   )
  #
  #   laser_wavelength <- gsub(
  #     pattern =  paste(
  #       ".+<Attenuator>.+<Wavelength>(.+)</Wavelength>.+</Attenuator>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1",
  #     x = metadata
  #   )
  #
  #   emission_wavelengths <- gsub(
  #     pattern =  paste(
  #       ".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1",
  #     x = metadata
  #   )
  #
  #   laser_name <- gsub(
  #     pattern = paste(
  #       ".+<LaserName>(.+)</LaserName>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1",
  #     x = metadata
  #   )
  #
  #   laser_power <-  gsub(
  #     pattern = paste(".+<LaserPower>(.+)</LaserPower>.+",
  #                     sep = ""),
  #     replacement = "\\1",
  #     x = metadata
  #   )
  #
  #   laser_attenuation <-  gsub(
  #     pattern = paste(".+<Attenuation>(.+)</Attenuation>.+",
  #                     sep = ""),
  #     replacement = "\\1",
  #     x = metadata
  #   )
  #
  #   laser_transmission <-  gsub(
  #     pattern = paste(".+<Transmission>(.+)</Transmission>.+",
  #                     sep = ""),
  #     replacement = "\\1",
  #     x = metadata
  #   )
  #
  #   # Detector
  #
  #   # Pixel time in \mu s
  #   pixel_time <- gsub(
  #     pattern = paste(".+<PixelTime>(.+)</PixelTime>.+",
  #                     sep = ""),
  #     replacement = "\\1",
  #     x = metadata
  #   )
  #
  #   photon_conversion_factor <- gsub(
  #     pattern = paste(
  #       ".+<PhotonConversionFactor>(.+)</PhotonConversionFactor>.+",
  #       sep = ""
  #     ),
  #     replacement = "\\1",
  #     x = metadata
  #   )
  #
  #   detector_gain <- gsub(
  #     pattern = paste(".+<Voltage>(.+)</Voltage>.+",
  #                     sep = ""),
  #     replacement = "\\1",
  #     x = metadata
  #   )
  #
  #   digital_gain <- gsub(
  #     pattern = paste(".+<AmplifierGain>(.+)</AmplifierGain>.+",
  #                     sep = ""),
  #     replacement = "\\1",
  #     x = metadata
  #   )
  #
  #   amplifier_offset <- gsub(
  #     pattern = paste(".+<AmplifierOffset>(.+)</AmplifierOffset>.+",
  #                     sep = ""),
  #     replacement = "\\1",
  #     x = metadata
  #   )
  #
  #   # pinhole diameter in \mu m
  #   pinhole_diameter <- gsub(
  #     pattern = paste(".+<PinholeDiameter>(.+)</PinholeDiameter>.+",
  #                     sep = ""),
  #     replacement = "\\1",
  #     x = metadata
  #   )
  #
  #   detection_range_wavelength <- gsub(
  #     pattern = paste(".+<Ranges>(.+)</Ranges>.+",
  #                     sep = ""),
  #     replacement = "\\1",
  #     x = metadata
  #   )
  #
  # }
  #
  # fluor <- unlist(strsplit(x = fluor, split = ","))
  # fluor <- trimws(fluor)
  # detection_wavelenght_start <- as.numeric(unlist(strsplit(detection_wavelenght_start, split = ",")))
  # detection_wavelenght_end <- as.numeric(unlist(strsplit(detection_wavelenght_end, split = ",")))
  #
  # # Define sorting vector (in which order are the wavelengths of the channels)
  # sorting_vector <- order(detection_wavelenght_start)
  #
  # laser_wavelength <- as.numeric(unlist(strsplit(laser_wavelength, split = ",")))
  # emission_wavelengths <-
  #   as.numeric(unlist(strsplit(emission_wavelengths, split = ",")))
  # laser_name <- unlist(strsplit(x = laser_name, split = ","))
  # laser_name <- trimws(laser_name)
  # laser_name_2 <- unlist(strsplit(x = laser_name_2, split = ","))
  # laser_name_2 <- trimws(laser_name_2)
  # order_lasers <- match(laser_name, laser_name_2)
  #
  # laser_power <-
  #   as.numeric(unlist(strsplit(x = laser_power, split = ",")))
  # laser_power <- laser_power[order_lasers]
  #
  # laser_attenuation <-
  #   as.numeric(unlist(strsplit(x = laser_attenuation, split = ",")))
  # laser_transmission <-
  #   as.numeric(unlist(strsplit(x = laser_transmission, split = ",")))
  #
  # pixel_time <-
  #   as.numeric(unlist(strsplit(x = pixel_time, split = ",")))
  # pixel_time <- pixel_time * 1e6
  # photon_conversion_factor <-
  #   as.numeric(unlist(strsplit(x = photon_conversion_factor, split = ",")))
  # detector_gain <-
  #   as.numeric(unlist(strsplit(x = detector_gain, split = ",")))
  # digital_gain <-
  #   as.numeric(unlist(strsplit(x = digital_gain, split = ",")))
  # amplifier_offset <-
  #   as.numeric(unlist(strsplit(x = amplifier_offset, split = ",")))
  #
  # pinhole_diameter <-
  #   as.numeric(unlist(strsplit(x = pinhole_diameter, split = ",")))
  # pinhole_diameter <- pinhole_diameter * 1e6
  #
  # detection_range_wavelength <-
  #   unlist(strsplit(x = detection_range_wavelength, split = ","))
  # detection_range_wavelength <-
  #   unlist(strsplit(x = detection_range_wavelength, split = "-"))
  # detection_range_wavelength <-
  #   as.numeric(detection_range_wavelength)

  # Put information into a data frame
  df_metadata <- data.frame(
    "fileName" = NA,
    "acquisition_date" = NA,
    "acquisition_time" = NA,
    "microscopy_system" = NA,
    "color_system" = NA,
    "number_of_channels" = NA,
    "objective_magnification" = NA,
    "objective" = NA,
    "dim_x" = NA,
    "dim_y" = NA,
    "dim_z" = NA,
    "scaling_x" = NA,
    "scaling_y" = NA,
    "scaling_z" = NA,
    "channel_name_1" = channel_names[1],
    "channel_name_2" = channel_names[2],
    "channel_name_3" = channel_names[3],
    "fluorophore_1" = fluorophores[1],
    "fluorophore_2" = fluorophores[2],
    "fluorophore_3" = fluorophores[3],
    "detection_wavelength_start_1" = detection_wavelength_start[1],
    "detection_wavelength_end_1" = detection_wavelength_end[1],
    "detection_wavelength_start_2" = detection_wavelength_start[2],
    "detection_wavelength_end_2" = detection_wavelength_end[2],
    "detection_wavelength_start_3" = detection_wavelength_start[3],
    "detection_wavelength_end_3" = detection_wavelength_end[3],
    "excitation_wavelength_1"= excitation_wavelengths[1],
    "excitation_wavelength_2"= excitation_wavelengths[2],
    "excitation_wavelength_3"= excitation_wavelengths[3],
    "emission_wavelength_1"= emission_wavelengths[1],
    "emission_wavelength_2"= emission_wavelengths[2],
    "emission_wavelength_3"= emission_wavelengths[3],
    "laser_scan_pixel_time_1" = laser_scan_pixel_times[1],
    "laser_scan_pixel_time_2" = laser_scan_pixel_times[2],
    "laser_scan_pixel_time_3" = laser_scan_pixel_times[3],
    "laser_scan_averaging_1" = averaging[1],
    "laser_scan_averaging_2" = averaging[2],
    "laser_scan_averaging_3" = averaging[3],
    "laser_scan_zoom_x_1" = laser_scan_zoom_x[1],
    "laser_scan_zoom_x_2" = laser_scan_zoom_x[2],
    "laser_scan_zoom_x_3" = laser_scan_zoom_x[3],
    "laser_scan_zoom_y_1" = laser_scan_zoom_y[1],
    "laser_scan_zoom_y_2" = laser_scan_zoom_y[2],
    "laser_scan_zoom_Y_3" = laser_scan_zoom_y[3],
    "photon_conversion_factor_1" = photon_conversion_factors[1],
    "photon_conversion_factor_2" = photon_conversion_factors[2],
    "photon_conversion_factor_3" = photon_conversion_factors[3],
    "detector_gain_1" = detector_gain[1],
    "detector_gain_2" = detector_gain[2],
    "detector_gain_3" = detector_gain[3],
    "digital_gain_1" = digital_gain[1],
    "digital_gain_2" = digital_gain[2],
    "digital_gain_3" = digital_gain[3],
    "amplifier_offset_1" = amplifier_offset[1],
    "amplifier_offset_2" = amplifier_offset[2],
    "amplifier_offset_3" = amplifier_offset[3],
    "laser_wavelength_1" = laser_wavelength[1],
    "laser_wavelength_2" = laser_wavelength[2],
    "laser_wavelength_3" = laser_wavelength[3],
    "laser_name_1" = laser_name[1],
    "laser_name_2" = laser_name[2],
    "laser_name_3" = laser_name[3],
    "laser_power_1" = laser_power[1],
    "laser_power_2" = laser_power[2],
    "laser_power_3" = laser_power[3],
    "laser_transmission_1" = laser_transmission[1],
    "laser_transmission_2" = laser_transmission[2],
    "laser_transmission_3" = laser_transmission[3]
  )

  return(df_metadata)

}
