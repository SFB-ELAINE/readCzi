#' Check for LSM-specific values in metadata
#'
#' @param metadata Loaded metadata as a string.
#' @param number_of_channels A number representing the number of channels
#' (color layers).
#' @param number_of_tracks A number representing the number of experimental
#' tracks.
#'
#' @keywords internal

readCziMetadata_LSM <- function(metadata = NULL,
                                number_of_channels = NULL,
                                number_of_tracks = NULL) {

  # Default values for missing arguments ###################################
  if(is.null(metadata) ||
     is.null(number_of_channels) ||
     is.null(number_of_tracks)){
    print(paste("Please call the function correctly.", sep=""))
    return()
  }

  # Get information of laser scanning microscopes ##########################
  metadata_XML <- xml2::read_xml(x = metadata)

  # Empty tibble with channel info
  df_channel_info <- tibble::tibble(track_id = rep(NA, number_of_channels),
                                    channel_id = rep(NA, number_of_channels),
                                    channel_name = rep(NA, number_of_channels))

  # Go through each track and write down channels
  tracks_information <- xml2::xml_find_all(x = metadata_XML, xpath = ".//Tracks/Track")
  tracks_ids <- unique(xml2::xml_attr(x = tracks_information, attr = "Id"))

  for(i in 1:length(tracks_ids)){
    # Find all channels belonging to a track
    current_track <- tracks_ids[i]
    current_track_information <- xml2::xml_find_all(
      x = metadata_XML,
      xpath = paste0(".//Tracks/Track[@Id='",current_track,"']"))
    channel_refs <- xml2::xml_attr(x = xml2::xml_find_all(
      x = current_track_information, xpath = ".//ChannelRef"),
      attr = "Id")

    # Fill tibble
    first_empty_row <- which(is.na(df_channel_info$channel_id))[1]

    df_channel_info$track_id[
      first_empty_row:(first_empty_row+length(channel_refs)-1)] <- current_track

    df_channel_info$channel_id[
      first_empty_row:(first_empty_row+length(channel_refs)-1)] <- channel_refs
  }

  rm(i)

  # Go through all channel ids and add channel names
  for(i in 1:length(df_channel_info$channel_id)){
    current_channel_id <- df_channel_info$channel_id[i]
    current_channel_information <- xml2::xml_find_all(
      x = metadata_XML,
      xpath = paste0(".//Dimensions/Channels/Channel[@Id='",
                     current_channel_id, "']"))
    current_channel_name <- xml2::xml_attr(x = current_channel_information,
                                           attr = "Name")
    df_channel_info$channel_name[
      df_channel_info$channel_id == current_channel_id] <- current_channel_name
  }

  if(number_of_channels != sum(!is.na(df_channel_info$channel_name))){
    print("The number of channels does not correspond to the number of channel names in the metadata.")
  }

  channel_names <- df_channel_info$channel_name
  track_ids <- df_channel_info$track_id

  # Get channel information ------------------------------------------------

  # Empty vectors
  detector_identifier <- rep(x = NA, number_of_channels)
  contrast_method <- rep(x = NA, number_of_channels)
  fluorophores <- rep(x = NA, number_of_channels)
  detection_wavelength_start_in_nm <- rep(x = NA, number_of_channels)
  detection_wavelength_end_in_nm <- rep(x = NA, number_of_channels)
  excitation_wavelengths_in_nm <- rep(x = NA, number_of_channels)
  emission_wavelengths_in_nm <- rep(x = NA, number_of_channels)
  laser_scan_pixel_times_in_ms <- rep(x = NA, number_of_channels)
  laser_scan_line_times_in_ms <- rep(x = NA, number_of_channels)
  laser_scan_frame_times_in_ms <- rep(x = NA, number_of_channels)
  averaging <- rep(x = NA, number_of_channels)
  laser_scan_zoom_x <- rep(x = NA, number_of_channels)
  laser_scan_zoom_y <- rep(x = NA, number_of_channels)
  photon_conversion_factors <- rep(x = NA, number_of_channels)
  pinhole_size_in_airy_unit <- rep(x = NA, number_of_channels)
  pinhole_diameter_in_m <- rep(x = NA, number_of_channels)
  detector_gain <- rep(x = NA, number_of_channels)
  digital_gain <- rep(x = NA, number_of_channels)
  amplifier_offset <- rep(x = NA, number_of_channels)

  # Go through each channel and get information
  for(i in 1:number_of_channels){

    # Filter for channel ID
    look_for <- paste(".//Dimensions/Channels/Channel[@Id='", df_channel_info$channel_id[i], "']", sep="")
    channel_information <- xml2::xml_find_all(x = metadata_XML, xpath = look_for)
    channel_information <- xml2::as_list(channel_information)

    # Contrast Method
    if(!is.null(unlist(channel_information[[1]]$ContrastMethod))){
      contrast_method[i] <- unlist(channel_information[[1]]$ContrastMethod)
    }

    # Fluorphore
    if(!is.null(unlist(channel_information[[1]]$Fluor))){
      fluorophores[i] <- unlist(channel_information[[1]]$Fluor)
    }

    # Detection wavelengths
    if(!is.null(unlist(channel_information[[1]]$DetectionWavelength$Ranges))){
      detection_wavelength_start_in_nm[i] <- as.numeric(unlist(
        strsplit(
          x = unlist(channel_information[[1]]$DetectionWavelength$Ranges),
          split = "-"))[1])
      detection_wavelength_end_in_nm[i] <- as.numeric(unlist(
        strsplit(
          x = unlist(channel_information[[1]]$DetectionWavelength$Ranges),
          split = "-"))[2])
    }


    # Excitation and emission wavelengths
    if(!is.null(unlist(channel_information[[1]]$ExcitationWavelength))){
      excitation_wavelengths_in_nm[i] <- as.numeric(unlist(channel_information[[1]]$ExcitationWavelength))
    }

    if(!is.null(unlist(channel_information[[1]]$EmissionWavelength))){
      emission_wavelengths_in_nm[i] <- as.numeric(unlist(channel_information[[1]]$EmissionWavelength))
    }

    # Laser scan pixel times
    if(!is.null(unlist(channel_information[[1]]$LaserScanInfo$PixelTime))){
      laser_scan_pixel_times_in_ms[i] <- as.numeric(unlist(channel_information[[1]]$LaserScanInfo$PixelTime))
      if(laser_scan_pixel_times_in_ms[i] < 1e-3){
        laser_scan_pixel_times_in_ms[i] <- laser_scan_pixel_times_in_ms[i]*1e6
      }
    }
    # Laser scan line times
    if(!is.null(unlist(channel_information[[1]]$LaserScanInfo$LineTime))){
      laser_scan_line_times_in_ms[i] <- as.numeric(unlist(channel_information[[1]]$LaserScanInfo$LineTime))
      if(laser_scan_line_times_in_ms[i] < 1e-3){
        laser_scan_line_times_in_ms[i] <- laser_scan_line_times_in_ms[i]*1e6
      }
    }
    # Laser scan fame times
    if(!is.null(unlist(channel_information[[1]]$LaserScanInfo$FrameTime))){
      laser_scan_frame_times_in_ms[i] <- as.numeric(unlist(channel_information[[1]]$LaserScanInfo$FrameTime))
      if(laser_scan_frame_times_in_ms[i] < 1e-3){
        laser_scan_frame_times_in_ms[i] <- laser_scan_frame_times_in_ms[i]*1e6
      }
    }

    # Laser scan averaging
    if(!is.null(unlist(channel_information[[1]]$LaserScanInfo$Averaging))){
      averaging[i] <- as.numeric(unlist(channel_information[[1]]$LaserScanInfo$Averaging))
    }

    # Laser scan zoom
    if(!is.null(unlist(channel_information[[1]]$LaserScanInfo$ZoomX))){
      laser_scan_zoom_x[i] <- as.numeric(unlist(channel_information[[1]]$LaserScanInfo$ZoomX))
    }
    if(!is.null(unlist(channel_information[[1]]$LaserScanInfo$ZoomY))){
      laser_scan_zoom_y[i] <- as.numeric(unlist(channel_information[[1]]$LaserScanInfo$ZoomY))
    }

    # Pinhole settings
    if(!is.null(unlist(channel_information[[1]]$PinholeSizeAiry))){
      pinhole_size_in_airy_unit[i] <- as.numeric(unlist(unlist(channel_information[[1]]$PinholeSizeAiry)))
    }

    # Detector settings
    if(!is.null(unlist(channel_information[[1]]$DetectorSettings$PhotonConversionFactor))){
      photon_conversion_factors[i] <- as.numeric(unlist(channel_information[[1]]$DetectorSettings$PhotonConversionFactor))
    }
    if(!is.null(unlist(channel_information[[1]]$DetectorSettings$Gain))){
      detector_gain[i] <- as.numeric(unlist(channel_information[[1]]$DetectorSettings$Gain))
    }
    if(!is.null(unlist(channel_information[[1]]$DetectorSettings$DigitalGain))){
      digital_gain[i] <- as.numeric(unlist(channel_information[[1]]$DetectorSettings$DigitalGain))
    }
    if(!is.null(unlist(channel_information[[1]]$DetectorSettings$Offset))){
      amplifier_offset[i] <- as.numeric(unlist(channel_information[[1]]$DetectorSettings$Offset))
    }

    # Get detector identifier
    look_for <- paste(".//Detector[@Id='", df_channel_info$channel_id[i], "']", sep="")
    detector_information <- xml2::xml_find_all(x = metadata_XML, xpath = look_for)
    detector_information <- xml2::xml_find_all(x = detector_information, xpath = ".//DetectorIdentifier")

    # Contrast Method
    if(length(detector_information)>0){
      detector_identifier[i] <- xml2::xml_text(x = detector_information)
    }

    look_for <- paste(".//Detectors/Detector[@Id='", df_channel_info$channel_id[i], "']", sep="")
    detector_information <- xml2::xml_find_all(x = metadata_XML, xpath = look_for)
    detector_information <- xml2::as_list(detector_information)

    # Pinhole diameter
    if(!is.null(unlist(detector_information[[1]]$PinholeDiameter))){
      pinhole_diameter_in_m[i] <- as.numeric(unlist(detector_information[[1]]$PinholeDiameter))
    }

  }
  rm(i)

  # Get experiment tracks information --------------------------------------

  # Empty vectors
  laser_wavelength_in_nm <- rep(x = NA, number_of_channels)
  laser_transmission <- rep(x = NA, number_of_channels)
  laser_name <- rep(x = NA, number_of_channels)
  laser_power_in_mW <- rep(x = NA, number_of_channels)


  look_for <- paste0(".//Experiment")
  experiment_information <- xml2::xml_find_all(x = metadata_XML, xpath = look_for)

  tracksetup_information <- xml2::xml_find_all(x = experiment_information, xpath = ".//TrackSetup")
  track_names <- xml2::xml_attr(x = tracksetup_information, attr = "Name")

  laserpowers <- xml2::xml_find_all(x = experiment_information,
                                    xpath = ".//Lasers/Laser/LaserPower")
  laserpowers <- xml2::xml_double(x = laserpowers)

  lasernames <- xml2::xml_find_all(x = experiment_information,
                                   xpath = ".//Lasers/Laser/LaserName")
  lasernames <- xml2::xml_text(x = lasernames)

  for(i in 1:number_of_tracks){

    current_track_name <- track_names[i]

    current_track_information <- tracksetup_information[i]

    current_laser <- xml2::xml_find_all(x = current_track_information,
                                        xpath = ".//Laser")

    laser_name[i] <- paste0(xml2::xml_text(x = current_laser), collapse = ", ")

    if(any(unlist(strsplit(x = laser_name[i], split = ", ")) %in% lasernames)){
      # Laser power in mW
      laser_power_in_mW[i] <- paste0(laserpowers[
        which(lasernames %in% unlist(
          strsplit(x = laser_name[i], split = ", ")))], collapse = ", ")
    }

    wavelength <- xml2::xml_find_all(x = current_track_information,
                                     xpath = ".//Wavelength")
    wavelength <- xml2::xml_double(wavelength)

    if(length(wavelength)>0){
      if(length(wavelength) > 1){
        laser_wavelength_in_nm[i] <- paste0(wavelength*1e9, collapse = ", ")
      }else{
        laser_wavelength_in_nm[i] <- wavelength*1e9
      }

    }

    lasertransmission <- xml2::xml_find_all(x = current_track_information,
                                            xpath = ".//Transmission")
    lasertransmission <- xml2::xml_double(lasertransmission)

    if(length(lasertransmission)>0){
      if(length(wavelength) > 1){
        laser_transmission[i] <- paste0(lasertransmission, collapse = ", ")
      }else{
        laser_transmission[i] <- lasertransmission
      }

    }

  }

  rm(i)

  if(!any(grepl(pattern = ",", x = laser_power_in_mW))){
    laser_power_in_mW <- as.numeric(laser_power_in_mW)
  }
  if(!any(grepl(pattern = ",", x = laser_wavelength_in_nm))){
    laser_wavelength_in_nm <- as.numeric(laser_wavelength_in_nm)
  }
  if(!any(grepl(pattern = ",", x = laser_transmission))){
    laser_transmission <- as.numeric(laser_transmission)
  }

  # Finding the color of each channel and the corresponding channel number ----

  # Upper and lower limits of emission wavelengths to determine the colors
  red_limit <- 600 #>600nm
  # green: #>= 500nm and <= 600nm
  blue_limit <- 500 #< 500nm

  # Sorting the channels information regarding wavelength
  channel_order <- order(detection_wavelength_start_in_nm)

  if(number_of_channels == 3){
    # 1: blue, 2: green, 3: red
    channel_color <- channel_order
  }else{

    channel_color <- rep(NA, 3)

    for(i in 1:number_of_channels){

      if(!is.na(emission_wavelengths_in_nm[i])){
        if(emission_wavelengths_in_nm[i] < blue_limit){
          # Finding the blue channel
          channel_color[1] <- channel_order[i]

        }else if(emission_wavelengths_in_nm[i] > red_limit){
          # Finding the red channel
          channel_color[3] <- channel_order[i]

        }else{
          # Finding the green channel
          channel_color[2] <- channel_order[i]

        }
      }else{

        if(!is.na(detection_wavelength_start_in_nm[i])){
          if(detection_wavelength_start_in_nm[i] < (blue_limit-50)){
            # Finding the blue channel
            channel_color[1] <- channel_order[i]

          }else if(detection_wavelength_start_in_nm[i] > (red_limit-50)){
            # Finding the red channel
            channel_color[3] <- channel_order[i]

          }else{
            # Finding the green channel
            channel_color[2] <- channel_order[i]

          }
        }
      }
    }
  }

  # Recalculate if numbers are not in the right unit
  detection_wavelength_start_in_nm[detection_wavelength_start_in_nm < 1e-6] <-
    detection_wavelength_start_in_nm[detection_wavelength_start_in_nm < 1e-6] * 1e9

  detection_wavelength_end_in_nm[detection_wavelength_end_in_nm < 1e-6] <-
    detection_wavelength_end_in_nm[detection_wavelength_end_in_nm < 1e-6] * 1e9

  excitation_wavelengths_in_nm[excitation_wavelengths_in_nm < 1e-6] <-
    excitation_wavelengths_in_nm[excitation_wavelengths_in_nm < 1e-6] * 1e9

  emission_wavelengths_in_nm[emission_wavelengths_in_nm < 1e-6] <-
    emission_wavelengths_in_nm[emission_wavelengths_in_nm < 1e-6] * 1e9

  # Put information into a data frame
  df_metadata <- data.frame(
    "fileName" = NA,
    "acquisition_date" = NA,
    "acquisition_time" = NA,
    "microscopy_system" = NA,
    "color_system" = NA,
    "number_of_channels" = NA,
    "number_of_tracks" = NA,
    "objective" = NA,
    "objective_magnification" = NA,
    "dim_x" = NA,
    "dim_y" = NA,
    "dim_z" = NA,
    "scaling_x_in_um" = NA,
    "scaling_y_in_um" = NA,
    "scaling_z_in_um" = NA,
    "blue_channel" = channel_color[1],
    "green_channel" = channel_color[2],
    "red_channel" = channel_color[3],
    "channel_name_1" = channel_names[1],
    "channel_name_2" = channel_names[2],
    "channel_name_3" = channel_names[3],
    "track_id_channel_1" = track_ids[1],
    "track_id_channel_2" = track_ids[2],
    "track_id_channel_3" = track_ids[3],
    "contrast_method_channel_1" = contrast_method[1],
    "contrast_method_channel_2" = contrast_method[2],
    "contrast_method_channel_3" = contrast_method[3],
    "fluorophore_channel_1" = fluorophores[1],
    "fluorophore_channel_2" = fluorophores[2],
    "fluorophore_channel_3" = fluorophores[3],
    "pinhole_size_in_airy_unit_1" = pinhole_size_in_airy_unit[1],
    "pinhole_size_in_airy_unit_2" = pinhole_size_in_airy_unit[2],
    "pinhole_size_in_airy_unit_3" = pinhole_size_in_airy_unit[3],
    "pinhole_diameter_in_m_1" = pinhole_diameter_in_m[1],
    "pinhole_diameter_in_m_2" = pinhole_diameter_in_m[2],
    "pinhole_diameter_in_m_3" = pinhole_diameter_in_m[3],
    "detector_identifier_1" = detector_identifier[1],
    "detector_identifier_2" = detector_identifier[2],
    "detector_identifier_3" = detector_identifier[3],
    "detection_wavelength_start_channel_1_in_nm" = detection_wavelength_start_in_nm[1],
    "detection_wavelength_end_channel_1_in_nm" = detection_wavelength_end_in_nm[1],
    "detection_wavelength_start_channel_2_in_nm" = detection_wavelength_start_in_nm[2],
    "detection_wavelength_end_channel_2_in_nm" = detection_wavelength_end_in_nm[2],
    "detection_wavelength_start_channel_3_in_nm" = detection_wavelength_start_in_nm[3],
    "detection_wavelength_end_channel_3_in_nm" = detection_wavelength_end_in_nm[3],
    "excitation_wavelength_channel_1_in_nm"= excitation_wavelengths_in_nm[1],
    "excitation_wavelength_channel_2_in_nm"= excitation_wavelengths_in_nm[2],
    "excitation_wavelength_channel_3_in_nm"= excitation_wavelengths_in_nm[3],
    "emission_wavelength_channel_1_in_nm"= emission_wavelengths_in_nm[1],
    "emission_wavelength_channel_2_in_nm"= emission_wavelengths_in_nm[2],
    "emission_wavelength_channel_3_in_nm"= emission_wavelengths_in_nm[3],
    "laser_scan_pixel_time_channel_1_in_ms" = laser_scan_pixel_times_in_ms[1],
    "laser_scan_pixel_time_channel_2_in_ms" = laser_scan_pixel_times_in_ms[2],
    "laser_scan_pixel_time_channel_3_in_ms" = laser_scan_pixel_times_in_ms[3],
    "laser_scan_line_time_channel_1_in_ms" = laser_scan_line_times_in_ms[1],
    "laser_scan_line_time_channel_2_in_ms" = laser_scan_line_times_in_ms[2],
    "laser_scan_line_time_channel_3_in_ms" = laser_scan_line_times_in_ms[3],
    "laser_scan_frame_time_channel_1_in_ms" = laser_scan_frame_times_in_ms[1],
    "laser_scan_frame_time_channel_2_in_ms" = laser_scan_frame_times_in_ms[2],
    "laser_scan_frame_time_channel_3_in_ms" = laser_scan_frame_times_in_ms[3],
    "laser_scan_averaging_channel_1" = averaging[1],
    "laser_scan_averaging_channel_2" = averaging[2],
    "laser_scan_averaging_channel_3" = averaging[3],
    "laser_scan_zoom_x_channel_1" = laser_scan_zoom_x[1],
    "laser_scan_zoom_x_channel_2" = laser_scan_zoom_x[2],
    "laser_scan_zoom_x_channel_3" = laser_scan_zoom_x[3],
    "laser_scan_zoom_y_channel_1" = laser_scan_zoom_y[1],
    "laser_scan_zoom_y_channel_2" = laser_scan_zoom_y[2],
    "laser_scan_zoom_Y_channel_3" = laser_scan_zoom_y[3],
    "photon_conversion_factor_channel_1" = photon_conversion_factors[1],
    "photon_conversion_factor_channel_2" = photon_conversion_factors[2],
    "photon_conversion_factor_channel_3" = photon_conversion_factors[3],
    "detector_gain_channel_1" = detector_gain[1],
    "detector_gain_channel_2" = detector_gain[2],
    "detector_gain_channel_3" = detector_gain[3],
    "digital_gain_channel_1" = digital_gain[1],
    "digital_gain_channel_2" = digital_gain[2],
    "digital_gain_channel_3" = digital_gain[3],
    "amplifier_offset_channel_1" = amplifier_offset[1],
    "amplifier_offset_channel_2" = amplifier_offset[2],
    "amplifier_offset_channel_3" = amplifier_offset[3],
    "laser_wavelength_track_1_in_nm" = laser_wavelength_in_nm[1],
    "laser_wavelength_track_2_in_nm" = laser_wavelength_in_nm[2],
    "laser_wavelength_track_3_in_nm" = laser_wavelength_in_nm[3],
    "laser_name_track_1" = laser_name[1],
    "laser_name_track_2" = laser_name[2],
    "laser_name_track_3" = laser_name[3],
    "laser_power_in_mW_track_1" = laser_power_in_mW[1],
    "laser_power_in_mW_track_2" = laser_power_in_mW[2],
    "laser_power_in_mW_track_3" = laser_power_in_mW[3],
    "laser_transmission_track_1" = laser_transmission[1],
    "laser_transmission_track_2" = laser_transmission[2],
    "laser_transmission_track_3" = laser_transmission[3]
  )

  return(df_metadata)

}
