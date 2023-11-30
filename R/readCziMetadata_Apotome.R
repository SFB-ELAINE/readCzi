#' Check for apotome-specific values in metadata
#' @param metadata A character (loaded metadata in a string)
#' @param number_of_channels A number
#' @keywords internal

readCziMetadata_Apotome <- function(metadata = NULL,
                                    number_of_channels = NULL,
                                    number_of_tracks = NULL) {

  # Default values for missing arguments ###################################
  if(is.null(metadata) ||
     is.null(number_of_channels) ||
     is.null(number_of_tracks)){
    print(paste("Please call the function correctly.", sep=""))
    return()
  }

  # Get information of laser microscopes with Apotome ######################
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

  # Empty vectors
  contrast_method <- rep(x = NA, number_of_channels)
  acquisition_mode <- rep(x = NA, number_of_channels)
  illumination_type <- rep(x = NA, number_of_channels)
  excitation_wavelengths_in_nm <- rep(x = NA, number_of_channels)
  emission_wavelengths_in_nm <- rep(x = NA, number_of_channels)
  illumination_wavelengths_in_nm <- rep(x = NA, number_of_channels)
  light_source_intensities <- rep(x = NA, number_of_channels)
  exposure_times_in_ms <- rep(x = NA, number_of_channels)
  fluorophores <- rep(x = NA, number_of_channels)

  # Go through each channel and get information ############################
  for(i in 1:number_of_channels){

    # Filter for channel name
    look_for <- paste(".//Dimensions/Channels/Channel[@Name='", channel_names[i], "']", sep="")
    channel_information <- xml2::xml_find_all(x = metadata_XML, xpath = look_for)
    channel_information <- xml2::as_list(channel_information)

    # Contrast Method
    if(grepl(pattern = "ContrastMethod", x = channel_information, ignore.case = TRUE)){
      contrast_method[i] <- unlist(channel_information[[1]]$ContrastMethod)
    }

    # Acquisition Mode
    if(grepl(pattern = "AcquisitionMode", x = channel_information, ignore.case = TRUE)){
      acquisition_mode[i] <- unlist(channel_information[[1]]$AcquisitionMode)
    }else{
      acquisition_mode[i] <- NA
    }

    # Illumination Type
    if(grepl(pattern = "IlluminationType", x = channel_information, ignore.case = TRUE)){
      illumination_type[i] <- unlist(channel_information[[1]]$IlluminationType)
    }else{
      illumination_type[i] <- NA
    }

    # LED widefield

    # Excitation and emission wavelengths
    if(grepl(pattern = "ExcitationWavelength", x = channel_information, ignore.case = TRUE)){
      excitation_wavelengths_in_nm[i] <- as.numeric(unlist(channel_information[[1]]$ExcitationWavelength))
    }
    if(grepl(pattern = "EmissionWavelength", x = channel_information, ignore.case = TRUE)){
      emission_wavelengths_in_nm[i] <- as.numeric(unlist(channel_information[[1]]$EmissionWavelength))
    }

    # Illumination wavelengths and light source intensity
    if(grepl(pattern = "IlluminationWavelength", x = channel_information, ignore.case = TRUE)){
      illumination_wavelengths_in_nm[i] <- as.numeric(unlist(channel_information[[1]]$IlluminationWavelength$SinglePeak))
    }
    if(grepl(pattern = "LightSourceSettings", x = channel_information, ignore.case = TRUE)){
      dump_light_source_intensities <- unlist(channel_information[[1]]$LightSourcesSettings$LightSourceSettings$Intensity)
      dump_light_source_intensities <- gsub(pattern = "\\%", replacement = "", x = dump_light_source_intensities)
      dump_light_source_intensities <- gsub(pattern = " ", replacement = "", x = dump_light_source_intensities)
      light_source_intensities[i] <- as.numeric(dump_light_source_intensities)
      rm(dump_light_source_intensities)
    }

    # Exposure Time
    if(grepl(pattern = "ExposureTime", x = channel_information, ignore.case = TRUE)){
      exposure_times_in_ms[i] <- as.numeric(unlist(channel_information[[1]]$ExposureTime))

      if(exposure_times_in_ms[i] > 1e6){
        exposure_times_in_ms[i] <- exposure_times_in_ms[i]/1e6
      }
    }

    # Fluorphore
    fluorophores[i] <- unlist(channel_information[[1]]$Fluor)

  }

  # Get scene information ##################################################

  # Filter for scene information
  if(grepl(pattern = "<Scenes", x = metadata_XML, ignore.case = TRUE)){
    if(grepl(pattern = "Scene Index=\"1\"", x = metadata_XML, ignore.case = TRUE)){
      print("Please split the scenes before using this package.")
      return()
    }else{
      look_for <- paste(".//Scene", sep="")
      scenes_information <- xml2::xml_find_all(x = metadata_XML, xpath = look_for)

      scene_name <- unique(xml2::xml_attr(x = scenes_information, attr = "Name"))


      scenes_information <- xml2::as_list(scenes_information)

      scene_center_position <- unlist(scenes_information[[1]]$CenterPosition)
      scene_center_position_x <- as.numeric(strsplit(x = scene_center_position, split = ",")[[1]][1])
      scene_center_position_y <- as.numeric(strsplit(x = scene_center_position, split = ",")[[1]][2])
      rm(scene_center_position)

      contour_size <- unlist(scenes_information[[1]]$ContourSize)
      contour_size_x <- as.numeric(strsplit(x = contour_size, split = ",")[[1]][1])
      contour_size_y <- as.numeric(strsplit(x = contour_size, split = ",")[[1]][2])
      rm(contour_size)
    }
  }else{
    scene_name <- NA
    scene_center_position_x <- NA
    scene_center_position_y <- NA
    contour_size_x <- NA
    contour_size_y <- NA
  }


  # Finding the color of each channel and the corresponding channel number ----

  # Sorting the channels information regarding wavelength
  channel_order <- order(illumination_wavelengths_in_nm)

  # Upper and lower limits of emission wavelengths to determine the colors
  red_limit <- 600 #>600nmnm
  # green: #>= 500nm and <= 600nm
  blue_limit <- 500 #< 500nm

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
        if(illumination_wavelengths_in_nm[i] < (blue_limit-50)){
          # Finding the blue channel
          channel_color[1] <- channel_order[i]

        }else if(illumination_wavelengths_in_nm[i] > (red_limit-50)){
          # Finding the red channel
          channel_color[3] <- channel_order[i]

        }else{
          # Finding the green channel
          channel_color[2] <- channel_order[i]

        }
      }


    }

  }

  # Recalculate if numbers are not in the right unit
  excitation_wavelengths_in_nm[!is.na(excitation_wavelengths_in_nm) &
                                 excitation_wavelengths_in_nm < 1e-6] <-
    excitation_wavelengths_in_nm[excitation_wavelengths_in_nm < 1e-6] * 1e9

  emission_wavelengths_in_nm[!is.na(emission_wavelengths_in_nm) &
                               emission_wavelengths_in_nm < 1e-6] <-
    emission_wavelengths_in_nm[emission_wavelengths_in_nm < 1e-6] * 1e9

  illumination_wavelengths_in_nm[!is.na(illumination_wavelengths_in_nm) &
                                   illumination_wavelengths_in_nm < 1e-6] <-
    illumination_wavelengths_in_nm[illumination_wavelengths_in_nm < 1e-6] * 1e9

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
    "scene_name" = scene_name,
    "scene_center_position_x" = scene_center_position_x,
    "scene_center_position_y" = scene_center_position_y,
    "contour_size_x" = contour_size_x,
    "contour_size_y" = contour_size_y,
    "acquisition_mode_channel_1" = acquisition_mode[1],
    "acquisition_mode_channel_2" = acquisition_mode[2],
    "acquisition_mode_channel_3" = acquisition_mode[3],
    "illumination_type_channel_1" = illumination_type[1],
    "illumination_type_channel_2" = illumination_type[2],
    "illumination_type_channel_3" = illumination_type[3],
    "illumination_wavelength_channel_1_in_nm" =  illumination_wavelengths_in_nm[1],
    "illumination_wavelength_channel_2_in_nm" =  illumination_wavelengths_in_nm[2],
    "illumination_wavelength_channel_3_in_nm" =  illumination_wavelengths_in_nm[3],
    "excitation_wavelength_channel_1_in_nm" =  excitation_wavelengths_in_nm[1],
    "excitation_wavelength_channel_2_in_nm" =  excitation_wavelengths_in_nm[2],
    "excitation_wavelength_channel_3_in_nm" =  excitation_wavelengths_in_nm[3],
    "emission_wavelength_channel_1_in_nm" =  emission_wavelengths_in_nm[1],
    "emission_wavelength_channel_2_in_nm" =  emission_wavelengths_in_nm[2],
    "emission_wavelength_channel_3_in_nm" =  emission_wavelengths_in_nm[3],
    "light_source_intensity_channel_1" =  light_source_intensities[1],
    "light_source_intensity_channel_2" =  light_source_intensities[2],
    "light_source_intensity_channel_3" =  light_source_intensities[3],
    "exposure_time_channel_1_in_ms" = exposure_times_in_ms[1],
    "exposure_time_channel_2_in_ms" = exposure_times_in_ms[2],
    "exposure_time_channel_3_in_ms" = exposure_times_in_ms[3]
  )

  return(df_metadata)

}
