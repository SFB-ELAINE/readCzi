#' Check for apotome-specific values in metadata
#' @param metadata A character (loaded metadata in a string)
#' @param number_of_channels A number
#' @keywords internal

readCziMetadata_Apotome <- function(metadata = metadata,
                                    number_of_channels = number_of_channels) {

  metadata_XML <- xml2::read_xml(x = metadata)

  # Get information of laser microscopes with Apotome ######################

  # Channel names
  look_for <- paste(".//Channel", sep="")
  channel_information <- xml2::xml_find_all(x = metadata_XML, xpath = look_for)

  xml2::xml_attr(x = channel_information, attr = "Name")

  channel_names <- unique(xml2::xml_attr(x = channel_information, attr = "Name"))

  # Check different source for channel names if the number of names
  # does not match the number of channels
  if(number_of_channels != length(channel_names)){

    look_for <- paste(".//DisplaySetting/Channels/Channel", sep="")
    channel_information <- xml2::xml_find_all(x = metadata_XML, xpath = look_for)
    channel_names <- unique(xml2::xml_attr(x = channel_information, attr = "Name"))

    if(number_of_channels != length(channel_names)){
      print("The number of channels does not correspond to the number of channel names in the metadata.")
      return()
    }
  }

  # Empty vectors
  acquisition_mode <- rep(x = NA, number_of_channels)
  illumination_type <- rep(x = NA, number_of_channels)
  excitation_wavelengths_in_nm <- rep(x = NA, number_of_channels)
  emission_wavelengths_in_nm <- rep(x = NA, number_of_channels)
  illumination_wavelengths_in_nm <- rep(x = NA, number_of_channels)
  light_source_intensitys <- rep(x = NA, number_of_channels)
  exposure_times <- rep(x = NA, number_of_channels)
  fluorophores <- rep(x = NA, number_of_channels)

  # Go through each channel and get information ############################
  for(i in 1:number_of_channels){

    # Filter for channel name
    look_for <- paste(".//Dimensions/Channels/Channel[@Name='", channel_names[i], "']", sep="")
    channel_information <- xml2::xml_find_all(x = metadata_XML, xpath = look_for)
    channel_information <- xml2::as_list(channel_information)

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
      dump_light_source_intensitys <- unlist(channel_information[[1]]$LightSourcesSettings$LightSourceSettings$Intensity)
      dump_light_source_intensitys <- gsub(pattern = "\\%", replacement = "", x = dump_light_source_intensitys)
      dump_light_source_intensitys <- gsub(pattern = " ", replacement = "", x = dump_light_source_intensitys)
      light_source_intensitys[i] <- as.numeric(dump_light_source_intensitys)
      rm(dump_light_source_intensitys)
    }

    # Exposure Time
    if(grepl(pattern = "ExposureTime", x = channel_information, ignore.case = TRUE)){
      exposure_times[i] <- as.numeric(unlist(channel_information[[1]]$ExposureTime))
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

  # Sorting the channels information regarding wavelength ------------------
  channel_order <- order(illumination_wavelengths_in_nm)

  order_of_channels <- 1:number_of_channels
  if(!all.equal(order_of_channels, channel_order)){

    acquisition_mode_copy <- acquisition_mode
    illumination_type_copy <- illumination_type
    illumination_wavelengths_copy <- illumination_wavelengths_in_nm
    excitation_wavelengths_copy <- excitation_wavelengths_in_nm
    emission_wavelengths_copy <- emission_wavelengths_in_nm
    light_source_intensitys_copy <- light_source_intensitys
    fluorophores_copy <- fluorophores

    for(i in 1:number_of_channels){
      acquisition_mode[i] <- acquisition_mode_copy[channel_order[i]]
      illumination_type[i] <- illumination_type_copy[channel_order[i]]
      illumination_wavelengths_in_nm[i] <- illumination_wavelengths_copy[channel_order[i]]
      excitation_wavelengths_in_nm[i] <- excitation_wavelengths_copy[channel_order[i]]
      emission_wavelengths_in_nm[i] <- emission_wavelengths_copy[channel_order[i]]
      light_source_intensitys[i] <- light_source_intensitys_copy[channel_order[i]]
      fluorophores[i] <- fluorophores_copy[channel_order[i]]
    }
  }

  # Finding the color of each channel and the corresponding chan number ----

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
    "fluorophore_1" = fluorophores[1],
    "fluorophore_2" = fluorophores[2],
    "fluorophore_3" = fluorophores[3],
    "scene_name" = scene_name,
    "scene_center_position_x" = scene_center_position_x,
    "scene_center_position_y" = scene_center_position_y,
    "contour_size_x" = contour_size_x,
    "contour_size_y" = contour_size_y,
    "acquisition_mode_1" = acquisition_mode[1],
    "acquisition_mode_2" = acquisition_mode[2],
    "acquisition_mode_3" = acquisition_mode[3],
    "illumination_type_1" = illumination_type[1],
    "illumination_type_2" = illumination_type[2],
    "illumination_type_3" = illumination_type[3],
    "illumination_wavelength_1_in_nm" =  illumination_wavelengths_in_nm[1],
    "illumination_wavelength_2_in_nm" =  illumination_wavelengths_in_nm[2],
    "illumination_wavelength_3_in_nm" =  illumination_wavelengths_in_nm[3],
    "excitation_wavelength_1_in_nm" =  excitation_wavelengths_in_nm[1],
    "excitation_wavelength_2_in_nm" =  excitation_wavelengths_in_nm[2],
    "excitation_wavelength_3_in_nm" =  excitation_wavelengths_in_nm[3],
    "emission_wavelength_1_in_nm" =  emission_wavelengths_in_nm[1],
    "emission_wavelength_2_in_nm" =  emission_wavelengths_in_nm[2],
    "emission_wavelength_3_in_nm" =  emission_wavelengths_in_nm[3],
    "light_source_intensity_1" =  light_source_intensitys[1],
    "light_source_intensity_2" =  light_source_intensitys[2],
    "light_source_intensity_3" =  light_source_intensitys[3],
    "exposure_time_1" = exposure_times[1],
    "exposure_time_2" = exposure_times[2],
    "exposure_time_3" = exposure_times[3]
  )

  return(df_metadata)

}
