#' Check for apotome-specific values in metadata
#' @param metadata A character (loaded metadata in a string)
#' @param number_of_channels A number
#' @keywords internal

readCziMetadata_Apotome <- function(metadata = metadata,
                                    number_of_channels = number_of_channels) {

  metadata_XML <- xml2::read_xml(x = metadata)

  # Get information of laser microscopes with apotome ######################

  # Channel names
  look_for <- paste(".//Channel", sep="")
  channel_information <- xml2::xml_find_all(x = metadata_XML, xpath = look_for)
  channel_names <- unique(xml2::xml_attr(x = channel_information, attr = "Name"))

  if(number_of_channels != length(channel_names)){
    print("The number of channels does not correspond to the number of channel names in the metadata.")
  }

  # Empty vectors
  acquisition_mode <- rep(x = NA, number_of_channels)
  illumination_type <- rep(x = NA, number_of_channels)
  excitation_wavelengths <- rep(x = NA, number_of_channels)
  emission_wavelengths <- rep(x = NA, number_of_channels)
  illumination_wavelengths <- rep(x = NA, number_of_channels)
  light_source_intensitys <- rep(x = NA, number_of_channels)

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
      excitation_wavelengths[i] <- as.numeric(unlist(channel_information[[1]]$ExcitationWavelength))
    }
    if(grepl(pattern = "EmissionWavelength", x = channel_information, ignore.case = TRUE)){
      emission_wavelengths[i] <- as.numeric(unlist(channel_information[[1]]$EmissionWavelength))
    }

    # Illumination wavelengths and light source intensity
    if(grepl(pattern = "IlluminationWavelength", x = channel_information, ignore.case = TRUE)){
      illumination_wavelengths[i] <- as.numeric(unlist(channel_information[[1]]$IlluminationWavelength$SinglePeak))
    }
    if(grepl(pattern = "LightSourceSettings", x = channel_information, ignore.case = TRUE)){
      light_source_intensitys[i] <- unlist(channel_information[[1]]$LightSourcesSettings$LightSourceSettings$Intensity)
      light_source_intensitys[i] <- as.numeric(gsub(pattern = "\\%", replacement = "", x = light_source_intensitys[i]))
    }

  }

  # Get scene information ##################################################

  # Filter for scene information
  if(grepl(pattern = "Scenes", x = metadata_XML, ignore.case = TRUE)){
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

  # # Get information of Apotome (LED widefield) #############################
  #
  # # Get information depending on the number of channels
  #
  # if(number_of_channels == 3){
  #
  #   # LED widefield
  #   excitation_wavelengths <- gsub(
  #     pattern =  paste(".+<ExcitationWavelength>(.+)</ExcitationWavelength>.+",
  #                      ".+<ExcitationWavelength>(.+)</ExcitationWavelength>.+",
  #                      ".+<ExcitationWavelength>(.+)</ExcitationWavelength>.+",
  #                      sep=""),
  #     replacement = "\\1,\\2,\\3",
  #     x = metadata)
  #
  #   emission_wavelengths <- gsub(
  #     pattern =  paste(".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
  #                      ".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
  #                      ".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
  #                      sep=""),
  #     replacement = "\\1,\\2,\\3",
  #     x = metadata)
  #
  #   illumination_wavelengths <- gsub(
  #     pattern =  paste(
  #       ".+<IlluminationWavelength>.+<SinglePeak>(.+)</SinglePeak>.+</IlluminationWavelength>.+",
  #       ".+<IlluminationWavelength>.+<SinglePeak>(.+)</SinglePeak>.+</IlluminationWavelength>.+",
  #       ".+<IlluminationWavelength>.+<SinglePeak>(.+)</SinglePeak>.+</IlluminationWavelength>.+",
  #       sep=""),
  #     replacement = "\\1,\\2,\\3",
  #     x = metadata)
  #
  #
  #   light_source_intensitys <-  gsub(
  #     pattern = paste(".+<Intensity>(.+)</Intensity>.+",
  #                     ".+<Intensity>(.+)</Intensity>.+",
  #                     ".+<Intensity>(.+)</Intensity>.+",
  #                     sep=""),
  #     replacement = "\\1, \\2, \\3", x = metadata)
  #
  # }else if(number_of_channels == 2){
  #
  #   # LED widefield
  #   excitation_wavelengths <- gsub(
  #     pattern =  paste(".+<ExcitationWavelength>(.+)</ExcitationWavelength>.+",
  #                      ".+<ExcitationWavelength>(.+)</ExcitationWavelength>.+",
  #                      sep=""),
  #     replacement = "\\1,\\2",
  #     x = metadata)
  #
  #   emission_wavelengths <- gsub(
  #     pattern =  paste(".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
  #                      ".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
  #                      sep=""),
  #     replacement = "\\1,\\2",
  #     x = metadata)
  #
  #   illumination_wavelengths <- gsub(
  #     pattern =  paste(
  #       ".+<IlluminationWavelength>.+<SinglePeak>(.+)</SinglePeak>.+</IlluminationWavelength>.+",
  #       ".+<IlluminationWavelength>.+<SinglePeak>(.+)</SinglePeak>.+</IlluminationWavelength>.+",
  #       sep=""),
  #     replacement = "\\1,\\2",
  #     x = metadata)
  #
  #
  #   light_source_intensitys <-  gsub(
  #     pattern = paste(".+<Intensity>(.+)</Intensity>.+",
  #                     ".+<Intensity>(.+)</Intensity>.+",
  #                     sep=""),
  #     replacement = "\\1, \\2", x = metadata)
  #
  # }else if(number_of_channels == 1){
  #   # LED widefield
  #   excitation_wavelengths <- gsub(
  #     pattern =  paste(".+<ExcitationWavelength>(.+)</ExcitationWavelength>.+",
  #                      sep=""),
  #     replacement = "\\1",
  #     x = metadata)
  #
  #   emission_wavelengths <- gsub(
  #     pattern =  paste(".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
  #                      sep=""),
  #     replacement = "\\1",
  #     x = metadata)
  #
  #   illumination_wavelengths <- gsub(
  #     pattern =  paste(
  #       ".+<IlluminationWavelength>.+<SinglePeak>(.+)</SinglePeak>.+</IlluminationWavelength>.+",
  #       sep=""),
  #     replacement = "\\1",
  #     x = metadata)
  #
  #
  #   light_source_intensitys <-  gsub(
  #     pattern = paste(".+<Intensity>(.+)</Intensity>.+",
  #                     sep=""),
  #     replacement = "\\1", x = metadata)
  #
  # }

  # Sorting the channels information regarding wavelength ------------------
  channel_order <- order(illumination_wavelengths)

  if(!all.equal(c(1,2,3), channel_order)){

    acquisition_mode_copy <- acquisition_mode
    illumination_type_copy <- illumination_type
    illumination_wavelengths_copy <- illumination_wavelengths
    excitation_wavelengths_copy <- excitation_wavelengths
    emission_wavelengths_copy <- emission_wavelengths
    light_source_intensitys_copy <- light_source_intensitys

    for(i in 1:number_of_channels){
      acquisition_mode[i] <- acquisition_mode_copy[channel_order[i]]
      illumination_type[i] <- illumination_type_copy[channel_order[i]]
      illumination_wavelengths[i] <- illumination_wavelengths_copy[channel_order[i]]
      excitation_wavelengths[i] <- excitation_wavelengths_copy[channel_order[i]]
      emission_wavelengths[i] <- emission_wavelengths_copy[channel_order[i]]
      light_source_intensitys[i] <- light_source_intensitys_copy[channel_order[i]]
    }
  }

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
    "scaling_x" = NA,
    "scaling_y" = NA,
    "scaling_z" = NA,
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
    "illumination_wavelength_1" =  illumination_wavelengths[1],
    "illumination_wavelength_2" =  illumination_wavelengths[2],
    "illumination_wavelength_3" =  illumination_wavelengths[3],
    "excitation_wavelength_1" =  excitation_wavelengths[1],
    "excitation_wavelength_2" =  excitation_wavelengths[2],
    "excitation_wavelength_3" =  excitation_wavelengths[3],
    "emission_wavelength_1" =  emission_wavelengths[1],
    "emission_wavelength_2" =  emission_wavelengths[2],
    "emission_wavelength_3" =  emission_wavelengths[3],
    "light_source_intensity_1" =  light_source_intensitys[1],
    "light_source_intensity_2" =  light_source_intensitys[2],
    "light_source_intensity_3" =  light_source_intensitys[3]
  )

  return(df_metadata)

}
