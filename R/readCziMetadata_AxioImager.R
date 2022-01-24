#' Check for specific values in metadata for all other microscope types
#' @param metadata A character (loaded metadata in a string)
#' @param number_of_channels A number
#' @keywords internal

readCziMetadata_AxioImager <- function(metadata = metadata,
                                       number_of_channels = number_of_channels) {


  metadata_XML <- xml2::read_xml(x = metadata)

  # Get information of laser microscopes with apotome ######################

  # Channel names
  look_for <- paste(".//Channel", sep="")
  channel_information <- xml2::xml_find_all(x = metadata_XML, xpath = look_for)
  channel_ids <- unique(xml2::xml_attr(x = channel_information, attr = "Id"))
  channel_names <- unique(xml2::xml_attr(x = channel_information, attr = "Name"))
  channel_names <- channel_names[!is.na(channel_names)]

  if(number_of_channels != length(channel_ids)){
    print("The number of channels does not correspond to the number of channel ids in the metadata.")
  }
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
  exposure_times <- rep(x = NA, number_of_channels)
  fluorophores <- rep(x = NA, number_of_channels)

  # Go through each channel and get information ############################
  for(i in 1:number_of_channels){

    # Filter for channel name
    look_for <- paste(".//Dimensions/Channels/Channel[@Id='", channel_ids[i], "']", sep="")
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

    # Exposure Time
    if(grepl(pattern = "ExposureTime", x = channel_information, ignore.case = TRUE)){
      exposure_times[i] <- as.numeric(unlist(channel_information[[1]]$ExposureTime))
    }

    # Fluorphore
    fluorophores[i] <- unlist(channel_information[[1]]$Fluor)

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
    "acquisition_mode" = acquisition_mode,
    "illumination_type" = illumination_type,
    "exposure_time_1" = exposure_times[1],
    "exposure_time_2" = exposure_times[2],
    "exposure_time_3" = exposure_times[3],
    "channel_name_1" = channel_names[1],
    "channel_name_2" = channel_names[2],
    "channel_name_3" = channel_names[3],
    "fluorophore_1" = fluorophores[1],
    "fluorophore_2" = fluorophores[2],
    "fluorophore_3" = fluorophores[3]
  )

  return(df_metadata)
}
