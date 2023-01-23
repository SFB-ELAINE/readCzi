#' Check for specific Axio-imager values in metadata
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
  channel_ids <- channel_ids[!is.na(channel_ids)]
  channel_names <- unique(xml2::xml_attr(x = channel_information, attr = "Name"))
  channel_names <- channel_names[!is.na(channel_names)]

  if(number_of_channels != length(channel_ids)){
    print("The number of channels does not correspond to the number of channel ids in the metadata.")
  }
  if(number_of_channels != length(channel_names)){
    print("The number of channels does not correspond to the number of channel names in the metadata.")
  }

  # Empty vectors
  contrast_method <- rep(x = NA, number_of_channels)
  acquisition_mode <- rep(x = NA, number_of_channels)
  illumination_type <- rep(x = NA, number_of_channels)
  excitation_wavelengths <- rep(x = NA, number_of_channels)
  emission_wavelengths <- rep(x = NA, number_of_channels)
  illumination_wavelengths <- rep(x = NA, number_of_channels)
  light_source_intensities <- rep(x = NA, number_of_channels)
  exposure_times_in_ms <- rep(x = NA, number_of_channels)
  fluorophores <- rep(x = NA, number_of_channels)

  # Go through each channel and get information ############################
  for(i in 1:number_of_channels){

    # Filter for channel Id
    look_for <- paste(".//Dimensions/Channels/Channel[@Id='", channel_ids[i], "']", sep="")
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
    "contrast_method_1" = contrast_method[1],
    "contrast_method_2" = contrast_method[2],
    "contrast_method_3" = contrast_method[3],
    "acquisition_mode" = acquisition_mode,
    "illumination_type" = illumination_type,
    "exposure_time_1_in_ms" = exposure_times_in_ms[1],
    "exposure_time_2_in_ms" = exposure_times_in_ms[2],
    "exposure_time_3_in_ms" = exposure_times_in_ms[3],
    "channel_name_1" = channel_names[1],
    "channel_name_2" = channel_names[2],
    "channel_name_3" = channel_names[3],
    "fluorophore_1" = fluorophores[1],
    "fluorophore_2" = fluorophores[2],
    "fluorophore_3" = fluorophores[3]
  )

  return(df_metadata)
}
