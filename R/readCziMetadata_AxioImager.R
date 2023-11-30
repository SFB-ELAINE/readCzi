#' Check for specific Axio-imager values in metadata
#' @param metadata A character (loaded metadata in a string)
#' @param number_of_channels A number
#' @keywords internal

readCziMetadata_AxioImager <- function(metadata = NULL,
                                       number_of_channels = NULL,
                                       number_of_tracks = NULL) {

  # Default values for missing arguments ###################################
  if(is.null(metadata) ||
     is.null(number_of_channels) ||
     is.null(number_of_tracks)){
    print(paste("Please call the function correctly.", sep=""))
    return()
  }

  # Get information of laser microscopes with apotome ######################
  metadata_XML <- xml2::read_xml(x = metadata)

  # Empty tibble with channel info
  df_channel_info <- tibble::tibble(track_id = rep(NA, number_of_channels),
                                    channel_id = rep(NA, number_of_channels),
                                    channel_name = rep(NA, number_of_channels))

  # Go through each track and write down channels
  if(number_of_tracks > 0){
    tracks_information <- xml2::xml_find_all(x = metadata_XML, xpath = ".//Tracks/Track")
    tracks_ids <- unique(xml2::xml_attr(x = tracks_information, attr = "Id"))
  }else{
    tracks_ids <- NA
  }

  if(number_of_tracks > 0){
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
  }else{
    # Find Channel information if there is no track
    channel_ids <- xml2::xml_attr(x = xml2::xml_find_all(
      x = metadata_XML, xpath = ".//Channel"),
      attr = "Id")

    channel_ids <- unique(channel_ids)

    if(length(channel_ids) > 1){
      print("Attention! We have more than one channel but no track.")
    }

    df_channel_info$track_id[1] <- NA

    df_channel_info$channel_id[1] <- channel_ids

  }


  # Go through all channel ids and add channel names
  #todo: anderen ort für id nehmen, wenn nicht dort vorhanden
  for(i in 1:length(df_channel_info$channel_id)){
    current_channel_id <- df_channel_info$channel_id[i]
    current_channel_information <- xml2::xml_find_all(
      x = metadata_XML,
      xpath = paste0(".//Dimensions/Channels/Channel[@Id='",
                     current_channel_id, "']"))
    current_channel_name <- xml2::xml_attr(x = current_channel_information,
                                           attr = "Name")

    # Use alternative node for finding channel name
    if(is.na(current_channel_name)){
      current_channel_information <- xml2::xml_find_all(
        x = metadata_XML,
        xpath = paste0(".//DisplaySetting/Channels/Channel[@Id='",
                       current_channel_id, "']"))
      current_channel_name <- xml2::xml_attr(x = current_channel_information,
                                             attr = "Name")
    }


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
  excitation_wavelengths <- rep(x = NA, number_of_channels)
  emission_wavelengths <- rep(x = NA, number_of_channels)
  illumination_wavelengths <- rep(x = NA, number_of_channels)
  light_source_intensities <- rep(x = NA, number_of_channels)
  exposure_times_in_ms <- rep(x = NA, number_of_channels)
  fluorophores <- rep(x = NA, number_of_channels)

  # Go through each channel and get information ############################
  for(i in 1:number_of_channels){

    # Filter for channel Id
    look_for <- paste(".//Dimensions/Channels/Channel[@Id='", df_channel_info$channel_id[i], "']", sep="")
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
    "number_of_tracks" = NA,
    "objective" = NA,
    "objective_magnification" = NA,
    "dim_x" = NA,
    "dim_y" = NA,
    "dim_z" = NA,
    "scaling_x_in_um" = NA,
    "scaling_y_in_um" = NA,
    "scaling_z_in_um" = NA,
    "channel_name_1" = channel_names[1],
    "channel_name_2" = channel_names[2],
    "channel_name_3" = channel_names[3],
    "track_id_channel_1" = track_ids[1],
    "track_id_channel_2" = track_ids[2],
    "track_id_channel_3" = track_ids[3],
    "contrast_method_channel_1" = contrast_method[1],
    "contrast_method_channel_2" = contrast_method[2],
    "contrast_method_channel_3" = contrast_method[3],
    "acquisition_mode" = acquisition_mode,
    "illumination_type" = illumination_type,
    "exposure_time_channel_1_in_ms" = exposure_times_in_ms[1],
    "exposure_time_channel_2_in_ms" = exposure_times_in_ms[2],
    "exposure_time_channel_3_in_ms" = exposure_times_in_ms[3],
    "fluorophore_channel_1" = fluorophores[1],
    "fluorophore_channel_2" = fluorophores[2],
    "fluorophore_channel_3" = fluorophores[3]
  )

  return(df_metadata)
}
