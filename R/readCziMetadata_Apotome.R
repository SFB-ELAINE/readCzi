#' Check for apotome-specific values in metadata
#' @param metadata A character (loaded metadata in a string)
#' @param number_of_channels A number
#' @keywords internal

readCziMetadata_Apotome <- function(metadata = metadata,
                                    number_of_channels = number_of_channels) {

  # Acquisition Mode
  if(grepl(pattern = "<AcquisitionMode>", x = metadata, ignore.case = TRUE)){
    acquisition_mode <- gsub(pattern = ".+<AcquisitionMode>(.+)</AcquisitionMode>.+",
                             replacement = "\\1", x = metadata)
  }else{
    acquisition_mode <- NA
  }

  # Illumination Type
  if(grepl(pattern = "<IlluminationType>", x = metadata, ignore.case = TRUE)){
    illumination_type <- gsub(pattern = ".+<IlluminationType>(.+)</IlluminationType>.+",
                              replacement = "\\1", x = metadata)
  }else{
    illumination_type <- NA
  }

  # Get scene information ##################################################
  if(grepl(pattern = "<Scenes>", x = metadata, ignore.case = TRUE)){
    scene_name <- gsub(pattern = ".+<Scene Index=\"0\" Name=\"(.{1,10})\">.+",
                              replacement = "\\1", x = metadata)

    if(grepl(pattern = "<Scene Index=\"1\"", x = metadata, ignore.case = TRUE)){
      print("Please split the scenes before using this package.")
      return()
    }
  }else{
    scene_name <- NA
  }

  if(grepl(pattern = "<Scenes>", x = metadata, ignore.case = TRUE)){
    scene_center_position <- gsub(
      pattern = ".+<Scenes>.+<CenterPosition>(.+)</CenterPosition>.+</Scenes>.+",
                       replacement = "\\1", x = metadata)

    scene_center_position_x <- as.numeric(strsplit(x = scene_center_position, split = ",")[[1]][1])
    scene_center_position_y <- as.numeric(strsplit(x = scene_center_position, split = ",")[[1]][2])
  }else{
    scene_center_position_x <- NA
    scene_center_position_y <- NA
  }

  if(grepl(pattern = "<Scenes>", x = metadata, ignore.case = TRUE)){
    contour_size <- gsub(
      pattern = ".+<Scenes>.+<ContourSize>(.+)</ContourSize>.+</Scenes>.+",
      replacement = "\\1", x = metadata)

    contour_size_x <- as.numeric(strsplit(x = contour_size, split = ",")[[1]][1])
    contour_size_y <- as.numeric(strsplit(x = contour_size, split = ",")[[1]][2])
  }else{
    contour_size_x <- NA
    contour_size_y <- NA
  }


  # Get information of Apotome (LED widefield) #############################

  # Get information depending on the number of channels

  if(number_of_channels == 3){

    # LED widefield
    excitation_wavelengths <- gsub(
      pattern =  paste(".+<ExcitationWavelength>(.+)</ExcitationWavelength>.+",
                       ".+<ExcitationWavelength>(.+)</ExcitationWavelength>.+",
                       ".+<ExcitationWavelength>(.+)</ExcitationWavelength>.+",
                       sep=""),
      replacement = "\\1,\\2,\\3",
      x = metadata)

    emission_wavelengths <- gsub(
      pattern =  paste(".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
                       ".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
                       ".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
                       sep=""),
      replacement = "\\1,\\2,\\3",
      x = metadata)

    illumination_wavelengths <- gsub(
      pattern =  paste(
        ".+<IlluminationWavelength>.+<SinglePeak>(.+)</SinglePeak>.+</IlluminationWavelength>.+",
        ".+<IlluminationWavelength>.+<SinglePeak>(.+)</SinglePeak>.+</IlluminationWavelength>.+",
        ".+<IlluminationWavelength>.+<SinglePeak>(.+)</SinglePeak>.+</IlluminationWavelength>.+",
        sep=""),
      replacement = "\\1,\\2,\\3",
      x = metadata)


    light_source_intensitys <-  gsub(
      pattern = paste(".+<Intensity>(.+)</Intensity>.+",
                      ".+<Intensity>(.+)</Intensity>.+",
                      ".+<Intensity>(.+)</Intensity>.+",
                      sep=""),
      replacement = "\\1, \\2, \\3", x = metadata)

  }else if(number_of_channels == 2){

    # LED widefield
    excitation_wavelengths <- gsub(
      pattern =  paste(".+<ExcitationWavelength>(.+)</ExcitationWavelength>.+",
                       ".+<ExcitationWavelength>(.+)</ExcitationWavelength>.+",
                       sep=""),
      replacement = "\\1,\\2",
      x = metadata)

    emission_wavelengths <- gsub(
      pattern =  paste(".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
                       ".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
                       sep=""),
      replacement = "\\1,\\2",
      x = metadata)

    illumination_wavelengths <- gsub(
      pattern =  paste(
        ".+<IlluminationWavelength>.+<SinglePeak>(.+)</SinglePeak>.+</IlluminationWavelength>.+",
        ".+<IlluminationWavelength>.+<SinglePeak>(.+)</SinglePeak>.+</IlluminationWavelength>.+",
        sep=""),
      replacement = "\\1,\\2",
      x = metadata)


    light_source_intensitys <-  gsub(
      pattern = paste(".+<Intensity>(.+)</Intensity>.+",
                      ".+<Intensity>(.+)</Intensity>.+",
                      sep=""),
      replacement = "\\1, \\2", x = metadata)

  }else if(number_of_channels == 1){
    # LED widefield
    excitation_wavelengths <- gsub(
      pattern =  paste(".+<ExcitationWavelength>(.+)</ExcitationWavelength>.+",
                       sep=""),
      replacement = "\\1",
      x = metadata)

    emission_wavelengths <- gsub(
      pattern =  paste(".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
                       sep=""),
      replacement = "\\1",
      x = metadata)

    illumination_wavelengths <- gsub(
      pattern =  paste(
        ".+<IlluminationWavelength>.+<SinglePeak>(.+)</SinglePeak>.+</IlluminationWavelength>.+",
        sep=""),
      replacement = "\\1",
      x = metadata)


    light_source_intensitys <-  gsub(
      pattern = paste(".+<Intensity>(.+)</Intensity>.+",
                      sep=""),
      replacement = "\\1", x = metadata)

  }

  excitation_wavelengths <- as.numeric(unlist(strsplit(excitation_wavelengths, split = ",")))
  emission_wavelengths <- as.numeric(unlist(strsplit(x = emission_wavelengths, split = ",")))
  illumination_wavelengths <- as.numeric(unlist(strsplit(x = illumination_wavelengths, split = ",")))
  light_source_intensitys <- unlist(strsplit(x = light_source_intensitys, split = ","))
  light_source_intensitys <- gsub(pattern = "^[[:space:]]+",
                                  x = light_source_intensitys,
                                  replacement = "")
  light_source_intensitys <- gsub(pattern = "[[:space:]]+$",
                                  x = light_source_intensitys,
                                  replacement = "")

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
    "scene_name" = scene_name,
    "scene_center_position_x" = scene_center_position_x,
    "scene_center_position_y" = scene_center_position_y,
    "contour_size_x" = contour_size_x,
    "contour_size_y" = contour_size_y,
    "acquisition_mode" = acquisition_mode,
    "illumination_type" = illumination_type,
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
