#' Check for specific values in metadata for all other microscope types
#' @param metadata A character (loaded metadata in a string)
#' @keywords internal

readCziMetadata_AxioImager <- function(metadata = metadata) {

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
    "acquisition_mode" = acquisition_mode,
    "illumination_type" = illumination_type
  )

  return(df_metadata)
}
