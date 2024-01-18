#' Imports a czi file and saves it as a tif
#'
#' `convertCziToTif()` saves a multidimensional array (image) as tifs
#' and stacks/edits the images if required.
#'
#' @param input_file Path to a CZI file as a string.
#' @param output_dir Path to a directory where the results are to be saved
#' as a string.
#' @param convert_all_slices A Boolean stating whether all z-stack layers
#' of a CZI files are to be converted to tifs. (FALSE is the default value.)
#' @param zstack_projection A Boolean stating whether the a projection of a z-stack
#' image is to be calculated. (TRUE is the default value.)
#' @param projection_method A character representing the method for calculating
#' the image projection: mean or max (default value).
#' @param higher_contrast_layers A Boolean stating wheterh the contrast
#' of the z-stack layers should be enhanced using `EBImage::clahe()`.
#' (FALSE is the default value.)
#' @param higher_contrast_projection A Boolean stating whether the contrast
#' of the z-stack projection or the original image if it is not a z stack
#' shall be enhanced using `EBImage::clahe()`. (TRUE is the default value.)
#' @param normalize_projection A Boolean statin whether the z-stack projection
#' shall be normalized. (TRUE is the default value.)
#' @param change_color_layers A character stating the color layers to be
#' swuitched (either "none" or, e.g., "green<->red").
#' @param add_scale_bar A Boolean stating whether to add scale bar to all
#' images that are saved. (TRUE is the default value.)
#'
#' @returns Nothing but saves the converted czi file as tif file(s).
#' @export convertCziToTif

convertCziToTif <- function(input_file = NULL,
                            output_dir = "output",
                            convert_all_slices = FALSE,
                            zstack_projection = TRUE,
                            projection_method = "max",
                            higher_contrast_layers = FALSE,
                            higher_contrast_projection = TRUE,
                            normalize_projection = TRUE,
                            change_color_layers = "none",
                            add_scale_bar = TRUE) {

  if(is.null(input_file)){
    print("Please call function with input file.")
    return()
  }

  if(!("EBImage" %in% utils::installed.packages())){
    print("Installing EBImage.")
    BiocManager::install("EBImage")
  }

  # 1. Load image ----------------------------------------------------------

  # Create output directory
  directory_of_file <- dirname(input_file)
  file_name <- basename(input_file)
  image_name_wo_czi <- gsub("\\.czi", "", file_name)
  output_dir <- file.path(directory_of_file, output_dir)
  dir.create(output_dir, showWarnings = FALSE)

  # Load image and convert it to Image class
  # Dimensions of the image: 1: row, 2: col, 3: channels (r,g,b), 4: z-layer,
  image_data <- readCzi::readCzi(input_file = input_file)
  dim_z <- dim(image_data)[4]

  # Drop z-layer of multidimensional array if dim_z == 1
  if(dim_z == 1){
    # Drop all arrays with dim=1
    image_data <- drop(image_data)
  }

  # 2. Switch color layers -------------------------------------------------

  if(change_color_layers != "none"){

    change_color_layers <- tolower(change_color_layers)
    change_color_layers <- gsub(pattern = " ", replacement = "", x = change_color_layers)
    image_data_copy <- image_data

    if(change_color_layers == "red<->green" || change_color_layers == "green<->red"){
      if(dim_z > 1){
        image_data[,,1,] <- image_data[,,2,]
        image_data[,,2,] <- image_data_copy[,,1,]
      }else{
        image_data[,,1] <- image_data[,,2]
        image_data[,,2] <- image_data_copy[,,1]
      }
    }else if(change_color_layers == "red<->blue" || change_color_layers == "blue<->red"){
      if(dim_z > 1){
        image_data[,,1,] <- image_data[,,3,]
        image_data[,,3,] <- image_data_copy[,,1,]
      }else{
        image_data[,,1] <- image_data[,,3]
        image_data[,,3] <- image_data_copy[,,1]
      }
    }else if(change_color_layers == "green<->blue" || change_color_layers == "blue<->green"){
      if(dim_z > 1){
        image_data[,,2,] <- image_data[,,3,]
        image_data[,,3,] <- image_data_copy[,,2,]
      }else{
        image_data[,,2] <- image_data[,,3]
        image_data[,,3] <- image_data_copy[,,2]
      }
    }
    rm(image_data_copy)
  }


  # Convert image array to EBImage image class
  Image_Data <- EBImage::Image(data = image_data, colormode = "Color")

  # Save length per pixel in x-direction
  df_metadata <- readCzi::readCziMetadata(input_file = input_file, save_metadata = FALSE)
  length_per_pixel_x_in_um <- df_metadata$scaling_x_in_um[1]

  # Check that pixels in x and y direction have the same lengths
  length_per_pixel_y_in_um <- df_metadata$scaling_y_in_um[1]

  if(length_per_pixel_x_in_um != length_per_pixel_y_in_um){
    print("Dimension in x- and y-directions are different! ERROR!")
  }

  # 3. Save all z-stack layers ---------------------------------------------

  if(dim_z > 1 && convert_all_slices){
    tif_file_names <- rep(image_name_wo_czi, dim(image_data)[4])
    tif_file_names <- file.path(output_dir,
                                paste0(tif_file_names, "_z",
                                      1:dim(image_data)[4], ".tif"))

    Image_Data_copy <- Image_Data
    if(add_scale_bar){
      Image_Data <- addScaleBar(image = Image_Data,
                                length_per_pixel_in_um = length_per_pixel_x_in_um)
    }

    EBImage::writeImage(x = Image_Data, files = tif_file_names,
                        type = "tiff", bits.per.sample = 8)

    Image_Data <- Image_Data_copy
    rm(Image_Data_copy)
  }

  # 4. Save z-stack projection or original image (if dim_z=1) --------------
  if(dim_z > 1 && zstack_projection){

    Image_Stack <- zstackProjection(image_data = Image_Data,
                                    projection_method = projection_method)

    projection_file_name <- file.path(output_dir,
                                 paste0(image_name_wo_czi, "_projection.tif"))

    Image_Stack_copy <- Image_Stack
    if(add_scale_bar){
      Image_Stack <- addScaleBar(image = Image_Stack,
                                length_per_pixel_in_um = length_per_pixel_x_in_um)
    }

    EBImage::writeImage(x = Image_Stack, files = projection_file_name,
                        type = "tiff", bits.per.sample = 8)

    Image_Stack <- Image_Stack_copy
    rm(Image_Stack_copy)
  }else{
    # Not a z-stack image (dim_z==1)

    output_file_name <- file.path(output_dir,
                                  paste0(image_name_wo_czi, ".tif"))

    Image_Data_copy <- Image_Data
    if(add_scale_bar){
      Image_Data <- addScaleBar(image = Image_Data,
                                length_per_pixel_in_um = length_per_pixel_x_in_um)
    }
    EBImage::writeImage(x = Image_Data, files = output_file_name,
                        type = "tiff", bits.per.sample = 8)

    Image_Data <- Image_Data_copy
    rm(Image_Data_copy)
  }


  # 5. Enhance contrast of z-stack layers ----------------------------------
  if(dim_z > 1 && higher_contrast_layers){

    Image_Data_histogram_equalization <- EBImage::combine(
      lapply(X = EBImage::getFrames(y = Image_Data, type = "render"),
             FUN = function(frame){EBImage::clahe(x = frame, nx = 4)}))

    tif_file_names <- rep(image_name_wo_czi, dim(image_data)[4])
    tif_file_names <- file.path(output_dir,
                                paste0(tif_file_names, "_z",
                                       1:dim(image_data)[4],
                                       "_histogram_equalized.tif"))

    Image_Data_copy <- Image_Data
    if(add_scale_bar){
      Image_Data <- addScaleBar(image = Image_Data,
                                length_per_pixel_in_um = length_per_pixel_x_in_um)
    }
    EBImage::writeImage(x = Image_Data, files = tif_file_names,
                        type = "tiff", bits.per.sample = 8)

    Image_Data <- Image_Data_copy
    rm(Image_Data_copy)
  }

  # 6. Enhance contrast of z-stack projection ------------------------------
  if(zstack_projection && higher_contrast_projection){

    if(dim_z > 1){
      # Use Contrast Limited Adaptive Histogram Equalization
      Image_Stack_histogram_equalization <- EBImage::clahe(x = Image_Stack, nx = 4)
      output_file_name <- file.path(output_dir,
                                    paste0(image_name_wo_czi,
                                           "_projection_histogram_equalized.tif"))
      projection <- TRUE
    }else{
      # Use Contrast Limited Adaptive Histogram Equalization
      Image_Stack_histogram_equalization <- EBImage::clahe(x = Image_Data, nx = 4)
      output_file_name <- file.path(output_dir,
                                    paste0(image_name_wo_czi,
                                           "_histogram_equalized.tif"))
      projection <- FALSE
    }

    Image_Stack_histogram_equalization_copy <- Image_Stack_histogram_equalization
    if(add_scale_bar){
      Image_Stack_histogram_equalization <- addScaleBar(
        image = Image_Stack_histogram_equalization,
        length_per_pixel_in_um = length_per_pixel_x_in_um)
    }
    EBImage::writeImage(x = Image_Stack_histogram_equalization,
                        files = output_file_name, type = "tiff",
                        bits.per.sample = 8)

    Image_Stack_histogram_equalization <- Image_Stack_histogram_equalization_copy
    rm(Image_Stack_histogram_equalization_copy)
  }

  # 7. Normalize contrast of z-stack projection ----------------------------
  if(zstack_projection && normalize_projection){

    if(higher_contrast_projection){
      Image_Stack_normalized <- Image_Stack_histogram_equalization
      if(projection){
        output_file_name <- file.path(output_dir,
                                      paste0(image_name_wo_czi,
                                             "_projection_histogram_equalized_normalized.tif"))
      }else{
        output_file_name <- file.path(output_dir,
                                      paste0(image_name_wo_czi,
                                             "_histogram_equalized_normalized.tif"))
      }

    }else{
      Image_Stack_normalized <- Image_Stack
      if(projection){
        output_file_name <- file.path(output_dir,
                                      paste0(image_name_wo_czi,
                                             "_projection_normalized.tif"))
      }else{
        output_file_name <- file.path(output_dir,
                                      paste0(image_name_wo_czi,
                                             "_normalized.tif"))
      }

    }

    Image_Stack_normalized <- EBImage::normalize(Image_Stack_normalized)

    Image_Stack_normalized_copy <- Image_Stack_normalized

    if(add_scale_bar){
      Image_Stack_normalized <- addScaleBar(image = Image_Stack_normalized,
                                            length_per_pixel_in_um = length_per_pixel_x_in_um)
    }

    EBImage::writeImage(x = Image_Stack_normalized, files = output_file_name,
                        type = "tiff", bits.per.sample = 8)

    Image_Stack_normalized <- Image_Stack_normalized_copy
    rm(Image_Stack_normalized_copy)
  }

}
