#' @title convertCziToTif
#' @description Imports czi file and save them as tif
#' @details This functions saves a multidimensional array (image) as tifs
#' and stacks/edits the images if required.
#' @aliases convertczitotif convertCzitotif convertcziTotif convertczitoTif
#' convertCziTotif convertcziToTif
#' @author Kai Budde-Sagert
#' @export convertCziToTif
#' @param input_file A character (path to czi file to be converted)
#' @param output_dir A character (name of output directory)
#' @param convert_all_slices A logical (T/F for converting and saving all
#' z-slices)
#' @param stack_image A logical (T/F for calculating a z-stack)
#' @param stack_method A character (method for stacking the images: average,
#' maxprojection)
#' @param higher_contrast_slices A logical (T/F for enhancing the contrast
#' of the slices)
#' @param higher_contrast_stack A logical (T/F for enhancing the contrast
#' of the z-stack or original image if it is not a z stack)
#' @param normalize_stack A logical (T/F for normalizing intensities of stack)
#' @param change_layers A character (either "none" or, e.g., "green<->red")
#' @param add_scale_bar A logic (add scale bar to all images that are saved
#' if true)
#' @returns Nothing but saves the converted czi file as tif file(s).

convertCziToTif <- function(input_file = NULL,
                            output_dir = "output",
                            convert_all_slices = FALSE,
                            stack_image = TRUE,
                            stack_method = "average",
                            higher_contrast_slices = FALSE,
                            higher_contrast_stack = TRUE,
                            normalize_stack = TRUE,
                            change_layers = "none",
                            add_scale_bar = TRUE) {
  if(is.null(input_file)){
    print("Please call function with input file.")
    return()
  }

  if(!("EBImage" %in% utils::installed.packages())){
    print("Installing EBImage.")
    BiocManager::install("EBImage")
  }

  # Create output directory ------------------------------------------------
  directory_of_file <- dirname(input_file)
  file_name <- basename(input_file)
  image_name_wo_czi <- gsub("\\.czi", "", file_name)
  output_dir <- file.path(directory_of_file, output_dir)
  dir.create(output_dir, showWarnings = FALSE)

  # Load image and convert it to Image class -------------------------------
  # Dimensions of the image: 1: row, 2: col, 3: channels (r,g,b), 4: z-layer,
  image_data <- readCzi::readCzi(input_file = input_file)
  dim_z <- dim(image_data)[4]

  # Drop z-layer of multidimensional array if dim_z == 1
  if(dim_z == 1){
    # Drop all arrays with dim=1
    image_data <- drop(image_data)
  }

  # Switch layers

  if(change_layers != "none"){

    change_layers <- tolower(change_layers)
    change_layers <- gsub(pattern = " ", replacement = "", x = change_layers)
    image_data_copy <- image_data

    if(change_layers == "red<->green" || change_layers == "green<->red"){
      if(dim_z > 1){
        image_data[,,1,] <- image_data[,,2,]
        image_data[,,2,] <- image_data_copy[,,1,]
      }else{
        image_data[,,1] <- image_data[,,2]
        image_data[,,2] <- image_data_copy[,,1]
      }
    }else if(change_layers == "red<->blue" || change_layers == "blue<->red"){
      if(dim_z > 1){
        image_data[,,1,] <- image_data[,,3,]
        image_data[,,3,] <- image_data_copy[,,1,]
      }else{
        image_data[,,1] <- image_data[,,3]
        image_data[,,3] <- image_data_copy[,,1]
      }
    }else if(change_layers == "green<->blue" || change_layers == "blue<->green"){
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


  # Convert to EBImage
  Image_Data <- EBImage::Image(data = image_data, colormode = "Color")

  # Save length per pixel in x-direction
  df_metadata <- readCzi::readCziMetadata(input_file = input_file, save_metadata = FALSE)
  length_per_pixel_x_in_um <- df_metadata$scaling_x_in_um[1]

  # Check that pixels in x and y direction have the same lengths
  length_per_pixel_y_in_um <- df_metadata$scaling_y_in_um[1]

  if(length_per_pixel_x_in_um != length_per_pixel_y_in_um){
    print("Dimension in x- and y-directions are different! ERROR!")
  }

  # Save all slices --------------------------------------------------------

  if(dim_z > 1 && convert_all_slices){
    tif_file_names <- rep(image_name_wo_czi, dim(image_data)[4])
    tif_file_names <- paste(output_dir, "/", tif_file_names, "_z", 1:dim(image_data)[4], ".tif", sep="")

    Image_Data_copy <- Image_Data
    if(add_scale_bar){
      Image_Data <- addScaleBar(image = Image_Data,
                                length_per_pixel_in_um = length_per_pixel_x_in_um)
    }

    EBImage::writeImage(x = Image_Data, files = tif_file_names, type = "tiff", bits.per.sample = 8)
    Image_Data <- Image_Data_copy
    rm(Image_Data_copy)
  }

  # Save z-stack or original image (with z=1) ------------------------------
  if(dim_z > 1 && stack_image){

    Image_Stack <- stackLayers(image_data = Image_Data,
                               stack_method = stack_method)

    stack_file_name <- paste(output_dir, "/", image_name_wo_czi, "_zstack.tif", sep="")

    Image_Stack_copy <- Image_Stack
    if(add_scale_bar){
      Image_Stack <- addScaleBar(image = Image_Stack,
                                length_per_pixel_in_um = length_per_pixel_x_in_um)
    }
    EBImage::writeImage(x = Image_Stack, files = stack_file_name, type = "tiff", bits.per.sample = 8)
    Image_Stack <- Image_Stack_copy
    rm(Image_Stack_copy)
  }else{
    # Not a z-stack image (dim_z==1)

    output_file_name <- file.path(output_dir, paste0(image_name_wo_czi, ".tif"))

    Image_Data_copy <- Image_Data
    if(add_scale_bar){
      Image_Data <- addScaleBar(image = Image_Data,
                                length_per_pixel_in_um = length_per_pixel_x_in_um)
    }
    EBImage::writeImage(x = Image_Data, files = output_file_name, type = "tiff", bits.per.sample = 8)

    Image_Data <- Image_Data_copy
    rm(Image_Data_copy)
  }


  # Enhance contrast of z-slices ------------------------------------------
  if(dim_z > 1 && higher_contrast_slices){

    Image_Data_histogram_equalization <- EBImage::combine(lapply(X = EBImage::getFrames(y = Image_Data, type = "render"),
                                                                 FUN = function(frame){EBImage::clahe(x = frame, nx = 4)}))

    tif_file_names <- rep(image_name_wo_czi, dim(image_data)[4])
    tif_file_names <- paste(output_dir, "/", tif_file_names, "_z", 1:dim(image_data)[4], "_histogram_equalized.tif", sep="")

    Image_Data_copy <- Image_Data
    if(add_scale_bar){
      Image_Data <- addScaleBar(image = Image_Data,
                                length_per_pixel_in_um = length_per_pixel_x_in_um)
    }
    EBImage::writeImage(x = Image_Data, files = tif_file_names, type = "tiff", bits.per.sample = 8)

    Image_Data <- Image_Data_copy
    rm(Image_Data_copy)
  }

  # Enhance contrast of z-stack ------------------------------------------
  if(higher_contrast_stack){

    if(dim_z > 1){
      # Use Contrast Limited Adaptive Histogram Equalization
      Image_Stack_histogram_equalization <- EBImage::clahe(x = Image_Stack, nx = 4)
      output_file_name <- paste(output_dir, "/", image_name_wo_czi, "_zstack_histogram_equalized.tif", sep="")
    }else{
      # Use Contrast Limited Adaptive Histogram Equalization
      Image_Stack_histogram_equalization <- EBImage::clahe(x = Image_Data, nx = 4)
      output_file_name <- paste(output_dir, "/", image_name_wo_czi, "_histogram_equalized.tif", sep="")
    }

    Image_Stack_histogram_equalization_copy <- Image_Stack_histogram_equalization
    if(add_scale_bar){
      Image_Stack_histogram_equalization <- addScaleBar(image = Image_Stack_histogram_equalization,
                                length_per_pixel_in_um = length_per_pixel_x_in_um)
    }
    EBImage::writeImage(x = Image_Stack_histogram_equalization, files = output_file_name, type = "tiff", bits.per.sample = 8)

    Image_Stack_histogram_equalization <- Image_Stack_histogram_equalization_copy
    rm(Image_Stack_histogram_equalization_copy)
  }

  # Normalize contrast of enhanced z-stack ---------------------------------
  if(normalize_stack){

    Image_Stack_normalized <- Image_Stack_histogram_equalization

    if(dim_z > 1){

      # for(i in 1:3){
      #   if(max(Image_Stack[,,i]) > 0){
      #     Image_Stack_normalized[,,i] <- Image_Stack_normalized[,,i]/max(Image_Stack_normalized[,,i])
      #   }
      # }

      disregarded_layers <- c()
      for(i in 1:3){
        if(max(Image_Stack[,,i]) == 0){
          disregarded_layers <- c(disregarded_layers, i)
        }
      }

      Image_Stack_normalized <- normalizeIntensity(image = Image_Stack_normalized,
                                                   disregarded_layers = disregarded_layers)

      output_file_name <- paste(output_dir, "/", image_name_wo_czi, "_zstack_histogram_equalized_normalized.tif", sep="")
    }else{

      # for(i in 1:3){
      #   if(max(Image_Data[,,i]) > 0){
      #     Image_Stack_normalized[,,i] <- Image_Stack_normalized[,,i]/max(Image_Stack_normalized[,,i])
      #   }
      # }

      disregarded_layers <- c()
      for(i in 1:3){
        if(max(Image_Data[,,i]) == 0){
          disregarded_layers <- c(disregarded_layers, i)
        }
      }

      Image_Stack_normalized <- normalizeIntensity(image = Image_Stack_normalized,
                                                   disregarded_layers = disregarded_layers)

      output_file_name <- paste(output_dir, "/", image_name_wo_czi, "_histogram_equalized_normalized.tif", sep="")
    }

    Image_Stack_normalized_copy <- Image_Stack_normalized
    if(add_scale_bar){
      Image_Stack_normalized <- addScaleBar(image = Image_Stack_normalized,
                                            length_per_pixel_in_um = length_per_pixel_x_in_um)
    }
    EBImage::writeImage(x = Image_Stack_normalized, files = output_file_name, type = "tiff", bits.per.sample = 8)

    Image_Stack_normalized <- Image_Stack_normalized_copy
    rm(Image_Stack_normalized_copy)
  }

}
