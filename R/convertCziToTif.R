#' @title convertCziToTif
#' @description Imports czi file and save them as tif
#' @details This functions saves a multidimensional array (image) as tifs
#' and stacks/edits the images if required.
#' @aliases convertczitotif convertCzitotif convertcziTotif convertczitoTif
#' convertCziTotif convertcziToTif
#' @author Kai Budde
#' @export convertCziToTif
#' @param input_file A character (path to czi file to be converted)
#' @param convert_all_slices A logical (T/F for converting and saving all z-slices)
#' @param stack_image A logical (T/F for calculating a z-stack)
#' @param stack_method A character (method for stacking the images)
#' @param higher_contrast_slices A logical (T/F for enhancing the contrast of the slices)
#' @param higher_contrast_stack A logical (T/F for enhancing the constrast of the z-stack)

convertCziToTif <- function(input_file = NULL,
                            convert_all_slices = TRUE,
                            stack_image = TRUE,
                            stack_method = "average",
                            higher_contrast_slices = TRUE,
                            higher_contrast_stack = TRUE) {
  if(is.null(input_file)){
    print("Please call function with input file.")
    return()
  }

  # Create output directory ------------------------------------------------
  directory_of_file <- dirname(input_file)
  file_name <- basename(input_file)
  image_name_wo_czi <- gsub("\\.czi", "", file_name)
  output_dir <- paste(directory_of_file, "/output", sep="")
  dir.create(output_dir, showWarnings = FALSE)

  # Load image and convert it to Image class -------------------------------
  # Dimensions of the image: 1: row, 2: col, 3: channels (r,g,b), 4: z-layer,
  image_data <- readCzi(input_file = input_file)

  Image_Data <- EBImage::Image(data = image_data, colormode = "Color")


  # Save all slices --------------------------------------------------------

  if(convert_all_slices){
    tif_file_names <- rep(image_name_wo_czi, dim(image_data)[4])
    tif_file_names <- paste(output_dir, "/", tif_file_names, "_z", 1:dim(image_data)[4], ".tif", sep="")

    EBImage::writeImage(x = Image_Data, files = tif_file_names, type = "tiff", bits.per.sample = 8)
  }

  # Save z-stack -----------------------------------------------------------
  if(stack_image){
    stack_method <- tolower(stack_method)

    if(stack_method == "average" || stack_method ==  "mean"){
      Image_Stack <- EBImage::Image(data = array(0, dim = dim(Image_Data)[1:3]), colormode = "Color")

      for(i in 1:dim(Image_Data)[3]){
        Image_Stack[,,i] = apply(Image_Data[,,i,], c(1,2), mean)
      }

      #Alternatives?
      # Get all color frames
      #getFrames(y = Input_Image, type = "render")
      #nuc_th = combine( mapply(function(frame, th) frame > th, getFrames(nuc), threshold, SIMPLIFY=FALSE) )

      #https://dahtah.github.io/imager/imager.html

    }else if(stack_method == "max"){

    }else if(stack_method == "addandnormalize"){

    }else if(stack_method == "maxcontrast"){

    }
    # Check out https://bioconductor.riken.jp/packages/3.7/bioc/vignettes/MaxContrastProjection/inst/doc/MaxContrastProjection.pdf
    # It is not working right now
  }

  stack_file_name <- paste(output_dir, "/", image_name_wo_czi, "_zstack.tif", sep="")
  EBImage::writeImage(x = Image_Stack, files = stack_file_name, type = "tiff", bits.per.sample = 8)

  # Enhance contrast of z-sclices ------------------------------------------
  if(higher_contrast_slices){

    Image_Data_histogram_equalization <- EBImage::combine(lapply(X = EBImage::getFrames(y = Image_Data, type = "render"),
                                                   FUN = function(frame){EBImage::clahe(x = frame)}))

    tif_file_names <- rep(image_name_wo_czi, dim(image_data)[4])
    tif_file_names <- paste(output_dir, "/", tif_file_names, "_z", 1:dim(image_data)[4], "_histogram_equalized.tif", sep="")

    EBImage::writeImage(x = Image_Data, files = tif_file_names, type = "tiff", bits.per.sample = 8)

  }

  # Enhance contrast of z-stack ------------------------------------------
  if(higher_contrast_stack){
    # Use Contrast Limited Adaptive Histogram Equalization
    Image_Stack_histogram_equalization <- EBImage::clahe(x = Image_Stack)

    stack_file_name <- paste(output_dir, "/", image_name_wo_czi, "_zstack_histogram_equalized.tif", sep="")
    EBImage::writeImage(x = Image_Stack_histogram_equalization, files = stack_file_name, type = "tiff", bits.per.sample = 8)

  }

  return()
}
