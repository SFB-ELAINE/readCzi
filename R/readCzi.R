#' @title readCzi
#' @description Imports czi files
#' @details The function returns a multidimensional array with pixel
#' identities from 0 to 1. The order of the array's dimensions are:
#' row (y), column (x), channel (red, green, blue), slices (z)
#' @aliases readczi readCZI
#' @author Kai Budde
#' @export readCzi
#' @param input_file A character (path to czi file to be converted)

readCzi <- function(input_file = NULL) {

  if(is.null(input_file)){
    print("Please call function with input file.")
    return()
  }

  # Read in Python package for reading czi files
  # (Users will be asked to install miniconda
  # when starting for the first time)
  if(!reticulate::py_module_available("czifile")){
    reticulate::py_install("czifile")
  }

  # Import module for czi files
  zis <- reticulate::import("czifile")

  # Load image directly from czi
  image_loaded <- zis$imread(input_file)

  # Save bit depth of the image --------------------------------------------
  czi_class <- zis$CziFile(input_file)

  bit_depth <- czi_class$dtype
  if(bit_depth == "uint16"){
    bit_depth <- 16
  }else if(bit_depth == "uint8"){
    bit_depth <- 8
  }else{
    print(paste("Something went wrong with the bit depth.", sep=""))
    return()
  }

  metadata <- czi_class$metadata(czi_class)

  # ComponentBitCount -> shows the number of bits the camera can record
  # (is 0 or not specified if it is the same as dtype)
  camera_bit_depth <- gsub(
    pattern = ".+<ComponentBitCount>(.+)</ComponentBitCount>.+",
    replacement = "\\1",
    x = metadata)
  camera_bit_depth <- as.numeric(camera_bit_depth)
  if(!is.na(camera_bit_depth) && (camera_bit_depth != 0)){
    bit_depth <- camera_bit_depth
  }

  # Convert to intensities between 0 and 1
  image_loaded <- image_loaded/(2^bit_depth-1)

  # Save the dimension information -----------------------------------------
  # C: Channel in a Multi-Channel data set
  # X: Pixel index / offset in the X direction. Used for tiled images (width)
  # Y: Pixel index / offset in the y direction. Used for tiled images (height)
  # Z: Slice (depth)
  # T: Time
  # R: Rotation,
  # S: Scene (contiguous regions of interest in a mosaic image)
  # I: Illumination (direction)
  # B: (Acquisition) Block index in segmented experiments.
  # M: Mosaic (index of tile for compositing a scene)
  # H: Phase e.g., (Airy detector fibers)
  # V: View (e.g., for SPIM)
  # 0: Sample (e.g., RGBA)

  # Compression:
  # 0: Uncompressed
  # 1: JpgFile
  # 2: LZW
  # 4: JpegXrFile
  # 100 and up: camera/system specific specific RAW data

  axes <- czi_class$axes
  axes <- unlist(strsplit(x = axes, split = ""))
  pos_channels <- grep(pattern = "C", x = axes)
  pos_x        <- grep(pattern = "X", x = axes)
  pos_y        <- grep(pattern = "Y", x = axes)
  pos_z        <- grep(pattern = "Z", x = axes)
  number_of_channels <- dim(image_loaded)[pos_channels]

  # Reorganize the layers to have the order red, green, blue ---------------
  # Find red, green, and blue channel ID
  if(number_of_channels == 3){
    wavelengths <- gsub(
      pattern =  paste(".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
                       ".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
                       ".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
                       sep=""),
      replacement = "\\1,\\2,\\3",
      x = metadata)
  }else if(number_of_channels == 2){
    wavelengths <- gsub(
      pattern =  paste(".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
                       ".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
                       sep=""),
      replacement = "\\1,\\2",
      x = metadata)
  }else if(number_of_channels == 1){
    wavelengths <- gsub(
      pattern =  paste(".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
                       sep=""),
      replacement = "\\1",
      x = metadata)
  }

  wavelengths <- as.numeric(strsplit(wavelengths, split = ",")[[1]])

  # Upper and lower limits of wavelengths to determine the colors
  # red: > 600nm
  # green: >= 500nm and <= 600nm
  # blue: < 500nm

  upper_limit_blue <- 500
  lower_limit_red <- 600

  test_red <- any(wavelengths > lower_limit_red)
  red_id <- ifelse(test = test_red, yes = which(wavelengths > lower_limit_red), no = 0)
  if(length(red_id) > 1){print("More than one red channel!?")}

  test_green <- any( (upper_limit_blue) <= wavelengths & (wavelengths <= lower_limit_red))
  green_id <- ifelse(test = test_green, yes = which((upper_limit_blue <= wavelengths) & (wavelengths <= lower_limit_red)), no = 0)
  if(length(green_id) > 1){print("More than one green channel!?")}

  test_blue <- any(wavelengths < upper_limit_blue)
  blue_id <- ifelse(test = test_blue, yes = which(upper_limit_blue > wavelengths), no = 0)
  if(length(blue_id) > 1){print("More than one blue channel!?")}

  rgb_layers <- c(red_id, green_id, blue_id)

  # Make a 3-D rgb-array of the image
  dim_x <- dim(image_loaded)[pos_x]
  dim_y <- dim(image_loaded)[pos_y]
  dim_z <- dim(image_loaded)[pos_z]

  if(dim_z > 1){

    # Reorganize the array according according to: "X","Y", "C", "Z"
    new_array_order <- c("X", "Y", "C", "Z")
    rearrange_array <- match(x = new_array_order, table = axes)

    # Fill the vector with missing indices
    rearrange_array <- c(rearrange_array,
                         which(!(1:length(axes) %in% rearrange_array)))

    # Reorganize array
    image_loaded <- aperm(image_loaded, perm = rearrange_array)


    # Reorder the layers according to the colors and add empty layer for
    # not existing colors
    copy_image_loaded <- image_loaded

    image_loaded <- array(data = 0, dim = c(dim_x, dim_y, 3, dim_z))

    # add missing channel(s)
    if(number_of_channels != 3){
      image_loaded[,,-which(rgb_layers == 0),] <- copy_image_loaded
    }

    if(length(dim(copy_image_loaded)) == 8){
      if(rgb_layers[1] != 0){
        image_loaded[,,1,] <- copy_image_loaded[,,rgb_layers[1],,,,,]
      }
      if(rgb_layers[2] != 0){
        image_loaded[,,2,] <- copy_image_loaded[,,rgb_layers[2],,,,,]
      }
      if(rgb_layers[3] != 0){
        image_loaded[,,3,] <- copy_image_loaded[,,rgb_layers[3],,,,,]
      }
    }else{
      print("The dimensions of the image does not fit.")
    }


    # Drop all arrays with dim=1 (except the channel, if C=1)
    #image_loaded <- drop(image_loaded)

    # Check if the resulting array contains as many entries as needed
    if(length(dim(image_loaded)) != length(new_array_order)){
      print("There is more or less information than thought in the image.")
      return()
    }

  }else{
    print(paste("We do not have a z-stack", sep=""))
    return()
  }

  # Reverse y-axis (It will go from top to bottom instead from bottom to top)
  #image_loaded <- EBImage::flip(image_loaded)

  rm(copy_image_loaded)

  rm(list= c("czi_class"))

  return(image_loaded)
}
