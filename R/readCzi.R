#' @title readCzi
#' @description Imports czi files and save the metadata
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

  zis <- reticulate::import("czifile")

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

  # Reorganize the layers to have the order red, green, blue ---------------
  # Find red, green, and blue channel ID
  wavelengths <- gsub(
    pattern =  paste(".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
                     ".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
                     ".+<EmissionWavelength>(.+)</EmissionWavelength>.+",
                     sep=""),
    replacement = "\\1,\\2,\\3",
    x = metadata)
  wavelengths <- as.numeric(strsplit(wavelengths, split = ",")[[1]])

  red_id <- which(wavelengths == max(wavelengths))
  blue_id <- which(wavelengths == min(wavelengths))

  if(length(wavelengths) == 3){
    green_id <- c(1:3)[!(c(1:3) %in% red_id | c(1:3) %in% blue_id)]
  }else{
    print("There are more or fewer than 3 wavelengths.")
    return()
  }

  rgb_layers <- c(red_id, green_id, blue_id)

  # Make a 3-D array of the image
  number_of_channels <- dim(image_loaded)[pos_channels]
  dim_x <- dim(image_loaded)[pos_x]
  dim_y <- dim(image_loaded)[pos_y]
  dim_z <- dim(image_loaded)[pos_z]

  if(number_of_channels == 3 & dim_z > 1){

    # Reorganize the array according according to: "Y", "X", "C", "Z"
    new_array_order <- c("Y", "X", "C", "Z")
    rearrange_array <- match(x = new_array_order, table = axes)

    # Fill the vector with missing indices
    rearrange_array <- c(rearrange_array,
                         which(!(1:length(axes) %in% rearrange_array)))

    # Reorganize array
    image_loaded <- aperm(image_loaded, perm = rearrange_array)

    # Drop all arrays with dim=1
    image_loaded <- drop(image_loaded)

    # Check if the resulting array contains as many entries as needed
    if(length(dim(image_loaded)) != length(new_array_order)){
      print("There is more or less information than thought in the image.")
      return()
    }

    # Reorder the layers according to the colors
    copy_image_loaded <- image_loaded

    image_loaded[,1,,] <- copy_image_loaded[,rgb_layers[1],,]
    image_loaded[,2,,] <- copy_image_loaded[,rgb_layers[2],,]
    image_loaded[,3,,] <- copy_image_loaded[,rgb_layers[3],,]

    rm(copy_image_loaded)

  }else{
    print(paste("We do not have a tif-image with three layers",
                " representing red, green, and blue, ",
                "and a z-stack", sep=""))
    return()
  }

  rm(list= c("czi_class"))

  return(image_loaded)
}
