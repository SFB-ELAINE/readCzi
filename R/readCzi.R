#' Imports czi files
#'
#' `readCzi()` returns a multidimensional array with pixel intensities
#' from 0 to 1. The array's dimensions are: 1) row (y), 2) column (x),
#' 3) channel (red, green, blue), 4) z sections (z).
#'
#' @param input_file Path to a CZI file as a string.
#'
#' @returns A multidimensional array with the following dimensions:
#' \code{(x, y, c, z)} with the channels \code{c}.
#' @export readCzi

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
  czi_class <- zis$CziFile(input_file)

  # Load metadata
  df_metadata <- readCziMetadata(input_file = input_file,
                                 save_metadata = FALSE)
  metadata <- czi_class$metadata(czi_class)

  # Save bit depth of the image --------------------------------------------
  bit_depth <- czi_class$dtype
  if(bit_depth == "uint16"){
    bit_depth <- 16
  }else if(bit_depth == "uint8"){
    bit_depth <- 8
  }else{
    print(paste("Something went wrong with the bit depth.", sep=""))
    return()
  }

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

  pos_channels <- NULL
  pos_x <- NULL
  pos_y <- NULL
  pos_z <- NULL
  pos_rgba <- NULL

  if("S" %in% axes){
    pos_scenes <- grep(pattern = "S", x = axes)
  }
  if("C" %in% axes){
    pos_channels <- grep(pattern = "C", x = axes)
  }
  if("X" %in% axes){
    pos_x <- grep(pattern = "X", x = axes)
  }
  if("Y" %in% axes){
    pos_y <- grep(pattern = "Y", x = axes)
  }
  if("Z" %in% axes){
    pos_z <- grep(pattern = "Z", x = axes)
  }
  if("0" %in% axes){
    pos_rgba <- grep(pattern = "0", x = axes)
  }

  number_of_channels <- dim(image_loaded)[pos_channels]

  # Reorganize the layers to have the order red, green, blue ---------------
  # Find red, green, and blue channel ID

  # Check if RGBA (red, green, blue, alpha) is used
  # If yes: Use this
  if(!is.null(pos_rgba) & dim(image_loaded)[pos_rgba] > 1){
    if(dim(image_loaded)[pos_channels] > 1){
      print(paste("We have information both in the RGBA as well as ",
                  "in channels layers. Something might be wrong.", sep=""))
    }

    color_axis <- "0"

    if(dim(image_loaded)[pos_rgba] > 3){
      print(paste("There is an alpha layer in the image data, ",
                  "which we have not incorporated yet.", sep=""))
      return()
    }

  }

  # Decide which channel is red/green/blue
  if(grepl(pattern = "EmissionWavelength",
           x = metadata,
           ignore.case = TRUE)){

    color_axis <- "C"

    rgb_layers <- c(df_metadata$red_channel,
                    df_metadata$green_channel,
                    df_metadata$blue_channel)

    # Check if all channels have a designated color, if not, provide a color
    if(df_metadata$number_of_channels > sum(!is.na(rgb_layers))){
      print("There are more channels than corresponding wavelengths.")
      if(df_metadata$number_of_tracks < df_metadata$number_of_channels){
        print("This is because multiple channels were recorded in the same track.")

        duplicated_tracks <- c(df_metadata$track_id_channel_1,
                               df_metadata$track_id_channel_2,
                               df_metadata$track_id_channel_3)[
                                 duplicated(c(df_metadata$track_id_channel_1,
                                              df_metadata$track_id_channel_2,
                                              df_metadata$track_id_channel_3))]

        channels_number_with_one_track <- which(
          c(df_metadata$track_id_channel_1,
            df_metadata$track_id_channel_2,
            df_metadata$track_id_channel_3) == duplicated_tracks)

        channels_with_one_track <- c(df_metadata$channel_name_1,
                                     df_metadata$channel_name_2,
                                     df_metadata$channel_name_3)[
                                       channels_number_with_one_track]


        print(paste0("This is true for channels: ",
                     paste0(channels_number_with_one_track, collapse = " and "),
                     " having the names ",
                     paste0(channels_with_one_track, collapse = " and "), "."))

        # Provide a color for channels without one
        available_channels <- which(c(!is.na(df_metadata$channel_name_1),
                                      !is.na(df_metadata$channel_name_2),
                                      !is.na(df_metadata$channel_name_3)))

        available_channel_names <- c(df_metadata$channel_name_1,
                                     df_metadata$channel_name_2,
                                     df_metadata$channel_name_3)[
                                       available_channels]

        channels_without_color <- available_channels[!available_channels %in% rgb_layers]

        for(i in 1:length(channels_without_color)){

          if(which(is.na(rgb_layers))[i] == 1){
            new_color <- "red"
          }else if(which(is.na(rgb_layers))[i] == 2){
            new_color <- "green"
          }else if(which(is.na(rgb_layers))[i] == 3){
            new_color <- "blue"
          }

          # Designated color for channel
          rgb_layers[which(is.na(rgb_layers))[i]] <- channels_without_color[i]

          current_channel_name <- c(df_metadata$channel_name_1,
                                    df_metadata$channel_name_2,
                                    df_metadata$channel_name_3)[
                                      channels_without_color[i]]

          print(paste0("Channel ", channels_without_color[i], " (name: ",
                       current_channel_name, ") now ", new_color,
                       "."))

        }
        rm(i)


        df_metadata$track_id_channel_1
      }

    }

  }

  # Make a 3-D rgb-array of the image
  dim_x <- dim(image_loaded)[pos_x]
  dim_y <- dim(image_loaded)[pos_y]
  dim_z <- ifelse(test = is.null(pos_z), yes = 1, no = dim(image_loaded)[pos_z])

  if(dim_z > 1){
    # Reorganize the array according according to: "X","Y", "C"/"0", "Z"
    new_array_order <- c("X", "Y", color_axis, "Z")
  }else{
    # Reorganize the array according according to: "X","Y", "C"/"0"
    new_array_order <- c("X", "Y", color_axis)
  }

  rearrange_array <- match(x = new_array_order, table = axes)

  # Fill the vector with missing indices
  rearrange_array <- c(rearrange_array,
                       which(!(1:length(axes) %in% rearrange_array)))

  # Reorganize array
  image_loaded <- aperm(image_loaded, perm = rearrange_array)


  # Add missing channel(s) for images with color_axis=="C"
  if(color_axis == "C"){

    # Reorder the layers according to the colors and add empty layer for
    # not existing colors
    copy_image_loaded <- image_loaded
    image_loaded <- array(data = 0, dim = c(dim_x, dim_y, 3, dim_z))

    # Copy image layers in the correct order depending on which dimension
    # the original image has
    if(length(dim(copy_image_loaded)) == 12){

      if(!is.na(rgb_layers[1]) && rgb_layers[1] != 0){
        image_loaded[,,1,] <- copy_image_loaded[,,rgb_layers[1],,,,,,,,,]
      }
      if(!is.na(rgb_layers[2]) && rgb_layers[2] != 0){
        image_loaded[,,2,] <- copy_image_loaded[,,rgb_layers[2],,,,,,,,,]
      }
      if(!is.na(rgb_layers[3]) && rgb_layers[3] != 0){
        image_loaded[,,3,] <- copy_image_loaded[,,rgb_layers[3],,,,,,,,,]
      }

      # Check if the resulting array contains as many entries as needed
      if(dim_z == 1){
        check_array_dims <- length(new_array_order) + 1
      }else{
        check_array_dims <- length(new_array_order)
      }

      # Check if the resulting array contains as many entries as needed
      if(length(dim(image_loaded)) != check_array_dims){
        print("There is more or less information than thought in the image.")
        return()
      }

    }else if(length(dim(copy_image_loaded)) == 8){

      if(!is.na(rgb_layers[1]) && rgb_layers[1] != 0){
        image_loaded[,,1,] <- copy_image_loaded[,,rgb_layers[1],,,,,]
      }
      if(!is.na(rgb_layers[2]) && rgb_layers[2] != 0){
        image_loaded[,,2,] <- copy_image_loaded[,,rgb_layers[2],,,,,]
      }
      if(!is.na(rgb_layers[3]) && rgb_layers[3] != 0){
        image_loaded[,,3,] <- copy_image_loaded[,,rgb_layers[3],,,,,]
      }

      # Check if the resulting array contains as many entries as needed
      if(dim_z == 1){
        check_array_dims <- length(new_array_order) + 1
      }else{
        check_array_dims <- length(new_array_order)
      }

      # Check if the resulting array contains as many entries as needed
      if(length(dim(image_loaded)) != check_array_dims){
        print("There is more or less information than thought in the image.")
        return()
      }


    }else if(length(dim(copy_image_loaded)) == 6){

      if(!is.na(rgb_layers[1]) & rgb_layers[1] != 0){
        image_loaded[,,1,] <- copy_image_loaded[,,rgb_layers[1],,,]
      }
      if(!is.na(rgb_layers[2]) & rgb_layers[2] != 0){
        image_loaded[,,2,] <- copy_image_loaded[,,rgb_layers[2],,,]
      }
      if(!is.na(rgb_layers[3]) & rgb_layers[3] != 0){
        image_loaded[,,3,] <- copy_image_loaded[,,rgb_layers[3],,,]
      }


      # Check if the resulting array contains as many entries as needed
      if(dim_z == 1){
        check_array_dims <- length(new_array_order) + 1
      }else{
        check_array_dims <- length(new_array_order)
      }

      if(length(dim(image_loaded)) != check_array_dims){
        print("There is more or less information than thought in the image.")
        return()
      }


    }else if(length(dim(copy_image_loaded)) == 5){

      if(!is.na(rgb_layers[1]) & rgb_layers[1] != 0){
        image_loaded[,,1,] <- copy_image_loaded[,,rgb_layers[1],,]
      }
      if(!is.na(rgb_layers[2]) & rgb_layers[2] != 0){
        image_loaded[,,2,] <- copy_image_loaded[,,rgb_layers[2],,]
      }
      if(!is.na(rgb_layers[3]) & rgb_layers[3] != 0){
        image_loaded[,,3,] <- copy_image_loaded[,,rgb_layers[3],,]
      }


      # Check if the resulting array contains as many entries as needed
      if(dim_z == 1){
        check_array_dims <- length(new_array_order) + 1
      }else{
        check_array_dims <- length(new_array_order)
      }

      if(length(dim(image_loaded)) != check_array_dims){
        print("There is more or less information than thought in the image.")
        return()
      }


    }else{
      print("The dimensions of the image (array) do not fit. Check readCzi.R.")
    }

  }else if(color_axis == "0"){
    copy_image_loaded <- image_loaded
    image_loaded <- array(data = 0, dim = c(dim_x, dim_y, 3, dim_z))

    # Drop all arrays with dim=1
    copy_image_loaded <- drop(copy_image_loaded)

    if(dim_z == 1){
      image_loaded[,,,1] <- copy_image_loaded[,,]
    }else{
      image_loaded <- copy_image_loaded
    }

  }else{
    print(paste("There is no color axis.", sep=""))
  }

  rm(copy_image_loaded)

  rm(list= c("czi_class"))

  return(image_loaded)
}
