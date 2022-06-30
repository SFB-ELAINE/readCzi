#' @title addScaleBar
#' @description Add a horizontal scale bare to image
#' @details Add a scale bar to a tiff image depending on the meta data of
#' the czi file
#' @aliases addscaleBar addScalebar addscalebar
#' @author Kai Budde
#' @export addScaleBar
#' @param image An array (2 or 3 dimensional array, could also be of
#' formal class Image by package EBImage)
#' @param length_per_pixel_in_um A number (length per pixel in um)
#' @param distance_from_border A number (distance in pixels for the scale bar)
#' @param number_size_factor A number (factor for resizing the number)


addScaleBar <- function(image = NULL,
                        length_per_pixel_in_um = NULL,
                        distance_from_border = 20,
                        number_size_factor = 1){

  # dim_x <- dim(image)[2]
  # dim_y <- dim(image)[1]
  # dim_z <- dim(image)[3]
  dim_x <- dim(image)[1]
  dim_y <- dim(image)[2]
  dim_z <- dim(image)[3]


  dim_x_microns <- dim_x * length_per_pixel_in_um
  length_scale_bar_microns <- round(dim_x_microns / 10)

  if(length_scale_bar_microns < 10){
    length_scale_bar_microns <- 10
  }else if(length_scale_bar_microns < 20){
    length_scale_bar_microns <- 20
  }else if(length_scale_bar_microns < 50){
    length_scale_bar_microns <- 50
  }else if(length_scale_bar_microns < 100){
    length_scale_bar_microns <- 100
  }else if(length_scale_bar_microns < 200){
    length_scale_bar_microns <- 200
  }else if(length_scale_bar_microns < 500){
    length_scale_bar_microns <- 500
  }

  length_scale_bar_pixels <- length_scale_bar_microns /
    (length_per_pixel_in_um)

  length_scale_bar_pixels <- as.integer(round(length_scale_bar_pixels))
  heigth_scale_bar_pixels <- as.integer(round(dim_y/150))

  # Add the scale_bar
  # image[(dim_y-distance_from_border-heigth_scale_bar_pixels):(dim_y-distance_from_border),
  #       (dim_x-distance_from_border-length_scale_bar_pixels):(dim_x-distance_from_border),
  #       1:dim_z] <- 1
  image[(dim_x-distance_from_border-length_scale_bar_pixels):(dim_x-distance_from_border),
        (dim_y-distance_from_border-heigth_scale_bar_pixels):(dim_y-distance_from_border),
        1:dim_z] <- 1

  # Add length and unit
  # number_pos_x <- dim_y-distance_from_border-heigth_scale_bar_pixels
  # number_pos_y <- dim_x-distance_from_border-0.5*length_scale_bar_pixels
  number_pos_x <- dim_x-distance_from_border-0.5*length_scale_bar_pixels
  number_pos_y <- dim_y-distance_from_border-heigth_scale_bar_pixels

  # Get the file names with the text
  scale_legend_path <- paste(length_scale_bar_microns, "microns.tif", sep="")
  scale_legend_path <- system.file("scale", scale_legend_path, package = "cellPixels")

  scale_legend_image <- tiff::readTIFF(source = scale_legend_path, convert = TRUE,
                                       info = FALSE)
  # Only keep the black layer
  scale_legend_image <- scale_legend_image[,,4]

  # Rescale image
  resize_factor <- dim_x/15/dim(scale_legend_image)[2]
  scale_legend_image <- resizeImage(image = scale_legend_image,
                                    resize_factor = resize_factor)

  dim_legend_x <- dim(scale_legend_image)[2]
  dim_legend_y <- dim(scale_legend_image)[1]
  # dim_legend_x <- dim(scale_legend_image)[1]
  # dim_legend_y <- dim(scale_legend_image)[2]

  # Add number images to image
  start_x <- dim_x-distance_from_border-0.5*length_scale_bar_pixels-0.5*dim_legend_x
  start_y <- dim_y-distance_from_border-heigth_scale_bar_pixels-1.1*dim_legend_y
  # start_x <- dim_x-distance_from_border-0.5*length_scale_bar_pixels-0.5*dim_legend_x
  # start_y <- dim_y-distance_from_border-heigth_scale_bar_pixels-1.1*dim_legend_y

  # for(row in 1:dim_legend_y){
  #   for(col in 1:dim_legend_x){
  for(col in 1:dim_legend_x){
    for(row in 1:dim_legend_y){
      if(scale_legend_image[row, col] > 0){
        # if(scale_legend_image[col, row] > 0){
        # image[start_y+row, start_x+col, 1:dim_z] <- scale_legend_image[row, col]
        image[start_x+col, start_y+row, 1:dim_z] <- scale_legend_image[row, col]
      }
    }
  }

  return(image)
}
