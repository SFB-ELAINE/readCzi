#' @title addScaleBar
#' @description Add a horizontal scale bare to image
#' @details Add a scale bar to a tiff image depending on the meta data of
#' the czi file
#' @aliases addscaleBar addScalebar addscalebar
#' @author Kai Budde-Sagert
#' @export addScaleBar
#' @param image An array (2 or 3 dimensional array, could also be of
#' formal class image by package EBimage)
#' @param length_per_pixel_in_um A number (length per pixel in um)
#' @param distance_from_border A number (distance in pixels for the scale bar)
#' @param number_size_factor A number (factor for resizing the number)
#' @returns An array (image with added scale bar).


addScaleBar <- function(image = NULL,
                        length_per_pixel_in_um = NULL,
                        distance_from_border = 20,
                        number_size_factor = 1){

  dim_x <- dim(image)[1]
  dim_y <- dim(image)[2]
  dim_c <- dim(image)[3]
  if(length(dim(image)) == 4){
    dim_z <- dim(image)[4]
  }else{
    dim_z <- 1
  }

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
  if(dim_z == 1){
    image[(dim_x-distance_from_border-length_scale_bar_pixels):(dim_x-distance_from_border),
          (dim_y-distance_from_border-heigth_scale_bar_pixels):(dim_y-distance_from_border),
          1:dim_c] <- 1
  }else{
    image[(dim_x-distance_from_border-length_scale_bar_pixels):(dim_x-distance_from_border),
          (dim_y-distance_from_border-heigth_scale_bar_pixels):(dim_y-distance_from_border),
          1:dim_c,1:dim_z] <- 1
  }


  # Add length of scale bar and unit
  number_pos_x <- dim_x-distance_from_border-0.5*length_scale_bar_pixels
  number_pos_y <- dim_y-distance_from_border-heigth_scale_bar_pixels

  # Get the file names with the text
  scale_legend_path <- paste(length_scale_bar_microns, "microns.tif", sep="")
  scale_legend_path <- system.file("scale", scale_legend_path, package = "cellPixels")

  scale_legend_image <- EBImage::readImage(files = scale_legend_path,
                                           type = "tiff")

  # Only keep the black layer
  scale_legend_image <- scale_legend_image[,,4]

  # Rescale image
  resize_factor <- dim_x/15/dim(scale_legend_image)[1]
  scale_legend_image <- resizeImage(image = scale_legend_image,
                                    resize_factor = resize_factor)

  dim_legend_x <- dim(scale_legend_image)[1]
  dim_legend_y <- dim(scale_legend_image)[2]

  # Add number images to image
  start_x <- floor(dim_x-distance_from_border-0.5*length_scale_bar_pixels-0.5*dim_legend_x)
  start_y <- floor(dim_y-distance_from_border-heigth_scale_bar_pixels-1.1*dim_legend_y)

  # Add \um image to original image
  scale_legend_image_copy <- scale_legend_image
  scale_legend_image <- EBImage::Image(dim = dim(image), colormode = "color")

  if(dim_z == 1){
    scale_legend_image[start_x:(start_x+dim_legend_x-1),
                       start_y:(start_y+dim_legend_y-1),] <-
      scale_legend_image_copy
  }else{
    scale_legend_image[start_x:(start_x+dim_legend_x-1),
                       start_y:(start_y+dim_legend_y-1),,] <-
      scale_legend_image_copy
  }


  image <- scale_legend_image + image
  image[image > 1] <- 1

  return(image)
}
