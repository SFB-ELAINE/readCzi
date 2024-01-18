#' Calculate the projection of a z-stack image
#'
#' @param image_data An array of EBImage object
#' @param projection_method A character stating the method for calculating
#' the z-stack projection (either "mean" or "max")
#' @param as_array A Boolean stating whether the returned data is a array or
#' of class EBImage. (The default value is FALSE.)
#'
#' @returns An array (z-stack projection of a 3D image).

zstackProjection <- function(image_data = NULL,
                        projection_method = "max",
                        as_array = FALSE) {

  if(is.null(image_data)){
    print("Please call function with image data.")
    return()
  }

  projection_method <- tolower(projection_method)
  Image_projection <- EBImage::Image(data = array(0, dim = dim(image_data)[1:3]), colormode = "Color")

  if(isa(x = image_data, what = "array")){
    # Convert to EBImage
    image_data <- EBImage::Image(data = image_data, colormode = "Color")
  }

  if(projection_method ==  "mean" || projection_method == "average"){

    for(i in 1:dim(image_data)[3]){
      Image_projection[,,i] <- apply(image_data[,,i,], c(1,2), mean)
    }

  }else if(projection_method == "max" || projection_method == "maxprojection"){

    for(i in 1:dim(image_data)[3]){
      Image_projection[,,i] = apply(image_data[,,i,], c(1,2), max)
    }

  }

  if(as_array){
    Image_projection <- as.array(Image_projection)
  }

  return(Image_projection)
}
