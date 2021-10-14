#' @title normalizeIntensity
#' @description Normalize the intensity of the image
#' @details Normalize the intensity of an image by dividing all values by
#' the maximum intensity.
#' @aliases normalizeintensity
#' @author Kai Budde
#' @export normalizeIntensity
#' @param image An three-dimensional array of numbers between 0 and 1
#' @param disregarded_layers A vector of layers that shall not be normalized

normalizeIntensity <- function(image = NULL,
                               disregarded_layers = NULL){

  # Default values for missing arguments -----------------------------------
  if(is.null(image)){
    print(paste("Please call the function with an image.", sep=""))
    return()
  }

  # Normalize intensity of each layer --------------------------------------
  for(i in 1:dim(image)[3]){
    if(is.null(disregarded_layers) || !(i %in% disregarded_layers)){
      image[,,i] <- image[,,i]/max(image[,,i])
    }
  }

  return(image)
}
