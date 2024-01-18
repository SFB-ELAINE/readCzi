#' Resize an image
#'
#' `resizeImage()` resizes an image with a given scaling factor
#'
#' @param image A one- to three-dimensional array of numbers between 0 and 1.
#' @param resize_factor A number being the factor for resizing an image.
#'
#' @returns An array (resized image).
#' @export resizeImage

resizeImage <- function(image = NULL,
                        resize_factor = 1){

  # Default values for missing arguments -----------------------------------
  if(is.null(image)){
    print(paste("Please call the function with an image.", sep=""))
    return()
  }

  if(is.null(resize_factor)){
    print(paste("Please call the function with a number", sep=""))
    return()
  }

  if(resize_factor < 0){
    print(paste("The factor may only be a number > 0", sep=""))
    return()
  }

  # Leave image as is if factor == 1 ---------------------------------------
  if(resize_factor == 1){
    return(image)
  }

  # Shrink image if resize_factor </> 1 ------------------------------------
  if(resize_factor > 0){

    new_height <-  round(resize_factor * dim(image)[1])
    new_width  <-  round(resize_factor * dim(image)[2])

    new_img = apply(image, 2, function(y){return (stats::spline(y, n = new_height)$y)})
    new_img = t(apply(new_img, 1, function(y){return (stats::spline(y, n = new_width)$y)}))

    new_img[new_img < 0] = 0
    new_img = round(new_img)

    return(new_img)
  }

  print("Something went wrong.")
  return(0)
}
