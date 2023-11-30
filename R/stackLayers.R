#' @title stackLayers
#' @description Stacks z-layers
#' @details This functions saves a multidimensional array (image) as tifs
#' and stacks/edits the images if required.
#' @aliases stacklayers StackLayers
#' @author Kai Budde-Sagert
#' @export stackLayers
#' @param image_data An array of EBImage object
#' @param stack_method A character (method for stacking the images: average,
#' maxprojection)
#' @param as_array A Boolean (if TRUE, the data is return as an array and not
#' an EBImage class)
#' @returns An array (z-stack projection of a 3D image).

stackLayers <- function(image_data = NULL,
                        stack_method = "average",
                        as_array = FALSE) {

  if(is.null(image_data)){
    print("Please call function with image data.")
    return()
  }

  stack_method <- tolower(stack_method)
  Image_Stack <- EBImage::Image(data = array(0, dim = dim(image_data)[1:3]), colormode = "Color")

  if(isa(x = image_data, what = "array")){
    # Convert to EBImage
    image_data <- EBImage::Image(data = image_data, colormode = "Color")
  }

  if(stack_method == "average" || stack_method ==  "mean"){

    for(i in 1:dim(image_data)[3]){
      Image_Stack[,,i] <- apply(image_data[,,i,], c(1,2), mean)
    }

  }else if(stack_method == "maxprojection" || stack_method == "max"){

    for(i in 1:dim(image_data)[3]){
      Image_Stack[,,i] = apply(image_data[,,i,], c(1,2), max)
    }

  }else if(stack_method == "addandnormalize"){

  }else if(stack_method == "maxcontrast"){

  }

  if(as_array){
    Image_Stack <- as.array(Image_Stack)
  }

  return(Image_Stack)
}
