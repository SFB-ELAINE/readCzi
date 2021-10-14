#' @title stackLayers
#' @description Stacks z-layers
#' @details This functions saves a multidimensional array (image) as tifs
#' and stacks/edits the images if required.
#' @aliases stacklayers StackLayers
#' @author Kai Budde
#' @export stackLayers
#' @param image_data An array of EBImage object
#' @param stack_method A character (method for stacking the images: average,
#' maxprojection)
#'
stackLayers <- function(image_data = NULL,
                        stack_method = "average") {

  if(is.null(image_data)){
    print("Please call function with image data.")
    return()
  }

  stack_method <- tolower(stack_method)
  Image_Stack <- EBImage::Image(data = array(0, dim = dim(image_data)[1:3]), colormode = "Color")

  if(class(image_data) == "array"){
    # Convert to EBImage
    image_data <- EBImage::Image(data = image_data, colormode = "Color")
  }

  if(stack_method == "average" || stack_method ==  "mean"){

    for(i in 1:dim(image_data)[3]){
      Image_Stack[,,i] <- apply(image_data[,,i,], c(1,2), mean)
    }

    #Alternatives?
    # Get all color frames
    #getFrames(y = Input_Image, type = "render")
    #nuc_th = combine( mapply(function(frame, th) frame > th, getFrames(nuc), threshold, SIMPLIFY=FALSE) )

    #https://dahtah.github.io/imager/imager.html

  }else if(stack_method == "maxprojection" || stack_method == "max"){

    for(i in 1:dim(image_data)[3]){
      Image_Stack[,,i] = apply(image_data[,,i,], c(1,2), max)
    }

  }else if(stack_method == "addandnormalize"){

  }else if(stack_method == "maxcontrast"){

  }
  # Check out https://bioconductor.riken.jp/packages/3.7/bioc/vignettes/MaxContrastProjection/inst/doc/MaxContrastProjection.pdf
  # It is not working right now

  return(Image_Stack)

}
