#' calculation of areas
#'
#' @param h height or radius of the figure.
#' @param w weight of the figure.
#' @param fig type of geometrical figure, "rectangle" (r), "triangle" (t), "circle" (c)
#'
#' @return A float.
#' @export
#'
#' @examples
#' hg <- 3
#' wg <- 2
#' fig <-"triangle"
#' area(hg, wg, fig)



area <- function(h,w, fig) {
  fig = tolower(fig)
  if(fig=='rectangle' | fig=="r"){
    area = h*w
  } else {
    if(fig=="circle" | fig=="c"){
      area = h*h*pi
    } else {
      if(fig=="triangle" | fig=="t"){
        area = (h*w) / 2
      }
    }
  }
  return(area)
}
