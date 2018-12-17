focalWindow <- function (x, d, type = c("circle", "rectangle")){
  type <- match.arg(type)
  x <- res(x)
  if (type == "circle") {
    circularWindow(x, d)
  } else {
    rectangleWindow(x, d)
  }
}