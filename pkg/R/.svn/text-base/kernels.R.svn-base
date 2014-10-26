sobolevKernel <- function(s, r, t = 1, sub = NULL, constraint = 'boundary') {
  smin <- pmin(s, r)/t
  r <- pmax(s, r)/t
  s <- smin
  if (constraint == 'boundary') {
    if (is.null(sub)) {
      1 + s * r  +
        10 * (1 - r) * s * (2 * r - r * r - s * s) 
    } else if (sub == 0) {
      1 + s * r 
    } else {
      10 * (1 - r) * s * (2 * r - r * r - s * s)
    }
  } else if (constraint == 'initial') {
    if (is.null(sub)) {
      1 + s * r  + 5 * s * s * (3 * r - s) 
    } else if (sub == 0) {
      1 + s * r 
    } else {
      5 * s * s * (3 * r - s)
    }
  } else {
    stop("Argument 'constraint' has to be either 'boundary' or 'initial'.")
  }
}

gaussianKernel <- function(s, r, t = 1, c = 1) {
  s <- s/t
  r <- r/t
    exp( - c * (s - r)^2)
}

## Deprecated kernel
  
.sobolevKernel <- function(s, r, t = 1, sub = NULL) {
    smin <- pmin(s, r)/t
    r <- pmax(s, r)/t
    s <- smin
    if (is.null(sub)) {
      1 + s * r / 48 +
        (1 - r) * s * (2 * r - r * r - s * s) / 6
    } else if (sub == 0) {
      1 + s * r / 48
    } else {
      (1 - r) * s * (2 * r - r * r - s * s) / 6
    }
  }
  