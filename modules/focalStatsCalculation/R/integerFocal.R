integerFocal <- function (x, ...){
  library("raster")
  .local <- function(x, w, fun, filename = "", na.rm = FALSE, 
                      pad = FALSE, padValue = NA, NAonly = FALSE, ...){
    stopifnot(hasValues(x))
    dots <- list(...)
    if (!is.null(dots$filter)) {
      warning("argument \"filter\" is ignored!")
    }
    if (!is.null(dots$ngb)) {
      warning("argument \"ngb\" is ignored!")
    }
    stopifnot(is.matrix(w))
    d <- dim(w)
    if (prod(d) == 0) {
      stop("ncol and nrow of w must be > 0")
    }
    if (min(d%%2) == 0) {
      stop("w must have uneven sides")
    }
    w[] <- as.vector(t(w))
    out <- raster(x)
    filename <- trim(filename)
    padrows <- FALSE
    if (pad) {
      padrows <- TRUE
    }
    gll <- as.integer(raster:::.isGlobalLonLat(out))
    if (gll) {
      pad <- TRUE
    }
    dofun <- TRUE
    domean <- FALSE
    if (missing(fun)) {
      dofun <- FALSE
      domean <- FALSE
    }
    else {
      fun2 <- raster:::.makeTextFun(fun)
      if (is.character(fun2)) {
        if (fun2 == "mean") {
          domean <- TRUE
          dofun <- FALSE
        }
        else if (fun2 == "sum") {
          dofun <- FALSE
        }
      }
    }
    if (dofun) {
      e <- new.env()
      if (na.rm) {
        runfun <- function(x) as.double(fun(x, na.rm = TRUE))
      }
      else {
        runfun <- function(x) as.double(fun(x))
      }
    }
    NAonly <- as.integer(NAonly)
    narm <- as.integer(na.rm)
    domean <- as.integer(domean)
    if (canProcessInMemory(out)) {
      if (pad) {
        f <- floor(d/2)
        v <- as.matrix(x) # Still integer here
        if (padrows) {
          padRows <- matrix(padValue, ncol = ncol(out), 
                            nrow = f[1])
          v <- rbind(padRows, v, padRows)
        }
        if (gll) {
          v <- cbind(v[, (ncol(v) - f[2] + 1):ncol(v)], 
                     v, v[, 1:f[2]])
        }
        else {
          padCols <- matrix(padValue, nrow = nrow(v), 
                            ncol = f[2])
          v <- cbind(padCols, v, padCols)
        }
        paddim <- as.integer(dim(v))
        if (dofun) {
          v <- as.integer(.Call("_focal_fun", as.vector(t(v)), w, 
                     paddim, runfun, NAonly, e, NAOK = TRUE, PACKAGE = "raster"))
        }
        else {
          v <- as.integer(.Call("_focal_sum", as.vector(t(v)), w,
                     paddim, narm, NAonly, domean = domean, NAOK = TRUE, 
                     PACKAGE = "raster"))
        }
        v <- matrix(v, nrow = paddim[1], ncol = paddim[2], 
                    byrow = TRUE)
        if (padrows) {
          v <- v[-c(1:f[1], (nrow(v) - f[1] + 1):nrow(v)), 
                 -c(1:f[2], (ncol(v) - f[2] + 1):ncol(v))]
        }
        else {
          v <- v[, -c(1:f[2], (ncol(v) - f[2] + 1):ncol(v))]
        }
        v <- as.vector(t(v))
      }
      else {
        if (dofun) {
          v <- as.integer(.Call("_focal_fun", values(x), w, as.integer(dim(out)),
                     runfun, NAonly, e, NAOK = TRUE, PACKAGE = "raster"))
        }
        else {
          v <- as.integer(.Call("_focal_sum", values(x), w, as.integer(dim(out)), 
                     narm, NAonly, domean = domean, NAOK = TRUE, 
                     PACKAGE = "raster"))
        }
      }
      if (!is.null(dots$datatype)){
        warning("datatype argument passed. This changes only the metadata, as the raster is in memory. 
                \nThe raster will still have integer values. To pass a datatype to a written raster, pass a filename")
        dataType(out) <- dots$datatype
      }
      out <- setValues(out, v)
      if (filename != "") {
        out <- raster::writeRaster(out, filename, datatype = dots$datatype, format = "GTiff", ...)
      }
    }
    else {
      out <- writeStart(out, filename, ...)
      tr <- blockSize(out, minblocks = 3, minrows = 3)
      pb <- pbCreate(tr$n, label = "focal", ...)
      addr <- floor(nrow(w)/2)
      addc <- floor(ncol(w)/2)
      nc <- ncol(out)
      nc1 <- 1:(nc * addc)
      if (pad) {
        f <- floor(d/2)
        v <- getValues(x, row = 1, nrows = tr$nrows[1] + 
                         addr)
        v <- matrix(v, ncol = ncol(out), byrow = TRUE)
        if (padrows) {
          padRows <- matrix(padValue, ncol = ncol(out), 
                            nrow = f[1])
          v <- rbind(padRows, v, padRows)
        }
        if (gll) {
          v <- cbind(v[, (ncol(v) - f[2] + 1):ncol(v)], 
                     v, v[, 1:f[2]])
        }
        else {
          padCols <- matrix(padValue, nrow = nrow(v), 
                            ncol = f[2])
          v <- cbind(padCols, v, padCols)
        }
        paddim <- as.integer(dim(v))
        if (dofun) {
          v <- as.integer(.Call("_focal_fun", as.vector(t(v)), w, 
                     paddim, runfun, NAonly, e, NAOK = TRUE, PACKAGE = "raster"))
        }
        else {
          v <- as.integer(.Call("_focal_sum", as.vector(t(v)), w, 
                     paddim, narm, NAonly, domean = domean, NAOK = TRUE, 
                     PACKAGE = "raster"))
        }
        v <- matrix(v, nrow = paddim[1], ncol = paddim[2], 
                    byrow = TRUE)
        if (padrows) {
          v <- v[-c(1:f[1], (nrow(v) - f[1] + 1):nrow(v)), 
                 -c(1:f[2], (ncol(v) - f[2] + 1):ncol(v))]
        }
        else {
          v <- v[, -c(1:f[2], (ncol(v) - f[2] + 1):ncol(v))]
        }
        v <- as.vector(t(v))
        out <- writeValues(out, v, 1)
        pbStep(pb)
        for (i in 2:(tr$n - 1)) {
          v <- getValues(x, row = tr$row[i] - addr, nrows = tr$nrows[i] + 
                           (2 * addr))
          v <- matrix(v, ncol = ncol(out), byrow = TRUE)
          if (padrows) {
            padRows <- matrix(padValue, ncol = ncol(out), 
                              nrow = f[1])
            v <- rbind(padRows, v, padRows)
          }
          if (gll) {
            v <- cbind(v[, (ncol(v) - f[2] + 1):ncol(v)], 
                       v, v[, 1:f[2]])
          }
          else {
            padCols <- matrix(padValue, nrow = nrow(v), 
                              ncol = f[2])
            v <- cbind(padCols, v, padCols)
          }
          paddim <- as.integer(dim(v))
          if (dofun) {
            v <- as.integer(.Call("_focal_fun", as.vector(t(v)), 
                       w, paddim, runfun, NAonly, e, NAOK = TRUE, 
                       PACKAGE = "raster"))
          }
          else {
            v <- as.integer(.Call("_focal_sum", as.vector(t(v)), 
                       w, paddim, narm, NAonly, domean = domean, 
                       NAOK = TRUE, PACKAGE = "raster"))
          }
          v <- matrix(v, nrow = paddim[1], ncol = paddim[2], 
                      byrow = TRUE)
          if (padrows) {
            v <- v[-c(1:f[1], (nrow(v) - f[1] + 1):nrow(v)), 
                   -c(1:f[2], (ncol(v) - f[2] + 1):ncol(v))]
          }
          else {
            v <- v[, -c(1:f[2], (ncol(v) - f[2] + 1):ncol(v))]
          }
          v <- as.vector(t(v))
          out <- writeValues(out, v[-nc1], tr$row[i])
          pbStep(pb)
        }
        i <- tr$n
        v <- getValues(x, row = tr$row[i] - addr, nrows = tr$nrows[i] + 
                         addr)
        v <- matrix(v, ncol = ncol(out), byrow = TRUE)
        if (padrows) {
          padRows <- matrix(padValue, ncol = ncol(out), 
                            nrow = f[1])
          v <- rbind(padRows, v, padRows)
        }
        if (gll) {
          v <- cbind(v[, (ncol(v) - f[2] + 1):ncol(v)], 
                     v, v[, 1:f[2]])
        }
        else {
          padCols <- matrix(padValue, nrow = nrow(v), 
                            ncol = f[2])
          v <- cbind(padCols, v, padCols)
        }
        paddim <- as.integer(dim(v))
        if (dofun) {
          v <- .Call("_focal_fun", as.vector(t(v)), w, 
                     paddim, runfun, NAonly, e, NAOK = TRUE, PACKAGE = "raster")
        }
        else {
          v <- .Call("_focal_sum", as.vector(t(v)), w, 
                     paddim, narm, NAonly, domean = domean, NAOK = TRUE, 
                     PACKAGE = "raster")
        }
        v <- matrix(v, nrow = paddim[1], ncol = paddim[2], 
                    byrow = TRUE)
        if (padrows) {
          v <- v[-c(1:f[1], (nrow(v) - f[1] + 1):nrow(v)), 
                 -c(1:f[2], (ncol(v) - f[2] + 1):ncol(v))]
        }
        else {
          v <- v[, -c(1:f[2], (ncol(v) - f[2] + 1):ncol(v))]
        }
        v <- as.vector(t(v))
        out <- writeValues(out, v[-nc1], tr$row[i])
        pbStep(pb)
      }
      else {
        v <- getValues(x, row = 1, nrows = tr$nrows[1] + 
                         addr)
        if (dofun) {
          v <- .Call("_focal_fun", v, w, as.integer(c(tr$nrows[1] + 
                                                        addr, nc)), runfun, NAonly, e, NAOK = TRUE, 
                     PACKAGE = "raster")
        }
        else {
          v <- .Call("_focal_sum", v, w, as.integer(c(tr$nrows[1] + 
                                                        addr, nc)), narm, NAonly, domean = domean, 
                     NAOK = TRUE, PACKAGE = "raster")
        }
        out <- writeValues(out, v, 1)
        pbStep(pb)
        for (i in 2:(tr$n - 1)) {
          v <- getValues(x, row = tr$row[i] - addr, nrows = tr$nrows[i] + 
                           (2 * addr))
          if (dofun) {
            v <- .Call("_focal_fun", v, w, as.integer(c(tr$nrows[i] + 
                                                          (2 * addr), nc)), runfun, NAonly, e, NAOK = TRUE, 
                       PACKAGE = "raster")
          }
          else {
            v <- .Call("_focal_sum", v, w, as.integer(c(tr$nrows[i] + 
                                                          (2 * addr), nc)), narm, NAonly, domean = domean, 
                       NAOK = TRUE, PACKAGE = "raster")
          }
          out <- writeValues(out, v[-nc1], tr$row[i])
          pbStep(pb)
        }
        i <- tr$n
        v <- getValues(x, row = tr$row[i] - addr, nrows = tr$nrows[i] + 
                         addr)
        if (dofun) {
          v <- .Call("_focal_fun", v, w, as.integer(c(tr$nrows[i] + 
                                                        addr, nc)), runfun, NAonly, e, NAOK = TRUE, 
                     PACKAGE = "raster")
        }
        else {
          v <- .Call("_focal_sum", v, w, as.integer(c(tr$nrows[i] + 
                                                        addr, nc)), narm, NAonly, domean = domean, 
                     NAOK = TRUE, PACKAGE = "raster")
        }
        out <- writeValues(out, v[-nc1], tr$row[i])
        pbStep(pb)
      }
      out <- writeStop(out)
      pbClose(pb)
    }
    return(out)
  }
  .local(x, ...)
}
