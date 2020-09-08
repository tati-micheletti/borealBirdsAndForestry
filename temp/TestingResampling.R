# Testing the resample function
library("raster")
library("quickPlot")
library("reproducible")
ras <- raster(ncol = 15, nrow = 15, 
              ext = new("Extent", xmin = -7.5, xmax = 7.5, ymin = -7.5, ymax = 7.5))
valRas <- c(
  # Column 1
  rep(c(
    rep(1, times = 5),
    rep(0, times = 5),
    rep(1, times = 3), rep(0, times = 2)), 
    times = 5),
  # Column 2
  rep(c(
    rep(1, times = 3), rep(0, times = 2),
    rep(1, times = 5),
    rep(0, times = 5)),
    times = 5),
  # Column 3
  rep(c(
    rep(0, times = 5),
    rep(1, times = 3), rep(0, times = 2),
    rep(1, times = 5)),
    times = 5)
)
  
ras <- setValues(x = ras, values = valRas)
ras2 <- raster(ncol = 3, nrow = 3, 
              ext = new("Extent", xmin = -7.5, xmax = 7.5, ymin = -7.5, ymax = 7.5))
# What resample outputs
rasRes <- resample(ras, ras2)
# What resample outputs
rasResNGB <- resample(ras, ras2, method = "ngb")
# What I would expect to be outputted
rasExp <- setValues(x = ras2, values = c(1, 0, 0.66667, 0.66667, 1, 0, 0, 0.66667, 1))
rasAgg <- aggregate(ras, fact = c(5, 5), expand = FALSE, method = 'bilinear')

clearPlot()
Plot(ras, title = "1x1 res")
Plot(rasRes, title = "1x1 resampled 5x5 billinear")
Plot(rasResNGB, title = "1x1 resampled 5x5 ngb")
Plot(rasExp, title = "1x1 expected 5x5")
Plot(rasAgg, title = "1x1 aggregated to 5x5")

rasAgg[]
rasExp[]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 

ras0 <- raster(ncol = 25, nrow = 25, 
              ext = new("Extent", xmin = -12.5, xmax = 12.5, 
                        ymin = -12.5, ymax = 12.5))
valRas <- c(
  # Row 1
  rep(c(
    rep(1, times = 8),
    rep(0, times = 8),
    rep(1, times = 4), rep(0, times = 5)), 
    times = 8),
  # Row 2
  rep(c(
    rep(1, times = 4), rep(0, times = 5),
    rep(1, times = 8),
    rep(0, times = 8)),
    times = 8),
  # Row 3
  rep(c(
    rep(0, times = 8),
    rep(1, times = 4), rep(0, times = 5),
    rep(1, times = 8)),
    times = 9)
)

valRas <- c(
  # Row 1
  rep(c(
    rep(1, times = 8),
    rep(0, times = 8),
    rep(1, times = 4), rep(0, times = 5)), 
    times = 8),
  # Row 2
  rep(c(
    rep(1, times = 4), rep(0, times = 4),
    rep(1, times = 8),
    rep(0, times = 9)),
    times = 8),
  # Row 3
  rep(c(
    rep(0, times = 8),
    rep(1, times = 4), rep(0, times = 4),
    rep(1, times = 9)),
    times = 9)
)

ras0 <- setValues(x = ras0, values = valRas)
ras2 <- raster(ncol = 3, nrow = 3)

# What resample outputs
rasRes <- resample(ras, ras2)
# What resample outputs
rasResNGB <- resample(ras, ras2, method = "ngb")
# What I would expect to be outputted
rasExp <- setValues(x = ras2, values = c(1, 0, 0.66667, 0.66667, 1, 0, 0, 0.66667, 1))
rasAgg <- aggregate(ras0, fact = c(8.3333, 8.3333), expand = FALSE, method = 'bilinear')

clearPlot()
Plot(ras, title = "1x1 res")
Plot(rasRes, title = "1x1 resampled 5x5 billinear")
Plot(rasResNGB, title = "1x1 resampled 5x5 ngb")
Plot(rasExp, title = "1x1 expected 5x5")
Plot(rasAgg, title = "1x1 aggregated to 5x5")
