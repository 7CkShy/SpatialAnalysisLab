library(terra)
library(dplyr)
library(sf)

data <- rast(
  "./data/annual_mean_tem/Mean_Temperature_Annual_21000BP-10BP_step10_size10.nc"
)

global_mean_fun <- function(x) {
  lat <- cos(init(x) * (pi / 180))
  weight <- lat * x
  global_mean <- global(lat) / global(weight)
  global_mean
}

local_slope_fun <- function(x) {
  n <- seq_along(x)
  m <- tryCatch(nlme::gls(x ~ n), error = function(e) NULL)
  if (is.null(m)) return(NA_real_)
  coef <- tryCatch(coef(m), error = function(e) NULL)
  if (is.null(coef) || length(coef) < 2) return(NA_real_)
  as.numeric(coef[2])
}

local_sd_fun <- function(x) {
  n <- seq_along(x)
  m <- tryCatch(nlme::gls(x ~ n), error = function(e) NULL)
  if (is.null(m)) return(NA_real_)
  res <- tryCatch(residuals(m), error = function(e) NULL)
  if (is.null(res)) return(NA_real_)
  as.numeric(sd(res, na.rm = TRUE))
}

slope_list <- list()

nl <- nlyr(data)
total_iters <- nl - 9
pb <- NULL
if (total_iters >= 1) {
  pb <- utils::txtProgressBar(min = 1, max = total_iters, style = 3)
  for (i in seq_len(total_iters)) {
    r <- data[[i:(i + 9)]]
    if (nlyr(r) != 10) {
      utils::setTxtProgressBar(pb, i)
      next
    }
    r_slope <- tryCatch(
      app(r, local_slope_fun, cores = 6), error = function(e) NULL
    )
    if (!is.null(r_slope)) slope_list[[length(slope_list) + 1]] <- r_slope
    utils::setTxtProgressBar(pb, i)
  }
  close(pb)
} else {
  # no windows of length 10 to process
}

if (length(slope_list) > 0) {
  slope_stack <- do.call(c, slope_list)
  if (!dir.exists("./result")) dir.create("./result")
  writeRaster(slope_stack, "./result/slope_stack.tif", overwrite = TRUE)
}