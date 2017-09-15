#' @title Rolling average
#' @description ... which can also average truncated areas
#' @export

rollAvg <- function(dat, k = 3, align = "center", avg_trunc = T) {

  dat_rollavg <- rep(NA, length(dat))

  # Implementation of moving average with filling truncated areas
  # First value remains the same
  # Second to kth value is truncated, and averaged over values < k

  if (align == "left") {
    for(i in 1:(length(dat)-k+1) ) {
      start <- i
      end <- (i+k-1)
      dat_rollavg[i] <- mean(dat[start:end], na.rm = T)
    }

    if (avg_trunc) {
      for (i in (length(dat) - (k-1)):length(dat)) {
        dat_rollavg[i] <- mean(dat[i:length(dat_rollavg)], na.rm = T)
      }
    }

  }

  if (align == "center") {

    if (avg_trunc) {
      for (i in 1:floor(k/2)) {
        dat_rollavg[i] <- mean(dat[1:i], na.rm = T)
      }

      for (i in (length(dat) - (floor(k/2)-1)):length(dat)) {
        dat_rollavg[i] <- mean(dat[(i-1):length(dat)], na.rm = T)
      }
    }

    from <- ceiling(k/2)
    to <- length(dat) - floor(k/2)
    for(i in from:to) {
      start <- i - from + 1
      end <- start + k - 1
      dat_rollavg[i] <- mean(dat[start:end], na.rm = T)
    }



  }

  if (align == "right") {
    for(i in k:length(dat) ) {
      start <- (i-k+1)
      end <- i
      dat_rollavg[i] <- mean(dat[start:end], na.rm = T)
    }

    if (avg_trunc) {
      for (i in 1:(k-1)) {
        dat_rollavg[i] <- mean(dat[i:1], na.rm = T)
      }
    }

  }

  return(dat_rollavg)
}
