## Animation of Confidence Interval
SD <- 1
MEAN <- 0
N_S <- 100
N_CI <- 50


confidence_interval <- function(...) {
  m <- mean(rnorm(N_S, MEAN, SD))
  low <- m - 1.96 / sqrt(N_S)
  high <- m + 1.96 / sqrt(N_S)
  return(c(low, high))
}

CIS <- lapply(1:N_CI, confidence_interval)
library(animation)
ani.options(interval = .25)
saveGIF({
  for (i in 1:N_CI) {
    plot(
      0,
      xlim = c(1, N_CI),
      ylim = c(-0.6, 0.6),
      xlab = "Sample",
      ylab = "Confidence Interval",
      main = "Confidence Interval Animation"
    )
    abline(h = MEAN, col = "gray", lwd = 2)
    x = 1:i
    y1 = unlist(lapply(CIS[1:i], function(ci) ci[[1]]))
    y2 = unlist(lapply(CIS[1:i], function(ci) ci[[2]]))
    color = unlist(lapply(CIS[1:i], function(ci) {
      if (all(ci > MEAN) || all(ci < MEAN)) {
        return("red")
      }
      return("gray")
    }))
    segments(x, y1, x, y2, color, lwd = 3)
  }
},)