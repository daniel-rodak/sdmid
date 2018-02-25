library(plotly)

nWaves <- 6

p <- function(x) {
  (sin(x) + 1) / (nWaves * 2 * pi)
}

d <- function(x) {
  ((1 - cos(x)) / 1.1 + x) / (nWaves * 2 * pi)
}

dInv <- function(x) {
  xVec <- seq(0, (nWaves * 2 * pi), length.out = 10000)
  dVec <- d(xVec)
  dInvSingle <- function(y) xVec[which.min(abs(dVec - y))]
  sapply(x, dInvSingle)
}

R <- dInv(sqrt(runif(10000)))
phi <- runif(10000, 0, 2*pi)
X <- R * cos(phi)
Y <- R * sin(phi)
ax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = TRUE
)
p <- plot_ly(x = ~X, y = ~Y, type = 'scatter', mode = 'markers',
             width = 900, height = 900,
             marker = list(size = 4,
                           color = 'rgba(174, 201, 110, .5)',
                           line = list(color = 'rgba(66, 76, 41, .7)',
                                       width = 1)))
p <- layout(p, xaxis = ax, yaxis = ax)
print(p)
