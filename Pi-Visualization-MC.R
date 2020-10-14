library("ggplot2")
library("gganimate")
library("gifski")
library("png")
library("gridExtra")

getwd()
monte_data <- function(n, scale = 1.0) {
  x <- abs(runif(n))
  y <- abs(runif(n))
  return(data.frame(x, y, z = (1:n)))
}

### A function that builds a series of PNGs representing 
### The data draw of random positions with relevance given
### To points inside the unit circle centered at (0.5, 0.5)
plot_data_drawing <- function(n, randoms) {
  plot <- ggplot(data = randoms, aes(x, y))
  val_location <-  rep(FALSE, n)
  colors <- rep("yellow", n)
  i = 1
  val_location = ((randoms$x - 0.5)^2 + (randoms$y - 0.5)^2 < 0.25 )
  colors[val_location] = "light blue"
  rel_alpha = 0.05 + 0.3*(val_location)
  plot.data <- plot + geom_point(data = randoms, aes(group = seq_along(c(1:n))), size = 4, colour = colors, alpha = rel_alpha) + transition_time(z) + shadow_mark(past = TRUE, future = FALSE, alpha = rel_alpha) 
  return (plot.data)                                                                                                                                                    
}


#######
## area = pi * r^2 
## area = l * w
## r = 0.5 
## l = 1
## proportion = pi / 1 -> proportion = pi
lineplot_pi <- function(randoms, n) {
  i = 0
  j = 0
  pi.time <- runif((n/1000))
  print(pi.time)
  while (i <= n) {
    j = j + 1
    i = i + n / 1000
    pi.time[j] = 4 *(sum(val_location[1:i], na.rm = TRUE) / i)
  }
  lineplot.data <- (data.frame(pi = pi.time, z = (1:(j))))
  plot(pi~z, data = lineplot.data)
  
  plot_pi.line <- ggplot(data = lineplot.data, aes(z, pi))
  anim_pi.line <- plot_pi.line + geom_line() + transition_reveal(z) + geom_abline(intercept = 3.145, slope = 0)
  return(anim_pi.line)
}

n <- 10000
randoms <- monte_data(n)
plot.drawing <- plot_data_drawing(n, randoms)
plot.pi <- lineplot_pi(randoms, n)
plot.combined <- grid.arrange(plot.drawing, plot.pi, ncol = 2)

animate(plot.combined, renderer = gifski_renderer())


