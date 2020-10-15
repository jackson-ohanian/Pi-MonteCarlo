require("ggplot2")
require("gganimate")
require("gifski")
require("png")
require("gridExtra")
require("ggrepel")

theme_set(theme_bw())

############################################################################
### A function to generate n random values inside of (1,1) with indexing ###
############################################################################
monte.data <- function(n, scale = 1.0) {
  x <- abs(runif(n))
  y <- abs(runif(n))
  return(data.frame(x, y, z = (1:n)))
}
############################################################################
### A function that builds a series of PNGs representing   #################
### The data draw of random positions with relevance given #################
### To points inside the unit circle centered at (0.5, 0.5)#################
############################################################################
plot_data_drawing <- function(n, randoms) {
  plot <- ggplot(data = randoms, aes(x, y))
  val_location <-  rep(FALSE, n)
  colors <- rep("yellow", n)
  i = 1
  val_location = ((randoms$x - 0.5)^2 + (randoms$y - 0.5)^2 < 0.25 )
  colors[val_location] = "light blue"
  rel_alpha = 0.05 + 0.3*(val_location)
  plot.data <- plot +  labs(title = "Samples: {frame_time}") + geom_point(data = randoms, aes(group = seq_along(c(1:n))), size = 4, colour = colors, alpha = rel_alpha) + transition_time(z) + shadow_mark(past = TRUE, future = FALSE, alpha = rel_alpha) 
  return (plot.data)                                                                                                                                                    
}


##########################################################################################
### A function that uses a vector containing randomly placed points in a 1x1          ####
### Square to calculate Pi via the Monte Carlo method over a unit circle centered at  ####
### (0.5, 0.5)                                                                        ####
### In: randoms - a vector of random (x,y) coordinates in (0,1)(0,1)                  ####
###     n - the size of randoms                                                       ####                                               
### Out: An animated plot of the estimated value of Pi over the course of the Monte Carlo#
##########################################################################################
lineplot.pi <- function(randoms, n) {
  i = 0
  j = 0
  val_location <-  rep(FALSE, n)
  val_location = ((randoms$x - 0.5)^2 + (randoms$y - 0.5)^2 < 0.25 )
  pi.time <- runif((n/1000))
  while (i <= n) {
    j = j + 1
    i = i + n / 1000
    pi.time[j] = 4 *(sum(val_location[1:i], na.rm = TRUE) / i)
  }
  lineplot.data <- (data.frame(pi = pi.time, z = (1:(j))))

  plot_pi.line <- ggplot(data = lineplot.data, aes(z, pi), main = "Estimate of Pi", xlab = "# drawings", ylab = "pi hat" )
  anim_pi.line <- plot_pi.line + geom_line(col = "red", lwd = 2, alpha = 0.7) + transition_reveal(z) + geom_abline(intercept = 3.14159, slope = 0, col = "light blue", alpha = 0.3, lwd = 5) + geom_text_repel(aes(label=as.character(round(pi,5))))

  return(anim_pi.line)
}

### DRIVER ###

n <- 10000
randoms <- monte.data(n)
plot.drawing <- plot_data_drawing(n, randoms)
plot.pi <- lineplot.pi(randoms, n)

animate(plot.pi, renderer = gifski_renderer(), end_pause = 30)

#animate(plot.drawing, renderer = gifski_renderer(), end_pause = 30)


