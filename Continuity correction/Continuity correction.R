library(ggplot2)
library(gridExtra)

# n: Number of trials
n <- 25
# p: Probability of success
p <- 1/3

# Create CDF layer # Binomial distribution
dat <- data.frame(x = 1:n, y = pbinom(q = 0:(n-1), size = n, prob = p))
dat2 <- data.frame(x = 0:n, y = c(0, dat$y[-(n+1)]), xend = 1:(n+1), yend = c(0,dat$y[-(n+1)]))
cdf.plot.binomial <- ggplot(dat, aes(x = x, y = y)) + geom_point() +
        geom_segment(aes(x = x, y = y, xend = xend, yend = yend), data = dat2) +
        scale_x_continuous(breaks = seq(0,n,1), labels = 0:n) +
        scale_y_continuous(breaks = seq(0,1,0.1), labels = c(0,paste(".",1:9),1))
graph <- cdf.plot.binomial
graph

# Insert axes to make it look nicer
x.axis <- geom_hline(yintercept = 0, size = 0.001)
y.axis <-geom_vline(xintercept = 0, size = 0.001)
graph <- graph + x.axis + y.axis
graph

# Get rid of the grey background
theme <- theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank()
)
# Create an empty graph
graph <- graph + theme
# Plot empty graph
graph

# Define labels on the graph
labels <- labs(x = 'x', y = 'F(x) : Binomial CDF')
graph <- graph + labels
graph

# Create CDF layer # Normal aproximation
cdf.normal <- function(x, mean = n*p, sd = sqrt(n*p*(1-p))) {
  pnorm(x, mean = mean, sd = sd)
}
cdf.plot.normal <- stat_function(fun = cdf.normal, color = 'blue')
graph <- graph + cdf.plot.normal
graph

# Create CDF layer # Normal aproximation
cdf.normal.correction <- function(x, mean = n*p, sd = sqrt(n*p*(1-p))) {
  pnorm(x - 0.5, mean = mean, sd = sd)
}
cdf.plot.normal.correction <- stat_function(fun = cdf.normal.correction, color = 'green')
graph <- graph + cdf.plot.normal.correction
graph

# Define limits on the graph
y.lim <- ylim(0, 1)
x.lim <- xlim(0, n)
graph <- graph + y.lim + x.lim + scale_x_continuous(breaks = seq(0,n,1), labels = 0:n) +
  scale_y_continuous(breaks = seq(0,1,0.1), labels = c(0,paste(".",1:9),1))
graph

# Insert a horizontal line on y=1 because CDF
asinth.axis <- geom_hline(yintercept = 1, size = 0.01)
graph <- graph + asinth.axis
graph

vertical.line <- geom_vline(xintercept = 10, linetype = "dotted", color = "black", size = 1)
graph <- graph + vertical.line
graph
