# This R script will plot the probability distribution function (PDF) of the Gumbel distribution with various location and scale parameters.

# Ploting method 2

library(ggplot2)
library(gridExtra)

# PDF Gumbel distribution
pdf.gumbel <- function(x, location = 0, scale = 1) {
  l <- location
  s <- scale
  (1 / s) * exp( - exp( - (x - l) / s) - (x - l) / s)
}

# Create a plot
plot <- ggplot(mapping = aes(0))
# Insert axes to make it look nicer
x.axis <- geom_hline(yintercept = 0, size = 0.001)
y.axis <-geom_vline(xintercept = 0, size = 0.001)
# Get rid of the grey background
theme <- theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank()
)
# Create an empty graph
emptyGraph <- plot + x.axis + y.axis + theme
# Plot empty graph
emptyGraph

# Define labels on the graph
labels <- labs(x = 'x', y = 'f(x) : Gumbel PDF')

# STANDARD CDF:
# Color
bw <- FALSE
if (bw) {
  color <- 'black'
} else {
  color <- 'green'
}
# Create PDF layer
pdf.plot <- stat_function(fun = pdf.gumbel, color = color)
# Define limits on the graph
y.lim <- ylim(0, 0.4)
x.lim <- xlim(-6, 6)
###################
# Display PDF plot without preserving scale
p1 <- emptyGraph + labels + y.lim + x.lim + pdf.plot
p1
###################
# Display PDF plot preserving scale
p2 <- emptyGraph + labels + y.lim + x.lim + pdf.plot + coord_fixed()
p2


# PDFs COMPARISON:
# Colors
if (bw) {
  colors <- c('black', 'black', 'black', 'black', 'black', 'black')
} else {
  colors <- c('goldenrod1', 'darkorange2', 'darkorange4', 'pink', 'deeppink1', 'purple')
}
# Create layers
pdf.plot1 <- stat_function(fun = pdf.gumbel, color = colors[1])
pdf.plot2 <- stat_function(fun = pdf.gumbel, args = list(location = 0.5, scale = 2), color = colors[2])
pdf.plot3 <- stat_function(fun = pdf.gumbel, args = list(location = 1, scale = 2), color = colors[3])
pdf.plot4 <- stat_function(fun = pdf.gumbel, args = list(location = 1.5, scale = 3), color = colors[4])
pdf.plot5 <- stat_function(fun = pdf.gumbel, args = list(location = 3, scale = 4), color = colors[5])
# Annotations
a1 <- annotate('text', x = -2, y = 0.3, label = 'u=0, s=1') 
a2 <- annotate('text', x = -3, y = 0.1, label = 'u=0.5, s=2') 
a3 <- annotate('text', x = 3, y = 0.18, label = 'u=1, s=2') 
a4 <- annotate('text', x = 1, y = 0.14, label = 'u=1.5, s=3') 
a5 <- annotate('text', x = 1.2, y = 0.05, label = 'u=3, s=4') 
###################
# Display PDFs comparison plot without preserving the scale on axis
p3 <- emptyGraph + labels + y.lim + x.lim + pdf.plot1 + pdf.plot2 + pdf.plot3 + pdf.plot4 + pdf.plot5 + a1 + a2 + a3 + a4 + a5
p3
###################
# Display PDFs comparison plot preserving the scale on axis
p4 <- emptyGraph + labels + y.lim + x.lim + pdf.plot1 + pdf.plot2 + pdf.plot3 + pdf.plot4 + pdf.plot5 + coord_fixed()
p4

p5 <- grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2, layout_matrix = rbind(c(1,3), c(2,4)))
p5

# Save in the working directory
#ggsave('plot.png', plot = p5, width = 10, height = 10)
