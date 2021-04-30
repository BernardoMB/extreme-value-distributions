# This R script will plot the probability distribution function (PDF) of the Gumbel distribution with various location and scale parameters.

# Ploting method 3 (best method)

library(ggplot2)
library(gridExtra)

# PDF Gumbel Distribution
pdf.gumbel <- function(x, location = 0, scale = 1) {
  l <- location
  s <- scale
  (1 / s) * exp( - exp( - ((x - l) / s)) - ((x - l) / s))
}

# Create a plot
plot <- ggplot(mapping = aes(0))
# Insert axes to make it look nicer
x.axis <- geom_hline(yintercept = 0, size = 0.001)
y.axis <-geom_vline(xintercept = 0, size = 0.001)
# Get rid of the grey background
theme <- theme(
  panel.grid.major=element_blank(),
  panel.grid.minor=element_blank(),
  panel.background=element_blank(),
  # Legend boxes
  legend.key = element_rect(
    colour = 'white',
    fill = 'white',
    linetype = 'dashed',
    size = 0.5
  ),
  # Legend position
  #legend.position = c(0.25, 0.5)
  legend.position = 'top'
)
# Create an empty graph
emptyGraph <- plot + x.axis + y.axis + theme
# Plot empty graph
emptyGraph

# Define labels on the graph
labels <- labs(x = 'x', y = 'f(x) : Gumbel PDF')

# STANDARD PDF:
# Color
bw <- FALSE
if (bw) color = 'black' else color = 'greenyellow'
# Create PDF layer
pdf.plot <- stat_function(
  fun = pdf.gumbel,
  color = color
)
# Define limits on the graph
y.lim <- ylim(0, 0.4)
x.lim <- xlim(-2, 6)
###################
# Display PDF plot without preserving scale
p1 <- emptyGraph + labels + y.lim + x.lim + pdf.plot
p1
###################
# Display PDF plot preserving scale
p2 <- emptyGraph + labels + y.lim + x.lim + pdf.plot + coord_fixed()
p2

# PDFs COMPARISON:
# Colors and line types
if (bw) {
  colors <- c('black', 'black', 'black', 'black', 'black', 'black')
  sizes <- c(1.5, 1.3, 1.1, 0.9, 0.7, 0.5)
  linetypes <-c(1, 2, 3, 4, 5, 6)
} else {
  colors <- c('goldenrod1', 'darkorange2', 'darkorange4','pink', 'deeppink1', 'purple')
  # sizes <- c(1, 1, 1, 1, 1, 1)
  # linetypes <-c(1, 1, 1, 1, 1, 1)
  sizes <- c(1.5, 1.3, 1.1, 0.9, 0.7, 0.5)
  linetypes <-c(1, 2, 3, 4, 5, 6)
}
# Must define types of graphs in order for this method to work
types <- c('1', '2', '3', '4', '5', '6')
gplot <- ggplot(
  data = data.frame(type = types),
  aes(colour = type, linetype = type, size = type)
)
# Create layers
pdf.plot1 <- geom_path(
  data = data.frame(type = '1'),
  stat = 'function',
  fun = pdf.gumbel,
  args = list(location = 0, scale = 1)
)
pdf.plot2 <- stat_function(
  data = data.frame(type = '2'),
  fun = pdf.gumbel,
  args = list(location = 0, scale = 2)
)
pdf.plot3 <- stat_function(
  data = data.frame(type = '3'),
  fun = pdf.gumbel,
  args = list(location = 0.5, scale = 2)
)
pdf.plot4 <- stat_function(
  data = data.frame(type = '4'),
  fun = pdf.gumbel,
  args = list(location = 1, scale = 2)
)
pdf.plot5 <- stat_function(
  data = data.frame(type = '5'),
  fun = pdf.gumbel,
  args = list(location = 1.5, scale = 3)
)
pdf.plot6 <- stat_function(
  data = data.frame(type = '6'),
  fun = pdf.gumbel,
  args = list(location = 3, scale = 4)
)
# Legends
an <- c('\u03BC = 0; \u03B8 = 1',
        '\u03BC = 0; \u03B8 = 2',
        '\u03BC = 0.5; \u03B8 = 2',
        '\u03BC = 1; \u03B8 = 2',
        '\u03BC = 1.5; \u03B8 = 3',
        '\u03BC = 3; \u03B8 = 4')
###################
# DisplayPDFs comparison plot without preserving the scale on axis
p3 <- gplot + x.axis + y.axis + theme + labels + y.lim + x.lim + 
  pdf.plot1 + pdf.plot2 + pdf.plot3 + pdf.plot4 + pdf.plot5 + pdf.plot6 +
  scale_colour_manual(
    name = 'Functions', 
    values = colors,
    labels = an,
    breaks = types
  ) + 
  scale_linetype_manual(
    name = 'Functions',
    values = linetypes,
    labels = an,
    breaks = types
  ) +
  scale_size_manual(
    name = 'Functions',
    values = sizes,
    labels = an,
    breaks = types
  )
p3
###################
# Display CDFs comparison plot preserving the scale on axis
p4 <- p3 + coord_fixed() + theme(legend.position = 'none')
p4

grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2, layout_matrix = rbind(c(1,3), c(2,4)))

p5 <- grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2, layout_matrix = rbind(c(1,3), c(2,4)))
p5

# Save in the working directory
#ggsave('plot.png', plot = p5, width = 10, height = 10)
