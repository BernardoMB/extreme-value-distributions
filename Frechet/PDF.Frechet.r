# This R script will plot the probability distribution function (PDF) of the Frechet distribution with various location and scale parameters.

# Ploting method 1

library(ggplot2)
library(gridExtra)

# PDF Frechet distribution
pdf.frechet <- function(x, form = 1, location = 0, scale = 1) {
  f <- form
  l <- location
  s <- scale
  (f / s) * (((x - l) / s) ^ ( - f - 1)) * exp( - ((x - l) / s) ^ ( - f))
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
labels <- labs(x = 'x', y = 'f(x) : Frechet PDF')
# STANDARD PDF:
# Color
bw <- FALSE
if (bw) {
  color <- 'black'
} else {
  color <- 'green'
}
# Create PDF layer
pdf.plot <- stat_function(fun = pdf.frechet, color = color)
# Define limits on the graph
y.lim <- ylim(0, 0.6)
x.lim <- xlim(0, 6)
###################
# Display PDF plot without preserving scale on axes
p1 <- emptyGraph + labels + y.lim + x.lim + pdf.plot
p1
###################
# Display PDF plot preserving scale on axes
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
pdf1.plot <- stat_function(fun = pdf.frechet, args = list(form = 1, location = 0, scale = 1), color = colors[1], size = 1.1)
pdf2.plot <- stat_function(fun = pdf.frechet, args = list(form = 2, location = 0, scale = 1), color = colors[2], size = 1.3)
pdf3.plot <- stat_function(fun = pdf.frechet, args = list(form = 3, location = 0, scale = 1), color = colors[3], size = 1.5)
pdf4.plot <- stat_function(fun = pdf.frechet, args = list(form = 1, location = 0, scale = 2), color = colors[4], size = 0.5)
pdf5.plot <- stat_function(fun = pdf.frechet, args = list(form = 2, location = 0, scale = 2), color = colors[5], size = 0.7)
pdf6.plot <- stat_function(fun = pdf.frechet, args = list(form = 3, location = 0, scale = 2), color = colors[6], size = 0.9)
# Define limits on the graph
y.lim2 <- ylim(0, 1.2)
x.lim2 <- xlim(0, 6)
# Create anotations later
# Define anotations starting point in the graph
x0 <- 3
y0 <- 1
# Segments
segSize <- 1
vSep <- 0.06
seg1 <- annotate('segment', size = 1.5, x = x0, y = y0 - (vSep) * (1 - 1), xend = x0 + 0.5, yend = y0 - (vSep) * (1 - 1), colour = colors[3])
seg2 <- annotate('segment', size = 1.3, x = x0, y = y0 - (vSep) * (2 - 1), xend = x0 + 0.5, yend = y0 - (vSep) * (2 - 1), colour = colors[2])
seg3 <- annotate('segment', size = 1.1, x = x0, y = y0 - (vSep) * (3 - 1), xend = x0 + 0.5, yend = y0 - (vSep) * (3 - 1), colour = colors[1])
seg4 <- annotate('segment', size = 0.9, x = x0, y = y0 - (vSep) * (4 - 1), xend = x0 + 0.5, yend = y0 - (vSep) * (4 - 1), colour = colors[6])
seg5 <- annotate('segment', size = 0.7, x = x0, y = y0 - (vSep) * (5 - 1), xend = x0 + 0.5, yend = y0 - (vSep) * (5 - 1), colour = colors[5])
seg6 <- annotate('segment', size = 0.5, x = x0, y = y0 - (vSep) * (6 - 1), xend = x0 + 0.5, yend = y0 - (vSep) * (6 - 1), colour = colors[4])
# Text
txtSize <- 3
hSep <- 1.6
t1 <- as.character(expression(alpha == 1 ~ ',' ~ mu == 0 ~ ',' ~ theta == 1))
t2 <- as.character(expression(alpha == 2 ~ ',' ~ mu == 0 ~ ',' ~ theta == 1))
t3 <- as.character(expression(alpha == 3 ~ ',' ~ mu == 0 ~ ',' ~ theta == 1))
t4 <- as.character(expression(alpha == 1 ~ ',' ~ mu == 0 ~ ',' ~ theta == 2))
t5 <- as.character(expression(alpha == 2 ~ ',' ~ mu == 0 ~ ',' ~ theta == 2))
t6 <- as.character(expression(alpha == 3 ~ ',' ~ mu == 0 ~ ',' ~ theta == 2))
txt1 <- annotate('text', size = txtSize, x = x0 + hSep, y = y0 + 0.01 - (vSep) * (1 - 1), label = t1, parse = TRUE)
txt2 <- annotate('text', size = txtSize, x = x0 + hSep, y = y0 + 0.01 - (vSep) * (2 - 1), label = t2, parse = TRUE)
txt3 <- annotate('text', size = txtSize, x = x0 + hSep, y = y0 + 0.01 - (vSep) * (3 - 1), label = t3, parse = TRUE)
txt4 <- annotate('text', size = txtSize, x = x0 + hSep, y = y0 + 0.01 - (vSep) * (4 - 1), label = t4, parse = TRUE)
txt5 <- annotate('text', size = txtSize, x = x0 + hSep, y = y0 + 0.01 - (vSep) * (5 - 1), label = t5, parse = TRUE)
txt6 <- annotate('text', size = txtSize, x = x0 + hSep, y = y0 + 0.01 - (vSep) * (6 - 1), label = t6, parse = TRUE)
###################
# Display PDFs comparison plot without preserving the scale on axes
p3 <- emptyGraph + labels + y.lim2 + x.lim2 + pdf1.plot + pdf2.plot + pdf3.plot + pdf4.plot + pdf5.plot + pdf6.plot + seg1 + seg2 + seg3 + seg4 + seg5 + seg6 + txt1 + txt2 + txt3 + txt4 + txt5 + txt6
p3
###################
# Display PDFs comparison plot preserving scale on axes
p4 <- emptyGraph + labels + y.lim2 + x.lim2 + pdf1.plot + pdf2.plot + pdf3.plot + pdf4.plot + pdf5.plot + pdf6.plot + coord_fixed()
p4

p5 <- grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2, layout_matrix = rbind(c(1,3), c(2,4)))
p5

# Save in the working directory
#ggsave('plot.png', plot = p5, width = 10, height = 10)

