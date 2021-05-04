library(ggplot2)
library(grid)
library(gridExtra)

# Colors
bw <- FALSE
if (bw) {
  colors <- c('black', 'black', 'black', 'black', 'black', 'black') 
} else {
  colors <- c('goldenrod1', 'darkorange2', 'darkorange4', 'pink', 'deeppink1', 'purple')
}

# l: Expected value of Poisson Distribution
l <- 15
lim <- 40

# X intercept
x.intercept <- 18
vertical.line <- geom_vline(xintercept = x.intercept, linetype = 12, color = colors[1], size = 1)

# --- CDF ----
sup <- seq(0,lim,1)
cdf <- ppois(sup, lambda = l)
cdf.data <- data.frame(sup, cdf, xend = sup + 1, yend = cdf)

scaleFUN <- function(x) sprintf("%.2f", x)
cdf.plot <- ggplot(cdf.data, aes(x = sup, y = cdf)) + 
  geom_point() +
  geom_point(aes(x = sup + 1, y = cdf), shape = 21, fill = "white") +
  geom_segment(aes(x = sup, y = cdf, xend = xend, yend = yend)) +
  scale_x_continuous(breaks=0:lim, limits = c(0, lim)) +
  scale_y_continuous(labels=scaleFUN) +
  labs(x = "Value of x", y = "F(x)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 15), axis.title.x=element_blank())
graph <- cdf.plot
graph

# Create CDF layer # Normal aproximation
cdf.normal <- function(x, mean = l, sd = sqrt(l)) {
  pnorm(x, mean = mean, sd = sd)
}
cdf.plot.normal <- stat_function(fun = cdf.normal, color = colors[2])
graph <- graph + cdf.plot.normal
graph

# Create CDF layer # Normal aproximation with correction < x
cdf.normal.correction <- function(x, mean = l, sd = sqrt(l)) {
  pnorm(x - 0.5, mean = mean, sd = sd)
}
cdf.plot.normal.correction <- stat_function(fun = cdf.normal.correction, color = colors[3], linetype = "dashed", size = 1)
graph <- graph + cdf.plot.normal.correction
graph

# Create CDF layer # Normal aproximation with correction <= x
cdf.normal.correction.2 <- function(x, mean = l, sd = sqrt(l)) {
  pnorm(x + 0.5, mean = mean, sd = sd)
}
cdf.plot.normal.correction.2 <- stat_function(fun = cdf.normal.correction.2, color = colors[4], linetype = "dotdash", size = 1)
graph <- graph + cdf.plot.normal.correction.2
graph

graph <- graph + vertical.line
graph

# ---- PMF ----
sup <- seq(0,lim,1)
pmf <- dpois(sup, lambda = l)
pmf.data <- data.frame(sup, pmf, yend = rep(0,(lim+1)))

graph2 <- ggplot(pmf.data, aes(x = sup, y = pmf)) +
  geom_col(stat="identity", color="black", fill="white", width = 1, size = 1) +
  geom_point() +
  geom_segment(aes(xend = sup, yend = yend), color="grey", linetype="dashed") +
  geom_text(
    aes(label = round(pmf,3), y = pmf + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust =  0
  ) +
  scale_x_continuous(breaks=0:lim, limits = c(0, lim)) +
  scale_y_continuous(labels=scaleFUN) +
  labs(x = "Value of x",
       y = "P(x) vs. f(x)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 15))
graph2

# Create CDF layer # Normal aproximation
pdf.normal <- function(x, mean = l, sd = sqrt(l)) {
  dnorm(x, mean = mean, sd = sd)
}
pdf.plot.normal <- stat_function(fun = pdf.normal, color = colors[2])
graph2 <- graph2 + pdf.plot.normal
graph2

# Create CDF layer # Normal aproximation < x
pdf.normal.correction <- function(x, mean = l, sd = sqrt(l)) {
  dnorm(x - 0.5, mean = mean, sd = sd)
}
pdf.plot.normal.correction <- stat_function(fun = pdf.normal.correction, color = colors[3], linetype = "dashed", size = 1)
graph2 <- graph2 + pdf.plot.normal.correction
graph2

# Create CDF layer # Normal aproximation <= x
pdf.normal.correction.2 <- function(x, mean = l, sd = sqrt(l)) {
  dnorm(x + 0.5, mean = mean, sd = sd)
}
pdf.plot.normal.correction.2 <- stat_function(fun = pdf.normal.correction.2, color = colors[4], linetype = "dotdash", size = 1)
graph2 <- graph2 + pdf.plot.normal.correction.2
graph2

graph2 <- graph2 + vertical.line
graph2

# ---- Grid ----
grid.arrange(graph, graph2, nrow = 2, ncol = 1, layout_matrix = rbind(c(1), c(2)), top = textGrob("Poisson Distribution vs. Normal Approximation", gp = gpar(fontsize = 14)))

aprox.data.1 <- data.frame(
  sup, 
  ppois(sup-1, lambda = l), 
  cdf.normal(sup),
  abs(ppois(sup-1, lambda = l) - cdf.normal(sup)),
  cdf.normal.correction(sup),
  abs(ppois(sup-1, lambda = l) - cdf.normal.correction(sup))  
)
names(aprox.data.1) <- c('x', 'P(X<x)', 'P(N<x)', 'Err P(N<x)', 'P(N<x+0.5)', 'Err P(N<x+0.5)')
aprox.data.1

aprox.data.2 <- data.frame(
  sup, 
  ppois(sup, lambda = l), 
  cdf.normal(sup),
  abs(ppois(sup, lambda = l) - cdf.normal(sup)),
  cdf.normal.correction.2(sup),
  abs(ppois(sup, lambda = l) - cdf.normal.correction.2(sup))  
)
names(aprox.data.2) <- c('x', 'P(X<=x)', 'P(N<=x)', 'Err P(N<=x)', 'P(N<=x+0.5)', 'Err P(N<x+0.5)')
aprox.data.2


