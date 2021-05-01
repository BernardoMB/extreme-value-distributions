library(ggplot)


# n: Number of trials
n <- 30
# p: Probability of success
p <- 1/2

## Code for PMF
sup <- seq(0,n,1)
probs <- dbinom(sup, size = n, prob = p)
graph <- ggplot(data = data.frame(x = sup, y = probs, yend = rep(0,(n+1))), aes(x = x, y = y, xend = x, yend = yend)) +
  geom_point() +
  geom_segment() +
  scale_x_continuous(name="\nValue of X",
                     breaks=0:(n+1),
                     limits = c(0, n+1)) +
  scale_y_continuous(name="Probability\n",
                     limits = c(0.0,0.2)) +
  ggtitle("PMF for discrete random variable X\n") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 15))
graph

# Create CDF layer # Normal aproximation
pdf.normal <- function(x, mean = n*p, sd = sqrt(n*p*(1-p))) {
  dnorm(x, mean = mean, sd = sd)
}
pdf.plot.normal <- stat_function(fun = pdf.normal, color = 'blue')
graph <- graph + pdf.plot.normal
graph

# Create CDF layer # Normal aproximation
pdf.normal.correction <- function(x, mean = n*p, sd = sqrt(n*p*(1-p))) {
  dnorm(x - 0.5, mean = mean, sd = sd)
}
pdf.plot.normal.correction <- stat_function(fun = pdf.normal.correction, color = 'green')
graph <- graph + pdf.plot.normal.correction
graph

