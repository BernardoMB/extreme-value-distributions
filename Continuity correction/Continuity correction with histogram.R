library(dplyr)
library(ggplot2)

# n: Number of trials
n <- 30
# p: Probability of success
p <- 1/2

## Code for PMF
sup <- seq(0,n,1)
pmf <- dbinom(sup, size = n, prob = p)
data <- data.frame(sup, pmf, yend = rep(0,(n+1)))

graph <- ggplot(data, aes(x = sup, y = pmf)) +
  geom_col(stat="identity", color="black", fill="white") +
  geom_point() +
  geom_segment(aes(xend = sup, yend = yend), color="grey", linetype="dashed") +
  geom_text(
    aes(label = round(pmf,3), y = pmf + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust =  0
  ) +
  scale_x_continuous(name="\nValue of X",
                     breaks=0:n,
                     limits = c(0, n)) +
  labs(title = "Probability of X = x successes.",
       x = "Successes (x)",
       y = "probability") +
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
pdf.plot.normal.correction <- stat_function(fun = pdf.normal.correction, color = 'red', linetype = "dashed")
graph <- graph + pdf.plot.normal.correction
graph



#binoms <- rbinom(10000, n, p)
#qplot(binoms, geom="histogram") + pdf.plot.normal
