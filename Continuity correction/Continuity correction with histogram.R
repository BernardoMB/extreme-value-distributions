library(dplyr)
library(ggplot2)

# n: Number of trials
n <- 30
# p: Probability of success
p <- 1/2

  graph <- ggplot(data.frame(heads = 0:n, pmf = dbinom(x = 0:n, size = n, prob = p)),
         aes(x = factor(heads), y = pmf)) +
  geom_col() +
  geom_text(
    aes(label = round(pmf,2), y = pmf + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = 0
  ) +
  labs(title = "Probability of X = x successes.",
       x = "Successes (x)",
       y = "probability") 
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