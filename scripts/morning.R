library(ggplot2)

# ## Grid method

x <- seq(0, 3)
y <- c(1, 3, 3, 1)
plot(y ~ x)
y <- dbinom(x, 3, 0.5)
ggplot(data = NULL, aes(x, y)) +
  geom_point() +
  geom_line()

x <- 0:9
y <- dbinom(x, 9, 0.1)
ggplot(data = NULL, aes(x, y)) +
  geom_point() +
  geom_line()

likelihood <- dbinom(x, 9, 0.67)
ggplot(data = NULL, aes(x, likelihood)) +
  geom_point() +
  geom_line()

prior <- c(0, 1, 2, 3, 2, 1, 0.5, 0.2, 0.1, 0)
post <- likelihood * prior / sum(likelihood * prior)
ggplot(data = NULL, aes(x, post)) +
  geom_point() +
  geom_line() +
  geom_line(aes(x, prior), color = "red") +
  geom_line(aes(x, likelihood), color = "green")

# 6 dead people out of 9 patients
x <- seq(0, 1, length.out = 20)  # params space
prior <- rep(1, 20)
likelihood <- dbinom(6, size = 9, x)
post <- likelihood * prior / sum(likelihood * prior)
ggplot(data = NULL, aes(x, post)) +
  geom_line() +
  geom_line(aes(x, prior), color = "red") +
  geom_line(aes(x, likelihood), color = "green")

# Updating posterior 
# 1 patient out of 1 dies
likelihood <- dbinom(1, size = 1, prob = x)
post <- likelihood * prior / sum(likelihood * prior)
ggplot(data = NULL, aes(x, post)) +
  geom_line() +
  geom_line(aes(x, prior), color = "red") +
  geom_line(aes(x, likelihood), color = "green")

# We have another dead patient (likelihood remains same as in previous step)
prior <- post
post2 <- likelihood * prior / sum(likelihood * prior)
ggplot(data = NULL, aes(x, post2)) +
  geom_line() +
  geom_line(aes(x, prior), color = "red") +
  geom_line(aes(x, likelihood), color = "green")

# Third patient survives
likelihood <- dbinom(0, size = 1, prob = x)
prior <- post2
post3 <- likelihood * prior / sum(likelihood * prior)
ggplot(data = NULL, aes(x, post3)) +
  geom_line() +
  geom_line(aes(x, prior), color = "red") +
  geom_line(aes(x, likelihood), color = "green")


# Testing example  (covid-19)
# Probability of being covid-19 positive in case of positive test result
N <- 1000
p_h1 <- 5 / N # false positive rate
p_h2 <- 1 - p_h1
l_h1 <- 840 / N # sensitivity
l_h2 <- 5 / N

# First test
p_infected <- (p_h1 * l_h1) / (p_h1 * l_h1 + p_h2 * l_h2)
p_infected

# Second test
p_h1 <- p_infected
(p_h1 * l_h1) / (p_h1 * l_h1 + p_h2 * l_h2)
