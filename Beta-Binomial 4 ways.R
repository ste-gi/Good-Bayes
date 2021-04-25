
### Beta Binomial model: get posterior in 4 ways 
# generative model: outcome ~ Binomial(N, p)
# prior: p ~ Beta(1, 1)
# observed value: x = 8

library(rjags)
library(ggplot2)
library(gridExtra)

# Example values
N <- 12
a <- 1
b <- 1
observed_outcome <- 8

# 1. Simulation
  paramet <- rbeta(10000000, a, b)
  outcome <- rbinom(10000000, N, paramet)
  poster1 <- data.frame(p = paramet[outcome == observed_outcome])
  
# 2. Calculation 
  grid <- seq(from = 0, to = 1, by = 0.000001)
  
  likelihood <- dbinom(observed_outcome, N, grid)
  prior <- dbeta(grid, a, b)
  
  poster2 <- data.frame(p = grid,
                        prop  = likelihood * prior)
  
# 3. Formula for posterior ~ Beta(a + observed_outcome, b + N - observed_outcome)
  poster3 <- data.frame(p = rbeta(10000000, a + observed_outcome, b + N - observed_outcome))

# 4. JAGS
  mod <- "model{X ~ dbin(p, n)
                p ~ dbeta(a, b)}"
  run <- jags.model(textConnection(mod),
                    data = list(a = a, b = b, n = N, X = observed_outcome),
                    inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 12)) 
  vals <- coda.samples(model = run, variable.names = c("p"), n.iter = 1000000)

  poster4 <- data.frame(p = as.vector(vals[[1]][,"p"]))

# Comparison of posteriors
  g1 <- ggplot(poster1, aes(x = p)) + geom_density()
  g2 <- ggplot(poster2, aes(x = p, y = prop)) + geom_line()
  g3 <- ggplot(poster3, aes(x = p)) + geom_density()
  g4 <- ggplot(poster4, aes(x = p)) + geom_density()
  
  grid.arrange(g1, g2, g3, g4)