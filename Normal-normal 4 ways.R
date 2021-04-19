
library(ggplot2)
library(gridExtra)
library(BEST)
library(rjags)


### Get and visualize some normally-distributed data 
dat <- data.frame(var = morley$Speed)
ggplot(dat, aes(x = var)) + geom_density()

# Normal-normal model for data: var ~ N(m, s^2)
# prior for m: m ~ N(a, b^2)
# prior for s: s ~ Unif(c, d)
a <- 850
b <- 150
c <- 0
d <- 300

# 1. simulation: generate a lot of draws from priors and simulate outcomes.
# select outcomes that are close to the observed data
  N <- 1000000
  m <- rnorm(N, mean = a, sd = b)
  s <- runif(N, min = c, max = d)
  
  outcomes <- replicate(n = nrow(dat), 
                        expr = rnorm(N, mean = m, sd = s), 
                        simplify = "vector")

  means <- apply(outcomes, 1, mean)
  sds <- apply(outcomes, 1, sd)
  
  mean_observed <- mean(dat$var)
  sd_observed <- sd(dat$var)
  
  ind <- (means < (mean_observed + 5) & 
            means > (mean_observed - 5) & 
            sds < (sd_observed + 2) & 
            sds > (sd_observed - 2))
  
  m_post <- data.frame(m = m[ind])

  m1 <- ggplot(data = m_post, aes(x = m)) + geom_density() +
    scale_x_continuous(limits = c(800, 900)) +
    scale_y_continuous(limits = c(0, 0.06))

# 2. calculation: use grid approximation and Bayes formula directly
    # priors
    grid <- expand.grid(m = seq(800, 900, by = 0.75),
                        s = seq(1, 300, by = 1))
    prior_m   <- dnorm(grid$m, mean = a, sd = b)
    prior_s   <- dunif(grid$s, min = c, max = d)
    grid$prior <- prior_m * prior_s
    
    # likelihood
    for(i in 1:nrow(grid)){
      res <- dnorm(dat$var, mean = grid$m[i], sd = grid$s[i])
      grid$likelihood[i] <- prod(res)
    }
    
    # posteriors
    grid$probability <- grid$prior * grid$likelihood
    grid$probability <- grid$probability / sum(grid$probability)
    
    # sample from posterior
    ind <- sample(1:nrow(grid),
                  size = N,
                  replace = TRUE,
                  prob = grid$probability)
    posterior_sample <- grid[ind, c("m", "s")]
    
    # distribution
    m2 <- ggplot(data = posterior_sample, aes(x = m)) + geom_density()+ 
      scale_x_continuous(limits = c(800, 900)) +
      scale_y_continuous(limits = c(0, 0.06))

# 3. BEST
  res <- BESTmcmc(dat$var, priors = list(muM = 850, muSD = 150, sigmaMode = 5, sigmaSD = 20))
  m3 <- ggplot(data = res, aes(x = mu)) + geom_density() + 
    scale_x_continuous(limits = c(800, 900)) +
    scale_y_continuous(limits = c(0, 0.06))

# 4. Jags
  mod <- "model{
      for (i in 1:length(Y)) {
          Y[i] ~ dnorm(m, s^(-2))}
      m ~ dnorm(850, 150^(-2))
      s ~ dunif(0, 300)
      }"
  
  com <- jags.model(textConnection(mod),
                    data = list(Y = dat$var))
  res <- coda.samples(model = com,
                      variable.names = c("m"),
                      n.iter = 100000)
  m_post <- data.frame(m = res[[1]][,"m"])

  m4 <- ggplot(data = m_post, aes(x = var1)) + geom_density() + 
    scale_x_continuous(limits = c(800, 900))+
    scale_y_continuous(limits = c(0, 0.06))

#  Comparison: compare posteriors obtained thru the 4 ways
grid.arrange(m1, m2, m3, m4, nrow = 4)


