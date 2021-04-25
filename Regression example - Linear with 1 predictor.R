
# An example of simple Bayesian regression with RJags
# one numerical predictor, numerical response
#
# example data taken from the 'openintro' package or downloaded 
# from https://www.openintro.org/data/index.php?data=bdims

library(rjags)
library(ggplot2)

### PART 1. Get data and plot trend
  dat <- openintro::bdims[,c("hgt", "wgt")]
  plot(dat$hgt, dat$wgt)
  
  # model: Y[i] ~ N(M[i], s^2) where M[i] = a + bX[i]
  # priors: 
  # a ~ N(0, 200)
  # b ~ N(1, 0.5)
  # s ~ Unif(0, 20)
  
  large_N <- 100000

### PART 2. Approximate posterior distributions with RJags
  mod <- "model{
    for(i in 1:length(Y)) {
    Y[i] ~ dnorm(m[i], s^(-2))
    m[i] <- a + b * X[i]
    }
    
    a ~ dnorm(0, 200^(-2))
    b ~ dnorm(1, 0.5^(-2))
    s ~ dunif(0, 20)
    }"
  run <- jags.model(textConnection(mod),
                    data = list(X = dat$hgt, Y = dat$wgt),
                    inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 22))
  
  res <- coda.samples(model = run,
                       variable.names = c("a", "b", "s"),
                       n.iter = large_N)  
  
  # plot results and view trace stability
  plot(res)  

### PART 3. Extract posteriors, compare with priors
  chains <- data.frame(res[[1]][,c("a", "b", "s")])
  
  prior_a  <- data.frame(a = rnorm(large_N, mean = 0, sd = 200))
  prior_b  <- data.frame(b = rnorm(large_N, mean = 1, sd = 0.5))
  prior_s  <- data.frame(s = runif(large_N, min = 0, max = 20))
  
  ggplot(data = chains, aes(x = a)) + geom_density() + 
    geom_density(data = prior_a, aes(x = a))
  
  ggplot(data = chains, aes(x = b)) + geom_density() + 
    geom_density(data = prior_b, aes(x = b))
  
  ggplot(data = chains, aes(x = s)) + geom_density() + 
    geom_density(data = prior_s, aes(x = s))

### PART 4. Get posterior mean and 95% credible interval for a, b and s
  results <- summary(res)
  results
  
  posterior_mean_a <- results[["statistics"]]["a","Mean"]
  posterior_95CI_a <- results[["quantiles"]]["a", c("2.5%", "97.5%")]
  
  posterior_mean_b <- results[["statistics"]]["b","Mean"]
  posterior_95CI_b <- results[["quantiles"]]["b", c("2.5%", "97.5%")]
  # equation for trend is M[i] = -104 + 1.01 * X[i]
  
  posterior_mean_s <- results[["statistics"]]["s","Mean"]
  posterior_95CI_s <- results[["quantiles"]]["s", c("2.5%", "97.5%")]

  # visualize trend and data
  ggplot(dat, aes(x = hgt, y = wgt)) +  geom_point() +
    geom_abline(intercept = posterior_mean_a, 
                slope = posterior_mean_b,
                color = "red")

### PART 5. Prediction: estimate wgt of an individual with hgt = 180
  # use chains to get m first and then the predicted value
  chains$m_180 <- with(chains, a + b * 180)
  chains$predicted <- with(chains, rnorm(large_N, mean = m_180, sd = s))
  
  # predicted value of an individual 
  posterior_mean_a + posterior_mean_b * 180 
  # should be about the same as
  mean(chains$m_180) 
  mean(chains$predicted)
  
  # credible interval for m
  quantile(chains$m_180, c(0.025, 0.975))

  # credible interval for predicted value
  CI <- quantile(chains$predicted, c(0.025, 0.975))
  
  # plot prediction with 95% CI
  ggplot(dat, aes(x = hgt, y = wgt)) + geom_point(color = "darkgrey") +
    geom_abline(intercept = posterior_mean_a, 
                slope = posterior_mean_b,
                color = "red") +
    geom_segment(x = 180, xend = 180, y = CI[1], yend = CI[2], color = "red")