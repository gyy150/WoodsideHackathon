setwd("C:/Users/User/Desktop/Hackathon (MK)")
HackathonData = read.table("Hackathon Data.csv", header=TRUE, sep = ",")
JPHDD        = HackathonData[1:27,9][-1]
JPLNGImports = HackathonData[1:27,23][-1]
JPPop        = HackathonData[1:27,65][-1]
NROW(JPHDD)

JPLNGImports = as.numeric(gsub(",","", as.character(JPLNGImports)))
JPPop = as.numeric(gsub(",","", as.character(JPPop)))
JPHDD = as.numeric(gsub(",","", as.character(JPHDD)))

TestLT1 <- lm(JPLNGImports ~ JPPop + JPHDD)
summary(TestLT1)
plot(resid(TestLT1))


model = "model
{
  beta0 ~ dnorm(0, 1/1000^2)
  beta1 ~ dnorm(0, 1/1000^2)
  beta2 ~ dnorm(0, 1/1000^2)
  log_sigma ~ dunif(-10, 10)
  sigma <- exp(log_sigma)
  for(i in 1:N)
  {
  y[i] ~ dnorm(beta0 + beta1*x[i] + beta2*z[i], 1/sigma^2)
  
  }
  y_new ~ dnorm(beta0 + beta1 * 1026 + beta2 * 126890, 1/sigma^2)
  }
  "
  
  # The data (use NA for no data)
  data = list(x = JPHDD, 
              y = JPLNGImports,
              z = JPPop,
              N = 26)
  # Variables to monitor
  variable_names = c('beta0','beta1', 'beta2', 'sigma','y_new')
  
  # How many burn-in steps?
  burn_in = 1000
  
  # How many proper steps?
  steps = 100000
  
  # Thinning?
  thin = 10
  
  # Random number seed
  seed = 10000
  
  
  # NO NEED TO EDIT PAST HERE!!!
  # Just run it all and use the results list.
  
  library('rjags')
  
  # Write model out to file
  fileConn=file("model.temp")
  writeLines(model, fileConn)
  close(fileConn)
  
  if(all(is.na(data)))
  {
    m = jags.model(file="model.temp", inits=list(.RNG.seed=seed, .RNG.name="base::Mersenne-Twister"))
  } else
  {
    m = jags.model(file="model.temp", data=data, inits=list(.RNG.seed=seed, .RNG.name="base::Mersenne-Twister"))
  }
  update(m, burn_in)
  draw = jags.samples(m, steps, thin=thin, variable.names = variable_names)
  # Convert to a list
  make_list <- function(draw)
  {
    results = list()
    for(name in names(draw))
    {
      # Extract "chain 1"
      results[[name]] = as.array(draw[[name]][,,1])
      
      # Transpose 2D arrays
      if(length(dim(results[[name]])) == 2)
        results[[name]] = t(results[[name]])
    }
    return(results)
  }
  results = make_list(draw)
  
  mean(results$beta0)
  sd(results$beta0)
  mean(results$beta1)
  sd(results$beta1)
  mean(results$beta2)
  sd(results$beta2)
  mean(results$sigma)
  sd(results$sigma)
  
  
  quantile(results$beta0, probs=c(0.025,0.975))
  quantile(results$beta1, probs=c(0.025,0.975))
  quantile(results$beta2, probs=c(0.025,0.975))
  quantile(results$sigma, probs=c(0.025,0.975))
  
  
  hist(results$y_new)
  mean(results$y_new)
  quantile(results$y_new, probs=c(0.025,0.975))
  
  summary(results)
  
  
