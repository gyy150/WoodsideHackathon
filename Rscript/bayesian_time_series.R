setwd("C:/Users/young/Desktop/work/Woodside_Hackathon")

#HackathonData = read.table("Hackathon Data.csv", header=TRUE, sep = ",")
HackathonData <- (read.xlsx("HackathonS.xlsx", sheet = 1,startRow = 1, colNames = TRUE))[-1,]
Taiwan        <- (read.xlsx("HackathonS.xlsx", sheet=2, startRow = 1, colNames = TRUE))
Korea         <- (read.xlsx("HackathonS.xlsx", sheet=3, startRow = 1, colNames = TRUE))
Japan         <- (read.xlsx("HackathonS.xlsx", sheet=4, startRow = 1, colNames = TRUE))
China         <- (read.xlsx("HackathonS.xlsx", sheet=5, startRow = 1, colNames = TRUE))

TW_emplyment           = Taiwan[,1]
TW_exchange_rate       = Taiwan[,2]
TW_GDP                 = Taiwan[,3]
TW_gas_coal_price_diff = Taiwan[,4] - Taiwan[,5]
TW_primary_energy      = Taiwan[,7]

#to be forecasted 
TW_domestic_demand     = Taiwan[,6]

# The data (use NA for no data)
data = list(x1 = TW_emplyment, 
            x2 = TW_exchange_rate,
            x3 = TW_GDP,
            x4 = TW_gas_coal_price_diff,
            x5 = TW_primary_energy,
            
            y= TW_domestic_demand,
            N = 27)
# Variables to monitor
variable_names = c('beta0','beta1', 'beta2', 'beta3', 'beta4', 'beta5' ,'sigma','y_new')

# How many burn-in steps?
burn_in = 1000

# How many proper steps?
steps = 100000

# Thinning?
thin = 10

# Random number seed
seed = 1752204


model = "model
{
  beta0 ~ dnorm(0, 1/1000^2)
  beta1 ~ dnorm(0, 1/1000^2)
  beta2 ~ dnorm(0, 1/1000^2)
  beta3 ~ dnorm(0, 1/1000^2)
  beta4 ~ dnorm(0, 1/1000^2)
  beta5 ~ dnorm(0, 1/1000^2)
  
  log_sigma ~ dunif(-5, 5)
  sigma <- exp(log_sigma)

  for(i in 1:N)
  {
        mu[i] <- beta0 + beta1*x1[i] + beta2*x2[i] + beta3*x3[i] + beta4*x4[i] + beta5*x5[i]
        y[i]  ~  dnorm(mu[i], 1/sigma^2)
  }
  y_new ~ dnorm(beta0 + beta1 * 50 + beta2 * 50, 1/sigma^2)

}
"




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
quantile(results$sigma, probs=c(0.025,0.975))


mean(results$y_new)
quantile(results$y_new, probs=c(0.025,0.975))

#mean(results$y_new2)
#quantile(results$y_new2, probs=c(0.025,0.975))