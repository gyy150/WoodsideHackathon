setwd("C:/Users/young/Desktop/work/Woodside_Hackathon")

#HackathonData = read.table("Hackathon Data.csv", header=TRUE, sep = ",")
HackathonData <- (read.xlsx("HackathonS.xlsx", sheet = 1,startRow = 1, colNames = TRUE))[-1,]
Korea         <- (read.xlsx("HackathonS.xlsx", sheet=3, startRow = 1, colNames = TRUE))

##################################################
##################################################
employment              = Korea[,1]
exchange_rate          = Korea[,2]
GDP                    = Korea[,3]
gas_coal_price_diff    = Korea[,7] - Korea[,8]
primary_energy         = Korea[,29]
Cooling_Degree_Days    = Korea[,31]
Heating_Degree_Days    = Korea[,30]

domestic_demand     = Korea[,9]

#to be forecasted
LNG_import          = Korea[,10] 

#########################################################################
#########################################################################

# The data (use NA for no data)
data = list(
  
  x1 = employment, 
  x2 = exchange_rate,
  x3 = GDP,
  x4 = gas_coal_price_diff,
  x5 = primary_energy,
  x6 = Cooling_Degree_Days,
  x7 = Heating_Degree_Days,
  x8 = domestic_demand,
  
  y  = LNG_import ,
  N = 27)

# Variables to monitor
variable_names = c(   
  'beta0',
  'beta1',
  'beta2',
  'beta3',
  'beta4',
  'beta5',
  'beta6',
  'beta7',
  'beta8',
  
  'sigma',
  'y_new')

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
  #  beta1 ~ dnorm(0, 1/1000^2)
  #  beta2 ~ dnorm(0, 1/1000^2)
  #  beta3 ~ dnorm(0, 1/1000^2)
  #  beta4 ~ dnorm(0, 1/1000^2)
  #  beta5 ~ dnorm(0, 1/1000^2)
  #  beta6 ~ dnorm(0, 1/1000^2)
     beta7 ~ dnorm(0, 1/1000^2)  
     beta8 ~ dnorm(0, 1/1000^2)
  
  log_sigma ~ dunif(-5, 5)
  sigma <- exp(log_sigma)
  
  for(i in 1:N)
  {
  mu[i] <-             beta0 
  #                  + beta1*x1[i] 
  #                  + beta2*x2[i] 
  #                  + beta3*x3[i]
  #                  + beta4*x4[i]
  #                  + beta5*x5[i] 
  #                  + beta6*x6[i] 
  #                  + beta7*x7[i]
                     + beta8*x8[i]
  y[i]  ~  dnorm(mu[i], 1/sigma^2)
  }
  
  
  ########################  currently using 2015 q4 value for testing
  #    employment_2016_q1           = 11304
  #   exchange_rate_2016_q1       = 33.132
  #   GDP_2016_q1                 = 93873
  #   gas_coal_price_diff_2016_q1 = -16.203
  #   primary_energy_2016_q1      = 113.522
  #    Cooling_Degree_Days_2016_q1    = 832
      domestic_demand               = 44
  ########################
  
  y_new ~ dnorm(
                         beta0 
  #                      + beta1 * employment_2016_q1
  #                      + beta2 * exchange_rate_2016_q1 
  #                      + beta3 * GDP_2016_q1 
  #                      + beta4 * gas_coal_price_diff_2016_q1 
  #                      + beta5 * primary_energy_2016_q1
  #                      + beta6 * Cooling_Degree_Days_2016_q1
                         + beta8 * domestic_demand


  ,   1/sigma^2)
  
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

mean(results$beta3)
sd(results$beta3)

mean(results$beta3)
sd(results$beta3)

mean(results$beta4)
sd(results$beta4)

mean(results$beta5)
sd(results$beta5)

mean(results$sigma)
sd(results$sigma)


quantile(results$beta0, probs=c(0.025,0.975))
quantile(results$beta1, probs=c(0.025,0.975))
quantile(results$beta2, probs=c(0.025,0.975))
quantile(results$beta3, probs=c(0.025,0.975))
quantile(results$beta4, probs=c(0.025,0.975))
quantile(results$beta5, probs=c(0.025,0.975))
quantile(results$sigma, probs=c(0.025,0.975))





mean(results$y_new)
quantile(results$y_new, probs=c(0.025,0.975))


# hist(results$y_new)
# mean(results$y_new)
# quantile(results$y_new, probs=c(0.025,0.975))
# 
# summary(results)

