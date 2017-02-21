setwd("C:/Users/young/Desktop/work/Woodside_Hackathon")


#HackathonData = read.table("Hackathon Data.csv", header=TRUE, sep = ",")
HackathonData <- (read.xlsx("HackathonS.xlsx", sheet = 1,startRow = 1, colNames = TRUE))[-1,]
Korea         <- (read.xlsx("HackathonS.xlsx", sheet=3, startRow = 1, colNames = TRUE))


employment              = Korea[,1]
exchange_rate          = Korea[,2]
GDP                    = Korea[,3]
population             = Korea[,6]
gas_coal_price_diff    = Korea[,7] - Korea[,8]

Nuclear_energy         = Korea[,24]

primary_energy         = Korea[,29]
Cooling_Degree_Days    = Korea[,31]
Heating_Degree_Days    = Korea[,30]

Degrees_Days           = Cooling_Degree_Days + Heating_Degree_Days 

domestic_demand     = Korea[,9]

#to be forecasted
LNG_import          = Korea[,10] 

#removing the seasonality in LNG_import
# a <- LNG_import
# 
# a_timeseries <- ts(a, frequency=4, start=c(2009,2))
# 
# plot.ts(a_timeseries)
# 
# a_timeseries_component <- decompose(a_timeseries)
# 
# plot(a_timeseries_component$trend)
# 
# a_adjusted <- a_timeseries - a_timeseries_component$seasonal
# 
# plot(a_adjusted)



plot(LNG_import ~ GDP)



fit    <- lm(     LNG_import ~ 
                    
#                  + employment 
#                  + exchange_rate 
                  + GDP 
#                   + population
#                  + domestic_demand
#                   +Nuclear_energy
#                  + gas_coal_price_diff 
#                  + primary_energy 
#                  + Cooling_Degree_Days
#                  + Heating_Degree_Days
                  + Degrees_Days
)


summary( fit   )
#residuals( fit   )

layout(matrix(c(1,2,3,4),2,2))

plot(fit)
