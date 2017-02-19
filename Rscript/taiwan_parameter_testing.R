setwd("C:/Users/young/Desktop/work/Woodside_Hackathon")


#HackathonData = read.table("Hackathon Data.csv", header=TRUE, sep = ",")
HackathonData <- (read.xlsx("HackathonS.xlsx", sheet = 1,startRow = 1, colNames = TRUE))[-1,]
Taiwan        <- (read.xlsx("HackathonS.xlsx", sheet=2, startRow = 1, colNames = TRUE))


TW_emplyment           = Taiwan[,1]
TW_exchange_rate       = Taiwan[,2]
TW_GDP                 = Taiwan[,3]
TW_gas_coal_price_diff = Taiwan[,4] - Taiwan[,5]
TW_primary_energy      = Taiwan[,7]
TW_Cooling_Degree_Days = Taiwan[,10]
TW_Heating_Degree_Days = Taiwan[,9]


#considered to be irrelevant 
TW_domestic_demand     = Taiwan[,6]

#to be forecasted
TW_LNG_import          = Taiwan[,8] 


fit    <- lm(     TW_LNG_import ~ 
                    
                                + TW_emplyment 
#                                + TW_exchange_rate 
                                + TW_GDP 
#                                + TW_gas_coal_price_diff 
#                                + TW_primary_energy 
                                + TW_Cooling_Degree_Days
#                                + TW_Heating_Degree_Days
            )


summary( fit   )
#residuals( fit   )

layout(matrix(c(1,2,3,4),2,2))

plot(fit)
