setwd("C:/Users/young/Desktop/work/Woodside_Hackathon")


#HackathonData = read.table("Hackathon Data.csv", header=TRUE, sep = ",")
HackathonData <- (read.xlsx("HackathonS.xlsx", sheet = 1,startRow = 1, colNames = TRUE))[-1,]
Korea         <- (read.xlsx("HackathonS.xlsx", sheet=3, startRow = 1, colNames = TRUE))


TW_emplyment           = Korea[,1]
TW_exchange_rate       = Korea[,2]
TW_GDP                 = Korea[,3]
TW_gas_coal_price_diff = Korea[,4] - Taiwan[,5]
TW_primary_energy      = Korea[,7]
TW_Cooling_Degree_Days = Korea[,10]
TW_Heating_Degree_Days = Korea[,9]


#considered to be irrelevant 
TW_domestic_demand     = Korea[,6]

#to be forecasted
Korea_LNG_import          = Korea[,2] 


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