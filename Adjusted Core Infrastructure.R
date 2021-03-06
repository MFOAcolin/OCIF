library(dplyr)
library(tidyr)

download.file("https://efis.fma.csc.gov.on.ca/fir/fir.rda", "fir.rda")
load("fir.rda")

#load household Income Data
MOFIncomeData <- read.csv("./data/Median Household Income 2011 - MOF.csv")

OCIFeligible <- fir%>%filter(as.numeric(slc.02.41.01) <= 100000 | grepl("North", region))

CoreInfrastructure <- fir%>%
  filter(MARSYEAR >= 2012)%>%
  mutate("Roads.Paved" = slc.51.611.06,
         "Roads.Unpaved" = slc.51.612.06,
         "Roads.Bridges.Culverts" = slc.51.613.06,
         "Roadways.TORM" = slc.51.614.06,
         "Winter.Ctl" = slc.51.621.06,
         "Winter.Ctl.SW.PL" = slc.51.622.06,
         "Street.Lighting" = slc.51.650.06,
         "Wastewater.Coll.Con" = slc.51.811.06,
         "Wasetwater.Treat.Disp" = slc.51.812.06,
         "Urban.Storm.Sewer" = slc.51.821.06,
         "Rural.Storm.Sewer" = slc.51.822.06,
         "Water.Treat" = slc.51.831.06,
         "Water.Dist" = slc.51.832.06,
         "CI.Total"= Roads.Paved+Roads.Unpaved+Roads.Bridges.Culverts+
           Roadways.TORM+Winter.Ctl+Winter.Ctl.SW.PL+Street.Lighting+
           Wastewater.Coll.Con+Wasetwater.Treat.Disp+Urban.Storm.Sewer+
           Rural.Storm.Sewer+Water.Treat+Water.Dist)%>%
  select(MARSYEAR, MUNID,  MUNTIER, LT1NO,  CI.Total)%>%
  spread(MARSYEAR, CI.Total)

colnames(CoreInfrastructure) <- make.names(colnames(CoreInfrastructure))

UTCoreInfrastructure <- CoreInfrastructure%>%
  filter(MUNTIER =="UT")%>%
  mutate("UT.Total.Core.CI" = ifelse(X2014>X2015|is.na(X2015), X2014, X2015))%>%
  select(LT1NO, UT.Total.Core.CI)

CoreInfrastructure <- CoreInfrastructure%>%
  mutate("Highest.CI" = ifelse(X2014>X2015|is.na(X2015), X2014, X2015))

CoreInfrastructure <- left_join(CoreInfrastructure, UTCoreInfrastructure, by="LT1NO")%>%
  select(MUNID, MUNTIER, LT1NO, Highest.CI, UT.Total.Core.CI)

UTWeightedAssessment <- fir%>%
  filter(MARSYEAR >2011, MUNTIER == "UT")%>%
  mutate("weighted.cva" = slc.261.9199.02)%>%
  select(MARSYEAR, MUNID, MUNTIER, LT1NO, weighted.cva)%>%
  spread(MARSYEAR, weighted.cva)
  
colnames(UTWeightedAssessment) <- make.names(colnames(UTWeightedAssessment))

UTWeightedAssessment <- UTWeightedAssessment%>%
  mutate("UT.Weighted.CVA" = ifelse(!is.na(X2015)& X2015 != 0, X2015,
                                        ifelse(!is.na(X2014)& X2014 != 0, X2014,
                                               ifelse(!is.na(X2013)& X2013 !=0, X2013,
                                                      X2012))))%>%
  select(LT1NO, UT.Weighted.CVA)

WeightedAssessment <- fir%>%
  filter(MARSYEAR >2011)%>%
  mutate("weighted.cva" = slc.261.9199.02)%>%
  select(MARSYEAR, MUNID, MUNTIER, LT1NO, weighted.cva)%>%
  spread(MARSYEAR, weighted.cva)

colnames(WeightedAssessment) <- make.names(colnames(WeightedAssessment))

WeightedAssessment <- WeightedAssessment%>%
  mutate("Recent.Weighted.CVA" = ifelse(!is.na(X2015)& X2015 != 0, X2015,
                                        ifelse(!is.na(X2014)& X2014 != 0, X2014,
                                               ifelse(!is.na(X2013)& X2013 !=0, X2013,
                                                      X2012))))%>%
  select(MUNID, MUNTIER, LT1NO, Recent.Weighted.CVA)
  
WeightedAssessment <- left_join(WeightedAssessment, UTWeightedAssessment, 
                                 by = "LT1NO")%>%
  mutate("LTUTAssessmentRatio" = Recent.Weighted.CVA/UT.Weighted.CVA)

AdjustedCoreInfrastructure <- left_join(CoreInfrastructure, WeightedAssessment,
                                        by = c("MUNID", "MUNTIER", "LT1NO"))%>%
  mutate("adjusted.core.infrastructure" = ifelse(!is.na(UT.Total.Core.CI)&!MUNTIER=="UT",
    Highest.CI +(LTUTAssessmentRatio*UT.Total.Core.CI), Highest.CI),
    "Indicator1" = adjusted.core.infrastructure/Recent.Weighted.CVA,
    "min" = min(Indicator1, na.rm = TRUE),
    "median" = median(Indicator1, na.rm = TRUE),
    "max" = max(Indicator1, na.rm = TRUE),
    "Weighted.Indicator1" = ifelse(Indicator1 > median, (Indicator1 - median)/(max-median), 
                                   (Indicator1-median)/(median-min))
  )

AdjustedCoreInfrastructure <- left_join(AdjustedCoreInfrastructure, MOFIncomeData, by = "MUNID")

Households <- fir%>%filter(MARSYEAR == 2014)%>%
  select(MUNID, "households" = as.numeric(slc.02.40.01))


AdjustedCoreInfrastructure <- left_join(AdjustedCoreInfrastructure, Households, by = "MUNID")%>%
  mutate("Indicator2" = (adjusted.core.infrastructure/households)/MEDIAN.HOUSEHOLD.INCOME..FINANCE.2011....,
         "Weighted.Indicator2" =  ifelse(Indicator2 > median(Indicator2), 
                                           (Indicator2-median(Indicator2))/(max(Indicator2)-Indicator2),
                                           (Indicator2-median(Indicator2))/(Indicator2 - min(Indicator2))
                                           ),
         "Infrastructure.Index" = (Weighted.Indicator1 + Weighted.Indicator2)/2
         )

#### $$$ Calculations

Dollars <- AdjustedCoreInfrastructure%>%
  mutate("funding.per.100k" = (394+24*(Infrastructure.Index-median(Infrastructure.Index)))*1.1,
         "Total.Funding" = ifelse((funding.per.100k*Highest.CI/100000) > 50000, funding.per.100k*Highest.CI/100000, 50000))%>%
  select(MUNID, LT1NAME, funding.per.100k, Total.Funding)
