#medianincome
library(tidyr)
library(dplyr)
library(stringr)
library(data.table)

firgeocodes <- fir%>%filter(MARSYEAR == 2014)%>%select(MUNID, LT1NAME, SGC_CODE)

NHSCPT <- read.delim('~/OCIF/data/99-004-XWE2011001-101.tab', sep="\t", header = TRUE, strip.white = FALSE)%>%
  filter(Topic == "Income of households in 2010")
NHSCD <- read.delim('~/OCIF/data/99-004-XWE2011001-701.tab', sep="\t", header = TRUE, as.is = TRUE)%>%
  filter(Topic == "Income of households in 2010")%>%
  select(-CD_Name, -Geo_Type, Geo_Code = Geo_code)
ONNHSCSD <- read.delim('~/OCIF/data/99-004-XWE2011001-301-ONT.tab', sep="\t", header = TRUE, as.is = TRUE)%>%
  filter(Topic == "Income of households in 2010")%>%
  select(-CSD_type, -CD_Name, -CSD_Name)
NHSCMACA <- read.delim('~/OCIF/data/99-004-XWE2011001-201.tab', sep="\t", header = TRUE, as.is = TRUE)%>%
  filter(Topic == "Income of households in 2010")%>%
  select(-CMA_CA_Name, -Geo_Type)
ONNHSCT <- read.delim('~/OCIF/data/99-004-XWE2011001-401-ONT.tab', sep="\t", header = TRUE, as.is = TRUE)%>%
  filter(Topic == "Income of households in 2010")%>%
  select(-CMA_CA_Name, -CT_Name)
NHSFED2003 <- read.delim('~/OCIF/data/99-004-XWE2011001-501.tab', sep="\t", header = TRUE, as.is = TRUE)%>%
  filter(Topic == "Income of households in 2010")%>%
  select(-FED_Name)
NHSFED2013 <- read.delim('~/OCIF/data/99-004-XWE2011001-511.tab', sep="\t", header = TRUE, as.is = TRUE)%>%
  filter(Topic == "Income of households in 2010")%>%
  select(-FED_Name)
NHSHR2013 <- read.delim('~/OCIF/data/99-004-XWE2011001-1701.tab', sep="\t", header = TRUE, as.is = TRUE)%>%
  filter(Topic == "Income of households in 2010")%>%
  select(-HR_name, Prov_Name = Prov_name)

ONNHSIncomeData <- do.call("rbind", list(NHSCPT, NHSCD, ONNHSCSD, NHSCMACA, ONNHSCT, NHSFED2003, 
                                         NHSFED2013, NHSHR2013))

total.nhs <- ONNHSIncomeData%>%
  filter(grepl('^\\s{2}Median+', Characteristic))%>%
  mutate("SGC_CODE" = ifelse(grepl('\\d{7}', Geo_Code), as.numeric(gsub('^\\d{2}', '', Geo_Code)), Geo_Code),
         "MHHI2011" = Total)%>%
  filter(MHHI2011 > 0)%>%
  select(SGC_CODE, MHHI2011)

firMHHI <- left_join(firgeocodes, total.nhs, by = "SGC_CODE")