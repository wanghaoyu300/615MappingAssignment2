library(readr)
library(tidyverse)
library(magrittr)
library(lubridate)

PublicAssistanceFundedProjectsDetails <- read_csv("PublicAssistanceFundedProjectsDetails.csv")

PAF <- PublicAssistanceFundedProjectsDetails
PAF_hurricane <- PAF %>% filter(incidentType == "Hurricane")
PAF_hurricane$declarationDate %<>% year()
PAF_hurricane %<>% filter(declarationDate >2008 & declarationDate < 2019 )
PAF_hurricane$stateNumberCode %<>% formatC(width = 2, flag = 0)
PAF_hurricane$countyCode %<>% formatC(width = 3, flag = 0)
PAF_hurricane %<>% mutate(fips = str_c(stateNumberCode, countyCode))
PAF_hurricane %<>% select(-c(3, 4, 5, 6, 8, 9, 10, 12, 14, 15, 16, 17, 20, 21, 22))
PAF_hurricane$obligatedDate %<>% round_date(unit = 'year')
PAF_hurricane$fips %<>% as.character()
PAF_hurricane %<>% .[-which(.$county == "Statewide"), ]
temp <- group_by(PAF_hurricane, fips)
display <- summarise(temp, state = unique(state),
                     Amount = mean(totalObligated))

display %<>% rename(GEO_ID = fips)

write.csv(PAF_hurricane, file = "PAF_hurricane.csv", row.names = F)
