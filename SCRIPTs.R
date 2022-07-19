library(readr)
library(tidyverse) 

library(readr)
library(tidyverse)
library(dplyr)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus) 
library(bs4Dash)
library(leaflet)
library(DT)
library(plotly)
library(ggplot2)
library(ggthemes)
library(leaflet)
library(geojsonio)
library(tidyjson)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(tm)
library(tidyverse)
library(tidyr)



mapd <- read_csv("md_all.csv")

colnames(mapd)
"state" "D_19"  "C_19"  "P_19"  "MP_19" "D_18"  "C_18"  "P_18"  "MP_18" "D_17"  "C_17"  "P_17"  "MP_17"

mapd_19 <- pivot_longer(mapd,
                      c("D_19","C_19","P_19","MP_19"),
                      names_to= "Average19", values_to= "2019")
mapd_18 <- pivot_longer(mapd,
                        c("D_18","C_18","P_18","MP_18"),
                        names_to= "Average18", 
                        values_to= "2018")
mapd_17 <- pivot_longer(mapd,
                       c("D_17","C_17","P_17","MP_17"),
                       names_to= "Average17", 
                       values_to= "2017")

mapdj <- full_join(mapd_19,mapd_18)
mapdj <- full_join(mapdj,mapd_17)
mapdl <- mapdj[,c(1,10,11,16,17,18,19)] 

combine(mapdl, Average = c("Average19","Average18","Average17"))

mapdl <- 
  
#bind_rows(mapd_19, mapd_18, mapd_17)
mapdl <- as.data.frame(mapdl) %>% 
  
  mapdl <- mapdl[,c(1,10,11,16,17)] 
mapdll=as.data.frame(mapdl, union[,mapdl$Average17,mapdl$Average19])
 view(mapdl)
subset(c("state","Average","2019","2018","2017"))


%>% 
pivot_longer(c("2019","2018","2017"), names_to= "Year", values_to= "Amount") 
  
cbind ("state","Average","2019","2018","2017")
mapd_19 %>% 
  full_join(mapd_18$`2018` , by= "state")


nhe_L <- as.data.frame(nhe_L)

######### ISAAC
insurance <- read_csv("insurance.csv")
gender <- insurance %>% distinct(sex) %>% pull(sex)
children <- insurance %>% distinct(children) %>%
  arrange(children, desc(children)) %>%
  pull(children)
insurance = insurance %>%
  mutate(bmi_label = case_when(bmi>=40~"morbidly obese",
                               bmi>=30~ "obese",
                               bmi>25~"overweight",
                               bmi>=18.5~"healthy",
                               bmi<18.5~"underweight"
  ))
bmi_label_list = c("morbidly obese","obese","overweight","healthy","underweight")
age <- insurance %>%
  mutate(age_range = case_when(age<=40~"18-40",
                               age>=41~"41-64"))

bmi_label_list = c("morbidly obese","obese","overweight","healthy","underweight")
age_list = c("18-40", "41-64")


write_csv(insurance, "insurance_cleaned.csv")
write_csv(age, "age.csv")
############ END of ISAAC





##### WC into Long######
wc <-   read_csv("wc19.csv")
wc %>% 
group_by(word=word)%>%
summarise(freq=sum(freq))


##### NHE into Long######
nhe <- read_csv("NHE.csv")
nhe$Medicare <- as.numeric(nhe$Medicare)
nhe$Medicaid <- as.numeric(nhe$Medicaid)
nhe_L <- pivot_longer(nhe,
             c("Total","Insurance", "Medicare", "Medicaid"),
             names_to= "X", values_to= "Y")
nhe_L <- as.data.frame(nhe_L)

write_csv("nhe_L", path = "nhe_long2.csv")

write_csv(nhe_L, "nhe_long.csv")
###### Codes #######
write_csv(old, "new name")

pivot_wider(data, names_from= Type, values_from= Value)

pivot_longer(data,
             c("col name", "col name", "col name", "", "", ""),
             names_to= "New col name for old", values_to= "cole name for old rows")
pivot_longer(,
             c("","", "", "", ""),
             names_to= "", values_to= "")

##################################
col_types  = cols(.default =)  
col_only
write_csv("NHE_long", path = "foo.csv")

bar <- read_csv("foo.csv", col_types = cols(x = col_factor()), col_names = T)