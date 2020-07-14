library(tidyverse)
library(lubridate)
library(timetk)
library(dplyr)
library(roll)   # For rolling average with roll_mean()
library(ggthemes)
library(pracma)     # for moving average or upload moving average from parent folder.
library(ggplot2)
library(zoo)        # for rolling sum 
library(gganimate)
library(emmeans)
library(plotly)
library(flexdashboard)               # install.packages("flexdashboard")
library(dimple)                      # devtools::install_github("Bart6114/dimple")


datachip<-read.csv("C:/COVID-19/Chip_valley/Chippewa County/Chippewa_modeling/COVID-19_chip.csv",header=T, na.strings=c(""," ","NA","  "), stringsAsFactors=F )

data1<- datachip %>% 
  separate(LoadDttm,c("Date", "Time"), sep=" ",remove = F, extra="drop") %>% 
  mutate(s_date=format(ymd(Date),"%m/%d")) %>% 
  separate(s_date,c("MONTH", "DAY"), sep="/",remove = F, extra="drop") %>% 
  mutate(POS_NEW=ifelse(POS_NEW==-1,0,.$POS_NEW)) %>%
  mutate(POS_PERC=POS_NEW/TEST_NEW) %>% 
  mutate(POS_PERC=round(POS_PERC,3)) %>% 
  filter(!is.na(POS_NEW), POS_NEW>=0, TEST_NEW>=0) %>% 
  mutate(POS_PERC_MEAN=movavg(POS_PERC,7, "s")) %>%       # Another way to compute moving average is to use function roll_mean instead of movavg()
  mutate(POS_PERC_MEAN1=movavg(POS_PERC,7, "e")) %>% 
  mutate(POS_NEW_MEAN=round(movavg(POS_NEW,7,"s"),0)) %>%   # Round the average to avoid dealing with decimal cases :)
  mutate(POS_PERC_MEAN2=movavg(POS_PERC,14, "s")) %>% 
  mutate(CUMUL=cumsum(POS_NEW)) %>% 
  mutate(MONTH=as.numeric(MONTH)) %>% 
  mutate(POS_MSUM0=rollsum(POS_NEW,7,fill=NA, align = "right")) %>% 
  mutate(POS_MSUM=rollsum(POS_NEW,14,fill=NA, align = "right")) %>%
  mutate(POS_MSUM_CODE=ifelse(POS_MSUM<=20, "Low",
                              ifelse(POS_MSUM>20 & POS_MSUM<=35, "Moderate",
                                     ifelse(POS_MSUM>35,"High",NA)))) %>% 
  mutate(BURD=(POS_MSUM*100000)/64658) %>% 
  mutate(BURD_CODE=ifelse(BURD<10,"Low",
                          ifelse(BURD>=10 & BURD<50, "Moderate",
                                 ifelse(BURD>=50&BURD<100,"Moderately high",
                                        ifelse(BURD>=100,"High",NA))))) %>% 
  # let try trajectory using log transformation 
  mutate(POS_MSUM0_NEW=log(POS_MSUM0, base=2)) %>% 
  mutate(POS_MSUM_NEW=log(POS_MSUM, base=2)) %>%
  mutate(CUMUL_NEW=log(CUMUL, base=2))



### I should add this to the metrics

ggplot(filter(data1,MONTH==06), aes(x=CUMUL_NEW, y=POS_MSUM0_NEW))+geom_line(size=2)+theme_economist()

p<-ggplot(filter(data1,MONTH==06), aes(x=CUMUL_NEW, y=POS_MSUM_NEW))+geom_line(size=2)+theme_economist()

ggplotly(p)

Jun<-data1 %>% 
  filter(MONTH==6) %>% 
  ggplot(., aes(x=s_date, y=POS_PERC_MEAN))+
  geom_bar(stat="identity", color="navyblue",size=1, fill="yellow")+
  geom_text(aes(label=scales::percent(POS_PERC_MEAN,accuracy = .1)), vjust=-0.7)+
  theme_hc()+
  scale_y_continuous(labels=scales::percent_format())+
  labs(x="Date", y="Simple moving average", title="COVID-19 positivity percent 7-day simple moving average - Month of June",
       subtitle=" ", 
       caption="Source: Wisconsin Department of Public Health", fill=" ")


Jun
ggplotly(Jun)

install.packages("flexdashboard")

aggregate(POS_PERC~as.factor(MONTH), data=data1,FUN=mean)

Avg<-data1 %>% 
  group_by(MONTH) %>% 
  filter(!is.na(POS_PERC)) %>% 
  summarize(Average=mean(POS_PERC), SEM=sd(POS_PERC), Minimum=min(POS_PERC), Maximum=max(POS_PERC)) %>% 
  mutate(Average=scales::percent(Average),SEM=scales::percent(SEM),Minimum=scales::percent(Minimum),Maximum=scales::percent(Maximum)) %>% 
  ungroup()

Avg
install.packages("DT")
library(DT)