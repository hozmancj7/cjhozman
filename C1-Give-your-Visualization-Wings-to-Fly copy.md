---
title: "Consulting Project"
author: "Camilo Hozman"
date: "May 29, 2022"
output:
  html_document:  
    keep_md: true
    toc: true
    toc_float: true
    code_folding: hide
    fig_height: 6
    fig_width: 12
    fig_align: 'center'
---


```r
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE, error = TRUE )
```


```r
# Use this R-Chunk to load all your libraries!
#install.packages("tidyverse") # run this line once in console to get package
library(tidyverse) 
library(mosaic)
library(DT)
library(dplyr)
library(nycflights13)
library(ggplot2)
library(dplyr)
library(pander)
library(gapminder)
library(hrbrthemes)
```

## Background

You just started your internship at a big firm in New York, and your manager gave 
you an extensive file of flights that departed JFK, LGA, or EWR in 2013. From 
this data (which you can obtain in R) your manager wants you to answer the 
following questions:

1. If I am leaving before noon, which two airlines do you recommend at each 
airport (JFK, LGA, EWR) that will have the lowest delay time at the 75th 
percentile?

2.Which origin airport is best to minimize my chances of a late arrival when 
I am using Delta Airlines?

3.Which destination airport is the worst airport for arrival time? You decide 
on the metric for “worst”.

## Data Wrangling
 I was able to filter the data to show the lowest delayed on each airport. As 
 well as the lowest arrival when using Delta airlines and the wost airport to 
 arrive into. 
 

```r
EWR <- filter( flights, sched_dep_time<= 1200, dep_time<= 1200, origin == "EWR")
EWR.1 <- EWR %>% 
  group_by(carrier) %>% 
  summarize( air = quantile(arr_delay, 0.75,na.rm = T)) %>% 
  mutate(origin = 'EWR') %>% 
  arrange(air) %>% 
  slice(1:2) 


JFK <- filter( flights, sched_dep_time<= 1200, dep_time<= 1200,origin == "JFK")
JFK.1 <- JFK %>% 
  group_by(carrier) %>% 
  summarize( air = quantile(arr_delay, 0.75,na.rm = T)) %>% 
  mutate(origin = 'JFK') %>% 
  arrange(air) %>% 
  slice(1:2) 
LGA <- filter( flights,sched_dep_time<= 1200, dep_time<= 1200, origin == "LGA")
LGA.1 <- LGA %>% 
  group_by(carrier) %>% 
  summarize( air = quantile(arr_delay, 0.75,na.rm = T)) %>% 
  mutate(origin = 'LGA') %>% 
  arrange(air) %>% 
  slice(1:2) 
 
Deltaflights <- filter(flights, carrier == "DL") %>% 
  mutate(status = if_else(arr_delay <=0,"On Time","Late"))




Worst_Arr_Delay <- filter(flights, arr_delay >= 600,)
Worst_Arr_Delay %>% 
arrange(desc(arr_delay))
```

```
## # A tibble: 39 × 19
##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
##  1  2013     1     9      641            900      1301     1242           1530
##  2  2013     6    15     1432           1935      1137     1607           2120
##  3  2013     1    10     1121           1635      1126     1239           1810
##  4  2013     9    20     1139           1845      1014     1457           2210
##  5  2013     7    22      845           1600      1005     1044           1815
##  6  2013     4    10     1100           1900       960     1342           2211
##  7  2013     3    17     2321            810       911      135           1020
##  8  2013     7    22     2257            759       898      121           1026
##  9  2013    12     5      756           1700       896     1058           2020
## 10  2013     5     3     1133           2055       878     1250           2215
## # … with 29 more rows, and 11 more variables: arr_delay <dbl>, carrier <chr>,
## #   flight <int>, tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>,
## #   distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>
```

```r
#max(Worst_Arr_Delay$arr_delay)
```


## Data Visualization
1.If I am leaving before noon, which two airlines do you recommend at each 
airport (JFK, LGA, EWR) that will have the lowest delay time at the 75th 
percentile? 


```r
airbind <- rbind(LGA.1,JFK.1,EWR.1)

   ggplot(airbind, aes(x=origin, y=air, size = carrier, color= carrier)) +
    geom_point(alpha=0.7)+
      labs(title = " Lowest Delay Time at the 75th 
Percentile ", x = "Airport",y = "Percentile") 
```

![](C1-Give-your-Visualization-Wings-to-Fly_files/figure-html/unnamed-chunk-2-1.png)<!-- -->
My suggestions of airlines vary among the three different airports. From Newark
Liberty International Airport I suggest to use Alaska Airlines (AS) and Virgin
America Airlines (VX).From John F. Kennedy Airport I recommend Delta (DL) and 
Virgin America Airlines (VX). Last, for La Guardia Airport I recommend American Airlines (AA) and Mesa Airline (YV).

2.Which origin airport is best to minimize my chances of a late arrival when I am using Delta Airlines?


```r
Deltaflights <- filter(flights, carrier == "DL", ) %>% 
  mutate(status = if_else(arr_delay <=0,"On Time","Late"))
Deltaflights%>% 
  ggplot(aes(x= origin,fill= status)) +
  geom_bar(position="fill") +
  ggtitle("Airports Chances of Late Arrival")
```

![](C1-Give-your-Visualization-Wings-to-Fly_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
new<- table(Deltaflights$origin)

new <- na.omit(Deltaflights)
```

The best airport minimize my chances of late arrival is John F. Kennedy (JFK).
Since, it represents the lowest red area covered.

3. Which destination airport is the worst airport for arrival time? You decide 
on the metric for “worst”.

I have decided to filter only the flights that were delay more than 10 hours 
and on which airport these flights landed at.


```r
ggplot(Worst_Arr_Delay, aes(x= dest, y= arr_delay/60, fill= dest)) +
  geom_bar(stat='identity')+
  labs(title = "Worst Airport for Arrival ",x = "Airports", y = "Arrival delay (hours)")+
  theme_classic()+
   theme_bw()
```

![](C1-Give-your-Visualization-Wings-to-Fly_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

According to the data the airport that reported the highest sum of  delayed hours
at a minimum delay time of 10 hours was The Atlanta Airport.

## Complexity of The Data


```r
Complx <- filter(flights, dest == c("ATL"),arr_delay >= 600)
ggplot(Complx, aes(x= month, y= dep_delay, color=carrier)) + 
    geom_point(size=2) +
  labs(title = "Delayed Flights that Landed in Atlanta Airport",x =  "Month", y = "Minutes")+
  theme_classic()+
   theme_bw()
```

![](C1-Give-your-Visualization-Wings-to-Fly_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

On the last graph, I was able to show the complexity of the data. From question number three we learned that among all the flights that reported a delay of 10 hours or more Atlanta had the highest amount of fights delayed in the United States. We further look into this and found that there were 5 total flights from Delta Airlines that had a delay or 10 or more hours. The total combined delayed of these flights was 63.05 hours or 3783 minutes.



















