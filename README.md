Title: "Capstone Project_Cyclistic-bike-share-analysis-case-study"
Author: "Harrison Osiezagha"
Date: '2022-07-06'


##INTRODUCTION

I am a junior data analyst working in the marketing analyst team at Cyclistic, a bike-share company in Chicago.
In 2016, Cyclistic launched a successful bike-share offering. Since then, the program has grown to a fleet of 5,824 bicycles that are geotracked and locked into a network of 692 stations across Chicago. The bikes can be unlocked from one station and returned to any other station in the system anytime.

Cyclistic’s marketing strategy relied on building general awareness and appealing to broad consumer segments.One approach that helped make these things possible was the flexibility of its pricing plans: single-ride passes, full-day passes,
and annual memberships. Customers who purchase single-ride or full-day passes are referred to as casual riders. Customers who purchase annual memberships are Cyclistic members.

##ASK

In order to increase revenue, the company wants to come up with a new marketing strategy to convert casual riders into annual members. Therefore, I am tasked with analysing bike usage data to understand **how casual riders and annual members use Cyclistic bikes differently**, to help create a data driven marketing strategy.

Key stakeholders:

   *  Lily Moreno,the Director of marketing and my manager.
   *  The Cyclistic marketing analytics team(my teammates).
   *  The Cyclistic executive team.

   
##PREPARE

I used Cyclistic’s historical trip data to analyze and identify trends, which was downloaded [here] (link https://divvy-tripdata.s3.amazonaws.com/index.html). The data was made available by Motivate International Inc. under this [license] (https://www.divvybikes.com/data-license-agreement). Analysis for this case study was done using data from **Jan 2021 to Dec 2021**, which is stored in separate files for each month.
Files were saved in a folder(Capstone_project) for easy access. I was able to confirm that each sheet has the same number of columns having the same names.
For each sheet i created a column called “ride_length”, then Calculated the length of each ride by subtracting the column “started_at” from the column “ended_at” (for example, =D2-C2) and formatted as HH:MM:SS using Format > Cells > Time > 37:30:55.
A column called “day_of_week” was created and  i calculated the day of the week that each ride started using the “WEEKDAY” command (for example, =WEEKDAY(C2,1)) in each month file, then formatted as a number with no decimals, noting that 1 = Sunday and 7 = Saturday.
I deleted rows that had ride start time later than ride end time using "started_at > ended_at".

##ANALYZE

I imported the data into R and merged the 12 files into one data frame after loading the following packages.
```{r}
install.packages("tidyverse")
install.packages("plyr")
install.packages("dplyr")
install.packages("readr")
install.packages("ggplot2")
library(tidyverse)
library(plyr)
library(dplyr)
library(readr)
library(ggplot2)
```

```{r}
cyclist_df <- list.files(path='~/capstone_project', pattern = "*.csv", full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows
cyclist_df
```

The below shows a view of the newly created data frame
```{r}
head(cyclist_df)
```

The newly created extra column was removed
```{r}
cyclist_df <- cyclist_df[,-c(16)]
cyclist_df
```

The below packages were installed for further data clean up and analysis

```{r}
install.packages("here")
install.packages("skimr")
install.packages("janitor")
install.packages("tidyr")
install.packages("stringr")
install.packages("lubridate")
library(here)
library(skimr)
library(janitor)
library(tidyr)
library(stringr)
library(lubridate)
```

Rows with null values were removed and a new data frame was created.
```{r}
cyclist1_df <-na.omit(cyclist_df)
cyclist1_df
```

The below was used to check the overview of the data.

```{r}
colnames(cyclist1_df)
dim(cyclist1_df)
summary(cyclist1_df)
str(cyclist1_df)
skim_without_charts(cyclist1_df)
```

There are a few problems that needed to be fixed:
(1) The data can only be aggregated at the ride-level, which is too granular. Additional columns of data — such as day, month, year — that provide additional opportunities to aggregate the data.
(2) Verify the “ride_length” column for the entire data frame.

```{r}
cyclist1_df$date <- as.Date(cyclist1_df$started_at)
cyclist1_df$year <- format(as.Date(cyclist1_df$date), "%Y")
cyclist1_df$month <- format(as.Date(cyclist1_df$date), "%m")
cyclist1_df$day <- format(as.Date(cyclist1_df$date), "%d")
cyclist1_df$day_of_week <- format(as.Date(cyclist1_df$date), "%A")

cyclist1_df$ride_lenght <- difftime(cyclist1_df$ended_at,cyclist1_df$started_at)
cyclist1_df
```

Irrelevant columns were removed.
```{r}
cyclist1_df <- cyclist1_df[,-c(16:17)]
cyclist1_df
```
Descriptive analysis was done as shown below.
Analysis on ride_length (all figures in seconds).
```{r}
mean(cyclist1_df$ride_lenght)
median(cyclist1_df$ride_lenght)
max(cyclist1_df$ride_lenght)
```

Now, we compare members and casual users with aggregate feature.
```{r}
aggregate(cyclist1_df$ride_lenght ~ cyclist1_df$member_casual, FUN = mean)
aggregate(cyclist1_df$ride_lenght ~ cyclist1_df$member_casual, FUN = median)
aggregate(cyclist1_df$ride_lenght ~ cyclist1_df$member_casual, FUN = max)
```

Average ride time by each day for members vs casual users.
```{r}
aggregate(cyclist1_df$ride_lenght ~ cyclist1_df$member_casual + cyclist1_df$day_of_week, FUN = mean)
```

Next we ordered the days of the week, then checked the average ride time by each day for members vs casual users
```{r}
cyclist1_df$day_of_week <- ordered(cyclist1_df$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

cyclist1_df %>%
aggregate(ride_lenght ~ member_casual + day_of_week, FUN = mean)
```

##SHARE

I created plot in **R** using ggplot2 then exported data to **Tableau** for more visuals

Ride lenght by members for days of the week.
```{r}
class(cyclist1_df$ride_lenght) = "Numeric"
cyclist1_df
cyclist1_df %>%
  ggplot(aes(x = day_of_week, y = ride_lenght, fill = member_casual)) +
  geom_col(position = "dodge")
```
Below is a dashboard created using **Tableau** showing usage by members.
[Dashboard] (https://public.tableau.com/views/CAPSTONEPROJECT_16570229578750/Dashboard1?:language=en-US&:display_count=n&:origin=viz_share_link)

Save data to CSV.
```{r}
getwd()
setwd("~/capstone_project")
write.csv(cyclist1_df, file = "~/capstone_project/cyclist_year.csv", row.names = FALSE)
```


##ACT

Bases on insights from our analysis we can guide the marketing campaign to convert casual riders into annual members. This well help show *how casual riders and annual members use Cyclistic bikes differently*.

**Findings**:
* Casual riders prefer to take longer trips averaging more than twice from members, which could mean Casual riders used Cyclistic bikes for leisure.
* Casual riders could save more money in the long run by becoming a member instead of paying for rides based on trip duration.
* Casual riders often ride on weekends, have longer rides weekends whereas annual members use the program more over the week than on weekends, which could be an indication that annual members are using the bikes to commute to work.
* Casual riders preferred using docked bikes to other ride types like classic and electric bikes. Annual Members used classic bikes more than other types.
* Both members and casual users have same peak season day (Average ride lenght of members vs day of week chat), Saturday and Sunday.
* Weekends have the highest number of rides.

**Recommendation**:

* Develop a weekend voucher promo for members which can be achieved by Cyclistic collaborating with a sports store or fashion store. When members go to that store, they get a discount or other promo by showing their bicycle as a voucher.
* Casual users that take long rides could be offered a discount on becoming annual members when they reach a certain distance say for instance 20 miles should be offered a discount.
* A weekend only membership that costs less than the current 7-day membership could be introduced.
