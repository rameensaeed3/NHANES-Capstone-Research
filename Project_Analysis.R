library(foreign)
library(Hmisc)
library(ggplot2)
library(magrittr)
library(dplyr)
library(chron)
library(reshape2)

#load data
setwd("C:/Users/ramee/OneDrive/Documents/Research Project")

PA_HD_2013_14 <- readRDS("PA_HD_2013_14.rds")

PA_DAY_2013_14 <- readRDS("PA_DAY_2013_14.rds")

PA_HR_2013_14 <- readRDS("PA_HR_2013_14.rds")

DEMO_2013_14 <- readRDS("DEMO_2013_14.rds")

BODY_2013_14 <- readRDS("BODY_2013_14.rds")

CH_2013_14 <- readRDS("CH_2013_14.rds")

# Explore Data

str(PA_HD_2013_14)
summary(PA_HD_2013_14)

# demographics age & weight data-----------------------------------------------
Demo_13_14_Filtered <- DEMO_2013_14[,c("seqn","wtmec2yr","ridageyr","riagendr", "dmdeduc3", "dmdeduc2", "indhhin2")]

Demo_13_14_Filtered <- Demo_13_14_Filtered %>%
  mutate(agegroup = case_when(ridageyr<18 ~ "under 18",
                              ridageyr >= 18 & ridageyr <= 35 ~ "18-35",
                              ridageyr > 35 & ridageyr <= 50 ~ "36-50",
                              ridageyr > 50 & ridageyr <= 75 ~ "51-75",
                              ridageyr > 75 ~ "75+"))

Demo_13_14_Filtered$agegroup <- factor(Demo_13_14_Filtered$agegroup, levels = c("under 18", "18-35", "36-50", "12-21", "51-75", "75+"))
Demo_13_14_Filtered$seqn <- factor(Demo_13_14_Filtered$seqn)
Demo_13_14_Filtered$riagendr <- factor(Demo_13_14_Filtered$riagendr)
Demo_13_14_Filtered$dmdeduc3 <- factor(Demo_13_14_Filtered$dmdeduc3)
Demo_13_14_Filtered$dmdeduc2 <- factor(Demo_13_14_Filtered$dmdeduc2)
Demo_13_14_Filtered$indhhin2 <- factor(Demo_13_14_Filtered$indhhin2)

# Header Data--------------------------------------------------------------

#Filter to grab data where confirmed physical activity monitor data status
# & last day of wear is day 8 or 9

HD_Filter <- PA_HD_2013_14$paxsts == 1 & as.integer(PA_HD_2013_14$paxlday) >=8
HD_13_14_Filtered <- PA_HD_2013_14[HD_Filter,]

HD_13_14_Filtered$seqn <- factor(HD_13_14_Filtered$seqn)
HD_13_14_Filtered$paxsts <- factor(HD_13_14_Filtered$paxsts)
levels(HD_13_14_Filtered$paxfday) <- c("Sun", "Mon", "Tues", "Wed",
                                   "Thurs", "Fri", "Sat")
HD_13_14_Filtered$paxsenid <- NULL


summary(HD_13_14_Filtered)
str(HD_13_14_Filtered)

#Header visualization

ggplot(data=HD_13_14_Filtered, aes(x=paxfday)) + 
  geom_histogram(stat = "count", fill = "pink", color = "black") + 
  ggtitle("2013-14 Day Measurements Started") +
  ylab("People Count") + xlab("Day of the Week") #slide 5

# Day Data-----------------------------------------------------------------

summary(PA_DAY_2013_14)
str(PA_DAY_2013_14)

#Filter to grab data where valid data >1440 mins (full day's worth)

Day_Filter <- PA_DAY_2013_14$paxvmd >= 1440 

Day_13_14_Filtered <- PA_DAY_2013_14[Day_Filter,]

summary(Day_13_14_Filtered)

levels(Day_13_14_Filtered$paxdaywd) <- c("Sun", "Mon", "Tues", "Wed",
                                     "Thurs", "Fri", "Sat")
Day_13_14_Filtered$seqn <- factor(Day_13_14_Filtered$seqn) #7659 distinct
#Day_13_14_Filtered$paxssndp <- NULL
#Day_13_14_Filtered$paxmstd <- NULL
Day_13_14_Filtered$paxtmd <- NULL
Day_13_14_Filtered$paxaismd <- NULL
Day_13_14_Filtered$paxwwmd <- NULL
Day_13_14_Filtered$paxswmd <- NULL
Day_13_14_Filtered$paxnwmd <- NULL
Day_13_14_Filtered$paxumd <- NULL
Day_13_14_Filtered$paxqfd <- NULL

summary(Day_13_14_Filtered)


#Merge day with age/weight data--------------------------------------------
                                       
merged_ageday_13_14 <- merge(Day_13_14_Filtered, Demo_13_14_Filtered, 
                      by.x = "seqn", by.y = "seqn")

merged_ageday_13_14 <- merged_ageday_13_14[,c("seqn", "wtmec2yr","paxdayd", "paxdaywd", 
                                              "paxvmd","paxmtsd", "paxlxsd", "ridageyr", 
                                              "agegroup")]

t<-ggplot(data= merged_ageday_13_14, aes(x= paxdaywd, y= paxmtsd, 
                                group=seqn, color = agegroup, weight = wtmec2yr)) + 
  labs(x="Day of week", y = "MIMS triaxial value", title = "MIMS by age")

t + geom_point() #slide 9

#facet
t + geom_line() + facet_grid(.~agegroup) #slide 9

u<-ggplot(data= merged_ageday_13_14, aes(x= paxlxsd, y= paxmtsd, weight = wtmec2yr,
                               color = agegroup)) + 
  labs(x="Ambient light (lux) values", y = "MIMS triaxial value", title = "MIMS vs Lux")
u + geom_smooth(fill=NA) #slide 10
u + geom_point(alpha=0.1) + geom_smooth(fill=NA) #slide 10

# Day Visualizations

means <- aggregate(paxmtsd ~  paxdaywd, merged_ageday_13_14, weighted.mean, weight = wtmec2yr)
means$common <- c("common")

qplot(data = merged_ageday_13_14, x= paxdaywd, y = paxmtsd, stat = weighted.mean, 
      geom = "boxplot") +
  ggtitle("2013-2014 Day Measurements") +
  ylab("MIMS triaxial value") + xlab("Day of the Week") +
  stat_summary(fun=weighted.mean,  color="darkred", geom="point", 
               shape=18, size=2) +
  geom_text(data = means, aes(label= as.integer(paxmtsd)),
            vjust = "top", size = 3, color ="blue") #slide 6

ggplot(data=means, aes(x=paxdaywd, y=paxmtsd, group = common)) + 
  geom_point() + geom_line(fill=NA) +
  ggtitle("2013-2014 Average Day Measurements") +
  ylab("Avg MIMS triaxial value") + xlab("Day of the Week") #slide 7


ggplot(data= merged_ageday_13_14, aes(x= paxdaywd, y= paxmtsd, group = seqn, weight = wtmec2yr)) +
  geom_point() + geom_line(color="pink") +
  ggtitle("2013-2014 Day Measurements by SEQN") +
  ylab("MIMS triaxial value") + xlab("Day of the Week") #slide 8

# Hour Data--------------------------------------------------------------------

HR_13_14_Filtered <- PA_HR_2013_14

HR_13_14_Filtered$seqn <- factor(HR_13_14_Filtered$seqn)
HR_13_14_Filtered$paxaismh <- NULL
HR_13_14_Filtered$paxqfh <- NULL

levels(HR_13_14_Filtered$paxdaywh) <- c("Sun", "Mon", "Tues", "Wed",
                                         "Thurs", "Fri", "Sat")

     # add activity levels to hour file 
HR_13_14_Filtered <- HR_13_14_Filtered %>%
  mutate(activity = case_when(paxnwmh >= 30 ~ "NW", #non-wear
                              paxumh >= 30 ~ "UN", #unknown
                              paxswmh >= 30 ~ "SL", #sleep
                              paxmtsh <= 40 ~ "SD", #sedentary
                              paxmtsh > 40 & paxmtsh <= 807 ~ "LPA", #light
                              paxmtsh > 807 & paxmtsh <= 2399 ~ "MPA", #moderate
                              paxmtsh > 2399 ~ "VPA")) #vigorous

HR_13_14_Filtered$activity <- factor(HR_13_14_Filtered$activity, levels = c("NW", "UN", "SL", "SD", "LPA", "MPA", "VPA"))


summary(HR_13_14_Filtered)

#Hour Visualization
ggplot(data= HR_13_14_Filtered, aes(x= paxlxsh, y= paxmtsh )) + 
  labs(x="Lux values", y = "MIMS triaxial value", title = "MIMS vs Lux") +
  geom_point(alpha=0.1) +
  facet_grid(paxdaywh~.) #slide 42 (xtra)


# 9-8 work------------------------------------------------------------------

#Merge day,hour & age/weight data (looking at only days where we have 24 hrs of activity: days 2-8 of wear)

Day_HR_merged <- merge (HR_13_14_Filtered, Day_13_14_Filtered, 
                        by.x = c("seqn", "paxdayh"),
                        by.y = c("seqn", "paxdayd"))

Day_HR_merged <- merge (Day_HR_merged, Demo_13_14_Filtered, 
                        by.x = c("seqn"),
                        by.y = c("seqn"))

Day_HR_merged <- arrange(Day_HR_merged, seqn, paxssnhp)

# we have 7,659 distinct seqns in day hour age

Day_HR_merged$hour <- c(1:24)
Day_HR_merged <- Day_HR_merged[,c("seqn", "wtmec2yr","hour", "paxdayh", "paxdaywh",
                                  "paxmtsh", "paxlxsh", "activity" ,"paxmstd",
                                  "paxssnhp","paxtmh", "paxvmh", "paxvmd", 
                                  "ridageyr","agegroup")]  

#Day Hour Merged Visualizations

ggplot(Day_HR_merged, aes(x=hour, y=paxmtsh, color = paxdaywh, weight = wtmec2yr)) +
  geom_point(alpha=0.3) + geom_smooth(fill=NA) +
  coord_cartesian(ylim=c(0,6000)) + 
  labs(x="Hour", y = "MIMS triaxial value", title = "MIMS by Hour per SEQN") #12

ggplot(Day_HR_merged, aes(x=hour, y=paxmtsh, color=paxdaywh, weight = wtmec2yr)) +
  geom_smooth(fill=NA) +
  coord_cartesian(ylim=c(0,1000)) + 
  labs(x="Hour", y = "MIMS triaxial value", title = "MIMS by Hour") #12

ggplot(Day_HR_merged, aes(x=hour, y=paxmtsh, linetype=agegroup, color=paxdaywh, weight = wtmec2yr)) +
  geom_smooth(fill=NA) +
  coord_cartesian(ylim=c(0,1000)) + 
  labs(x="Hour", y = "MIMS triaxial value", title = "Avg Daily MIMS by Hour")#13

ggplot(Day_HR_merged, aes(x=hour, y=paxmtsh, color=agegroup, weight = wtmec2yr)) +
  geom_smooth(fill=NA) +
  coord_cartesian(ylim=c(0,1000)) + 
  labs(x="Hour", y = "MIMS triaxial value", title = "Avg Daily MIMS by Hour")#13

# Weekday
Weekday_HR <- filter(Day_HR_merged, paxdaywh %in% c("Mon", "Tues", "Wed",
                                                    "Thurs", "Fri"))
#there are 7605 distinct seqns for weekdays

ggplot(Weekday_HR, aes(x=hour, y=paxmtsh, color=agegroup, weight = wtmec2yr)) +
  geom_smooth(fill=NA) +
  coord_cartesian(ylim=c(0,1000)) + 
  labs(x="Hour", y = "MIMS triaxial value", title = "Weekdays") 

# Weekend
Weekend_HR <- filter(Day_HR_merged, paxdaywh %in% c("Sat", "Sun"))
#there are 7275 distinct seqns for weekends

ggplot(Weekend_HR, aes(x=hour, y=paxmtsh, color=agegroup, weight = wtmec2yr)) +
  geom_smooth(fill=NA) +
  coord_cartesian(ylim=c(0,1000)) + 
  labs(x="Hour", y = "MIMS triaxial value", title = "Weekends") 

#18-35 Analysis (3 yr intervals)

Analysis_18_35 <- filter(Day_HR_merged, agegroup == "18-35")
Analysis_18_35 <- Analysis_18_35 %>%
  mutate(agesubgroup = case_when(
                              ridageyr >= 18 & ridageyr <= 20 ~ "18-20",
                              ridageyr >= 21 & ridageyr <= 23 ~ "21-23",
                              ridageyr >= 24 & ridageyr <= 26 ~ "24-26",
                              ridageyr >= 27 & ridageyr <= 29 ~ "27-29",
                              ridageyr >= 30 & ridageyr <= 32 ~ "30-32",
                              ridageyr >= 33 & ridageyr <= 35 ~ "33-35"
                              ))


ggplot(Analysis_18_35, aes(x=hour, y=paxmtsh, color=agesubgroup, weight = wtmec2yr)) +
  geom_smooth(fill=NA) +
  coord_cartesian(ylim=c(50,800)) + 
  labs(x="Hour", y = "MIMS triaxial value", title = "18-35 Breakdown") 

#Merge header,hour & age/weight data------------------------------------------

HD_To_Merge <- HD_13_14_Filtered[,c("seqn","paxfday", "paxftime", "paxetldy")]
Week_13_14 <- merge(HR_13_14_Filtered, HD_To_Merge, by.x = "seqn",
                      by.y = "seqn")

Week_13_14 <- merge(Week_13_14, Demo_13_14_Filtered, by.x = "seqn",
                      by.y = "seqn")


 #filter for days 2-8 of wear so no overlap & full day data - 7580 seqn
Week_13_14 <- filter (Week_13_14, paxdayh != 1 & paxdayh != 9) 
 #--

 #filter for full week's worth of data - 3654 seqn

full<- Week_13_14 %>%                           # Specify data frame
  group_by(seqn) %>%                         # Specify group indicator
  summarise_at(vars(paxvmh),                 # Specify column
               list(name = sum))             # Specify function

full_seqn <- filter(full,name==10080)

Week_13_14 <- merge(Week_13_14, full_seqn, by.x = "seqn",
                 by.y = "seqn")
Week_13_14$name <- NULL
 #--

Week_13_14 <- arrange(Week_13_14, seqn, paxssnhp)
Week_13_14$activity <- factor(Week_13_14$activity,                            
                              levels = c("VPA","MPA","LPA","SD","SL","UN","NW"))

#minpos function
minpositive = function(x) min(x[x >= 0])

#testing with single seqn

test <- filter (Week_13_14, seqn == 73557) 


for (i in 1:nrow(test)) {
  test$hrsincesun[i] <- minpositive(c((as.integer(test$paxfday[i])-1)*24 
                                   + hours(times(test$paxftime[i])) + 
                                     minutes(times(test$paxftime[i]))/60 + 
                                     as.double(test$paxssnhp[i])/288000,
                                   (as.integer(test$paxfday[i])-1)*24 + 
                                     hours(times(test$paxftime[i])) + 
                                     minutes(times(test$paxftime[i]))/60 + 
                                     as.double(test$paxssnhp[i])/288000 - 168)) 
}

ggplot(test, aes(x=hrsincesun, y=paxmtsh, weight = wtmec2yr)) +
  scale_x_continuous(breaks = seq(0, 168, by = 12)) +
  geom_line(aes(group=seqn, color=paxdaywh))+
  geom_smooth(method = "loess",color="blue", fill=NA, aes(linetype=agegroup), span = .02) +
  labs(x="Hours Since Sunday 00:00", y = "MIMS triaxial value", title = "Seqn 73557", color = "Day of Week") +
  theme(axis.title.x = element_text(color="DarkGreen"), 
        axis.title.y = element_text(color="DarkGreen"),
        plot.title = element_text(hjust = 0.5))


#Apply to whole dataset

for (i in 1:nrow(Week_13_14)) {
  Week_13_14$hrsincesun[i] <- minpositive(c((as.integer(Week_13_14$paxfday[i])-1)*24 
                                      + hours(times(Week_13_14$paxftime[i])) + 
                                        minutes(times(Week_13_14$paxftime[i]))/60 + 
                                        as.double(Week_13_14$paxssnhp[i])/288000,
                                      (as.integer(Week_13_14$paxfday[i])-1)*24 + 
                                        hours(times(Week_13_14$paxftime[i])) + 
                                        minutes(times(Week_13_14$paxftime[i]))/60 + 
                                        as.double(Week_13_14$paxssnhp[i])/288000 - 168))
}


week_plot <- ggplot(Week_13_14[sample(dim(Week_13_14)[1],300000),], aes(x=hrsincesun, y=paxmtsh, weight = wtmec2yr )) + 
  scale_x_continuous(breaks = seq(0, 168, by = 12)) +
  #geom_line(aes(group=seqn, color=paxdaywh)) +
  labs(x="Hours Since Sunday 00:00", y = "MIMS triaxial value", title = "Physical Activity over the Week", color = "Day of Week" ) +
  theme(axis.title.x = element_text(color="DarkGreen"), 
        axis.title.y = element_text(color="DarkGreen"),
        plot.title = element_text(hjust = 0.5)) 

#slide 25
week_plot + coord_cartesian(ylim=c(0,1300)) +
  scale_y_continuous(breaks = seq(0, 1300, by = 200)) +
  geom_smooth(method = "loess", fill=NA, aes(color=agegroup), span =.02)

#slide 26
week_plot + coord_cartesian(ylim=c(0,1300)) +
  scale_y_continuous(breaks = seq(0, 1300, by = 300)) +
  geom_smooth(method = "loess", fill=NA, color="blue", span =.02) +
  facet_grid(agegroup~.)

# 9-22 work------------------------------------------------------------------

#graph one person's week with activity (31-32)
ggplot(test, aes(x=hrsincesun, y=paxmtsh, weight = wtmec2yr)) +
  scale_x_continuous(breaks = seq(0, 168, by = 12)) +
  #geom_point(aes(color = activity))+
  geom_line(aes(group=seqn, color=activity))+
  #geom_smooth(color="blue", fill=NA, span=0.02) +
  labs(x="Hours Since Sunday 00:00", y = "MIMS triaxial value", title = "Seqn 73557", color = "Activity Level") +
  theme(axis.title.x = element_text(color="DarkGreen"), 
        axis.title.y = element_text(color="DarkGreen"),
        plot.title = element_text(hjust = 0.5))


#expand to everyone (33)
activity_plot <- ggplot(Week_13_14, aes(x=hrsincesun, y=activity, weight = wtmec2yr)) +
  scale_x_continuous(breaks = seq(0, 168, by = 12)) +
  geom_line(aes(group=seqn, color=activity)) +
  labs(x="Hours Since Sunday 00:00", y = "MIMS triaxial value", title = "Physical Activity over the Week", color = "Activity Level" ) +
  theme(axis.title.x = element_text(color="DarkGreen"), 
        axis.title.y = element_text(color="DarkGreen"),
        plot.title = element_text(hjust = 0.5)) 

activity_plot 

#slide 34
MIMS_activity_plot <- ggplot(Week_13_14[sample(dim(Week_13_14)[1],100000),], aes(x=hrsincesun, y=paxmtsh, weight = wtmec2yr)) +
  scale_x_continuous(breaks = seq(0, 168, by = 12)) +
  geom_line(aes(group=seqn, color=activity)) +
  labs(x="Hours Since Sunday 00:00", y = "MIMS triaxial value", title = "Physical Activity over the Week", color = "Activity Level" ) +
  theme(axis.title.x = element_text(color="DarkGreen"), 
        axis.title.y = element_text(color="DarkGreen"),
        plot.title = element_text(hjust = 0.05)) 

MIMS_activity_plot +
  coord_cartesian(ylim=c(0,1000)) + 
  geom_smooth (method = "loess", span=0.02, fill=NA) #aes(linetype=agegroup),

#use geom_count - slide 35
ggplot(Week_13_14, aes(x=agegroup, y=activity, weight = wtmec2yr)) + 
  geom_count() + labs(title = "Hourly Activity Counts by Age Group")

#10/1 work----------------------------------------------------------------

#unscaled barplot
ggplot(data=Week_13_14, aes(x=agegroup, fill = activity, weight = wtmec2yr)) + 
  geom_bar() + #default is stat = count
  labs(x="Age Group", y = "Hourly Counts", title = "Unscaled Plot") 

#Create data frame with hourly counts for scaled barplot
HD_HR_Stats <- data.frame(
  
  agegroup = c("under 18","under 18","under 18","under 18","under 18","under 18","under 18",
               "18-35","18-35","18-35","18-35","18-35","18-35","18-35",
               "36-50","36-50","36-50","36-50","36-50","36-50","36-50",
               "51-75","51-75","51-75","51-75","51-75","51-75","51-75",
               "75+","75+","75+","75+","75+","75+","75+"),
  activity = c("Nonwear", "Unknown", "Sleep", "Sedentary", "Light Physical Activity", "Moderate Physical Activity", "Vigorous Physical Activity"),
  hourly_counts = c(nrow(filter(Week_13_14, agegroup == "under 18" & activity == "NW")),
            nrow(filter(Week_13_14, agegroup == "under 18" & activity == "UN")),
            nrow(filter(Week_13_14, agegroup == "under 18" & activity == "SL")),
            nrow(filter(Week_13_14, agegroup == "under 18" & activity == "SD")),
            nrow(filter(Week_13_14, agegroup == "under 18" & activity == "LPA")),
            nrow(filter(Week_13_14, agegroup == "under 18" & activity == "MPA")),
            nrow(filter(Week_13_14, agegroup == "under 18" & activity == "VPA")),
            nrow(filter(Week_13_14, agegroup == "18-35" & activity == "NW")),
            nrow(filter(Week_13_14, agegroup == "18-35" & activity == "UN")),
            nrow(filter(Week_13_14, agegroup == "18-35" & activity == "SL")),
            nrow(filter(Week_13_14, agegroup == "18-35" & activity == "SD")),
            nrow(filter(Week_13_14, agegroup == "18-35" & activity == "LPA")),
            nrow(filter(Week_13_14, agegroup == "18-35" & activity == "MPA")),
            nrow(filter(Week_13_14, agegroup == "18-35" & activity == "VPA")),
            nrow(filter(Week_13_14, agegroup == "36-50" & activity == "NW")),
            nrow(filter(Week_13_14, agegroup == "36-50" & activity == "UN")),
            nrow(filter(Week_13_14, agegroup == "36-50" & activity == "SL")),
            nrow(filter(Week_13_14, agegroup == "36-50" & activity == "SD")),
            nrow(filter(Week_13_14, agegroup == "36-50" & activity == "LPA")),
            nrow(filter(Week_13_14, agegroup == "36-50" & activity == "MPA")),
            nrow(filter(Week_13_14, agegroup == "36-50" & activity == "VPA")),
            nrow(filter(Week_13_14, agegroup == "51-75" & activity == "NW")),
            nrow(filter(Week_13_14, agegroup == "51-75" & activity == "UN")),
            nrow(filter(Week_13_14, agegroup == "51-75" & activity == "SL")),
            nrow(filter(Week_13_14, agegroup == "51-75" & activity == "SD")),
            nrow(filter(Week_13_14, agegroup == "51-75" & activity == "LPA")),
            nrow(filter(Week_13_14, agegroup == "51-75" & activity == "MPA")),
            nrow(filter(Week_13_14, agegroup == "51-75" & activity == "VPA")),
            nrow(filter(Week_13_14, agegroup == "75+" & activity == "NW")),
            nrow(filter(Week_13_14, agegroup == "75+" & activity == "UN")),
            nrow(filter(Week_13_14, agegroup == "75+" & activity == "SL")),
            nrow(filter(Week_13_14, agegroup == "75+" & activity == "SD")),
            nrow(filter(Week_13_14, agegroup == "75+" & activity == "LPA")),
            nrow(filter(Week_13_14, agegroup == "75+" & activity == "MPA")),
            nrow(filter(Week_13_14, agegroup == "75+" & activity == "VPA"))),
  
  weight = c(sum(filter(Week_13_14, agegroup == "under 18" & activity == "NW")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "under 18" & activity == "UN")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "under 18" & activity == "SL")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "under 18" & activity == "SD")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "under 18" & activity == "LPA")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "under 18" & activity == "MPA")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "under 18" & activity == "VPA")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "18-35" & activity == "NW")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "18-35" & activity == "UN")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "18-35" & activity == "SL")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "18-35" & activity == "SD")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "18-35" & activity == "LPA")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "18-35" & activity == "MPA")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "18-35" & activity == "VPA")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "36-50" & activity == "NW")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "36-50" & activity == "UN")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "36-50" & activity == "SL")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "36-50" & activity == "SD")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "36-50" & activity == "LPA")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "36-50" & activity == "MPA")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "36-50" & activity == "VPA")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "51-75" & activity == "NW")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "51-75" & activity == "UN")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "51-75" & activity == "SL")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "51-75" & activity == "SD")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "51-75" & activity == "LPA")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "51-75" & activity == "MPA")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "51-75" & activity == "VPA")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "75+" & activity == "NW")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "75+" & activity == "UN")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "75+" & activity == "SL")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "75+" & activity == "SD")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "75+" & activity == "LPA")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "75+" & activity == "MPA")$wtmec2yr),
             sum(filter(Week_13_14, agegroup == "75+" & activity == "VPA")$wtmec2yr))
)

#modify order of agegroup & activity factor levels
HD_HR_Stats$agegroup <- factor(HD_HR_Stats$agegroup,                            
                  levels = c("under 18","18-35","36-50","51-75","75+"))

HD_HR_Stats$activity <- factor(HD_HR_Stats$activity,                            
                  levels = c("Vigorous Physical Activity","Moderate Physical Activity","Light Physical Activity","Sedentary","Sleep", "Unknown", "Nonwear"))

#scaled barplot
ggplot(data=HD_HR_Stats, aes(x=agegroup, y=hourly_counts, fill=activity, weight = weight)) + 
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = scales::percent_format()) + 
  labs(x="Age Group", y = "Hourly Counts Percentage", title = "Scaled Boxplot") 

#10/8 work----------------------------------------------------------------------

#create dataframe that summarize activity by person - 3654 seqn
Seqn_Activity <- Week_13_14 %>%                         
  group_by(seqn) %>%                         
  summarise(
            nonwear = sum(activity =="NW"),
            unknown = sum(activity =="UN"),
            sleep = sum(activity=="SL"),
            sedentary = sum(activity=="SD"),
            light = sum(activity=="LPA"),
            moderate = sum(activity=="MPA"),
            vigorous = sum(activity=="VPA"),
            total_hr = 168
            )   

Seqn_Activity <- merge (Seqn_Activity, Demo_13_14_Filtered[,c("seqn","ridageyr", "wtmec2yr", "riagendr", "dmdeduc3", "dmdeduc2", "indhhin2")], 
                        by.x = c("seqn"),
                        by.y = c("seqn"))

Seqn_Activity <- merge (Seqn_Activity, BODY_2013_14[,c("seqn", "bmxht", "bmxwaist", "bmxbmi")], 
                        by.x = c("seqn"),
                        by.y = c("seqn"))

#age group levels
Seqn_Activity <- Seqn_Activity %>%
  mutate(agegroup = case_when(ridageyr<20 ~ "under 20",
                              ridageyr >= 20 & ridageyr <= 35 ~ "20-35",
                              ridageyr > 35 & ridageyr <= 50 ~ "36-50",
                              ridageyr > 50 & ridageyr <= 75 ~ "51-75",
                              ridageyr > 75 ~ "75+"))

Seqn_Activity$agegroup <- factor(Seqn_Activity$agegroup,                            
                               levels = c("under 20","20-35","36-50","51-75","75+"))

#converting income levels
Seqn_Activity <- Seqn_Activity %>%
  mutate(house_income = case_when(indhhin2 %in% c(77,99) ~ "missing",
                                  indhhin2 %in% c(1,2,3,4,5,6,7,13) ~ "low",
                                  indhhin2 %in% c(8,9,10,12,14) ~ "medium",
                                  indhhin2 %in% c(15) ~ "high"
                                  ))

Seqn_Activity$house_income <- factor(Seqn_Activity$house_income,                            
                               levels = c("missing","low","medium","high"))
#add cholesterol
Seqn_Activity <- merge (Seqn_Activity, CH_2013_14[,c("seqn", "lbxtc")], 
                        by.x = c("seqn"),
                        by.y = c("seqn"))

# 10/16 work -------------------------------------------------------------------
# PREDICTING WAIST SIZE

# 20+ Linear model using age group as categorical var
basic_model <- lm(bmxwaist ~ unknown + sleep + sedentary + light + moderate +
     vigorous + agegroup + riagendr + dmdeduc2 + house_income + bmxht, 
   data = Seqn_Activity, weights = wtmec2yr)

summary(basic_model)

#under 20 model 
Seqn_Activity_Under_20 <- filter(Seqn_Activity, agegroup=="under 20")

Under_20_model <- lm(bmxwaist ~  unknown + sleep + sedentary + light + moderate +
                       vigorous + riagendr + ridageyr + house_income + bmxht, 
                     data = Seqn_Activity_Under_20, weights = wtmec2yr)

summary(Under_20_model)

#20-35 model
Seqn_Activity_20_35 <- filter(Seqn_Activity, agegroup=="20-35")

model_20_35 <- lm(bmxwaist ~ unknown + sleep + sedentary + light + moderate +
                       vigorous + riagendr + dmdeduc2 + house_income + bmxht, 
                     data = Seqn_Activity_20_35, weights = wtmec2yr)

summary(model_20_35)

#36-50 model
Seqn_Activity_36_50 <- filter(Seqn_Activity, agegroup=="36-50")

model_36_50 <- lm(bmxwaist ~  unknown + sleep + sedentary + light + moderate +
                    vigorous + riagendr + dmdeduc2 + house_income + bmxht, 
                  data = Seqn_Activity_36_50, weights = wtmec2yr)

summary(model_36_50)

#51-75 model
Seqn_Activity_51_75 <- filter(Seqn_Activity, agegroup=="51-75")

model_51_75 <- lm(bmxwaist ~ unknown + sleep + sedentary + light + moderate +
                    vigorous + riagendr + dmdeduc2 + house_income + bmxht, 
                  data = Seqn_Activity_51_75, weights = wtmec2yr)

summary(model_51_75)

#75+ model 
Seqn_Activity_75plus <- filter(Seqn_Activity, agegroup=="75+")

model_75plus <- lm(bmxwaist ~ unknown + sleep + sedentary + light + moderate +
                    vigorous + riagendr + dmdeduc2 + house_income + bmxht, 
                  data = Seqn_Activity_75plus, weights = wtmec2yr)

summary(model_75plus)

# 11/3 work---------------------------------------------------------------------
#
#
#PREDICTING BMI

# 20+ Linear model using age group as categorical var
bmi_basic_model <- lm(bmxbmi ~ unknown + sleep + sedentary + light + moderate +
                    vigorous + agegroup + riagendr + dmdeduc2 + house_income + bmxht, 
                  data = Seqn_Activity, weights = wtmec2yr)

summary(bmi_basic_model)

#under 20 model 
bmi_Under_20_model <- lm(bmxbmi ~  unknown + sleep + sedentary + light + moderate +
                       vigorous + riagendr + ridageyr + house_income + bmxht, 
                     data = Seqn_Activity_Under_20, weights = wtmec2yr)

summary(bmi_Under_20_model)

#20-35 model
bmi_model_20_35 <- lm(bmxbmi ~ unknown + sleep + sedentary + light + moderate +
                    vigorous + riagendr + dmdeduc2 + house_income + bmxht, 
                  data = Seqn_Activity_20_35, weights = wtmec2yr)

summary(bmi_model_20_35)

#36-50 model
bmi_model_36_50 <- lm(bmxbmi ~  unknown + sleep + sedentary + light + moderate +
                    vigorous + riagendr + dmdeduc2 + house_income + bmxht, 
                  data = Seqn_Activity_36_50, weights = wtmec2yr)

summary(bmi_model_36_50)

#51-75 model
bmi_model_51_75 <- lm(bmxbmi ~ unknown + sleep + sedentary + light + moderate +
                    vigorous + riagendr + dmdeduc2 + house_income + bmxht, 
                  data = Seqn_Activity_51_75, weights = wtmec2yr)

summary(bmi_model_51_75)

#75+ model 
bmi_model_75plus <- lm(bmxbmi ~ unknown + sleep + sedentary + light + moderate +
                     vigorous + riagendr + dmdeduc2 + house_income + bmxht, 
                   data = Seqn_Activity_75plus, weights = wtmec2yr)

summary(bmi_model_75plus)

#
#
#PREDICTING CHOLESTROL

# 20+ Linear model using age group as categorical var
ch_basic_model <- lm(lbxtc ~ unknown + sleep + sedentary + light + moderate +
                        vigorous + agegroup + riagendr + dmdeduc2 + house_income + bmxht, 
                      data = Seqn_Activity, weights = wtmec2yr)

summary(ch_basic_model)

#under 20 model 
ch_Under_20_model <- lm(lbxtc ~  unknown + sleep + sedentary + light + moderate +
                           vigorous + riagendr + ridageyr + house_income + bmxht, 
                         data = Seqn_Activity_Under_20, weights = wtmec2yr)

summary(ch_Under_20_model)

#20-35 model
ch_model_20_35 <- lm(lbxtc ~ unknown + sleep + sedentary + light + moderate +
                        vigorous + riagendr + dmdeduc2 + house_income + bmxht, 
                      data = Seqn_Activity_20_35, weights = wtmec2yr)

summary(ch_model_20_35)

#36-50 model
ch_model_36_50 <- lm(lbxtc ~  unknown + sleep + sedentary + light + moderate +
                        vigorous + riagendr + dmdeduc2 + house_income + bmxht, 
                      data = Seqn_Activity_36_50, weights = wtmec2yr)

summary(ch_model_36_50)

#51-75 model
ch_model_51_75 <- lm(lbxtc ~ unknown + sleep + sedentary + light + moderate +
                        vigorous + riagendr + dmdeduc2 + house_income + bmxht, 
                      data = Seqn_Activity_51_75, weights = wtmec2yr)

summary(ch_model_51_75)

#75+ model 
ch_model_75plus <- lm(lbxtc ~ unknown + sleep + sedentary + light + moderate +
                         vigorous + riagendr + dmdeduc2 + house_income + bmxht, 
                       data = Seqn_Activity_75plus, weights = wtmec2yr)

summary(ch_model_75plus)


#11/10 work------------------------------------------------------------------------

#waist vs age
ggplot(Seqn_Activity,aes(x=ridageyr, y=bmxwaist, weight = wtmec2yr)) + 
  geom_smooth(fill=NA) +
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  labs(x="Age", y = "Waist Size (cm)", title = "Waist Size vs Age")

#bmi vs age
ggplot(Seqn_Activity,aes(x=ridageyr, y=bmxbmi, weight = wtmec2yr)) + 
  geom_smooth(fill=NA) +
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  labs(x="Age", y = "BMI (kg/m^2)", title = "BMI vs Age")

#cholestrol vs age
ggplot(Seqn_Activity,aes(x=ridageyr, y=lbxtc, weight = wtmec2yr)) + 
  geom_smooth(fill=NA) +
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  labs(x="Age", y = "Total Cholesterol (mg/dL)", title = "Total Cholesterol vs Age")

#cholestrol vs height
ggplot(Seqn_Activity,aes(x=bmxht, y=lbxtc, weight = wtmec2yr)) + 
  geom_smooth(fill=NA) +
  labs(x="Height (cm)", y = "Total Cholesterol (mg/dL)", title = "Total Cholesterol vs Height")

# 11/28 work-------------------------------------------------------------------

Under_18_merged_ageday_13_14 <- filter( merged_ageday_13_14, agegroup == "under 18")

means_under18 <- aggregate(paxmtsd ~  paxdaywd, Under_18_merged_ageday_13_14, weighted.mean, weight = wtmec2yr)
means_under18$common <- c("common")


ggplot(data=means_under18, aes(x=paxdaywd, y=paxmtsd, group = common)) + 
  geom_point() + geom_line(color = "red",fill=NA) +
  ggtitle("Under 18 2013-2014 Average Day Measurements") +
  ylab("Avg MIMS triaxial value") + xlab("Day of the Week") 

  # Daily Hour Visualization by age group
ggplot(Day_HR_merged, aes(x=hour, y=paxmtsh, color=agegroup, weight = wtmec2yr)) +
  geom_smooth(fill=NA) +
  coord_cartesian(ylim=c(0,1000)) + 
  labs(x="Hour", y = "MIMS triaxial value", title = "MIMS by Hour") 

