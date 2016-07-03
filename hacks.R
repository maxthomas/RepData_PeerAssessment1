library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)

if (!file.exists('activity.csv'))
  unzip('activity.zip')

data <- fread('activity.csv')
dpd <- tbl_df(data)
noNil <- filter(dpd, !is.na(steps))
grped <- noNil %>%
  mutate(dto = ymd(date)) %>%
  group_by(dto)
summed <- summarize(grped, sum(steps))

plot1 <- ggplot(data = summed, 
               aes(summed$`sum(steps)`)) +
  geom_histogram() + 
  labs(x = "Sum of steps taken",
       y = "Count")

mean(summed$`sum(steps)`)
median(summed$`sum(steps)`)

meanSteps <- noNil %>% 
  group_by(interval) %>% 
  summarize(mean(steps))

plot2 <- ggplot(meanSteps, 
                aes(x = interval,
                    y = `mean(steps)`)) +
  labs(x = "Interval (5 minute periods)",
       y = "Mean Number of Steps") +
  geom_line()

filter(meanSteps, 
       `mean(steps)` == max(`mean(steps)`))

wNils <- filter(dpd, is.na(steps))
count(wNils)

nonNils <- dpd %>% 
  group_by(interval) %>% 
  mutate(steps = ifelse(is.na(steps), 
                        mean(steps, na.rm=TRUE), steps)) %>% 
  ungroup %>%
  mutate(dto = ymd(date))

nonNilsSummed <- nonNils %>%
  group_by(dto) %>%
  summarize(sum(steps))

mean(nonNilsSummed$`sum(steps)`)
median(nonNilsSummed$`sum(steps)`)

nonNilPlot <- ggplot(data = nonNilsSummed, 
                     aes(nonNilsSummed$`sum(steps)`)) +
  geom_histogram() + 
  labs(x = "Sum of steps taken",
       y = "Count")

isWeekendDay <- function(x) {
  x == "Sunday" | x == "Saturday"
}

nonNilsWithWeekendLabel <- nonNils %>%
  mutate(weekendLabel = factor(ifelse(isWeekendDay(weekdays(dto)), 
                                      "weekend", 
                                      "weekday"))) %>%
  group_by(interval, weekendLabel) %>%
  summarize(mean(steps))

weekendPlot <- ggplot(nonNilsWithWeekendLabel, 
                      aes(x=nonNilsWithWeekendLabel$interval,
                          y=nonNilsWithWeekendLabel$`mean(steps)`,
                          col = weekendLabel)) +
  labs(x="Interval",
       y="Steps") +
  facet_wrap(interval ~ weekendLabel, nrow = 2) +
  theme(legend.position = "top") +
  geom_line()
