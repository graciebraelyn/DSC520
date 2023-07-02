# DSC 520
# Gracie Inman
# Week 4 
# 07/02/23
# Part 1 
# 1-Q1 The observational units in this study are the 38 students enrolled in either section.
# 1-Q2 The variables mentioned are the two sections(categorical), course grades(quantitative),
#      and total points (quantitative).
setwd("/Users/gracieinman/Documents/GitHub/dsc520/data")
library(ggplot2)
library(plyr)
library(pastecs)
test_scores_df <- read.csv("scores.csv")
print(test_scores_df)
# 1-Q3
sports_section <- subset.data.frame(test_scores_df, Section=="Sports")
regular_section <- subset.data.frame(test_scores_df, Section=="Regular")
# 1-Q4
ggplot(sports_section, aes(Score)) + geom_histogram(binwidth = 25) + 
  ggtitle("Score Distribution in Sports Section") + 
  xlab('Score') + ylab('Number of Students')
ggplot(sports_section, aes(sample=Score)) + ggtitle("Score Distribution in Sports Section") + stat_qq() +
  stat_qq_line()
ggplot(regular_section, aes(Score)) + geom_histogram(binwidth = 20) + 
  ggtitle("Score Distribution in Regular Section") + 
  xlab('Score') + ylab('Number of Students')
ggplot(regular_section, aes(sample=Score)) + ggtitle("Score Distribution in Regular Section") + stat_qq() +
  stat_qq_line()
stat.desc(sports_section)
stat.desc(regular_section)
# 1-Q4-1 While the study would likely need to be repeated to conclude definite results,
#.       within the scope of this course I can conclude that students in the regular section
#.       tended to score more points. Both sections had 19 samples.The regular section had
#.       385 more total points than the sports section. The regular section also had a smaller
#        standard deviation (33.27 vs 58.03) which means there was a smaller amount of variation in
#        the regular section scores (they were more consistent). A higher mean (327.63 vs 307.37). The regular 
#        section also had a higher minimum score (265 vs 200) and a smaller range (115 vs 195).

# 1-Q4-2 No, one section did not score higher than every student in another. This means that a typical student
#.       in the regular section scored more points than a typical student in the sports section.
         
# 1-Q4-3 I believe one variable that could have influenced score distribution is sex. Depending on the sport referenced
#        men typically know more about sports than women. Not understanding the examples could lead to lower scores.

# Part 2 
library(readxl)
library(plyr)
library(ggplot2)
library(pastecs)
housing_data <- read_excel("week-7-housing.xlsx")
# 2-Q1
apply(housing_data[c('Sale Price')], 2, mean)
# 2-Q2
aggregate(`Sale Price` ~ zip5, data = housing_data, FUN = mean)
# 2-Q3
year_built <- ddply(housing_data, .variables = "year_built", fun = sum)
new_year_built <- transform(year_built, year_built = year_built - 5)
housing_data <- new_year_built
print(housing_data)
# 2-Q4
# Too many data points for the Shapiro-Wilk Test
ggplot(housing_data, aes(sample=Sale.Price)) + ggtitle("Sale Price Distribution in Housing") + stat_qq() +
  stat_qq_line()
ggplot(housing_data, aes(sample=year_built)) + ggtitle("Year Built Distribution in Housing") + stat_qq() +
  stat_qq_line()
ggplot(housing_data, aes(sample=zip5)) + ggtitle("Zip Code Distribution in Housing") + stat_qq() +
  stat_qq_line()
ggplot(housing_data, aes(sample=building_grade)) + ggtitle("Price Distribution in Housing") + stat_qq() +
  stat_qq_line()
ggplot(housing_data, aes(sample=year_renovated)) + ggtitle("Year Renovated Distribution in Housing") + stat_qq() +
  stat_qq_line()
ggplot(housing_data, aes(sample=sq_ft_lot)) + ggtitle("Lot Footage Distribution in Housing") + stat_qq() +
  stat_qq_line()
ggplot(housing_data, aes(sample=bedrooms)) + ggtitle("Bedroom Distribution in Housing") + stat_qq() +
  stat_qq_line()
ggplot(housing_data, aes(sample=bath_full_count)) + ggtitle("Full Bath Distribution in Housing") + stat_qq() +
  stat_qq_line()
ggplot(housing_data, aes(sample=bath_half_count)) + ggtitle("Half Bath Distribution in Housing") + stat_qq() +
  stat_qq_line()
ggplot(housing_data, aes(sample=square_feet_total_living)) + ggtitle("House Square Footage Distribution in Housing") + stat_qq() +
  stat_qq_line()
# 2-Q5
z_score_price = (housing_data$Sale.Price - mean(housing_data$Sale.Price))/ sd(housing_data$Sale.Price)
sort(z_score_price, decreasing = TRUE)
# Sale Price contains at least one outlier.
z_score_yb = (housing_data$year_built - mean(housing_data$year_built))/ sd(housing_data$year_built)
sort(z_score_yb, decreasing = TRUE)
# Year built contains outliers.
z_score_zip = (housing_data$zip5 - mean(housing_data$zip5))/ sd(housing_data$zip5)
sort(z_score_zip, decreasing = TRUE)
# Contains outliers.
z_score_bg = (housing_data$building_grade - mean(housing_data$building_grade))/ sd(housing_data$building_grade)
sort(z_score_bg, decreasing = TRUE)
# Contains outliers.
z_score_yr = (housing_data$year_renovated - mean(housing_data$year_renovated))/ sd(housing_data$year_renovated)
sort(z_score_yr, decreasing = TRUE)
# Contains outliers.
z_score_sfl = (housing_data$sq_ft_lot - mean(housing_data$sq_ft_lot))/ sd(housing_data$sq_ft_lot)
sort(z_score_sfl, decreasing = TRUE)
# Contains outliers.
z_score_bd = (housing_data$bedrooms - mean(housing_data$bedrooms)/ sd(housing_data$bedrooms))
sort(z_score_bd, decreasing = TRUE)
# Contains outliers.
z_score_fb = (housing_data$bath_full_count - mean(housing_data$bath_full_count))/ sd(housing_data$bath_full_count)
sort(z_score_fb, decreasing = TRUE)
# contains outliers
z_score_hb = (housing_data$bath_half_count - mean(housing_data$bath_half_count))/ sd(housing_data$bath_half_count)
sort(z_score_hb, decreasing = TRUE)
# contains outliers
z_score_sqftl = (housing_data$square_feet_total_living - mean(housing_data$square_feet_total_living))/ sd(housing_data$square_feet_total_living)
sort(z_score_sqftl, decreasing = TRUE)
# contains outliers

# 2-Q6

if_sold <- c('yes')
is_it_haunted <- c('no')
housing_data_updated <- cbind(housing_data, if_sold, is_it_haunted)
print(housing_data_updated)
