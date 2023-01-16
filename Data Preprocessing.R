#Install packages
install.packages("tidyverse")
install.packages("funModeling")
install.packages("moments")
install.packages("GGally")
install.packages("dlookr")

#Load packages
library(dplyr)
library(stringr)
library(ggplot2)
library(factoextra)
library(ggcorrplot)
library(tidyr)
library(ggmap)
library(maps)
library(mapdata)
library(ggpubr)
library(treemapify)
library(readxl)
library(tidyverse)
library(funModeling)
library(moments)
library(ggpubr)
library(dlookr)

#Import dataset
df = read_excel("C:\\Users\\chery\\Downloads\\Player_FIFA22.xlsx")
View(df)

#Row and Columns Exploration
#Get column names
names(df)

#Calculate the number of rows
nrow(df)

#Calculate the number of columns
ncol(df)

#Get dataset dimension
dim(df)

#Inspect the first 10 rows
head(df, 10)

#Inspect the last 10 rows
tail(df, 10)

#Obtain data structure
status <- status(df)
status(df2)
status %>%
  select('variable', 'p_na') %>%
  filter(p_na != 0)


#Descriptive statistics
summary(df)
summary(df2)

#Mean of missing values
col_names <- names(which(colSums(is.na(df)) > 0))
df2 <- df[,c(col_names)]
df2

num_cols <- unlist(lapply(df2, is.numeric))
data_num <- df2[ , num_cols]
colMeans(data_num, na.rm=TRUE)

barplot(colMeans(data_num, na.rm=TRUE), col = "yellow")

summary(data_num)

#Median of Performance Stats
performance_col <- c("Pace", "Shooting", "Passing", "Dribbling", "Defending", "Physic")
performance_stats <- c(median(df$pace, na.rm = TRUE), median(df$shooting, na.rm = TRUE), 
                       median(df$passing, na.rm = TRUE),
                     median(df$dribbling, na.rm = TRUE), median(df$defending, na.rm = TRUE), 
                     median(df$physic, na.rm = TRUE))
performance <- data.frame(performance_col, performance_stats)
performance

ggplot(performance, aes(x=performance_col, y=performance_stats, fill=performance_col)) +
  geom_bar(stat = "identity") + 
  ggtitle("Median of Performance Stats")

#Value ranges
num_cols2 <- unlist(lapply(df, is.numeric))
data_num2 <- df[ , num_cols2]
boxplot(log(data_num2), col="purple")

data_num3 <- df2 %>%
  select(value_eur, wage_eur, age, release_clause_eur, club_contract_valid_until,
         height_cm, weight_kg)
boxplot(log(data_num3), col="purple", outline=FALSE)


#Standard Deviation
stats = profiling_num(df)
stats
stats <- select(stats, c(variable), c(std_dev))
stats
stats_data <- stats[18:23,]
stats_data

ggplot(stats_data, aes(x=variable, y=std_dev, color=variable, label=round(std_dev,3))) +
  geom_point() +
  geom_text(vjust=1.5) +
  ggtitle("Standard Deviation of Performance Stats")

#Standard Deviation
stats = profiling_num(df2)
stats
stats <- select(stats, c(variable), c(std_dev))
stats
stats_data <- stats[c(3:7, 11, 52, 58),]
stats_data

ggplot(stats_data, aes(x=variable, y=std_dev, color=variable, label=round(std_dev,3))) +
  geom_point() +
  geom_text(vjust=1.5) +
  ggtitle("Standard Deviation of Performance Stats")

#Normality
print(normality(df), n=75)

#Duplicated values
sum(duplicated(df))

#Percentage of categorical variables
freq(df)
freq(df2)

#Distribution of numerical variables
plot_num(df)
plot_num(df2)

plot_num(df2[,25:37])

set_one <- data_num2[,1:10]
plot_num(set_one)

set_two <- data_num2[,11:20]
plot_num(set_two)

set_three <- data_num2[,21:30]
plot_num(set_three)

set_four <- data_num2[,31:42]
plot_num(set_four)

set_five <- data_num2[,43:55]
plot_num(set_five)

#Correlation Plot
player_corr <- df %>% select(overall,potential,value_eur,wage_eur,age,height_cm,weight_kg,
                             release_clause_eur,shooting,passing,dribbling,physic,defending)
corr <- round(cor(na.omit(player_corr)),1)
corrp.mat <- cor_pmat(player_corr)
ggcorrplot(corr, method ="square", hc.order = TRUE, lab=TRUE) +
  labs(title="Correlation Plot for Player Data")

#Correlation Plot 2
player_corr1 <- df2 %>% select(overall,potential,value_eur,wage_eur,age,height_cm,weight_kg,
                             release_clause_eur,shooting,passing,dribbling,physic,defending)
corr <- round(cor(na.omit(player_corr1)),1)
corrp.mat <- cor_pmat(player_corr1)
ggcorrplot(corr, method ="square", hc.order = TRUE, lab=TRUE) +
  labs(title="Correlation Plot for Player Data")

#Value counts of categorical variables
#Too many categories to be plotted
player_positions = df %>% count(player_positions, sort = TRUE)
club_name = df %>% count(club_name, sort = TRUE)
league_name = df %>% count(league_name, sort = TRUE)
nationality = df %>% count(nationality_name, sort = TRUE)

#Plots for top 10
#Player Positions
player_positions = head(player_positions, 10)
ggplot(player_positions, aes(x=player_positions, y=n, fill = player_positions, label=n)) + 
  geom_bar(stat = 'identity') +
  ggtitle("Value Count of Player Position") +
  geom_text()

#Club Name
club_name = head(club_name, 10)
ggplot(club_name, aes(x=club_name, y=n, fill = club_name, label=n)) + 
  geom_bar(stat = 'identity') +
  ggtitle("Value Count of Club Name") +
  geom_text()

#League Name
league_name = head(league_name, 10)
ggplot(league_name, aes(x=league_name, y=n, fill = league_name, label=n)) + 
  geom_bar(stat = 'identity') +
  ggtitle("Value Count of League Name") +
  geom_text()

#Nationality Name
nationality = head(nationality, 10)
ggplot(nationality, aes(x=nationality_name, y=n, fill = nationality_name, label=n)) + 
  geom_bar(stat = 'identity') +
  ggtitle("Value Count of Nationality Name") +
  geom_text()

#Categories with plots
#Club Position
ggplot(df, aes(x=club_position, fill=club_position)) + 
  geom_bar() +
  ggtitle("Value Count of Club Position")

#National Position
ggplot(df, aes(x=nation_position, fill=nation_position)) + 
  geom_bar() +
  ggtitle("Value Count of Nation Position")

#Preferred Foot
ggplot(df, aes(x=preferred_foot, fill=preferred_foot)) + 
  geom_bar() +
  ggtitle("Value Count of Preferred Foot")

#Weak foot
ggplot(df, aes(x=weak_foot, fill=weak_foot)) + 
  geom_bar() +
  ggtitle("Value Count of Weak Foot")

#Skill moves
ggplot(df, aes(x=skill_moves, fill=skill_moves)) + 
  geom_bar() +
  ggtitle("Value Count of Skill Moves")

#Work Rate
ggplot(df, aes(x=work_rate, fill=work_rate)) + 
  geom_bar() +
  ggtitle("Value Count of Work Rate")

#Body Type
ggplot(df, aes(x=body_type, fill=body_type)) + 
  geom_bar() +
  ggtitle("Value Count of Body Type")

#Real Face
ggplot(df, aes(x=real_face, fill=real_face)) + 
  geom_bar() +
  ggtitle("Value Count of Real Face")

#Distribution and density of numerical values
#Overall
ggplot(df, aes(x=overall, y=..density..)) + 
  geom_histogram(binwidth = 5, fill="#A093F5") +
  geom_density(fill="#512ACC", alpha = 0.8) +
  ggtitle("Distribution of Overall Column")

skewness(df$overall)

#Potential
ggplot(df, aes(x=potential, y=..density..)) + 
  geom_histogram(binwidth = 5, fill="#A093F5") +
  geom_density(fill="#512ACC", alpha = 0.8) +
  ggtitle("Distribution of Potential Column")

skewness(df$potential)

#Age
ggplot(df, aes(x=age, y=..density..)) + 
  geom_histogram(binwidth = 5, fill="#A093F5") +
  geom_density(fill="#512ACC", alpha = 0.8) +
  ggtitle("Distribution of Age Column")

skewness(df$age)

#Height
ggplot(df, aes(x=height_cm, y=..density..)) + 
  geom_histogram(binwidth = 5, fill="#A093F5") +
  geom_density(fill="#512ACC", alpha = 0.8) +
  ggtitle("Distribution of Height Column")

skewness(df$height_cm)

#Weight
ggplot(df, aes(x=weight_kg, y=..density..)) + 
  geom_histogram(binwidth = 5, fill="#A093F5") +
  geom_density(fill="#512ACC", alpha = 0.8) +
  ggtitle("Distribution of Weight Column")

skewness(df$weight_kg)

#Release Clause Eur
ggplot(df, aes(x=release_clause_eur, y=..density..)) + 
  geom_histogram(binwidth = 5, fill="#A093F5") +
  geom_density(fill="#512ACC", alpha = 0.8) +
  ggtitle("Distribution of Release Clause Eur Column")

skewness(df$release_clause_eur)

#Pace
ggplot(df, aes(x=pace, y=..density..)) + 
  geom_histogram(binwidth = 5, fill="#A093F5") +
  geom_density(fill="#512ACC", alpha = 0.8) +
  ggtitle("Distribution of Pace Column")

skewness(df$pace)

#Shooting
ggplot(df, aes(x=shooting, y=..density..)) + 
  geom_histogram(binwidth = 5, fill="#A093F5") +
  geom_density(fill="#512ACC", alpha = 0.8) +
  ggtitle("Distribution of Shooting Column")

#Passing
ggplot(df, aes(x=passing, y=..density..)) + 
  geom_histogram(binwidth = 5, fill="#A093F5") +
  geom_density(fill="#512ACC", alpha = 0.8) +
  ggtitle("Distribution of Passing Column")

#Dribbling
ggplot(df, aes(x=dribbling, y=..density..)) + 
  geom_histogram(binwidth = 5, fill="#A093F5") +
  geom_density(fill="#512ACC", alpha = 0.8) +
  ggtitle("Distribution of Dribbling Column")

#Defending
ggplot(df, aes(x=defending, y=..density..)) + 
  geom_histogram(binwidth = 5, fill="#A093F5") +
  geom_density(fill="#512ACC", alpha = 0.8) +
  ggtitle("Distribution of Defending Column")

#Physic
ggplot(df, aes(x=physic, y=..density..)) + 
  geom_histogram(binwidth = 5, fill="#A093F5") +
  geom_density(fill="#512ACC", alpha = 0.8) +
  ggtitle("Distribution of Physic Column")

#Attacking Crossing
ggplot(df, aes(x=attacking_crossing, y=..density..)) + 
  geom_histogram(binwidth = 5, fill="#A093F5") +
  geom_density(fill="#512ACC", alpha = 0.8) +
  ggtitle("Distribution of Attacking Crossing Column")

skewness(df$attacking_crossing)

#Attacking Finishing
ggplot(df, aes(x=attacking_finishing, y=..density..)) + 
  geom_histogram(binwidth = 5, fill="#A093F5") +
  geom_density(fill="#512ACC", alpha = 0.8) +
  ggtitle("Distribution of Attacking Finishing Column")

skewness(df$attacking_finishing)

#Attacking Heading Accuracy
ggplot(df, aes(x=attacking_heading_accuracy, y=..density..)) + 
  geom_histogram(binwidth = 5, fill="#A093F5") +
  geom_density(fill="#512ACC", alpha = 0.8) +
  ggtitle("Distribution of Attacking Heading Accuracy Column")

skewness(df$attacking_heading_accuracy)

#Relationship between variables
#Relationship between potential and value_eur
ggplot(df, aes(x=potential, y=value_eur)) +
  geom_point(aes(color=value_eur)) +
  geom_smooth(method="loess") +
  ggtitle("Relationship Between Potential and Value Eur")

#Relationship between height and physic
ggplot(df, aes(x=overall, y=wage_eur)) +
  geom_point(aes(color=wage_eur)) +
  geom_smooth(method="loess") +
  ggtitle("Relationship Between Overall and Wage Eur")

#Detect null values
sapply(df, function(x) sum(is.na(x)))

#Detect outliers
col <- data.frame(df$value_eur, df$wage_eur, df$age, df$height_cm, df$weight_kg, df$release_clause_eur)
col_boxplot <- boxplot(log(col), col="blue")

value_out <- boxplot(df$value_eur,plot=FALSE)$out
length(value_out)
wage_out <- boxplot(df$wage_eur,plot=FALSE)$out
length(wage_out)
age_out <- boxplot(df$age,plot=FALSE)$out
length(age_out)
height_out <- boxplot(df$height_cm,plot=FALSE)$out
length(height_out)
weight_out <- boxplot(df$weight_kg,plot=FALSE)$out
length(weight_out)
release_out <- boxplot(df$release_clause_eur,plot=FALSE)$out
length(release_out)

outlier_number <- c(length(value_out),length(wage_out),length(age_out), length(height_out),
                    length(weight_out),length(release_out))
outlier_names <- c('value_eur', 'wage_eur', 'age', 'height_cm', 'weight_kg', 'release_clause_eur')
col_outlier <- data.frame(outlier_names, outlier_number)
col_outlier

ggplot(col_outlier, aes(x=outlier_names, y=outlier_number, fill=outlier_names, label=outlier_number)) +
  geom_bar(stat = 'identity') +
  ggtitle("Number of Outlier Per Variable") +
  geom_text()

#Detect inconsistent values
unique(df$age)
unique(df$dob)
unique(df$club_position) #inconsistent value
unique(df$club_jersey_number)
unique(df$nationality_name)
unique(df$nation_position)
unique(df$preferred_foot) #inconsistent value
unique(df$weak_foot)
unique(df$skill_moves)
unique(df$body_type)
unique(df$work_rate)

#Drop columns with more than 70% of null values
drop <- c('club_loaned_from', 'nation_position', 'nation_jersey_number', 'player_tags', 'goalkeeping_speed')
df2 <- df[ , !(names(df) %in% drop)]
df2

dim(df2)
View(df2)

#Replacing club_name null values
df2$club_name[is.na(df2$club_name)] = 'No Club'

sum(is.na(df2$club_name))
nrow(filter(df2, club_name=='No Club'))

#Replacing league_name null values
df2$league_name[is.na(df2$league_name)] <- 'Not Assigned'

sum(is.na(df2$league_name))
nrow(filter(df2, league_name=='Not Assigned'))

#Replacing player_traits null values
df2$player_traits[is.na(df2$player_traits)] = 'No Special Traits'

sum(is.na(df2$player_traits))
nrow(filter(df2, player_traits=='No Special Traits'))

#Replacing club_position null values
df2$club_position[is.na(df2$club_position)] = 'No Position Assigned'
sum(is.na(df2$club_position))

nrow(filter(df2, club_position=='No Position Assigned'))

#Replacing club_jersey_number null values
df2$club_jersey_number[is.na(df2$club_jersey_number)] = "Not Assigned"
sum(is.na(df2$club_jersey_number))

nrow(filter(df2, club_jersey_number=='Not Assigned'))

#Replacing league_level null values
df2$league_level[is.na(df2$league_level)] <- 'Not Assigned'

sum(is.na(df2$league_level))
nrow(filter(df2, league_level=='Not Assigned'))

#Replacing null values with KNN
install.packages("VIM")
library(VIM)

df2 <- kNN(df2, variable = c("value_eur", "wage_eur", "release_clause_eur"), k=6)
summary(df2)
summary(df)

#Replacing null values with median
library(dplyr)
library(tidyr)
df2 <- df2 %>% mutate(across(c(pace, shooting, passing, dribbling, defending, physic),
                      ~replace_na(., median(., na.rm=TRUE))))
sum(is.na(c(df2$pace, df2$shooting, df2$passing, df2$dribbling, df2$defending, df2$physic)))

#Find out start date
install.packages("lubridate")
library(lubridate)
birth_date <- as.Date(df$dob)
end_dob <- birth_date %m+% years(df$age)

end_dob[end_dob >= "2021-06-01"]
end_dob[end_dob >= "2021-07-01"]
#DOB is calculated from 2021-07-01

#Replacing age values
install.packages("eeptools")
library("eeptools")

birth_date <- as.Date(df$dob)
x_date   <- as.Date("2021-07-01")

x_age <- age_calc(birth_date, 
                  x_date,
                  units = "years")
age <- floor(x_age)
age

df2$age = NULL
df2 <- cbind(df2, age)
sum(is.na(df2$age))
View(df2)
df2

#Calculate the mean years of contract for each club
df2[c('Year', 'Month', 'Day')] <- str_split_fixed(df$club_joined,'-', 3)
join_year <- as.numeric(df2$Year)
difference <- df$club_contract_valid_until - join_year
difference
df2 <- cbind(df2, difference)
df2$difference[is.na(df2$difference)] <- 0


df2 <- df2 %>%
  group_by(club_name) %>%
  mutate(club_mean = floor(mean(difference, na.rm = T))) %>%
  as.data.frame()

class(df2$club_mean)
class(df2$Year)

df2$club_mean[is.na(df2$club_mean)] <- 0

#Replace null values of club_joined

sum(is.na(df2$club_name))
sum(is.na(df2$club_joined))
sum(is.na(df2$join_year))
sum(is.na(df2$club_contract_valid_until))

df2$club_joined[(df2$club_name == "No Club") & (is.na(df2$club_joined))] <- '1900-01-01'
year_part <- as.character(df2$club_contract_valid_until - df2$club_mean)
year_part[is.na(year_part)] <- '1900'
sum(is.na(year_part))
month_part <- '0101'
df2$join_year <- paste(year_part, month_part, sep='')
df2$join_year[(is.na(df2$join_year))] <- '19000101'
as.numeric(df2$join_year)

df2$club_joined[is.na(df2$club_joined)] <- ymd(df2$join_year)

which(is.na(df2$join_year))
which(is.na(df2$club_joined))

#Replace null values of club_contract_valid_until

df2$club_contract_valid_until[(df2$club_name == "No Club") & (is.na(df2$club_contract_valid_until))] <- 0
nrow(filter(df2, club_contract_valid_until==0))
df2$club_contract_valid_until[is.na(df2$club_contract_valid_until)] <- sum(as.numeric(year_part), df2$club_mean)
df2$club_contract_valid_until[(is.na(df2$club_contract_valid_until))] <- 0

which(is.na(df2$club_contract_valid_until))

#Standardising values
df2$preferred_foot[(df2$preferred_foot == 'L')] <- 'Left'
df2$preferred_foot[(df2$preferred_foot == 'R')] <- 'Right'

freq(df2)

#Replacing outliers
df2$value_eur[df2$value_eur %in% value_out] <- median(df2$value_eur)
df2$wage_eur[df2$wage_eur %in% wage_out] <- median(df2$wage_eur)
df2$height_cm[df2$height_cm %in% height_out] <- median(df2$height_cm)
df2$weight_kg[df2$weight_kg %in% weight_out] <- median(df2$weight_kg)
df2$release_clause_eur[df$release_clause_eur %in% release_out] <- median(df2$release_clause_eur)

year_out <- boxplot(df2$club_contract_valid_until)$out
df2$club_contract_valid_until[df2$club_contract_valid_until %in% year_out] <- median(df2$club_contract_valid_until)
boxplot(df2$club_contract_valid_until)

#Change data type
df2$sofifa_id <- as.character(df2$sofifa_id)
df2$league_level <- as.character(df2$league_level)
df2$club_jersey_number <- as.character(df2$club_jersey_number)
df2$weak_foot <- as.character(df2$weak_foot)

class(c(df2$sofifa_id, df2$league_level, df2$club_jersey_number, df2$weak_foot))

#Split player position
df2[c('Position 1', 'Position 2', 'Position 3')] <- str_split_fixed(df2$player_position,', ', 3)
df2$`Position 2`[df2$`Position 2`== ''] <- 'No Additional Positions'
df2$`Position 3`[df2$`Position 3`== ''] <- 'No Additional Positions'

#Calculate BMI
df2$bmi <- df2$weight_kg / (df2$height_cm/100)^2

#Data Normalisation
library(caret)

minMax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

num_cols3 <- unlist(lapply(df2, is.numeric))
data_num3 <- df2[ , num_cols3]
normalised_df2 <- as.data.frame(lapply(data_num3, minMax))
head(normalised_df2)
View(normalised_df2)


