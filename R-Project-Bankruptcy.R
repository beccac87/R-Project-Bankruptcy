#Clears environment and helps you start fresh
rm(list = ls())

#Library Statements
library(tidyverse)
library(ggplot2)

#DATASET 1
#Download Midterm Data from the “data for class” folder on Blackboard and save it to your computer.  Read the data into R using a file path. 
b_data <-read.csv("~/Desktop/FIN454/bankruptcy_data.csv")
head(b_data)

#QUESTION 1
#Use an appropriate file path to read bankruptcy_data.csv into R Studio.  Find the dimensions of the data. 
dim(b_data)

#QUESTION 2
#Two key variables to our analysis of firm bankruptcy are ROA and current_ratio.
#ROA is a measure of firm profitability, calculated as the firm’s operating income divided by its total assets.
#current_ratio is a ratio of the firm’s current assets to current liabilities, and measures how many times over a firm could pay off its current liabilities using its current assets. 

#Calculate the average and standard deviation of both ROA and current_ratio.  Round your answers to three decimal places and fill in the blanks:
round(mean(b_data$ROA), digits = 3)
round(sd(b_data$ROA), digits = 3)

round(mean(b_data$current_ratio), digits = 3)
round(sd(b_data$current_ratio), digits = 3)

#QUESTION 3
#Using the following definitions, create four binary variables that are measures of firm size:  micro_firms, small_firms, mid_firms, and large_firms:
#1)micro_firms equals one for firms with total_assets less than 15.
#2)small_firms equals one for firms with total_assets greater than 15 and less than 40.
#3_mid_firms equals one for firms with total_assets greater than 40 and less than 1000.
#4)large_firms equals one for firms with total_assets greater than 1000.
b_data <- b_data %>%
  mutate(micro_firms = ifelse(total_assets < 15, 1, 0),
         small_firms = ifelse(total_assets > 15 & total_assets < 40, 1, 0),
         mid_firms = ifelse(total_assets > 40 & total_assets < 1000, 1, 0),
         large_firms = ifelse(total_assets > 1000, 1, 0))

#Note that the unit of the total_assets variable is millions, so micro_firms is a binary variable indicating firms with total assets less than $15 million (USD), and large_firms is a binary variable indicating firms with total assets greater than $1 billion (USD).

#What proportion of firms in the sample belong in each category?  Round your answers to three decimal places and fill in the blanks:
round(mean(b_data$micro_firms), digits = 3)
round(mean(b_data$small_firms), digits = 3)
round(mean(b_data$mid_firms), digits = 3)
round(mean(b_data$large_firms), digits = 3)

#QUESTION 4
#Estimate a linear regression with an outcome variable (y-variable) of ROA and three explanatory variables:  small_firms, mid_firms, and large_firms.  Round your estimates to three decimal places and fill in the blanks:
reg_1 <- lm(ROA ~ small_firms + mid_firms + large_firms, data = b_data)
summary(reg_1)

#QUESTION 6
#Estimate a linear regression with an outcome variable (y-variable) of ROA and three explanatory variables:  Altman_z, large_firms, and an interaction between Altman_z and large firms.
#Altman_z is a variable calculated from measures of firm size and profitability that is intended to capture bankruptcy risk.
#Generally, firms with lower Altman Z scores have higher bankruptcy risk.
reg_2 <- lm(ROA ~ large_firms + Altman_z + Altman_z * large_firms, data = b_data)
summary(reg_2)

#QUESTION 7 
#Use your regression output from question 6 to predict ROA for a large firm with an Altman_z equal to 3 and a firm that is not large (i.e., large firm equal to zero) with an Altman_z equal to 3.
round(exp(reg_2$coefficients[[1]] + reg_2$coefficients[[2]] + reg_2$coefficients[[3]] + reg_2$coefficients[[4]]*3), digits = 3)
round(exp(reg_2$coefficients[[1]] + reg_2$coefficients[[2]]*3), digits = 3)

#QUESTION 9
#Create two binary variables.  First, create neg_Altman_binary, equal to one if Altman_z is negative and zero otherwise.  Then, create bankrupt_binary equal to one if status_label is “failed” and zero otherwise.  If status_label is “failed”, this means that the firm declared bankruptcy during this year.
#Use bankrupt_binary to calculate the probability and odds of firm bankruptcy.  Do not round any intermediate calculations.  Round your final answers to three decimal places and fill in the blanks:
b_data <- b_data %>%
  mutate(neg_Altman_binary = ifelse(Altman_z < 0, 1, 0),
         bankrupt_binary = ifelse(status_label == "failed", 1, 0))


bankrupt_table <- table(b_data$bankrupt_binary)
bankrupt_table

probability_of_bankrupt <- 318 / (3391 + 318)
probability_of_bankrupt

odds_of_bankrupt <- 0.0857374 / (1 - 0.0857374)
odds_of_bankrupt

#QUESTION 10
#Make a cross table and fill it in
bankrupt_table_xtab <- table(b_data$neg_Altman_binary, 	b_data$bankrupt_binary)
bankrupt_table_xtab

margin.table(bankrupt_table_xtab, 1)
margin.table(bankrupt_table_xtab, 2)

#QUESTION 11
#Calculate the following.  Do not round any intermediate calculations, but round your final solution to three decimal places. 
p_positiveAndbankruptcy <- 255 / (3052 + 255)
p_positiveAndbankruptcy

odds_positiveAndbankruptcy <- p_positiveAndbankruptcy / (1 - p_positiveAndbankruptcy)
odds_positiveAndbankruptcy

#What are the odds a firm with a negative Altman Z score declares bankruptcy?
p_negativeAndbankruptcy <- 63 / (339 + 63)
p_negativeAndbankruptcy

odds_negativeAndbankruptcy <- p_negativeAndbankruptcy / (1 - p_negativeAndbankruptcy)
odds_negativeAndbankruptcy

#What is the odds ratio?
odds_positiveAndbankruptcy / odds_negativeAndbankruptcy

log(odds_positiveAndbankruptcy / odds_negativeAndbankruptcy)

exp(-0.799)

#QUESTION 12
#Estimate a logistic model with bankrupt_binary as the outcome variable (y-variable) and one explanatory variable, neg_Altman_binary.  Fill in the blanks below, and round your answers to three decimal places.
reg_3 <- glm(bankrupt_binary ~ neg_Altman_binary, data = b_data, family = binomial(link = "logit"))
summary(reg_3)

#question 15
reg_4 <- glm(bankrupt_binary ~ current_ratio, data = b_data, family = binomial(link = "logit"))
summary(reg_4)

#question 17
prob_bankrupt <- exp(reg_4$coefficients[[1]] + reg_4$coefficients[[2]]*5 ) / 
  (1 + exp(reg_4$coefficients[[1]] + reg_4$coefficients[[2]]*5 )) 

round(prob_bankrupt, digits = 3)

