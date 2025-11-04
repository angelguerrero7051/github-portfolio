{r}
#loading libraries to read, analyze, and plot data 
library(NHANES)
library(tidyverse)
library(knitr)
library(corrplot)
library(dplyr)
library(corrplot)
library(ggplot2)

#To view NHANES data before filtering uncomment the line below
#View(NHANES)

#Pipeline to filter and select variables wanted to get final end result 
sleep_data <- NHANES %>%
  select(Age, SleepHrsNight, SleepTrouble,
         DaysMentHlthBad, LittleInterest, 
         Depressed, HealthGen
         ) %>%
  filter(Age >= 18) %>%
  drop_na()

#Used to make sure I did the filtering correctly for names, can uncomment if wanting to see correct names
#names(sleep_data)

#See if filtering process was correct using knitr library (will only see 10 rows)
kable(head(sleep_data, 10), caption = "AG-Few Variables of Output From Sleep_Data")

# Sample Size of Participants with no N/A's of Categories Measured
sample_size <- nrow(sleep_data)
cat("Sample Size of Participants:", sample_size, "\n\n\n")

# Full Summary for All Variables
summary_output <- summary(sleep_data)
print(summary_output)

# Clarity of General Health For Patients and Summary of it + Proportions
cat("\n\n--- Qualitative Variable Summaries ---\n\n")

# General Health According to Participants
cat("HealthGen (General Health):\n")
healthgen_counts <- table(sleep_data$HealthGen)
print(healthgen_counts)
cat("\nProportions:\n")
print(prop.table(healthgen_counts))

# Sleep Trouble Participants May Be Experiencing
cat("\nSleepTrouble:\n")
sleeptrouble_counts <- table(sleep_data$SleepTrouble)
print(sleeptrouble_counts)
cat("\nProportions:\n")
print(prop.table(sleeptrouble_counts))

# Little Interest According to Data
cat("\nLittleInterest:\n")
littleinterest_counts <- table(sleep_data$LittleInterest)
print(littleinterest_counts)
cat("\nProportions:\n")
print(prop.table(littleinterest_counts))

# Depression Severity According to Data
cat("\nDepressed:\n")
depressed_counts <- table(sleep_data$Depressed)
print(depressed_counts)
cat("\nProportions:\n")
print(prop.table(depressed_counts))

# Selecting Variables for Correlation Heatmap
sleep_data_quant <- sleep_data %>%
  select(Age, SleepHrsNight, SleepTrouble,
         DaysMentHlthBad, LittleInterest, 
         Depressed,HealthGen)

# Take All Columns in Data Quant to Convert to Numeric (as.numeric) 
sleep_data_quant <- sleep_data_quant %>%
  mutate(across(everything(), as.numeric)) 

# Compute Correlation of Sleep_data_quant that do not contain N/A
cor_matrix <- cor(sleep_data_quant, use = "complete.obs")

# Plot the correlation matrix with colored cells and coefficient values (2 decimals) and adjust size of text
par(mar = c(0, 7, 3, 0))
corrplot(cor_matrix, 
         method = "color", 
         type = "upper", 
         addCoef.col = "black", 
         tl.col = "black", 
         tl.srt = 45, 
         number.cex = 0.7)

# mtext places title text on left side vertically using side, margin line, horiz orientation, and increased size
mtext("AG - Correlation HeatMap", 
      side = 2, 
      line = 4, 
      las = 0, 
      cex = 1.2)

# Add participant IDs
sleep_data$ID <- 1:nrow(sleep_data)

# HealthGen Boxplot
ggplot(sleep_data, aes(x = HealthGen, y = SleepHrsNight, fill = HealthGen)) +
  geom_boxplot() +
  theme_bw() +
  labs(title = "AG-Sleep Hours by General Health",
       x = "General Health",
       y = "Hours of Sleep per Night") +
  theme(legend.position = "none")

# LittleInterest Boxplot
ggplot(sleep_data, aes(x = LittleInterest, y = SleepHrsNight, fill = LittleInterest)) +
  geom_boxplot() +
  theme_bw() +
  labs(title = "AG-Sleep Hours by Little Interest",
       x = "Level of Interest",
       y = "Hours of Sleep per Night") +
  theme(legend.position = "none")

# Depressed Boxplot
ggplot(sleep_data, aes(x = Depressed, y = SleepHrsNight, fill = Depressed)) +
  geom_boxplot() +
  theme_bw() +
  labs(title = "AG-Sleep Hours by Depression Level",
       x = "Depression Level",
       y = "Hours of Sleep per Night") +
  theme(legend.position = "none")

# SleepTrouble Boxplot
ggplot(sleep_data, aes(x = SleepTrouble, y = SleepHrsNight, fill = SleepTrouble)) +
  geom_boxplot() +
  theme_bw() +
  labs(title = "AG-Sleep Hours by Sleep Trouble",
       x = "Sleep Trouble",
       y = "Hours of Sleep per Night") +
  theme(legend.position = "none")

# Scatterplot for DaysMentHlthBad
ggplot(sleep_data, aes(x = DaysMentHlthBad, y = SleepHrsNight)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  theme_bw() +
  labs(title = "AG-SleepHours vs DaysMentHlthBad",
       x = "Number of Bad Mental Health Days",
       y = "Hours of Sleep per Night")

# Defining the vectors containing all categorical predictor variables
cat_vars <- c("HealthGen", "LittleInterest", "Depressed", "SleepTrouble")

# ANOVA and Tukey's HSD
cat("\n\n--- One-WAY ANOVA & TUKEY'S HSD ---\n\n")

# Using lapply (apply specified function over list/vector and return results as a list) to run ANOVA for each variable and store results while treating SleepHrsNight as dependent variable
anova_results <- lapply(cat_vars, function(var) {
  formula <- as.formula(paste("SleepHrsNight ~", var))
  model <- aov(formula, data = sleep_data)
  list(
    Variable = var,
    ANOVA = summary(model),
    Tukey = TukeyHSD(model)
  )
})

# View ANOVA & Tukey results for each variable
anova_results

# Regression and Correlation 
cat("\n\n--- SIMPLE REGRESSION AND CORRELATION (DaysMentHlthBad) ---\n\n")

# Create linear regression for SleepHrsNight & continuous predictor (DaysMentHlthBad)
lm_model <- lm(SleepHrsNight ~ DaysMentHlthBad, data = sleep_data)

# Print out summary (includes: slope, intercept, R-squared, and p-values for model)
summary(lm_model)

# Calculate the 95% CI for the estimated slope & intercept
confint(lm_model)

# To measure linear relationship use pearson correlation coefficient 
cor.test(sleep_data$SleepHrsNight, sleep_data$DaysMentHlthBad, method = "pearson")

# Spearman correlation coefficient used which is used to measure consistent relationship and non-linearity/outliers
cor.test(sleep_data$SleepHrsNight, sleep_data$DaysMentHlthBad, method = "spearman")

# Multiple linear Regression
cat("\n\n--- MULTIPLE LINEAR REGRESSION (All Predictors) ---\n\n")

# Used to run multiple linear regression with selection predictors (e.g. HealthGen, etc) to combine predictive power and unique contribution of each predictor while controlling for the others
lm_multi <- lm(SleepHrsNight ~ HealthGen + LittleInterest + Depressed + SleepTrouble + DaysMentHlthBad,
               data = sleep_data)

# Print out the multiple linear regression created above
summary(lm_multi)

