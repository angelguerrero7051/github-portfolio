# 🔬 NHANES Sleep-Health Analysis (R)

[cite_start]This project conducts a **quantitative cross-sectional analysis** using data from the **National Health and Nutrition Examination Survey (NHANES)** [cite: 8] [cite_start]to investigate the association between sleep duration, depressive symptoms, and overall self-reported health in U.S. adults (N=6,652)[cite: 32].

[cite_start]My goal was to determine if insufficient or excessive sleep duration is linked to poorer health outcomes, particularly depression and low interest in daily activities[cite: 5, 7, 11].

---

## 🛠️ Methods and Statistical Analysis

The analysis employed R for data cleaning, visualization (ggplot2), and statistical modeling.

### 1. Data Cleaning and Preparation

[cite_start]The NHANES dataset was filtered to include participants aged 18 or older with complete data for the variables of interest[cite: 14, 57].

# 🔬 NHANES Sleep-Health Analysis (R)

[cite_start]This project conducts a **quantitative cross-sectional analysis** using data from the **National Health and Nutrition Examination Survey (NHANES)** [cite: 8] [cite_start]to investigate the association between sleep duration, depressive symptoms, and overall self-reported health in U.S. adults (N=6,652)[cite: 32].

[cite_start]My goal was to determine if insufficient or excessive sleep duration is linked to poorer health outcomes, particularly depression and low interest in daily activities[cite: 5, 7, 11].

---

## 🛠️ Methods and Statistical Analysis

The analysis employed R for data cleaning, visualization (ggplot2), and statistical modeling.

### 1. Data Cleaning and Preparation

[cite_start]The NHANES dataset was filtered to include participants aged 18 or older with complete data for the variables of interest[cite: 14, 57].

```r
# Load libraries
library(NHANES)
library(tidyverse)
library(corrplot)

# Filter for adults (Age >= 18) and select key variables
sleep_data <- NHANES::NHANES %>%
  filter(Age >= 18) %>%
  select(Age, SleepHrsNight, SleepTrouble, DaysMentHlthBad, LittleInterest, Depressed, HealthGen) %>%
  na.omit()

# Final Sample Size (N)
print(paste("Sample Size of Participants:", nrow(sleep_data)))
# Output: Sample Size of Participants: 6652
