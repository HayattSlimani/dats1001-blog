library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)


cleaned_heart_disease_dataset <- read.csv ("heart_disease_dataset.csv")|>
  select(
    Age =  age,
    Sex = sex,
    Resting_ECG = restecg,
    Chest_Pain_Type = cp, 
    Cholesterol = chol, 
    Resting_Blood_Pressure = trestbps, 
    Heart_Disease = num
  ) |>
  filter(Cholesterol != 0.00) |>
  arrange(Age)

cleaned_heart_disease_dataset


#makes the values binary 0 = no/false + 1-4 = yes/true
cleaned_heart_disease_dataset <- cleaned_heart_disease_dataset |>
  mutate(
    Heart_Disease = ifelse(Heart_Disease == "0", "No", "Yes")
  ) 




#Continuous Variables : showing range, outliers, and typical patient stats 
# - more patients younger vs older, any extreme cholesterol values/ bp  

Histogram_Age <-ggplot(cleaned_heart_disease_dataset) + 
  aes(x = Age) + 
  geom_histogram(bins = 15, fill = "darkseagreen3", color = "white") +
  labs(
    x = "Age in years", 
    y = "Count", 
    title = "Distribution of Patient Age", 
    caption = "Source: "
  ) +
  geom_vline(xintercept = mean(cleaned_heart_disease_dataset$Age))
  theme_minimal()

Histogram_Age

mean(cleaned_heart_disease_dataset$Age)
summary(cleaned_heart_disease_dataset$Age)
IQR(cleaned_heart_disease_dataset$Age)


Histogram_RestingBP <-ggplot(cleaned_heart_disease_dataset) + 
  aes(x = Resting_Blood_Pressure) + 
  geom_histogram(bins = 20, fill = "darkseagreen3", color = "white") +
  labs(
    x = "Resting Blood Pressure (mm Hg)", 
    y = "Count", 
    title = "Distribution of Blood Pressure at Hospital Admission", 
    caption = "Source: "
  ) +
  geom_vline(xintercept = mean(cleaned_heart_disease_dataset$Resting_Blood_Pressure))
theme_minimal()

Histogram_RestingBP


mean(cleaned_heart_disease_dataset$Resting_Blood_Pressure)
summary(cleaned_heart_disease_dataset$Resting_Blood_Pressure)
IQR(cleaned_heart_disease_dataset$Resting_Blood_Pressure)


#add vline to all histograms!!
Histogram_Cholesterol <-ggplot(cleaned_heart_disease_dataset) + 
  aes(x = Cholesterol) + 
  geom_histogram(bins = 18, fill = "darkseagreen3", color = "white") +
  labs(
    x = "Serum Cholesterol (mg/dl)", 
    y = "Count", 
    title = "Distribution of Cholesterol Levels", 
    caption = "Source: Kaggle Heart Disease Dataset"
  ) + 
  geom_vline(xintercept = mean(cleaned_heart_disease_dataset$Cholesterol))
  theme_minimal()

Histogram_Cholesterol


mean(cleaned_heart_disease_dataset$Cholesterol)
summary(cleaned_heart_disease_dataset$Cholesterol)
IQR(cleaned_heart_disease_dataset$Cholesterol)



# Categorical Variables: showing counts for each = gender, chest pain type, ekg levels, 

Barchart_Sex <-ggplot(cleaned_heart_disease_dataset, aes(x = Sex, fill = Sex)) +
  geom_bar() +
  labs(
    x = "Sex", 
    y = "Count", 
    title = "Heart Disease Distribution by Gender", 
    caption = "Source: Kaggle Heart Disease Dataset"
  ) +
  theme_minimal()

Barchart_Sex


Barchart_Chest_Pain_Type <-ggplot(cleaned_heart_disease_dataset, aes(x = Chest_Pain_Type, fill = Chest_Pain_Type)) +
  geom_bar() +
  labs(
    x = "Type of Chest Pain", 
    y = "Count", 
    title = "Heart Disease Distribution by Type of Chest Pain", 
    caption = "Source: Kaggle Heart Disease Dataset"
  ) +
  theme_minimal()

Barchart_Chest_Pain_Type



#change legend title 

Barchart_ECG <-ggplot(cleaned_heart_disease_dataset, aes(x = Resting_ECG, fill = Resting_ECG)) +
  geom_bar() +
  labs(
    x = "Resting Electrocardiogrpahic Result", 
    y = "Count", 
    title = "Heart Disease Distribution by", 
    caption = "Source: Kaggle Heart Disease Dataset"
  ) +
  guides(fill = guide_legend(title = 'Levels of ECG'))
  theme_minimal()

Barchart_ECG





Barchart_Heart_Disease <-ggplot(cleaned_heart_disease_dataset, aes(x = Heart_Disease, fill = Heart_Disease)) +
  geom_bar() +
  labs(
    x = "Presence of Heart Disease", 
    y = "Count", 
    title = "Heart Disease Distribution", 
    caption = "Source: Kaggle Heart Disease Dataset"
  ) +
  guides(fill = guide_legend(title = 'Does the patient have heart disease'))
  theme_minimal()

Barchart_Heart_Disease


# box plots : for comparison of continuous + categorical variables
ggplot(cleaned_heart_disease_dataset, aes(x = Heart_Disease, y = Age)) +
  geom_boxplot(fill = "darkseagreen3", color = "black") +
  labs(x = "Age of Patient", y = "Presense of Heart Disease", title = "title", caption = "Source: Kaggle Heart Disease Dataset")



ggplot(cleaned_heart_disease_dataset, aes(x = Resting_ECG, y = Resting_Blood_Pressure)) +
  geom_boxplot(fill = "darkseagreen3", color = "black") +
  labs(x = "Resting Blood Pressure (mm Hg)", y = "Resting ECG Result",title = "title", caption = "Source: Kaggle Heart Disease Dataset")


