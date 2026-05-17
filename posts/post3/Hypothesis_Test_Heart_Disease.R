library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(infer)


# cleaning
hypothesis_test_dataset <- read.csv ("heart_disease_dataset.csv") |> 
  filter(chol != 0.00) |>
  select ( Resting_Blood_Pressure = trestbps) |>
  mutate (High_BP = ifelse(Resting_Blood_Pressure >= 130, "Yes", "No")
  )

#hypothesis_test_dataset|> 
#  count (High_BP)

#WHO reports 33% of adults in 2024 had hypertension 
# out of 748 individuals, 457 have high blood pressure (~61%)




Barchart_BP <-ggplot(hypothesis_test_dataset, aes(x = High_BP, fill = High_BP)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Yes" = "darkseagreen3", "No" = "seagreen4"))+
  labs(
    x = "Resting High Blood Pressure (mmHg)", 
    y = "Count", 
    title = "Presence of High Resting Blood Presure", 
    caption = "Source: Kaggle Heart Disease Dataset"
  ) +
  guides(fill = guide_legend(title = 'High BP'))+
  theme_minimal()

#Barchart_BP




#bootstrap confidence interval 

BP_boot_dist <- hypothesis_test_dataset|> 
  specify(response = High_BP, success = "Yes")|>
  generate (reps = 10000, type = "bootstrap") |>
  calculate (stat = "prop")


#CI 
ci <- BP_boot_dist |> get_ci(level = 0.95)

#ci


#visualizing the bootstrap CI
BP_boot_dist |>
  visualize() +
  shade_confidence_interval(ci, color = "red", fill = NULL) +
  labs( 
    title = "Distribution of the Proportions of the Bootstrap Samples",
    x = "Proportions",
    y = "Count"
  ) +
  theme_minimal()




#null distribution and p-value 

BP_null_distribution <- hypothesis_test_dataset |> 
  specify (response = High_BP, success = "Yes") |> 
  hypothesize (null = "point", p = 0.33) |> 
  generate (reps = 10000, type = "draw") |>
  calculate (stat = "prop")



#BP_null_distribution |>
 # summarize(mean = mean(stat))
  
# p-value
BP_p_value <- BP_null_distribution |> get_p_value(obs_stat = 457/748, direction = "greater")
#BP_p_value

#visualizing p-value 
visualize(BP_null_distribution) +
  shade_p_value(obs_stat = 457/748, direction = "greater") + 
  labs( 
    title = "Null Distribution of the Proportions of with High Resting Blood Pressure",
    x = "Proportion",
    y = "Count"
  ) +
  theme_minimal()

