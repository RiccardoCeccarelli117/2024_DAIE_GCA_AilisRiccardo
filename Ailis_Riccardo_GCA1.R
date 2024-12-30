# BE SURE TO READ COMMENTS

# Check the structure of the dataset
str(participants_data_final_1_)  

# Get summary statistics
summary(participants_data_final_1_)  

# Count missing values in each column
colSums(is.na(participants_data_final_1_));

# Remove duplicate rows
participants_data_final_1_ <- participants_data_final_1_[!duplicated(participants_data_final_1_), ];

# Convert Group and Gender to factors to check data types
participants_data_final_1_$Group <- as.factor(participants_data_final_1_$Group)
participants_data_final_1_$Gender <- as.factor(participants_data_final_1_$Gender)

# Create boxplot for Pre-treatment scores
boxplot(participants_data_final_1_$Pre_Treatment_Score, main = "Boxplot of Pre-treatment Scores");
# Create boxplot for Post-treatment scores
boxplot(participants_data_final_1_$Post_Treatment_Score, main = "Boxplot of Post-treatment Scores")

# Exploratory Data Analysis (EDA)

# If first run, be sure to uncomment these two install, as the packages are required for data manipulation and visualization
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("tidyr")

library(dplyr)
library(ggplot2)
library(tidyr)


# Split the two groups into Control and Experimental
control_group <- participants_data_final_1_ %>% filter(Group == "Control (CBT)")
experimental_group <- participants_data_final_1_ %>% filter(Group == "Experimental (VR)")

# Summary statistics for control group
summary(control_group)

# Summary statistics for experimental group
summary(experimental_group)

# DATA VISUALIZATION

# Boxplot for Pre-treatment scores
ggplot(participants_data_final_1_, aes(x = Group, y = Pre_Treatment_Score)) +
  geom_boxplot() + labs(title = "Pre-treatment Scores by Group", x = "Group", y = "Pre-treatment Score")

# Boxplot for Post-treatment scores
ggplot(participants_data_final_1_, aes(x = Group, y = Post_Treatment_Score)) +
  geom_boxplot() + labs(title = "Post-treatment Scores by Group", x = "Group", y = "Post-treatment Score")
  
# Histogram for Pre-treatment scores
ggplot(participants_data_final_1_, aes(x = Pre_Treatment_Score, fill = Group)) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.7) + labs(title = "Histogram of Pre-treatment Scores", x = "Pre-treatment Score", y = "Count")

# Histogram for Post-treatment scores
ggplot(participants_data_final_1_, aes(x = Post_Treatment_Score, fill = Group)) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.7) + labs(title = "Histogram of Post-treatment Scores", x = "Post-treatment Score", y = "Count")
  
# Bar Chart

# Data Manipulation for plotting
average_scores <- participants_data_final_1_ %>%
  pivot_longer(cols = c(Pre_Treatment_Score, Post_Treatment_Score), names_to = "Treatment_Phase", values_to = "Score") %>%
  group_by(Group, Treatment_Phase) %>%
  summarise(Average_Score = mean(Score, na.rm = TRUE))

# Chart Creation
ggplot(average_scores, aes(x = Treatment_Phase, y = Average_Score, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") + labs(title = "Average Scores by Group and Treatment Phase", 
       x = "Treatment Phase", 
       y = "Average Score") +
  theme_minimal()

  
  