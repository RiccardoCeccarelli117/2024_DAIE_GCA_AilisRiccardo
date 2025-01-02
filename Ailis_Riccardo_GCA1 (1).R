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

# Calculate mean, median, and standard deviation for Pre- and Post-Treatment scores by group
control_stats <- control_group %>%
  summarise(
    Mean_Pre = mean(Pre_Treatment_Score, na.rm = TRUE),
    Median_Pre = median(Pre_Treatment_Score, na.rm = TRUE),
    SD_Pre = sd(Pre_Treatment_Score, na.rm = TRUE),
    Mean_Post = mean(Post_Treatment_Score, na.rm = TRUE),
    Median_Post = median(Post_Treatment_Score, na.rm = TRUE),
    SD_Post = sd(Post_Treatment_Score, na.rm = TRUE)
  )

experimental_stats <- experimental_group %>%
  summarise(
    Mean_Pre = mean(Pre_Treatment_Score, na.rm = TRUE),
    Median_Pre = median(Pre_Treatment_Score, na.rm = TRUE),
    SD_Pre = sd(Pre_Treatment_Score, na.rm = TRUE),
    Mean_Post = mean(Post_Treatment_Score, na.rm = TRUE),
    Median_Post = median(Post_Treatment_Score, na.rm = TRUE),
    SD_Post = sd(Post_Treatment_Score, na.rm = TRUE)
  )

# Combine the statistics into one table for better presentation
group_stats <- bind_rows(
  control_stats %>% mutate(Group = "Control (CBT)"),
  experimental_stats %>% mutate(Group = "Experimental (VR)")
)

print(control_stats)
print(experimental_stats)
print(group_stats)

# DATA VISUALIZATION

# Boxplots

# Boxplot for Pre-treatment scores
ggplot(participants_data_final_1_, aes(x = Group, y = Pre_Treatment_Score)) +
  geom_boxplot() + labs(title = "Pre-treatment Scores by Group", x = "Group", y = "Pre-treatment Score")

# Boxplot for Post-treatment scores
ggplot(participants_data_final_1_, aes(x = Group, y = Post_Treatment_Score)) +
  geom_boxplot() + labs(title = "Post-treatment Scores by Group", x = "Group", y = "Post-treatment Score")
  
# Histograms

# Histogram for Pre-treatment scores
ggplot(participants_data_final_1_, aes(x = Pre_Treatment_Score, fill = Group)) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.7) + labs(title = "Histogram of Pre-treatment Scores", x = "Pre-treatment Score", y = "Count")

# Histogram for Post-treatment scores
ggplot(participants_data_final_1_, aes(x = Post_Treatment_Score, fill = Group)) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.7) + labs(title = "Histogram of Post-treatment Scores", x = "Post-treatment Score", y = "Count")
  
# Bar Charts

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

# Line graphs

# Create a line graph showing Pre- and Post-Treatment scores for each group
ggplot(average_scores, aes(x = Treatment_Phase, y = Average_Score, group = Group, color = Group)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Average Scores by Group Over Treatment Phases", x = "Treatment Phase", y = "Average Score") +
  theme_minimal()

# Scatter plots

# Create scatter plots to explore relationships between Pre- and Post-Treatment scores
ggplot(participants_data_final_1_, aes(x = Pre_Treatment_Score, y = Post_Treatment_Score, color = Group)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Scatter Plot of Pre- vs Post-Treatment Scores", x = "Pre-Treatment Score", y = "Post-Treatment Score") +
  theme_minimal()

# Confidence Interval

# Create Confidence Interval (CI) plots for Pre- and Post-Treatment scores
ci_data <- participants_data_final_1_ %>%
  pivot_longer(cols = c(Pre_Treatment_Score, Post_Treatment_Score), names_to = "Treatment_Phase", values_to = "Score") %>%
  group_by(Group, Treatment_Phase) %>%
  summarise(
    Mean_Score = mean(Score, na.rm = TRUE),
    SD_Score = sd(Score, na.rm = TRUE),
    n = n(),
    CI_Lower = Mean_Score - qt(0.975, df = n - 1) * SD_Score / sqrt(n),
    CI_Upper = Mean_Score + qt(0.975, df = n - 1) * SD_Score / sqrt(n)
  )

ggplot(ci_data, aes(x = Treatment_Phase, y = Mean_Score, group = Group, color = Group)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  labs(title = "Confidence Intervals for Scores by Group", x = "Treatment Phase", y = "Mean Score with 95% CI") +
  theme_minimal()

# QQ Plots

# Create QQ plots for normality checks
qqnorm(control_group$Pre_Treatment_Score, main = "QQ Plot - Control Group Pre-Treatment")
qqline(control_group$Pre_Treatment_Score, col = "blue")

qqnorm(control_group$Post_Treatment_Score, main = "QQ Plot - Control Group Post-Treatment")
qqline(control_group$Post_Treatment_Score, col = "blue")

qqnorm(experimental_group$Pre_Treatment_Score, main = "QQ Plot - Experimental Group Pre-Treatment")
qqline(experimental_group$Pre_Treatment_Score, col = "red")

qqnorm(experimental_group$Post_Treatment_Score, main = "QQ Plot - Experimental Group Post-Treatment")
qqline(experimental_group$Post_Treatment_Score, col = "red")

# Summary tables

# Create summary tables for Pre- and Post-Treatment Scores
summary_table <- participants_data_final_1_ %>%
  group_by(Group) %>%
  summarise(
    Mean_Pre = mean(Pre_Treatment_Score, na.rm = TRUE),
    SD_Pre = sd(Pre_Treatment_Score, na.rm = TRUE),
    Mean_Post = mean(Post_Treatment_Score, na.rm = TRUE),
    SD_Post = sd(Post_Treatment_Score, na.rm = TRUE)
  )
summary_table

# Statistical testing (t-test), to perform on the post-treatment scores 

# Extract numeric vectors for Post-Treatment scores from each group
control_scores <- control_group$Post_Treatment_Score
experimental_scores <- experimental_group$Post_Treatment_Score

# Perform the t-test on Post-Treatment scores
t_test_result <- t.test(control_scores, experimental_scores, 
                        alternative = "two.sided", 
                        var.equal = TRUE)  # Assuming equal variances; set to FALSE if not

# Print the t-test result
print(t_test_result)

