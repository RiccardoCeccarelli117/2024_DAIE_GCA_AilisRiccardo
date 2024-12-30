# Check the structure of the dataset
str(participants_data_final_1_)  

# Get summary statistics
summary(participants_data_final_1_)  

# Count missing values in each column
colSums(is.na(participants_data_final_1_));

# Remove duplicate rows
participants_data_final_1_ <- participants_data_final_1_[!duplicated(participants_data_final_1_), ];

# Convert Group and Gender to factors
participants_data_final_1_$Group <- as.factor(participants_data_final_1_$Group)
participants_data_final_1_$Gender <- as.factor(participants_data_final_1_$Gender)

# Create boxplot for Pre-treatment scores
boxplot(participants_data_final_1_$Pre_Treatment_Score, main = "Boxplot of Pre-treatment Scores");
# Create boxplot for Post-treatment scores
boxplot(participants_data_final_1_$Post_Treatment_Score, main = "Boxplot of Post-treatment Scores")

