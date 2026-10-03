# ==========================================
# TASK 1 - DATA CLEANING AND EDA USING R
# ==========================================

# Load dataset
data <- read.csv("train.csv")

# View first 6 rows
head(data)

# Check dataset structure
str(data)

# Summary statistics
summary(data)
# ==========================================
# MISSING VALUE ANALYSIS
# ==========================================

# Count missing values in each column
colSums(is.na(data))

# ==========================================
# CONVERT BLANK VALUES TO NA
# ==========================================

# Convert blank Cabin values to NA
data$Cabin[data$Cabin == ""] <- NA

# Convert blank Embarked values to NA
data$Embarked[data$Embarked == ""] <- NA

# Check missing values again
colSums(is.na(data))

# ==========================================
# HANDLE MISSING VALUES
# ==========================================

# 1. Fill missing Age values with median
age_median <- median(data$Age, na.rm = TRUE)
data$Age[is.na(data$Age)] <- age_median

# 2. Fill missing Embarked values with mode
embarked_mode <- names(sort(table(data$Embarked), decreasing = TRUE))[1]
data$Embarked[is.na(data$Embarked)] <- embarked_mode

# 3. Remove Cabin column because most values are missing
data$Cabin <- NULL

# Check missing values after cleaning
colSums(is.na(data))


# ==========================================
# DUPLICATE VALUE CHECK
# ==========================================

# Count duplicate rows
duplicate_count <- sum(duplicated(data))

duplicate_count

# ==========================================
# CATEGORICAL VARIABLE ENCODING
# ==========================================

# Convert categorical variables to factors
data$Sex <- as.factor(data$Sex)
data$Embarked <- as.factor(data$Embarked)
data$Pclass <- as.factor(data$Pclass)

# Check the structure
str(data)

# Outlier detection
boxplot(data$Fare,
        main = "Fare Outlier Detection",
        ylab = "Fare")


# ==========================================
# NORMALIZATION
# ==========================================

# Min-Max normalization of Fare
data$Fare_normalized <- (data$Fare - min(data$Fare)) /
  (max(data$Fare) - min(data$Fare))

# Check normalized values
summary(data$Fare_normalized)





# ==========================================
# EXPLORATORY DATA ANALYSIS
# ==========================================

# Descriptive statistics
summary(data[, c("Age", "SibSp", "Parch", "Fare")])

# Correlation matrix
cor(data[, c("Survived", "Age", "SibSp", "Parch", "Fare")])

# 1. Age distribution
hist(data$Age,
     main = "Age Distribution",
     xlab = "Age")

# 2. Fare distribution
hist(data$Fare,
     main = "Fare Distribution",
     xlab = "Fare")

# 3. Survival distribution
barplot(table(data$Survived),
        main = "Survival Distribution",
        xlab = "Survived (0 = No, 1 = Yes)",
        ylab = "Number of Passengers")

# 4. Survival by gender
barplot(table(data$Sex, data$Survived),
        beside = TRUE,
        main = "Survival by Gender",
        xlab = "Survival (0 = No, 1 = Yes)",
        ylab = "Number of Passengers",
        legend = TRUE)
# ==========================================
# SAVE CLEANED DATASET
# ==========================================

write.csv(data, "titanic_cleaned.csv", row.names = FALSE)


str(data)



# FINAL CLEANING EVIDENCE

cat("Missing Values After Cleaning:\n")
print(colSums(is.na(data)))

cat("\nDuplicate Rows:\n")
print(sum(duplicated(data)))