# Problem 1: Exploring hotels dataset
# Load necessary package
library(dplyr)

#1.overal structure of dataset
#reading dataset using read.csv 
Dataset <- read.csv('/Users/pavansainathreddyvajrala/Downloads/LasVegasTripAdvisorReviews-Dataset.csv',TRUE,';')
class(Dataset)
head(Dataset)
str(Dataset)
summary(Dataset)
#2.
# Checking variables wheather it is categorical or quantitaive
c_vars <- sapply(Dataset, function(x) is.character(x) | is.factor(x))
q_vars <- sapply(Dataset, is.numeric)
categorical_data <- Dataset[, c_vars]
quantitative_data <- Dataset[, q_vars]
cat("Categorical Variables:\n")
print(names(categorical_data))
cat("Quantitative Variables:\n")
print(names(quantitative_data))
print(names(Dataset))

# Identify nominal and ordinal columns using s apply
# Function to categorize variables
c_variables <- function(data) {
  # Identify nominal and ordinal variables
  nominal <- names(data)[sapply(data, function(x) is.factor(x) && !is.ordered(x))]
  ordinal <- names(data)[sapply(data, function(x) is.factor(x) && is.ordered(x))]
  # Identify numeric variables
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  # Separate discrete and continuous numeric variables
  discrete <- numeric_vars[sapply(data[numeric_vars], function(x) all(x %% 1 == 0))]
  continuous <- setdiff(numeric_vars, discrete)
  # Return results
  list(
    nominal = nominal,
    ordinal = ordinal,
    discrete = discrete,
    continuous = continuous
  )
}
#creating a vector of variables using dataframe "Dataset"
var_categories <- c_variables(Dataset)
# Function to determine variable type
get_var_type <- function(x) {
  if (is.factor(x) || is.character(x)) {
    return("Categorical")
  } else if (is.numeric(x)) {
    return("Quantitative")
  } else {
    return("Other")
  }
}
# Function to determine qualitative type
get_qual_type <- function(x) {
  if (is.factor(x)) {
    return(ifelse(is.ordered(x), "Ordinal", "Nominal"))
  } else if (is.character(x)) {
    return("Nominal")
  } else {
    return(NA)
  }
}
# Function to determine quantitative type
get_quant_type <- function(x) {
  if (is.numeric(x)) {
    return(ifelse(all(x %% 1 == 0), "Discrete", "Continuous"))
  } else {
    return(NA)
  }
}
# Create variable information dataframe
variable_info <- data.frame(
  Variable = names(Dataset),
  Type = sapply(Dataset, get_var_type),
  Qualitative_Type = sapply(Dataset, get_qual_type),
  Quantitative_Type = sapply(Dataset, get_quant_type)
)
# Add Interval_Scale column (you may need to adjust this based on your specific dataset)
#This line initializes the Interval_Scale column to NA and sets it to "No" for all quantitative variables in variable_info
variable_info$Interval_Scale <- NA
variable_info$Interval_Scale[variable_info$Type == "Quantitative"] <- "No"

# Print the variable information
print(variable_info)

# 3 Convert all categorical variables (character and factors) to factors
Dataset <- Dataset %>%mutate(across(where(is.character), as.factor))
str(Dataset)
# Check the structure of the dataset to confirm changes
#This line converts specified columns in the Dataset dataframe to factor type using the mutate and across functions from dplyr
Dataset <- Dataset %>%mutate(across(c(User.country, Period.of.stay, Traveler.type, Pool, Gym, Tennis.court, Spa, Casino, Free.internet, Hotel.name, Hotel.stars, User.continent, Review.month, Review.weekday), as.factor))
# Check for missing values in the entire dataframe
missing_values <- colSums(is.na(Dataset))
# Display the result
missing_values

# Normalisation
mean_score <- mean(Dataset$Score, na.rm = TRUE)
#this is normalization score formula given in assignment doccument
Dataset <- Dataset %>%mutate(Normalized_Score = (Score * Nr..reviews + mean_score * 50) / (Nr..reviews + 50))
head(Dataset)
# Combine all levels that are not "USA" or "UK" and make them as other
Dataset <- Dataset %>%mutate(User.country = factor(ifelse(User.country %in% c("USA", "UK"), as.character(User.country),  "other")))
Dataset$User.country <- factor(Dataset$User.country, levels = c("USA", "UK", "other"))
#this head and tail will help us to identify changes that we have made
head(Dataset)
tail(Dataset)
levels(Dataset$User.country)
# plot
library(ggplot2)
# Function to perform ANOVA and plot boxplots for categorical variables
#creating loop that traverse all variables 
analyze_categorical <- function(variable) {
  anova_result <- aov(Normalized_Score ~ get(variable), data = Dataset)
  summary_result <- summary(anova_result)
  # Plot
  boxplot(Normalized_Score ~ get(variable), data = Dataset,
          main = paste("Normalized Score by", variable),
          xlab = variable, ylab = "Normalized Score")
  return(summary_result)
}
# List of categorical variables
categorical_vars <- c("Pool", "Gym", "Free.internet", "Traveler.type", "User.country","Period.of.stay", "Hotel.stars")
# Analyze each categorical variable using for loop 
for (var in categorical_vars) {
  print(analyze_categorical(var))
}
#Therefore
#Pool,Free Internet, Hotel Stars are Associated with normalized score

#________________________________________________________________________________________________________________________________________________________________________________________________
# Problem2b ploring Heart Disease Dataset
# Load necessary packages
# (install.packages("dplyr") if needed)
library(dplyr)
library(ggplot2)
library(ggpubr)

# Load each dataset using the full path(Open these files)
cleveland_data <- read.table("/Users/pavansainathreddyvajrala/Downloads/processed.cleveland.data", header = FALSE, sep = ",", na.string = "?")
hungarian_data <- read.table("/Users/pavansainathreddyvajrala/Downloads/processed.hungarian.data", header = FALSE, sep = ",", na.string = "?")
switzerland_data <- read.table("/Users/pavansainathreddyvajrala/Downloads/processed.switzerland.data", header = FALSE, sep = ",", na.string = "?")
va_data <- read.table("/Users/pavansainathreddyvajrala/Downloads/processed.va.data", header = FALSE, sep = ",", na.string = "?")
# using str method to check the structure of individual dataframe
str(cleveland_data)
str(hungarian_data)
str(switzerland_data)
str(va_data)
# View the first five rows of each dataset
head(cleveland_data)
head(hungarian_data)
head(switzerland_data)
head(va_data)

# 1 Combine all datasets into one using rbind
combined_data <- rbind(cleveland_data, hungarian_data, switzerland_data, va_data)
str(combined_data)
# As we don't have colummn names we are assining column names manually
colnames(combined_data) <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num")

# 2 Check the number of rows that have missing values in one or more attributes
summary(combined_data)
#this method is used to count the number of null values in combined dataframe
sum(is.na(combined_data))
# Explore the structure of the dataset
na_rows <- combined_data %>%filter(rowSums(is.na(.)) > 0)
total <- nrow(combined_data)
rows_na_count <- nrow(na_rows)
# this is formula to calculate percentage of null value rows in entire dataframe
percentage <- 100*(rows_na_count / total)
cat("Percentage in % =:", percentage, "%\n")
# 3
# Function to determine if a variable is categorical or numeric
var <- data.frame(
  Var = c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg",
          "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num"),
  Type = c("Numeric", "Categorical", "Categorical", "Numeric", "Numeric",
           "Categorical", "Categorical", "Numeric", "Categorical",
           "Numeric", "Categorical", "Categorical", "Categorical", "Categorical"),
  Subtype = c("Continuous", "Nominal", "Ordinal", "Continuous", "Continuous",
              "Nominal", "Nominal", "Continuous", "Nominal",
              "Continuous", "Ordinal", "Ordinal", "Nominal", "Ordinal"),
  IntervalScale = c("Yes", "No", "No", "Yes", "Yes", "No", "No",
                    "Yes", "No", "Yes", "No", "No", "No", "No")
)
print(var)

# 4 Convert all categorical variables using mutate method
combined_data <- combined_data %>%mutate(across(c(sex, cp, fbs, restecg, exang, slope, ca, thal, num), as.factor))
#5 median and mode of the age attribute
median_age <- median(combined_data$age, na.rm = TRUE)
mode_age <- as.numeric(names(sort(table(combined_data$age), decreasing = TRUE)[1]))
# dividing middle attribute with sum of individual attributes
cat("Median age:", median_age, "\n")
# most repeated value 
cat("Mode age:", mode_age, "\n")

# 6 creating a new factor variable and naming it as diagnosis
combined_data$num <- ifelse(combined_data$num == 0, "No", "Yes")
# Replace column 14 of your dataframe with this new variable
colnames(combined_data)[which(names(combined_data) == "num")] <- "diagnosis"
combined_data$diagnosis <- factor(combined_data$diagnosis)
table(combined_data$diagnosis)
head(combined_data)

# let us assume alpha = 0.05
# performimg t test and applying boxplot on age and diagnosis
Age=t.test(combined_data$age ~ combined_data$diagnosis, data = combined_data)
print("T-test for Diagnosis and Age:")
print(Age)
#p-value < 2.2e-16, Significantly associated
# Box plot
boxplot(age ~ diagnosis, data = combined_data,
        main = "Box Plot of Age by Diagnosis",
        xlab = "Age", ylab = "Diagnosis")

# performing chi-square test and applying mosaic plot on sex and diagnosis
Sex <- chisq.test(table(combined_data$diagnosis, combined_data$sex))
print("Chi square test on Diagnosis and Sex :")
print(Sex)
#p-value < 2.2e-16, Significantly associated
# Mosaic plot
mosaicplot(table(combined_data$diagnosis, combined_data$sex), main = "Mosaic Plot of Diagnosis and Sex",shade = TRUE)

# performing chi-square test and applying mosaic plot on cp and diagnosis
Cp <- chisq.test(table(combined_data$diagnosis, combined_data$cp))
print("Chi square test on Diagnosis and CP :")
print(Cp)
#p-value < 2.2e-16, Significantly associated
# Mosaic plot
mosaicplot(table(combined_data$diagnosis, combined_data$cp), main = "Mosaic Plot of Diagnosis and CP", shade = TRUE)
 
# performing t test and applying box lot on trestbps and diagnosis
Trestbps <- t.test(trestbps ~ diagnosis, data = combined_data)
print("T-test for Diagnosis and Trestbps:")
print(Trestbps)
#p-value = 0.001485, Significantly associated
# Box plot
boxplot(trestbps ~ diagnosis, data = combined_data,
        main = "Box Plot on  Trestbps and Diagnosis :",
        xlab = "Trestbps", ylab = "Diagnosis" )

# performimg t test and applying box plot on chol and diagnosis
Chol <- t.test(chol ~ diagnosis, data = combined_data)
print("T-test for Diagnosis and Chol:")
print(Chol)
# p-value = 1.951e-13, Significantly associated
# Box plot
boxplot(chol ~ diagnosis, data = combined_data,
        main = "Box Plot of Chol by Diagnosis",
        xlab = "Chol", ylab = "Diagnosis" )

# performimg chi-square test and applying mosac plot on fbs and diagnosis
fbs <- chisq.test(table(combined_data$diagnosis, combined_data$fbs))
print("Chi-square test for Diagnosis and FBS:")
print(fbs)
#p-value = 5.972e-05, Significantly associated
# Mosaic plot
mosaicplot(table(combined_data$diagnosis, combined_data$fbs), main = "Mosaic Plot of Diagnosis and FBS", shade = TRUE)

# performing chi-square test and applying mosac plot on restecg and diagnosis
restecg <- chisq.test(table(combined_data$diagnosis, combined_data$restecg))
print("Chi-square test for Diagnosis and Restecg:")
print(restecg)
#p-value = 0.002863, Significatly associated
# Mosaic plot
mosaicplot(table(combined_data$diagnosis, combined_data$restecg), main = "Mosaic Plot of Diagnosis and Restecg",shade = TRUE)

# performing t test and applying box plot on thalach and diagnosis
thalach <- t.test(thalach ~ diagnosis, data = combined_data)
print("T-test for Diagnosis and Thalach:")
print(thalach)
#p-value < 2.2e-16, significatly associated
# Box plot
boxplot(thalach ~ diagnosis, data = combined_data,
        main = "Box Plot of Thalach by Diagnosis",
        xlab = "Thalach", ylab = "Diagnosis" )

# performing chi-square test and applying mosac plot on exang and diagnosis
exang <- chisq.test(table(combined_data$diagnosis, combined_data$exang))
print("Chi-square test for Diagnosis and Exang:")
print(exang)
#p-value < 2.2e-16, Significantly Associated
# Mosaic plot
mosaicplot(table(combined_data$diagnosis, combined_data$exang), main = "Mosaic Plot of Diagnosis and Exang", shade = TRUE)

# performing t test and applying box plot on oldpeak and diagnosis
oldpeak <- t.test(oldpeak ~ diagnosis, data = combined_data)
print("T-test for Diagnosis and Oldpeak:")
print(oldpeak)
#p-value < 2.2e-16, significantly associated
# Box plot
boxplot(oldpeak ~ diagnosis, data = combined_data,
        main = "Box Plot of Oldpeak by Diagnosis",
        xlab = "Oldpeak", ylab = "Diagnosis")

# performing chi-square test and applying mosaic plot on slope and diagnosis
slope <- chisq.test(table(combined_data$diagnosis, combined_data$slope))
print("Chi-square test for Diagnosis and Slope:")
print(slope)
#p-value < 2.2e-16, Significantly associated
# Mosaic plot
mosaicplot(table(combined_data$diagnosis, combined_data$slope), main = "Mosaic Plot of Diagnosis and Slope",shade = TRUE)

# performing chi-square test and applying mosaic plot on ca and diagnosis
Ca <- chisq.test(table(combined_data$diagnosis, combined_data$ca))
print("Chi-square test for Diagnosis and CA:")
print(Ca)
#p-value = 8.806e-16, significantly associated
# Mosaic plot
mosaicplot(table(combined_data$diagnosis, combined_data$ca), main = "Mosaic Plot of Diagnosis and CA",shade = TRUE)

# applying chi-square test and applying mosaic plot on thal and diagnosis
chisq_test_thal <- chisq.test(table(combined_data$diagnosis, combined_data$thal))
print("Chi-square test for Diagnosis and Thal:")
print(chisq_test_thal)
#p-value < 2.2e-16, significantly associated
# Mosaic plot
mosaicplot(table(combined_data$diagnosis, combined_data$thal), main = "Mosaic Plot of Diagnosis and Thal",shade = TRUE)

# all p values of 13 variables are less than alpha so all variables are significantly associated


