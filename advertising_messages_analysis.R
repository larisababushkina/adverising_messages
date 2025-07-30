library(readr)
library(dplyr)
library(ggplot2)
library(psych)
library(patchwork)
library(tidyr)
library(car)
library(lmtest)
library(sandwich)

# Set working directory
setwd("/Users/larisakarimova/Downloads")

# Import the raw CSV file
df_raw <- read_csv("data_advertisingmessages_2025-07-17_12-54.csv")

# Use the first row as column names
colnames(df_raw) <- as.character(unlist(df_raw[1, ]))

# Remove the first row (used as header)
df <- df_raw[-1, ]

# Convert all values to numeric where possible
df[] <- lapply(df, function(x) suppressWarnings(as.numeric(as.character(x))))

# Remove unnecessary columns
cols_to_remove <- c(
  "Serial number (if provided)",
  "Reference (if provided in link)",
  "Questionnaire that has been used in the interview",
  "Interview mode",
  "Time the interview has started (Europe/Berlin)",
  "RM: Complete clearances of the ballot, yet",
  "RP: Complete clearances of the ballot, yet",
  "Time spent on page 2",
  "Time spent on page 3",
  "Time spent on page 5",
  "Time spent on page 6",
  "Time spent on page 7",
  "Time spent on page 8",
  "Time when the invitation mailing was sent (personally identifiable recipients, only)",
  "Time when the data was most recently updated",
  "Interview status marker",
  "Has the interview been finished (reached last page)?",
  "Did the respondent only view the questionnaire, omitting mandatory questions?",
  "Last page that the participant has handled in the questionnaire",
  "Hindmost page handled by the participant",
  "Missing answers in percent",
  "Missing answers (weighted by relevance)",
  "Completion Speed (relative)"
)

df_clean <- df %>% select(-all_of(cols_to_remove))

# Rename participant ID
df_clean <- df_clean %>%
  rename(id = `Interview number (sequential)`)
df_clean$id <- as.factor(df_clean$id)

# Convert Likert-type continuous variables to numeric
likert_vars <- c(
  "attitude_AI", "experience_AI", "effort", "expertise", "relevance",
  "msg1_clarity", "msg1_persuasion", "msg1_info", "msg1_emotion", "msg1_purchase", "msg1_authenticity",
  "msg2_clarity", "msg2_persuasion", "msg2_info", "msg2_emotion", "msg2_purchase", "msg2_authenticity",
  "eng_proficiency"
)
df_clean[likert_vars] <- lapply(df_clean[likert_vars], as.numeric)

# Convert categorical variables to factors without labels (levels remain numeric)
factor_vars <- c("gender", "age", "income", "RM: Code drawn", "RP: Code drawn")
df_clean[factor_vars] <- lapply(df_clean[factor_vars], function(x) factor(as.numeric(as.character(x))))


# Identify outliers in survey completion time to detect potential low-quality responses (e.g., respondents who rushed through the survey by randomly clicking)

# Boxplot for "Time spent overall" to check for outliers
boxplot(df_clean$`Time spent overall (except outliers)`,
        main = "Survey Completion Time",
        ylab = "Seconds")

# Identify outliers using the Tukey method
time <- df_clean$`Time spent overall (except outliers)`
outliers <- df_clean[time < quantile(time, 0.25, na.rm = TRUE) - 1.5 * IQR(time, na.rm = TRUE) |
                       time > quantile(time, 0.75, na.rm = TRUE) + 1.5 * IQR(time, na.rm = TRUE), ]
# Number of outliers
nrow(outliers)

# View row of outliers in the data viewer
View(outliers)

# One upper outlier detected (long response time), likely not indicative of low-quality data → Not removing it from the dataset

summary(time)

# ----- SUMMARIZING DATA -----


# ----- Categorical variables -----


# ----- Age -----

# Absolute frequency table for age
table(factor(df_clean$age,
             levels = c(1, 2, 3, 4, 5, 6, 7),
             labels = c("Under 18", "18–24", "25–34", "35–44", "45–54", "55–64", "65+")))

# Relative frequency table for age
round(prop.table(table(factor(df_clean$age,
                              levels = c(1, 2, 3, 4, 5, 6, 7),
                              labels = c("Under 18", "18–24", "25–34", "35–44", "45–54", "55–64", "65+")))) * 100, 1)

# ----- Gender -----

# Absolute frequency table for gender
table(factor(df_clean$gender,
             levels = c(1, 2, 3, 4),
             labels = c("Male", "Female", "Non-binary", "Prefer not to say")))

# Relative frequency table for gender
round(prop.table(table(factor(df_clean$gender,
                              levels = c(1, 2, 3, 4),
                              labels = c("Male", "Female", "Non-binary", "Prefer not to say")))) * 100, 1)

# ----- Income -----

# Absolute frequency table for income
table(factor(df_clean$income,
             levels = c(1, 2, 3, 4, 5, 6, 7, 8),
             labels = c("< 1000", "1000–1999", "2000–2999", "3000–3999", "4000–4999", "5000–5999", "> 6000", "Prefer not to say")))

# Relative frequency table for income
round(prop.table(table(factor(df_clean$income,
                              levels = c(1, 2, 3, 4, 5, 6, 7, 8),
                              labels = c("< 1000", "1000–1999", "2000–2999", "3000–3999", "4000–4999", "5000–5999", "> 6000", "Prefer not to say")))) * 100, 1)

# ----- Histograms for age, gender and income -----

# Define label mappings
age_labels <- c("1" = "Under 18", "2" = "18–24", "3" = "25–34", "4" = "35–44",
                "5" = "45–54", "6" = "55–64", "7" = "65+")
gender_labels <- c("1" = "Male", "2" = "Female", "3" = "Non-binary", "4" = "Prefer not to say")
income_labels <- c("1" = "< 1000", "2" = "1000–1999", "3" = "2000–2999", "4" = "3000–3999",
                   "5" = "4000–4999", "6" = "5000–5999", "7" = "> 6000", "8" = "Prefer not to say")

# Bar plot for one demographic variable
make_demo_plot <- function(var, labels) {
  ggplot(df_clean, aes(x = factor(.data[[var]]))) +
    geom_bar(fill = "lightblue", color = "lightblue", width = 0.7) +
    scale_x_discrete(labels = labels) +
    labs(title = paste("Distribution of", var), x = var, y = "Count") +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 30, hjust = 1),
      plot.title = element_text(size = 12)
    )
}

# Generate plots 
p_age <- make_demo_plot("age", age_labels)
p_gender <- make_demo_plot("gender", gender_labels)
p_income <- make_demo_plot("income", income_labels)

# Arrange plots in a row
p_age | p_gender | p_income

# Absolute frequency table for eng_proficiency
table(factor(df_clean$eng_proficiency))

# Relative frequency table for eng_proficiency
round(prop.table(table(factor(df_clean$eng_proficiency))) * 100, 1)


# ----- Content creation paradigm -----

# Absolute frequency table for RP 
table(factor(df_clean$`RP: Code drawn`,
             levels = c(1, 2, 3, 4),
             labels = c("Human-Written", "AI-Generated", "Human-Edited AI", "AI-Edited Human")))

# Relative frequency table for RP 
round(prop.table(table(factor(df_clean$`RP: Code drawn`,
                              levels = c(1, 2, 3, 4),
                              labels = c("Human-Written", "AI-Generated", "Human-Edited AI", "AI-Edited Human")))) * 100, 1)

# ----- Message type -----

# Absolute frequency table for RM
table(factor(df_clean$`RM: Code drawn`,
             levels = c(1, 2),
             labels = c("Factual", "Emotional")))

# Relative frequency table for RM
round(prop.table(table(factor(df_clean$`RM: Code drawn`,
                              levels = c(1, 2),
                              labels = c("Factual", "Emotional")))) * 100, 1)


# ----- Continuous variables -----


# ----- Control variables -----

control_vars <- c("attitude_AI", "experience_AI", "effort", "expertise", "relevance")

# Descriptive statistics table
control_summary <- df_clean %>%
  select(all_of(control_vars)) %>%
  psych::describe()  
print(control_summary)

# Histograms

# Histogram for a single variable
make_hist_plot <- function(var) {
  mean_val <- mean(df_clean[[var]], na.rm = TRUE)
  median_val <- median(df_clean[[var]], na.rm = TRUE)
  
  ggplot(df_clean, aes_string(x = var)) +
    geom_bar(fill = "lightblue", color = "lightblue", width = 0.7) +
    geom_vline(aes(xintercept = mean_val, color = "Mean"), size = 1) +
    geom_vline(aes(xintercept = median_val, color = "Median"), size = 1) +
    scale_color_manual(name = "", values = c("Mean" = "steelblue", "Median" = "red")) +
    labs(title = paste("Distribution of", var), x = var, y = "Count") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      panel.grid = element_blank()  
    )
}

# Generate plots 
plots <- lapply(control_vars, make_hist_plot)

# Combine plots in a 2-row, 3-column grid
(plots[[1]] | plots[[2]] | plots[[3]]) /
  (plots[[4]] | plots[[5]] | plot_spacer()) +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')


# ----- Dependant variables -----

# Create factual/emotional versions of each DV using message type (RM)
df_clean <- df_clean %>%
  mutate(
    clarity_factual = ifelse(`RM: Code drawn` == 1, msg1_clarity, msg2_clarity),
    clarity_emotional = ifelse(`RM: Code drawn` == 1, msg2_clarity, msg1_clarity),
    
    persuasion_factual = ifelse(`RM: Code drawn` == 1, msg1_persuasion, msg2_persuasion),
    persuasion_emotional = ifelse(`RM: Code drawn` == 1, msg2_persuasion, msg1_persuasion),
    
    info_factual = ifelse(`RM: Code drawn` == 1, msg1_info, msg2_info),
    info_emotional = ifelse(`RM: Code drawn` == 1, msg2_info, msg1_info),
    
    emotion_factual = ifelse(`RM: Code drawn` == 1, msg1_emotion, msg2_emotion),
    emotion_emotional = ifelse(`RM: Code drawn` == 1, msg2_emotion, msg1_emotion),
    
    purchase_factual = ifelse(`RM: Code drawn` == 1, msg1_purchase, msg2_purchase),
    purchase_emotional = ifelse(`RM: Code drawn` == 1, msg2_purchase, msg1_purchase),
    
    auth_factual = ifelse(`RM: Code drawn` == 1, msg1_authenticity, msg2_authenticity),
    auth_emotional = ifelse(`RM: Code drawn` == 1, msg2_authenticity, msg1_authenticity)
  )

# List of dependent variables (already split into factual and emotional versions)
dv_vars <- c("clarity_factual", "clarity_emotional", "persuasion_factual", "persuasion_emotional",
             "info_factual", "info_emotional", "emotion_factual", "emotion_emotional","purchase_factual", 
             "purchase_emotional","auth_factual", "auth_emotional")

# Descriptive statistics table for all DVs
dv_summary <- df_clean %>%
  select(all_of(dv_vars)) %>%
  psych::describe()
print(dv_summary)

# Pivot to long format for plotting
dv_long <- df_clean %>%
  select(all_of(dv_vars)) %>%
  pivot_longer(cols = everything(),
               names_to = c("variable", "type"),
               names_sep = "_",
               values_to = "value") %>%
  mutate(
    type = factor(type, levels = c("factual", "emotional")),
    variable = dplyr::recode(variable,
                             clarity = "Clarity",
                             persuasion = "Persuasiveness",
                             info = "Informativeness",
                             emotion = "Emotional engagement",
                             purchase = "Purchase intention",
                             auth = "Authenticity")
  )

# Plot
ggplot(dv_long, aes(x = type, y = value, fill = type)) +
  geom_boxplot() +
  facet_wrap(~variable, nrow = 2) +
  labs(title = "Boxplots of dependent variables by message type",
       x = "Message type",
       y = "Rating") +
  scale_fill_manual(values = c("factual" = "steelblue", "emotional" = "lightblue")) +
  theme_minimal() +
  theme(legend.position = "none")



# ----- MAIN HYPOTHESES -----

# Create aggregated DV variables (averaging factual and emotional versions)
df_clean <- df_clean %>%
  mutate(
    clarity_avg = rowMeans(select(., clarity_factual, clarity_emotional), na.rm = TRUE),
    persuasion_avg = rowMeans(select(., persuasion_factual, persuasion_emotional), na.rm = TRUE),
    info_avg = rowMeans(select(., info_factual, info_emotional), na.rm = TRUE),
    emotion_avg = rowMeans(select(., emotion_factual, emotion_emotional), na.rm = TRUE),
    purchase_avg = rowMeans(select(., purchase_factual, purchase_emotional), na.rm = TRUE),
    auth_avg = rowMeans(select(., auth_factual, auth_emotional), na.rm = TRUE)
  )


# ----- CLARITY -----

# Regression for clarity without control variables (clarity_avg ~ content generation paradigm)
model_clarity_simple <- lm(clarity_avg ~ `RP: Code drawn`, data = df_clean)
summary(model_clarity_simple)

# Regression for clarity with control variables
model_clarity_controls <- lm(clarity_avg ~ `RP: Code drawn` + attitude_AI + experience_AI + effort + expertise + relevance, data = df_clean)
summary(model_clarity_controls)

# Check regression assumptions

# Residuals vs Fitted, Q-Q plot, Scale-Location, Residuals vs Leverage
par(mfrow = c(2, 2))
plot(model_clarity_controls)
par(mfrow = c(1, 1))

# Normality of residuals
shapiro.test(residuals(model_clarity_controls)) 

# Homoscedasticity
ncvTest(model_clarity_controls) 
bptest(model_clarity_controls)   

# Multicollinearity
vif(model_clarity_controls) 

# Influential observations

# Calculate metrics
n <- nobs(model_clarity_controls)
p <- length(coef(model_clarity_controls))
leverage <- hatvalues(model_clarity_controls) # leverage
cutoff_leverage <- 2 * p / n
student_resid <- rstudent(model_clarity_controls) # studentized residuals
cooks_d <- cooks.distance(model_clarity_controls) # Cook's distance
cutoff_cooks <- 4 / n

# Identify observations 
influential_leverage <- which(leverage > cutoff_leverage)
influential_resid <- which(abs(student_resid) > 2)
influential_cooks <- which(cooks_d > cutoff_cooks)
influential_all <- sort(unique(c(
  influential_leverage,
  influential_resid,
  influential_cooks
)))
influential_all

# Cook's distance plot
plot(cooks.distance(model_clarity_controls), 
     type = "h", 
     main = "Cook's Distance", 
     ylab = "Cook's D", 
     xlab = "Observation")
abline(h = 4 / nobs(model_clarity_controls), col = "red", lty = 2)
points(influential_all, cooks.distance(model_clarity_controls)[influential_all], 
       col = "red", pch = 19)
text(x = influential_all,
     y = cooks.distance(model_clarity_controls)[influential_all],
     labels = influential_all,
     pos = 3, cex = 0.7, col = "red")

# Bubble plot
lev <- hatvalues(model_clarity_controls)
res <- rstudent(model_clarity_controls)
cook <- cooks.distance(model_clarity_controls)
plot(lev, res,
     xlab = "Leverage",
     ylab = "Studentized Residuals",
     main = "Leverage vs Studentized Residuals",
     pch = 20,
     col = "gray")
symbols(lev, res, circles = sqrt(cook), inches = 0.1, add = TRUE)
abline(h = c(-2, 2), col = "blue", lty = 2)
abline(v = 2 * length(coef(model_clarity_controls)) / nobs(model_clarity_controls), col = "blue", lty = 2)
text(lev[influential_all], res[influential_all], labels = influential_all, pos = 3, cex = 0.7, col = "red")


# Check id of row 200
df_clean[c(1, 24, 34, 39, 63, 108, 172, 200), ]

# Model without rows 1, 24, 34, 39, 63, 108, 172, 200
model_clarity_controls_drop <- lm(clarity_avg ~ `RP: Code drawn` + attitude_AI + experience_AI + effort + expertise + relevance, 
                                  data = df_clean[-c(1, 24, 34, 39, 63, 108, 172, 200), ])
summary(model_clarity_controls_drop)

# Check regression assumptions

# Residuals vs Fitted, Q-Q plot, Scale-Location, Residuals vs Leverage
par(mfrow = c(2, 2))
plot(model_clarity_controls_drop)
par(mfrow = c(1, 1))

# Normality of residuals
shapiro.test(residuals(model_clarity_controls_drop)) 

# Homoscedasticity
ncvTest(model_clarity_controls_drop) 
bptest(model_clarity_controls_drop)   

# Multicollinearity
vif(model_clarity_controls_drop) 


# ----- PERSUASIVENESS -----

# Regression for persuasiveness without control variables (persuasion_avg ~ content generation paradigm)
model_persuasion_simple <- lm(persuasion_avg ~ `RP: Code drawn`, data = df_clean)
summary(model_persuasion_simple)

# Regression for persuasiveness with control variables
model_persuasion_controls <- lm(persuasion_avg ~ `RP: Code drawn` + attitude_AI + experience_AI + effort + expertise + relevance, data = df_clean)
summary(model_persuasion_controls)

# Check regression assumptions

# Residuals vs Fitted, Q-Q plot, Scale-Location, Residuals vs Leverage
par(mfrow = c(2, 2))
plot(model_persuasion_controls)
par(mfrow = c(1, 1))

# Normality of residuals
shapiro.test(residuals(model_persuasion_controls)) 

# Homoscedasticity
ncvTest(model_persuasion_controls) 
bptest(model_persuasion_controls)   

# Multicollinearity
vif(model_persuasion_controls) 

# Influential observations

# Calculate metrics
n <- nobs(model_persuasion_controls)
p <- length(coef(model_persuasion_controls))
leverage <- hatvalues(model_persuasion_controls) # leverage
cutoff_leverage <- 2 * p / n
student_resid <- rstudent(model_persuasion_controls) # studentized residuals
cooks_d <- cooks.distance(model_persuasion_controls) # Cook's distance
cutoff_cooks <- 4 / n

# Identify observations 
influential_leverage <- which(leverage > cutoff_leverage)
influential_resid <- which(abs(student_resid) > 2)
influential_cooks <- which(cooks_d > cutoff_cooks)
influential_all <- sort(unique(c(
  influential_leverage,
  influential_resid,
  influential_cooks
)))
influential_all

# Cook's distance plot
plot(cooks.distance(model_persuasion_controls), 
     type = "h", 
     main = "Cook's Distance", 
     ylab = "Cook's D", 
     xlab = "Observation")
abline(h = 4 / nobs(model_persuasion_controls), col = "red", lty = 2)
points(influential_all, cooks.distance(model_persuasion_controls)[influential_all], 
       col = "red", pch = 19)
text(x = influential_all,
     y = cooks.distance(model_persuasion_controls)[influential_all],
     labels = influential_all,
     pos = 3, cex = 0.7, col = "red")

# Bubble plot
lev <- hatvalues(model_persuasion_controls)
res <- rstudent(model_persuasion_controls)
cook <- cooks.distance(model_persuasion_controls)
plot(lev, res,
     xlab = "Leverage",
     ylab = "Studentized Residuals",
     main = "Leverage vs Studentized Residuals",
     pch = 20,
     col = "gray")
symbols(lev, res, circles = sqrt(cook), inches = 0.1, add = TRUE)
abline(h = c(-2, 2), col = "blue", lty = 2)
abline(v = 2 * length(coef(model_persuasion_controls)) / nobs(model_persuasion_controls), col = "blue", lty = 2)
text(lev[influential_all], res[influential_all], labels = influential_all, pos = 3, cex = 0.7, col = "red")

# Check rows 102, 145, 151, 161, 172, 200, 201
df_clean[c(102, 145, 151, 161, 172, 200, 201), ] 

# Model without rows 102, 145, 151, 161, 172, 200, 201
model_persuasion_controls_drop <- lm(persuasion_avg ~ `RP: Code drawn` + attitude_AI + experience_AI + effort + expertise + relevance,
                                     data = df_clean[-c(102, 145, 151, 161, 172, 200, 201), ])
summary(model_persuasion_controls_drop)

# Check regression assumptions

# Residuals vs Fitted, Q-Q plot, Scale-Location, Residuals vs Leverage
par(mfrow = c(2, 2))
plot(model_persuasion_controls_drop)
par(mfrow = c(1, 1))

# Normality of residuals
shapiro.test(residuals(model_persuasion_controls_drop)) 

# Homoscedasticity
ncvTest(model_persuasion_controls_drop) 
bptest(model_persuasion_controls_drop)   

# Multicollinearity
vif(model_persuasion_controls_drop) 

# Robust standard errors (HC3)
coeftest(model_persuasion_controls_drop, vcov = vcovHC(model_persuasion_controls_drop, type = "HC3"))


# ----- INFORMATIVENESS -----

# Regression for informativeness without control variables (persuasion_avg ~ content generation paradigm)
model_info_simple <- lm(info_avg ~ `RP: Code drawn`, data = df_clean)
summary(model_info_simple)
# Although the model is statistically significant, it explains only a negligible proportion of variance (R² = 0.07). 
# Therefore, it does not offer meaningful insight and is not considered further in the analysis.

# Regression for informativeness with contol variables
model_info_controls <- lm(info_avg ~ `RP: Code drawn` + attitude_AI + experience_AI + effort + expertise + relevance, data = df_clean)
summary(model_info_controls)

# Check regression assumptions

# Residuals vs Fitted, Q-Q plot, Scale-Location, Residuals vs Leverage
par(mfrow = c(2, 2))
plot(model_info_controls)
par(mfrow = c(1, 1))

# Normality of residuals
shapiro.test(residuals(model_info_controls)) 

# Homoscedasticity
ncvTest(model_info_controls) 
bptest(model_info_controls)   

# Multicollinearity
vif(model_info_controls) 

# Influential observations

# Calculate metrics
n <- nobs(model_info_controls)
p <- length(coef(model_info_controls))
leverage <- hatvalues(model_info_controls) # leverage
cutoff_leverage <- 2 * p / n
student_resid <- rstudent(model_info_controls) # studentized residuals
cooks_d <- cooks.distance(model_info_controls) # Cook's distance
cutoff_cooks <- 4 / n

# Identify observations 
influential_leverage <- which(leverage > cutoff_leverage)
influential_resid <- which(abs(student_resid) > 2)
influential_cooks <- which(cooks_d > cutoff_cooks)
influential_all <- sort(unique(c(
  influential_leverage,
  influential_resid,
  influential_cooks
)))
influential_all

# Cook's distance plot
plot(cooks.distance(model_info_controls), 
     type = "h", 
     main = "Cook's Distance", 
     ylab = "Cook's D", 
     xlab = "Observation")
abline(h = 4 / nobs(model_info_controls), col = "red", lty = 2)
points(influential_all, cooks.distance(model_info_controls)[influential_all], 
       col = "red", pch = 19)
text(x = influential_all,
     y = cooks.distance(model_info_controls)[influential_all],
     labels = influential_all,
     pos = 3, cex = 0.7, col = "red")

# Bubble plot
lev <- hatvalues(model_info_controls)
res <- rstudent(model_info_controls)
cook <- cooks.distance(model_info_controls)
plot(lev, res,
     xlab = "Leverage",
     ylab = "Studentized Residuals",
     main = "Leverage vs Studentized Residuals",
     pch = 20,
     col = "gray")
symbols(lev, res, circles = sqrt(cook), inches = 0.1, add = TRUE)
abline(h = c(-2, 2), col = "blue", lty = 2)
abline(v = 2 * length(coef(model_info_controls)) / nobs(model_info_controls), col = "blue", lty = 2)
text(lev[influential_all], res[influential_all], labels = influential_all, pos = 3, cex = 0.7, col = "red")

# Check rows 24, 89, 108, 161, 172, 200
df_clean[c(24, 89, 108, 161, 172, 200), ] 

# Model without rows 24, 89, 108, 161, 172, 200
model_info_controls_drop <- lm(info_avg ~ `RP: Code drawn` + attitude_AI + experience_AI + effort + expertise + relevance,
                               data = df_clean[-c(24, 89, 108, 161, 172, 200), ])
summary(model_info_controls_drop)

# Check regression assumptions

# Residuals vs Fitted, Q-Q plot, Scale-Location, Residuals vs Leverage
par(mfrow = c(2, 2))
plot(model_info_controls_drop)
par(mfrow = c(1, 1))

# Normality of residuals
shapiro.test(residuals(model_info_controls_drop)) 

# Homoscedasticity
ncvTest(model_info_controls_drop) 
bptest(model_info_controls_drop)   

# Multicollinearity
vif(model_info_controls_drop) 


# ----- PURCHASE INTENTION -----

# Regression for purchase intention without control variables (purchase_avg ~ content generation paradigm)
model_purchase_simple <- lm(purchase_avg ~ `RP: Code drawn`, data = df_clean)
summary(model_purchase_simple)

# Regression for purchase intention with control variables
model_purchase_controls <- lm(purchase_avg ~ `RP: Code drawn` + attitude_AI + experience_AI + effort + expertise + relevance, data = df_clean)
summary(model_purchase_controls)

# Check regression assumptions

# Residuals vs Fitted, Q-Q plot, Scale-Location, Residuals vs Leverage
par(mfrow = c(2, 2))
plot(model_purchase_controls)
par(mfrow = c(1, 1))

# Normality of residuals
shapiro.test(residuals(model_purchase_controls)) 

# Homoscedasticity
ncvTest(model_purchase_controls) 
bptest(model_purchase_controls)   

# Multicollinearity
vif(model_purchase_controls) 

# Influential observations

# Calculate metrics
n <- nobs(model_purchase_controls)
p <- length(coef(model_purchase_controls))
leverage <- hatvalues(model_purchase_controls) # leverage
cutoff_leverage <- 2 * p / n
student_resid <- rstudent(model_purchase_controls) # studentized residuals
cooks_d <- cooks.distance(model_purchase_controls) # Cook's distance
cutoff_cooks <- 4 / n

# Identify observations 
influential_leverage <- which(leverage > cutoff_leverage)
influential_resid <- which(abs(student_resid) > 2)
influential_cooks <- which(cooks_d > cutoff_cooks)
influential_all <- sort(unique(c(
  influential_leverage,
  influential_resid,
  influential_cooks
)))
influential_all

# Cook's distance plot
plot(cooks.distance(model_purchase_controls), 
     type = "h", 
     main = "Cook's Distance", 
     ylab = "Cook's D", 
     xlab = "Observation")
abline(h = 4 / nobs(model_purchase_controls), col = "red", lty = 2)
points(influential_all, cooks.distance(model_purchase_controls)[influential_all], 
       col = "red", pch = 19)
text(x = influential_all,
     y = cooks.distance(model_purchase_controls)[influential_all],
     labels = influential_all,
     pos = 3, cex = 0.7, col = "red")

# Bubble plot
lev <- hatvalues(model_purchase_controls)
res <- rstudent(model_purchase_controls)
cook <- cooks.distance(model_purchase_controls)
plot(lev, res,
     xlab = "Leverage",
     ylab = "Studentized Residuals",
     main = "Leverage vs Studentized Residuals",
     pch = 20,
     col = "gray")
symbols(lev, res, circles = sqrt(cook), inches = 0.1, add = TRUE)
abline(h = c(-2, 2), col = "blue", lty = 2)
abline(v = 2 * length(coef(model_purchase_controls)) / nobs(model_purchase_controls), col = "blue", lty = 2)
text(lev[influential_all], res[influential_all], labels = influential_all, pos = 3, cex = 0.7, col = "red")

# Check rows 24, 97, 108, 172, 200
df_clean[c(24, 97, 108, 172, 200), ] 

# Model without rows 224, 97, 108, 172, 200
model_purchase_controls_drop <- lm(info_avg ~ `RP: Code drawn` + attitude_AI + experience_AI + effort + expertise + relevance,
                                   data = df_clean[-c(24, 97, 108, 172, 200), ])
summary(model_purchase_controls_drop)

# Check regression assumptions

# Residuals vs Fitted, Q-Q plot, Scale-Location, Residuals vs Leverage
par(mfrow = c(2, 2))
plot(model_purchase_controls_drop)
par(mfrow = c(1, 1))

# Normality of residuals
shapiro.test(residuals(model_purchase_controls_drop)) 

# Homoscedasticity
ncvTest(model_purchase_controls_drop) 
bptest(model_purchase_controls_drop)   

# Multicollinearity
vif(model_purchase_controls_drop) 

# Robust standard errors (HC3)
coeftest(model_purchase_controls_drop, vcov = vcovHC(model_purchase_controls, type = "HC3"))


# ----- EMOTIONAL ENGAGEMENT -----

# Regression for emotional engagement without control variables (emotion_avg ~ content generation paradigm)
model_emotion_simple <- lm(emotion_avg ~ `RP: Code drawn`, data = df_clean)
summary(model_emotion_simple)

# Regression for purchase intention with contol variables
model_emotion_controls <- lm(emotion_avg ~ `RP: Code drawn` + attitude_AI + experience_AI + effort + expertise + relevance, data = df_clean)
summary(model_emotion_controls)

# Check regression assumptions

# Residuals vs Fitted, Q-Q plot, Scale-Location, Residuals vs Leverage
par(mfrow = c(2, 2))
plot(model_emotion_controls)
par(mfrow = c(1, 1))

# Normality of residuals
shapiro.test(residuals(model_emotion_controls)) 

# Homoscedasticity
ncvTest(model_emotion_controls) 
bptest(model_emotion_controls)   

# Multicollinearity
vif(model_emotion_controls) 

# Influential observations

# Calculate metrics
n <- nobs(model_emotion_controls)
p <- length(coef(model_emotion_controls))
leverage <- hatvalues(model_emotion_controls) # leverage
cutoff_leverage <- 2 * p / n
student_resid <- rstudent(model_emotion_controls) # studentized residuals
cooks_d <- cooks.distance(model_emotion_controls) # Cook's distance
cutoff_cooks <- 4 / n

# Identify observations 
influential_leverage <- which(leverage > cutoff_leverage)
influential_resid <- which(abs(student_resid) > 2)
influential_cooks <- which(cooks_d > cutoff_cooks)
influential_all <- sort(unique(c(
  influential_leverage,
  influential_resid,
  influential_cooks
)))
influential_all

# Cook's distance plot
plot(cooks.distance(model_emotion_controls), 
     type = "h", 
     main = "Cook's Distance", 
     ylab = "Cook's D", 
     xlab = "Observation")
abline(h = 4 / nobs(model_emotion_controls), col = "red", lty = 2)
points(influential_all, cooks.distance(model_emotion_controls)[influential_all], 
       col = "red", pch = 19)
text(x = influential_all,
     y = cooks.distance(model_emotion_controls)[influential_all],
     labels = influential_all,
     pos = 3, cex = 0.7, col = "red")

# Bubble plot
lev <- hatvalues(model_emotion_controls)
res <- rstudent(model_emotion_controls)
cook <- cooks.distance(model_emotion_controls)
plot(lev, res,
     xlab = "Leverage",
     ylab = "Studentized Residuals",
     main = "Leverage vs Studentized Residuals",
     pch = 20,
     col = "gray")
symbols(lev, res, circles = sqrt(cook), inches = 0.1, add = TRUE)
abline(h = c(-2, 2), col = "blue", lty = 2)
abline(v = 2 * length(coef(model_emotion_controls)) / nobs(model_emotion_controls), col = "blue", lty = 2)
text(lev[influential_all], res[influential_all], labels = influential_all, pos = 3, cex = 0.7, col = "red")

# Check rows 24, 39, 70, 89, 97,  108, 171, 172, 200
df_clean[c(24, 39, 70, 89, 97,  108, 171, 172, 200), ] 

# Model without rows 24, 39, 70, 89, 97,  108, 171, 172, 200
model_enotion_controls_drop <- lm(emotion_avg ~ `RP: Code drawn` + attitude_AI + experience_AI + effort + expertise + relevance,
                                  data = df_clean[-c(24, 39, 70, 89, 97,  108, 171, 172, 200), ])
summary(model_enotion_controls_drop)

# Check regression assumptions

# Residuals vs Fitted, Q-Q plot, Scale-Location, Residuals vs Leverage
par(mfrow = c(2, 2))
plot(model_enotion_controls_drop)
par(mfrow = c(1, 1))

# Normality of residuals
shapiro.test(residuals(model_enotion_controls_drop)) 

# Homoscedasticity
ncvTest(model_enotion_controls_drop) 
bptest(model_enotion_controls_drop)   

# Multicollinearity
vif(model_enotion_controls_drop) 

# Robust standard errors (HC3)
coeftest(model_enotion_controls_drop, vcov = vcovHC(model_emotion_controls, type = "HC3"))


# ----- AUTHENTICITY -----

# Regression for authenticity without control variables (auth_avg ~ content generation paradigm)
model_auth_simple <- lm(auth_avg ~ `RP: Code drawn`, data = df_clean)
summary(model_auth_simple)

# Regression for authenticity with control variables
model_auth_controls <- lm(auth_avg ~ `RP: Code drawn` + attitude_AI + experience_AI + effort + expertise + relevance, data = df_clean)
summary(model_auth_controls)

# Check regression assumptions

# Residuals vs Fitted, Q-Q plot, Scale-Location, Residuals vs Leverage
par(mfrow = c(2, 2))
plot(model_auth_controls)
par(mfrow = c(1, 1))

# Normality of residuals
shapiro.test(residuals(model_auth_controls)) 

# Homoscedasticity
ncvTest(model_auth_controls) 
bptest(model_auth_controls)   

# Multicollinearity
vif(model_auth_controls) 

# Influential observations

# Calculate metrics
n <- nobs(model_auth_controls)
p <- length(coef(model_auth_controls))
leverage <- hatvalues(model_auth_controls) # leverage
cutoff_leverage <- 2 * p / n
student_resid <- rstudent(model_auth_controls) # studentized residuals
cooks_d <- cooks.distance(model_auth_controls) # Cook's distance
cutoff_cooks <- 4 / n

# Identify observations 
influential_leverage <- which(leverage > cutoff_leverage)
influential_resid <- which(abs(student_resid) > 2)
influential_cooks <- which(cooks_d > cutoff_cooks)
influential_all <- sort(unique(c(
  influential_leverage,
  influential_resid,
  influential_cooks
)))
influential_all

# Cook's distance plot
plot(cooks.distance(model_auth_controls), 
     type = "h", 
     main = "Cook's Distance", 
     ylab = "Cook's D", 
     xlab = "Observation")
abline(h = 4 / nobs(model_auth_controls), col = "red", lty = 2)
points(influential_all, cooks.distance(model_auth_controls)[influential_all], 
       col = "red", pch = 19)
text(x = influential_all,
     y = cooks.distance(model_auth_controls)[influential_all],
     labels = influential_all,
     pos = 3, cex = 0.7, col = "red")

# Bubble plot
lev <- hatvalues(model_auth_controls)
res <- rstudent(model_auth_controls)
cook <- cooks.distance(model_auth_controls)
plot(lev, res,
     xlab = "Leverage",
     ylab = "Studentized Residuals",
     main = "Leverage vs Studentized Residuals",
     pch = 20,
     col = "gray")
symbols(lev, res, circles = sqrt(cook), inches = 0.1, add = TRUE)
abline(h = c(-2, 2), col = "blue", lty = 2)
abline(v = 2 * length(coef(model_auth_controls)) / nobs(model_auth_controls), col = "blue", lty = 2)
text(lev[influential_all], res[influential_all], labels = influential_all, pos = 3, cex = 0.7, col = "red")

# Check rows 24, 39, 55, 97, 108, 158, 170, 172, 200, 201
df_clean[c(24, 39, 55, 97, 108, 158, 170, 172, 200, 201), ] 

# Model without rows 24, 39, 55, 97, 108, 158, 170, 172, 200, 201
model_auth_controls_drop <- lm(auth_avg ~ `RP: Code drawn` + attitude_AI + experience_AI + effort + expertise + relevance,
                               data = df_clean[-c(24, 39, 55, 97, 108, 158, 170, 172, 200, 201), ])
summary(model_auth_controls_drop)

# Check regression assumptions

# Residuals vs Fitted, Q-Q plot, Scale-Location, Residuals vs Leverage
par(mfrow = c(2, 2))
plot(model_auth_controls_drop)
par(mfrow = c(1, 1))

# Normality of residuals
shapiro.test(residuals(model_auth_controls_drop)) 

# Homoscedasticity
ncvTest(model_auth_controls_drop) 
bptest(model_auth_controls_drop)   

# Multicollinearity
vif(model_auth_controls_drop) 

# Robust standard errors (HC3)
coeftest(model_auth_controls, vcov = vcovHC(model_emotion_controls, type = "HC3"))




# ----- MODERATING HYPOTHESES -----

# Long format
df_long <- df_clean %>%
  select(
    id,
    `RP: Code drawn`, `RM: Code drawn`,
    attitude_AI, experience_AI, effort, expertise, relevance,
    clarity_factual, clarity_emotional,
    persuasion_factual, persuasion_emotional,
    info_factual, info_emotional,
    emotion_factual, emotion_emotional,
    purchase_factual, purchase_emotional,
    auth_factual, auth_emotional
  ) %>%
  pivot_longer(
    cols = ends_with("_factual") | ends_with("_emotional"),
    names_to = c("DV", "RM_type"),
    names_pattern = "(.*)_(factual|emotional)",
    values_to = "score"
  ) %>%
  mutate(
    RP = `RP: Code drawn`,
    RM = ifelse(RM_type == "factual", 1, 2)
  ) %>%
  select(
    id, RP, RM, DV, score,
    attitude_AI, experience_AI, effort, expertise, relevance
  )

# ----- CLARITY -----

# CLMM for clarity
install.packages("ordinal")
library(ordinal)
df_long$score <- factor(df_long$score, ordered = TRUE)

clmm_clarity <- clmm(score ~ factor(RP) * factor(RM) +
                       attitude_AI + experience_AI + effort + expertise + relevance +
                       (1 | id),
                     data = df_long %>% filter(DV == "clarity"))
summary(clmm_clarity)

# Multicollinearity check
df_tmp <- df_long %>% 
  filter(DV == "clarity") %>% 
  mutate(score_num = as.numeric(score))
lm_tmp <- lm(score_num ~ factor(RP) * factor(RM) + 
               attitude_AI + experience_AI + effort + expertise + relevance,
             data = df_tmp)
vif(lm_tmp)

# ----- PERSUASIVENESS -----

# CLMM for persuasiveness
df_long$score <- factor(df_long$score, ordered = TRUE)

clmm_persuasion <- clmm(score ~ factor(RP) * factor(RM) +
                          attitude_AI + experience_AI + effort + expertise + relevance +
                          (1 | id),
                        data = df_long %>% filter(DV == "persuasion"))
summary(clmm_persuasion)

# Multicollinearity check
df_tmp <- df_long %>% 
  filter(DV == "persuasion") %>% 
  mutate(score_num = as.numeric(score))
lm_tmp <- lm(score_num ~ factor(RP) * factor(RM) + 
               attitude_AI + experience_AI + effort + expertise + relevance,
             data = df_tmp)
vif(lm_tmp)

# ----- INFORMATIVENESS -----

# CLMM for informativeness
df_long$score <- factor(df_long$score, ordered = TRUE)

clmm_info <- clmm(score ~ factor(RP) * factor(RM) +
                    attitude_AI + experience_AI + effort + expertise + relevance +
                    (1 | id),
                  data = df_long %>% filter(DV == "info"))
summary(clmm_info)

# Multicollinearity check
df_tmp <- df_long %>% 
  filter(DV == "info") %>% 
  mutate(score_num = as.numeric(score))
lm_tmp <- lm(score_num ~ factor(RP) * factor(RM) + 
               attitude_AI + experience_AI + effort + expertise + relevance,
             data = df_tmp)
vif(lm_tmp)

# ----- PURCHASE INTENTION -----

# CLMM for purchase intention
df_long$score <- factor(df_long$score, ordered = TRUE)

clmm_purchase <- clmm(score ~ factor(RP) * factor(RM) +
                        attitude_AI + experience_AI + effort + expertise + relevance +
                        (1 | id),
                      data = df_long %>% filter(DV == "purchase"))
summary(clmm_purchase)

# Multicollinearity check
df_tmp <- df_long %>% 
  filter(DV == "purchase") %>% 
  mutate(score_num = as.numeric(score))
lm_tmp <- lm(score_num ~ factor(RP) * factor(RM) + 
               attitude_AI + experience_AI + effort + expertise + relevance,
             data = df_tmp)
vif(lm_tmp)

# ----- EMOTIONAL ENGAGEMENT -----

# CLMM for emotional engagement
df_long$score <- factor(df_long$score, ordered = TRUE)

clmm_emotion <- clmm(score ~ factor(RP) * factor(RM) +
                       attitude_AI + experience_AI + effort + expertise + relevance +
                       (1 | id),
                     data = df_long %>% filter(DV == "emotion"))
summary(clmm_emotion)

# Multicollinearity check
df_tmp <- df_long %>% 
  filter(DV == "emotion") %>% 
  mutate(score_num = as.numeric(score))
lm_tmp <- lm(score_num ~ factor(RP) * factor(RM) + 
               attitude_AI + experience_AI + effort + expertise + relevance,
             data = df_tmp)
vif(lm_tmp)

# ----- AUTHENTICITY -----

# CLMM for authenticity
df_long$score <- factor(df_long$score, ordered = TRUE)

clmm_auth <- clmm(score ~ factor(RP) * factor(RM) +
                    attitude_AI + experience_AI + effort + expertise + relevance +
                    (1 | id),
                  data = df_long %>% filter(DV == "auth"))
summary(clmm_auth)

# Multicollinearity check
df_tmp <- df_long %>% 
  filter(DV == "auth") %>% 
  mutate(score_num = as.numeric(score))
lm_tmp <- lm(score_num ~ factor(RP) * factor(RM) + 
               attitude_AI + experience_AI + effort + expertise + relevance,
             data = df_tmp)
vif(lm_tmp)
