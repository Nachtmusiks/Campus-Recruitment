## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(readr)
library(ggplot2)
library(dplyr)

# Load the dataset
df <- read_csv("Placement_Data_Full_Class.csv")

str(df)
head(df)

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Removing unnecessary columns
college_df <- df %>% select(-sl_no)

# Cleaning up null data by replacing salary with 0
college_df$salary[is.na(college_df$salary)] <- 0

# One Hot Encoding
college_df <- college_df %>% 
  mutate(specialisation_Mkt_Fin = as.integer(specialisation == 'Mkt&Fin'),
         specialisation_Mkt_HR = as.integer(specialisation == 'Mkt&HR'),
         ssc_b_Others = as.integer(ssc_b == 'Others'),
         hsc_b_Others = as.integer(hsc_b == 'Others'),
         hsc_s_Arts = as.integer(hsc_s == 'Arts'),
         hsc_s_Commerce = as.integer(hsc_s == 'Commerce'),
         hsc_s_Science = as.integer(hsc_s == 'Science'),
         degree_t_Comm_Mgmt = as.integer(degree_t == 'Comm&Mgmt'),
         degree_t_Sci_Tech = as.integer(degree_t == 'Sci&Tech'),
         degree_t_Others = as.integer(degree_t == 'Others'))

college_df <- college_df %>% select(-c(specialisation, ssc_b, hsc_b, hsc_s, degree_t))

# Dictionary replacement
college_df$gender <- ifelse(college_df$gender == 'M', 0, 1)
college_df$workex <- ifelse(college_df$workex == 'No', 0, 1)
college_df$status <- ifelse(college_df$status == 'Placed', 0, 1)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Load necessary packages
library(reshape2)
library(ggplot2)

# Create a correlation matrix
corr_mat <- round(cor(college_df), 2)

# Melt the correlation matrix for plotting
melted_corr_mat <- melt(corr_mat)

ggplot(data = melted_corr_mat, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(width = 1, height = 1) +
  geom_text(aes(label = value), size = 2) +
  labs(title = "Correlation Heatmap") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Gender Count
ggplot(df, aes(x = gender, fill = gender)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
  labs(x = 'Gender', y = 'Count') +
  scale_fill_brewer(palette = 'Pastel1')

# Work Experience
ggplot(df, aes(x = workex, fill = workex)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
  labs(x = 'Work Experience', y = 'Count') +
  scale_fill_brewer(palette = 'Pastel1')  

# Placement Status
ggplot(df, aes(x = status, fill = status)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
  labs(x = 'Placement Status', y = 'Count') + 
  scale_fill_brewer(palette = 'Pastel1')  

# Placement Status by Genders
ggplot(df, aes(x = status, fill = gender)) +
  geom_bar(position = 'dodge') +
  geom_text(stat = 'count', aes(label = after_stat(count)), position = position_dodge(0.9), vjust = -0.5) +
  labs(x = 'Placement Status by Genders', y = 'Count') +
  scale_fill_brewer(palette = 'Pastel1')  

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Set the plot size
options(repr.plot.width = 15, repr.plot.height = 8)

# Create distribution plots
par(mfrow = c(2, 2))

# Secondary Education percentage
ggplot(df, aes(x = ssc_p)) +
  geom_histogram(binwidth = 5, fill = 'lightblue', color = 'black') +
  labs(x = 'Secondary Education percentage', y = 'Frequency')

# Higher Secondary Education percentage
ggplot(df, aes(x = hsc_p)) +
  geom_histogram(binwidth = 5, fill = 'lightgreen', color = 'black') +
  labs(x = 'Higher Secondary Education percentage', y = 'Frequency')

# Undergraduate percentage
ggplot(df, aes(x = degree_p)) +
  geom_histogram(binwidth = 5, fill = 'lightcoral', color = 'black') +
  labs(x = 'Undergraduate percentage', y = 'Frequency')

# MBA percentage
ggplot(df, aes(x = mba_p)) +
  geom_histogram(binwidth = 5, fill = 'lightsalmon', color = 'black') +
  labs(x = 'MBA percentage', y = 'Frequency')

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
college_glm <- glm(status ~ . -salary, data = college_df, family = "binomial", maxit = 1000)
summary(college_glm)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table = step(college_glm )

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(41)
test_error = numeric(10)

for (i in 1:10) {
  sample_indices = sample.int(n = nrow(college_df), size = floor(0.8 * nrow(college_df)), replace = FALSE)
  train = college_df[sample_indices,]
  test = college_df[-sample_indices,]

  college_glm = glm(status ~ gender + ssc_p + hsc_p + degree_p + workex + mba_p + degree_t_Comm_Mgmt,
                     data = train,
                     family = "binomial")

  college_pred = predict.glm(college_glm, newdata = test, type = "response")
  yhat = ifelse(college_pred < 0.5, 'Not Placed', 'Placed')

  conf.test = table(test$status, yhat)
  test_error[i] = (conf.test[1, 2] + conf.test[2, 1]) / nrow(test)
}

mean(test_error)

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
exams.glm2 = glm(status ~ gender + ssc_p + hsc_p + degree_p + workex + mba_p + degree_t_Comm_Mgmt,
                     data = train,
                     family = "binomial")
summary(exams.glm2)

