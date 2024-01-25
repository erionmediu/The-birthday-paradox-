rm(list = ls())
filepath = rstudioapi::getSourceEditorContext()$path
dirpath = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dirpath)
dirpath

# Loading libraries necessary ----


if (!("dplyr" %in% installed.packages())) {
  install.packages("dplyr")
}
library(dplyr)
if (!("lubridate" %in% installed.packages())) {
  install.packages("lubridate")
}
library(lubridate)
library(ggplot2)




# Loading and preparing the data. ---- 


data1= read.csv(file = 'Basic_Stats.csv')
data2= read.csv('congress-terms.csv')

prep_pipeline <- function(data1, data2) {

  # Convert column names to lowercase
  names(data1) <- tolower(names(data1))
  names(data2) <- tolower(names(data2))
  
  data1$birthday = dmy(data1$birthday)
  data2$birthday = ymd(data2$birthday)
  # failed to parse = symbolize NA data points
  
  # Select 'birthday' column if it exists
  if ('birthday' %in% names(data1)) {
    data1 <- data1 %>% select(birthday)
  } else {
    stop('No birthday column found in data1')
  }
  
  if ('birthday' %in% names(data2)) {
    data2 <- data2 %>% select(birthday)
  } else {
    stop('No birthday column found in data2')
  }
  
  # Merge the datasets row-wise
  merged_data <- bind_rows(data1, data2)
  
  # Remove NA dataset is representative
  merged_data = na.omit(merged_data)
  
  # Add an ID column
  merged_data <- merged_data %>% mutate(ID = row_number())
  
  
  merged_data <- merged_data %>%
    mutate(
      day = day(birthday),
      month = month(birthday),
      year = year(birthday)
    )
  
  return(merged_data)
  
}

data = prep_pipeline(data1, data2)
rm(data1 , data2)
summary(data)


# Plotting the data ---- 

## Most popular birth month: 

data$month <- month(data$birthday, label = TRUE, abbr = TRUE)
ggplot(data, aes(x = month)) +
  geom_bar(aes(fill = month)) +
  theme_minimal() +
  labs(title = "Popularity of Birth Months",
       x = "Month",
       y = "Count of Birthdays") +
  scale_fill_brewer(palette = "Set3") +  
  theme(legend.position = "none")  


## Most popular days to be born in the data

data$day <- factor(data$day)
ggplot(data, aes(x = day)) +
  geom_bar(aes(fill = factor(day))) +  # Use day as a factor for different colors
  scale_fill_viridis_d() +  # Optional: A color scale that provides distinct colors
  theme_minimal() +
  labs(title = "Most Popular Birth Days",
       x = "Day of the Month",
       y = "Count of Birthdays") +
  theme(legend.position = "none")  # Removes the legend



## Birthday distribution in the data throught the years:
ggplot(data, aes(x = birthday)) +
  geom_histogram(binwidth = 700, aes(fill = ..count..)) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") + # Formatting x-axis
  theme_minimal() +
  labs(title = "Distribution of Birthdays in the Data",
       x = "Birthday",
       y = "Frequency") +
  scale_fill_viridis_c()  # Adds a color gradient



### Calculating probabilities ----


  run_simulation <- function(data, group_size, num_simulations = 100, min_shared = 2) {
    count_shared_birthday <- function(min_shared, birthdays) {
      any(table(birthdays) >= min_shared)
    }
    
    successful_simulations <- 0
    
    for (i in 1:num_simulations) {
      sampled_birthdays <- sample(data$birthday, group_size, replace = TRUE)
      if (count_shared_birthday(min_shared, sampled_birthdays)) {
        successful_simulations <- successful_simulations + 1
      }
    }
    
    probability = successful_simulations / num_simulations
    return(probability)
  }

# Define a range of group sizes
group_sizes <- 2:50  # Modify this as needed

# Store probabilities for each group size
probabilities <- sapply(group_sizes, function(size) {
  run_simulation(data, size, num_simulations = 1000, min_shared = 2)
})

# Combine group sizes with their probabilities
results <- data.frame(Group_Size = group_sizes, Probability = probabilities)

# View the results
print(results)




run_simulation <- function(data, group_size, num_simulations = 10000, min_shared) {
  count_shared_birthday <- function(min_shared, birthdays) {
    any(table(birthdays) >= min_shared)
  }
  
  successful_simulations <- 0
  
  for (i in 1:num_simulations) {
    sampled_birthdays <- sample(data$birthday, group_size, replace = TRUE)
    if (count_shared_birthday(min_shared, sampled_birthdays)) {
      successful_simulations <- successful_simulations + 1
    }
  }
  
  probability = successful_simulations / num_simulations
  return(probability)
}


# Define a range of group sizes and min_shared values
group_sizes <- 23:50  # Modify this as needed
min_shared_values <- 2:5  # From 2 to 10 identical birthdays

# Store probabilities for each combination of group size and min_shared
results <- expand.grid(Group_Size = group_sizes, Min_Shared = min_shared_values)
results$Probability <- mapply(run_simulation, data = list(data), 
                              group_size = results$Group_Size, 
                              min_shared = results$Min_Shared)

# Reshape results for better readability
library(reshape2)
results_wide <- dcast(results, Group_Size ~ Min_Shared, value.var = "Probability")
names(results_wide)[-1] <- paste0("prob", 2:5)

# View the results
print(results_wide)




