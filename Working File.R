install.packages("tidyverse")
install.packages("janitor")
install.packages("skimr")

library(tidyverse)
library(janitor)
library(skimr)


# Importing data

exchange_raw_df <- read_csv("euro_exchange.csv")
skim_without_charts(exchange_raw_df) # Checking the data types

#--------------------------------------------------------------------------------

### 1. Data cleaning process

## a. Selecting only the unique rows from the data frame.

exchange_clean_df <- unique(exchange_raw_df) # Selects only the unique rows. So, no duplicate rows !

## b. Removal of [] from the column names.

col_names_new <- colnames(exchange_clean_df)
col_names_edit <- str_replace_all(col_names_new, "\\[|\\ ]\\s*", "") 
# First, gsub was used to get rid of [] from the column names but it removed [] partially. 
# Probably, there are some hidden characters in the column names. Therefore, str was used instead. 
# In this, \\ followed by "[" and "]" is used to treat both "[" & "]" as characters, while \\s* is used to remove
# empty space after "]". "|" is used to match either "[" or "]". 
colnames(exchange_clean_df) <- col_names_edit

## c. Change of data types of the columns.

character_col <- sapply(exchange_clean_df, is.character) # Here, sapply means simplify and apply. 
# It is used to check if the data in columns are characters or numeric. It will return TRUE for character
# & FALSE for numeric
exchange_clean_df[, character_col] <- lapply(exchange_clean_df[, character_col], as.numeric) 
# Once you identify the column with characters, you can convert them to numeric using lapply.
# This means list apply. 

## d. Renaming column names to lower snake case for convenience.

exchange_clean_df <- clean_names(exchange_clean_df) 
exchange_clean_df <- rename(exchange_clean_df, date = period_unit)
# Clean_names converts column names to snake case with lower characters

#--------------------------------------------------------------------------------

### 2. Looking for trends in the Euro Exchange for Indian Rupee

exchange_india_summary <- exchange_clean_df %>% 
  summarise(min_india = min(indian_rupee, na.rm = T), max_india = max(indian_rupee, na.rm = T), 
            mean_india = mean(indian_rupee, na.rm = T), std_india = sd(indian_rupee, na.rm = T), 
            min_year = min(date), max_year = max(date))

### 3. Checking for date at which the Indian Rupee touched the maximum against Euro 

max_exchange_date <- exchange_clean_df %>%
  filter(indian_rupee == exchange_india_summary$max_india) %>% 
  select(date)

### 4. Plot of trend of INR against Euro

ggplot(exchange_clean_df, aes(x = date, y = indian_rupee)) + geom_line(color = "#0057e7") + 
  labs(title = "Trend of INR against Euro", x = "Year", y = "INR") + 
  theme(plot.title = element_text(hjust = 0.5, color = "#0057e7"), panel.grid =  element_blank(), 
        panel.background = element_blank(), axis.text.x = element_text(color = "#0057e7"),
        axis.text.y = element_text(color = "#0057e7"), axis.title = element_text(color = "#0057e7")) + 
  scale_y_continuous(limits = c(0, 100)) + scale_x_date(breaks = seq(from = as.Date("1999-01-04"), to = as.Date("2023-05-26"), 
                                                                     by = "4 years"), date_labels = "%Y")
#--------------------------------------------------------------------------------

### 5. Investigation of quarterly trend of INR against Euro

exchange_quarter_df <- exchange_clean_df %>% 
  select(-date) %>% 
  mutate(date_quarter = quarter(exchange_clean_df$date)) %>% 
  group_by(date_quarter) %>% 
  summarise(indian_rupee_quarter = mean(indian_rupee, na.rm = T))

ggplot(exchange_quarter_df, aes(x = date_quarter, y = exchange_quarter_df$indian_rupee_quarter)) + 
  geom_col(fill = "#0057e7") + labs(title = "Quarterly Trend of INR against Euro", x = "Quarter", y = "INR", 
                                    caption = "Q1 (Jan-Mar), Q2 (Apr-Jun), Q3 (Jul-Sep), Q4 (Oct-Dec)") +
  theme(plot.title = element_text(hjust = 0.5, color = "#0057e7"),
        axis.title = element_text(color = "#0057e7"), axis.text.x = element_text(color = "#0057e7"), 
        axis.text.y = element_text(color = "#0057e7"), panel.background = element_blank(), 
        panel.grid = element_blank(), plot.caption = element_text(color = "#0057e7")) + 
  scale_y_continuous(limit = c(0,80))


### 6. Correlation between various India and other countries

correlation_value <- c(round(cor(exchange_clean_df$indian_rupee, exchange_clean_df$us_dollar, use = "complete.obs"), 2), 
                            round(cor(exchange_clean_df$indian_rupee, exchange_clean_df$chinese_yuan_renminbi, use = "complete.obs"), 2),
                            round(cor(exchange_clean_df$indian_rupee, exchange_clean_df$canadian_dollar, use = "complete.obs"), 2), 
                            round(cor(exchange_clean_df$indian_rupee, exchange_clean_df$japanese_yen, use = "complete.obs"), 2))
correlation_countries <- c("USA", "China", "Canada", "Japan")

correlation_with_india_df <- data.frame(correlation_countries, correlation_value)

### 7. ML Models

exchange_model <- exchange_clean_df %>% 
  select(date, indian_rupee) %>% 
  na.omit()

lr_model <- lm(indian_rupee ~ date, data = exchange_model)

summary(lr_model) 


new_date <- as.Date("2026-01-01")
predicted_value <- predict(lr_model, newdata = data.frame(date = new_date))



