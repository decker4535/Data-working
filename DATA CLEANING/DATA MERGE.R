library(dplyr)
library(lubridate)
library(stringr)
# install.packages("readr")
# install.packages("tidyr")
library(readr)
library(tidyr)

# turn csv files into data frames
df1 <- read_csv("Zip Means as csv/0820 means.csv")
df2 <- read_csv("Zip Means as csv/0821 means.csv")
df3 <- read_csv("Zip Means as csv/0822 means.csv")
df4 <- read_csv("Zip Means as csv/0823 means.csv")
df5 <- read_csv("Zip Means as csv/0824 means.csv")
df6 <- read_csv("Zip Means as csv/0825 means.csv")
df7 <- read_csv("Zip Means as csv/0826 means.csv")
df8 <- read_csv("Zip Means as csv/0827 means.csv")
df9 <- read_csv("Zip Means as csv/0828 means.csv")
df10 <- read_csv("Zip Means as csv/0829 means.csv")
df11 <- read_csv("Zip Means as csv/0830 means.csv")
df12 <- read_csv("Zip Means as csv/0831 means.csv")

#remove objectid and globalid columns from all data frames
df1 <- df1 %>% select(-c(OBJECTID, GlobalID))
df2 <- df2 %>% select(-c(OBJECTID, GlobalID))
df3 <- df3 %>% select(-c(OBJECTID, GlobalID))
df4 <- df4 %>% select(-c(OBJECTID, GlobalID))
df5 <- df5 %>% select(-c(OBJECTID, GlobalID))
df6 <- df6 %>% select(-c(OBJECTID, GlobalID))
df7 <- df7 %>% select(-c(OBJECTID, GlobalID))
df8 <- df8 %>% select(-c(OBJECTID, GlobalID))
df9 <- df9 %>% select(-c(OBJECTID, GlobalID))
df10 <- df10 %>% select(-c(OBJECTID, GlobalID))
df11 <- df11 %>% select(-c(OBJECTID, GlobalID))
df12 <- df12 %>% select(-c(OBJECTID, GlobalID))

# merge data frames into one with all zip codes and means without duplicating objectid and globalid
merged = merge(df1, df2, by = "Zip_Code", all = TRUE)
merged = merge(merged, df3, by = "Zip_Code", all = TRUE)
merged = merge(merged, df4, by = "Zip_Code", all = TRUE)
merged = merge(merged, df5, by = "Zip_Code", all = TRUE)
merged = merge(merged, df6, by = "Zip_Code", all = TRUE)
merged = merge(merged, df7, by = "Zip_Code", all = TRUE)
merged = merge(merged, df8, by = "Zip_Code", all = TRUE)
merged = merge(merged, df9, by = "Zip_Code", all = TRUE)
merged = merge(merged, df10, by = "Zip_Code", all = TRUE)
merged = merge(merged, df11, by = "Zip_Code", all = TRUE)
merged = merge(merged, df12, by = "Zip_Code", all = TRUE)

# sum across variables _0820fixedmean to _0831fixedmean for each zip code and create a new variable called "Total_Mean"
merged <- merged %>%
  rowwise() %>%
  mutate(Total_Mean = sum(c_across(starts_with("_08")), na.rm = TRUE)) %>%
  ungroup()

# visualize total mean by zip using a bar chart
install.packages("ggplot2")
library(ggplot2)
ggplot(merged, aes(x = Zip_Code, y = Total_Mean)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Total Mean by Zip Code", x = "Zip Code", y = "Total Mean")


# save merged data frame as csv file
write_csv(merged, "merged_zip_means.csv")


# add FEMA data as df
fema <- read_csv("IndividualAssistanceMultipleLossFloodProperties.csv")

# remove all observations without disasterNumber 4332
fema <- fema %>% filter(disasterNumber == 4332)

# use fema data to create a new data frame with total observations of each zip code in the fema data frame and merge with merged data frame
fema_zip_counts <- fema %>%
  group_by(damagedZipCode) %>%
  summarise(count = n())
# rename damagedZipCode to Zip_Code in fema_zip_counts
fema_zip_counts <- fema_zip_counts %>% rename(Zip_Code = damagedZipCode)
merged <- merge(merged, fema_zip_counts, by = "Zip_Code", all.x = TRUE)

# visualize count of FEMA observations by rain mean total mean using a scatter plot
ggplot(merged, aes(x = Total_Mean, y = count)) +
  geom_point() +
  labs(title = "FEMA Observations by Rain Mean Total Mean", x = "Rain Mean Total Mean", y = "FEMA Observations Count")

# Save plot as png file
ggsave("fema_observations_scatter.png")

# run regression analysis to see if there is a relationship between rain mean total mean and FEMA observations count
model <- lm(count ~ Total_Mean, data = merged)
summary(model)
model2 = lm(Total_Mean ~ count, data = merged)
summary(model2)

# visualize count of FEMA observations by rain mean total mean using a scatter plot with a regression line
ggplot(merged, aes(x = Total_Mean, y = count)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "FEMA Observations by Rain Mean Total Mean with Regression Line", x = "Rain Mean Total Mean", y = "FEMA Observations Count")

  # add realtor data as df
realtor <- read_csv("RDC_Inventory_Core_Metrics_Zip_History.csv")

# remove observations not in zip codes in merged data frame
realtor <- realtor %>% filter(postal_code %in% merged$Zip_Code)

# clean data to just have postal_code, median_listing_price, and month_date_yyyymm variables
realtor <- realtor %>% select(postal_code, median_listing_price, month_date_yyyymm)

# rename postal_code to Zip_Code in realtor data frame
realtor <- realtor %>% rename(Zip_Code = postal_code)

# merge realtor data frame with merged data frame
merged <- merge(merged, realtor, by = "Zip_Code", all.x = TRUE)

# remove rain variables _0820fixedmean to _0831fixedmean from merged data frame
merged <- merged %>% select(-c(starts_with("_08")))

# use count to create a new variable called "FEMA_Impact" that categorizes zip codes into "High Impact" (count > 50), and "Low Impact" (count < 50)
merged <- merged %>%
  mutate(FEMA_Impact = case_when(
    count > 50 ~ "High Impact",
    count < 50 ~ "Low Impact",
    TRUE ~ NA_character_
  ))

# plot median listing price over time for each different FEMA impact category using a line graph. Combining all zips of same FEMA impact category and taking the average median listing price for each month_date_yyyymm variable
realtor_impact <- merged %>%
  group_by(FEMA_Impact, month_date_yyyymm) %>%
  summarise(avg_median_listing_price = mean(median_listing_price, na.rm = TRUE))

# visualize with line graph
ggplot(realtor_impact, aes(x = month_date_yyyymm, y = avg_median_listing_price, color = FEMA_Impact)) +
  geom_line() +
  labs(title = "Average Median Listing Price Over Time by FEMA Impact Category", x = "Month", y = "Average Median Listing Price") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# save the plot as a png file
ggsave("median_listing_price_over_time.png")

# Create a DiD regression to see if their is a difference in median listing price before 201708 and after for each FEMA impact category
# create a new variable called "Post_Disaster" that is 0 for month_date_yyyymm before 201708 and 1 for month_date_yyyymm after 201708
realtor_impact <- realtor_impact %>%
  mutate(Post_Disaster = ifelse(month_date_yyyymm >= 201708, 1, 0))

# turn High Impact, and Low Impact into 1,0 for regression analysis
realtor_impact <- realtor_impact %>%
  mutate(FEMA_Impact_Num = case_when(
    FEMA_Impact == "High Impact" ~ 1,
    FEMA_Impact == "Low Impact" ~ 0,
    TRUE ~ NA_real_
  ))
# run DiD regression with interaction term between Post_Disaster and FEMA_Impact_Num
model_did <- lm(avg_median_listing_price ~ Post_Disaster * FEMA_Impact_Num, data = realtor_impact)
summary(model_did)

# use rain total mean to IV for FEMA impact category in DiD regression
model_iv <- lm(avg_median_listing_price ~ Post_Disaster * FEMA_Impact_Num + Total_Mean, data = realtor_impact)
summary(model_iv)

# use log of median listing price as dependent variable in DiD regression
model_did_log <- lm(log(avg_median_listing_price) ~ Post_Disaster * FEMA_Impact_Num, data = realtor_impact)
summary(model_did_log)
 
# add post diaster to merged
merged <- merged %>%
  mutate(Post_Disaster = ifelse(month_date_yyyymm >= 201708, 1, 0))
   # add FEMA impact num to merged
merged <- merged %>%
  mutate(FEMA_Impact_Num = case_when(
    FEMA_Impact == "High Impact" ~ 1,
    FEMA_Impact == "Low Impact" ~ 0,
    TRUE ~ NA_real_
  ))
  # run it
model_did_log_control <- lm(log(median_listing_price) ~ Post_Disaster * FEMA_Impact_Num + Total_Mean, data = merged)
summary(model_did_log_control)
# Regress with total mean instead of FEMA impact category in DiD regression
model_did_total_mean <- lm(log(avg_median_listing_price) ~ Post_Disaster * Total_Mean, data = realtor_impact)
summary(model_did_total_mean)

# create variables to use as controls in DiD regression such as month fixed effects and zip code fixed effects

# add control variables to DiD regression such as month fixed effects and zip code fixed effects
model_did_controls <- lm(log(avg_median_listing_price) ~ Post_Disaster * FEMA_Impact_Num + factor(month_date_yyyymm) + factor(Zip_Code), data = merged)
summary(model_did_controls)

# Save final merged data frame as csv file
write_csv(merged, "final_merged_data.csv")
