library(readr)
library(dplyr)
library(janitor)

# -----------------------------
# Load IA Multiple Loss Flood Properties data
# -----------------------------
ia_df <- read_csv("IndividualAssistanceMultipleLossFloodProperties.csv") %>%
  clean_names()

# Optional structure check
glimpse(ia_df)

# -----------------------------
# Total number of claims for disaster code 4332
# -----------------------------
total_claims_4332 <- ia_df %>%
  filter(disaster_number == 4332) %>%
  summarise(total_claims = n())

total_claims_4332

library(readr)
library(dplyr)
library(janitor)

# -----------------------------
# Load IA Multiple Loss Flood Properties data
# -----------------------------
ia_df <- read_csv("IndividualAssistanceMultipleLossFloodProperties.csv") %>%
  clean_names()

# Optional structure check
glimpse(ia_df)

# -----------------------------
# Total number of claims for disaster code 4332
# -----------------------------
total_claims_4332 <- ia_df %>%
  filter(disaster_number == 4332) %>%
  summarise(total_claims = n())

total_claims_4332

# -----------------------------
# Total number of claims for disaster code 4332 in Texas (TX)
total_claims_4332_tx <- ia_df %>%
  filter(
    disaster_number == 4332,
    state_abbreviation == "TX"   # use state_code == "TX" if that is the column name
  ) %>%
  summarise(total_claims = n())

total_claims_4332_tx


ia_sub <- ia_df %>%
  filter(disaster_number == 4332) %>%
  select(
    disaster_number, declaration_date, fips_state_code, fips_county_code,
    place_code, region, state_abbreviation, county, city, damaged_zip_code,
    high_risk_property_type, residence_type, flood_insurance, destroyed,
    foundation_type, water_level, high_water_location, flood_damage,
    number_of_losses, latitude, longitude, id
  )
# 1. ZIP-code level summary
# -----------------------------
zip_summary <- ia_sub %>%
  filter(!is.na(damaged_zip_code)) %>%
  group_by(state_abbreviation, damaged_zip_code) %>%
  summarise(
    n_claims = n(),
    n_unique_properties = n_distinct(id),
    avg_losses = mean(number_of_losses, na.rm = TRUE),
    pct_destroyed = mean(destroyed == "Yes", na.rm = TRUE),
    pct_insured = mean(flood_insurance == "Yes", na.rm = TRUE),
    avg_flood_damage = mean(flood_damage, na.rm = TRUE),
    avg_water_level = mean(water_level, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_claims))

# -----------------------------
# 2. County-level summary (FIPS-based)
# -----------------------------
county_summary <- ia_sub %>%
  group_by(fips_state_code, fips_county_code, county) %>%
  summarise(
    n_claims = n(),
    n_zips = n_distinct(damaged_zip_code),
    pct_high_risk = mean(high_risk_property_type == "Yes", na.rm = TRUE),
    pct_destroyed = mean(destroyed == "Yes", na.rm = TRUE),
    avg_losses = mean(number_of_losses, na.rm = TRUE),
    avg_flood_damage = mean(flood_damage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_claims))

# -----------------------------
# 3. City-level summary
# -----------------------------
city_summary <- ia_sub %>%
  filter(!is.na(city)) %>%
  group_by(state_abbreviation, county, city) %>%
  summarise(
    n_claims = n(),
    pct_insured = mean(flood_insurance == "Yes", na.rm = TRUE),
    pct_destroyed = mean(destroyed == "Yes", na.rm = TRUE),
    avg_flood_damage = mean(flood_damage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_claims))

# -----------------------------
# 4. Exposure-type summary (risk & structure)
# -----------------------------
structure_summary <- ia_sub %>%
  group_by(residence_type, foundation_type) %>%
  summarise(
    n_claims = n(),
    pct_high_risk = mean(high_risk_property_type == "Yes", na.rm = TRUE),
    avg_flood_damage = mean(flood_damage, na.rm = TRUE),
    avg_water_level = mean(water_level, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_claims))

# -----------------------------
# View outputs
# -----------------------------
zip_summary
county_summary
city_summary
structure_summary


# vizualized using ggplot2
library(ggplot2)
library(ggplot2)

zip_plot_data <- ia_sub %>%
  filter(!is.na(damaged_zip_code)) %>%
  count(damaged_zip_code, sort = TRUE) %>%
  slice_head(n = 20)

ggplot(zip_plot_data,
       aes(x = reorder(damaged_zip_code, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 20 ZIP Codes by IA Flood Claims (Disaster 4332)",
    x = "ZIP Code",
    y = "Number of Claims"
  ) +
  theme_minimal()

ggplot(
  ia_sub %>% filter(!is.na(flood_damage), flood_damage > 0),
  aes(x = flood_damage)
) +
  geom_histogram(bins = 50) +
  scale_x_log10() +
  labs(
    title = "Distribution of Flood Damage per Claim (Log Scale)",
    x = "Flood Damage (log scale)",
    y = "Number of Claims"
  ) +
  theme_minimal()


county_plot_data <- ia_sub %>%
  group_by(county) %>%
  summarise(
    n_claims = n(),
    pct_destroyed = mean(destroyed == "Yes", na.rm = TRUE)
  ) %>%
  filter(n_claims >= 50) %>%   # avoid noisy small counties
  arrange(desc(pct_destroyed))

ggplot(county_plot_data,
       aes(x = reorder(county, pct_destroyed), y = pct_destroyed)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Share of Destroyed Properties by County (Harvey)",
    x = "County",
    y = "Percent Destroyed"
  ) +
  theme_minimal()
