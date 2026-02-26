# Week 1: Initial Data Exploration ====
# Author: [Lee Fowler]
# Date: [13/02/2026]




# Load packages ====
library(tidyverse)

library(here)
library(naniar)
library(janitor)
library(skimr)

# Load data ====
mosquito_egg_raw <- read_csv(here("data", "mosquito_egg_data.csv"),
                             name_repair = janitor::make_clean_names)
  # 205 rows, 9 columns, 

# Basic overview ====
glimpse(mosquito_egg_raw)
summary(mosquito_egg_raw) #egg hatched min 2.0, max 75.0,median 24.0, mean 25.5, 17 NA's
skim(mosquito_egg_raw) # age and eggs laid normally distributed, body mass negative skew, eggs hatched positive skew

# React table====
# view interactive table of data
view(mosquito_egg_raw)


# Counts by site and treatment====

mosquito_egg_raw |> 
  group_by(site, treatment) |> 
  summarise(n = n())

# Observations ====
# Your observations (add as comments below):
# - What biological system is this?
#   mosquitos; eggs laid and hatched at 3 sites under different treatments (Low, high, medium dose, or control)
# - What's being measured?
#   body mass, eggs laid, eggs hatched
# - How many observations?
#   205 rows, 9 columns
# - Anything surprising?
#
# - Any obvious problems?
# 16 NA values in eggs laid, 17 NA values in eggs hatched, 15 NA values in body mass, NA values present in collector name, inconsistent capitalization in treatment




# Tutorial applications checks  ####
# checking for missing data####
library(skimr)
skimr::skim(mosquito_egg_raw)

library(naniar)
naniar::vis_miss(mosquito_egg_raw) # visual representation of missing data

naniar::gg_miss_upset(mosquito_egg_raw) #visual representation of intersection of missing data (between multiple variables)



# checking for duplicates####

mosquito_egg_raw |> 
  filter(duplicated(across(everything())))
sum() 

# use ! to keep only unduplicated rows
mosquito_egg_raw |> 
  filter(!duplicated(across(everything())))

mos |> 
  summarise(
    n = n(),
    n_distinct(individual_id)
  )

# checking data consistency ####

# check for negative values
mosquito_egg_raw|>
  filter(if_any(c(body_mass_mg, age_days, eggs_laid, eggs_hatched), ~.<0))

# check for zero or negative values where zero doesn't make sense
mosquito_egg_raw|>
  filter(body_mass_mg <= 0)

# check range for body_mass_mg
mosquito_egg_raw|>
  summarise(
    min_mass = min(body_mass_mg, na.rm = TRUE),
    max_mass = max(body_mass_mg, na.rm = TRUE)
  )



# FIX 1: Site names are inconsistent; capitalization, and spaces vs _ vs - ==== ####

# Show the problem:

# Print only unique character strings in this variable
mosquito_egg_raw |>  distinct(site)


# Fix it:


  # use stringr, mutate and case_when to make site names consistent 

mosquito_egg_data <- mosquito_egg_raw |> mutate(site = stringr::str_to_upper(site))

mosquito_egg_data<- mosquito_egg_data |>  mutate(site = case_when(
  site == "SITE A" ~ "A",
  site == "SITE-A" ~ "A",
  site == "SITE_A" ~ "A",
  .default = as.character(site)
)
)
  
mosquito_egg_data<- mosquito_egg_data |>  mutate(site = case_when(
  site == "SITE B" ~ "B",
  site == "SITE-B" ~ "B",
  site == "SITE_B" ~ "B",
  .default = as.character(site)
)
)

mosquito_egg_data<- mosquito_egg_data |>  mutate(site = case_when(
  site == "SITE C" ~ "C",
  site == "SITE-C" ~ "C",
  site == "SITE_C" ~ "C",
  .default = as.character(site)
)
)

  # Verify it worked:
mosquito_egg_data |>  distinct(site)
  
  # What changed and why it matters:
  # Site names are all consistent and data can now be filtered by site
  
  

# FIX 2: treatment names inconsistent  ==== ####

# Show the problem:
mosquito_egg_data |>  distinct(treatment)


# Fix it:
mosquito_egg_data <- mosquito_egg_data|>  mutate(treatment = stringr::str_to_sentence(treatment))

mosquito_egg_data <- mosquito_egg_data|> 
  mutate(treatment = stringr::str_c("Medium", "dose", sep = "_") |> 
  mutate(treatment = stringr::str_c("High", "dose", sep = "_")) |> 
  mutate(treatment = stringr::str_c("Low", "dose", sep = "_")))
  
  
  # Verify it worked:
mosquito_egg_data |>  distinct(treatment)
  
  # What changed and why it matters:
  # treatment names now have the same capitalisation and spacing style
  

# FIX 3: Inconsistent/impossible negative values in body_mass ####
# Show the problem:

# check for negative values
mosquito_egg_data|>
  filter(if_any(c(body_mass_mg, age_days, eggs_laid, eggs_hatched), ~.<0))
# check range
skimr::skim(mosquito_egg_data)


# Fix it:

# multiply negative values by -1
mosquito_egg_data <- mosquito_egg_data |>
  mutate(body_mass_mg = if_else(body_mass_mg < 0, body_mass_mg * -1, body_mass_mg))



# Verify it worked:

mosquito_egg_data|>
  filter(if_any(c(body_mass_mg, age_days, eggs_laid, eggs_hatched), ~.<0))
# check range
skimr::skim(mosquito_egg_data)


# What changed and why it matters:
# All body mass values are now positive.

# FIX 4: Duplicate rows -- incomplete ####
# Show the problem:

# show duplicate rows, in this case 10 rows (or 5 pairs of rows)

mosquito_egg_data |> 
  get_dupes()

# count unique entries
mosquito_egg_data |> 
  summarise(
    n = n(),
    n_distinct(female_id)
  )

# Fix it:

# Keep only unduplicated data - not sure why this isn't workning ????
mosquito_egg_data <- mosquito_egg_data |> 
  mutate( distinct(mosquito_egg_data))


# Verify it worked:




# What changed and why it matters:




