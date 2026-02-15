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
#   mosquitos; eggs laid and hatched under different treatments
# - What's being measured?
#   age, body mass, eggs laid, eggs hatched
# - How many observations?
#   205 rows, 9 columns
# - Anything surprising?
#   
# - Any obvious problems?
# 16 NA values in eggs laid, 17 NA values in eggs hatched, 15 NA values in body mass, NA values present in collector name, inconsistent capitalisation in treatment
