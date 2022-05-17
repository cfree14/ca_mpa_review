


# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(lubridate)
library(tidyverse)
library(countrycode)

# Directories
indir <- "data/mpas/raw"
outdir <- "data/mpas/processed"
plotdir <- "data/mpas/figures"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "CA_MPA_variables.xlsx"), na = c("NA", "."), trim_ws = T)

# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(area_km2=size_km2) %>%
  # Add implementation year
  mutate(implementation_date=ymd(implementation_date),
         implementation_year=year(implementation_date)) %>%
  # Update region
  mutate(bioregion=recode_factor(bioregion,
                                 "SoCal"="Southern",
                                 "CenCal"="Central",
                                 "NorCal"="Northern")) %>%
  # Shorten name
  mutate(name_short=gsub(" SMCA| SMR| SMRMA", "", name)) %>%
  # Fix duplicated short names
  group_by(name_short) %>%
  mutate(n=n()) %>%
  ungroup() %>%
  mutate(name_short=ifelse(n==2, name, name_short)) %>%
  select(-n) %>%
  # Order protection
  mutate(protection=stringr::str_to_sentence(protection),
         protection=recode(protection,
                           "Moderate low"="Moderate-low",
                           "Moderate high"="Moderate-high"),
         protection=factor(protection,
                           levels=c("Low", "Moderate-low", "Moderate", "Moderate-high",
                                    "High", "Very high"))) %>%
  # Arrange
  select(bioregion,
         name, name_short,
         designation, cluster, tier, coastal_estuary, latitude,
         implementation_date, implementation_year, age_yr, everything())

# Confirm unique IDS
anyDuplicated(data$name)
freeR::which_duplicated(data$name_short)

# Inspect
str(data)
table(data$bioregion)
table(data$coastal_estuary)
table(data$protection)
table(data$protection_score)
table(data$take)
table(data$military_base)

# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "CA_MPA_traits.Rds"))





