


# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
indir <- "data/gis_data/raw"
outdir <- "data/gis_data/processed"
plotdir <- "data/gis_data/figures"

# Read confidential data
data_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/wc_cc_synthesis/data/landings/cdfw/confidential/processed/CDFW_2000_2020_monthly_landings_by_port_block_species.Rds")

# Ports
ports <- wcfish::ports

# Build data
################################################################################

# TO-DO
# Not all ports have lat/long

# Build data
data <- data_orig %>%
  # Group by
  group_by(port_code, port_name, year) %>%
  summarise(landings_lb=sum(landings_lb, na.rm=T),
            value_usd=sum(value_usd), na.rm=T) %>%
  ungroup() %>%
  # Average from 2016-2020
  filter(year>2016) %>%
  group_by(port_code, port_name) %>%
  summarize(landings_lb=mean(landings_lb),
            value_usd=mean(value_usd)) %>%
  ungroup() %>%
  # Filter
  filter(!is.na(port_name)) %>%
  # Add lat/long
  left_join(ports %>% select(port, lat_dd, long_dd), by=c("port_name"="port"))

# Export
write.csv(data, file=file.path(outdir, "2016_2020_average_catch_by_port.csv"), row.names=F)

