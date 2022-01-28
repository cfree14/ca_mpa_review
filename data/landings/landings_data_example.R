


# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(lubridate)
library(tidyverse)
library(countrycode)

# Directories
indir <- "data/gis_data/raw"
outdir <- "data/gis_data/processed"
plotdir <- "data/gis_data/figures"

# Read confidential data
data_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/wc_cc_synthesis/data/landings/cdfw/confidential/processed/CDFW_2000_2020_monthly_landings_by_port_block_species.Rds") %>%
  # Add date
  mutate(date=paste(year, month_code, 1) %>% ymd())

# Blocks
blocks <- wcfish::blocks


# Build data
################################################################################

# Subset example data
data_ex <- data_orig %>%
  # Reduce to species and dates
  filter(comm_name %in% c("Lingcod", "Kelp greenling", "California halibut", "Chilipepper rockfish") & date %in% c(ymd("2015-05-01"), ymd("2018-05-01")) & !is.na(date)) %>%
  select(date, block_type, block_id, comm_name, landings_lb, value_usd) %>%
  mutate(block_id=as.numeric(block_id))  %>%
  filter(block_type!="Offshore") %>%
  # Scale to maximum
  group_by(comm_name, date) %>%
  mutate(value_prop=value_usd/sum(value_usd))

# Add landings to blocks (lazy version)
key <- data_ex %>%
  select(comm_name, date) %>%
  unique()
blocks1 <- purrr::map_df(1:nrow(key), function(x){

  # Subset data
  spp <- key$comm_name[x]
  date1 <- key$date[x]
  sdata <- data_ex %>%
    filter(comm_name==spp & date==date1)
  blocks_d <- blocks %>%
    left_join(sdata) %>%
    mutate(comm_name=spp,
           date=date1)

})


# Plot data
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Sonoma-Mendocino county line
son_mend_county <- 38+46.125/60

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=7),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = "bottom",
                   legend.key = element_rect(fill=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot() +
  # Facet
  facet_grid(date~comm_name) +
  # Plot blocks
  geom_sf(data=blocks1, mapping=aes(fill=value_prop), color="grey40", lwd=0.1) +
  # Sononoma-Mendocino county line
  geom_hline(yintercept=son_mend_county) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
  # Plot MPAs
  # geom_sf(data=data, fill="black", color=NA) +
  # Labels
  labs(x="", y="") +
  # Axis
  scale_x_continuous(breaks=seq(-124, -116, 2)) +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Legend
  scale_fill_gradientn(name="Proportion of\nmonthly value", colors=RColorBrewer::brewer.pal(9, "YlOrRd")[3:9], na.value="white") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
#g

# Export
ggsave(g, filename=file.path(plotdir, "CA_monthly_block_landings_example.png"),
       width=6.5, height=6, units="in", dpi=600)





