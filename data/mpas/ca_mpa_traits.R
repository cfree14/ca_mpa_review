


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
gisdir <- "data/gis_data/processed"
plotdir <- "data/mpas/figures"

# Read data
data <- readRDS(file.path(outdir, "CA_MPA_traits.Rds"))

# Read GIS data
mpas <- readRDS(file.path(gisdir, "CA_MPA_polygons.Rds"))


# Build data
################################################################################

# Coverage stats
range(data$implementation_year)
years <- 2003:2022
coverage <- purrr::map_df(years, function(x){

  # Build data
  ydata <- data %>%
    filter(implementation_year<=x) %>%
    mutate(year=x) %>%
    group_by(year, designation, bioregion) %>%
    summarize(n=n(),
              area_km2=sum(area_km2),
              pcover=area_km2/14279.68) %>%
    ungroup()

})


# Map
################################################################################

# Which is missing from Jacob's data?
mpas_use_gis <- mpas %>%
  sf::st_drop_geometry() %>%
  filter(type %in% c("SMCA", "SMCA (No-Take)", "SMR", "SMRMA")) %>%
  pull(name) %>% gsub("\\(|\\)|No-Take", "", .) %>% stringr::str_trim()
mpas_use_gis[!mpas_use_gis %in% data$name]

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Setup theme
map_theme <-  theme(axis.text=element_text(size=7),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = c(0.8, 0.8),
                   legend.key = element_rect(fill=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
  # Plot MPAs
  geom_sf(data=mpas, mapping=aes(fill=type), color=NA) +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_discrete(name="Designation") +
  # Axis
  scale_y_continuous(breaks=seq(30, 42, 1)) +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + map_theme +
  theme(legend.position=c(0.8, 0.8))

# Export
ggsave(g, filename=file.path(plotdir, "ca_mpa_map.png"),
       width=4.5, height=6.5, units="in", dpi=600)

# Sample size
table(mpas$type)
n_distinct(mpas$name)


# Evolution
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Number of MPAs
g1 <- ggplot(coverage, mapping=aes(x=year, y=n, fill=bioregion)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="", y="Number of MPAs") +
  scale_x_continuous(lim=c(2000, NA)) +
  scale_y_continuous(lim=c(0, NA)) +
  # Legend
  scale_fill_discrete(name="Region") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.2, 0.8))
g1

# Area of MPAs
g2 <- ggplot(coverage, mapping=aes(x=year, y=area_km2, fill=bioregion)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="", y="Area (sqkm)") +
  scale_x_continuous(lim=c(2000, NA)) +
  scale_y_continuous(lim=c(0, NA)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g2

# Percent of state waters
g3 <- ggplot(coverage, mapping=aes(x=year, y=pcover, fill=bioregion)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="", y="Percent of state waters") +
  scale_x_continuous(lim=c(2000, NA)) +
  scale_y_continuous(lim=c(0, NA), labels=scales::percent) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1)

# Export
ggsave(g, filename=file.path(plotdir, "ca_mpa_evolution.png"),
       width=6.5, height=2.25, units="in", dpi=600)


# Size
################################################################################

# Number of MPAs
g1 <- ggplot(data, mapping=aes(x=area_km2, fill=bioregion)) +
  geom_histogram() +
  # Labels
  labs(x="Area (sqkm)", y="Number of MPAs") +
  # Legend
  scale_fill_discrete(name="Region") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.8, 0.8))
g1

# Area of MPAs
g2 <- ggplot(data, mapping=aes(x=shore_span_km, fill=bioregion)) +
  geom_histogram() +
  # Labels
  labs(x="Shoreline span (km)", y="Number of MPAs") +
  # Legend
  scale_fill_discrete(name="Region") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g2

# Depth range
g3 <- ggplot(data) +
  geom_segment(mapping=aes(y=reorder(name_short, max_depth_m),
                           yend=reorder(name_short, max_depth_m),
                           x=min_depth_m, xend=max_depth_m)) +
  # Labels
  labs(x="Depth profile (m)", y="MPA") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.ticks.y=element_blank(),
        axis.text.y=element_blank())
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1)

# Export
ggsave(g, filename=file.path(plotdir, "ca_mpa_size.png"),
       width=6.5, height=2.25, units="in", dpi=600)


# Habitat area
################################################################################

# New theme
hab_theme <- theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   axis.title.y=element_blank(),
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
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# New theme
hab_theme_wide <- theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   axis.title.x=element_blank(),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=5),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
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
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Column names
colnames1 <- colnames(data)
hab_area_names <- colnames1[grepl("km2", colnames1)]

# Build habitat data
hab_data <- data %>%
  # Simplify
  select(bioregion, name, name_short, designation, area_km2, hab_area_names) %>%
  # Gather
  gather(key="habitat", value="habitat_km2", 6:ncol(.)) %>%
  # Add proportion
  mutate(habitat_prop=habitat_km2/area_km2)

# Plot data
g1 <- ggplot(hab_data, aes(y=name_short, x=habitat_prop, fill=habitat)) +
  # Facet
  facet_grid(bioregion~., space="free_y", scales="free_y") +
  # Bar plot
  geom_bar(stat="identity") +
  # Reference line
  geom_vline(xintercept=1) +
  # Labels
  labs(x="Proportion of MPA area", y="") +
  # Legend
  scale_fill_discrete(name="Habitat type") +
  # Theme
  theme_bw() + hab_theme
g1

# Export
ggsave(g1, filename=file.path(plotdir, "ca_mpa_habitat_area.png"),
       width=6.5, height=8, units="in", dpi=600)



# Habitat - coast
################################################################################

# Column names
colnames1 <- colnames(data)
hab_area_names_full <- colnames1[grepl("km", colnames1)]
hab_area_names_coast <- hab_area_names_full[!hab_area_names_full %in% hab_area_names]

# Jacob told me to use these columns:
# Sandy_beach_km
# Rocky_inter_km
# Coastal_marsh_km
# Tidal_flats_km
# Hardened_armored_shore_km

# Build habitat data
hab_data_coast <- data %>%
  # Simplify
  select(bioregion, name, name_short, designation,
         sandy_beach_km, rocky_inter_km, coastal_marsh_km, tidal_flats_km, hardened_armored_shore_km) %>%
  # Gather
  gather(key="habitat", value="habitat_km", 5:ncol(.)) %>%
  # Recode habitat
  mutate(habitat=recode(habitat,
                        "sandy_beach_km"="Sandy beach",
                        "rocky_inter_km"="Rocky intertidal",
                        "coastal_marsh_km"="Coastal marsh",
                        "tidal_flats_km"="Tidal flats",
                        "hardened_armored_shore_km"="Armored shore")) %>%
  # Add proportion
  group_by(name) %>%
  mutate(habitat_prop=habitat_km/sum(habitat_km))

# Plot data
g2 <- ggplot(hab_data_coast, aes(y=name_short, x=habitat_prop, fill=habitat)) +
  # Facet
  facet_grid(bioregion~., space="free_y", scales="free_y") +
  # Bar plot
  geom_bar(stat="identity", color="grey30", lwd=0.1) +
  # Reference line
  geom_vline(xintercept=1) +
  # Labels
  labs(x="Percentage of coastline", y="") +
  scale_x_continuous(breaks=seq(0,1,0.2), labels=scales::percent) +
  # Legend
  scale_fill_manual(name="Habitat type",
                    values=c("grey20", "darkgreen", "lightskyblue3", "gold1", "tan4")) +
  # Theme
  theme_bw() + hab_theme
g2

# Export
ggsave(g2, filename=file.path(plotdir, "ca_mpa_habitat_coast.png"),
       width=6.5, height=8, units="in", dpi=600)


# Plot data
g2 <- ggplot(hab_data_coast, aes(x=name_short, y=habitat_prop, fill=habitat)) +
  # Facet
  facet_grid(.~bioregion, space="free_x", scales="free_x") +
  # Bar plot
  geom_bar(stat="identity", color="grey30", lwd=0.1) +
  # Labels
  labs(y="Percentage of coastline", x="") +
  scale_y_continuous(breaks=seq(0,1,0.2), labels=scales::percent) +
  # Legend
  scale_fill_manual(name="Habitat type",
                    values=c("grey20", "darkgreen", "lightskyblue3", "gold1", "tan4")) +
  # Theme
  theme_bw() + hab_theme_wide +
  theme(legend.position = "bottom")
g2

# Export
ggsave(g2, filename=file.path(plotdir, "ca_mpa_habitat_coast_wide.png"),
       width=8, height=4, units="in", dpi=600)



