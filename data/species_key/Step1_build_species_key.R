

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(rinat)
library(tidyverse)

# Directories
indir <- "data/species_key"

# Read landings data
data_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/consulting/halibut_bycatch/data/landings_receipts/processed/CDFW_2000_2020_landings_receipts.Rds")

# Read species key
spp_key <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/consulting/halibut_bycatch/data/cdfw_keys/processed/CDFW_species_key.Rds")

# Build data
################################################################################

# Average catch
data <- data_orig %>%
  # Annual sum
  group_by(year, species_id, species) %>%
  summarise(landings_lb=sum(landings_lb, na.rm=T)) %>%
  ungroup() %>%
  # Annual average
  group_by(species_id, species) %>%
  summarize(landings_lb=mean(landings_lb)) %>%
  ungroup() %>%
  # Convert
  mutate(landings_kg=measurements::conv_unit(landings_lb, "lbs", "kg"),
         landings_mt=landings_kg/1000) %>%
  # Arrange
  arrange(desc(landings_lb)) %>%
  # Add taxa
  left_join(spp_key %>% select(spp_code_num, comm_name, sci_name), by=c("species_id"="spp_code_num")) %>%
  select(-species) %>%
  # Order taxa
  filter(!is.na(comm_name)) %>%
  mutate(comm_name=factor(comm_name, levels=comm_name))


# Plot data
################################################################################

# Split data
nrow(data)/3
117+116
data1 <- data[1:116,]
data2 <- data[117:233,]
data3 <- data[234:nrow(data),]

# Setup theme
my_theme <-  theme(axis.text=element_text(size=5),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=5),
                   legend.title=element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot(data1, aes(x=landings_lb, y=comm_name)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Annual landings (lbs/yr)", y="") +
  # Axis
  scale_x_continuous(trans="log10", breaks=10^c(0:8),
                     labels=parse(text=paste0("10^", 0:8)),
                     limits=c(0.01, 10^9)) +
  # Theme
  theme_bw() + my_theme
g1

# Plot data
g2 <- ggplot(data2, aes(x=landings_lb, y=comm_name)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Annual landings (lbs/yr)", y="") +
  # Axis
  scale_x_continuous(trans="log10", breaks=10^c(0:8),
                     labels=parse(text=paste0("10^", 0:8)),
                     limits=c(0.01, 10^9)) +
  # Theme
  theme_bw() + my_theme
g2

# Plot data
g3 <- ggplot(data3, aes(x=landings_lb, y=comm_name)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Annual landings (lbs/yr)", y="") +
  # Axis
  scale_x_continuous(trans="log10", breaks=10^c(0:8),
                     labels=parse(text=paste0("10^", 0:8)),
                     limits=c(0.01, 10^9)) +
  # Theme
  theme_bw() + my_theme
g3

# Merge
g <- gridExtra::grid.arrange(g3, g2, g1, nrow=1)
g

# Export
ggsave(g, filename=file.path(indir, "species_annual_landings.png"),
       width=8.5, height=7.5, units="in", dpi=600)

