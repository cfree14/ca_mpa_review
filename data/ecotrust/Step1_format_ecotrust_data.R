
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
indir <- "data/ecotrust/raw"
outdir <- "data/ecotrust/processed"
plotdir <- "data/ecotrust/figures"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "MPAHumanUses_FocusGroup_QuantitativeData_raw_090921.xlsx"),
                                sheet="Responses_WB+MPA_Commercial", na=c("-", "998", "999"))

# Read key
q_key <- readxl::read_excel(file.path(indir, "question_key.xlsx"))

# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Slice
  slice(1:85) %>%
  # Rename
  rename(port_complex=port_portgroup) %>%
  # Gather
  gather(key="question", value="response", 2:ncol(.)) %>%
  # Count
  count(port_complex, question, response) %>%
  # Format port complex
  mutate(port_complex=recode_factor(port_complex,
                                    "1"="Crescent City",
                                    "2"="Trinidad",
                                    "3"="Eureka",
                                    "4"="Shelter Cove",
                                    "5"="Fort Bragg/Albion",
                                    "6"="Point Arena",
                                    "7"="Bodega Bay",
                                    "8"="San Francisco",
                                    "9"="Princeton/Half Moon Bay",
                                    "10"="Santa Cruz",
                                    "11"="Moss Landing",
                                    "12"="Monterey",
                                    "13"="Morro Bay/Port San Luis",
                                    "14"="Santa Barbara",
                                    "15"="Ventura/Channel Islands",
                                    "16"="Los Angeles/Long Beach",
                                    "17"="Orange County",
                                    "18"="Oceanside",
                                    "19"="San Diego")) %>%
  # Format question
  mutate(question=recode(question,
                         "access_econ"="Access to harvestable resources",
                         "covid19"="Covid-19 impacts",
                         "ecological_mpa"="Ecological",
                         "enforcement_mpa"="Enforcement",
                         "income_econ"="Income from fishing",
                         "infrastructure_econ"="Infrastructure",
                         "jobsatisfaction_soc"="Job satisfaction",
                         "labor_soc"="Labor/new participants",
                         "livelihood_mpa"="Livelihoods",
                         "management_mpa"="Management",
                         "marineresourcefuture_env"="Marine resource health-future",
                         "marineresourcepresent_env"="Marine resource health-present",
                         "markets_econ"="Markets",
                         "monitoring_mpa"="Monitoring",
                         "relationshipsexternal_soc"="Social relationships=external",
                         "relationshipsinternal_soc"="Social relationships-internal")) %>%
  # Arrange
  arrange(port_complex, question) %>%
  # Add prop
  group_by(port_complex, question) %>%
  mutate(prop=n/sum(n)) %>%
  ungroup() %>%
  # Add question
  left_join(q_key, by="question") %>%
  # Arrange
  select(port_complex, category, question, everything())

# Inspect
sort(unique(data$question))


# Plot data
################################################################################

# Plot data
g <- ggplot(data, aes(x=prop, y=port_complex, fill=response %>% as.character())) +
  facet_wrap(~question) +
  geom_bar(stat="identity", col="grey30", lwd=0.1) +
  # Labels
  labs(x="Proportion of respondents", y="") +
  # Legend
  scale_fill_manual(name="Score", values=RColorBrewer::brewer.pal(5, "RdBu")) +
  # Theme
  theme_bw()
g



