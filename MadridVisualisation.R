library(ggplot2)
library(scales)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(cowplot)

# Load dataset made in Python
pollutants <- read.csv("C:/Users/jarne/Documents/School/Visualization in Data Science/Project/complete_means_pollutants.csv")

head(pollutants)

pollutants <- pollutants%>% mutate(date = lubridate::make_date(Year, Month, 1))

# Pivot the dataframe
pollutants_pivot <- pollutants %>%
  select(date, PM25, NO_2, O_3, PM10, SO_2) %>% pivot_longer(-date, names_to = "Pollutant",values_to = "Concentration")

# Create the CAQI bands (based on the CAQI tresholds)
caqi_zones <- tribble(~Pollutant, ~ymin, ~ymax,   ~zone,
  # O3
  "O_3",         0,     50,   "Very Good",
  "O_3",        50,    100,   "Good",
  "O_3",       100,    130,   "Medium",
  "O_3",       130,    240,   "Poor",
  "O_3",       240,    380,   "Very Poor",
  "O_3",       380,    800,   "Extremely Poor",
  # NO2
  "NO_2",        0,     40,   "Very Good",
  "NO_2",       40,     90,   "Good",
  "NO_2",       90,    120,   "Medium",
  "NO_2",      120,    230,   "Poor",
  "NO_2",      230,    340,   "Very Poor",
  "NO_2",      340,   1000,   "Extremely Poor",
  # SO2
  "SO_2",        0,    100,   "Very Good",
  "SO_2",      100,    200,   "Good",
  "SO_2",      200,    350,   "Medium",
  "SO_2",      350,    500,   "Poor",
  "SO_2",      500,    750,   "Very Poor",
  "SO_2",      750,   1250,   "Extremely Poor",
  # PM10
  "PM10",        0,     20,   "Very Good",
  "PM10",       20,     40,   "Good",
  "PM10",       40,     50,   "Medium",
  "PM10",       50,    100,   "Poor",
  "PM10",      100,    150,   "Very Poor",
  "PM10",      150,   1200,   "Extremely Poor",
  # PM25
  "PM25",        0,     10,   "Very Good",
  "PM25",       10,     20,   "Good",
  "PM25",       20,     25,   "Medium",
  "PM25",       25,     50,   "Poor",
  "PM25",       50,     75,   "Very Poor",
  "PM25",       75,    800,   "Extremely Poor")

caqi_zones <- caqi_zones %>%
  mutate(zone = factor(zone,
                       levels = c("Very Good","Good","Medium","Poor","Very Poor","Extremely Poor")))

ggplot() +
  geom_rect(
    data = caqi_zones,
    aes(
      xmin = as.Date("2001-01-01"),
      xmax = as.Date("2018-12-31"),
      ymin = ymin, ymax = ymax,
      fill = zone),
    inherit.aes = FALSE, alpha = 1) +
  geom_line(
    data = pollutants_pivot,
    aes(x = date, y = Concentration),
    color = "black", size = 0.5) +
  facet_wrap(~Pollutant, scales = "fixed") +
  coord_cartesian(ylim = c(0, 100)) +
  # Assign colors to each CAQI index band
  scale_fill_manual(values = c(
    "Very Good"       = "#00B050",
    "Good"            = "#92D050",
    "Medium"          = "#FFFF00",
    "Poor"            = "#FFC000",
    "Very Poor"       = "#FF0000",
    "Extremely Poor"  = "#C00000")) +
  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",expand = expansion(add = c(0,0))) +
  labs(title = "Monthly Pollutant Concentrations with CAQI Bands",
    x = "Date", y = "Concentration (µg/m³)",
    fill = "CAQI Zone") +
  # Keep gridlines visually away
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text = element_text(face = "bold"))

##

# Individual air pollutant plots

pollutant_plots <- unique(pollutants_pivot$Pollutant)

plots <- map(pollutant_plots, function(p) {
  df_p   <- filter(pollutants_pivot,   Pollutant == p)
  zones_p<- filter(caqi_zones, Pollutant == p)
  
  ggplot() +
    geom_rect(
      data = zones_p,
      aes(
        xmin = as.Date("2001-01-01"),
        xmax = as.Date("2018-12-31"),
        ymin = ymin, ymax = ymax,
        fill = zone
      ),
      inherit.aes = FALSE, alpha = 1
    ) +
    geom_line(
      data = df_p,
      aes(x = date, y = Concentration),
      color = "black", size = 0.8
    ) +
    # Force the y-axis from 0 to 100
    coord_cartesian(ylim = c(0, 100)) +
    # assign the CAQI colors
    scale_fill_manual(values = c(
      "Very Good"       = "#00B050",
      "Good"            = "#92D050",
      "Medium"          = "#FFFF00",
      "Poor"            = "#FFC000",
      "Very Poor"       = "#FF0000",
      "Extremely Poor"  = "#C00000"
    )) +
    scale_x_date(
      date_breaks = "1 year", date_labels = "%Y",
      expand = expansion(add = c(0,0))
    ) +
    labs(
      title = glue::glue("{p} Concentrations (2001–2018)"),
      x     = "Year", 
      y     = "Concentration (µg/m³)",
      fill  = "CAQI Zone"
    ) +
    theme_minimal() +
    theme(
      panel.grid    = element_blank(),
      axis.ticks    = element_line(color = "black"),
      axis.text     = element_text(color = "black"),
      strip.background = element_rect(fill = "grey90", color = NA),
      strip.text    = element_text(face = "bold"))})

names(plots) <- pollutant_plots

# print each pollutant's plot (add some optional layers)

plots[["NO_2"]] <- plots[["NO_2"]] +
  labs(title = "Mean Monthly NO2 Concentrations from 2001 to 2018 in Madrid") +
  scale_fill_manual(
    values = c(
      "Very Good"       = "#00B050",
      "Good"            = "#92D050",
      "Medium"          = "#FFFF00",
      "Poor"            = "#FFC000",
      "Very Poor"       = "#FF0000",
      "Extremely Poor"  = "#C00000"
    ),
    breaks = c("Very Good", "Good", "Medium"))

print(plots[["NO_2"]])

plots[["O_3"]] <- plots[["O_3"]] +
  labs(title = "Mean Monthly O3 Concentrations from 2001 to 2018 in Madrid") +
  scale_fill_manual(
    values = c(
      "Very Good"       = "#00B050",
      "Good"            = "#92D050",
      "Medium"          = "#FFFF00",
      "Poor"            = "#FFC000",
      "Very Poor"       = "#FF0000",
      "Extremely Poor"  = "#C00000"
    ),
    breaks = c("Very Good", "Good", "Medium")
  )

print(plots[["O_3"]])

plots[["SO_2"]] <- plots[["SO_2"]] +
  labs(title = "Mean Monthly SO2 Concentrations from 2001 to 2018 in Madrid") +
  coord_cartesian(ylim = c(0, 40)) +
  scale_fill_manual(
    values = c(
      "Very Good"       = "#00B050",
      "Good"            = "#92D050",
      "Medium"          = "#FFFF00",
      "Poor"            = "#FFC000",
      "Very Poor"       = "#FF0000",
      "Extremely Poor"  = "#C00000"
    ),
    breaks = c("Very Good"))

print(plots[["SO_2"]])

plots[["PM10"]] <- plots[["PM10"]] +
  labs(title = "Mean Monthly PM10 Concentrations from 2001 to 2018 in Madrid") +
  coord_cartesian(ylim = c(0, 70)) +
  scale_fill_manual(
    values = c(
      "Very Good"       = "#00B050",
      "Good"            = "#92D050",
      "Medium"          = "#FFFF00",
      "Poor"            = "#FFC000",
      "Very Poor"       = "#FF0000",
      "Extremely Poor"  = "#C00000"
    ),
    breaks = c("Very Good", "Good", "Medium", "Poor"))

print(plots[["PM10"]])

plots[["PM25"]] <- plots[["PM25"]] +
  labs(title = "Mean Monthly PM2.5 Concentrations from 2004 to 2018 in Madrid") +
  coord_cartesian(ylim = c(0, 40)) +
  scale_fill_manual(
    values = c(
      "Very Good"       = "#00B050",
      "Good"            = "#92D050",
      "Medium"          = "#FFFF00",
      "Poor"            = "#FFC000",
      "Very Poor"       = "#FF0000",
      "Extremely Poor"  = "#C00000"
    ),
    breaks = c("Very Good", "Good", "Medium", "Poor")
  )

 print(plots[["PM25"]])

## Plot with all pollutants:

pollutants <- pollutants %>%
  mutate(date = make_date(Year, Month, 1))

pollutants_pivot   <- pollutants %>%
  pivot_longer(
    cols      = c(PM25, NO_2, O_3, PM10, SO_2),
    names_to  = "Pollutant",
    values_to = "Concentration"
  )

pollutants_pivot <- pollutants_pivot %>%
  mutate(
    Pollutant = recode(Pollutant,
                       "SO_2" = "SO2",
                       "NO_2" = "NO2",
                       "O_3"  = "O3",
                       "PM25" = "PM2.5",
                       .default = Pollutant
    )
  )
# Add specific pollutant symbols
shape_values <- c(
  "PM2.5" = 16,
  "NO2"   = 17, 
  "O3"    = 15,  
  "PM10"  = 3,  
  "SO2"   = 7   
)
# Add custom color values
color_values <- c(
  "PM2.5" = "#1f77b4",
  "NO2"   = "#ff7f0e",
  "O3"    = "#9467bd",
  "PM10"  = "#d62728",
  "SO2"   = "#2ca02c" 
)

ggplot() +
  geom_line(
    data = pollutants_pivot,
    aes(x = date, y = Concentration, color = Pollutant),
    size = 1
  ) +
  geom_point(
    data = pollutants_pivot,
    aes(x = date, y = Concentration,
        color = Pollutant,
        shape = Pollutant),
    size = 2
  ) +
  scale_shape_manual(values = shape_values) +
  scale_color_manual(values = color_values) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(add = c(0, 0))
  ) +
  labs(
    x     = "Time",
    y     = "Monthly Concentration (µg/m³)",
    title = "Mean Monthly Air Pollutant Levels in Madrid (2001–2018)",
    color = "Pollutant",
    shape = "Pollutant"
  ) +
  # Minimal theme for minimalistic details
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_blank(),
    panel.border     = element_blank(),
    panel.grid       = element_blank(),
    plot.background  = element_blank(),
    axis.line        = element_blank(),
    axis.ticks       = element_blank(),
    axis.text        = element_text(color = "black"),
    legend.background = element_blank(),
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold")
  )

# plot 2
pv2 <- pollutants_pivot %>%
  left_join(caqi_zones, by = "Pollutant") %>%
  filter(Concentration >= ymin, Concentration < ymax) %>%
  select(date, Pollutant, Concentration, zone)

ggplot() +
  geom_line(
    data = pollutants_pivot,
    aes(x = date, y = Concentration, group = Pollutant),
    color = "grey70", size = 0.5
  ) +
  # colored symbols at each point, shape by pollutant
  geom_point(
    data = pv2,
    aes(x = date, y = Concentration,
        color = zone,
        shape = Pollutant),
    size = 2
  ) +
  scale_color_manual(values = c(
    "Very Good"      = "#00B050",
    "Good"           = "#92D050",
    "Medium"         = "#FFFF00",
    "Poor"           = "#FFC000",
    "Very Poor"      = "#FF0000",
    "Extremely Poor" = "#C00000"
  )) +
  scale_shape_manual(values = c(
    "O_3"  = 16,
    "NO_2" = 17,
    "SO_2" = 15,
    "PM10" = 18,
    "PM25" = 8
  )) +
  coord_cartesian(ylim = c(0, NA)) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    expand = expansion(add = c(0, 0))
  ) +
  labs(
    title = "Mean Monthly Air Pollutant Concentrations with CAQI Zones in Madrid (2001–2018)",
    x     = "Year",
    y     = "Concentration (µg/m³)",
    color = "CAQI Zone",
    shape = "Pollutant"
  ) +
  theme_minimal() +
  theme(
    panel.grid    = element_blank(),
    axis.ticks    = element_line(color = "black"),
    axis.text     = element_text(color = "black")
  )

## Plots for daily means

daily_pollutants <- read.csv(
  "C:/Users/jarne/Documents/School/Visualization in Data Science/Project/daily_means_pollutants.csv",
  stringsAsFactors = FALSE
)

# Create a proper daily date column (Year, Month, Day)
daily_pollutants <- daily_pollutants %>%
  mutate(
    date = make_date(Year, Month, Day)
  )

# Pivot the dataframe into long format
pollutants_pivot <- daily_pollutants %>%
  select(date, PM25, NO_2, O_3, PM10, SO_2) %>%
  pivot_longer(
    -date,
    names_to  = "Pollutant",
    values_to = "Concentration"
  )

# Define CAQI bands for each pollutant
caqi_zones <- tribble(
  ~Pollutant, ~ymin,   ~ymax,    ~zone,
  # O3
  "O_3",        0,      50,  "Very Good",
  "O_3",       50,     100,  "Good",
  "O_3",      100,     130,  "Medium",
  "O_3",      130,     240,  "Poor",
  "O_3",      240,     380,  "Very Poor",
  "O_3",      380,     800,  "Extremely Poor",
  # NO2
  "NO_2",       0,      40,  "Very Good",
  "NO_2",      40,      90,  "Good",
  "NO_2",      90,     120,  "Medium",
  "NO_2",     120,     230,  "Poor",
  "NO_2",     230,     340,  "Very Poor",
  "NO_2",     340,    1000,  "Extremely Poor",
  # SO2
  "SO_2",       0,     100,  "Very Good",
  "SO_2",     100,     200,  "Good",
  "SO_2",     200,     350,  "Medium",
  "SO_2",     350,     500,  "Poor",
  "SO_2",     500,     750,  "Very Poor",
  "SO_2",     750,    1250,  "Extremely Poor",
  # PM10
  "PM10",       0,      20,  "Very Good",
  "PM10",      20,      40,  "Good",
  "PM10",      40,      50,  "Medium",
  "PM10",      50,     100,  "Poor",
  "PM10",     100,     150,  "Very Poor",
  "PM10",     150,    1200,  "Extremely Poor",
  # PM25
  "PM25",       0,      10,  "Very Good",
  "PM25",      10,      20,  "Good",
  "PM25",      20,      25,  "Medium",
  "PM25",      25,      50,  "Poor",
  "PM25",      50,      75,  "Very Poor",
  "PM25",      75,     800,  "Extremely Poor"
)

caqi_zones <- caqi_zones %>%
  mutate(zone = factor(
    zone,
    levels = c("Very Good","Good","Medium","Poor","Very Poor","Extremely Poor")
  ))

pollutant_list <- unique(pollutants_pivot$Pollutant)

plots <- map(pollutant_list, function(pol) {
  dat     <- filter(pollutants_pivot, Pollutant==pol)
  bands   <- filter(caqi_zones,    Pollutant==pol)
  
  ggplot() +
    # CAQI–background
    geom_rect(
      data = bands,
      aes(
        xmin = as.Date("2001-01-01"),
        xmax = as.Date("2018-12-31"),
        ymin = ymin, ymax = ymax,
        fill = zone
      ),
      inherit.aes = FALSE, alpha = 0.4
    ) +
    # Add line
    geom_line(
      data = dat,
      aes(x = date, y = Concentration),
      color = "black", size = 0.5
    ) +
    coord_cartesian(ylim = c(0, 220)) +
    scale_fill_manual(values = c(
      "Very Good"       = "#00B050",
      "Good"            = "#92D050",
      "Medium"          = "#FFFF00",
      "Poor"            = "#FFC000",
      "Very Poor"       = "#FF0000",
      "Extremely Poor"  = "#C00000"
    )) +
    scale_x_date(
      date_breaks = "2 years",
      date_labels = "%Y",
      expand = expansion(add = c(0,0))
    ) +
    labs(
      title = paste0(pol, " daily pollution, 2001–2018)"),
      x = "Year", y = "Concentration (µg/m³)",
      fill = "CAQI Zone"
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text  = element_text(color="black"),
      strip.text = element_text(face="bold")
    )
})

names(plots) <- pollutant_list

# Print each pollutant's plot (add some optional layers)

plots[["NO_2"]] <- plots[["NO_2"]] +
  labs(title = "Mean Daily NO2 Concentrations from 2001 to 2018 in Madrid") +
  scale_fill_manual(
    values = c(
      "Very Good"       = "#00B050",
      "Good"            = "#92D050",
      "Medium"          = "#FFFF00",
      "Poor"            = "#FFC000",
      "Very Poor"       = "#FF0000",
      "Extremely Poor"  = "#C00000"
    ),
    breaks = c("Very Good", "Good", "Medium", "Poor")
  )

print(plots[["NO_2"]])

plots[["O_3"]] <- plots[["O_3"]] +
  labs(title = "Mean Daily O3 Concentrations from 2001 to 2018 in Madrid") +
  coord_cartesian(ylim = c(0, 120)) +
  scale_fill_manual(
    values = c(
      "Very Good"       = "#00B050",
      "Good"            = "#92D050",
      "Medium"          = "#FFFF00",
      "Poor"            = "#FFC000",
      "Very Poor"       = "#FF0000",
      "Extremely Poor"  = "#C00000"
    ),
    breaks = c("Very Good", "Good", "Medium")
  )

print(plots[["O_3"]])

plots[["SO_2"]] <- plots[["SO_2"]] +
  labs(title = "Mean Daily SO2 Concentrations from 2001 to 2018 in Madrid") +
  coord_cartesian(ylim = c(0, 70)) +
  scale_fill_manual(
    values = c(
      "Very Good"       = "#00B050",
      "Good"            = "#92D050",
      "Medium"          = "#FFFF00",
      "Poor"            = "#FFC000",
      "Very Poor"       = "#FF0000",
      "Extremely Poor"  = "#C00000"
    ),
    breaks = c("Very Good", "Good")
  )

print(plots[["SO_2"]])

plots[["PM10"]] <- plots[["PM10"]] +
  labs(title = "Mean Daily PM10 Concentrations from 2001 to 2018 in Madrid") +
  coord_cartesian(ylim = c(0, 220)) +
  scale_fill_manual(
    values = c(
      "Very Good"       = "#00B050",
      "Good"            = "#92D050",
      "Medium"          = "#FFFF00",
      "Poor"            = "#FFC000",
      "Very Poor"       = "#FF0000",
      "Extremely Poor"  = "#C00000"
    ),
    breaks = c("Very Good", "Good", "Medium", "Poor", "Very Poor", "Extremely Poor")
  )

print(plots[["PM10"]])

plots[["PM25"]] <- plots[["PM25"]] +
  labs(title = "Mean Daily PM2.5 Concentrations from 2004 to 2018 in Madrid") +
  coord_cartesian(ylim = c(0, 70)) +
  scale_fill_manual(
    values = c(
      "Very Good"       = "#00B050",
      "Good"            = "#92D050",
      "Medium"          = "#FFFF00",
      "Poor"            = "#FFC000",
      "Very Poor"       = "#FF0000",
      "Extremely Poor"  = "#C00000"
    ),
    breaks = c("Very Good", "Good", "Medium", "Poor", "Very Poor")
  )

print(plots[["PM25"]])

## Figure with 5 plots

plots[["NO_2"]] <- plots[["NO_2"]] +
  labs(title = "", y = "NO2 Concentration (µg/m³)") +
  scale_fill_manual(
    values = c(
      "Very Good"       = "#00B050",
      "Good"            = "#92D050",
      "Medium"          = "#FFFF00",
      "Poor"            = "#FFC000",
      "Very Poor"       = "#FF0000",
      "Extremely Poor"  = "#C00000"
    ),
    breaks = c("Very Good", "Good", "Medium", "Poor")
  )

plots[["O_3"]] <- plots[["O_3"]] +
  labs(title = "", y = "O3 Concentration (µg/m³)") +
  coord_cartesian(ylim = c(0, 120)) +
  scale_fill_manual(
    values = c(
      "Very Good"       = "#00B050",
      "Good"            = "#92D050",
      "Medium"          = "#FFFF00",
      "Poor"            = "#FFC000",
      "Very Poor"       = "#FF0000",
      "Extremely Poor"  = "#C00000"
    ),
    breaks = c("Very Good", "Good", "Medium"))

plots[["SO_2"]] <- plots[["SO_2"]] +
  labs(title = "", y = "SO2 Concentration (µg/m³)") +
  coord_cartesian(ylim = c(0, 70)) +
  scale_fill_manual(
    values = c(
      "Very Good"       = "#00B050",
      "Good"            = "#92D050",
      "Medium"          = "#FFFF00",
      "Poor"            = "#FFC000",
      "Very Poor"       = "#FF0000",
      "Extremely Poor"  = "#C00000"
    ),
    breaks = c("Very Good", "Good"))

plots[["PM10"]] <- plots[["PM10"]] +
  labs(title = "", y = "PM10 Concentration (µg/m³)") +
  coord_cartesian(ylim = c(0, 220)) +
  scale_fill_manual(
    values = c(
      "Very Good"       = "#00B050",
      "Good"            = "#92D050",
      "Medium"          = "#FFFF00",
      "Poor"            = "#FFC000",
      "Very Poor"       = "#FF0000",
      "Extremely Poor"  = "#C00000"
    ),
    breaks = c("Very Good", "Good", "Medium", "Poor", "Very Poor", "Extremely Poor"))

print(plots[["PM10"]])

plots[["PM25"]] <- plots[["PM25"]] +
  labs(title = "", y ="PM25 Concentration (µg/m³)") +
  coord_cartesian(ylim = c(0, 70)) +
  scale_fill_manual(
    values = c(
      "Very Good"       = "#00B050",
      "Good"            = "#92D050",
      "Medium"          = "#FFFF00",
      "Poor"            = "#FFC000",
      "Very Poor"       = "#FF0000",
      "Extremely Poor"  = "#C00000"
    ),
    breaks = c("Very Good", "Good", "Medium", "Poor", "Very Poor"))

print(plots[["PM25"]])

final_figure <- plot_grid(
  plotlist = plots,
  nrow     = 3,
  ncol     = 2,
  labels   = "")

# add title
ggdraw() +
  draw_plot(final_figure, height = 0.90) +
  draw_label("Mean Daily Pollutant Concentrations in Madrid (2001-2018)",
             fontface = "bold", x = 0.5, y = 0.98, hjust = 0.5, vjust = 1)
