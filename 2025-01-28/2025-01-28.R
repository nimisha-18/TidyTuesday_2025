library(readr)
library(ggplot2)
library(ggimage)
library(tidyverse)
library(showtext)
library(magick)
library(grid)
library(png)

# Add Custom Fonts
font_add("IBMPlexSerif-Bold", "IBMPlexSerif-Bold.ttf") # Title font
font_add("Baskervville-Regular", "Baskervville-Regular.ttf") # Subtitle font
font_add_google("Lato", "Static") # Axis font
showtext_auto()

# Read the csv directly
data_2023 <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-28/water_insecurity_2023.csv'
)

head(data_2023, 5)

# Exploratory data analysis 
summary(data_2023)
colSums(is.na(data_2023))

# Remove NA in the data
data_2023 <- data_2023 %>%
  dplyr::filter(!is.na(data_2023$plumbing))


# Split the data into region and subregion
split_data_2023 <- data_2023 %>%
  separate(name, 
           into = c("subregion", "region"), 
           sep = ", ", 
           remove = FALSE)

# Mutate the requirements (optional)
split_data_2023 <- split_data_2023 %>%
  dplyr::mutate(region = toupper(region), 
                subregion = toupper(subregion))

# Filtered the data as per requirement (optional)
top_counties <- split_data_2023 %>%
  filter(plumbing > 5)%>%
  arrange(desc(plumbing))

# Top 20 counties with the plumbing issues
filter_counties <- top_counties %>%
  slice_max(percent_lacking_plumbing, n = 20) %>%
  group_by(region)

# The drop image (from: https://pixabay.com/vectors/water-water-droplet-droplet-2072211/)
water_drop <- "drop.png"

# Plot-------------
ggplot(
  filter_counties, 
  aes(
    x = percent_lacking_plumbing, 
    y = reorder(subregion, percent_lacking_plumbing),
    color = region
  )
)+
  geom_segment(
    aes(
      xend = 0, 
      yend = subregion
    ), 
    linewidth = 1.0
  ) +  # Lollipop stick
  geom_image(
    aes(
      x = percent_lacking_plumbing,  # Ensure x is mapped
      y = reorder(subregion, percent_lacking_plumbing),
      image = water_drop
    ), 
    size = 0.03,
    inherit.aes = FALSE
  ) +  # Water drop at the end
  scale_color_hue()+
  theme_minimal() +
  labs(
    title = "Pipes, Privilege & Plumbing: \nWho’s Left Without Access?",
    subtitle = "This lollipop chart visualizes the percentage of households lacking plumbing across the U.S., revealing the top 20 counties where access to essential water infrastructure remains a significant challenge.\nDespite modern advancements, certain regions still struggle with basic plumbing, underscoring ongoing disparities in water access.",
    x = "% Lacking Plumbing",
    y = "Counties"
  ) +
  theme(
    axis.text = element_text(
      family = "Static",
      size = 10, 
      color = "black"
    ), 
    axis.title = element_text(size = 12),
    plot.title = element_text(
      family = "IBMPlexSerif-Bold", 
      size = 30, 
      color = "darkblue", 
      face = "bold"
    ),
    plot.subtitle = element_text(
      family = "Baskervville-Regular",
      size = 14,
      color = "black"
    ),
    plot.background = element_rect(
      fill = "azure"
    )
  )
