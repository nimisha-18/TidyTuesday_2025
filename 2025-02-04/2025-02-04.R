# Load all the libraries
library(tidyverse)
library(readr)
library(dplyr)
library(grid)
library(ggplot2)
library(MexBrewer)
library(showtext)
library(ggpubr)
library(cowplot)

font_add("BreNoelle", "BreNoelle.ttf") # Font for the title
font_add("TimesNewRomanPS-BoldMT","C:/WINDOWS/Fonts/timesbd.ttf") # Font for the subtitle
showtext_auto() # Automate

# Read the file ----------
script_lines <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_script_lines.csv'
)

# Exploratory Data Analysis ---------- 
summary(script_lines)
colSums(is.na(script_lines))

# Removing NAs in the data ----------
filtered_script <- script_lines %>%
  drop_na() %>%
  group_by(raw_character_text)%>%
  arrange(desc(word_count))

# Most lines spoken ---------
most_spoken <- filtered_script %>%
  group_by(raw_character_text)%>%
  summarise(normalized_text = n())%>%
  arrange(desc(normalized_text))

most_spoken <- most_spoken%>%
  head(10)

# Background color for the plot ---------
bar_color <- linearGradient(
  c("#F0F8FF", "#70D1FE"),
  x1 = 0.5, y1 = 0, x2 = 0.5, y2 = 1
)

ggplot() +
  geom_blank() +
  theme(
    plot.background = element_rect(
      fill = bar_color),
    panel.background = element_blank())

# Plot ------------
sp <- most_spoken%>%
  ggplot(
    aes(
      x = raw_character_text,
      y = normalized_text,
      fill = raw_character_text 
    )
  ) +
  geom_bar(
    stat = "identity"
  )+
  scale_fill_manual(
    values = mex.brewer("Ronda")
  )+
  theme_minimal()+
  theme(
    legend.position = "none"
  )+
    labs(
      title = "Well, It's called 'The Simpsons' for a reason!", 
      subtitle = "Among all the characters in The Simpsons, Homer speaks the most! \nThis bar plot highlights the top 10 most vocal residents of Springfield, \nrevealing the dialogue dominance of our favorite animated family.",
      x = "Talkative members!",
      y = "Dialogue Volume"
    )+
  theme(
    plot.title = element_text(
      family = "BreNoelle", 
      size = 40, 
      color = "black"
    ),
    plot.background = element_rect(
      fill = bar_color
    ),
    axis.text.x = element_text(
      face = "bold",
      size = 8,
      angle = 45,
      colour = "black"
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 10,
      colour = "black"
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 10,
      color = "black"
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 10,
      color = "black",
      margin = margin(r = 5)
    ),
    plot.subtitle = element_text(
      family = "TimesNewRomanPS-BoldMT",
      face = "bold", 
      size = 12,
      color = "gray25"
    )
  )

# Save the file 
save_plot(
  "most_spoking_member.png",
  plot = sp, 
  base_height = 3,
  base_aspect_ratio = 1.5
)

