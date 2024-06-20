# Tidy Tuesday - 2024-05-14 - Coffee Survey
# Date of Creation: 18-05-2024

##libraries for data, font
library(tidyverse)
library(tidytuesdayR)
library(tidymodels) 
library(MetBrewer)

color_palette <- met.brewer("Signac")

data <- tidytuesdayR::tt_load('2024-05-14')
df_coffee <- data$coffee_survey

age_groups <- c(
  "<18",
  "18-24",
  "25-34",
  "35-44",
  "45-54",
  "55-64",
  ">65"
)

df_coffee_age <- df_coffee |>
  #Clean up values in age, favorite columns
  mutate(age = str_remove(age, " years old"),
         age = fct_relevel(age, age_groups),
         favorite = str_remove(favorite, "Regular "),
         favorite = str_replace(favorite, "drip", "Drip"),
         favorite = str_remove(favorite, "\\(.*?\\)")) |>
  #filter out NA age 
  filter(!is.na(age)) |>   
  #frequency of (age, count) combination
  count(age, favorite) |>   
  mutate(total_age = sum(n, na.rm = TRUE), .by=age,
         pct = round(n/total_age *100, digits = 1)) |>
  group_by(age) |>
  #create column 'rank' that keeps pct of the MOST favorite coffee 
  mutate(rank = ifelse(pct %in% sort(pct, decreasing = TRUE)[1:1], 
                       paste0(favorite," ",pct,"%"), 
                       ""))


my_plot <- df_coffee_age |>
  ggplot(aes(y = age, 
             x = pct,
             fill = favorite)) +
  geom_bar(stat='identity',
           linewidth = .3,
           width = .7) +
  geom_text(aes(label=rank),
            alpha = .7,
            position = position_stack(vjust = .5),
            color = "white",
            size = 3) +
  scale_fill_manual(values = color_palette) +
  scale_y_discrete(limits = rev) +
  labs(title = "Coffee Prefrences: How do they change across age groups?",
       subtitle = str_wrap("According to data from the Great American Coffee Taste test, coffee preferences differ amongst different age groups, with pourover being the number one coffee beverage of choice across four age groups.", 105),
       caption = "Data Source: Cometeer | @kristenjc") +
  theme_minimal(base_family="serif") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(), 
    legend.key.height = unit(0.5, "line"),
    legend.key.width = unit(1, "line"),
    plot.background = element_rect(fill = "#faf9f6"),
    plot.margin = margin(20,15,15,25),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_text(margin = margin(r=-20))
  ) +
  guides(fill = guide_legend(nrow=2))

ggsave("2024-05-14-Viz.png")





