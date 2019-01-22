library(tidyverse)
library(gganimate)
library(statebins)

prison_pop <- read_csv("data/prison_population.csv") %>%
        na.omit() %>%
        filter(pop_category == "Total") %>%
        group_by(year, state) %>%
        summarise(population = sum(population, na.rm = TRUE),
                  prison_population = sum(prison_population, na.rm = TRUE),
                  pc_in_prison = prison_population / population * 100)

plot <-
    plot_df %>%
    ggplot(aes(state = state, fill = pc_in_prison)) +
    geom_statebins(na.rm = TRUE) +
    coord_equal() +
    theme_statebins(legend_position = "top") +
    scale_fill_viridis_c(direction = -1, option = "magma") +
    transition_states(states = year, transition_length = 10, state_length = 10, wrap = FALSE) +
    labs(fill = str_c("Percentage of Population in Prisons from ", min(plot_df$year), " to ", max(plot_df$year)))


animate(plot, start_pause = 5, end_pause = 5)
