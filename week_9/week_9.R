library(fivethirtyeight)
library(tidyverse)
library(skimr)
library(stringr)


data(comic_characters)

skim(comic_characters)  



my_comic_characters <- 
  comic_characters %>% 
  select(publisher, align:gsm, appearances) %>% 
  mutate_at(vars(align, sex, gsm),
            ~ str_replace(., "Characters", "") %>% str_trim()) %>% 
  mutate(eye   = str_replace(eye, "Eyes", "") %>% str_trim(),
         hair  = str_replace(hair, "Hair", "") %>% str_trim(),
         align = factor(align, levels = c("Bad",
                                          "Neutral",
                                          "Reformed",
                                          "Good"))) %>% 
  mutate_if(is.character, factor) %>% 
  filter(!is.na(align))
  # gather(-align, - publisher, key = "variable", value = "form")


appearances_vs <- function(data, var) {
  # var <- enquo(var)
  
  ggplot(data,
         aes_string(var, "appearances", fill = "publisher")) +
    geom_boxplot(alpha = 0.1) +
    facet_grid(publisher ~ .)
}

names(my_comic_characters) %>% 
  setdiff(c("publisher", "appearances")) %>% 
  map(~ appearances_vs(data = my_comic_characters,
                 var = .x) +
        scale_y_log10())


library(mlr)

task <- makeClassifTask(id = "Predict alignment",
                        data = my_comic_characters,
                        target = "align")
