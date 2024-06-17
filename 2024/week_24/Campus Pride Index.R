# Option 1: tidytuesdayR package 
## install.packages("tidytuesdayR")
## library(tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load('2024-06-11')
## OR
tuesdata <- tidytuesdayR::tt_load(2024, week = 24)

pride_index <- tuesdata$pride_index
pride_index_tags <- tuesdata$pride_index_tags

# Option 2: Read directly from GitHub

pride_index <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-11/pride_index.csv')
pride_index_tags <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-11/pride_index_tags.csv')

###Cleaning Script

library(tidyverse)
library(here)
library(fs)
library(rvest)
library(polite)

working_dir <- here::here("data", "2024", "2024-06-11")
session <- polite::bow(
  "https://campusprideindex.org/searchresults/display/0",
  user_agent = "TidyTuesday (https://tidytues.day, jonthegeek+tidytuesday@gmail.com)",
  delay = 0
)
pride_index_rows <- session |> 
  polite::scrape() |> 
  rvest::html_elements(".campus-item")

pride_index_data <- tibble::tibble(
  campus_name = pride_index_rows |> 
    rvest::html_element(".s3 h4 a") |> 
    rvest::html_text2() |> 
    stringr::str_squish(),
  campus_location = pride_index_rows |> 
    rvest::html_element(".s3 p") |> 
    rvest::html_text2() |> 
    stringr::str_squish(),
  rating = pride_index_rows |>
    rvest::html_element(".rating") |>
    purrr::map_dbl(
      \(rating_node) {
        full <- rating_node |> 
          html_elements(".rating-icon-Star-Full") |> 
          length()
        half <- rating_node |> 
          html_elements(".rating-icon-Star-Half") |> 
          length()
        full + half / 2
      }
    ),
  students = pride_index_rows |> 
    rvest::html_element(".s1 .currency") |> 
    rvest::html_text2() |> 
    readr::parse_number() |> 
    as.integer(),
  community_type = pride_index_rows |> 
    rvest::html_element(".s1 + .s2") |> 
    rvest::html_text2() |> 
    stringr::str_remove("\\([^)]+\\)") |> 
    stringr::str_squish() |> 
    tolower(),
  other_data = pride_index_rows |> 
    rvest::html_element(".s3 + .s3") |> 
    rvest::html_text2() |> 
    stringr::str_remove_all("\r\n") |> 
    stringr::str_split("\n")
)

pride_index_tags <- pride_index_data |> 
  dplyr::select(campus_name, campus_location, other_data) |> 
  tidyr::unnest_longer(other_data) |> 
  dplyr::mutate(
    other_data = dplyr::case_match(
      other_data,
      "Doctoral/Research University" ~ "doctoral",
      "Public/State University" ~ "public",
      "Residential Campus" ~ "residential",
      "Asian American and Pacific Islander Serving Institution" ~ "aapi_serving",
      "Master's College/University" ~ "masters",
      "Baccalaureate College/University" ~ "baccalaureate",
      "Nonresidential Campus" ~ "nonresidential",
      "Community College" ~ "community",
      "Hispanic Serving Institution" ~ "hispanic_serving",
      "Liberal Arts College" ~ "liberal_arts",
      "Private Institution" ~ "private",
      "Religious Affiliation" ~ "religious",
      "Historically Black College/University" ~ "hbcu",
      "Military Institution" ~ "military",
      "Technical Institute" ~ "technical",
      "Other Minority Serving Institution" ~ "other_minority_serving"
    ),
    value = TRUE
  ) |> 
  tidyr::pivot_wider(
    names_from = other_data,
    values_from = value
  ) |> 
  dplyr::select(
    campus_name,
    campus_location,
    public, private, 
    doctoral, masters, baccalaureate, community,
    residential, nonresidential,
    liberal_arts, technical,
    religious, military, 
    hbcu, hispanic_serving, aapi_serving, other_minority_serving
  )

pride_index <- pride_index_data |> 
  dplyr::select(-other_data)

# Save -------------------------------------------------------------------------
readr::write_csv(
  pride_index,
  fs::path(working_dir, "pride_index.csv")
)
readr::write_csv(
  pride_index_tags,
  fs::path(working_dir, "pride_index_tags.csv")
)

############################################################################################################################################

## join dataframes

pride_index_new <-
    pride_index %>%
    dplyr::left_join(pride_index_tags, by = dplyr::join_by(campus_name,campus_location))


library(tidyr)

##count na values in df
colSums(is.na(pride_index_new))

###prepare the main df###

##replace na values to zero(0) and logical values to numeric for gather
pride_index_main <-
  pride_index_new %>%
  mutate_if(is.logical, ~replace_na(., 0)) %>%  
  mutate_if(is.logical,as.numeric)

pride_index_main

##use gather function for campus tags

pride_index_main2 <-
  gather(pride_index_main, key="category", value="is_true", 6:21)

pride_index_main2

##select custom columns and group by columns
pride_index_main3 <-
  pride_index_main2 %>%
  select ("rating","category","is_true") %>%
  group_by (rating,category) 

pride_index_main3 <-
  aggregate(pride_index_main3$is_true, 
            by= list(pride_index_main3$rating,pride_index_main3$category), 
            FUN=sum)

pride_index_main3


##fix the tags

pride_index_main3$Group.2 = pride_index_main3$Group.2 %>% 
  str_replace_all("_", " ") %>% 
  str_to_title()

pride_index_main3


##Graph : Heatmap

heatmap <- 
  ggplot(
    data = pride_index_main3, 
    mapping = aes(x = Group.1, y = reorder(Group.2, x), fill = x)
    ) +
  
  geom_tile(
    aes(fill = x)
    ) +
  
  geom_text(
    aes(label = round(x, 1)), size = 9, color = "#808080"
    ) + #add values
  
  xlab(label = "Campus Rating") + #add x-axis label
  
  ylab(label = "Campus Tags") + ##add y-axis label
  
  facet_grid(
    ~ Group.1, 
    switch = "x", #change location
    scales = "free_x", #remove the empty scales
    space = "free_x"
    )  +
  
  theme(
    axis.text.x = element_blank(), #remove axis title
    axis.ticks.x = element_blank(), #remove axis ticks
    axis.title.x = element_text(vjust = -3), #move axis title
    
    axis.text.y = element_text(color = "#808080"),
    axis.ticks.y = element_blank(), #remove axis ticks
    axis.title.y = element_text(vjust = 0),#move axis title
    
    text = element_text(color = "#808080", family = "sans", size = 30),
    
    plot.title = 
      element_text(
        size = 60,
        face = "bold",
        color = "#732982",
        margin = margin(b = 5)
        ),
    
    plot.subtitle = 
      element_text(
        size = 36,
        color = "#732982",
        margin = margin(b = 25)
        ),
    
    legend.text = 
      element_text(
        family = "sans", 
        size = 20, 
        color = "#808080"
        ),
    
    plot.background = element_rect(colour = "#FFFFFF")
    ) +
  
  scale_fill_gradient(
    name = "Count",
    low = "#FFFFFF",
    high = "#732982") +
  
  labs(
    title = "Campus Pride Index",
    subtitle = "Distribution of campus rating and various campus tags",
    caption = "#TidyTuesday: 2024 Week 24
               github.com/zulalcelik"
    )

heatmap

##Save

png('heatmap_cpi.png',width=2400,height=2000,units="px",bg = "transparent")
print(heatmap)
dev.off()

