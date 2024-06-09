# Option 1: tidytuesdayR package 
install.packages("tidytuesdayR")
library(tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load('2024-05-14')
## OR
tuesdata <- tidytuesdayR::tt_load(2024, week = 20)

coffee_survey <- tuesdata$coffee_survey


# Option 2: Read directly from GitHub

coffee_survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-14/coffee_survey.csv')


##Cleaning Script  

install.packages("tidyverse")
install.packages("janitor")
install.packages("here")
install.packages("fs")


library(tidyverse)
library(janitor)
library(here)
library(fs)

working_dir <- here::here("tidytuesday_data", "2024", "2024-05-14")

url <- "https://bit.ly/gacttCSV"

coffee_survey_raw <- readr::read_csv(url)

# Grab the raw questions for the dictionary.
coffee_survey_raw |> 
  colnames() |> 
  cat(sep = "\n")

coffee_survey_rn <- coffee_survey_raw |> 
  janitor::clean_names() |> 
  # Get rid of one-hot encoding; users can do that if they'd like. Also,
  # "flavorings" columns are empty.
  dplyr::select(
    submission_id,
    age = what_is_your_age,
    cups = how_many_cups_of_coffee_do_you_typically_drink_per_day,
    where_drink = where_do_you_typically_drink_coffee,
    brew = how_do_you_brew_coffee_at_home,
    brew_other = how_else_do_you_brew_coffee_at_home,
    purchase = on_the_go_where_do_you_typically_purchase_coffee,
    purchase_other = where_else_do_you_purchase_coffee,
    favorite = what_is_your_favorite_coffee_drink,
    favorite_specify = please_specify_what_your_favorite_coffee_drink_is,
    additions = do_you_usually_add_anything_to_your_coffee,
    additions_other = what_else_do_you_add_to_your_coffee,
    dairy = what_kind_of_dairy_do_you_add,
    sweetener = what_kind_of_sugar_or_sweetener_do_you_add,
    style = before_todays_tasting_which_of_the_following_best_described_what_kind_of_coffee_you_like,
    strength = how_strong_do_you_like_your_coffee,
    roast_level = what_roast_level_of_coffee_do_you_prefer,
    caffeine = how_much_caffeine_do_you_like_in_your_coffee,
    expertise = lastly_how_would_you_rate_your_own_coffee_expertise,
    starts_with("coffee"),
    prefer_abc = between_coffee_a_coffee_b_and_coffee_c_which_did_you_prefer,
    prefer_ad = between_coffee_a_and_coffee_d_which_did_you_prefer,
    prefer_overall = lastly_what_was_your_favorite_overall_coffee,
    wfh = do_you_work_from_home_or_in_person,
    total_spend = in_total_much_money_do_you_typically_spend_on_coffee_in_a_month,
    why_drink = why_do_you_drink_coffee,
    why_drink_other = other_reason_for_drinking_coffee,
    taste = do_you_like_the_taste_of_coffee,
    know_source = do_you_know_where_your_coffee_comes_from,
    most_paid = what_is_the_most_youve_ever_paid_for_a_cup_of_coffee,
    most_willing = what_is_the_most_youd_ever_be_willing_to_pay_for_a_cup_of_coffee,
    value_cafe = do_you_feel_like_you_re_getting_good_value_for_your_money_when_you_buy_coffee_at_a_cafe,
    spent_equipment = approximately_how_much_have_you_spent_on_coffee_equipment_in_the_past_5_years,
    value_equipment = do_you_feel_like_you_re_getting_good_value_for_your_money_with_regards_to_your_coffee_equipment,
    gender,
    gender_specify = gender_please_specify,
    education_level,
    ethnicity_race,
    ethnicity_race_specify = ethnicity_race_please_specify,
    employment_status,
    number_children = number_of_children,
    political_affiliation
  )

readr::write_csv(
  coffee_survey,
  fs::path(working_dir, "coffee_survey.csv")
)

##start##

##dataframe's name has changed.
coffee <- coffee_survey_rn

##prepare the base data

coffee_main <- 
  coffee %>% 
  drop_na(age,
          cups,
          favorite,
          strength,
          roast_level,
          caffeine,
          expertise,
          prefer_overall,
          total_spend,
          most_paid,
          most_willing,
          value_cafe,
          gender,
          education_level,
          ethnicity_race,
          coffee_a_bitterness,
          coffee_a_acidity,
          coffee_a_personal_preference,
          coffee_b_bitterness,
          coffee_b_acidity,
          coffee_b_personal_preference,
          coffee_c_bitterness,
          coffee_c_acidity,
          coffee_c_personal_preference,
          coffee_d_bitterness,
          coffee_d_acidity,
          coffee_d_personal_preference) %>%
  select("age","cups","favorite","strength","roast_level",
         "caffeine","expertise","prefer_overall","total_spend",
         "most_paid","most_willing","value_cafe","gender",
         "education_level","ethnicity_race",
         "coffee_a_bitterness","coffee_a_acidity","coffee_a_personal_preference",
         "coffee_b_bitterness","coffee_b_acidity","coffee_b_personal_preference",
         "coffee_c_bitterness","coffee_c_acidity","coffee_c_personal_preference",
         "coffee_d_bitterness","coffee_d_acidity","coffee_d_personal_preference")


##Mosaic Plot for 
##Roast Level Preference by Gender

##create factor for roast level order
coffee_main$roast_level <- factor(coffee_main$roast_level, levels = c("Light", 
                                                                      "Medium",
                                                                      "Dark"))
##create new df for graph

roast_c <-
  coffee_main %>%
  select("roast_level","gender") %>%
  filter(
    gender %in% c("Male","Female","Non-binary")
    &
      roast_level %in% c("Dark","Medium","Light")
  ) %>%
  arrange(roast_level)


##mosaic plot

##install.packages("ggplot2")
##install.packages("ggmosaic")
##library(ggplot2)
##library(ggmosaic)

roast_c_g <-
  ggplot(data = roast_c) +
  geom_mosaic(aes(x = product(gender), fill=roast_level)) +
  scale_fill_manual(values = c("Dark"="#A96338", "Medium"="#CAA46E", "Light"="#EDCC8B")) +
  labs(
    title = "Roast Level Preference by Gender",
    caption = "#TidyTuesday: 2024 Week 20
                github.com/zulalcelik"
  ) +
  theme(
    rect = element_rect(fill = "transparent"),
    
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    
    axis.title = element_blank(),
    axis.text.x = element_text(color = "#6c3c0c", angle = 90),
    axis.text.y = element_text(color = "#6c3c0c"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    
    text = element_text(family = "mono", size = 18),
    
    plot.title = element_text(size = 30, color = "#6c3c0c", hjust = 0.5, vjust = 1),
    plot.background = element_rect(fill = "transparent",
                                   colour = NA_character_),
    
    panel.background = element_rect(fill = "transparent",
                                    colour = NA_character_),
    legend.position = 'none',
    
    plot.caption = element_text(family = "mono", 
                                size = 6, 
                                color = "#6c3c0c")
  ) 

roast_c_g



##create factor for age order

coffee_main$age <- factor(coffee_main$age, levels = c("<18 years old", 
                                                      "18-24 years old",
                                                      "25-34 years old",
                                                      "35-44 years old",
                                                      "45-54 years old",
                                                      "55-64 years old",
                                                      ">65 years old"))

##create new df for graph

roast_age_c <-
  coffee_main %>%
  select("roast_level","age") %>%
  filter(
    roast_level %in% c("Dark","Medium","Light")
  ) %>%
  arrange(age,roast_level)


##mosaic plot

##install.packages("ggplot2")
##install.packages("ggmosaic")
##library(ggplot2)
##library(ggmosaic)

roast_age_c_g <-
  ggplot(data = roast_age_c) +
  geom_mosaic(aes(x = product(age), fill=roast_level)) +
  scale_fill_manual(values = c("Dark"="#A96338", "Medium"="#CAA46E", "Light"="#EDCC8B")) +
  labs(
    title = "Roast Level Preference by Age",
    caption = "#TidyTuesday: 2024 Week 20
                github.com/zulalcelik"
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    
    axis.title = element_blank(),
    axis.text.x = element_text(color = "#6c3c0c", angle = 90),
    axis.text.y = element_text(color = "#6c3c0c"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    
    text = element_text(family = "mono", size = 18),
    
    plot.title = element_text(size = 30, color = "#6c3c0c", hjust = 0.5, vjust = 1),
    plot.background = element_rect(fill = "transparent",
                                   colour = NA_character_),
    
    panel.background = element_rect(fill = "transparent",
                                    colour = NA_character_),
    
    legend.position = 'none',
    
    plot.caption = element_text(family = "mono", 
                                size = 6, 
                                color = "#6c3c0c")
  )

roast_age_c_g



##sankey diagram for gender&age&roast_level&prefer_all

all_category_c <-
  coffee_main %>%
  select("gender","age","roast_level","prefer_overall") %>%
  filter(
    roast_level %in% c("Dark","Medium","Light")
    &
      gender %in% c("Male","Female","Non-binary")
  ) %>%
  arrange(age,roast_level)


install.packages("devtools")
devtools::install_github("davidsjoberg/ggsankey")


library(ggsankey)
library(ggplot2)
library(dplyr)


##create sankey diagram

all_category_c$age<- as.integer(all_category_c$age)
all_category_c$age <- cut(all_category_c$age, 
                          breaks = c("<18 years old", 
                                     "18-24 years old",
                                     "25-34 years old",
                                     "35-44 years old",
                                     "45-54 years old",
                                     "55-64 years old",
                                     ">65 years old"), 
                          labels = c("<18",
                                     "18-24",
                                     "25-34",
                                     "35-44",
                                     "45-54",
                                     "55-64",
                                     ">65"),
                          right = FALSE)




# Step 1
df <- all_category_c %>%
  make_long(gender,age,roast_level, prefer_overall)

# Step 2
dagg <- df%>%
  dplyr::group_by(node)%>%
  tally()


# Step 3
df2 <- merge(df, dagg, by.x = 'node', by.y = 'node', all.x = TRUE)


# Chart
pl <- ggplot(df2, aes(x = x,
                      next_x = next_x,
                      node = node,
                      next_node = next_node,
                      fill = factor(node),
                      ##label = paste0(node),
                      label = ifelse(node == 1, "<18", 
                                ifelse(node == 2, "18-24", 
                                  ifelse(node == 3, "25-34",
                                    ifelse(node == 4, "35-44",
                                      ifelse(node == 5, "45-54",
                                        ifelse(node == 6, "55-64",
                                          ifelse(node == 7, ">65",
                                            paste0(node))))))))
)
) +
  
  geom_sankey(
    flow.alpha = 0.7, 
    show.legend = TRUE, 
    width = 0.15,
    position = "identity"
  ) +
  
  geom_sankey_label(
    family = "mono", 
    size = 4, 
    color = "white", 
    hjust = 0.5, 
    angle = 90
  ) +
  
  labs(
    title = "Coffee Preference to Participant Profiles",
    caption = "#TidyTuesday: 2024 Week 20
                github.com/zulalcelik"
  ) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(family = "mono", 
                              size = 30, 
                              color = "#6c3c0c", 
                              hjust = 0.5, 
                              vjust = 1),
    plot.background = element_rect(fill = "transparent",
                                   colour = NA_character_),
    panel.background = element_rect(fill = "transparent",
                                    colour = NA_character_),
    legend.title = element_blank(),
    legend.text = element_text(colour = "#6c3c0c", 
                               size = 72),
    legend.background = element_rect(fill = "transparent"),
    axis.text.x = element_blank(),
    
    plot.caption = element_text(family = "mono", 
                                size = 8, 
                                color = "#6c3c0c")
  ) + 
  
  scale_fill_manual(values = c('Dark'="#A96338", 
                               'Medium'="#CAA46E", 
                               'Light'="#EDCC8B",
                               'Male' = "#829B88", 
                               'Female' = "#A36361",
                               'Non-binary' = "#E48150",
                               '1' = "#AAB8BB",
                               '2' = "#CBD5C0",
                               '3' = "#BEE3AB",
                               '4' = "#79B791",
                               '5' = "#6C8976",
                               '6' = "#609595",
                               '7' = "#596869",
                               'Coffee A' = "#E8A995",
                               'Coffee B' = "#BBCBD2",
                               'Coffee C' = "#63767A",
                               'Coffee D' = "#C28B7C")
  ) 

pl












