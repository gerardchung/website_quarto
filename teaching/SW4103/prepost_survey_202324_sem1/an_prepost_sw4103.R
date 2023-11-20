# Title         : Pre and post test scors
# Description   : Analyzing scores for survey in SW4103. The pre and post are connected to same student system-assigned id
#
# Author        : Gerard
# Email         : 
# Data          : `r paste(date())`

rm(list=ls())

pacman::p_load(tidyverse, janitor, ggplot2)

library(readr)


# Pre-settings

pacman::p_load(ggplot2, tidyverse, gt, here, janitor, knitr)
getwd()
library(readxl)

library(showtext)

font_add_google("roboto condensed")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

jco_yellow = "#EFC000FF"
jco_blue = "#7AA6DCFF"
jco_darkblue = "#0073C2FF"
jco_red = "#CD534CFF"
jco_darkred = "#A73030FF"
jco_gray = "#868686FF"
font = "roboto condensed"
label_font <- "roboto condensed"

bcolor <- "#7EC8E3"
fontcolor <- "#000000"
nus_blue = "#003D7C"
nus_orange = "#EF7C00"

# LOAD DATA  ####
data_pre <- read_csv(here("prepost_survey_202324_sem1", "data", "pre_semester_survey Survey Student Analysis Report.csv"))
data_post <- read_csv(here("prepost_survey_202324_sem1", "data", "post_semester_survey Survey Student Analysis Report.csv"))

names(data_pre)

data_pre <-
    data_pre %>% 
    clean_names()

data_post <-
    data_post %>% 
    clean_names()
    
names(data_pre)
names(data_post)



# CLEAN NAMES ####

Q1 = "how_confident_are_you_that_you_can_create_and_carry_out_an_effective_search_strategy_for_academic_databases_and_related_resources_to_obtain_the_scholarly_literature_necessary_to_design_your_evaluation_of_social_work_practice"

Q2 = "how_confident_are_you_that_you_can_critically_examine_and_accurately_summarize_the_variety_of_infographics_used_to_convey_evaluation_data_visually"

Q3 = "how_confident_are_you_thatyou_can_critically_review_a_particular_area_of_social_science_theory_and_research_and_write_a_balanced_comprehensive_literature_review"

Q4 = "critically_examine_and_accurately_describe_the_findings_of_basic_systematic_reviews_and_meta_analyses"

Q5 = "create_a_program_logic_model_that_clearly_communicates_the_key_factors_and_processes_of_a_social_work_program_or_program_component"

Q6 = "create_and_carry_out_a_sampling_strategy_for_an_evaluation_of_social_work_practice"

Q7 = "create_and_carry_out_a_measurement_approach_for_an_evaluation_of_social_work_practice"

Q8 = "create_and_carry_out_a_qualitative_evaluation_of_social_work_practice"

Q9 = "create_and_carry_out_a_mixed_methods_evaluation_of_social_work_practice"

Q10 = "create_and_carry_out_a_study_regarding_the_implementation_or_processes_of_a_social_work_program_e_g_needs_assessment_formative_evaluation_program_description_program_monitoring"

Q11 = "create_and_carry_out_a_single_system_design_to_evaluate_the_outcomes_of_social_work_practice"

Q12 = "create_and_carry_out_a_group_research_design_to_evaluate_the_outcomes_of_social_work_practice"

Q13 = "create_and_carry_out_an_evaluation_of_social_work_practice_that_incorporates_social_work_values_and_ethics_e_g_protects_the_participants_in_the_evaluation"

Q14 = "create_and_carry_out_an_evaluation_of_social_work_practice_while_resisting_political_pressures_from_stakeholders_that_could_bias_the_results_or_the_evaluation_report"

Q15 = "create_and_carry_out_a_descriptive_data_analysis_plan_including_data_entry_and_management_for_an_evaluation_of_social_work_practice"

Q16 = "create_and_carry_out_an_inferential_data_analysis_plan_for_an_evaluation_of_social_work_practice"


## data_pre ====
data_pre <- 
    data_pre %>%
    rename_with(~ "Q1", contains(Q1)) %>% 
    rename_with(~ "Q2", contains(Q2)) %>% 
    rename_with(~ "Q3", contains(Q3)) %>% 
    rename_with(~ "Q4", contains(Q4)) %>% 
    rename_with(~ "Q5", contains(Q5)) %>% 
    rename_with(~ "Q6", contains(Q6)) %>% 
    rename_with(~ "Q7", contains(Q7)) %>% 
    rename_with(~ "Q8", contains(Q8)) %>% 
    rename_with(~ "Q9", contains(Q9)) %>% 
    rename_with(~ "Q10", contains(Q10)) %>% 
    rename_with(~ "Q11", contains(Q11)) %>% 
    rename_with(~ "Q12", contains(Q12)) %>% 
    rename_with(~ "Q13", contains(Q13)) %>% 
    rename_with(~ "Q14", contains(Q14)) %>% 
    rename_with(~ "Q15", contains(Q15)) %>% 
    rename_with(~ "Q16", contains(Q16)) 

names(data_pre)

data_pre <-
    data_pre %>% 
    select(id,  contains("Q"))

## make them all characters so that the next codes will not have problems if one of the var is numerical

data_pre <- data.frame(lapply(data_pre, function(x) {
    if (is.numeric(x) && any(grepl("Q", names(data_pre)))) {
        as.character(x)
    } else {
        x
    }
}))



data_pre <-
    data_pre %>% 
    mutate(across(contains("Q"), ~ if_else(str_detect(., pattern="Moderately certain can do"), "5", .)),
           across(contains("Q"), ~ if_else(str_detect(., pattern="Can not do at all"), "0", .)),
           across(contains("Q"), ~ if_else(str_detect(., pattern="Certain can do"), "10", .))) 



## data_post ====
data_post <- 
    data_post %>%
    rename_with(~ "Q1", contains(Q1)) %>% 
    rename_with(~ "Q2", contains(Q2)) %>% 
    rename_with(~ "Q3", contains(Q3)) %>% 
    rename_with(~ "Q4", contains(Q4)) %>% 
    rename_with(~ "Q5", contains(Q5)) %>% 
    rename_with(~ "Q6", contains(Q6)) %>% 
    rename_with(~ "Q7", contains(Q7)) %>% 
    rename_with(~ "Q8", contains(Q8)) %>% 
    rename_with(~ "Q9", contains(Q9)) %>% 
    rename_with(~ "Q10", contains(Q10)) %>% 
    rename_with(~ "Q11", contains(Q11)) %>% 
    rename_with(~ "Q12", contains(Q12)) %>% 
    rename_with(~ "Q13", contains(Q13)) %>% 
    rename_with(~ "Q14", contains(Q14)) %>% 
    rename_with(~ "Q15", contains(Q15)) %>% 
    rename_with(~ "Q16", contains(Q16)) 


data_post <-
    data_post %>% 
    select(id, contains("Q"))

## make them all characters so that the next codes will not have problems if one of the var is numerical

data_post <- data.frame(lapply(data_post, function(x) {
    if (is.numeric(x) && any(grepl("Q", names(data_post)))) {
        as.character(x)
    } else {
        x
    }
}))



data_post <-
    data_post %>% 
    mutate(across(contains("Q"), ~ if_else(str_detect(., pattern="Moderately certain can do"), "5", .)),
           across(contains("Q"), ~ if_else(str_detect(., pattern="Can not do at all"), "0", .)),
           across(contains("Q"), ~ if_else(str_detect(., pattern="Certain can do"), "10", .))) 

# MERGE #####


data_post <-
    data_post %>% 
    mutate(wave = "post")

data_pre <-
    data_pre %>% 
    mutate(wave = "pre")


data_combined <- bind_rows(data_pre, data_post)
    
# convert all to numeric

data_combined <-
    data_combined %>% 
    mutate(across(contains("Q"), ~ as.numeric(.)))

# ANALYSIS & CALCULATE AT GROUP-LEVEL  ####


group <- 
    data_combined %>% 
    rowwise() %>% 
    mutate(total_score = sum(c_across(contains("Q"))),
           mean_score = mean(c_across(contains("Q")))) %>% 
    ungroup() %>% 
    group_by(wave) %>% 
    summarise(mean_sum_overall = mean(total_score),
              mean_aver_overall = mean(mean_score),
              sd_overall = sd(total_score)) %>% 
    ungroup() 
    
#group %>% knitr::kable()

group %>% gt::gt()


## plot increases from pre to post
names(group)

group %>% 
  mutate(wave = factor(wave, levels = c("pre", "post"), labels = c("Pre-course", "Post-course"))) %>% 
  ggplot(aes(x = wave, y = mean_aver_overall)) +
  geom_line(aes(group = 1),
            arrow = arrow(type = "closed", length = unit(0.08, "inches"))) +
  geom_point(aes(color=wave), size = 4, alpha = .8) + 
  scale_color_manual(values = c("Pre-course" = jco_red, "Post-course" = jco_darkblue)) +
  scale_y_continuous(limits = c(0, 8))  +
  labs(title = "Evaluation Self-efficacy Improved at Post-SW4103 Course",
       subtitle = "Average improvements was 3.21",
       x = "",
       y = "Average Self-efficacy",
       caption = "Cohen's D at 2.82, interpreted as large ES") +
  theme_minimal() +
  theme(panel.grid.major.x=element_line(linewidth =0.05),
        legend.position = "none",
        plot.title = element_text(size=20, face="bold",  family = font),
        plot.subtitle = element_text(size=16, face="bold", family = font),
        axis.title.y = element_text(size = 12, family = font),
        axis.title.x = element_text(size = 12,family = font),
        axis.text.y = element_text(size=8, family = font),
        axis.text.x = element_text(size=8, family = font),
        axis.ticks.y = element_blank(),
        plot.caption = element_text(size=9, color = "black", family = font)) +
  
  geom_text(aes(label = sprintf("%.2f", mean_aver_overall)), 
            vjust = -1,    # Adjust vertical position of text
            size = 3)     # Adjust text size


 
data_combined1 <-
    data_combined %>% 
    rowwise() %>% 
    mutate(total = sum(c_across(contains("Q")))) %>% 
    ungroup() 
    
data_wide <-
    data_combined1 %>% 
    select(id, wave, total) %>% 
    pivot_wider(#id_cols = "id",
                names_from = wave,
                values_from = total)

t.test(Pair(pre, post) ~ 1, data = data_wide)
t.test(Pair(pre, post) ~ 1, data = data_wide)


library(effectsize)

es <- cohens_d(total ~ wave, data = data_combined1)
interpret_cohens_d(es, rules = "cohen1988")

# CREATE CHANGE IN SCORES FOR EACH ITEM #### 

data_item <-
    data_combined %>% 
    pivot_wider(#id_cols = "id",
        names_from = wave,
        values_from = contains("Q"))

    
names(data_item)
t.test(Pair(Q1_pre, Q1_post) ~ 1, data = data_item)

for (i in 1:16) {
    question_pre = paste0("Q", i, "_pre")
    question_post = paste0("Q", i, "_post")
    formula = as.formula(paste0("Pair(", question_pre, ", ", question_post, ") ~ 1"))
    print(t.test(formula, data = data_item))
}


# ANALYZE AND CHANGE FOR EACH ITEM #####


for (i in 1:16) {
    question_pre = sym(paste0("Q", i, "_pre"))
    question_post = sym(paste0("Q", i, "_post"))
    item_change = paste0("Q", i, "_change")
    
    data_item <- data_item %>%
        mutate(!!item_change := !!question_post - !!question_pre)
}


names(data_item)

data_item_long <-
    data_item %>% 
    pivot_longer(
        cols = -id, # Excluding the 'id' column from pivoting
        names_to = c(".value", "question"), # Splitting the column names
        names_pattern = "(Q\\d+)_(.*)" # Regex pattern to capture question number and suffix
    )


## LOOK ONLY AT CHANGES IN SCORES AT ITEM LEVEL ####

data_item_long_change <-
    data_item_long %>% 
    filter(question == "change") %>% 
    select(-question)

### LONG FORMAT =====
data_item_long_change2 <-
    data_item_long_change %>% 
    pivot_longer(
        cols = starts_with("Q"),       # Selecting columns that start with 'Q'
        names_to = "question_number",  # Name of the new column for question numbers
        names_pattern = "Q(\\d+)"      # Regex pattern to capture only the numbers after 'Q'
    )

mean_change <-
    data_item_long_change2 %>% 
    group_by(question_number) %>% 
    summarize(mean_change = mean(value)) %>% 
    ungroup()

mean(mean_change$mean_change)
sd(mean_change$mean_change)

data_item_long_change2 %>% 
    ggplot(aes(y = value, x = as.numeric(question_number))) +
    geom_point() +
    geom_jitter() + 
    geom_point(data = mean_change,
               aes(y = mean_change, x = as.numeric(question_number)),
               color = "red", alpha = .4,
               size = 8) +
    scale_x_continuous(breaks = 1:16, limits = c(1, 16))


# Plot mean pre and mean post 


data_item_long_prepost <-
    data_item_long %>% 
    filter(question != "change") 


mean_prepost <-
    data_item_long_prepost %>% 
    group_by(question) %>% 
    summarise(across(Q1:Q16, ~ mean(.))) %>% 
#    summarise(mean = across(Q1:Q16, mean)) %>% 
    ungroup()

names(mean_prepost)

mean_prepost_wide <-
    mean_prepost %>% 
    rename(wave = question) %>% 
    pivot_longer(cols = starts_with("Q"),      
                 names_to = "question_number", 
                 names_pattern = "Q(\\d+)")    
  
names(mean_prepost_wide)

library(ggrepel)


ggplot(mean_prepost_wide, aes(y = as.numeric(question_number), x = value, group=as.numeric(question_number))) +
    geom_line() +
    geom_point(aes(color=wave), 
               size = 4, alpha = .8) +

    theme_minimal() +
    scale_y_continuous(breaks = 1:16, limits = c(1, 16)) +
    theme(legend.position = "bottom") +
  scale_x_continuous(limits = c(0, 10), labels = scales::label_number(scale = 1, suffix = "", accuracy = 1)) +
  
  scale_color_manual(values = c("pre" = jco_red, "post" = jco_darkblue)) +
  labs(title = "Mean Scores of Evaluation Self-efficacy for each item",
       subtitle = "For Q16, the mean scores increased from 3.25 to 6.5 ",
       x = "Average ESE scores",
       y = "Question Items",
       caption = "") +
  theme(panel.grid.major.x=element_line(linewidth =0.05),
        legend.position = "bottom",
        plot.title = element_text(size=16, face="bold",  family = font),
        plot.subtitle = element_text(size=12, family = font),
        axis.title.y = element_text(size = 10, family = font),
        axis.title.x = element_text(size = 10,family = font),
        axis.text.y = element_text(size=8, family = font),
        axis.text.x = element_text(size=8, family = font),
        axis.ticks.y = element_blank(),
        plot.caption = element_text(size=9, color = "black", family = font)
  ) +
  geom_text_repel(
    aes(label = ifelse(question_number %in% c("1", "16"), sprintf("%.2f", value), "")),
    vjust = -0.5, hjust = 1.0, family = font
  )
  
  
  


d <- dput(mean_prepost_wide)


data_item_long_prepost <-
    data_item_long_prepost %>% 
    rowwise() %>% 
    mutate(mean = mean(c_across(Q1:Q16))) %>% 
    ungroup() %>% 
    select(id, question, mean)



data_item_long_prepost %>% 
    ggplot()




