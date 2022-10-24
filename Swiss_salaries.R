library(tidyquant)
library(tidyverse)
library(googlesheets4)
library(scales)
library(tidymodels)
library(yardstick) 
library(broom)

# Data ----
URL = "https://docs.google.com/spreadsheets/d/1SOgRtFXKUAKL9MkenNSFR6QENOgzs4OIAr-TrpzlrK0/edit#gid=0"
sheet_id = gs4_get(URL)
salary_data = read_sheet(ss = sheet_id) %>% 
    mutate(competence_level = case_when(
        competence_level == 4 ~ "Elevé",
        competence_level == 3 ~ "Moyen-Elevé",
        competence_level == 2 ~ "Moyen-Bas",
        TRUE ~ "Bas"
    ))


# Visualizations ----
plot_data = function(tbl, column){
    
    tbl %>% 
        group_by({{ column }}) %>% 
        summarise(median_salary = median(salary)) %>% 
        ungroup() %>% 
        
        mutate({{ column }} := {{ column }} %>% fct_reorder(median_salary)) %>% 
        
        ggplot(aes({{ column }}, median_salary)) +
        
        geom_col() +
        geom_text(aes(label = dollar(median_salary, prefix = "", big.mark = "'", suffix = " CHF"), hjust = 1.2), color = "white") +
    
        
        coord_flip() +
        scale_y_continuous(labels=dollar_format(prefix = "", big.mark = "'", suffix = " CHF"))
    
}

# professional group
plot_data(salary_data, professional_group) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 50))

# age
plot_data(salary_data, age)

# competence level
plot_data(salary_data, competence_level)

# gender
plot_data(salary_data, gender) +
    geom_text(aes(label = dollar(median_salary, prefix = "", big.mark = "'", suffix = " CHF"), hjust = 1.2), color = "white")

# histogram of salaries
salary_data %>% 
    
    ggplot(aes(salary)) +
    
    geom_histogram() +
    geom_vline(xintercept = salary_data$salary %>% median()) +
    
    theme_tq() +
    scale_x_continuous(labels=dollar_format(prefix = "", big.mark = "'", suffix = " CHF"))

lm(data = salary_data, salary ~ competence_level + professional_group + professional_subgroup + age + gender) %>% 
    anova() %>% 
    tidy() %>% 
    mutate(pct_variance = sumsq / sum(sumsq)) %>% 
    arrange(desc(pct_variance))

                     