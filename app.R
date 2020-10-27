library(shiny)
library(shinyjs)
library(ggrepel)
library(zoo)
library(directlabels)
library(scales)
library(zoo)
library(plotly)
library(lubridate)
library(RcppRoll)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Takhle by to mohlo být, ale zkuste si to sami!
             
             Projekce denních úmrtí pro ČR"),
  column(3,
         sliderInput(inputId = "week0",
                     label = "Hodnota R pro tento týden",
                     value = 1.5, min = 0, max = 5, step = 0.1, playButton),
         sliderInput(inputId = "week1",
                     label = "Hodnota R pro příští týden",
                     value = 1.5, min = 0, max = 5, step = 0.1),
         sliderInput(inputId = "week2",
                     label = "Hodnota R pro třetí týden",
                     value = 1.5, min = 0, max = 5, step = 0.1)
         ),
  column(3, 
         sliderInput(inputId = "week3",
                       label = "Hodnota R pro čtvrtý týden",
                       value = 1.5, min = 0, max = 5, step = 0.1),
         sliderInput(inputId = "week4",
                     label = "Hodnota R pro patý týden",
                     value = 1.5, min = 0, max = 5, step = 0.1),
         sliderInput(inputId = "week5",
                     label = "Hodnota R pro šestý a další týdny",
                     value = 1.5, min = 0, max = 5, step = 0.1)
  ),
  column(3, 
         dateInput(inputId = "date_model_start", label = "Vyberte datum pro začátek projekce", 
                      value = today(tzone = "GMT") - 1),
         dateInput(inputId = "date_model_end", label = "Vyberte konec modelovaného období (ne později než 31/12)", 
                   value = today(tzone = "GMT") + 50)),
  # column(3,
  #        actionButton(inputId = "refresh", "Run simulation")),
  plotOutput("chart", width = "80%",
             height = "800px")
)


server <- function(input, output, session) {
  CZ_dates_future <- read.csv("dates.csv") # required csv, download to make it work
  
  CZ_cases <- read.csv("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/nakaza.csv", 
                       colClasses=c('character',  'numeric','numeric')) %>%
    rename(date = ends_with("datum"),
           new_cases = prirustkovy_pocet_nakazenych,
           cases = kumulativni_pocet_nakazenych) %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d"))
  
  CZ_dead <- read.csv("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/umrti.csv", 
                      colClasses=c('character', 'numeric','factor', 'factor','factor')) %>%
    rename(date = ends_with("datum")) %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    mutate(month = month(date),
           week = week(date),
           age_category = if_else(vek < 20, "0-19", if_else(vek < 40 & vek >= 20, "20-39", 
                                                            if_else(vek < 50 & vek >= 40, "40-49", 
                                                                    if_else(vek < 60 & vek >= 50, "50-59", 
                                                                            if_else(vek < 70 & vek >= 60, "60-69", 
                                                                                    if_else(vek < 80 & vek >= 70, "70-79", 
                                                                                            if_else(vek < 90 & vek >= 80, "80-89", 
                                                                                                    if_else(vek >= 90, "90+", "error")))))))))
  
  
  CZ_positive <- read.csv("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/osoby.csv", 
                          colClasses=c('character', 'numeric','factor', 'factor','factor', 'factor', 'factor')) %>%
    rename(date = ends_with("datum")) %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    mutate(month = month(date),
           week = week(date),
           age_category = if_else(vek < 20, "0-19", 
                                  if_else(vek < 40 & vek >= 20, "20-39", 
                                          if_else(vek < 50 & vek >= 40, "40-49", 
                                                  if_else(vek < 60 & vek >= 50, "50-59", 
                                                          if_else(vek < 70 & vek >= 60, "60-69", 
                                                                  if_else(vek < 80 & vek >= 70, "70-79", 
                                                                          if_else(vek < 90 & vek >= 80, "80-89", 
                                                                                  if_else(vek >= 90, "90+", "error")))))))))
  
  
  CZ_death_timing <- read.csv("profil_umrti.csv", colClasses=c('factor', 'numeric', 'numeric')) # required csv, key assumptions
  
  CZ_all <- read.csv("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/nakazeni-vyleceni-umrti-testy.csv", 
                     colClasses=c('character', 'numeric','numeric', 'numeric','numeric')) %>%
    rename(date = ends_with("datum"),
           tests = kumulativni_pocet_testu,
           cases = kumulativni_pocet_nakazenych,
           deaths = kumulativni_pocet_umrti,
           recovered = kumulativni_pocet_vylecenych) %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d"),
           new_deaths = deaths - lag(deaths, 1),
           new_deaths = if_else(is.na(new_deaths) == TRUE, 0, new_deaths),
           new_cases = cases - lag(cases, 1),
           new_cases = if_else(is.na(new_cases) == TRUE, 0, new_cases),
           new_tests = tests - lag(tests, 1),
           new_tests = if_else(is.na(new_tests) == TRUE, 20, new_tests),
           new_recovered = recovered - lag(recovered, 1),
           new_recovered = if_else(is.na(new_recovered) == TRUE, 0, new_recovered),
           active_now = cases-recovered-deaths,
           active_change = new_cases-new_recovered-new_deaths,
           pct_positive = cases/tests,
           new_pct_positive = new_cases/new_tests,
           new_deaths_7 = rollapply(new_deaths, width=7, FUN=function(x) mean(x, na.rm=TRUE), 
                                    by=1,  by.column=TRUE, partial=TRUE, fill=NA, align="right"),
           new_cases_7 = rollapply(new_cases, width=7, FUN=function(x) mean(x, na.rm=TRUE), 
                                   by=1,  by.column=TRUE, partial=TRUE, fill=NA, align="right"),
           new_pct_positive_7 = rollapply(new_pct_positive, width=7, FUN=function(x) mean(x, na.rm=TRUE), by=1,  by.column=TRUE, partial=TRUE, fill=NA, align="right"),
           new_tests_7 = rollapply(new_tests, width=7, FUN=function(x) mean(x, na.rm=TRUE), 
                                   by=1,  by.column=TRUE, partial=TRUE, fill=NA, align="right"),
           new_recovered_7 = rollapply(new_recovered, width=7, FUN=function(x) mean(x, na.rm=TRUE), 
                                       by=1,  by.column=TRUE, partial=TRUE, fill=NA, align="right"),
           WoW_new_cases_7 = new_cases_7/lag(new_cases_7,7)-1,
           WoW_new_deaths_7 = new_deaths_7/lag(new_deaths_7,7)-1,
           WoW_new_recovered_7 = new_recovered_7/lag(new_recovered_7,7)-1,
           WoW_new_pct_positive_7 = new_pct_positive_7/lag(new_pct_positive_7,7)-1,
           WoW_new_tests_7 = new_tests_7/lag(new_tests_7,7)-1)
  
  
  ##### Postpone dates of positive cases to match-up the cases and deaths in order to be able to caluclate fatality in past data #####
  
  CZ_dead_delay <- CZ_dead %>%
    mutate(month = month(date),
           week = week(date))
  CZ_positive_delay <- CZ_positive %>%
    mutate(date = if_else(vek < 70, date + 14, 
                          if_else(vek < 80 & vek >= 70, date + 13, 
                                  if_else(vek < 90 & vek >= 80, date + 7, 
                                          if_else(vek >= 90, date + 6, date))))) %>%
    filter(date <= max(CZ_dead$date)) %>%
    mutate(month = month(date),
           week = week(date))
  
  ## summarise
  
  CZ_positive_age_category_month_delay <- CZ_positive_delay %>%
    group_by(month, age_category) %>%
    summarise(count_cases = n())
  CZ_dead_age_category_month_delay <- CZ_dead_delay %>%
    group_by(month, age_category) %>%
    summarise(count_dead = n())
  
  ## calculate fatality per age category
  
  CZ_positive_age_category <- CZ_positive_delay %>%
    group_by(age_category) %>%
    summarise(count_cases = as.numeric(n()))
  CZ_dead_age_category <- CZ_dead_delay %>%
    group_by(age_category) %>%
    summarise(count_dead = as.numeric(n()))
  CZ_positive_dead_age_category <- CZ_positive_age_category %>%
    left_join(CZ_dead_age_category, by = c("age_category")) %>%
    mutate(fatality = count_dead/count_cases)
  CZ_positive_dead_age_category[is.na(CZ_positive_dead_age_category)] <- 0
  CZ_fatality <- CZ_positive_dead_age_category %>%
    select(-count_cases, -count_dead)
  
  ##### get dates #####
  
  date_today <- today(tzone = "GMT") - 1
  week_now <- week(date_today)
  
  CZ_dates <- CZ_cases %>%
    distinct(date) %>%
    filter(date > as.Date("29/02/2020", format = "%d/%m/%Y")) %>%
    mutate(match = 1)
  
  CZ_age_categories <- CZ_fatality %>%
    select(age_category) %>%
    mutate(match = 1)
  
  CZ_dates_age_categories <- CZ_dates %>%
    left_join(CZ_age_categories, by = c("match")) %>%
    select(-match)
  
  
  CZ_dates_age_categories_future <- CZ_dates_future %>%
    select(-match) %>%
    mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
    filter(date > as.Date("29/02/2020", format = "%d/%m/%Y")) %>%
    filter(date <= date_today + 90) %>%
    mutate(match = 1) %>%
    left_join(CZ_age_categories, by = c("match")) %>%
    select(-match)
  
  
  ##### end for dates #####
  
  
  ##### Expected deaths time series - testing model #####
  
  CZ_Positive_expected_death <- CZ_positive %>%
    left_join(CZ_death_timing, by = c("age_category")) %>%
    left_join(CZ_fatality, by = c("age_category")) %>%
    mutate(date_expected_death = as.Date(date, format = "%d/%m/%Y") + delay,
           expected_death = probability * fatality)
  
  CZ_expected_deaths_timeline <- CZ_Positive_expected_death %>%
    group_by(date_expected_death) %>%
    summarise(expected_deaths = sum(expected_death)) %>%
    arrange(date_expected_death) %>%
    mutate(expected_deaths_7 = rollapply(expected_deaths, width=7, 
                                         FUN=function(x) mean(x, na.rm=TRUE), by=1,  
                                         by.column=TRUE, partial=TRUE, fill=NA, align="right"))
  
  ## Expected deaths by Age category
  
  CZ_expected_deaths_timeline_by_age <- CZ_Positive_expected_death %>%
    group_by(date_expected_death, age_category) %>%
    summarise(expected_deaths = sum(expected_death))
  
  CZ_expected_deaths_timeline_by_age <- CZ_dates_age_categories %>%
    left_join(CZ_expected_deaths_timeline_by_age, by = c("date" = "date_expected_death", "age_category")) %>%
    mutate(expected_deaths = if_else(is.na(expected_deaths) == TRUE, 0, expected_deaths)) %>%
    arrange(date) %>%
    arrange(age_category) %>%
    group_by(age_category) %>%
    mutate(expected_deaths_7 = rollapply(expected_deaths, 
                                         width=7, 
                                         FUN=function(x) mean(x, na.rm=TRUE), 
                                         by=1,  
                                         by.column=TRUE, partial=TRUE, fill=NA, 
                                         align="right"))
  
  ## compute deaths by age to backtest model
  
  CZ_deaths_by_age <- CZ_dead %>%
    group_by(date, age_category) %>%
    summarise(deaths = n()) %>%
    mutate(deaths = as.numeric(deaths))
  
  CZ_deaths_by_age <- CZ_dates_age_categories %>%
    left_join(CZ_deaths_by_age, by = c("date", "age_category")) %>%
    mutate(deaths = if_else(is.na(deaths) == TRUE, 0, deaths)) %>%
    arrange(date) %>%
    arrange(age_category) %>%
    group_by(age_category) %>%
    mutate(deaths_7 = rollapply(deaths, width=7, FUN=function(x) mean(x, na.rm=TRUE), by=1,  
                                by.column=TRUE, partial=TRUE, fill=NA, align="right"))
  
  
  
  ##### Expected deaths with projected cases into the future #####
  
  ## Predict R value of future cases growth per scenario per week
  
  CZ_expected_deaths_timeline_projected2 <- CZ_positive %>%
    group_by(date, age_category) %>%
    summarise(cases = as.numeric(n())) %>%
    ungroup()
  
  number_of_scenarios <- 5
  
  #!!!!! From now everything needs re-rendering each time input changes !!!!!# 
  
  
  output$chart <- renderPlot({
    date_today <- today(tzone = "GMT") - 1
    date_model <- input$date_model_start
    date_model_end <- input$date_model_end
    week_now <- week(date_today)
    
    
    CZ_cases_by_age_projected <- CZ_dates_age_categories_future %>%
      left_join(CZ_expected_deaths_timeline_projected2, by = c("date", "age_category")) %>%
      mutate(cases = if_else(is.na(cases) == TRUE, 0, cases),
             week = week(date),
             R_1 = case_when(week == week_now - 5 ~ 1.5,
                             week == week_now - 4 ~ 1.5,
                             week == week_now - 3 ~ 1.5,
                             week == week_now - 2 ~ 1.5,
                             week == week_now - 1 ~ 1.5,
                             week == week_now + 0 ~ 1.5, # measures inefficient, no extra action
                             week == week_now + 1 ~ 1.5,
                             week == week_now + 2 ~ 1.4,
                             week == week_now + 3 ~ 1.4,
                             week == week_now + 4 ~ 1.4,
                             week == week_now + 5 ~ 1.4,
                             week == week_now + 6 ~ 1.4,
                             week == week_now + 7 ~ 1.4,
                             week == week_now + 8 ~ 1.4,
                             week == week_now + 9 ~ 1.4,
                             week == week_now + 10  ~ 1.4)^(1/7),
             R_2 = case_when(week == week_now - 5 ~ 1.5,
                             week == week_now - 4 ~ 1.5,
                             week == week_now - 3 ~ 1.5,
                             week == week_now - 2 ~ 1.5,
                             week == week_now - 1 ~ 1.5,
                             week == week_now + 0 ~ 1.5, # lockdown in 2 weeks
                             week == week_now + 1 ~ 1.5,
                             week == week_now + 2 ~ 1.4,
                             week == week_now + 3 ~ 1.3,
                             week == week_now + 4 ~ 0.9,
                             week == week_now + 5 ~ 0.8,
                             week == week_now + 6 ~ 0.7,
                             week == week_now + 5 ~ 0.6,
                             week == week_now + 7 ~ 0.5,
                             week == week_now + 8 ~ 0.5,
                             week == week_now + 9 ~ 0.5,
                             week == week_now + 10 ~ 0.5)^(1/7),
             R_3 = case_when(week == week_now - 5 ~ 1.5,
                             week == week_now - 4 ~ 1.5,
                             week == week_now - 3 ~ 1.5,
                             week == week_now - 2 ~ 1.5,
                             week == week_now - 1 ~ 1.5,
                             week == week_now + 0 ~ 1.4, # lockdown now
                             week == week_now + 1 ~ 1.3,
                             week == week_now + 2 ~ 0.9,
                             week == week_now + 3 ~ 0.8,
                             week == week_now + 4 ~ 0.7,
                             week == week_now + 5 ~ 0.6,
                             week == week_now + 6 ~ 0.5,
                             week == week_now + 7 ~ 0.5,
                             week == week_now + 8 ~ 0.5,
                             week == week_now + 9 ~ 0.5,
                             week == week_now + 10 ~ 0.5)^(1/7),
             R_4 = case_when(week == week_now - 5 ~ 1.5,
                             week == week_now - 4 ~ 1.5,
                             week == week_now - 3 ~ 1.5,
                             week == week_now - 2 ~ 1.5,
                             week == week_now - 1 ~ 1.5,
                             week == week_now + 0 ~ 1.4, # lockdown  now, but lifted in two weeks
                             week == week_now + 1 ~ 1.3,
                             week == week_now + 2 ~ 0.9,
                             week == week_now + 3 ~ 0.9,
                             week == week_now + 4 ~ 1.3,
                             week == week_now + 5 ~ 1.3,
                             week == week_now + 6 ~ 1.4,
                             week == week_now + 7 ~ 1.4,
                             week == week_now + 8 ~ 1.4,
                             week == week_now + 9 ~ 1.5,
                             week == week_now + 10 ~ 1.5)^(1/7),
             R_5 = case_when(week == week(input$date_model_start) + 0 ~ as.numeric(input$week0), # Own scenario
                             week == week(input$date_model_start) + 1 ~ as.numeric(input$week1),
                             week == week(input$date_model_start) + 2 ~ as.numeric(input$week2),
                             week == week(input$date_model_start) + 3 ~ as.numeric(input$week3),
                             week == week(input$date_model_start) + 4 ~ as.numeric(input$week4),
                             week == week(input$date_model_start) + 5 ~ as.numeric(input$week5),
                             week == week(input$date_model_start) + 6 ~ as.numeric(input$week5),
                             week == week(input$date_model_start) + 7 ~ as.numeric(input$week5),
                             week == week(input$date_model_start) + 8 ~ as.numeric(input$week5),
                             week == week(input$date_model_start) + 9 ~ as.numeric(input$week5),
                             week == week(input$date_model_start) + 10 ~ as.numeric(input$week5),
                             week == week(input$date_model_start) + 11 ~ as.numeric(input$week5),
                             week == week(input$date_model_start) + 12 ~ as.numeric(input$week5),
                             week == week(input$date_model_start) + 13 ~ as.numeric(input$week5))^(1/7)) %>%
      group_by(age_category) %>%
      mutate(cases_7 = rollapply(cases, width=7, FUN=function(x) mean(x, na.rm=TRUE), by=1,  
                                 by.column=TRUE, partial=TRUE, fill=NA, align="right"),
             future = if_else(date > date_model, 1, 0)) %>%
      gather(key = "Scenario", value = "R", R_1, R_2, R_3, R_4, R_5) %>%
      group_by(age_category) %>%
      arrange(date) %>%
      mutate(cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7), 
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7), 
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases_7 = if_else(date > date_model, lag(cases_7, number_of_scenarios) * R, cases_7),
             cases = if_else(date > date_model, cases_7, cases))  %>%
      ungroup() %>%
      mutate(Scenario = case_when(Scenario == "R_1" ~ "Opatření nefungují, žádné další omezení",
                                  Scenario == "R_2" ~ "Lockdown za dva týdny",
                                  Scenario == "R_3" ~ "Lockdown teď", 
                                  Scenario == "R_4" ~ "Lockdown teď, ale uvolněn za 2 týdny",
                                  Scenario == "R_5" ~ "Vlastní scénář"))
    
    
    CZ_deaths_by_age_projected <- CZ_cases_by_age_projected %>%
      left_join(CZ_death_timing, by = c("age_category")) %>%
      left_join(CZ_fatality, by = c("age_category")) %>%
      mutate(date_expected_death = as.Date(date, format = "%d/%m/%Y") + delay,
             expected_death = probability * fatality * cases)
    
    CZ_expected_deaths_timeline_projected <- CZ_deaths_by_age_projected %>%
      group_by(date_expected_death, Scenario) %>%
      summarise(expected_deaths = sum(expected_death)) %>%
      arrange(date_expected_death) %>%
      group_by(Scenario) %>%
      mutate(expected_deaths_7 = rollapply(expected_deaths, width=7, FUN=function(x) mean(x, na.rm=TRUE), by=1,  by.column=TRUE, partial=TRUE, fill=NA, align="right")) %>%
      filter() # end projection before run out of cases? - Do in charts instead
    
    ##### summary data #####
    
    CZ_expected_deaths_timeline_projected_end <- CZ_expected_deaths_timeline_projected %>%
      filter(date_expected_death == date_model_end)
    
    CZ_expected_deaths_timeline_projected_summary <- CZ_expected_deaths_timeline_projected %>%
      mutate(month = month(date_expected_death)) %>%
      group_by(month, Scenario) %>%
      summarise(expected_deaths = sum(expected_deaths))
    
    ##### start charts #####
    
    CZ_expected_deaths_timeline_projected %>% 
      left_join(CZ_all, by = c("date_expected_death" = "date")) %>%
      filter(date_expected_death <= input$date_model_end) %>%
      ggplot(aes(x = date_expected_death, y = round(expected_deaths_7,2), col = Scenario)) +
      geom_line(size = 1) +
      geom_text_repel(aes(label = round(expected_deaths_7,0), colour = Scenario), data = CZ_expected_deaths_timeline_projected_end, size = 3, vjust = 0, hjust = -0.5) +
      geom_line(aes(y = new_deaths_7), col = "black", size = 1) +
      geom_vline(xintercept = date_model, col = "red", linetype = "dashed") +
      scale_color_manual(values = c("#4DAF4A", "#377EB8", "purple", "#E41A1C", "darkorange")) +
      scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels="%b") +
      theme_classic() +
      coord_cartesian(xlim = c(date_model - 30, date_model_end + 5)) +
      theme(panel.grid.minor = element_blank(),
            legend.position = "bottom",
            axis.text = element_text(size = 13),
            title = element_text(size = 25),
            axis.title = element_text(size = 17),
            legend.text = element_text(size = 17)) +
      guides(color=guide_legend(nrow=2,byrow=TRUE)) +
      labs(y = "Úmrtí (7 denní klouzavý průměr)", x = "", title = "Dlouhodobé scénaře denních úmrtí", 
           subtitle = "Založeno na současných datech a zvolených scénářů", col = "Scénář")
  })
  
  # observeEvent(input$refresh, {
  #   reset(CZ_expected_deaths_timeline_projected)
  # })
}


shinyApp(ui = ui, server = server)
