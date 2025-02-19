# Maya McCain
# February 18, 2025
# Daily Exercise 7

library(tidyverse)
url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv'
covid = read_csv(url)
head(covid, 5)

#Question 1
#Identify the six states with the most current cases (yesterdays assignment + dplyr::pull) 
top_states <- covid |> 
  filter(date == max(date)) |>
  group_by(state) |> 
  summarize(totalCases = sum(cases)) |> 
  arrange(-totalCases) |>
  slice(1:6) |>
  pull(state)

#Filter the raw data to those 6 states (hint: %in%)
covid_filtered <- covid |> 
  filter(state %in% top_states) |> 
  group_by(date, state) |> 
  summarize(totalCases = sum(cases))

#Set up a ggplot –> add layers –> add labels –> add a facet –> add a theme
plot <- ggplot(data=covid_filtered, 
               aes(x = date, y = totalCases, color = state)) +
  geom_line() +
  labs(title = "States with the Most Current COVID Cases",
       x = "Date",
       y = "Cases",
       color = "State",
       size = "Cases") +
  facet_wrap(~state, scales = "free_y") +
  theme_minimal()

#save the image to you img directory (hint: ggsave())


#Question 2
covid_national <- covid |> 
  group_by(date) |>  
  summarise(daily_cases = sum(cases))

plot2 <- ggplot(covid_national, aes(x = date, y = daily_cases)) +
  geom_col(fill = "steelblue") +  
  labs(title = "Daily COVID-19 Cases in the USA",
       x = "Date",
       y = "Number of Cases") +  
  theme_minimal() 

ggsave(plot2, filename = "img/nation.png")
