library(tidyverse)

url = paste('https://github.com/vera-institute/incarceration_trends/',
            'blob/master/incarceration_trends.csv?raw=true', sep = '')
incarceration <- read.csv(url)

total_black_pop <- incarceration %>%
  filter(year == 2016) %>%
  pull(black_pop_15to64) %>%
  sum(na.rm = TRUE)

total_pop <- incarceration %>%
  filter(year == 2016) %>%
  pull(total_pop_15to64) %>%
  sum(na.rm = TRUE)

percent_black_pop <- round(total_black_pop / total_pop * 100, 1)

black_prison_pop <- incarceration %>%
  filter(year == 2016) %>%
  pull(black_prison_pop) %>%
  sum(na.rm = TRUE)

total_prison_pop <- incarceration %>%
  filter(year == 2016) %>%
  pull(total_prison_pop) %>%
  sum(na.rm = TRUE)

percent_black_prison_pop <- round(black_prison_pop / total_prison_pop * 100, 1)

total_latinx_pop <- incarceration %>%
  filter(year == 2016) %>%
  pull(latinx_pop_15to64) %>%
  sum(na.rm = TRUE)

percent_latinx_pop <- round(total_latinx_pop / total_pop * 100, 1)

latinx_prison_pop <- incarceration %>%
  filter(year == 2016) %>%
  pull(latinx_prison_pop) %>%
  sum(na.rm = TRUE)

percent_latinx_prison_pop <- round(latinx_prison_pop / 
                                     total_prison_pop * 100, 1)

# data for time plot
time_chart_data <- incarceration %>%
  # remove na rows of columns being used
  drop_na(aapi_prison_pop, black_prison_pop, latinx_prison_pop, 
          native_prison_pop, white_prison_pop) %>%
  group_by(year) %>%
  # find total population by race for each year
  summarize(aapi = sum(aapi_prison_pop), black = sum(black_prison_pop),
            latinx = sum(latinx_prison_pop), native = sum(native_prison_pop),
            white = sum(white_prison_pop)) %>%
  # change format to 'long' for plotting
  pivot_longer(!year, 'race', values_to = 'prison_population')

time_plot <- ggplot(time_chart_data, mapping = aes(x = year, 
                                                   y = prison_population, 
                                                   color = race)) +
  geom_line() +
  ggtitle('Total Prison Population Over Time') +
  xlab('Year') + 
  ylab('Prison Population') +
  scale_color_discrete(name = 'Race', 
                       labels = c('Asian American/Pacific Islander', 'Black', 
                                  'Latinx', 'Native', 'White'))

# data for comparison chart
comp_chart_data <- incarceration %>%
  filter(year == 2016) %>%
  # remove na rows of columns being used
  drop_na(black_prison_pop, black_pop_15to64) %>%
  # calculate percents
  mutate(percent_black_population_imprisoned = 
           black_prison_pop / black_pop_15to64 * 100,
         percent_black_population = 
           black_pop_15to64 / total_pop_15to64 * 100) %>%
  select(year, county_name, percent_black_population_imprisoned, 
         percent_black_population)

max_county_black_prison_percent <- 
  max(comp_chart_data$percent_black_population_imprisoned)

county_max_black_prison_percent <- comp_chart_data %>%
  filter(percent_black_population_imprisoned == 
           max_county_black_prison_percent) %>%
  pull(county_name)

max_county_black_prison_percent <- round(max_county_black_prison_percent, 1)

comp_plot <- ggplot(comp_chart_data, 
                    mapping = aes(x = percent_black_population, 
                                  y = percent_black_population_imprisoned)) +
  geom_point() +
  # add line of best fit
  geom_smooth(se = FALSE, color = 'red') +
  # change to log scales
  scale_x_log10(name = 'Percent Black Population') + 
  scale_y_log10(name = 'Percent Black Population Imprisoned') +
  ggtitle(paste('Percent Black Population vs. Percent Black',
                'Population Imprisoned Per County'))

# data for map
map_data <- incarceration %>%
  filter(year == 2016) %>%
  group_by(state) %>%
  # calculate percents
  summarize(percent_black_population_imprisoned = 
              sum(black_prison_pop, na.rm = TRUE) / 
              sum(black_pop_15to64, na.rm = TRUE) * 100)

# create a table of state abbreviation and names for joining later
state_abbs <- data.frame(state = state.abb, region = str_to_lower(state.name))

state_shape <- map_data('state') %>%
  left_join(state_abbs, by = 'region') %>%
  left_join(map_data, by = 'state')

map <- ggplot(state_shape) +
  # plot the state shapes, filling by rate
  geom_polygon(mapping = aes(x = long, y = lat, group = group, 
                             fill = percent_black_population_imprisoned),
               color = 'white', size = 0.1) +
  # sets map based aspect resolution
  coord_map() + 
  # sets color scale
  scale_fill_continuous(low = "#132B43", high = "Red") +
  labs(title = 'Percent Black Population Imprisoned in 2016', 
       fill = 'Percent of Black Population \n (age 15-64) Imprisoned') +
  # sets minimal theme
  theme_minimal() +
  # removes axis labels and grid lines
  theme(axis.line = element_blank(), axis.text = element_blank(), 
        axis.title = element_blank(), panel.grid.major = element_blank())
