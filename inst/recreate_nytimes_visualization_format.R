
# recreating the NYTimes visualization map-triangles format in ggplot2

# dependencies
library(ggtriangles)
library(urbnmapr)
library(magrittr)
library(here)


# download county and state maps
counties <- get_urbn_map('counties', sf=T)
states <- get_urbn_map('states', sf=T)


# these are a darkgreen and lightgreen that roughly match the nytimes
# visualization
darkgreen <- colorspace::lighten('#4e7c71')
lightgreen <- colorspace::lighten("#bacac9", amount = 0.5)

# download the NYTimes data from their github
#
nyt2020 <- readr::read_csv("https://github.com/nytimes/covid-19-data/raw/master/rolling-averages/us-counties-2020.csv")
nyt2021 <- readr::read_csv("https://github.com/nytimes/covid-19-data/raw/master/rolling-averages/us-counties-2021.csv")


# calculate the 2020-average (after March 1) of the 7-day rolling average death rate
nyt2020avg <- nyt2020 %>% group_by(geoid, county, state) %>%
  filter(date > lubridate::ymd("2020-03-01")) %>%
  # note that we're filtering for data after march 1st from 2020 so that we're
  # comparing rates during COVID-2020 to rates from COVID-2021 (== all of 2021)
  summarize(avg_death_rate = mean(deaths_avg_per_100k, na.rm=T))
# these data which are being summarized, it's important to note, are
# rolling 7-day averages which omit anomalous days.


# calculate the 2021-average of the 7-day rolling average death rate
nyt2021avg <- nyt2021 %>% group_by(geoid, county, state) %>%
  summarize(avg_death_rate = mean(deaths_avg_per_100k, na.rm=T))

# rename appropriately
nyt2020avg %<>% rename(avg2020 = avg_death_rate)
nyt2021avg %<>% rename(avg2021 = avg_death_rate)

# merge data-years
avg_death_rates <- nyt2020avg %<>% left_join(nyt2021avg)

# compute the difference in average death rates
avg_death_rates %<>% mutate(change = avg2021 - avg2020)

# extract fips codes for matching on
avg_death_rates %<>% mutate(fips = stringr::str_extract(geoid, "[0-9]+"))

# use urbnmapr to get spatial map of counties
counties <- urbnmapr::get_urbn_map('counties', sf = TRUE)

# join in death rates with change (from 2020 to 2021 column)
counties %<>% left_join(avg_death_rates %>% select(fips, change), by = c('county_fips' = 'fips'))

# specify state abbreviations dataframe
state_abbreviations <-
  tibble::tribble(
    ~state_name, ~state_abbv, ~state_abbv_med,
    "Alabama",                 "AL",                 "Ala.",
    "Alaska",                 "AK",               "Alaska",
    "Arizona",                 "AZ",                "Ariz.",
    "Arkansas",                 "AR",                 "Ark.",
    "California",                 "CA",               "Calif.",
    "Colorado",                 "CO",               "Color.",
    "Connecticut",                 "CT",                "Conn.",
    "Delaware",                 "DE",                 "Del.",
    "Florida",                 "FL",                 "Fla.",
    "Georgia",                 "GA",                  "Ga.",
    "Hawaii",                 "HI",               "Hawaii",
    "Idaho",                 "ID",                "Idaho",
    "Illinois",                 "IL",                 "Ill.",
    "Indiana",                 "IN",                 "Ind.",
    "Iowa",                 "IA",                 "Iowa",
    "Kansas",                 "KS",                 "Kan.",
    "Kentucky",                 "KY",                  "Ky.",
    "Louisiana",                 "LA",                  "La.",
    "Maine",                 "ME",                "Maine",
    "Maryland",                 "MD",                  "Md.",
    "Massachusetts",                 "MA",                "Mass.",
    "Michigan",                 "MI",                "Mich.",
    "Minnesota",                 "MN",                "Minn.",
    "Mississippi",                 "MS",                "Miss.",
    "Missouri",                 "MO",                  "Mo.",
    "Montana",                 "MT",                "Mont.",
    "Nebraska",                 "NE",                 "Neb.",
    "Nevada",                 "NV",                 "Nev.",
    "New Hampshire",                 "NH",                 "N.H.",
    "New Jersey",                 "NJ",                 "N.J.",
    "New Mexico",                 "NM",                 "N.M.",
    "New York",                 "NY",                 "N.Y.",
    "North Carolina",                 "NC",                 "N.C.",
    "North Dakota",                 "ND",                 "N.D.",
    "Ohio",                 "OH",                 "Ohio",
    "Oklahoma",                 "OK",                "Okla.",
    "Oregon",                 "OR",                 "Ore.",
    "Pennsylvania",                 "PA",                  "Pa.",
    "Rhode Island",                 "RI",                 "R.I.",
    "South Carolina",                 "SC",                 "S.C.",
    "South Dakota",                 "SD",               "S.Dak.",
    "Tennessee",                 "TN",                "Tenn.",
    "Texas",                 "TX",                 "Tex.",
    "Utah",                 "UT",                 "Utah",
    "Vermont",                 "VT",                 "V.T.",
    "Virginia",                 "VA",                  "Va.",
    "Washington",                 "WA",                "Wash.",
    "West Virginia",                 "WV",                "W.Va.",
    "Wisconsin",                 "WI",                 "Wis.",
    "Wyoming",                 "WY",                 "Wyo."
  )


# calculate state centers using st_centroid so we can label them
state_centers <- sf::st_centroid(states)
state_centers_xy <- sf::st_coordinates(state_centers)
state_centers %<>% bind_cols(state_centers_xy %>% as.data.frame())
state_centers %<>% left_join(state_abbreviations)

# specify common scaling factors
height_scale <- 1.5e-2
width_scale <- 1.5e-3

ggplot() +
  geom_sf(data = counties,
          mapping = aes(
            fill = factor(change > 0),
            size = factor(change > 0) %>% forcats::fct_explicit_na(na_level = "FALSE"),
            color = factor(change > 0)
          )) +
  geom_sf(data = states, aes(), size = 0.5, color = 'black', fill = NA) +
  geom_triangles(
    data =
      bind_cols(
        counties %>% filter(change > 0),
        counties %>% filter(change > 0) %>%
          sf::st_centroid() %>%
          sf::st_coordinates(.) %>%
          as.data.frame()),
    mapping = aes(x = X, y = Y, z = change),
    width = width_scale, height_scale = height_scale,
    fill = '#c0392b', color = '#c0392b') +
  geom_text(
    data = state_centers,
    mapping = aes(x = X, y = Y, label = state_abbv_med),
    hjust = 0.5, vjust = 0, check_overlap = F, size = 2.5
  ) +
  scale_fill_manual(values = c(`TRUE` = 'lightpink', `FALSE` = 'white')) +
  scale_size_manual(values = c(`TRUE` = .05, `FALSE` = 0)) +
  scale_color_manual(values = c(`TRUE` = "#c0392b", `FALSE` = 'white')) +
  ggtitle("Counties where the average of 2021 weekly rolling average death rates were\ngreater than the average of 2020 weekly rolling average death rates beginning in March") +
  theme_void() +
  theme(legend.position = 'none', plot.title = element_text(hjust = 0.5, face = 'bold'))


ggsave(here("img/nyt_increased_counties.png"), width = 10, height = 8)
ggsave(here("img/nyt_increased_counties.jpeg"), width = 10, height = 8)

upper_threshold = -lower_threshold

ggplot() +
  geom_sf(data = counties,
          mapping = aes(
            fill = factor(change < 0),
            size = factor(change < 0) %>% forcats::fct_explicit_na(na_level = "FALSE"),
            color = factor(change < 0)
          )) +
  geom_sf(data = states, aes(), size = 0.5, color = 'black', fill = NA) +
  geom_triangles(
    data =
      bind_cols(
        counties %>% filter(change < 0),
        counties %>% filter(change < 0) %>%
          sf::st_centroid() %>%
          sf::st_coordinates(.) %>%
          as.data.frame()),
    mapping = aes(x = X, y = Y, z = change),
    width = width_scale, height_scale = height_scale,
    fill = darkgreen, color = darkgreen) +
  geom_text(
    data = state_centers,
    mapping = aes(x = X, y = Y, label = state_abbv_med),
    hjust = 0.5, vjust = 0, size = 2.5
  ) +
  scale_fill_manual(values = c(`TRUE` = lightgreen, `FALSE` = 'white')) +
  scale_size_manual(values = c(`TRUE` = .05, `FALSE` = 0)) +
  scale_color_manual(values = c(`TRUE` = darkgreen, `FALSE` = 'white')) +
  ggtitle("Counties where the average of 2021 weekly rolling average death rates was\nless than than the average of 2020 weekly rolling average death rates beginning in March") +
  theme_void() +
  theme(legend.position = 'none', plot.title = element_text(hjust = 0.5, face = 'bold'))

ggsave(here("img/nyt_decreased_counties.png"), width = 10, height = 8)
ggsave(here("img/nyt_decreased_counties.jpeg"), width = 10, height = 8)
