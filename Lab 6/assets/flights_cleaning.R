## Cleaning and subsetting for the 2015 Flight Delays and Cancellations dataset from Kaggle
## https://www.kaggle.com/usdot/flight-delays

library(tidyverse)

airports = read.csv('airports.csv')
airlines = read.csv('airlines.csv')
flights_raw = read.csv('flights.csv')

## Airline codes
## ID Abv  Airline
## 10  DL  DELTA
##     AA  American Airlines
##     WN  Southwest

outbound = flights_raw[flights_raw$ORIGIN_AIRPORT=='DTW',]
inbound = flights_raw[flights_raw$DESTINATION_AIRPORT=='DTW',]

#### ================
#### DATA EXPLORATION
#### ================

## Delays

hist(outbound$DEPARTURE_DELAY[outbound$DEPARTURE_DELAY < 100 & outbound$DEPARTURE_DELAY > 0],
     main = 'Departure delays at DTW \n(truncated to (0, 100))',
     xlab = 'minutes')

hist(inbound$ARRIVAL_DELAY[inbound$ARRIVAL_DELAY < 100 & inbound$ARRIVAL_DELAY > 0],
     main = 'Arrival delays at DTW \n(truncated to (0, 100))',
     xlab = 'minutes')

hist(outbound$AIRLINE_DELAY[outbound$AIRLINE_DELAY < 200],
     main = 'Delays at DTW caused by the airline itself\n(truncated at 200)',
     xlab = 'minutes')

hist(outbound$AIRLINE_DELAY[outbound$AIRLINE_DELAY < 200
                           & outbound$AIRLINE_DELAY > 0],
     main = 'Delays at DTW caused by the airline itself\n(truncated to (0, 200))',
     xlab = 'minutes')

## Delays by airline

# par(mfcol=c(3,1))

hist(outbound$AIRLINE_DELAY[outbound$AIRLINE_DELAY < 200
                           & outbound$AIRLINE_DELAY > 0][outbound$AIRLINE == 'DL'],
     main = 'Delays on Delta flights from DTW caused by Delta\n(truncated (0, 200))',
     xlab = 'minutes')

hist(outbound$AIRLINE_DELAY[outbound$AIRLINE_DELAY < 200
                           & outbound$AIRLINE_DELAY > 0][outbound$AIRLINE == 'AA'],
     main = 'Delays on AA flights from DTW caused by AA\n(truncated (0, 200))',
     xlab = 'minutes')

hist(outbound$AIRLINE_DELAY[outbound$AIRLINE_DELAY < 200
                           & outbound$AIRLINE_DELAY > 0][outbound$AIRLINE == 'WN'],
     main = 'Delays on Southwest flights from DTW caused by SW\n(truncated (0, 200))',
     xlab = 'minutes')

outbound %>%
  group_by(AIRLINE) %>%
  summarize("Mean departure airline delay"=mean(AIRLINE_DELAY, na.rm=TRUE))

## Delays by airport

outbound[outbound$DESTINATION_AIRPORT %in% airports$IATA_CODE[airports$STATE=='TX'],] %>%
  group_by(DESTINATION_AIRPORT) %>%
  summarize("Mean departure delay"=mean(AIRLINE_DELAY, na.rm=TRUE))

outbound[outbound$DESTINATION_AIRPORT %in% airports$IATA_CODE[airports$STATE=='TX'],] %>%
  group_by(DESTINATION_AIRPORT) %>%
  summarize("Mean arrival delay"=mean(ARRIVAL_DELAY, na.rm=TRUE))

# par(mfcol=c(2,1))

hist(outbound$DEPARTURE_DELAY[outbound$DEPARTURE_DELAY < 250
                             & outbound$DEPARTURE_DELAY > 0
                             & outbound$DESTINATION_AIRPORT=='AUS'],
     main = 'Delays on Delta flights from DTW to AUS\n(truncated (0, 250))',
     xlab = 'minutes')

hist(outbound$DEPARTURE_DELAY[outbound$DEPARTURE_DELAY < 250
                              & outbound$DEPARTURE_DELAY > 0
                              & outbound$DESTINATION_AIRPORT=='IAH'],
     main = 'Delays on Delta flights from DTW to IAH\n(truncated (0, 250))',
     xlab = 'minutes')

## Flight times

hist(inbound$ELAPSED_TIME,
     main = 'Total flight times for flights arriving in DTW',
     xlab = 'minutes')

hist(outbound$ELAPSED_TIME,
     main = 'Total flight times for flights departing in DTW',
     xlab = 'minutes')

## Flight time comparison by airline

# par(mfrow=c(2,2))

hist(inbound$ELAPSED_TIME[inbound$AIRLINE=='DL'],
     main = 'Total flight times for Delta flights arriving in DTW',
     xlab = 'minutes')

hist(outbound$ELAPSED_TIME[outbound$AIRLINE=='DL'],
     main = 'Total flight times for Delta flights departing DTW',
     xlab = 'minutes')

hist(inbound$ELAPSED_TIME[inbound$AIRLINE=='AA'],
     main = 'Total flight times for AA flights arriving in DTW',
     xlab = 'minutes')

hist(outbound$ELAPSED_TIME[outbound$AIRLINE=='AA'],
     main = 'Total flight times for AA flights departing DTW',
     xlab = 'minutes')

par(mfrow=c(1,1))

#### DECISION FOR FINAL COMPARISON: Delta vs AA outbound ARILINE_DELAY

#### ==================================
#### DATA CLEANING+TRIMMING BEGINS HERE
#### ==================================

## Parameters for sampling
set.seed(1028202)
sample_fraction = 0.38    # 0.38 gives two-sided p-val 0.08587

## Select only Delta or AA flights, originating in DTW,
##   with a delay caused by airline
## Replace NA values with zeroes in other delay variables
## Sample a fraction of this data

delayed_flights = 
  outbound %>%
  filter(CANCELLED==0 & DIVERTED==0) %>%
  select(FLIGHT_NUMBER,
         YEAR,
         MONTH,
         DAY,
         DAY_OF_WEEK,
         AIRLINE,
         DESTINATION_AIRPORT,
         LATE_AIRCRAFT_DELAY,
         AIRLINE_DELAY,
         WEATHER_DELAY) %>%
  filter(AIRLINE_DELAY > 0 & AIRLINE_DELAY < 200) %>%
  filter(AIRLINE %in% c('DL', 'AA')) %>%
  mutate_at(vars(LATE_AIRCRAFT_DELAY, WEATHER_DELAY),
            ~ ifelse(is.na(.), 0, .)) %>%
  slice_sample(prop = sample_fraction)

## Calculate statistics by airline
flight_stats = 
  delayed_flights %>%
  group_by(AIRLINE) %>%
  summarize(n = length(AIRLINE),
            mean_delay = mean(AIRLINE_DELAY),
            s_delay = sd(AIRLINE_DELAY))

## Perform test
t.test(x = delayed_flights %>% filter(AIRLINE=='AA') %>% select(AIRLINE_DELAY),
       y = delayed_flights %>% filter(AIRLINE=='DL') %>% select(AIRLINE_DELAY),
       conf.level = 0.9)

## Write data to csv
write.csv(delayed_flights, file="delayed_flights.csv", row.names=FALSE)
