#######################################################################
#######################################################################
######                                                           ######
######                     Problem Set #04                       ######
######                        Yang Han                           ######
######                                                           ######
#######################################################################
#######################################################################



###Problem 1


## Part a

#  Load necessary packages first.
library(tidyverse)
library(nycflights13)

#  Get the tibble "flights" and clean the data as we only need departure and arrival
#  delays, destination and departing airport in this case.
flight_tib <- nycflights13::flights
flight_times <- flight_tib %>%
  select(dep_delay,
         arr_delay,
         origin,
         dest)
#  Rename the departing airports.
flight_times <- flight_times %>%
  mutate(origin = replace(origin, origin == "EWR", airports$name[airports$faa == "EWR"])) %>%
  mutate(origin = replace(origin, origin == "LGA", airports$name[airports$faa == "LGA"])) %>%
  mutate(origin = replace(origin, origin == "JFK", airports$name[airports$faa == "JFK"]))

#  To generate the first table, we need to aggregate the data by departing airport and 
#  then get the mean and the median.
flight_dep_delay <- flight_times %>% 
  group_by(origin) %>% 
  summarize(mean_delay_time = mean(dep_delay, na.rm = TRUE), 
            median_delay_time = median(dep_delay, na.rm = TRUE)) %>%
  ungroup()

#  Reorder the table in descending mean delay and make it nice.
flight_dep_delay <- flight_dep_delay %>%
  arrange(desc(mean_delay_time)) %>% 
  rename(departing_airport = origin)
#  Display the first table.
print(flight_dep_delay)

#  To generate the second table, we need to exclude any destination with under 10 flights
#  first and then change the codes of the destination airport to their corresponding names.
airports_name <- airports %>% 
  select(faa, 
         name)
airports_name <- airports_name %>% 
  rename(dest = faa)
flight_counts <- flight_times %>%
  group_by(dest) %>%
  summarize(count = n()) %>%
  filter(count >= 10) %>% 
  ungroup()
flight_times <- flight_times %>% 
  filter(dest %in% flight_counts$dest) %>%
  left_join(airports_name, by = "dest") 

#  Then carry out the same thing we did for the first table.
flight_arr_delay <- flight_times %>% 
  group_by(name) %>% 
  summarize(mean_delay_time = mean(arr_delay, na.rm = TRUE), 
            median_delay_time = median(arr_delay, na.rm = TRUE)) %>%
  ungroup()

#  Reorder the table in descending mean delay and make it nice.
flight_arr_delay <- flight_arr_delay %>%
  arrange(desc(mean_delay_time)) %>% 
  rename(arriving_destination = name)
flight_arr_delay <- na.omit(flight_arr_delay)

#  Display the second table.
print(flight_arr_delay, n = 98)


## Part b

#  Firstly, combine two tibbles flights and planes and get the variables of interests.
flight_plane <- flights %>% 
  left_join(planes, by = "tailnum")
flight_plane <- flight_plane %>%
  mutate(speeds = distance / (air_time / 60)) %>%
  select(model,
         speeds)

#  Then get the result tibble by reordering the data and calculate the number of flights of the fastest model.
flight_plane <- flight_plane %>% 
  group_by(model) %>%
  summarize(avg_speed = mean(speeds), 
            number_of_flights = n()) %>%
  arrange(desc(avg_speed)) %>%
  ungroup()

#  Finalize the result and display the result.
result <- flight_plane[1,]
print(result)



###Problem 2


#  Load the data.
nnmaps <- read.csv("chicago-nnmaps.csv")

#  Define the function get_temp
get_temp <- function(month, year, data, celsius = FALSE, average_fn = mean) {
  #  Sanitize the input by ensuring 'month' is a valid value (numeric or string)
  if(!month %in% month.abb & !month %in% month.name & !month %in% 1:12) {
    return("Invalid month. Please provide a valid month as a numeric 1-12 or a string.")
  }
  #  Sanitize the input by ensuring 'year' is valid
  if(!is.double(year)) {
    return("Invalid year. Please provide a reasonable integer.")
  } else if(!year %in% 1997:2000) {
    return("The provided year is not included in the nnmaps dataset")
  }
  
  #  Select the variables we need
  #  Add a new variable which is the full name of "month"
  data <- data %>%
    mutate(month_full = month.name[match(data$month, month.abb)])
  #  Manipulate the data according to the format of input "month"
  if(month %in% month.abb) {
    target_data <- data %>%
      select(temp,
             year,
             month)
  } else if(month %in% month.name) {
    target_data <- data %>%
      select(temp,
             year,
             month_full) %>%
      rename(month = month_full)
  } else {
    target_data <- data %>%
      select(temp,
             year,
             month_numeric) %>% 
      rename(month = month_numeric)
  }
  #  Then compute the mean temperature
  avg_temp <- target_data %>% 
    group_by(month, year) %>%
    summarize(average_fn = average_fn(temp)) %>%
    ungroup()
  
  #  Convert to Celsius if Celsius is TRUE
  if(celsius) {
    avg_temp <- avg_temp %>% 
      mutate(average_fn = (average_fn - 32) * (5 / 9))
  }
  
  #  Extract the numeric result as a vector of length 1
  result <- avg_temp$average_fn[avg_temp$year == year & avg_temp$month == month]
  
  return(result)
}

#  Test the examples:
get_temp("Apr", 1999, data = nnmaps)
get_temp("Apr", 1999, data = nnmaps, celsius = TRUE)
get_temp(10, 1998, data = nnmaps, average_fn = median)
get_temp(13, 1998, data = nnmaps)
get_temp(2, 2005, data = nnmaps)
get_temp("November", 1999, data =nnmaps, celsius = TRUE,
         average_fn = function(x) {
           x %>% sort -> x
           x[2:(length(x) - 1)] %>% mean %>% return
         })



###Problem 4


## Part a

#  I think this codebook was generated by a text editor or any kinds of markdown. It was generated based on 
#  a survey questionnaire.


## Part h

#  Read the data and import necessary library first.
library(survey)
shed <- read.csv("shed.csv")

#  Then re-fit the logistic model as we did in Stata.
svy_design <- svydesign(id = ~CaseID, weight = ~weight_pop, data = shed)
logistic_model <- svyglm(financial ~ nd_chance + economic + ownership + as.factor(edu) + as.factor(reth), 
                        design = svy_design, family = quasibinomial)

#  Calculate the pseudo-R^2 and display the result.
pseudo_rsq <- psrsq(logistic_model)
print(pseudo_rsq)

















###Question 3



## Part a

#  Calculate the proportion and get the state with the highest percentage of records.
  proc sql;
SELECT state_name, sum(NWEIGHT) as num, (sum(NWEIGHT) / sum(sum(NWEIGHT)) OVER()) AS percentage_records
FROM recs
GROUP BY state_name
ORDER BY percentage_records DESC
LIMIT 1;
quit;

#  Get the percentage of all records correspond to Michigan.
  proc sql;
SELECT state_name, sum(NWEIGHT) as num, (sum(NWEIGHT) / sum(sum(NWEIGHT)) OVER()) AS percentage_records
FROM recs
WHERE state_name == "Michigan"
GROUP BY state_name;
quit;


/* Q3 */
  
  /* input and output paths: ------------------------------------------------- */
  %let in_path = /home/u63642906/sasuser.v94/yhan()/data/;
%let out_path = /home/u63642906/sasuser.v94/yhan()/sas/; 
libname in_lib "&in_path."; 
libname out_lib "&out_path.";
run;

/* import delimited data with proc import: --------------------------------- */
  proc import datafile = "&in_path.recs2020_public_v5.csv" out = recs replace;
run;


/* Part a */
  
  /* After importing the data, sort the state with the highest percentage of records. */
  proc sort
data = in_lib.recs2020_public_v5
out = recs2020; 
by state_name; 
run; 

data recs2020;
set recs2020;
nweight_sq = nweight * nweight;
run; 

proc summary data = recs2020;
class state_name; 
output out = ess_by_region_recs2020
sum(NWEIGHT) = num;
run; 

data out_lib.ess_by_region_recs2020; 
set ess_by_region_recs2020;
where _type_ = 1; 
den = sum(num)
percentage_record = num / sum(num); 
drop num den _type_;
run;


/* Part b */
  
  /* Generate the histogram. */
  proc univariate data = recs2020;
var DOLLAREL;
where DOLLAREL > 0;
histogram / normal;
run;


/* Part c */
  
  /* Generate the histogram. */
data recs2020;
set recs2020;  
log_DOLLAREL = log(DOLLAREL);
run;

proc univariate data = recs2020;
var log_DOLLAREL;
histogram / normal;
run;


/* Part d */
  
  /* Fit the model and get the result. */
proc reg data = in_lib.data plots(maxpoints = none);
model log_DOLLAREL = TOTROOMS PRKGPLC1;
weight NWEIGHT;
output out = predicted_values predicted = pred_DOLLAREL;
run;













