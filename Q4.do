*  Import the data into stata first.
import sas "shed"


** Part d

*  Check the data is successfully extracted.
describe

*  From the result we can see that there are 11667 observations and 8 variables,
*  Which is desired.


** Part e

*  Convert the response value to binary values with Same/Better = 0 and Worse off = 1.
generate financial = 0 if fin_condition == "About the same"
replace financial = 0 if missing(financial) & fin_condition == "Somewhat better off"
replace financial = 0 if missing(financial) & fin_condition == "Much better off"
replace financial = 1 if missing(financial) & fin_condition == "Much worse off"
replace financial = 1 if missing(financial) & fin_condition == "Somewhat worse off"


** Part f

*  Before fitting the logistic model, we need to transform the variables into numeric.
generate nd_chance = 1 if ND_chance == "Much higher"
replace nd_chance = 2 if missing(nd_chance) & ND_chance == "Somewhat higher"
replace nd_chance = 3 if missing(nd_chance) & ND_chance == "About the same"
replace nd_chance = 4 if missing(nd_chance) & ND_chance == "Somewhat lower"
replace nd_chance = 5 if missing(nd_chance) & ND_chance == "Much lower"

generate economic = 1 if econ_condition == "Poor"
replace economic = 2 if missing(economic) & econ_condition == "Only fair"
replace economic = 3 if missing(economic) & econ_condition == "Good"
replace economic = 4 if missing(economic) & econ_condition == "Excellent"

generate ownership = 1 if ownership_house == "Owned or being bought by you or someone in your household"
replace ownership = 2 if missing(ownership) & ownership_house == "Rented for cash"
replace ownership = 3 if missing(ownership) & ownership_house == "Occupied without payment of cash rent"

generate edu = 1 if education == "No high school diploma or GED"
replace edu = 2 if missing(edu) & education == "High school graduate (high school diploma or the equivalent GED)"
replace edu = 3 if missing(edu) & education == "Some college or Associate's degree"
replace edu = 4 if missing(edu) & education == "Bachelor's degree or higher"

generate reth = 1 if race == "White"
replace reth = 2 if race == "Black"
replace reth = 3 if race == "Hispanic"
replace reth = 4 if race == "Asian"
replace reth = 5 if race == "Other"

*  Use the given code and fit the logisitic model

svyset CaseID [pw = weight_pop]
svy: logistic financial nd_chance economic ownership i.edu i.reth

*  From the result we can see that the p-value for nd_chance is 0.336, which it is statistically insignificant.
*  In this case we should not consider people thinking that the chance of experiencing a natural disaster or severe  *  weather event will be higher, lower or about the same in 5 years as an influential predictor for the model. Hence, I
*  do not believe that long-term concerns about climate change impact current day concerns about financial stability.
*  Interestingly, How people rate the economic conditions today in the country is a good predictor of the model. 
*  An increase in economic significantly decreases the odds of being in the "Same/Better" category (p-value < 0.0001).
*  In this case, I would say that as an individual being more optimistic of the country's economic conditions, one's 
*  financial stability would have a lower likelihood of being "Worse off".


*  Part g

*  Before getting the data out of Stata, we need to clean the data first(to delete the variables we don't want).
drop fin_condition ND_chance econ_condition ownership_house education race

*  Then export the data and import it into R.
export delimited shed

















