* Construct t-tests for sig. differences in mortality rates

****************
** AGE GROUPS **
*****************

import excel using input/raw_nmr_gender.xlsx, firstrow clear

* Clean data
drop Notes YearCode Deaths *95Con* ofTotalDeaths
drop if Year == .

* Converting standard errors to standard deviations.
gen CrudeRateSD = CrudeRateStandardError * sqrt(Population)

replace CrudeRate = "" if CrudeRate == "Unreliable"
destring CrudeRate, replace

* Keep first and last year 
keep if Year == 2007 | Year == 2020

drop if TenYearAgeGroupsCode == "All years" | TenYearAgeGroupsCode == "25-34" | /// 
	TenYearAgeGroupsCode == "35-44" | TenYearAgeGroupsCode == "45-54" | ///
	TenYearAgeGroupsCode == "85+" | TenYearAgeGroupsCode == ""
	
replace TenYearAgeGroupsCode = "75+" if TenYearAgeGroupsCode == "75-84"

gen GenderAge = TenYearAgeGroups + ": " + GenderCode
drop Gender GenderCode TenYearAgeGroups TenYearAgeGroupsCode

reshape wide Crude* Population, i(GenderAge) j(Year)

* Rates by age bins:

gen t_test_agegroup = (CrudeRate2020 - CrudeRate2007) /  sqrt((CrudeRateSD2020^2 / Population2020) + (CrudeRateSD2007^2 / Population2007))

export delimited using output/t_tests_agegroup.csv, replace

************
** GENDER **
************

* import data for female/male age-adjusted and crude mortality rates
import delimited using input/cleaned_nmr_gender.csv, clear

* Clean data
drop if gender == "Both"

* Converting sample size to numeric.
gen population_numeric = real(subinstr(us_time_period_pop, ",", "", .))
format population_numeric %12.0f

* Converting standard errors to standard deviations.
gen raw_death_rate_sd = raw_death_rate_se * sqrt(population_numeric)
gen age_adj_death_rate_sd = age_adj_death_rate_se * sqrt(population_numeric)

keep time_period gender raw_death_count raw_death_rate raw_death_rate_sd age_adj_death_rate age_adj_death_rate_sd population_numeric

* Keep first and last year 
keep if time_period == "2007" | time_period == "2020" 

* Reshape to perform t-test
reshape wide raw_death_* age_adj_* pop*, i(gender) j(time_period) string

* Welch's T-Test for crude rates by gender:

gen t_test_raw = (raw_death_rate2020 - raw_death_rate2007) / sqrt((raw_death_rate_sd2020^2 / population_numeric2020) + (raw_death_rate_sd2007^2 / population_numeric2007))

* Next, age-adjusted rates by gender:

gen t_test_ageadj = (age_adj_death_rate2020 - age_adj_death_rate2007) / sqrt((age_adj_death_rate_sd2020^2 / population_numeric2020) + (age_adj_death_rate_sd2007^2 / population_numeric2007))

export delimited using output/t_tests_gender.csv, replace

***************************
** GENDER AND AGE GROUPS **
***************************

import delimited using input/cleaned_nmr_gender_specific.csv, varnames(1) clear

* Clean data
keep time_period gender age_groups age_specific_death_rate age_specific_death_rate_se us_time_period_pop

* Converting sample size to numeric.
gen population_numeric = real(subinstr(us_time_period_pop, ",", "", .))
format population_numeric %12.0f

* Converting standard errors to standard deviations.
gen age_specific_death_rate_sd = age_specific_death_rate_se * sqrt(population_numeric)

replace age_specific_death_rate = "" if age_specific_death_rate == "NA"
destring age_specific_death_rate, replace

drop if gender == "Both"

* Keep first and last year 
keep if time_period == "2007" | time_period == "2020"

drop if age_groups == "All years" | age_groups == "25-34 years" | /// 
	age_groups == "35-44 years" | age_groups == "45-54 years" | ///
	age_groups == "85+ years" | age_groups == ""
	
replace age_groups = "75+ years" if age_groups == "75-84 years"

gen GenderAge = gender + ": " + age_groups
drop gender age_groups us_time_period_pop

reshape wide age_specific_death* pop*, i(GenderAge) j(time_period) string

* Rates by age bins (and gender): 

gen t_test_gender_age = (age_specific_death_rate2020 - age_specific_death_rate2007)/sqrt(age_specific_death_rate_se2020^2 + age_specific_death_rate_se2007^2)

export delimited using output/t_tests_gender_age.csv, replace



