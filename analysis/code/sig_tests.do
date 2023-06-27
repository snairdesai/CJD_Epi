* Construct t-tests for sig. differences in mortality rates

* Note: t-test for "rates" = (rate1-rate2)/sqrt(SE1^2+SE2^2).

************
** GENDER **
************

* import data for female/male age-adjusted and crude mortality rates
import delimited using "/Users/sameernair-desai/Desktop/IndRes/CJD_Epi/data/output/cleaned_nmr_gender.csv", clear

* Clean data
drop if gender == "Both"

keep time_period gender raw_death_count raw_death_rate raw_death_rate_se age_adj_death_rate age_adj_death_rate_se

* Keep first and last year 
keep if time_period == "2007" | time_period == "2020" 

* Reshape to perform t-test
reshape wide raw_death_* age_adj_*, i(gender) j(time_period) string

* First, crude rate by gender:
gen t_test_raw = (raw_death_rate2020 - raw_death_rate2007)/sqrt(raw_death_rate_se2020^2 + raw_death_rate_se2007^2)

* Next, age-adjusted rate by gender:
gen t_test_ageadj = (age_adj_death_rate2020 - age_adj_death_rate2007)/sqrt(age_adj_death_rate_se2020^2 + age_adj_death_rate_se2007^2)

export delimited using "/Users/sameernair-desai/Desktop/IndRes/CJD_Epi/analysis/output/t_tests_gender.csv", replace

****************
** AGE GROUPS **
*****************

import excel using "/Users/sameernair-desai/Desktop/IndRes/CJD_Epi/raw/Raw and Age-Specific National Mortality Rates by Gender (2007 - 2020).xlsx", firstrow clear

* Clean data
drop Notes YearCode TenYearAgeGroups Deaths *95Con* ofTotalDeaths Population
drop if Year == .

replace CrudeRate = "" if CrudeRate == "Unreliable"
destring CrudeRate, replace

* Keep first and last year 
keep if Year == 2007 | Year == 2020

drop if TenYearAgeGroupsCode == "All years" | TenYearAgeGroupsCode == "25-34" | /// 
	TenYearAgeGroupsCode == "35-44" | TenYearAgeGroupsCode == "45-54" | ///
	TenYearAgeGroupsCode == "85+" | TenYearAgeGroupsCode == ""
	
replace TenYearAgeGroupsCode = "75+" if TenYearAgeGroupsCode == "75-84"

reshape wide Crude*, i(TenYearAgeGroupsCode) j(Year)

* Rates by age bins:
gen t_test_agegroup = (CrudeRate2020 - CrudeRate2007)/sqrt(CrudeRateStandardError2020^2 + CrudeRateStandardError2007^2)

export delimited using "/Users/sameernair-desai/Desktop/IndRes/CJD_Epi/analysis/output/t_tests_agegroup.csv", replace

***************************
** GENDER AND AGE GROUPS **
***************************

import delimited using "/Users/sameernair-desai/Desktop/IndRes/CJD_Epi/data/output/cleaned_nmr_gender_specific.csv", varnames(1) clear

* Clean data
keep time_period gender age_groups age_specific_death_rate age_specific_death_rate_se

replace age_specific_death_rate = "" if age_specific_death_rate == "NA"
destring age_specific_death_rate, replace

drop if gender == "Both"

* Keep first and last year 
keep if time_period == "2007" | time_period == "2020"

drop if age_groups == "All years" | age_groups == "25-34 years" | /// 
	age_groups == "35-44 years" | age_groups == "45-54 years" | ///
	age_groups == "85+ years" | age_groups == ""
	
replace age_groups = "75+ years" if age_groups == "75-84 years"
replace gender = "f" if gender == "Female"
replace gender = "m" if gender == "Male"

reshape wide age_specific_death*, i(age_groups gender) j(time_period) string

* Rates by age bins (and gender): 

gen t_test_agegroup_gender = (age_specific_death_rate2020 - age_specific_death_rate2007)/sqrt(age_specific_death_rate_se2020^2 + age_specific_death_rate_se2007^2) if gender == "f"

replace t_test_agegroup_gender = (age_specific_death_rate2020 - age_specific_death_rate2007)/sqrt(age_specific_death_rate_se2020^2 + age_specific_death_rate_se2007^2) if gender == "m"

export delimited using "/Users/sameernair-desai/Desktop/IndRes/CJD_Epi/analysis/output/t_tests_agegroup_gender.csv", replace



