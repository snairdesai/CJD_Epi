* Construct t-tests for sig. differences in mortality rates

* Note: t-test for "rates" = (rate1-rate2)/sqrt(SE1^2+SE2^2).

global RAW "/Users/sameernd/Desktop/CJD_Epi/raw"
global DATA "/Users/sameernd/Desktop/CJD_Epi/data/output"
global OUTPUT "/Users/sameernd/Desktop/CJD_Epi/analysis/output"

************
** GENDER **
************

* import data for female/male age-adjusted and crude mortality rates
import delimited using "$DATA/cleaned_nmr_gender.csv", clear

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

export delimited using "$OUTPUT/t_tests_gender.csv", replace

****************
** AGE GROUPS **
*****************

import excel using "$RAW/Raw and Age-Specific National Mortality Rates by Age Groups (2007 - 2020).xlsx", firstrow clear

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

export delimited using "$OUTPUT/t_tests_agegroup.csv", replace

