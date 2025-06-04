*************************************************************************************************
*Name: Isaac Asaamah-Kwesi
*Program: MS Economics
*Student ID: 80809135
*Project Topic: THE PARADOX OF ECONOMIC FREEDOM AND INCOME INEQUALITY: Investigating the Effect of Economic Freedom on Income Inequality at the Country-Level using Fixed Effect
*************************************************************************************************



* Clear the workspace
clear all
set more off


** Note: I cleaned the downloaded csv data files and merged them as one "Merged_P_Data.xlsx" using my Excel technique before importing it to Stata** 

import excel "/Users/user/Desktop/aki/Merged_P_Data.xlsx", sheet("Sheet1") firstrow clear

*----------------------------------------------------------------------------------------------------------------------
//Extra Data Cleaning Using Stata
*---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


* Convert variable names to lowercase for consistency
rename *, lower
replace series = lower(series)

replace series = "efi_index" if series == " economic freedom index"
replace series = "freetrade" if series == "freedom_to_trade"
replace series = "lpright" if series == "legal_system_property_rights"
replace series = "sgovt" if series == "size_of_government"
replace series = "regulation" if series == "regulation_of_credit_labor_business"
replace series = "smoney" if series == "sound_money"

* Reshape from wide to long format using year
reshape long yr, i(c_id series) j(year)
rename yr value

*Arrangement
move countryname series
move countrycode series
move s_id year




**Creating Individual dataset to merge**


preserve
keep if series == "efi_index"
rename value efi_index
drop series
save efi_index.dta, replace
restore

preserve
keep if series=="sgovt"
rename value sgovt
drop series
save sgovt
restore

preserve
keep if series=="lpright"
rename value lpright
drop series
save lpright
restore

preserve
keep if series=="smoney"
rename value smoney
drop series
save smoney
restore

preserve
keep if series=="freetrade"
rename value freetrade
drop series
save freetrade
restore

preserve
keep if series=="regulation"
rename value regulation
drop series
save regulation
restore

preserve
keep if series=="gddp"
rename value gddp
drop series
save gddp, replace 
restore

preserve
keep if series=="edu_attainment"
rename value edu_attainment
drop series
save edu_attainment.dta, replace
restore

preserve
keep if series=="corruption"
rename value corruption
drop series
save corruption
restore

preserve
keep if series=="pop_growth"
rename value pop_growth
drop series
save pop_growth.dta, replace
restore

preserve
keep if series=="unemprate"
rename value unemprate
drop series
save unemprate
restore

preserve
keep if series=="gini_index"
rename value gini_index
drop series
save gini_index.dta, replace
restore



**Merging my datasets**

use efi_index.dta, clear

merge 1:1 c_id year using "gddp.dta"
drop _merge

merge 1:1 c_id year using "sgovt.dta"
drop _merge

merge 1:1 c_id year using "lpright.dta"
drop _merge

merge 1:1 c_id year using "smoney.dta"
drop _merge

merge 1:1 c_id year using "freetrade.dta"
drop _merge

merge 1:1 c_id year using "regulation.dta"
drop _merge

merge 1:1 c_id year using "pop_growth.dta"
drop _merge

merge 1:1 c_id year using "unemprate.dta"
drop _merge

merge 1:1 c_id year using "corruption.dta"
drop _merge

merge 1:1 c_id year using "edu_attainment.dta"
drop _merge

merge 1:1 c_id year using "gini_index.dta"
drop _merge

save "project.dta", replace





*----------------------------------------------------------------------------------------------------------------------
*----------------------------------------------------------------------------------------------------------------------

use "project.dta", clear


//set the environment for panel analysis
xtset c_id year
ssc install xttest3
xtdescribe



*Check for missing values
 misstable summarize


* Interpolating Variable that have missing data to ensure smooth trend
 

bysort countrycode (year): ipolate gini_index year, gen(gini_index_imp) epolate
rename gini_index_imp gini_index1
 
 
bysort countrycode (year): ipolate edu_attainment year, gen(edu_attainment_imp) epolate
rename edu_attainment_imp edu_attainment1 





*Generate the square of Economic Freedom and other control variables due to the Non-linearity relation by Kuznet
gen efi_index_sq = efi_index^2
gen lngddp= log(gddp)
*----------------------------------------------------------------------------------------------------------------------
*----------------------------------------------------------------------------------------------------------------------
// Descriptive Statistics

* Classification of countries base on their GDP per Capita
gen income_group = .
replace income_group = 1 if gddp <= 1135  // Low-income countries
replace income_group = 2 if gddp > 1135 & gddp <= 4465  // Lower-middle-income
replace income_group = 3 if gddp > 4465 & gddp <= 13845  // Upper-middle-income
replace income_group = 4 if gddp > 13845  // High-income

label define income_lbl 1 "Low-income" 2 "Lower-middle-income" 3 "Upper-middle-income" 4 "High-income"
label values income_group income_lbl



sum gini_index1 efi_index efi_index_sq lngddp unemprate corruption edu_attainment1 pop_growth sgovt lpright smoney freetrade regulation


corr gini_index1 efi_index efi_index_sq lngddp unemprate corruption edu_attainment1 pop_growth sgovt lpright smoney freetrade regulation



*-----------------------------------------------------------------------------------------------------------------------
*-----------------------------------------------------------------------------------------------------------------------


//Diagnostic Checks

***********************************************************************************************************************
*** 3. Fixed Effects (FE) vs. Random Effects (RE) Models ***
***********************************************************************************************************************

xtset c_id year

* Fixed-Effects Model (Carter, 2006 approach)
xtreg gini_index1 efi_index efi_index_sq lngddp unemprate corruption edu_attainment1 pop_growth, fe 
estimates store fe_model

* Random-Effects Model
xtreg gini_index1 efi_index efi_index_sq lngddp unemprate corruption edu_attainment1 pop_growth, re
estimates store re_model

* Hausman Test (FE vs RE)
hausman fe_model re_model, sigmaless

// Heteroskadasticity check
xtreg gini_index1 efi_index efi_index_sq lngddp unemprate corruption edu_attainment1 pop_growth, fe

xttest3



*----------------------------------------------------------------------------------------------------------------------

*Visualizing the U-Shape relationship between Economic Freedom and Income Inequality 
****************************************************************************************
xtreg gini_index1 efi_index efi_index_sq lngddp unemprate edu_attainment1 corruption pop_growth i.year, fe vce(cluster c_id)


* Generate predicted Gini values
predict gini_pred, xb

*Create the Scatter Plot with Quadratic Fit
twoway (scatter gini_pred efi_index, mcolor(blue) msize(small)) ///
       (qfit gini_pred efi_index, lcolor(red) lwidth(medium)), ///
       title("Economic Freedom vs. Income Inequality") ///
       xtitle("Economic Freedom Index") ///
       ytitle("Gini Index (Income Inequality)") ///
       legend(label(1 "Observed Data") label(2 "Quadratic Fit"))
	   
graph export "economic_freedom_vs_inequality.png", replace





*******************************************************************************************************************************************************
*REGRESSION ANALYSIS: POOLED OLS AND FIXED EFFECT
********************************************************************************************************************************************************

* Clear previous estimates
ssc install estout
eststo clear 

* Column (1): OLS Pooled
reg gini_index1 efi_index efi_index_sq lngddp unemprate corruption edu_attainment1 pop_growth, robust
eststo m1
estadd local yearfe "No"

* Column (2): Fixed Effect
xtreg gini_index1 efi_index lngddp unemprate corruption  edu_attainment1 pop_growth i.year, fe vce(cluster c_id)
eststo m2
estadd local yearfe "Yes"

* Column 3: Non-linearity
xtreg gini_index1 efi_index efi_index_sq lngddp unemprate corruption  edu_attainment1 pop_growth, fe vce(cluster c_id)
eststo m3
estadd local yearfe "Yes"

*Column 4: Component of EFI that Affect Income Inequality
xtreg gini_index1 lngddp unemprate corruption  edu_attainment1 pop_growth sgovt lpright smoney freetrade regulation i.year, fe vce(cluster c_id)
eststo m4
estadd local yearfe "Yes"


**********************************************************************************************************************************************************************************************************************************************
*TABLE DESIGN FOR ALL MY RESULTS
***********************************************************************************************************************
esttab m1 m2 m3 m4 using results_table.tex, ///
replace se star(* 0.05 ** 0.01 *** 0.001) ///
title("Regression Results: Income Inequality and Economic Freedom") ///
stats(N r2 yearfe, labels("Observations" "R-squared" "F.E.")) ///
varlabels(efi_index "Economic Freedom Index (EFI)" ///
efi_index_sq "Economic Freedom Index Squared" ///
lngdp "Log GDP per capita" ///
unemprate "Unemployment Rate" ///
corruption "Corruption Index" ///
edu_attainment1 "Educational Attainment" ///
pop_growth "Population Growth Rate" ///
sgovt "Size of Government" ///
lpright "Legal System and Property Rights" ///
smoney "Sound Money" ///
freetrade "Freedom to Trade Internationally" ///
regulation "Business Regulation" ///
_cons "Constant") ///
mtitles("Pooled OLS" "Fixed Effects" "Non-Linearity" "Components of EFI that Affect Inequality") ///
drop(*.year) ///
addnotes("Standard errors in parentheses. Significance: * p<0.05, ** p<0.01, *** p<0.001")












