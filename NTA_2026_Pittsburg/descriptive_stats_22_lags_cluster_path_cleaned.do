version 14.0
set matsize 2000
set more off
* Run the COMPLETE file; import replaces the dataset currently in memory.
* Adapted for replicate_2026_03_01.csv; retains the path-cleaned DFM models.
cd "C:/Users/aleaw/Documents/PhD Fall 2021 - Spring 2022/Merriman RA/Levy Elasticity Research/Levy_Elasticity"
local levy_root "`c(pwd)'"
local levy_inputs "`levy_root'/Necessary_Files"
local levy_output "`levy_root'/results/stata_path_cleaned_betterdataset"
cap mkdir "`levy_root'/results"
cap mkdir "`levy_output'"
capture log close levy_replicate
log using "`levy_output'/replicate_run.log", text replace name(levy_replicate)
which esttab
which estadd

* Keep only raw inputs required by the models and the triad diagnostic.
* Ignore imported logs, lags, r and est_value; rebuild model variables below.
import delimited "`levy_root'/replicate_2026_03_01.csv", clear varnames(1)
keep year agency_group uniqueid type triad_name av total_final_levy reassess_year home_rule_ind
rename triad_name triad
foreach x in year av total_final_levy reassess_year home_rule_ind {
    capture confirm numeric variable `x'
    if _rc {
        replace `x' = "" if inlist(trim(`x'), "NA", "NaN", ".")
        destring `x', replace
    }
}
assert !missing(agency_group) & !missing(uniqueid) & !missing(year)
assert year == floor(year)
assert inlist(reassess_year,0,1)
assert inlist(home_rule_ind,0,1)
isid agency_group year

* Preserve DFM's restriction to agencies with a FIPS crosswalk match.
* Save excluded rows for review; do not introduce using-only observations.
merge m:1 agency_group using "`levy_inputs'/fips_all_agency_name.dta", keep(master match)
preserve
keep if _merge==1 | missing(fipsid)
export delimited using "`levy_output'/unmatched_agency_years.csv", replace
restore
keep if _merge==3 & !missing(fipsid)
drop _merge
isid fipsid year
merge 1:1 fipsid year using "`levy_inputs'/census_data.dta", ///
    keep(master match) keepusing(total_ig_revenue enrollment) generate(merge_census)
tab merge_census year, missing

* Final equalization factor, not the tentative factor.
tempfile levy_eq_factor
preserve
import delimited "`levy_inputs'/eq_factor.csv", clear varnames(1)
keep year eq_factor_final
isid year
save `levy_eq_factor'
restore
merge m:1 year using `levy_eq_factor', keep(master match) generate(merge_eq)
assert merge_eq==3 & eq_factor_final>0 & !missing(eq_factor_final)
drop merge_eq

* Agency group defines the time-series panel, as in the DFM source.
* uniqueid is retained for the original clustering / unit dummy definition.
encode agency_group, gen(n_agency_group)
xtset n_agency_group year
gen double n_total_final_levy=total_final_levy
forvalues j=1/3 {
    if `j'==1 {
        gen double lag_av=L.av
        gen double lag_eq_factor_final=L.eq_factor_final
    }
    else {
        gen double lag`j'_av=L`j'.av
        gen double lag`j'_eq_factor_final=L`j'.eq_factor_final
    }
}
* Retain DFM's sample timing: save AV lags first, then start at 2008.
* Growth in 2008 is missing; main models below start in 2009.
drop if year<2008
tab year reassess_year if triad=="City" & reassess_year==1
tab year reassess_year if triad=="North" & reassess_year==1
tab year reassess_year if triad=="South" & reassess_year==1
gen double d_av=100*log(av/L.av)
* In this CSV, av * eq_factor_final equals cty_cook_eav, NOT total_eav.
gen double d_eav=100*log((av*eq_factor_final)/(L.av*L.eq_factor_final))
gen double d_levy=100*log(n_total_final_levy/L.n_total_final_levy)

***************************************************************
* checking whether reassessment year is a VALID INSTRUMENT
***************************************************************
encode(uniqueid) , gen(n_uniqueid)
tab year
regress d_eav reassess_year, vce(cluster n_uniqueid)
	estimates store all_instrument
regress d_eav reassess_year if type=="Muni", vce(cluster n_uniqueid)
	estimates store muni_instrument
regress d_eav reassess_year if type=="Other", vce(cluster n_uniqueid)
	estimates store other_instrument
regress d_eav reassess_year if type=="School", vce(cluster n_uniqueid)
	estimates store school_instrument
regress d_eav reassess_year if type=="Township", vce(cluster n_uniqueid)
	estimates store township_instrument

cd "`levy_output'"
esttab all_instrument muni_instrument other_instrument school_instrument township_instrument  using "instrument validity checks", replace ///
	cells(b(star fmt(%9.3f)) se(par)) stats(F r2 N, fmt(%4.3f %4.3f %9.0g) labels(F R-squared N)) ///
	varlabels(_cons Constant) ///
	mtitles(All Muni Other School Township) ///
	postfoot("Cook county, Illinois data from 2008 through 2023") title(Table A1: Predict assessments by reassessment year by type)

***************************************************************************************************************
* reassessment year is a strong predictor of d_eav
*  eav falls 2.5% or so in each non-reassessment year but grows by about 6% (=-2.5+8.5) in reassessment years.
************************************************************************************************************************

*********************************************************************
*control variables
*************************************************************
gen d_total_ig_revenue=100*(log(total_ig_revenue/l1.total_ig_revenue))
gen d_enrollment=100*(log(enrollment/l1.enrollment))
gen has_ig_data=1
replace has_ig_data=0 if d_total_ig_revenue==.

tab has_ig_data year, missing
tab has_ig_data type if year>2008, missing
tab has_ig_data type if year>2008 & year<=2021, missing

tab home_rule_ind type
sum d_total_ig_revenue d_enrollment
count if d_enrollment==.
count if d_total_ig_revenue==.


*histogram d_total_ig_revenue, frequency addlabel
* histogram d_enrollment, frequency addlabel


**# Bookmark #1
*********************************************************************
* OLS is equation 5 with betas, gammas and lambda's assumed equal to zero
* 2SLS is equation 6 with betas, gammas and lambda's assumed equal to zero
*
* We allow seperate year dummies for home rule and non-home rule governments since PTEL limits non-home rule governments levy increases in some cases
* n_agency_group should be the same as i.m.uniqueid
****************************************************************************************************************************************************************
drop if year<=2008

reg d_levy d_eav, vce(cluster n_uniqueid) 
     estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
	estimates store all_ols_1
reg d_levy d_eav i.year##i.home_rule_ind, vce(cluster n_uniqueid)
    estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
	estimates store all_ols_2
reg d_levy d_eav i.year##i.home_rule_ind i.n_uniqueid, vce(cluster n_uniqueid)
   estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
	estimates store all_ols_3

	
	esttab all_ols_1 all_ols_2 all_ols_3 using "OLS all agencies", replace ///
	cells(b(star fmt(%9.3f)) se(par)) stats(r2 N upper_bound, fmt(%4.3f %9.0g %9.3f) labels(R-squared)) ///
	varlabels(_cons Constant)  ///
	mtitles(M1 M2 M3) keep(d_eav) ///
	postfoot("Cook county, Illinois data from 2008 through 2023" "Columns 2 and 3 include two sets of year dummies. One for home rule governments" "and another for non-home rule governments, columns 3 includes unit dummies") ///
	title(Table 2: All agencies OLS Predict levy using d_eav)

*******************************************************************************
* next do ivregress
*   
* Montiel Olea, J. L. and C. E. Pueger (2013) A robust test for weak instruments, Journal of Business and Economic Statistics, Vol. 31, pp. 358369.
* weakivtest
*
* estat endogenous performs tests to determine whether endogenous regressors in the model are in fact exogenous.  After GMM estimation, the C (difference-in-Sargan) statistic is reported.  After 2SLS
*   estimation with an unadjusted VCE, the Durbin (1954) and Wu-Hausman (Wu 1974; Hausman 1978) statistics are reported.  After 2SLS estimation with a robust VCE, Wooldridge's (1995) robust score test and a
*    robust regression-based test are reported.  In all cases, if the test statistic is significant, then the variables being tested must be treated as endogenous.  estat endogenous is not available after
*   LIML estimation.
******************************************************************************	
ivregress 2sls d_levy (d_eav = reassess_year), vce(cluster n_uniqueid)
	estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
	estat endogenous
		estadd scalar p_exog= r(p_regF)   /* p_exog tells me the level of certainty at which I can reject the hypothesis that d_eav is exogenous */
	estat firststage     
		matrix list r(singleresults)
		estadd scalar regF=el(r(singleresults),1,4) /* regF tells me the F statistic for the first stage  */
estimates store all_iv_1
***************************************************************
* 
* note that r2 is negative in this case but that does not necessarily mean that the model is mis-specified
*  see  https://www.stata.com/support/faqs/statistics/two-stage-least-squares/
*  "At any rate, the R2 really has no statistical meaning in the context of 2SLS/IV."
************************************************************************************

* 	estadd scalar r_sum_of_squares=e(rss)  (residual sum of squares) (error sum of squares?)
*	estadd scalar r_model_sum_of_squares=e(mss)
*   estadd scalar r_square=e(r2)
*
* I think that TSS=mss+rss so R2=1-(rss/(rss+mss))
*
*The model sum of squares (MSS) equals TSS − ESS. 
*The 𝑅2 is defined as 𝑅2 = 1 − ESS/TSS.
* r2=1-(rss/mss)<0 in this case
*************************************************************
* interpretation
* estat endogenous tells me I can reject the hypothesis that d_eav is exogenous with a 99% level of confisdence
* estat firststage tells me that my F statistic is very large so that assessment year is a strong instrument
* estat weakrobust, ci generates an estimate of the coefficient on d_eav that is robust to weak instruments (we have a strong instrument not a week instrument).  Even in this case a zero coefficient on d_eav is within the confidence interval.
***************************************************************************************************************************************************************************************************
ivregress 2sls  d_levy i.year##i.home_rule_ind (d_eav = reassess_year), vce(cluster n_uniqueid)
	estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
	estat endogenous
		estadd scalar p_exog= r(p_regF)   /* p_exog tells me the level of certainty at which I can reject the hypothesis that d_eav is exogenous */
	estat firststage     
		matrix list r(singleresults)
		estadd scalar regF=el(r(singleresults),1,4) /* regF tells me the F statistic for the first stage  */
	estimates store all_iv_2

*drop if triad=="City"
* i.n_agency_group is very similiar to i.n_uniqueid
display "estimate r2= " (1-(992006.71/(992006.71+39904.284)))

ivregress 2sls  d_levy i.year##i.home_rule_ind  i.n_uniqueid (d_eav = reassess_year)
	estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
	estat endogenous
		estadd scalar p_exog= r(p_wu)   /* p_exog tells me the level of certainty at which I can reject the hypothesis that d_eav is exogenous using Wu_Hausman F statistics */
	estat firststage     
		matrix list r(singleresults)
		estadd scalar regF=el(r(singleresults),1,4) /* regF tells me the F statistic for the first stage  */
	estimates store all_iv_3
	
esttab all_iv_1 all_iv_2 all_iv_3 using "IV all agencies", replace stats(N upper_bound p_exog regF, fmt(%9.0g %9.3f %9.3f %9.1f) labels(N)) cells(b(star fmt(%9.3f)) se(par)) ///
keep(d_eav) postfoot("Cook county, Illinois data from 2008 through 2023" "Columns 2 and 3 include two sets of year dummies. One for home rule governments" "and another for non-home rule governments, columns 3 includes unit dummies" ///
"p_exog is the p value for the hypothesis that d_eav is exogenous" "regF is the F statistic of the first stage regression") title(Table 3: All agencies IV Predict levy using d_eav)


tab uniqueid, missing
********************************************************
* we have less than 15 observations of a few uniqueids
* shjould fix that
****************************************************************

*************************************************************************************************
* next run regressions for four different types of agencies
* and both home rule and non-home rule municipalities
* code below adapted from Claude
***************************************************************************************************	
tab home_rule_ind type

gen type_2=type
replace type_2="HR_muni" if type=="Muni" & home_rule_ind==1

sort type_2
by type_2: count
*by type_2: count if d_total_ig_revenue~=.
*list agency_group total_ig_revenue d_total_ig_revenue if type_2=="Other" & d_total_ig_revenue~=. & total_ig_revenue>2000

count if type_2=="Other"

 
* Loop through government types
tab type_2

local gov_types "Other Township HR_muni Muni School "

foreach gov in `gov_types' {
    preserve
    keep if type_2 == "`gov'"
	count 
 
    * Run and store regressions
   regress d_levy d_eav, vce(cluster n_uniqueid)
		estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
		estadd scalar point_est=_b[d_eav]
		estimates store reg1_`gov'
		
   regress d_levy d_eav i.year, vce(cluster n_uniqueid)
		estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
		estadd scalar point_est=_b[d_eav]
		estimates store reg2_`gov'	
		
	regress d_levy d_eav i.year i.n_uniqueid 
		estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
		estadd scalar point_est=_b[d_eav]
		estimates store reg3_`gov'	
		
	restore
}		
**************************************************************************
* code below suggested by claude 
*  the code creates some blank estimates I can use to make the esttab tables format better
*********************************************************************************************************
cap drop _blank
gen _blank = 0
quietly reg _blank          // regression on all-missing var → empty results

forval i = 1/5 {
    estimates store blank`i'
}

**************************************************************************************************
* use data on intergovernmental revenues and enrollments
*******************************************************************************************
local gov_types_B "HR_muni Muni School "

foreach gov in `gov_types_B' {
    preserve
    keep if type_2 == "`gov'"
	count 

regress d_levy d_eav d_total_ig_revenue i.year i.n_uniqueid
		estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
		estadd scalar point_est=_b[d_eav]
		estimates store reg4_`gov'
	gen byte used_in_reg_x = e(sample)

regress d_levy d_eav i.year i.n_uniqueid if used_in_reg_x==1
		estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
		estadd scalar point_est=_b[d_eav]
		estimates store reg5_`gov'
		
		restore
}

regress d_levy d_eav d_enrollment i.year i.n_uniqueid if type_2=="School"
estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
estimates store reg6_school

esttab reg1_Other reg1_Township reg1_HR_muni reg1_Muni reg1_School using "ols_regressions", replace cells(none) stats(N upper_bound, fmt(%5.0fc %9.3f)) mtitles(Other Township HR_muni Muni School ) ///
 nolines title(Table 4 By agency type: OLS upper bounds estimate of ε_b )
esttab reg2_Other reg2_Township reg2_HR_muni reg2_Muni reg2_School  using "ols_regressions", append cells(none) stats(upper_bound, fmt(%9.3f))  nomtitles nolines nonumbers
esttab reg3_Other reg3_Township reg3_HR_muni reg3_Muni reg3_School  using "ols_regressions", append cells(none) stats(upper_bound, fmt(%9.3f))  nomtitles nolines nonumbers
esttab blank1 blank2 reg4_HR_muni reg4_Muni reg4_School  using "ols_regressions", append cells(none) stats(N upper_bound, fmt(%5.0fc %9.3f)) nomtitles nolines nonumbers
esttab blank1 blank2 reg5_HR_muni reg5_Muni reg5_School  using "ols_regressions", append cells(none) stats(N upper_bound, fmt(%5.0fc %9.3f)) nomtitles nolines nonumbers
esttab blank1 blank2 blank3 blank4 reg6_school  using "ols_regressions", append cells(none) stats(N upper_bound, fmt(%5.0fc %9.3f)) nomtitles nolines nonumbers ///
postfoot("Cook county, Illinois data from 2008 through 2023. Cell entries record the upper bound which is the maximum value of the elasticity"  ///
"that we fail to reject with 95 percent confidence. N is the number of non-missing observations in rows immediately below. Row 1 is based on " ///
"an ols regression on d_eav with no controls. Rows 2 and 3 are based on OLS regressions that also include year dummies; row 3 includes unit dummies." ///
"Row 4 is based on an OLS regression that include controls for the percentage change in intergovernmental revenues, year and unit dummies." ///
"Row 5 uses the same sample as in row 4 (i.e. it drops observations with missing value for the percentage change in intergovernmental revenues)" ///
"but the regression does not control for the percentage change in intergovernmental revenues. Row 6 is based on an OLS regression that includes" ///
"controls for the percentage change in enrollment, year and unit dummies.")


esttab reg1_Other reg1_Township reg1_HR_muni reg1_Muni reg1_School ///
    using "ols_regressions_appendix_point", replace cells(b(star fmt(%9.3f))) collabels("b") noobs noconstant keep(d_eav) mtitles(Other Township HR_muni Muni School) ///
    nolines title(Table A2 By agency type: OLS point estimate of ε_b)

esttab reg2_Other reg2_Township reg2_HR_muni reg2_Muni reg2_School using "ols_regressions_appendix_point", append cells(b(star fmt(%9.3f))) noobs noconstant keep(d_eav) ///
    nomtitles nolines nonumbers  collabels(none)

esttab reg3_Other reg3_Township reg3_HR_muni reg3_Muni reg3_School using "ols_regressions_appendix_point", append cells(b(star fmt(%9.3f))) noobs noconstant keep(d_eav) ///
    nomtitles nolines nonumbers  collabels(none)

esttab blank1 blank2 reg4_HR_muni reg4_Muni reg4_School using "ols_regressions_appendix_point", append cells(b(star fmt(%9.3f))) noobs noconstant keep(d_eav) ///
    nomtitles nolines nonumbers  collabels(none)

esttab blank1 blank2 reg5_HR_muni reg5_Muni reg5_School using "ols_regressions_appendix_point", append cells(b(star fmt(%9.3f))) noobs noconstant keep(d_eav) ///
    nomtitles nolines nonumbers  collabels(none)

esttab blank1 blank2 blank3 blank4 reg6_school using "ols_regressions_appendix_point", append cells(b(star fmt(%9.3f))) noobs noconstant keep(d_eav) ///
    nomtitles nolines nonumbers  collabels(none) ///
postfoot("Cook county, Illinois data from 2008 through 2023. Cell entries record a point estimate of the elasticity Row 1 is based on " ///
"an ols regression on d_eav with no controls. Rows 2 and 3 are based on OLS regressions that also include year dummies; row 3 includes unit dummies." ///
"Row 4 is based on an OLS regression that include controls for the percentage change in intergovernmental revenues, year and unit dummies." ///
"Row 5 uses the same sample as in row 4 (i.e. it drops observations with missing value for the percentage change in intergovernmental revenues)" ///
"but the regression does not control for the percentage change in intergovernmental revenues. Row 6 is based on an OLS regression that includes" ///
"controls for the percentage change in enrollment, year and unit dummies.")

*************************************************************************************************
* next run instrumental variable regressions for four different types of agencies
* and both home rule and non-home rule municipalities
* code below adapted from Claude
***************************************************************************************************		
	foreach gov in `gov_types' {
    preserve
    keep if type_2 == "`gov'"
    
    * Run and store regressions
	ivregress 2sls d_levy (d_eav = reassess_year), vce(cluster n_uniqueid)
		estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
		estimates store ivreg7_`gov'
		
    ivregress 2sls d_levy i.year (d_eav = reassess_year), vce(cluster n_uniqueid)
		estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
		estimates store ivreg8_`gov'
		
    ivregress 2sls d_levy i.year i.n_uniqueid (d_eav = reassess_year)
		estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
		estimates store ivreg9_`gov'
		
		restore
			}

**************************************************************************************************
* use data on intergovernmental revenues and enrollments
*******************************************************************************************	
**************************************************************************************************
* use data on intergovernmental revenues and enrollments
*******************************************************************************************
local gov_types_B "HR_muni Muni School "

foreach gov in `gov_types_B' {
    preserve
    keep if type_2 == "`gov'"
	count 

ivregress 2sls d_levy d_total_ig_revenue i.year i.n_uniqueid (d_eav = reassess_year)
		estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
		estimates store ivreg10_`gov'
	gen byte used_in_reg_x = e(sample)

ivregress 2sls d_levy i.year i.n_uniqueid (d_eav = reassess_year) if used_in_reg_x==1
		estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
		estimates store ivreg11_`gov'
		
		restore
}

ivregress 2sls d_levy d_enrollment i.year i.n_uniqueid (d_eav = reassess_year) if type_2=="School"
	estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
	estimates store ivreg12_School
	
esttab ivreg7_Other ivreg7_Township ivreg7_HR_muni ivreg7_Muni ivreg7_School using "iv_regressions", replace cells(none) stats(N upper_bound, fmt(%5.0fc %9.3f)) mtitles(Other Township HR_muni Muni School ) ///
nolines title(Table 5: By agency type: IV upper bounds estimate of ε_b)
esttab ivreg8_Other ivreg8_Township ivreg8_HR_muni ivreg8_Muni ivreg8_School  using "iv_regressions", append cells(none) stats(upper_bound, fmt(%9.3f))  nomtitles nolines nonumbers
esttab ivreg9_Other ivreg9_Township ivreg9_HR_muni ivreg9_Muni ivreg9_School  using "iv_regressions", append cells(none) stats(upper_bound, fmt(%9.3f))  nomtitles nolines nonumbers
esttab blank1 blank2 ivreg10_HR_muni ivreg10_Muni ivreg10_School  using "iv_regressions", append cells(none) stats(N upper_bound, fmt(%5.0fc %9.3f)) nomtitles nolines nonumbers
esttab blank1 blank2 ivreg11_HR_muni ivreg11_Muni ivreg11_School  using "iv_regressions", append cells(none) stats(N upper_bound, fmt(%5.0fc %9.3f)) nomtitles nolines nonumbers 
esttab blank1 blank2 blank3 blank4 ivreg12_School  using "iv_regressions", append cells(none) stats(N upper_bound, fmt(%5.0fc %9.3f)) nomtitles nolines nonumbers ///
postfoot("Cook county, Illinois data from 2008 through 2023. Cell entries record the upper bound which is the maximum value of the elasticity"  ///
"that we fail to reject with 95 percent confidence. N is the number of non-missing observations in rows immediately below. Row 1 is based on " ///
"an iv regression on d_eav with no controls. Rows 2 and 3 are based on iv regressions that also include year dummies; row 3 includes unit dummies." ///
"Row 4 is based on an iv regression that include controls for the percentage change in intergovernmental revenues, year and unit dummies." ///
"Row 5 uses the same sample as in row 4 (i.e. it drops observations with missing value for the percentage change in intergovernmental revenues)" ///
"but the regression does not control for the percentage change in intergovernmental revenues. Row 6 is based on an iv regression that includes" ///
"controls for the percentage change in enrollment, year and unit dummies.")

*******************************************************************************
* create  table A3
******************************************************************************

esttab ivreg7_Other ivreg7_Township ivreg7_HR_muni ivreg7_Muni ivreg7_School ///
    using "iv_regressions_appendix_point", replace cells(b(star fmt(%9.3f))) collabels("b") noobs noconstant keep(d_eav) mtitles(Other Township HR_muni Muni School) ///
    nolines title(Table A3 By agency type: IV point estimate of ε_b)
esttab ivreg8_Other ivreg8_Township ivreg8_HR_muni ivreg8_Muni ivreg8_School using "iv_regressions_appendix_point", append cells(b(star fmt(%9.3f))) noobs noconstant keep(d_eav) ///
    nomtitles nolines nonumbers  collabels(none)
esttab ivreg9_Other ivreg9_Township ivreg9_HR_muni ivreg9_Muni ivreg9_School using "iv_regressions_appendix_point", append cells(b(star fmt(%9.3f))) noobs noconstant keep(d_eav) ///
    nomtitles nolines nonumbers  collabels(none)
esttab blank1 blank2 ivreg10_HR_muni ivreg10_Muni ivreg10_School using "iv_regressions_appendix_point", append cells(b(star fmt(%9.3f))) noobs noconstant keep(d_eav) ///
    nomtitles nolines nonumbers  collabels(none)
esttab blank1 blank2 ivreg11_HR_muni ivreg11_Muni ivreg11_School using "iv_regressions_appendix_point", append cells(b(star fmt(%9.3f))) noobs noconstant keep(d_eav) ///
    nomtitles nolines nonumbers  collabels(none)
esttab blank1 blank2 blank3 blank4 ivreg12_School using "iv_regressions_appendix_point", append cells(b(star fmt(%9.3f))) noobs noconstant keep(d_eav) ///
    nomtitles nolines nonumbers  collabels(none) ///
postfoot("Cook county, Illinois data from 2008 through 2023. Cell entries record a point estimate of the elasticity Row 1 is based on " ///
"an iv regression on d_eav with no controls. Rows 2 and 3 are based on iv regressions that also include year dummies; row 3 includes unit dummies." ///
"Row 4 is based on an iv regression that include controls for the percentage change in intergovernmental revenues, year and unit dummies." ///
"Row 5 uses the same sample as in row 4 (i.e. it drops observations with missing value for the percentage change in intergovernmental revenues)" ///
"but the regression does not control for the percentage change in intergovernmental revenues. Row 6 is based on an IV regression that includes" ///
"controls for the percentage change in enrollment, year and unit dummies.")


******************************************************************************************************
**# Bookmark #2
* allow for asymmetry with respect to eav growth
******************************************************************************************************
gen eav_growth=0
replace eav_growth=1 if d_eav>0
tab eav_growth
gen pos_d_eav=d_eav*eav_growth
gen neg_d_eav=d_eav*(1-eav_growth)

reg d_levy d_eav 
reg d_levy d_eav pos_d_eav
reg d_levy pos_d_eav neg_d_eav

test pos_d_eav=neg_d_eav

*********************************************************************
*   run regressions for all governments allowing for asymmetry
* OLS is equation 5 with betas, gammas and lambda's assumed equal to zero
* 2SLS is equation 6 with betas, gammas and lambda's assumed equal to zero
*********************************************************************************
drop if year<=2008


reg d_levy pos_d_eav neg_d_eav, vce(cluster n_uniqueid)
		test neg_d_eav = pos_d_eav
		estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
		estadd scalar p_equal = r(p)
		estimates store reg_13
reg d_levy pos_d_eav neg_d_eav i.year##i.home_rule_ind, vce(cluster n_uniqueid)
		test neg_d_eav = pos_d_eav
		estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
		estadd scalar p_equal = r(p)
		estimates store reg_14
reg d_levy pos_d_eav neg_d_eav i.year##i.home_rule_ind i.n_uniqueid, vce(cluster n_uniqueid)
		test neg_d_eav = pos_d_eav
		estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
		estadd scalar p_equal = r(p)
		estimates store reg_15

	esttab reg_13 reg_14 reg_15 using "OLS all agencies_asym", replace ///
	cells(b(star fmt(%9.3f)) se(par)) stats(r2 N p_equal upper_bound, fmt(%4.3f %9.0fc %4.3f %9.3f) labels(R-squared)) ///
	mtitles(M1 M2 M3) keep(pos_d_eav neg_d_eav) ///
	postfoot("Cook county, Illinois data from 2008 through 2023. Columns 2 and 3 include two sets of year dummies. One for home rule governments" "and another for non-home rule governments, columns 3 includes unit dummies" ///
	"p_equal is the p value for the hypothesis that the elasticity of the levy with respect" "to d_eav is symmetric. Upper bound is the maximum value of the elasticity" ///
	"that we fail to reject with 95 percent confidence") title(Table 6: All agencies OLS Predict levy using d_eav allowing for asymmetry)

	
*******************************************************************************
* next do ivregress allowing for asymmetry -- TABLE 7 
******************************************************************************	
***************************************************************************************************		
*  since the interaction effect (positive d_eav*eav) I need to create an instrument for the interaction effect as well as for d_eav
*  to do this I am following the suggestion from Tirthankar Chakravarty from Dec 21, 2011
* I found on statalist at https://www.stata.com/statalist/archive/2012-01/msg00877.html#:~:text=Hi%20all%2C%20The%20described%20IV,in%20the%20first%20step%202.
***************************************************************************************************************************
reg d_eav reassess_year i.year
predict d_eav_hat
gen pos_d_eav_hat=pos_d_eav*d_eav_hat
ivregress 2sls d_levy (d_eav pos_d_eav= reassess_year pos_d_eav_hat) 

ivregress 2sls d_levy (neg_d_eav pos_d_eav= reassess_year pos_d_eav_hat)
 		test neg_d_eav = pos_d_eav
		estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
		estadd scalar p_equal = r(p)
		estadd scalar p_exog= r(p_regF)
		estat firststage,all  
			matrix list r(singleresults)
			estadd scalar F_neg=el(r(singleresults),1,4)
			estadd scalar F_pos=el(r(singleresults),2,4)
		estimates store reg_16	

*********************************************
* I have two endogenous variables.
*  Fstatistics on the first stage regressions are saved
*  these tell me about the strength of my instruments
***************************************************************

ivregress 2sls  d_levy i.year##i.home_rule_ind  (neg_d_eav pos_d_eav= reassess_year pos_d_eav_hat)
		test neg_d_eav = pos_d_eav
		estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
		estadd scalar p_equal = r(p)
		estat firststage, all 
			matrix list r(singleresults)
			estadd scalar F_neg=el(r(singleresults),1,4)
			estadd scalar F_pos=el(r(singleresults),2,4)
		estimates store reg_17
ivregress 2sls  d_levy  i.year##i.home_rule_ind  i.n_uniqueid (neg_d_eav pos_d_eav= reassess_year pos_d_eav_hat)
		test neg_d_eav = pos_d_eav
		estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
		estadd scalar p_equal = r(p)
		estat firststage,all  
			matrix list r(singleresults)
			estadd scalar F_neg=el(r(singleresults),1,4)
			estadd scalar F_pos=el(r(singleresults),2,4)
		estimates store reg_18
		
esttab reg_16 reg_17 reg_18 using "iv all agencies_asym", replace ///
	cells(b(star fmt(%9.3f)) se(par)) stats(F_neg F_pos N p_equal upper_bound, fmt(%9.1fc  %9.1fc  %5.0fc %9.3f %9.3f))  ///
	mtitles(M1 M2 M3) keep(pos_d_eav neg_d_eav) order(pos_d_eav neg_d_eav) ///
	postfoot("Cook county, Illinois data from 2008 through 2023. Columns 2 and 3 include two sets of year dummies. One for home rule governments" "and another for non-home rule governments, columns 3 includes unit dummies" ///
	"F_pos and F_neg are first stage F statistics for corresponding d_eav variables. p_equal is the p value for the hypothesis that the elasticity" ///
	"of the levy with respect to d_eav is symmetric. Upper bound is the maximum value of the elasticity that we fail to reject with 95 percent confidence") ///
	title(Table 7: All agencies IV Predict levy using d_eav allowing for asymmetry)

* Continue to by-type asymmetric and lagged models.

*************************************************************************************************
* next run regressions for four different types of agencies
* and both home rule and non-home rule municipalities
* code below adapted from Claude
***************************************************************************************************		
* Loop through government types
local gov_types "Other Township HR_muni Muni School "

foreach gov in `gov_types' {
    preserve
    keep if type_2 == "`gov'"
    
    * Run and store regressions
    eststo reg19_`gov': reg d_levy neg_d_eav pos_d_eav
		test neg_d_eav = pos_d_eav
		estadd scalar p_equal = r(p)
		estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
    eststo reg20_`gov': reg d_levy neg_d_eav pos_d_eav i.year
		test neg_d_eav = pos_d_eav
		estadd scalar p_equal = r(p)
		estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
    eststo reg21_`gov': reg d_levy neg_d_eav pos_d_eav i.year i.n_uniqueid
		test neg_d_eav = pos_d_eav
		estadd scalar p_equal = r(p)
		estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
		
    restore  
}


**************************************************************************************************
* use data on intergovernmental revenues and enrollments
*******************************************************************************************
local gov_types_B "HR_muni Muni School "

foreach gov in `gov_types_B' {
    preserve
    keep if type_2 == "`gov'"
	count 

regress d_levy neg_d_eav pos_d_eav d_total_ig_revenue i.year i.n_uniqueid
	test neg_d_eav = pos_d_eav
		estadd scalar p_equal = r(p)
		estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
		estimates store reg22_`gov'
		gen byte used_in_reg_x = e(sample)

regress d_levy neg_d_eav pos_d_eav i.year i.n_uniqueid if used_in_reg_x==1
		test neg_d_eav = pos_d_eav
		estadd scalar p_equal = r(p)
		estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
		estimates store reg23_`gov'
		
		restore
}

regress d_levy neg_d_eav pos_d_eav d_enrollment i.year i.n_uniqueid if type_2=="School"
		test neg_d_eav = pos_d_eav
		estadd scalar p_equal = r(p)
		estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
		estimates store reg24_School

* Create table with all results
esttab reg19_Other reg19_Township reg19_HR_muni reg19_Muni reg19_School using "asym_ols_regressions", replace cells(none) stats(N upper_bound, fmt(%5.0fc %9.3f)) mtitles(Other Township HR_muni Muni School ) ///
 nolines title(Table 8: By agency type: OLS upper bounds estimate of ε_b allowing for asymmetry)
esttab reg20_Other reg20_Township reg20_HR_muni reg20_Muni reg20_School using "asym_ols_regressions", append cells(none) stats(upper_bound, fmt(%9.3f)) nomtitles nolines nonumbers
esttab reg21_Other reg21_Township reg21_HR_muni reg21_Muni reg21_School using "asym_ols_regressions", append cells(none) stats(upper_bound, fmt(%9.3f)) nomtitles nolines nonumbers 
esttab blank1 blank2 reg22_HR_muni reg22_Muni reg22_School  using "asym_ols_regressions", append cells(none) stats(N upper_bound, fmt(%5.0fc %9.3f)) nomtitles nolines nonumbers
esttab blank1 blank2 reg23_HR_muni reg23_Muni reg23_School  using "asym_ols_regressions", append cells(none) stats(upper_bound, fmt(%9.3f)) nomtitles nolines nonumbers
esttab blank1 blank2 blank3 blank4 reg24_School  using "asym_ols_regressions", append cells(none) stats(upper_bound, fmt(%9.3f)) nomtitles nolines nonumbers ///
postfoot("Cook county, Illinois data from 2008 through 2023. Cell entries record the upper bound which is the maximum value of the elasticity"  ///
"that we fail to reject with 95 percent confidence. All estimates are derived from OLS regressions that allow for asymmetric reactions to postive and negative changes in EAV." ///
"Row 1 is based on an ols regression with no controls. Rows 2 and 3 also include year dummies; row 3 includes unit dummies." ///
"Row 4 is based on an ols regression with the percentage change in intergovernmental revenue, year dummies and unit dummies" ///
"Row 5 is based on an ols regression using the same sample as in row 4 but omitting the change in intergovernmental revenue while including year and unit dummies" ///
"Row 6 is based on an ols regression using the same sample as in rows 4 and 5 and including the percentage change in enrollments as well as year and unit dummies" )

* Create table A4
esttab reg19_Other reg19_Township reg19_HR_muni reg19_Muni reg19_School using "asym_ols_regressions_appendix", replace cells(none) stats(p_equal, fmt(%9.3f)) mtitles(Other Township HR_muni Muni School ) ///
nolines title(Table A4: P_stats for OLS tests of symmetry)
esttab reg20_Other reg20_Township reg20_HR_muni reg20_Muni reg20_School using "asym_ols_regressions_appendix", append cells(none) stats(p_equal, fmt(%9.3f)) nomtitles nolines nonumbers
esttab reg21_Other reg21_Township reg21_HR_muni reg21_Muni reg21_School using "asym_ols_regressions_appendix", append cells(none) stats(p_equal, fmt(%9.3f)) nomtitles nolines nonumbers 
esttab blank1 blank2 reg22_HR_muni reg22_Muni reg22_School  using "asym_ols_regressions_appendix", append cells(none) stats(p_equal, fmt(%9.3f)) nomtitles nolines nonumbers
esttab blank1 blank2 reg23_HR_muni reg23_Muni reg23_School  using "asym_ols_regressions_appendix", append cells(none) stats(p_equal, fmt(%9.3f)) nomtitles nolines nonumbers
esttab blank1 blank2 blank3 blank4 reg24_School  using "asym_ols_regressions_appendix", append cells(none) stats(p_equal, fmt(%9.3f)) nomtitles nolines nonumbers ///
postfoot("Cook county, Illinois data from 2008 through 2023. Cell entries record the p value for the hypothesis that the elasticity of the levy with respect to d_eav is symmetric"  ///
"All estimates are derived from the same OLS regressions used to estimate the upper bounds reported in Table 8." )

*******************************************************************************
* create  table A5  
******************************************************************************
esttab reg19_Other reg19_Township reg19_HR_muni reg19_Muni reg19_School ///
    using "ols_asym_regressions_appendix_point", replace cells(b(star fmt(%9.3f))) collabels("b") noobs noconstant keep(pos_d_eav) mtitles(Other Township HR_muni Muni School) ///
    nolines title(Table A5 By agency type: OLS point estimate of ε_b for EAV increases)

esttab reg20_Other reg20_Township reg20_HR_muni reg20_Muni reg20_School using "ols_asym_regressions_appendix_point", append cells(b(star fmt(%9.3f))) noobs noconstant keep(pos_d_eav) ///
    nomtitles nolines nonumbers  collabels(none)

esttab reg21_Other reg21_Township reg21_HR_muni reg21_Muni reg21_School using "ols_asym_regressions_appendix_point", append cells(b(star fmt(%9.3f))) noobs noconstant keep(pos_d_eav) ///
    nomtitles nolines nonumbers  collabels(none)

esttab blank1 blank2 reg22_HR_muni reg22_Muni reg22_School using "ols_asym_regressions_appendix_point", append cells(b(star fmt(%9.3f))) noobs noconstant keep(pos_d_eav) ///
    nomtitles nolines nonumbers  collabels(none)

esttab blank1 blank2 reg23_HR_muni reg23_Muni reg23_School using "ols_asym_regressions_appendix_point", append cells(b(star fmt(%9.3f))) noobs noconstant keep(pos_d_eav) ///
    nomtitles nolines nonumbers  collabels(none)

esttab blank1 blank2 blank3 blank4 reg24_School using "ols_asym_regressions_appendix_point", append cells(b(star fmt(%9.3f))) noobs noconstant keep(pos_d_eav) ///
    nomtitles nolines nonumbers  collabels(none) ///
postfoot("Cook county, Illinois data from 2008 through 2023. All estimates are derived from OLS regressions that allow for asymmetric reactions to postive and negative changes in EAV."  ///
"Cell entries record a point estimate of the elasticity Row 1 is based on an ols regression  d_eav with no controls. Rows 2 and 3 are based on OLS regressions" ///
"that also include year dummies; row 3 includes unit dummies." ///
"Row 4 is based on an OLS regression that include controls for the percentage change in intergovernmental revenues, year and unit dummies." ///
"Row 5 uses the same sample as in row 4 (i.e. it drops observations with missing value for the percentage change in intergovernmental revenues)" ///
"but the regression does not control for the percentage change in intergovernmental revenues. Row 6 is based on an OLS regression that includes" ///
"controls for the percentage change in enrollment, year and unit dummies.")

*************************************************************************************************
* next run instrumental variable regressions for four different types of agencies
* and both home rule and non-home rule municipalities
* code below adapted from  claude
***************************************************************************************************		
**# Bookmark #3
*******************************************************************************************
* Loop through government types
local gov_types "Other Township HR_muni Muni School "

	foreach gov in `gov_types' {
 preserve
    keep if type_2 == "`gov'"
    
    * Run and store regressions
    eststo ivreg25_`gov':ivregress 2sls d_levy (neg_d_eav pos_d_eav= reassess_year pos_d_eav_hat) 
			test neg_d_eav = pos_d_eav
			estadd scalar p_equal = r(p)
			estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
			estat firststage,all  
			matrix list r(singleresults)
				estadd scalar F_neg=el(r(singleresults),1,4)
				estadd scalar F_pos=el(r(singleresults),2,4)
	eststo ivreg26_`gov':ivregress 2sls d_levy i.year (neg_d_eav pos_d_eav= reassess_year pos_d_eav_hat) 
			test neg_d_eav = pos_d_eav
			estadd scalar p_equal = r(p)
			estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
			estat firststage,all  
			matrix list r(singleresults)
				estadd scalar F_neg=el(r(singleresults),1,4)
				estadd scalar F_pos=el(r(singleresults),2,4)
	eststo ivreg27_`gov':ivregress 2sls d_levy i.year i.n_uniqueid (neg_d_eav pos_d_eav= reassess_year pos_d_eav_hat) 
			test neg_d_eav = pos_d_eav
			estadd scalar p_equal = r(p)
			estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
			estat firststage,all  
			matrix list r(singleresults)
				estadd scalar F_neg=el(r(singleresults),1,4)
				estadd scalar F_pos=el(r(singleresults),2,4)
restore
	}
**************************************************************************************************
* use data on intergovernmental revenues and enrollments
*******************************************************************************************
local gov_types_B "HR_muni Muni School"

foreach gov in `gov_types_B' {
    preserve
    keep if type_2 == "`gov'"
	count 

ivregress 2sls d_levy d_total_ig_revenue i.year i.n_uniqueid (neg_d_eav pos_d_eav= reassess_year pos_d_eav_hat) 
	test neg_d_eav = pos_d_eav
		estadd scalar p_equal = r(p)
		estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
		estat firststage,all  
			matrix list r(singleresults)
				estadd scalar F_neg=el(r(singleresults),1,4)
				estadd scalar F_pos=el(r(singleresults),2,4)
		estimates store ivreg28_`gov'
		gen byte used_in_reg_x = e(sample)

ivregress 2sls d_levy i.year i.n_uniqueid (neg_d_eav pos_d_eav= reassess_year pos_d_eav_hat) if used_in_reg_x==1 
		test neg_d_eav = pos_d_eav
		estadd scalar p_equal = r(p)
		estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
		estat firststage,all  
			matrix list r(singleresults)
				estadd scalar F_neg=el(r(singleresults),1,4)
				estadd scalar F_pos=el(r(singleresults),2,4)
		estimates store ivreg29_`gov'
		
		restore
}



ivregress 2sls d_levy d_enrollment i.year i.n_uniqueid  (neg_d_eav pos_d_eav= reassess_year pos_d_eav_hat) if type_2=="School"
		test neg_d_eav = pos_d_eav 
		estadd scalar p_equal = r(p)
		estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
		estat firststage,all  
			matrix list r(singleresults)
				estadd scalar F_neg=el(r(singleresults),1,4)
				estadd scalar F_pos=el(r(singleresults),2,4)
		estimates store ivreg30_School


esttab ivreg25_* using "iv_by_agency_asym", replace mtitles(Other Township HR_muni Muni School ) cells(none) stats(N upper_bound, fmt(%5.0fc %9.3f))  nolines nonumbers ///
 title(Table 9: By agency type: IV upper bounds estimate of ε_b allowing for asymmetry)
esttab ivreg26_* using "iv_by_agency_asym", append  cells(none) stats(upper_bound, fmt(%9.3f))  nomtitles nolines nonumbers
esttab ivreg27_* using "iv_by_agency_asym", append  cells(none) stats(upper_bound, fmt(%9.3f))  nomtitles nolines nonumbers
esttab blank1 blank2 ivreg28_HR_muni ivreg28_Muni ivreg28_School  using "iv_by_agency_asym", append cells(none) stats(N upper_bound, fmt(%5.0fc %9.3f)) nomtitles nolines nonumbers
esttab blank1 blank2 ivreg29_HR_muni ivreg29_Muni ivreg29_School  using "iv_by_agency_asym", append cells(none) stats(upper_bound, fmt(%9.3f)) nomtitles nolines nonumbers 
esttab blank1 blank2 blank3 blank4 ivreg30_School  using "iv_by_agency_asym", append cells(none) stats(upper_bound, fmt(%9.3f)) nomtitles nolines nonumbers ///
postfoot("Cook county, Illinois data from 2008 through 2023. Cell entries in rows labeled N record the number of observations used in the rows" /// 
"immediately below. Cell entries in rows labeled upper bound record the maximum value of the elasticity that we fail to reject with" ///
"95 percent confidence. All estimates are derived from IV regressions that allow for asymmetric reactions to postive and negative changes in EAV." ///
"Row 1 is based on an IV regression with no controls. Rows 2 and 3 also include year dummies; row 3 includes unit dummies." ///
"Row 4 is based on an IV regression with the percentage change in intergovernmental revenue, year dummies and unit dummies" ///
"Row 5 is based on an IV regression using the same sample as in row 4 but omitting the change in intergovernmental revenue while including year and unit dummies" ///
"Row 6 is based on an IV regression using the same sample as in rows 4 and 5 and including the percentage change in enrollments as well as year and unit dummies" )

*Create Table A6
esttab ivreg25_* using "iv_by_agency_asym_appendix", replace cells(none) stats(p_equal, fmt(%9.3f)) mtitles(Other Township HR_muni Muni School ) nolines ///
  title(Table A6: P_stats for IV tests of symmetry)
esttab ivreg26_* using "iv_by_agency_asym_appendix", append cells(none) stats(p_equal, fmt(%9.3f)) nomtitles nolines nonumbers
esttab ivreg27_* using "iv_by_agency_asym_appendix", append cells(none) stats(p_equal, fmt(%9.3f)) nomtitles nolines nonumbers
esttab blank1 blank2 ivreg28_HR_muni ivreg28_Muni ivreg28_School  using "iv_by_agency_asym_appendix", append cells(none) stats(N p_equal, fmt(%5.0fc %9.3f)) nomtitles nolines nonumbers
esttab blank1 blank2 ivreg29_HR_muni ivreg29_Muni ivreg29_School  using "iv_by_agency_asym_appendix", append cells(none) stats(p_equal, fmt(%9.3f)) nomtitles nolines nonumbers 
esttab blank1 blank2 blank3 blank4 ivreg30_School  using "iv_by_agency_asym_appendix", append cells(none) stats(p_equal, fmt(%9.3f)) nomtitles nolines nonumbers ///
postfoot("Cook county, Illinois data from 2008 through 2023. Cell entries record the p value for the hypothesis that the elasticity of the levy with respect to d_eav is symmetric"  ///
"All estimates are derived from the same IV regressions used to estimate the upper bounds reported in Table 9." )


*Create Table A7
esttab ivreg25_Other ivreg25_Township ivreg25_HR_muni ivreg25_Muni ivreg25_School ///
    using "iv_async_regressions_appendix_point", replace cells(b(star fmt(%9.3f))) collabels("b") noobs noconstant keep(pos_d_eav) mtitles(Other Township HR_muni Muni School) ///
    nolines title(Table A7 By agency type: IV point estimate of ε_b for EAV increases)
esttab ivreg26_Other ivreg26_Township ivreg26_HR_muni ivreg26_Muni ivreg26_School using "iv_async_regressions_appendix_point", append cells(b(star fmt(%9.3f))) noobs noconstant keep(pos_d_eav) ///
    nomtitles nolines nonumbers  collabels(none)
esttab ivreg27_Other ivreg27_Township ivreg27_HR_muni ivreg27_Muni ivreg27_School using "iv_async_regressions_appendix_point", append cells(b(star fmt(%9.3f))) noobs noconstant keep(pos_d_eav) ///
    nomtitles nolines nonumbers  collabels(none)
esttab blank1 blank2 ivreg28_HR_muni ivreg28_Muni ivreg28_School using "iv_async_regressions_appendix_point", append cells(b(star fmt(%9.3f))) noobs noconstant keep(pos_d_eav) ///
    nomtitles nolines nonumbers  collabels(none)
esttab blank1 blank2 ivreg29_HR_muni ivreg29_Muni ivreg29_School using "iv_async_regressions_appendix_point", append cells(b(star fmt(%9.3f))) noobs noconstant keep(pos_d_eav) ///
    nomtitles nolines nonumbers  collabels(none)
esttab blank1 blank2 blank3 blank4 ivreg30_School using "iv_async_regressions_appendix_point", append cells(b(star fmt(%9.3f))) noobs noconstant keep(pos_d_eav) ///
    nomtitles nolines nonumbers  collabels(none) ///
postfoot("Cook county, Illinois data from 2008 through 2023. Cell entries record a point estimate of the elasticity Row 1 is based on " ///
"an IV regression on d_eav with no controls. Rows 2 and 3 are based on IV regressions that also include year dummies; row 3 includes unit dummies." ///
"Row 4 is based on an IV regression that include controls for the percentage change in intergovernmental revenues, year and unit dummies." ///
"Row 5 uses the same sample as in row 4 (i.e. it drops observations with missing value for the percentage change in intergovernmental revenues)" ///
"but the regression does not control for the percentage change in intergovernmental revenues. Row 6 is based on an IV regression that includes" ///
"controls for the percentage change in enrollment, year and unit dummies.")


**# Bookmark #1
*******************************************************************************
* study response to lagged changes in EAV
*****************************************************************************

gen d_2_eav=100*log((lag_av*lag_eq_factor_final)/(lag2_av*lag2_eq_factor_final))
gen d_3_eav=100*log((lag2_av*lag2_eq_factor_final)/(lag3_av*lag3_eq_factor_final))

reg  d_levy d_eav d_2_eav d_3_eav, vce(cluster n_uniqueid) 
						test d_2_eav+d_3_eav=0
						estadd scalar p_stat = r(p)
					lincom d_eav+d_2_eav+d_3_eav
						estadd scalar lagged_magnitude=r(estimate)
						estadd scalar A_upper_bound=r(estimate)+(1.645 * r(se))
						
estimates store lag_all_ols_31

					
reg d_levy d_eav d_2_eav d_3_eav i.year##i.home_rule_ind, vce(cluster n_uniqueid)
						test d_2_eav+d_3_eav=0
						estadd scalar p_stat = r(p)
					lincom d_eav+d_2_eav+d_3_eav
						estadd scalar lagged_magnitude=r(estimate)
						estadd scalar A_upper_bound=r(estimate)+(1.645 * r(se))
estimates store lag_all_ols_32

reg d_levy d_eav d_2_eav d_3_eav i.year##i.home_rule_ind i.n_uniqueid, vce(cluster n_uniqueid)
						test d_2_eav+d_3_eav=0
						estadd scalar p_stat = r(p)
					lincom d_eav+d_2_eav+d_3_eav
						estadd scalar lagged_magnitude=r(estimate)
						estadd scalar A_upper_bound=r(estimate)+(1.645 * r(se))
estimates store lag_all_ols_33

esttab lag_all_ols_31 lag_all_ols_32 lag_all_ols_33 using "OLS all agencies_lag", replace ///
	cells(b(star fmt(%9.3f)) se(par)) stats(r2 N p_stat lagged_magnitude A_upper_bound, fmt(%4.3f %5.0fc %9.3f %9.3f) labels(R-squared N p_stat point_est upper_bound)) ///
	mtitles(M1 M2 M3) keep(d_eav d_2_eav d_3_eav) ///
	postfoot("Cook county, Illinois data from 2008 through 2023. Columns 2 and 3 include two sets of year dummies. One for home rule governments" ///
	"and another for non-home rule governments, columns 3 includes unit dummies" ///
	"p_no_lag is the p value for the hypothesis that lags of the elasticity of the levy with" "respect to d_eav sum to zero. point_est is the sum of coefficient on d_eav and lags." ///
	"Upper bound is the maximum value of the summed elasticity that we fail to reject with 95 percent confidence") ///
	title(Table 10: All agencies OLS Predict levy using d_eav allowing for two lags of d_eav)
	

	
								
**#
*************************************************************************************************
* next run regressions for four different types of agencies
* and both home rule and non-home rule municipalities
* code below adapted from Claude
***************************************************************************************************		
* Loop through government types
local gov_types "Other Township HR_muni Muni School "

foreach gov in `gov_types' {
    preserve
    keep if type_2 == "`gov'"
	
regress d_levy d_eav d_2_eav d_3_eav 
						test d_2_eav+d_3_eav=0
						estadd scalar p_stat = r(p)
					lincom d_eav+d_2_eav+d_3_eav
						estadd scalar lagged_magnitude=r(estimate)
						estadd scalar lagged_p = r(p)
						estadd scalar A_upper_bound=r(estimate)+(1.645 * r(se))
estimates store lag_all_ols_34_`gov'

regress d_levy d_eav d_2_eav d_3_eav  i.year 
						test d_2_eav+d_3_eav=0
						estadd scalar p_stat = r(p)
					lincom d_eav+d_2_eav+d_3_eav
						estadd scalar lagged_magnitude=r(estimate)
						estadd scalar lagged_p = r(p)
						estadd scalar A_upper_bound=r(estimate)+(1.645 * r(se))
estimates store lag_all_ols_35_`gov'
		
regress d_levy d_eav d_2_eav d_3_eav  i.year i.n_uniqueid
						test d_2_eav+d_3_eav=0
						estadd scalar p_stat = r(p)
					lincom d_eav+d_2_eav+d_3_eav
						estadd scalar lagged_magnitude=r(estimate)
						estadd scalar lagged_p = r(p)
						estadd scalar A_upper_bound=r(estimate)+(1.645 * r(se))
estimates store lag_all_ols_36_`gov'
	
		restore
}

**************************************************************************************************
* use data on intergovernmental revenues and enrollments
*******************************************************************************************
local gov_types_B "HR_muni Muni School "

foreach gov in `gov_types_B' {
    preserve
    keep if type_2 == "`gov'"
	count 

regress d_levy d_eav d_2_eav d_3_eav d_total_ig_revenue i.year i.n_uniqueid
					test d_2_eav+d_3_eav=0
					estadd scalar p_stat = r(p)
					lincom d_eav+d_2_eav+d_3_eav
					estadd scalar lagged_magnitude=r(estimate)
					estadd scalar lagged_p = r(p)
					estadd scalar A_upper_bound=r(estimate)+(1.645 * r(se))
		
		estimates store lag_all_ols_37_`gov'
		gen byte used_in_reg_x = e(sample)

regress d_levy d_eav d_2_eav d_3_eav i.year i.n_uniqueid if used_in_reg_x==1
					test d_2_eav+d_3_eav=0
					estadd scalar p_stat = r(p)
					lincom d_eav+d_2_eav+d_3_eav
					estadd scalar lagged_magnitude=r(estimate)
					estadd scalar lagged_p = r(p)
					estadd scalar A_upper_bound=r(estimate)+(1.645 * r(se))
				estimates store lag_all_ols_38_`gov'
		
		restore
}

regress d_levy d_eav d_2_eav d_3_eav  d_enrollment i.year i.n_uniqueid if type_2=="School"
					test d_2_eav+d_3_eav=0
					estadd scalar p_stat = r(p)
					lincom d_eav+d_2_eav+d_3_eav
					estadd scalar lagged_magnitude=r(estimate)
					estadd scalar lagged_p = r(p)
					estadd scalar A_upper_bound=r(estimate)+(1.645 * r(se))
				estimates store lag_all_ols_39_School

* Create table with all results
esttab lag_all_ols_34_Other lag_all_ols_34_Township lag_all_ols_34_HR_muni lag_all_ols_34_Muni lag_all_ols_34_School using "by_type_lag_ols_regressions", replace cells(none)  ///
		stats(N A_upper_bound, fmt(%5.0fc %9.3f) labels(N upper_bound)) mtitles(Other Township HR_muni Muni School ) nolines /// 
		title(Table 11: By agency type: OLS upper bounds estimate of ε_b allowing for two lags of d_eav)
esttab lag_all_ols_35_Other lag_all_ols_35_Township lag_all_ols_35_HR_muni lag_all_ols_35_Muni lag_all_ols_35_School using "by_type_lag_ols_regressions", append cells(none) stats(A_upper_bound, fmt(%9.3f) labels(upper_bound)) ///
	nomtitles nolines nonumbers 
esttab lag_all_ols_36_Other lag_all_ols_36_Township lag_all_ols_36_HR_muni lag_all_ols_36_Muni lag_all_ols_36_School using "by_type_lag_ols_regressions", append cells(none) stats(A_upper_bound, fmt(%9.3f) labels(upper_bound)) /// 
	nomtitles nolines nonumbers
esttab blank1 blank2 lag_all_ols_37_HR_muni lag_all_ols_37_Muni lag_all_ols_37_School  using "by_type_lag_ols_regressions", append cells(none) stats(N A_upper_bound, fmt(%5.0fc %9.3f) labels(N upper_bound)) nomtitles nolines nonumbers 
esttab blank1 blank2 lag_all_ols_38_HR_muni lag_all_ols_38_Muni lag_all_ols_38_School  using "by_type_lag_ols_regressions", append cells(none) stats(A_upper_bound, fmt(%9.3f) labels(upper_bound)) nomtitles nolines nonumbers 
esttab blank1 blank2 blank3 blank4 lag_all_ols_39_School  using "by_type_lag_ols_regressions", append cells(none) stats(A_upper_bound, fmt(%9.3f) labels(upper_bound)) nomtitles nolines nonumbers ///
postfoot("Cook county, Illinois data from 2008 through 2023. Cell entries record the upper bound which is the maximum value of the elasticity"  ///
"that we fail to reject with 95 percent confidence. All estimates are derived from OLS regressions that allow for contemporaneous and two lags of changes in EAV." ///
"Row 1 is based on an ols regression with no controls. Rows 2 and 3 also include year dummies; row 3 includes unit dummies." ///
"Row 4 is based on an ols regression with the percentage change in intergovernmental revenue, year dummies and unit dummies" ///
"Row 5 is based on an ols regression using the same sample as in row 4 but omitting the change in intergovernmental revenue while including year and unit dummies" ///
"Row 6 is based on an ols regression using the same sample as in rows 4 and 5 and including the percentage change in enrollments as well as year and unit dummies" )


* Create table A8
esttab lag_all_ols_34_Other lag_all_ols_34_Township lag_all_ols_34_HR_muni lag_all_ols_34_Muni lag_all_ols_34_School using "by_type_lag_ols_regressions_appendix", replace cells(none)  ///
		stats(p_stat, fmt(%9.3f) labels(p_stat)) mtitles(Other Township HR_muni Muni School ) nolines title(Table A8: P_stats for OLS tests of lagged effects)
esttab lag_all_ols_35_Other lag_all_ols_35_Township lag_all_ols_35_HR_muni lag_all_ols_35_Muni lag_all_ols_35_School using "by_type_lag_ols_regressions_appendix", append cells(none) stats(p_stat, fmt(%9.3f) labels(p_stat)) ///
	nomtitles nolines nonumbers 
esttab lag_all_ols_36_Other lag_all_ols_36_Township lag_all_ols_36_HR_muni lag_all_ols_36_Muni lag_all_ols_36_School using "by_type_lag_ols_regressions_appendix", append cells(none) stats(p_stat, fmt(%9.3f) labels(p_stat)) /// 
	nomtitles nolines nonumbers
esttab blank1 blank2 lag_all_ols_37_HR_muni lag_all_ols_37_Muni lag_all_ols_37_School  using "by_type_lag_ols_regressions_appendix", append cells(none) stats(p_stat, fmt(%9.3fc) labels(p_stat)) nomtitles nolines nonumbers 
esttab blank1 blank2 lag_all_ols_38_HR_muni lag_all_ols_38_Muni lag_all_ols_38_School  using "by_type_lag_ols_regressions_appendix", append cells(none) stats(p_stat, fmt(%9.3f) labels(p_stat)) nomtitles nolines nonumbers 
esttab blank1 blank2 blank3 blank4 lag_all_ols_39_School  using "by_type_lag_ols_regressions_appendix", append cells(none) stats(p_stat, fmt(%9.3f) labels(p_stat)) nomtitles nolines nonumbers ///
postfoot("Cook county, Illinois data from 2008 through 2023. Cell entries record the p value for the hypothesis that the elasticity of the levy with respect lags of d_eav sum to zero"  ///
"All estimates are derived from the same OLS regressions used to estimate the upper bounds reported in Table 11." )
				

				
*******************************************************************************
* create  table A9 point estimate for the sum of lags  collabels("sum of lags")
******************************************************************************
esttab lag_all_ols_34_Other lag_all_ols_34_Township lag_all_ols_34_HR_muni lag_all_ols_34_Muni lag_all_ols_34_School ///
    using "ols_lag_regressions_appendix_point", replace cells(none)   mtitles(Other Township HR_muni Muni School) ///
    nolines title(Table A9 By agency type: OLS point estimate of ε_b allowing for 2 lags)  stats(lagged_magnitude lagged_p, fmt(%9.3f %9.3f) labels(sum_ε_b "    lagged_p")  ) 

esttab lag_all_ols_35_Other lag_all_ols_35_Township lag_all_ols_35_HR_muni lag_all_ols_35_Muni lag_all_ols_35_School using "ols_lag_regressions_appendix_point", append cells(none) nomtitles nolines nonumbers ///
	stats(lagged_magnitude lagged_p, fmt(%9.3f %9.3f) labels(sum_ε_b "   lagged_p")  )  

esttab lag_all_ols_36_Other lag_all_ols_36_Township lag_all_ols_36_HR_muni lag_all_ols_36_Muni lag_all_ols_36_School using "ols_lag_regressions_appendix_point", append cells(none) nomtitles nolines nonumbers ///
	stats(lagged_magnitude lagged_p, fmt(%9.3f %9.3f) labels(sum_ε_b "   lagged_p")  )  
	
esttab blank1 blank2 lag_all_ols_37_HR_muni lag_all_ols_37_Muni lag_all_ols_37_School using "ols_lag_regressions_appendix_point", append cells(none) nomtitles nolines nonumbers ///
	stats(lagged_magnitude lagged_p, fmt(%9.3f %9.3f) labels(sum_ε_b "   lagged_p")  )  

esttab blank1 blank2 lag_all_ols_38_HR_muni lag_all_ols_38_Muni lag_all_ols_38_School using "ols_lag_regressions_appendix_point", append cells(none) nomtitles nolines nonumbers ///
	stats(lagged_magnitude lagged_p, fmt(%9.3f %9.3f) labels(sum_ε_b "   lagged_p")  )  
	
esttab blank1 blank2 blank3 blank4 lag_all_ols_39_School using "ols_lag_regressions_appendix_point", append cells(none) nomtitles nolines nonumbers ///
	stats(lagged_magnitude lagged_p, fmt(%9.3f %9.3f) labels(sum_ε_b "   lagged_p")  )  ///
postfoot("Cook county, Illinois data from 2008 through 2023. Cell entries give the estimated sum of the coefficients d_eav, d2_eav and d3_eav and (in row below)" ///
"the p statistics for the hypothesis that those coefficients sum to zero" ///
"All estimates are derived from the same OLS regressions used to estimate the upper bounds reported in Table 11.") 


save "`levy_output'/analysis_data.dta", replace
di as result "REPLICATE RUN COMPLETED SUCCESSFULLY"
log close levy_replicate
cd "`levy_root'"
exit




