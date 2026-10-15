version 14.0
set matsize 2000
* DFM manuscript tables: updated October 14, 2026.
* Original model-by-model organization and DFM Box directories retained.
* Run this entire file: Table 1, Figure 1, Tables 2-11 and existing appendices.
* Put NTA_data_2026_10_02.csv in the existing "data from Micheal" folder.
* Required packages: estout, ivreg2, ranktest (install once with ssc install).
* All regressions use agency-clustered inference; IV uses ivreg2.
* The nonlinear instrument is the positive part of predicted EAV growth.
* Samples/coefficients follow the current R analysis. Stata finite-sample
* corrections and IV diagnostics can differ from R; retain Stata's own inference.
* Output goes to the existing DFM_AWM_aligned subfolder; reruns replace it.
set more off
* Check dependencies before reading inputs or replacing output.
foreach cmd in esttab estadd ivreg2 ranktest {
    capture which `cmd'
    if _rc {
        di as error "Missing `cmd'. Install estout, ivreg2 and ranktest from SSC."
        exit 499
    }
}
estimates clear
* ivreg2 supplies cluster-robust first-stage and conditional SW tests.
* Use matrix names (not row order); no stale r() values from unrelated tests.
capture program drop levy_iv_diagnostics
program define levy_iv_diagnostics, eclass
    version 14
    tempname first
    matrix `first' = e(first)
    ereturn scalar p_exog = e(estatp)
    local endog_terms "`e(instd)'"
    foreach term of local endog_terms {
        if missing(_se[`term']) | _se[`term']<=0 {
            di as error "Unavailable clustered SE for `term'; stopping before export."
            exit 498
        }
        if "`term'"=="d_eav" {
            ereturn scalar regF = `first'[rownumb(`first',"F"),colnumb(`first',"`term'")]
        }
        if "`term'"=="neg_d_eav" {
            ereturn scalar F_neg = `first'[rownumb(`first',"F"),colnumb(`first',"`term'")]
            ereturn scalar SW_neg = `first'[rownumb(`first',"SWF"),colnumb(`first',"`term'")]
        }
        if "`term'"=="pos_d_eav" {
            ereturn scalar F_pos = `first'[rownumb(`first',"F"),colnumb(`first',"`term'")]
            ereturn scalar SW_pos = `first'[rownumb(`first',"SWF"),colnumb(`first',"`term'")]
        }
    }
end

* Match R's complete-case and fixed-effect singleton samples.
capture program drop levy_sample
program define levy_sample
    syntax varlist(numeric) [if], [FE(varlist numeric)]
    capture drop model_sample
    marksample touse
    gen byte model_sample = `touse'
    markout model_sample n_uniqueid `fe'
    local changed=1
    while `changed' {
        local changed=0
        foreach f of local fe {
            tempvar n
            bysort `f': egen long `n'=total(model_sample)
            quietly count if model_sample & `n'==1
            if r(N)>0 {
                local changed=1
                quietly replace model_sample=0 if `n'==1
            }
            drop `n'
        }
    }
    sort n_agency_group year
end

clear
cd "C:\Users\dmerrim\Box\Super Cool RA Tree Fort & Club House\LevyEAV Elasticity\elasticity of levy\data from Micheal"
import delimited "NTA_data_2026_10_02.csv", varnames(1) case(preserve)
rename triad_name triad
sort type
merge m:1 agency_group using "C:\Users\dmerrim\Box\Super Cool RA Tree Fort & Club House\LevyEAV Elasticity\elasticity of levy\willamete university data\fips_all_agency_name.dta", keep(master match)
list agency_group year agency_name if _merge==1
drop if _merge==1
sort fipsid year
list agency_group agency_name if fipsid==.   
drop if fipsid==.
merge m:1 fipsid year using "C:\Users\dmerrim\Box\Super Cool RA Tree Fort & Club House\LevyEAV Elasticity\elasticity of levy\data from census\census_data.dta", generate(merge_2) keep(master match)
tab merge_2 year, missing
list name if merge_2==1 & year==2012
list name year agency_group if agency_group=="School District 89" | agency_group=="School District 88" 
tab type if year==2021

*********************************************************
* census of government years are 2007, 2012 and 2017
* in 2007 and 2012 we match everything in the master list
*  in 2012 we miss 2 units of government that have "name" missing
***************************************************************

sort year
tab year
***********************************************************************
* merge in equalization factors (multipliers) so that I can transform AV to EAV
*
*clear
*sort year
*tab year
*keep if year>=2006
*exit
***************************************************************************************
* Read the equalization-factor Stata dataset used by DFM.
merge m:1 year using "C:\Users\dmerrim\Box\Super Cool RA Tree Fort & Club House\LevyEAV Elasticity\elasticity of levy\post nta\eq_factor", generate(_merge2) keep(master match)
foreach v in av total_ig_revenue enrollment eq_factor_final {
    capture confirm numeric variable `v'
    if _rc destring `v', replace ignore("NA,")
}
desc total_final_levy
*******************************************************************
* there are some "NA" values of total_final_levy so it is imported as a string
***************************************************************


local s_vars "total_final_levy"
        foreach x of local s_vars {
		capture confirm numeric variable `x'
        if !_rc {
            generate double n_`x' = `x'
        }
        else {
            destring `x', gen(n_`x') ignore("NA")
        } 
                        }
						
*******************************************************
* moving this statement down  about 20 lines
*****************************************************						

*drop if year<2008

*********************************************************************
* note that FMV depeds on future values 
* we should not lose observations early in the data set because of missing values of FMV
*  we do lose observations later in the data set because we don't have future av values in reassessment years for 
* some observations after 2020
***********************************************************

**************************************************************************************
* below I caculate EstV for each agency in each year using equations presented in 
* 
*Assuming constant growth rate over the period between re-assessments
* What is the appropriate formula to calculate the value estimated market value (EstV) on January 1 of each year:
*
* Let V(t) equal the value on December 31 of year t, X(t+1) be the value on January 1 of year t+1 and r be the (constant) annual rate of return between January 1 of year (t+1) and January 1 of year (t+4). then
*
*	V[2009]=X[2010]	(1)
*	V[2010]=(1+r)X[2010]	(2)
*	V[2011]=(1+r)^2 X[2010]	(3)
*	V[2012]=(1+r)^3 X[2010]=X[2013]		(4)
*
* From equation (4) we know that:
*
*	(1+r)^3 X[2010]=X[2013]⇒(1+r)^3=(X[2013])/(X[2010])	(5)
*So
*	r=((X[2013])/(X[2010]))^((1/3) )-1	(6)
*
*
*******************************************************************************
*********************************************************
* first I have to xtset the data
********************************************************
tab agency_group
encode agency_group, gen(n_agency_group)
sort n_agency_group year

*preservelis
count
drop if n_agency_group==.
count
isid n_agency_group year
by n_agency_group: assert year==year[_n-1]+1 if _n>1
xtset n_agency_group year
**************************************************************
* create a few variables I will need later
************************************************************
gen double lag_av=l1.av
gen double lag2_av=l2.av
gen double lag3_av=l3.av

gen double lag_eq_factor_final=l1.eq_factor_final
gen double lag2_eq_factor_final=l2.eq_factor_final
gen double lag3_eq_factor_final=l3.eq_factor_final
**************************************************************
**# Bookmark #2
* back to the main program see section on lagged effects to sse 
* how variables created above are used.
************************************************************

drop if year<2008

gen t=0
replace t=1 if reassess_year==1
replace t=2 if l1.reassess_year==1
replace t=3 if l2.reassess_year==1
tab t reassess_year, missing
tab year if t==0

capture drop r
gen r=0
replace r=((f3.av/av)^(1/3))-1 if t==1 /* note f3.av is a three year lead of av*/
replace r=((f2.av/l1.av)^(1/3))-1 if t==2
replace r=((f1.av/l2.av)^(1/3))-1 if t==3

*******************************************************
* note r is constant within each assessment cycle
* check to make sure the numbers look right
***************************************************************
list year agency_group av r t reassess_year in 100/112 
gen EstV=0
replace EstV=av if t==1
replace EstV=(l1.av)*(1+r) if t==2 
replace EstV=(l2.av)*(1+r)^2 if t==3
gen ln_EstV=ln(EstV)

tab year reassess_year if triad=="City" & reassess_year==1
tab year reassess_year if triad=="North" & reassess_year==1
tab year reassess_year if triad=="South" & reassess_year==1



********************************************************************
* reassessment years
* city 2009,2012, 2015,2018, 2021
* north 2010, 2013, 2016, 2019, 2022
* south 2008, 2011, 2014, 2017, 2020, 2023
*****************************************************************
xtdescribe
tab type
tab year

gen double d_av=100*(log(av/l1.av))
gen double d_eav=100*log((av*eq_factor_final)/(l1.av*l1.eq_factor_final))
gen double d_levy=100*(log(n_total_final_levy/l1.n_total_final_levy))

***************************************************************
* checking instrument relevance: reassessment year predicts EAV growth
***************************************************************
encode(uniqueid), gen(n_uniqueid)
egen long year_hr_a1 = group(year home_rule_ind)
gen byte home_rule_for_fe = cond(missing(home_rule_ind),0,home_rule_ind)
egen long year_hr = group(year home_rule_for_fe)
tab year
levy_sample d_eav reassess_year, fe(year_hr_a1 n_uniqueid)
regress d_eav reassess_year i.year##i.home_rule_ind i.n_uniqueid if model_sample, vce(cluster n_uniqueid)
	estimates store all_instrument
levy_sample d_eav reassess_year if type=="Muni", fe(year n_uniqueid)
regress d_eav reassess_year i.year i.n_uniqueid if model_sample, vce(cluster n_uniqueid)
	estimates store muni_instrument
levy_sample d_eav reassess_year if type=="Other", fe(year n_uniqueid)
regress d_eav reassess_year i.year i.n_uniqueid if model_sample, vce(cluster n_uniqueid)
	estimates store other_instrument
levy_sample d_eav reassess_year if type=="School", fe(year n_uniqueid)
regress d_eav reassess_year i.year i.n_uniqueid if model_sample, vce(cluster n_uniqueid)
	estimates store school_instrument
levy_sample d_eav reassess_year if type=="Township", fe(year n_uniqueid)
regress d_eav reassess_year i.year i.n_uniqueid if model_sample, vce(cluster n_uniqueid)
	estimates store township_instrument

cap mkdir "C:\Users\dmerrim\Box\Cook County Property Tax Project 2023\Folder Fiefdoms\merriman scribbles\post nta\output\DFM_AWM_aligned"
cd "C:\Users\dmerrim\Box\Cook County Property Tax Project 2023\Folder Fiefdoms\merriman scribbles\post nta\output\DFM_AWM_aligned"
esttab all_instrument muni_instrument other_instrument school_instrument township_instrument  using "instrument validity checks", replace ///
	star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f)) se(par)) stats(F r2 N, fmt(%4.3f %4.3f %9.0g) labels(F R-squared N)) ///
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
gen double d_total_ig_revenue=100*(log(total_ig_revenue/l1.total_ig_revenue))
gen double d_enrollment=100*(log(enrollment/l1.enrollment))
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

* Table 1 and Figure 1: generated here from the same analysis panel.
local outdir "."
preserve
confirm variable agency_group year type home_rule_ind d_levy d_eav
isid agency_group year
keep if inrange(year,2009,2023)
assert inlist(type,"Muni","Other","School","Township")
tempfile panel table_data figure_audit
save `panel'

* Table 1: no +/-20 trimming and no joint complete-case restriction.
tempname tabpost
postfile `tabpost' str12 agency str8 variable long N double(p5 p25 p50 p75 p95) using `table_data'
foreach group in Muni Other School Township All {
    local condition "1"
    if "`group'"!="All" local condition `"type=="`group'""'
    foreach v in d_levy d_eav {
        quietly count if `condition' & !missing(`v')
        if r(N)==0 {
            post `tabpost' ("`group'") ("`v'") (0) (.) (.) (.) (.) (.)
        }
        else {
            local nvalues=r(N)
            * R quantile(type=7): linear interpolation at h=1+(N-1)*p.
            tempvar selected
            gen byte `selected'=(`condition') & !missing(`v')
            gsort -`selected' `v'
            foreach p in 5 25 50 75 95 {
                local h=1+(`nvalues'-1)*`p'/100
                local lo=floor(`h')
                local hi=ceil(`h')
                local q`p'=`v'[`lo']+(`h'-`lo')*(`v'[`hi']-`v'[`lo'])
            }
            post `tabpost' ("`group'") ("`v'") (`nvalues') (`q5') (`q25') (`q50') (`q75') (`q95')
            drop `selected'

        }
    }
}
postclose `tabpost'
use `table_data', clear
assert _N==10
export delimited using `"`outdir'/v22_table_01_distribution.csv"', replace
tempname html
file open `html' using `"`outdir'/v22_table_01_distribution.html"', write replace text
file write `html' "<!doctype html><html><head><meta charset='utf-8'><title>Table 1</title></head><body>" _n
file write `html' "<h2>Table 1. Distribution of annual percentage changes in levies and EAV</h2><table border='1'><tr><th>Agency</th><th>Variable</th><th>N</th><th>p5</th><th>p25</th><th>p50</th><th>p75</th><th>p95</th></tr>" _n
forvalues i=1/10 {
    file write `html' "<tr><td>" (agency[`i']) "</td><td>" (variable[`i']) "</td><td>" %12.0fc (N[`i']) "</td>"
    foreach v in p5 p25 p50 p75 p95 {
        file write `html' "<td>" %9.1f (`v'[`i']) "</td>"
    }
    file write `html' "</tr>" _n
}
file write `html' "</table><p>2009-2023. Changes are 100 times log ratios. N counts nonmissing agency-years separately for each variable. Percentiles use the untrimmed sample and R type-7 interpolation.</p></body></html>" _n
file close `html'

* Figure 1: untrimmed complete-pair annual agency counts; trimmed plotted N.
use `panel', clear
tempname figpost
postfile `figpost' str24 panel long annual_min annual_max plotted_N unique_agencies double(intercept slope) using `figure_audit'
local graphs ""
local clean_graphs ""
forvalues g=1/6 {
    local condition "1"
    local title "All local gov't types"
    if `g'==2 {
        local condition `"type=="Muni""'
        local title "Muni"
    }
    if `g'==3 {
        local condition `"type=="Muni" & home_rule_ind==1"'
        local title "Home Rule Muni"
    }
    if `g'>=4 {
        local title : word `=`g'-3' of Other School Township
        local condition `"type=="`title'""'
    }
    local nmin=.
    local nmax=0
    forvalues y=2009/2023 {
        quietly count if (`condition') & year==`y' & !missing(d_levy,d_eav)
        local nmin=min(`nmin',r(N))
        local nmax=max(`nmax',r(N))
    }
    local plotted "(`condition') & inrange(d_eav,-20,20) & inrange(d_levy,-20,20)"
    tempvar agency_tag
    egen byte `agency_tag'=tag(agency_group) if `plotted'
    quietly count if `agency_tag'==1
    local nunique=r(N)
    drop `agency_tag'
    local subtitle "N = `nunique' unique agencies"
    quietly regress d_levy d_eav if `plotted'
    post `figpost' ("`title'") (`nmin') (`nmax') (e(N)) (`nunique') (_b[_cons]) (_b[d_eav])
    tempname plot
    twoway (scatter d_levy d_eav if `plotted', mcolor(blue) msize(tiny)) ///
        (lfit d_levy d_eav if `plotted', lcolor(red) range(-20 20)) ///
        (function y=x, range(-20 20) lcolor(olive)), ///
        title("`title'") subtitle("`subtitle'", size(small)) ///
        xtitle("% change EAV") ytitle("% change levy") ///
        xlabel(-20(10)20, grid) ylabel(-20(10)20, grid) legend(off) ///
        graphregion(color(white)) name(`plot', replace) nodraw
    local graphs "`graphs' `plot'"
    local clean_title "`title'"
    if `g'==1 local clean_title "All agencies"
    if `g'==2 local clean_title "Municipalities"
    if `g'==3 local clean_title "Home-rule municipalities"
    if `g'==4 local clean_title "Other districts"
    if `g'==5 local clean_title "Schools"
    if `g'==6 local clean_title "Townships"
    tempname clean_plot
    twoway (function y=x, range(-20 20) lcolor(gs10) lpattern(longdash)) ///
        (scatter d_levy d_eav if `plotted', mcolor(teal) msize(tiny)) ///
        (lfit d_levy d_eav if `plotted', lcolor(cranberry) range(-20 20)), ///
        title("`clean_title'", size(medsmall)) subtitle("`subtitle'", size(small)) ///
        xtitle("Annual change in EAV (%)", size(small)) ytitle("Annual change in levy (%)", size(small)) ///
        xlabel(-20(10)20, grid) ylabel(-20(10)20, grid) legend(off) aspectratio(1) ///
        graphregion(color(white)) plotregion(color(white)) name(`clean_plot', replace) nodraw
    local clean_graphs "`clean_graphs' `clean_plot'"

}
postclose `figpost'
tempname combined
graph combine `graphs', cols(3) xcommon ycommon ///
    title("Observed % change levy wrt % change in assessments" "No controls: 2009 through 2023", size(medium)) ///
    note("Changes are 100 times log ratios. Both coordinates restricted to [-20,20]." ///
    "Red: OLS fitted on the displayed sample. Brown: levy growth equals EAV growth." ///
    "Above/below the diagonal: the levy/EAV ratio rises/falls. N counts unique agencies after trimming.", size(vsmall)) ///
    graphregion(color(white)) xsize(13) ysize(8) name(`combined', replace) nodraw
graph display `combined'
graph export `"`outdir'/v22_figure_01_observed_changes.png"', name(`combined') width(3900) replace
graph export `"`outdir'/v22_figure_01_observed_changes.pdf"', name(`combined') replace
tempname clean_combined
graph combine `clean_graphs', cols(3) xcommon ycommon graphregion(color(white)) ///
    xsize(12) ysize(8.5) name(`clean_combined', replace) nodraw
graph display `clean_combined'
graph export `"`outdir'/v22_figure_01_observed_changes_clean.png"', name(`clean_combined') width(3600) replace
graph export `"`outdir'/v22_figure_01_observed_changes_clean.pdf"', name(`clean_combined') replace
graph drop `graphs' `clean_graphs' `combined' `clean_combined'

use `figure_audit', clear
export delimited using `"`outdir'/v22_figure_01_counts.csv"', replace
restore


levy_sample d_levy d_eav
reg d_levy d_eav if model_sample, vce(cluster n_uniqueid) 
     estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
	estimates store all_ols_1
levy_sample d_levy d_eav, fe(year_hr)
reg d_levy d_eav i.year##i.home_rule_for_fe if model_sample, vce(cluster n_uniqueid)
    estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
	estimates store all_ols_2
levy_sample d_levy d_eav, fe(year_hr n_uniqueid)
reg d_levy d_eav i.year##i.home_rule_for_fe i.n_uniqueid if model_sample, vce(cluster n_uniqueid)
   estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
	estimates store all_ols_3

	
	esttab all_ols_1 all_ols_2 all_ols_3 using "OLS all agencies", replace ///
	star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f)) se(par)) stats(r2 N upper_bound, fmt(%4.3f %9.0g %9.3f) labels(R-squared)) ///
	varlabels(_cons Constant)  ///
	mtitles(M1 M2 M3) keep(d_eav) ///
	postfoot("Cook county, Illinois data from 2008 through 2023" "Columns 2 and 3 include two sets of year dummies. One for home rule governments" "and another for non-home rule governments, columns 3 includes unit dummies") ///
	title(Table 2: All agencies OLS Predict levy using d_eav)

*******************************************************************************
* IV inference: partial out nuisance FE to avoid singular dummy-variable VCE.
* ivreg2 reports clustered first-stage, endogeneity and identification tests.
* Do not use homoskedastic Stock-Yogo cutoffs for clustered KP/SW statistics.
******************************************************************************

levy_sample d_levy d_eav reassess_year
ivreg2 d_levy (d_eav = reassess_year) if model_sample, cluster(n_uniqueid) small partial(_cons) endog(d_eav) ffirst
levy_iv_diagnostics
	estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
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
* Interpret diagnostics from the current run; historical values are not reused.

levy_sample d_levy d_eav reassess_year, fe(year_hr)
ivreg2  d_levy i.year##i.home_rule_for_fe (d_eav = reassess_year) if model_sample, cluster(n_uniqueid) small partial(i.year##i.home_rule_for_fe) endog(d_eav) ffirst
levy_iv_diagnostics
	estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
	estimates store all_iv_2

*drop if triad=="City"
* i.n_agency_group is very similiar to i.n_uniqueid
display "estimate r2= " (1-(992006.71/(992006.71+39904.284)))

levy_sample d_levy d_eav reassess_year, fe(year_hr n_uniqueid)
ivreg2  d_levy i.year##i.home_rule_for_fe  i.n_uniqueid (d_eav = reassess_year) if model_sample, cluster(n_uniqueid) small partial(i.year##i.home_rule_for_fe i.n_uniqueid) endog(d_eav) ffirst
levy_iv_diagnostics
	estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
	estimates store all_iv_3
	
esttab all_iv_1 all_iv_2 all_iv_3 using "IV all agencies", replace stats(N upper_bound p_exog regF, fmt(%9.0g %9.3f %9.3f %9.1f) labels(N)) star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f)) se(par)) ///
keep(d_eav) postfoot("Cook county, Illinois data from 2008 through 2023" "Columns 2 and 3 include two sets of year dummies. One for home rule governments" "and another for non-home rule governments, columns 3 includes unit dummies" ///
"p_exog is ivreg2's clustered endogeneity C-test p value." "regF is Stata's reported first-stage statistic; it need not numerically equal fixest's clustered diagnostic.") title(Table 3: All agencies IV Predict levy using d_eav)


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
replace type_2="NonHR_muni" if type=="Muni" & home_rule_ind==0

sort type_2
by type_2: count
*by type_2: count if d_total_ig_revenue~=.
*list agency_group total_ig_revenue d_total_ig_revenue if type_2=="Other" & d_total_ig_revenue~=. & total_ig_revenue>2000

count if type_2=="Other"

 
* Loop through government types
tab type_2

local gov_types "Other Township HR_muni NonHR_muni School "

foreach gov in `gov_types' {
    preserve
    keep if type_2 == "`gov'"
	count 
 
    * Run and store regressions
levy_sample d_levy d_eav
   regress d_levy d_eav if model_sample, vce(cluster n_uniqueid)
		estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
		estadd scalar point_est=_b[d_eav]
		estimates store reg1_`gov'
		
levy_sample d_levy d_eav, fe(year)
   regress d_levy d_eav i.year if model_sample, vce(cluster n_uniqueid)
		estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
		estadd scalar point_est=_b[d_eav]
		estimates store reg2_`gov'	
		
levy_sample d_levy d_eav, fe(year n_uniqueid)
	regress d_levy d_eav i.year i.n_uniqueid if model_sample, vce(cluster n_uniqueid)
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
local gov_types_B "HR_muni NonHR_muni School "

foreach gov in `gov_types_B' {
    preserve
    keep if type_2 == "`gov'"
	count 

levy_sample d_levy d_eav d_total_ig_revenue, fe(year n_uniqueid)
regress d_levy d_eav d_total_ig_revenue i.year i.n_uniqueid if model_sample, vce(cluster n_uniqueid)
		estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
		estadd scalar point_est=_b[d_eav]
		estimates store reg4_`gov'
	gen byte used_in_reg_x = e(sample)

levy_sample d_levy d_eav if used_in_reg_x==1, fe(year n_uniqueid)
regress d_levy d_eav i.year i.n_uniqueid if model_sample, vce(cluster n_uniqueid)
		estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
		estadd scalar point_est=_b[d_eav]
		estimates store reg5_`gov'
		
		restore
}

levy_sample d_levy d_eav d_enrollment if type_2=="School", fe(year n_uniqueid)
regress d_levy d_eav d_enrollment i.year i.n_uniqueid if model_sample, vce(cluster n_uniqueid)
estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
estimates store reg6_school

esttab reg1_Other reg1_Township reg1_HR_muni reg1_NonHR_muni reg1_School using "ols_regressions", replace star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(N upper_bound, fmt(%5.0fc %9.3f)) mtitles(Other Township HR_muni NonHR_muni School ) ///
 nolines title(Table 4 By agency type: OLS upper bounds estimate of ε_b )
esttab reg2_Other reg2_Township reg2_HR_muni reg2_NonHR_muni reg2_School  using "ols_regressions", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(upper_bound, fmt(%9.3f))  nomtitles nolines nonumbers
esttab reg3_Other reg3_Township reg3_HR_muni reg3_NonHR_muni reg3_School  using "ols_regressions", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(upper_bound, fmt(%9.3f))  nomtitles nolines nonumbers
esttab blank1 blank2 reg4_HR_muni reg4_NonHR_muni reg4_School  using "ols_regressions", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(N upper_bound, fmt(%5.0fc %9.3f)) nomtitles nolines nonumbers
esttab blank1 blank2 reg5_HR_muni reg5_NonHR_muni reg5_School  using "ols_regressions", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(N upper_bound, fmt(%5.0fc %9.3f)) nomtitles nolines nonumbers
esttab blank1 blank2 blank3 blank4 reg6_school  using "ols_regressions", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(N upper_bound, fmt(%5.0fc %9.3f)) nomtitles nolines nonumbers ///
postfoot("Cook county, Illinois data from 2008 through 2023. Cell entries record the upper bound which is the maximum value of the elasticity"  ///
"that we fail to reject with 95 percent confidence. N is the number of non-missing observations in rows immediately below. Row 1 is based on " ///
"an ols regression on d_eav with no controls. Rows 2 and 3 are based on OLS regressions that also include year dummies; row 3 includes unit dummies." ///
"Row 4 is based on an OLS regression that include controls for the percentage change in intergovernmental revenues, year and unit dummies." ///
"Row 5 uses the same sample as in row 4 (i.e. it drops observations with missing value for the percentage change in intergovernmental revenues)" ///
"but the regression does not control for the percentage change in intergovernmental revenues. Row 6 is based on an OLS regression that includes" ///
"controls for the percentage change in enrollment, year and unit dummies.")


esttab reg1_Other reg1_Township reg1_HR_muni reg1_NonHR_muni reg1_School ///
    using "ols_regressions_appendix_point", replace star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f))) collabels("b") noobs noconstant keep(d_eav) mtitles(Other Township HR_muni NonHR_muni School) ///
    nolines title(Table A2 By agency type: OLS point estimate of ε_b)

esttab reg2_Other reg2_Township reg2_HR_muni reg2_NonHR_muni reg2_School using "ols_regressions_appendix_point", append star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f))) noobs noconstant keep(d_eav) ///
    nomtitles nolines nonumbers  collabels(none)

esttab reg3_Other reg3_Township reg3_HR_muni reg3_NonHR_muni reg3_School using "ols_regressions_appendix_point", append star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f))) noobs noconstant keep(d_eav) ///
    nomtitles nolines nonumbers  collabels(none)

esttab blank1 blank2 reg4_HR_muni reg4_NonHR_muni reg4_School using "ols_regressions_appendix_point", append star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f))) noobs noconstant keep(d_eav) ///
    nomtitles nolines nonumbers  collabels(none)

esttab blank1 blank2 reg5_HR_muni reg5_NonHR_muni reg5_School using "ols_regressions_appendix_point", append star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f))) noobs noconstant keep(d_eav) ///
    nomtitles nolines nonumbers  collabels(none)

esttab blank1 blank2 blank3 blank4 reg6_school using "ols_regressions_appendix_point", append star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f))) noobs noconstant keep(d_eav) ///
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
levy_sample d_levy d_eav reassess_year
ivreg2 d_levy (d_eav = reassess_year) if model_sample, cluster(n_uniqueid) small partial(_cons) endog(d_eav) ffirst
levy_iv_diagnostics
		estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
		estimates store ivreg7_`gov'
		
levy_sample d_levy d_eav reassess_year, fe(year)
ivreg2 d_levy i.year (d_eav = reassess_year) if model_sample, cluster(n_uniqueid) small partial(i.year) endog(d_eav) ffirst
levy_iv_diagnostics
		estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
		estimates store ivreg8_`gov'
		
levy_sample d_levy d_eav reassess_year, fe(year n_uniqueid)
ivreg2 d_levy i.year i.n_uniqueid (d_eav = reassess_year) if model_sample, cluster(n_uniqueid) small partial(i.year i.n_uniqueid) endog(d_eav) ffirst
levy_iv_diagnostics
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
local gov_types_B "HR_muni NonHR_muni School "

foreach gov in `gov_types_B' {
    preserve
    keep if type_2 == "`gov'"
	count 

levy_sample d_levy d_total_ig_revenue d_eav reassess_year, fe(year n_uniqueid)
ivreg2 d_levy d_total_ig_revenue i.year i.n_uniqueid (d_eav = reassess_year) if model_sample, cluster(n_uniqueid) small partial(i.year i.n_uniqueid) endog(d_eav) ffirst
levy_iv_diagnostics
		estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
		estimates store ivreg10_`gov'
	gen byte used_in_reg_x = e(sample)

levy_sample d_levy d_eav reassess_year if used_in_reg_x==1, fe(year n_uniqueid)
ivreg2 d_levy i.year i.n_uniqueid (d_eav = reassess_year) if model_sample, cluster(n_uniqueid) small partial(i.year i.n_uniqueid) endog(d_eav) ffirst
levy_iv_diagnostics
		estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
		estimates store ivreg11_`gov'
		
		restore
}

levy_sample d_levy d_enrollment d_eav reassess_year if type_2=="School", fe(year n_uniqueid)
ivreg2 d_levy d_enrollment i.year i.n_uniqueid (d_eav = reassess_year) if model_sample, cluster(n_uniqueid) small partial(i.year i.n_uniqueid) endog(d_eav) ffirst
levy_iv_diagnostics
	estadd scalar upper_bound=_b[d_eav]+(1.645 * _se[d_eav])
	estimates store ivreg12_School
	
esttab ivreg7_Other ivreg7_Township ivreg7_HR_muni ivreg7_NonHR_muni ivreg7_School using "iv_regressions", replace star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(N upper_bound, fmt(%5.0fc %9.3f)) mtitles(Other Township HR_muni NonHR_muni School ) ///
nolines title(Table 5: By agency type: IV upper bounds estimate of ε_b)
esttab ivreg8_Other ivreg8_Township ivreg8_HR_muni ivreg8_NonHR_muni ivreg8_School  using "iv_regressions", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(upper_bound, fmt(%9.3f))  nomtitles nolines nonumbers
esttab ivreg9_Other ivreg9_Township ivreg9_HR_muni ivreg9_NonHR_muni ivreg9_School  using "iv_regressions", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(upper_bound, fmt(%9.3f))  nomtitles nolines nonumbers
esttab blank1 blank2 ivreg10_HR_muni ivreg10_NonHR_muni ivreg10_School  using "iv_regressions", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(N upper_bound, fmt(%5.0fc %9.3f)) nomtitles nolines nonumbers
esttab blank1 blank2 ivreg11_HR_muni ivreg11_NonHR_muni ivreg11_School  using "iv_regressions", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(N upper_bound, fmt(%5.0fc %9.3f)) nomtitles nolines nonumbers 
esttab blank1 blank2 blank3 blank4 ivreg12_School  using "iv_regressions", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(N upper_bound, fmt(%5.0fc %9.3f)) nomtitles nolines nonumbers ///
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

esttab ivreg7_Other ivreg7_Township ivreg7_HR_muni ivreg7_NonHR_muni ivreg7_School ///
    using "iv_regressions_appendix_point", replace star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f))) collabels("b") noobs noconstant keep(d_eav) mtitles(Other Township HR_muni NonHR_muni School) ///
    nolines title(Table A3 By agency type: IV point estimate of ε_b)
esttab ivreg8_Other ivreg8_Township ivreg8_HR_muni ivreg8_NonHR_muni ivreg8_School using "iv_regressions_appendix_point", append star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) noobs noconstant keep(d_eav) ///
    nomtitles nolines nonumbers  collabels(none)
esttab ivreg9_Other ivreg9_Township ivreg9_HR_muni ivreg9_NonHR_muni ivreg9_School using "iv_regressions_appendix_point", append star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) noobs noconstant keep(d_eav) ///
    nomtitles nolines nonumbers  collabels(none)
esttab blank1 blank2 ivreg10_HR_muni ivreg10_NonHR_muni ivreg10_School using "iv_regressions_appendix_point", append star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) noobs noconstant keep(d_eav) ///
    nomtitles nolines nonumbers  collabels(none)
esttab blank1 blank2 ivreg11_HR_muni ivreg11_NonHR_muni ivreg11_School using "iv_regressions_appendix_point", append star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) noobs noconstant keep(d_eav) ///
    nomtitles nolines nonumbers  collabels(none)
esttab blank1 blank2 blank3 blank4 ivreg12_School using "iv_regressions_appendix_point", append star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) noobs noconstant keep(d_eav) ///
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
gen double pos_d_eav=d_eav*eav_growth
gen double neg_d_eav=d_eav*(1-eav_growth)

levy_sample d_levy d_eav
reg d_levy d_eav if model_sample, vce(cluster n_uniqueid)
levy_sample d_levy d_eav pos_d_eav
reg d_levy d_eav pos_d_eav if model_sample, vce(cluster n_uniqueid)
levy_sample d_levy pos_d_eav neg_d_eav
reg d_levy pos_d_eav neg_d_eav if model_sample, vce(cluster n_uniqueid)

test pos_d_eav=neg_d_eav

*********************************************************************
*   run regressions for all governments allowing for asymmetry
* OLS is equation 5 with betas, gammas and lambda's assumed equal to zero
* 2SLS is equation 6 with betas, gammas and lambda's assumed equal to zero
*********************************************************************************
drop if year<=2008


levy_sample d_levy pos_d_eav neg_d_eav
reg d_levy pos_d_eav neg_d_eav if model_sample, vce(cluster n_uniqueid)
		test neg_d_eav = pos_d_eav
		estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
		estadd scalar p_equal = r(p)
		estimates store reg_13
levy_sample d_levy pos_d_eav neg_d_eav, fe(year_hr)
reg d_levy pos_d_eav neg_d_eav i.year##i.home_rule_for_fe if model_sample, vce(cluster n_uniqueid)
		test neg_d_eav = pos_d_eav
		estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
		estadd scalar p_equal = r(p)
		estimates store reg_14
levy_sample d_levy pos_d_eav neg_d_eav, fe(year_hr n_uniqueid)
reg d_levy pos_d_eav neg_d_eav i.year##i.home_rule_for_fe i.n_uniqueid if model_sample, vce(cluster n_uniqueid)
		test neg_d_eav = pos_d_eav
		estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
		estadd scalar p_equal = r(p)
		estimates store reg_15

	esttab reg_13 reg_14 reg_15 using "OLS all agencies_asym", replace ///
	star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f)) se(par)) stats(r2 N p_equal upper_bound, fmt(%4.3f %9.0fc %4.3f %9.3f) labels(R-squared)) ///
	mtitles(M1 M2 M3) keep(pos_d_eav neg_d_eav) ///
	postfoot("Cook county, Illinois data from 2008 through 2023. Columns 2 and 3 include two sets of year dummies. One for home rule governments" "and another for non-home rule governments, columns 3 includes unit dummies" ///
	"p_equal is the p value for the hypothesis that the elasticity of the levy with respect" "to d_eav is symmetric. Upper bound is the maximum value of the elasticity" ///
	"that we fail to reject with 95 percent confidence") title(Table 6: All agencies OLS Predict levy using d_eav allowing for asymmetry)

	
*******************************************************************************
* next do ivregress allowing for asymmetry
******************************************************************************	
***************************************************************************************************		
*  since the interaction effect (positive d_eav*eav) I need to create an instrument for the interaction effect as well as for d_eav
*  to do this I am following the suggestion from Tirthankar Chakravarty from Dec 21, 2011
* I found on statalist at https://www.stata.com/statalist/archive/2012-01/msg00877.html#:~:text=Hi%20all%2C%20The%20described%20IV,in%20the%20first%20step%202.
***************************************************************************************************************************
* v7.4.1: generate the excluded nonlinear instrument from the positive part
* of predicted d_eav. DFM base used pos_d_eav*d_eav_hat; do not restore it.
* This auxiliary first stage follows R's year-FE construction. Its fitted
* values are used only to form the generated instrument; see diagnostic note.
levy_sample d_eav reassess_year, fe(year)
reg d_eav reassess_year i.year if model_sample, vce(cluster n_uniqueid)
predict double d_eav_hat if e(sample), xb
gen double pos_d_eav_hat = cond(d_eav_hat>0,d_eav_hat,0) if !missing(d_eav_hat)

levy_sample d_levy d_eav pos_d_eav reassess_year pos_d_eav_hat
ivreg2 d_levy (d_eav pos_d_eav= reassess_year pos_d_eav_hat) if model_sample, cluster(n_uniqueid) small partial(_cons) endog(d_eav pos_d_eav) ffirst
levy_iv_diagnostics

levy_sample d_levy neg_d_eav pos_d_eav reassess_year pos_d_eav_hat
ivreg2 d_levy (neg_d_eav pos_d_eav= reassess_year pos_d_eav_hat) if model_sample, cluster(n_uniqueid) small partial(_cons) endog(neg_d_eav pos_d_eav) ffirst
levy_iv_diagnostics
 		test neg_d_eav = pos_d_eav
		estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
		estadd scalar p_equal = r(p)
		estimates store reg_16	

*********************************************
* I have two endogenous variables.
*  Fstatistics on the first stage regressions are saved
*  these tell me about the strength of my instruments
***************************************************************

levy_sample d_levy neg_d_eav pos_d_eav reassess_year pos_d_eav_hat, fe(year_hr)
ivreg2  d_levy i.year##i.home_rule_for_fe  (neg_d_eav pos_d_eav= reassess_year pos_d_eav_hat) if model_sample, cluster(n_uniqueid) small partial(i.year##i.home_rule_for_fe) endog(neg_d_eav pos_d_eav) ffirst
levy_iv_diagnostics
		test neg_d_eav = pos_d_eav
		estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
		estadd scalar p_equal = r(p)
		estimates store reg_17
levy_sample d_levy neg_d_eav pos_d_eav reassess_year pos_d_eav_hat, fe(year_hr n_uniqueid)
ivreg2  d_levy  i.year##i.home_rule_for_fe  i.n_uniqueid (neg_d_eav pos_d_eav= reassess_year pos_d_eav_hat) if model_sample, cluster(n_uniqueid) small partial(i.year##i.home_rule_for_fe i.n_uniqueid) endog(neg_d_eav pos_d_eav) ffirst
levy_iv_diagnostics
		test neg_d_eav = pos_d_eav
		estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
		estadd scalar p_equal = r(p)
		estimates store reg_18
		
esttab reg_16 reg_17 reg_18 using "iv all agencies_asym", replace ///
	star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f)) se(par)) stats(F_neg F_pos SW_neg SW_pos N p_exog p_equal upper_bound, fmt(%9.1fc %9.1fc %9.1fc %9.1fc %5.0fc %9.3f %9.3f %9.3f))  ///
	mtitles(M1 M2 M3) keep(pos_d_eav neg_d_eav) order(pos_d_eav neg_d_eav) ///
	postfoot("Cook county, Illinois data from 2008 through 2023. Columns 2 and 3 include two sets of year dummies. One for home rule governments" "and another for non-home rule governments, columns 3 includes unit dummies" ///
	"SW_neg and SW_pos are conditional first-stage F statistics; p_exog is the clustered endogeneity C-test." "F_pos and F_neg are first stage F statistics for corresponding d_eav variables. p_equal is the p value for the hypothesis that the elasticity" ///
	"of the levy with respect to d_eav is symmetric. Upper bound is the maximum value of the elasticity that we fail to reject with 95 percent confidence") ///
	title(Table 7: All agencies IV Predict levy using d_eav allowing for asymmetry)

*************************************************************************************************
* next run regressions for four different types of agencies
* and both home rule and non-home rule municipalities
* code below adapted from Claude
***************************************************************************************************		
* Loop through government types
local gov_types "Other Township HR_muni NonHR_muni School "

foreach gov in `gov_types' {
    preserve
    keep if type_2 == "`gov'"
    
    * Run and store regressions
    eststo reg19_`gov': reg d_levy neg_d_eav pos_d_eav, vce(cluster n_uniqueid)
		test neg_d_eav = pos_d_eav
		estadd scalar p_equal = r(p)
		estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
    eststo reg20_`gov': reg d_levy neg_d_eav pos_d_eav i.year, vce(cluster n_uniqueid)
		test neg_d_eav = pos_d_eav
		estadd scalar p_equal = r(p)
		estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
    eststo reg21_`gov': reg d_levy neg_d_eav pos_d_eav i.year i.n_uniqueid, vce(cluster n_uniqueid)
		test neg_d_eav = pos_d_eav
		estadd scalar p_equal = r(p)
		estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
		
    restore  
}


**************************************************************************************************
* use data on intergovernmental revenues and enrollments
*******************************************************************************************
local gov_types_B "HR_muni NonHR_muni School "

foreach gov in `gov_types_B' {
    preserve
    keep if type_2 == "`gov'"
	count 

levy_sample d_levy neg_d_eav pos_d_eav d_total_ig_revenue, fe(year n_uniqueid)
regress d_levy neg_d_eav pos_d_eav d_total_ig_revenue i.year i.n_uniqueid if model_sample, vce(cluster n_uniqueid)
	test neg_d_eav = pos_d_eav
		estadd scalar p_equal = r(p)
		estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
		estimates store reg22_`gov'
		gen byte used_in_reg_x = e(sample)

levy_sample d_levy neg_d_eav pos_d_eav if used_in_reg_x==1, fe(year n_uniqueid)
regress d_levy neg_d_eav pos_d_eav i.year i.n_uniqueid if model_sample, vce(cluster n_uniqueid)
		test neg_d_eav = pos_d_eav
		estadd scalar p_equal = r(p)
		estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
		estimates store reg23_`gov'
		
		restore
}

levy_sample d_levy neg_d_eav pos_d_eav d_enrollment if type_2=="School", fe(year n_uniqueid)
regress d_levy neg_d_eav pos_d_eav d_enrollment i.year i.n_uniqueid if model_sample, vce(cluster n_uniqueid)
		test neg_d_eav = pos_d_eav
		estadd scalar p_equal = r(p)
		estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
		estimates store reg24_School

* Create table with all results
esttab reg19_Other reg19_Township reg19_HR_muni reg19_NonHR_muni reg19_School using "asym_ols_regressions", replace star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(N upper_bound, fmt(%5.0fc %9.3f)) mtitles(Other Township HR_muni NonHR_muni School ) ///
 nolines title(Table 8: By agency type: OLS upper bounds estimate of ε_b allowing for asymmetry)
esttab reg20_Other reg20_Township reg20_HR_muni reg20_NonHR_muni reg20_School using "asym_ols_regressions", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(upper_bound, fmt(%9.3f)) nomtitles nolines nonumbers
esttab reg21_Other reg21_Township reg21_HR_muni reg21_NonHR_muni reg21_School using "asym_ols_regressions", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(upper_bound, fmt(%9.3f)) nomtitles nolines nonumbers 
esttab blank1 blank2 reg22_HR_muni reg22_NonHR_muni reg22_School  using "asym_ols_regressions", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(N upper_bound, fmt(%5.0fc %9.3f)) nomtitles nolines nonumbers
esttab blank1 blank2 reg23_HR_muni reg23_NonHR_muni reg23_School  using "asym_ols_regressions", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(upper_bound, fmt(%9.3f)) nomtitles nolines nonumbers
esttab blank1 blank2 blank3 blank4 reg24_School  using "asym_ols_regressions", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(upper_bound, fmt(%9.3f)) nomtitles nolines nonumbers ///
postfoot("Cook county, Illinois data from 2008 through 2023. Cell entries record the upper bound which is the maximum value of the elasticity"  ///
"that we fail to reject with 95 percent confidence. All estimates are derived from OLS regressions that allow for asymmetric reactions to postive and negative changes in EAV." ///
"Row 1 is based on an ols regression with no controls. Rows 2 and 3 also include year dummies; row 3 includes unit dummies." ///
"Row 4 is based on an ols regression with the percentage change in intergovernmental revenue, year dummies and unit dummies" ///
"Row 5 is based on an ols regression using the same sample as in row 4 but omitting the change in intergovernmental revenue while including year and unit dummies" ///
"Row 6 is based on an ols regression using the same sample as in rows 4 and 5 and including the percentage change in enrollments as well as year and unit dummies" )

* Create table A4
esttab reg19_Other reg19_Township reg19_HR_muni reg19_NonHR_muni reg19_School using "asym_ols_regressions_appendix", replace star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(p_equal, fmt(%9.3f)) mtitles(Other Township HR_muni NonHR_muni School ) ///
nolines title(Table A4: P_stats for OLS tests of symmetry)
esttab reg20_Other reg20_Township reg20_HR_muni reg20_NonHR_muni reg20_School using "asym_ols_regressions_appendix", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(p_equal, fmt(%9.3f)) nomtitles nolines nonumbers
esttab reg21_Other reg21_Township reg21_HR_muni reg21_NonHR_muni reg21_School using "asym_ols_regressions_appendix", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(p_equal, fmt(%9.3f)) nomtitles nolines nonumbers 
esttab blank1 blank2 reg22_HR_muni reg22_NonHR_muni reg22_School  using "asym_ols_regressions_appendix", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(p_equal, fmt(%9.3f)) nomtitles nolines nonumbers
esttab blank1 blank2 reg23_HR_muni reg23_NonHR_muni reg23_School  using "asym_ols_regressions_appendix", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(p_equal, fmt(%9.3f)) nomtitles nolines nonumbers
esttab blank1 blank2 blank3 blank4 reg24_School  using "asym_ols_regressions_appendix", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(p_equal, fmt(%9.3f)) nomtitles nolines nonumbers ///
postfoot("Cook county, Illinois data from 2008 through 2023. Cell entries record the p value for the hypothesis that the elasticity of the levy with respect to d_eav is symmetric"  ///
"All estimates are derived from the same OLS regressions used to estimate the upper bounds reported in Table 8." )

*******************************************************************************
* create  table A5  
******************************************************************************
esttab reg19_Other reg19_Township reg19_HR_muni reg19_NonHR_muni reg19_School ///
    using "ols_asym_regressions_appendix_point", replace star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f))) collabels("b") noobs noconstant keep(pos_d_eav neg_d_eav) mtitles(Other Township HR_muni NonHR_muni School) ///
    nolines title(Table A5 By agency type: OLS point estimate of ε_b for EAV increases and decreases)

esttab reg20_Other reg20_Township reg20_HR_muni reg20_NonHR_muni reg20_School using "ols_asym_regressions_appendix_point", append star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f))) noobs noconstant keep(pos_d_eav neg_d_eav) ///
    nomtitles nolines nonumbers  collabels(none)

esttab reg21_Other reg21_Township reg21_HR_muni reg21_NonHR_muni reg21_School using "ols_asym_regressions_appendix_point", append star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f))) noobs noconstant keep(pos_d_eav neg_d_eav) ///
    nomtitles nolines nonumbers  collabels(none)

esttab blank1 blank2 reg22_HR_muni reg22_NonHR_muni reg22_School using "ols_asym_regressions_appendix_point", append star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f))) noobs noconstant keep(pos_d_eav neg_d_eav) ///
    nomtitles nolines nonumbers  collabels(none)

esttab blank1 blank2 reg23_HR_muni reg23_NonHR_muni reg23_School using "ols_asym_regressions_appendix_point", append star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f))) noobs noconstant keep(pos_d_eav neg_d_eav) ///
    nomtitles nolines nonumbers  collabels(none)

esttab blank1 blank2 blank3 blank4 reg24_School using "ols_asym_regressions_appendix_point", append star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f))) noobs noconstant keep(pos_d_eav neg_d_eav) ///
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
local gov_types "Other Township HR_muni NonHR_muni School "

	foreach gov in `gov_types' {
 preserve
    keep if type_2 == "`gov'"
    
    * Run and store regressions
levy_sample d_levy neg_d_eav pos_d_eav reassess_year pos_d_eav_hat
ivreg2 d_levy (neg_d_eav pos_d_eav= reassess_year pos_d_eav_hat) if model_sample, cluster(n_uniqueid) small partial(_cons) endog(neg_d_eav pos_d_eav) ffirst
levy_iv_diagnostics
			test neg_d_eav = pos_d_eav
			estadd scalar p_equal = r(p)
			estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
	estimates store ivreg25_`gov'

levy_sample d_levy neg_d_eav pos_d_eav reassess_year pos_d_eav_hat, fe(year)
ivreg2 d_levy i.year (neg_d_eav pos_d_eav= reassess_year pos_d_eav_hat) if model_sample, cluster(n_uniqueid) small partial(i.year) endog(neg_d_eav pos_d_eav) ffirst
levy_iv_diagnostics
			test neg_d_eav = pos_d_eav
			estadd scalar p_equal = r(p)
			estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
	estimates store ivreg26_`gov'

levy_sample d_levy neg_d_eav pos_d_eav reassess_year pos_d_eav_hat, fe(year n_uniqueid)
ivreg2 d_levy i.year i.n_uniqueid (neg_d_eav pos_d_eav= reassess_year pos_d_eav_hat) if model_sample, cluster(n_uniqueid) small partial(i.year i.n_uniqueid) endog(neg_d_eav pos_d_eav) ffirst
levy_iv_diagnostics
			test neg_d_eav = pos_d_eav
			estadd scalar p_equal = r(p)
			estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
	estimates store ivreg27_`gov'

	
restore
	}
**************************************************************************************************
* use data on intergovernmental revenues and enrollments
*******************************************************************************************
local gov_types_B "HR_muni NonHR_muni School"

foreach gov in `gov_types_B' {
    preserve
    keep if type_2 == "`gov'"
	count 

levy_sample d_levy d_total_ig_revenue neg_d_eav pos_d_eav reassess_year pos_d_eav_hat, fe(year n_uniqueid)
ivreg2 d_levy d_total_ig_revenue i.year i.n_uniqueid (neg_d_eav pos_d_eav= reassess_year pos_d_eav_hat) if model_sample, cluster(n_uniqueid) small partial(i.year i.n_uniqueid) endog(neg_d_eav pos_d_eav) ffirst
levy_iv_diagnostics
	test neg_d_eav = pos_d_eav
		estadd scalar p_equal = r(p)
		estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
		estimates store ivreg28_`gov'
		gen byte used_in_reg_x = e(sample)

levy_sample d_levy neg_d_eav pos_d_eav reassess_year pos_d_eav_hat if used_in_reg_x==1, fe(year n_uniqueid)
ivreg2 d_levy i.year i.n_uniqueid (neg_d_eav pos_d_eav= reassess_year pos_d_eav_hat) if model_sample, cluster(n_uniqueid) small partial(i.year i.n_uniqueid) endog(neg_d_eav pos_d_eav) ffirst
levy_iv_diagnostics
		test neg_d_eav = pos_d_eav
		estadd scalar p_equal = r(p)
		estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
		estimates store ivreg29_`gov'
		
		restore
}



levy_sample d_levy d_enrollment neg_d_eav pos_d_eav reassess_year pos_d_eav_hat if type_2=="School", fe(year n_uniqueid)
ivreg2 d_levy d_enrollment i.year i.n_uniqueid  (neg_d_eav pos_d_eav= reassess_year pos_d_eav_hat) if model_sample, cluster(n_uniqueid) small partial(i.year i.n_uniqueid) endog(neg_d_eav pos_d_eav) ffirst
levy_iv_diagnostics
		test neg_d_eav = pos_d_eav 
		estadd scalar p_equal = r(p)
		estadd scalar upper_bound=_b[pos_d_eav]+(1.645 * _se[pos_d_eav])
		estimates store ivreg30_School


esttab ivreg25_* using "iv_by_agency_asym", replace mtitles(Other Township HR_muni NonHR_muni School ) star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(N upper_bound, fmt(%5.0fc %9.3f))  nolines nonumbers ///
 title(Table 9: By agency type: IV upper bounds estimate of ε_b allowing for asymmetry)
esttab ivreg26_* using "iv_by_agency_asym", append  star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(upper_bound, fmt(%9.3f))  nomtitles nolines nonumbers
esttab ivreg27_* using "iv_by_agency_asym", append  star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(upper_bound, fmt(%9.3f))  nomtitles nolines nonumbers
esttab blank1 blank2 ivreg28_HR_muni ivreg28_NonHR_muni ivreg28_School  using "iv_by_agency_asym", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(N upper_bound, fmt(%5.0fc %9.3f)) nomtitles nolines nonumbers
esttab blank1 blank2 ivreg29_HR_muni ivreg29_NonHR_muni ivreg29_School  using "iv_by_agency_asym", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(upper_bound, fmt(%9.3f)) nomtitles nolines nonumbers 
esttab blank1 blank2 blank3 blank4 ivreg30_School  using "iv_by_agency_asym", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(upper_bound, fmt(%9.3f)) nomtitles nolines nonumbers ///
postfoot("Cook county, Illinois data from 2008 through 2023. Cell entries in rows labeled N record the number of observations used in the rows" /// 
"immediately below. Cell entries in rows labeled upper bound record the maximum value of the elasticity that we fail to reject with" ///
"95 percent confidence. All estimates are derived from IV regressions that allow for asymmetric reactions to postive and negative changes in EAV." ///
"Row 1 is based on an IV regression with no controls. Rows 2 and 3 also include year dummies; row 3 includes unit dummies." ///
"Row 4 is based on an IV regression with the percentage change in intergovernmental revenue, year dummies and unit dummies" ///
"Row 5 is based on an IV regression using the same sample as in row 4 but omitting the change in intergovernmental revenue while including year and unit dummies" ///
"Row 6 is based on an IV regression using the same sample as in rows 4 and 5 and including the percentage change in enrollments as well as year and unit dummies" )

*Create Table A6
esttab ivreg25_* using "iv_by_agency_asym_appendix", replace star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(p_equal, fmt(%9.3f)) mtitles(Other Township HR_muni NonHR_muni School ) nolines ///
  title(Table A6: P_stats for IV tests of symmetry)
esttab ivreg26_* using "iv_by_agency_asym_appendix", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(p_equal, fmt(%9.3f)) nomtitles nolines nonumbers
esttab ivreg27_* using "iv_by_agency_asym_appendix", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(p_equal, fmt(%9.3f)) nomtitles nolines nonumbers
esttab blank1 blank2 ivreg28_HR_muni ivreg28_NonHR_muni ivreg28_School  using "iv_by_agency_asym_appendix", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(N p_equal, fmt(%5.0fc %9.3f)) nomtitles nolines nonumbers
esttab blank1 blank2 ivreg29_HR_muni ivreg29_NonHR_muni ivreg29_School  using "iv_by_agency_asym_appendix", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(p_equal, fmt(%9.3f)) nomtitles nolines nonumbers 
esttab blank1 blank2 blank3 blank4 ivreg30_School  using "iv_by_agency_asym_appendix", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(p_equal, fmt(%9.3f)) nomtitles nolines nonumbers ///
postfoot("Cook county, Illinois data from 2008 through 2023. Cell entries record the p value for the hypothesis that the elasticity of the levy with respect to d_eav is symmetric"  ///
"All estimates are derived from the same IV regressions used to estimate the upper bounds reported in Table 9." )


*Create Table A7
esttab ivreg25_Other ivreg25_Township ivreg25_HR_muni ivreg25_NonHR_muni ivreg25_School ///
    using "iv_async_regressions_appendix_point", replace star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f))) collabels("b") noobs noconstant keep(pos_d_eav neg_d_eav) mtitles(Other Township HR_muni NonHR_muni School) ///
    nolines title(Table A7 By agency type: IV point estimate of ε_b for EAV increases and decreases)
esttab ivreg26_Other ivreg26_Township ivreg26_HR_muni ivreg26_NonHR_muni ivreg26_School using "iv_async_regressions_appendix_point", append star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) noobs noconstant keep(pos_d_eav neg_d_eav) ///
    nomtitles nolines nonumbers  collabels(none)
esttab ivreg27_Other ivreg27_Township ivreg27_HR_muni ivreg27_NonHR_muni ivreg27_School using "iv_async_regressions_appendix_point", append star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) noobs noconstant keep(pos_d_eav neg_d_eav) ///
    nomtitles nolines nonumbers  collabels(none)
esttab blank1 blank2 ivreg28_HR_muni ivreg28_NonHR_muni ivreg28_School using "iv_async_regressions_appendix_point", append star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) noobs noconstant keep(pos_d_eav neg_d_eav) ///
    nomtitles nolines nonumbers  collabels(none)
esttab blank1 blank2 ivreg29_HR_muni ivreg29_NonHR_muni ivreg29_School using "iv_async_regressions_appendix_point", append star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) noobs noconstant keep(pos_d_eav neg_d_eav) ///
    nomtitles nolines nonumbers  collabels(none)
esttab blank1 blank2 blank3 blank4 ivreg30_School using "iv_async_regressions_appendix_point", append star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) noobs noconstant keep(pos_d_eav neg_d_eav) ///
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

gen double d_2_eav=100*log((lag_av*lag_eq_factor_final)/(lag2_av*lag2_eq_factor_final))
gen double d_3_eav=100*log((lag2_av*lag2_eq_factor_final)/(lag3_av*lag3_eq_factor_final))

levy_sample d_levy d_eav d_2_eav d_3_eav
reg  d_levy d_eav d_2_eav d_3_eav if model_sample, vce(cluster n_uniqueid) 
						test d_2_eav+d_3_eav=0
						estadd scalar p_stat = r(p)
                        test d_2_eav d_3_eav
                        estadd scalar joint_lag_F = r(F)
                        estadd scalar joint_lag_p = r(p)
					lincom d_eav+d_2_eav+d_3_eav
						estadd scalar lagged_magnitude=r(estimate)
                        estadd scalar cumulative_p=r(p)
						estadd scalar A_upper_bound=r(estimate)+(1.645 * r(se))
						
estimates store lag_all_ols_31

					
levy_sample d_levy d_eav d_2_eav d_3_eav, fe(year_hr)
reg d_levy d_eav d_2_eav d_3_eav i.year##i.home_rule_for_fe if model_sample, vce(cluster n_uniqueid)
						test d_2_eav+d_3_eav=0
						estadd scalar p_stat = r(p)
                        test d_2_eav d_3_eav
                        estadd scalar joint_lag_F = r(F)
                        estadd scalar joint_lag_p = r(p)
					lincom d_eav+d_2_eav+d_3_eav
						estadd scalar lagged_magnitude=r(estimate)
                        estadd scalar cumulative_p=r(p)
						estadd scalar A_upper_bound=r(estimate)+(1.645 * r(se))
estimates store lag_all_ols_32

levy_sample d_levy d_eav d_2_eav d_3_eav, fe(year_hr n_uniqueid)
reg d_levy d_eav d_2_eav d_3_eav i.year##i.home_rule_for_fe i.n_uniqueid if model_sample, vce(cluster n_uniqueid)
						test d_2_eav+d_3_eav=0
						estadd scalar p_stat = r(p)
                        test d_2_eav d_3_eav
                        estadd scalar joint_lag_F = r(F)
                        estadd scalar joint_lag_p = r(p)
					lincom d_eav+d_2_eav+d_3_eav
						estadd scalar lagged_magnitude=r(estimate)
                        estadd scalar cumulative_p=r(p)
						estadd scalar A_upper_bound=r(estimate)+(1.645 * r(se))
estimates store lag_all_ols_33

esttab lag_all_ols_31 lag_all_ols_32 lag_all_ols_33 using "OLS all agencies_lag", replace ///
	star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f)) se(par)) stats(r2 N p_stat joint_lag_F joint_lag_p lagged_magnitude cumulative_p A_upper_bound, fmt(%4.3f %5.0fc %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f) labels(R-squared N p_lags joint_lag_F joint_lag_p cumulative_effect cumulative_p upper_bound)) ///
	mtitles(M1 M2 M3) keep(d_eav d_2_eav d_3_eav) ///
	postfoot("Cook county, Illinois data from 2008 through 2023. Columns 2 and 3 include two sets of year dummies. One for home rule governments" ///
	"and another for non-home rule governments, columns 3 includes unit dummies" ///
	"joint_lag_p tests whether BOTH lag coefficients are zero. The cumulative effect and upper bound include current EAV and both lags." "p_lags is the p value for the hypothesis that lags of the elasticity of the levy with" "respect to d_eav sum to zero. point_est is the sum of coefficient on d_eav and lags." ///
	"Upper bound is the maximum value of the summed elasticity that we fail to reject with 95 percent confidence") ///
	title(Table 10: All agencies OLS Predict levy using d_eav allowing for two lags of d_eav)
	

	
								
**#
*************************************************************************************************
* next run regressions for four different types of agencies
* and both home rule and non-home rule municipalities
* code below adapted from Claude
***************************************************************************************************		
* Loop through government types
local gov_types "Other Township HR_muni NonHR_muni School "

foreach gov in `gov_types' {
    preserve
    keep if type_2 == "`gov'"
	
levy_sample d_levy d_eav d_2_eav d_3_eav
regress d_levy d_eav d_2_eav d_3_eav if model_sample, vce(cluster n_uniqueid)
						test d_2_eav+d_3_eav=0
						estadd scalar p_stat = r(p)
                        test d_2_eav d_3_eav
                        estadd scalar joint_lag_F = r(F)
                        estadd scalar joint_lag_p = r(p)
					lincom d_eav+d_2_eav+d_3_eav
						estadd scalar lagged_magnitude=r(estimate)
                        estadd scalar cumulative_p=r(p)
						estadd scalar lagged_p = r(p)
						estadd scalar A_upper_bound=r(estimate)+(1.645 * r(se))
estimates store lag_all_ols_34_`gov'

levy_sample d_levy d_eav d_2_eav d_3_eav, fe(year)
regress d_levy d_eav d_2_eav d_3_eav  i.year if model_sample, vce(cluster n_uniqueid)
						test d_2_eav+d_3_eav=0
						estadd scalar p_stat = r(p)
                        test d_2_eav d_3_eav
                        estadd scalar joint_lag_F = r(F)
                        estadd scalar joint_lag_p = r(p)
					lincom d_eav+d_2_eav+d_3_eav
						estadd scalar lagged_magnitude=r(estimate)
                        estadd scalar cumulative_p=r(p)
						estadd scalar lagged_p = r(p)
						estadd scalar A_upper_bound=r(estimate)+(1.645 * r(se))
estimates store lag_all_ols_35_`gov'
		
levy_sample d_levy d_eav d_2_eav d_3_eav, fe(year n_uniqueid)
regress d_levy d_eav d_2_eav d_3_eav  i.year i.n_uniqueid if model_sample, vce(cluster n_uniqueid)
						test d_2_eav+d_3_eav=0
						estadd scalar p_stat = r(p)
                        test d_2_eav d_3_eav
                        estadd scalar joint_lag_F = r(F)
                        estadd scalar joint_lag_p = r(p)
					lincom d_eav+d_2_eav+d_3_eav
						estadd scalar lagged_magnitude=r(estimate)
                        estadd scalar cumulative_p=r(p)
						estadd scalar lagged_p = r(p)
						estadd scalar A_upper_bound=r(estimate)+(1.645 * r(se))
estimates store lag_all_ols_36_`gov'
	
		restore
}

**************************************************************************************************
* use data on intergovernmental revenues and enrollments
*******************************************************************************************
local gov_types_B "HR_muni NonHR_muni School "

foreach gov in `gov_types_B' {
    preserve
    keep if type_2 == "`gov'"
	count 

levy_sample d_levy d_eav d_2_eav d_3_eav d_total_ig_revenue, fe(year n_uniqueid)
regress d_levy d_eav d_2_eav d_3_eav d_total_ig_revenue i.year i.n_uniqueid if model_sample, vce(cluster n_uniqueid)
					test d_2_eav+d_3_eav=0
					estadd scalar p_stat = r(p)
                        test d_2_eav d_3_eav
                        estadd scalar joint_lag_F = r(F)
                        estadd scalar joint_lag_p = r(p)
					lincom d_eav+d_2_eav+d_3_eav
					estadd scalar lagged_magnitude=r(estimate)
                        estadd scalar cumulative_p=r(p)
					estadd scalar lagged_p = r(p)
					estadd scalar A_upper_bound=r(estimate)+(1.645 * r(se))
		
		estimates store lag_all_ols_37_`gov'
		gen byte used_in_reg_x = e(sample)

levy_sample d_levy d_eav d_2_eav d_3_eav if used_in_reg_x==1, fe(year n_uniqueid)
regress d_levy d_eav d_2_eav d_3_eav i.year i.n_uniqueid if model_sample, vce(cluster n_uniqueid)
					test d_2_eav+d_3_eav=0
					estadd scalar p_stat = r(p)
                        test d_2_eav d_3_eav
                        estadd scalar joint_lag_F = r(F)
                        estadd scalar joint_lag_p = r(p)
					lincom d_eav+d_2_eav+d_3_eav
					estadd scalar lagged_magnitude=r(estimate)
                        estadd scalar cumulative_p=r(p)
					estadd scalar lagged_p = r(p)
					estadd scalar A_upper_bound=r(estimate)+(1.645 * r(se))
				estimates store lag_all_ols_38_`gov'
		
		restore
}

levy_sample d_levy d_eav d_2_eav d_3_eav d_enrollment if type_2=="School", fe(year n_uniqueid)
regress d_levy d_eav d_2_eav d_3_eav  d_enrollment i.year i.n_uniqueid if model_sample, vce(cluster n_uniqueid)
					test d_2_eav+d_3_eav=0
					estadd scalar p_stat = r(p)
                        test d_2_eav d_3_eav
                        estadd scalar joint_lag_F = r(F)
                        estadd scalar joint_lag_p = r(p)
					lincom d_eav+d_2_eav+d_3_eav
					estadd scalar lagged_magnitude=r(estimate)
                        estadd scalar cumulative_p=r(p)
					estadd scalar lagged_p = r(p)
					estadd scalar A_upper_bound=r(estimate)+(1.645 * r(se))
				estimates store lag_all_ols_39_School

* Create table with all results
esttab lag_all_ols_34_Other lag_all_ols_34_Township lag_all_ols_34_HR_muni lag_all_ols_34_NonHR_muni lag_all_ols_34_School using "by_type_lag_ols_regressions", replace star(* 0.10 ** 0.05 *** 0.01) cells(none)  ///
		stats(N A_upper_bound, fmt(%5.0fc %9.3f) labels(N upper_bound)) mtitles(Other Township HR_muni NonHR_muni School ) nolines /// 
		title(Table 11: By agency type: OLS upper bounds estimate of ε_b allowing for two lags of d_eav)
esttab lag_all_ols_35_Other lag_all_ols_35_Township lag_all_ols_35_HR_muni lag_all_ols_35_NonHR_muni lag_all_ols_35_School using "by_type_lag_ols_regressions", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(A_upper_bound, fmt(%9.3f) labels(upper_bound)) ///
	nomtitles nolines nonumbers 
esttab lag_all_ols_36_Other lag_all_ols_36_Township lag_all_ols_36_HR_muni lag_all_ols_36_NonHR_muni lag_all_ols_36_School using "by_type_lag_ols_regressions", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(A_upper_bound, fmt(%9.3f) labels(upper_bound)) /// 
	nomtitles nolines nonumbers
esttab blank1 blank2 lag_all_ols_37_HR_muni lag_all_ols_37_NonHR_muni lag_all_ols_37_School  using "by_type_lag_ols_regressions", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(N A_upper_bound, fmt(%5.0fc %9.3f) labels(N upper_bound)) nomtitles nolines nonumbers 
esttab blank1 blank2 lag_all_ols_38_HR_muni lag_all_ols_38_NonHR_muni lag_all_ols_38_School  using "by_type_lag_ols_regressions", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(A_upper_bound, fmt(%9.3f) labels(upper_bound)) nomtitles nolines nonumbers 
esttab blank1 blank2 blank3 blank4 lag_all_ols_39_School  using "by_type_lag_ols_regressions", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(A_upper_bound, fmt(%9.3f) labels(upper_bound)) nomtitles nolines nonumbers ///
postfoot("Cook county, Illinois data from 2008 through 2023. Cell entries record the upper bound which is the maximum value of the elasticity"  ///
"that we fail to reject with 95 percent confidence. All estimates are derived from OLS regressions that allow for contemporaneous and two lags of changes in EAV." ///
"Row 1 is based on an ols regression with no controls. Rows 2 and 3 also include year dummies; row 3 includes unit dummies." ///
"Row 4 is based on an ols regression with the percentage change in intergovernmental revenue, year dummies and unit dummies" ///
"Row 5 is based on an ols regression using the same sample as in row 4 but omitting the change in intergovernmental revenue while including year and unit dummies" ///
"Row 6 is based on an ols regression using the same sample as in rows 4 and 5 and including the percentage change in enrollments as well as year and unit dummies" )


* Create table A8
esttab lag_all_ols_34_Other lag_all_ols_34_Township lag_all_ols_34_HR_muni lag_all_ols_34_NonHR_muni lag_all_ols_34_School using "by_type_lag_ols_regressions_appendix", replace star(* 0.10 ** 0.05 *** 0.01) cells(none)  ///
		stats(p_stat, fmt(%9.3f) labels(p_stat)) mtitles(Other Township HR_muni NonHR_muni School ) nolines title(Table A8: P_stats for OLS tests of lagged effects)
esttab lag_all_ols_35_Other lag_all_ols_35_Township lag_all_ols_35_HR_muni lag_all_ols_35_NonHR_muni lag_all_ols_35_School using "by_type_lag_ols_regressions_appendix", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(p_stat, fmt(%9.3f) labels(p_stat)) ///
	nomtitles nolines nonumbers 
esttab lag_all_ols_36_Other lag_all_ols_36_Township lag_all_ols_36_HR_muni lag_all_ols_36_NonHR_muni lag_all_ols_36_School using "by_type_lag_ols_regressions_appendix", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(p_stat, fmt(%9.3f) labels(p_stat)) /// 
	nomtitles nolines nonumbers
esttab blank1 blank2 lag_all_ols_37_HR_muni lag_all_ols_37_NonHR_muni lag_all_ols_37_School  using "by_type_lag_ols_regressions_appendix", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(p_stat, fmt(%9.3fc) labels(p_stat)) nomtitles nolines nonumbers 
esttab blank1 blank2 lag_all_ols_38_HR_muni lag_all_ols_38_NonHR_muni lag_all_ols_38_School  using "by_type_lag_ols_regressions_appendix", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(p_stat, fmt(%9.3f) labels(p_stat)) nomtitles nolines nonumbers 
esttab blank1 blank2 blank3 blank4 lag_all_ols_39_School  using "by_type_lag_ols_regressions_appendix", append star(* 0.10 ** 0.05 *** 0.01) cells(none) stats(p_stat, fmt(%9.3f) labels(p_stat)) nomtitles nolines nonumbers ///
postfoot("Cook county, Illinois data from 2008 through 2023. Cell entries record the p value for the hypothesis that the elasticity of the levy with respect lags of d_eav sum to zero"  ///
"All estimates are derived from the same OLS regressions used to estimate the upper bounds reported in Table 11." )
				

				
*******************************************************************************
* create  table A9 point estimate for the sum of lags  collabels("sum of lags")
******************************************************************************
esttab lag_all_ols_34_Other lag_all_ols_34_Township lag_all_ols_34_HR_muni lag_all_ols_34_NonHR_muni lag_all_ols_34_School ///
    using "ols_lag_regressions_appendix_point", replace star(* 0.10 ** 0.05 *** 0.01) cells(none)   mtitles(Other Township HR_muni NonHR_muni School) ///
    nolines title(Table A9 By agency type: OLS point estimate of ε_b allowing for 2 lags)  stats(lagged_magnitude lagged_p, fmt(%9.3f %9.3f) labels(sum_ε_b "    lagged_p")  ) 

esttab lag_all_ols_35_Other lag_all_ols_35_Township lag_all_ols_35_HR_muni lag_all_ols_35_NonHR_muni lag_all_ols_35_School using "ols_lag_regressions_appendix_point", append star(* 0.10 ** 0.05 *** 0.01) cells(none) nomtitles nolines nonumbers ///
	stats(lagged_magnitude lagged_p, fmt(%9.3f %9.3f) labels(sum_ε_b "   lagged_p")  )  

esttab lag_all_ols_36_Other lag_all_ols_36_Township lag_all_ols_36_HR_muni lag_all_ols_36_NonHR_muni lag_all_ols_36_School using "ols_lag_regressions_appendix_point", append star(* 0.10 ** 0.05 *** 0.01) cells(none) nomtitles nolines nonumbers ///
	stats(lagged_magnitude lagged_p, fmt(%9.3f %9.3f) labels(sum_ε_b "   lagged_p")  )  
	
esttab blank1 blank2 lag_all_ols_37_HR_muni lag_all_ols_37_NonHR_muni lag_all_ols_37_School using "ols_lag_regressions_appendix_point", append star(* 0.10 ** 0.05 *** 0.01) cells(none) nomtitles nolines nonumbers ///
	stats(lagged_magnitude lagged_p, fmt(%9.3f %9.3f) labels(sum_ε_b "   lagged_p")  )  

esttab blank1 blank2 lag_all_ols_38_HR_muni lag_all_ols_38_NonHR_muni lag_all_ols_38_School using "ols_lag_regressions_appendix_point", append star(* 0.10 ** 0.05 *** 0.01) cells(none) nomtitles nolines nonumbers ///
	stats(lagged_magnitude lagged_p, fmt(%9.3f %9.3f) labels(sum_ε_b "   lagged_p")  )  
	
esttab blank1 blank2 blank3 blank4 lag_all_ols_39_School using "ols_lag_regressions_appendix_point", append star(* 0.10 ** 0.05 *** 0.01) cells(none) nomtitles nolines nonumbers ///
	stats(lagged_magnitude lagged_p, fmt(%9.3f %9.3f) labels(sum_ε_b "   lagged_p")  )  ///
postfoot("Cook county, Illinois data from 2008 through 2023. Cell entries give the estimated sum of the coefficients d_eav, d2_eav and d3_eav and (in row below)" ///
"the p statistics for the hypothesis that those coefficients sum to zero" ///
"All estimates are derived from the same OLS regressions used to estimate the upper bounds reported in Table 11.") 


* Detailed IV review tables and machine-readable validation.
local review_symmetric "all_iv_1 all_iv_2 all_iv_3 ivreg7_Other ivreg7_Township ivreg7_HR_muni ivreg7_NonHR_muni ivreg7_School ivreg8_Other ivreg8_Township ivreg8_HR_muni ivreg8_NonHR_muni ivreg8_School ivreg9_Other ivreg9_Township ivreg9_HR_muni ivreg9_NonHR_muni ivreg9_School ivreg10_HR_muni ivreg10_NonHR_muni ivreg10_School ivreg11_HR_muni ivreg11_NonHR_muni ivreg11_School ivreg12_School"
esttab all_iv_1 all_iv_2 all_iv_3 ivreg7_Other ivreg7_Township using "IV_symmetric_full_details.txt", replace star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(6)) se(par fmt(6))) keep(d_eav) stats(N N_clust regF p_exog idp widstat, fmt(6)) mtitles title("IV symmetric: clustered SEs; endogeneity C test; KP rank diagnostics")
esttab ivreg7_HR_muni ivreg7_NonHR_muni ivreg7_School ivreg8_Other ivreg8_Township using "IV_symmetric_full_details.txt", append star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(6)) se(par fmt(6))) keep(d_eav) stats(N N_clust regF p_exog idp widstat, fmt(6)) mtitles title("IV symmetric: clustered SEs; endogeneity C test; KP rank diagnostics")
esttab ivreg8_HR_muni ivreg8_NonHR_muni ivreg8_School ivreg9_Other ivreg9_Township using "IV_symmetric_full_details.txt", append star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(6)) se(par fmt(6))) keep(d_eav) stats(N N_clust regF p_exog idp widstat, fmt(6)) mtitles title("IV symmetric: clustered SEs; endogeneity C test; KP rank diagnostics")
esttab ivreg9_HR_muni ivreg9_NonHR_muni ivreg9_School ivreg10_HR_muni ivreg10_NonHR_muni using "IV_symmetric_full_details.txt", append star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(6)) se(par fmt(6))) keep(d_eav) stats(N N_clust regF p_exog idp widstat, fmt(6)) mtitles title("IV symmetric: clustered SEs; endogeneity C test; KP rank diagnostics")
esttab ivreg10_School ivreg11_HR_muni ivreg11_NonHR_muni ivreg11_School ivreg12_School using "IV_symmetric_full_details.txt", append star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(6)) se(par fmt(6))) keep(d_eav) stats(N N_clust regF p_exog idp widstat, fmt(6)) mtitles title("IV symmetric: clustered SEs; endogeneity C test; KP rank diagnostics")
local review_asymmetric "reg_16 reg_17 reg_18 ivreg25_Other ivreg25_Township ivreg25_HR_muni ivreg25_NonHR_muni ivreg25_School ivreg26_Other ivreg26_Township ivreg26_HR_muni ivreg26_NonHR_muni ivreg26_School ivreg27_Other ivreg27_Township ivreg27_HR_muni ivreg27_NonHR_muni ivreg27_School ivreg28_HR_muni ivreg28_NonHR_muni ivreg28_School ivreg29_HR_muni ivreg29_NonHR_muni ivreg29_School ivreg30_School"
esttab reg_16 reg_17 reg_18 ivreg25_Other ivreg25_Township using "IV_asymmetric_full_details.txt", replace star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(6)) se(par fmt(6))) keep(neg_d_eav pos_d_eav) stats(N N_clust F_neg F_pos SW_neg SW_pos p_exog p_equal idp widstat, fmt(6)) mtitles title("IV asymmetric: clustered SEs; endogeneity C test; KP rank diagnostics")
esttab ivreg25_HR_muni ivreg25_NonHR_muni ivreg25_School ivreg26_Other ivreg26_Township using "IV_asymmetric_full_details.txt", append star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(6)) se(par fmt(6))) keep(neg_d_eav pos_d_eav) stats(N N_clust F_neg F_pos SW_neg SW_pos p_exog p_equal idp widstat, fmt(6)) mtitles title("IV asymmetric: clustered SEs; endogeneity C test; KP rank diagnostics")
esttab ivreg26_HR_muni ivreg26_NonHR_muni ivreg26_School ivreg27_Other ivreg27_Township using "IV_asymmetric_full_details.txt", append star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(6)) se(par fmt(6))) keep(neg_d_eav pos_d_eav) stats(N N_clust F_neg F_pos SW_neg SW_pos p_exog p_equal idp widstat, fmt(6)) mtitles title("IV asymmetric: clustered SEs; endogeneity C test; KP rank diagnostics")
esttab ivreg27_HR_muni ivreg27_NonHR_muni ivreg27_School ivreg28_HR_muni ivreg28_NonHR_muni using "IV_asymmetric_full_details.txt", append star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(6)) se(par fmt(6))) keep(neg_d_eav pos_d_eav) stats(N N_clust F_neg F_pos SW_neg SW_pos p_exog p_equal idp widstat, fmt(6)) mtitles title("IV asymmetric: clustered SEs; endogeneity C test; KP rank diagnostics")
esttab ivreg28_School ivreg29_HR_muni ivreg29_NonHR_muni ivreg29_School ivreg30_School using "IV_asymmetric_full_details.txt", append star(* 0.10 ** 0.05 *** 0.01) cells(b(star fmt(6)) se(par fmt(6))) keep(neg_d_eav pos_d_eav) stats(N N_clust F_neg F_pos SW_neg SW_pos p_exog p_equal idp widstat, fmt(6)) mtitles title("IV asymmetric: clustered SEs; endogeneity C test; KP rank diagnostics")
tempname audit
postfile `audit' str32 model str20 term double b se N clusters first_F SW_F p_endog p_underid KP_F using "iv_model_audit.dta", replace
foreach model in `review_symmetric' `review_asymmetric' {
    estimates restore `model'
    local endog_terms "`e(instd)'"
    foreach term of local endog_terms {
        assert !missing(_b[`term']) & !missing(_se[`term']) & _se[`term']>0
        local fs = e(regF)
        local sw = e(regF)
        if "`term'"=="neg_d_eav" {
            local fs = e(F_neg)
            local sw = e(SW_neg)
        }
        if "`term'"=="pos_d_eav" {
            local fs = e(F_pos)
            local sw = e(SW_pos)
        }
        assert !missing(`fs', `sw', e(p_exog), e(idp), e(widstat))
        post `audit' ("`model'") ("`term'") (_b[`term']) (_se[`term']) (e(N)) (e(N_clust)) (`fs') (`sw') (e(p_exog)) (e(idp)) (e(widstat))
    }
}
postclose `audit'
preserve
use "iv_model_audit.dta", clear
export delimited using "iv_model_audit.csv", replace
restore

di as result "DFM ALIGNED MAIN TABLES RUN COMPLETED SUCCESSFULLY"
exit








