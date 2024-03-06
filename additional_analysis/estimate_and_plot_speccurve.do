clear
use replication_code_data/Prepped_Data/charity_cleaned_data.dta

* Identify incoherent wtps using stated WTPs.
gen switch = 0
forvalues p = 3 / 17 {
    local j = `p' - 1
	local k = `p' - 2
	replace switch = switch + 1 if wtp`p' > wtp`j' & wtp`j' < wtp`k'
	replace switch = switch + 1 if wtp`p' < wtp`j' & wtp`j' > wtp`k'
}
gen incoherent = switch > 1
gen incoherent_b = 1 - consistent_b

li wtp* if incoherent_b == 1 & incoherent == 0

** The original paper drops observations if points are too high. 
gen high_pts = 0
foreach round in anom earn recog{
	replace high_pts = 1 if `round'pts > 3000
	}

replace gender = . if gender == 4


* We need to reshape the data set to a long form where each individual has three entries:
* one containing the points earned in anonymous with and w/o additional financial incentives
* and one row containing the points earned in public recognition. 
rename anompts pts1 
rename recogpts pts2
rename earnpts pts3

reshape long pts, i(id) j(treatment)

label define tr 1 "Anonymous" 2 "Public Recognition" 3 "Financial Incentives"
label values treatment tr 

gen prolific = sample == "Prolific" 
gen bu = sample == "BU" 
gen berkeley = sample == "Berkeley"

gen round = "anom" if treatment == 1
replace round = "recog" if treatment == 2
replace round = "earn" if treatment == 3

gen PublicRecognition = treatment == 2
gen FinancialIncentives = treatment == 3


* Call a corrected file which fixes the error in the order variable.
do additional_analysis/build/Label_Rounds.do

* Run regressions. 
tempname tmpnm 
tempfile tmpfl

local contol_list "AgeControls GenderControls OrderControls IncludeProlific IncludeBU IncludeBerkeley Consistency"

postfile `tmpnm' `contol_list' ///
b_pub b_fin se_pub se_fin /// 
using `tmpfl'

loc flag = "flag_attention_check == 0"

local AgeOptions "0 1"
local GenderControls "0 1"
local OrderControls "0 1"
local IncludeProlific "0 1"
local IncludeBU "0 1"
local IncludeBerkeley "0 1"
local ConsCrit "0 1 2"


foreach consistcrit in `ConsCrit' {
foreach ageopt in `AgeOptions' {
foreach genopt in `GenderControls' {
foreach ordopt in `OrderControls' {
foreach prolopt in `IncludeProlific' {
foreach buopt in `IncludeBU' {
foreach berkopt in `IncludeBerkeley' {

	if `consistcrit' == 0 {
		local consistent "" 
	}
	if `consistcrit' == 1 {
		local consistent " & consistent_b == 1"
	}
	if `consistcrit' == 3 {
		local consistent " & incoherent == 0"
	}
	local controls ""
	if `ageopt' == 1 {
		local controls "`controls' age"
	}
	if `genopt' == 1 {
		local controls "`controls' i.gender"
	}
	if `ordopt' == 1 {
		local controls "`controls' i.order"
	}
	if `prolopt' == 1 & `buopt' == 1 & `berkopt' == 1 {
		local sample_include "prolific == 1 | bu == 1 | berkeley == 1"
	}
	if `prolopt' == 0 & `buopt' == 1 & `berkopt' == 1 {
		local sample_include "bu == 1 | berkeley == 1"
	}
	if `prolopt' == 0 & `buopt' == 0 & `berkopt' == 1 {
		local sample_include "berkeley == 1"
	}
	if `prolopt' == 1 & `buopt' == 0 & `berkopt' == 1 {
		local sample_include "prolific == 1 | berkeley == 1"
	}
	if `prolopt' == 1 & `buopt' == 1 & `berkopt' == 0 {
		local sample_include "prolific == 1 | bu == 1"
	}
	if `prolopt' == 0 & `buopt' == 1 & `berkopt' == 0 {
		local sample_include "bu == 1"
	}
	if `prolopt' == 1 & `buopt' == 0 & `berkopt' == 0 {
		local sample_include "prolific == 1"
	}
	if `prolopt' == 0 & `buopt' == 0 & `berkopt' == 0 {
		local sample_include "skip"
	}
	
	
	if "`sample_include'" != "skip" {
		qui reg pts PublicRecognition FinancialIncentives `controls' if high_pts == 0 & (`sample_include') & `flag' `consistent', vce(cluster id)
		local b_pub = _b[PublicRecognition]
		local b_fin = _b[FinancialIncentives]
		local se_pub = _se[PublicRecognition]
		local se_fin = _se[FinancialIncentives]
		post `tmpnm' (`ageopt') (`genopt') (`ordopt') (`prolopt') (`buopt') (`berkopt') (`consistcrit') /// 
		(`b_pub') (`b_fin') (`se_pub') (`se_fin')
	}
}
}
}
}
}
}
}

postclose `tmpnm'
foreach explvar in pub fin {

if "`explvar'" == "pub" {
	local explvarname "Public recognition"
	}
else {
	local explvarname "Financial incentives"
	}

clear
use `tmpfl' 

keep if (IncludeProlific == 1 & IncludeBU == 0 & IncludeBerkeley == 0) | (IncludeProlific == 0 & IncludeBU == 1 & IncludeBerkeley == 0) | ///
(IncludeProlific == 0 & IncludeBU == 0 & IncludeBerkeley == 1)

egen rk_b = rank(b_`explvar'), unique


tempname tmpnm 
tempfile pltfl

postfile `tmpnm' control_indicator specification using `pltfl' 

qui sum rk_b 
local max_rk = `r(max)'
forvalues p = `r(min)' / `r(max)' {
	local i = 0 
	foreach control in Consistency {
		qui sum `control' if rk_b == `p' 
		if `r(mean)' == 1 {
			post `tmpnm' (`i') (`p')
		}
		if `r(mean)' == 2 {
			post `tmpnm' (`i' + 1) (`p')
		}
		local i = `i' + 2	
	}	
	local i = `i' + 1	
	foreach control in IncludeProlific IncludeBU IncludeBerkeley {
		qui sum `control' if rk_b == `p' 
		if `r(mean)' == 1 {
			post `tmpnm' (`i') (`p')
		}
		local i = `i' + 1	
	}	
	local i = `i' + 1
	foreach control in AgeControls GenderControls OrderControls {
		qui sum `control' if rk_b == `p' 
		if `r(mean)' == 1 {
			post `tmpnm' (`i') (`p')
		}
		local i = `i' + 1	
	}	
}
postclose `tmpnm'

gen ci_l_`explvar' = b_`explvar' - 1.96 * se_`explvar'
gen ci_u_`explvar' = b_`explvar' + 1.96 * se_`explvar'


label define b 100 "                       100"
label values b_`explvar' b 

local categories = 15
local scatterplots ""
local rplots ""
qui sum b_`explvar'
local b_min = `r(min)'
local step = (`r(max)' - `r(min)' - .1) / `categories'
colorpalette hue, hue(0 -120) chroma(50) luminance(70) n(`categories')
forvalues p = 1 / `categories' {
	local minval = (`p' - 1) * `step' + `b_min' - .1
	local maxval = `p' * `step' + `b_min' - .1
	local scatterplots "`scatterplots' (scatter b_`explvar' rk_b if b_`explvar' > `minval' & b_`explvar' <= `maxval', mcolor("`r(p`p')'") msymbol(O))"
	local rplots "`rplots' (rcap ci_l_`explvar' ci_u_`explvar' rk_b if b_`explvar' > `minval' & b_`explvar' <= `maxval', lcolor("`r(p`p')'"))"
	}
	
qui twoway `scatterplots' /// 
`rplots' ///
, name(g1, replace) xscale(off) legend(off) ytitle("`explvarname' coefficient") ylabel(, valuelabel)

clear 
use `pltfl', replace 


label define cnt_ind 0 "BMMT coherency" 1 "Alt. coherency" 2 " " 3 "Prolific" 4 "BU" 5 "Berkeley" 6 " " 7 "Age" 8 "Gender" 9 "Order"
label values control_indicator cnt_ind

colorpalette s1
local scatterplots ""
forvalues p = 0 / 9 {
	local scatterplots "`scatterplots' (scatter control_indicator specification if control_indicator == `p', mcolor("`r(p`p')'") msymbol(O))"
	}
	
twoway `scatterplots', name(g2, replace) xscale(off) ylabel(0 1 2 3 4 5 6 7 8 9, valuelabel) yline(2) yline(6) legend(off) ytitle(" ")


graph combine g1 g2, cols(1) xcommon
graph export additional_analysis/figures/`explvar'_specification_curve.png, replace 
}
