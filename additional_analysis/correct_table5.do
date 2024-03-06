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

** The original paper drops observations if points are too high. 
gen high_pts = 0
foreach round in anom earn recog{
	replace high_pts = 1 if `round'pts > 3000
	}

* Badgegroupsize (Prolific) 
foreach size in 15 75 300 {
	gen group`size' = badgegroupsize == `size'
	label variable group`size' "Group of `size'"
	label define lb_`size' 0 " " 1 "Group of `size'"
	label values group`size' lb_`size'
}


	
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


* Call the original file that constructs the order variable. 
* (watch out, this order variable is coded incorrectly.) 
do replication_code_data/build/Label_Rounds.do
rename order order_original

* Call a corrected file which fixes the error in the order variable.
do additional_analysis/build/Label_Rounds.do

** Replicate Table 5 using the original and updated order variable. 

reg pts i.treatment i.order_original if sample == "Prolific" & flag_attention_check == 0 & consistent_b == 1 & high_pts == 0, vce(cluster id)
test (2.order_original = 0) (3.order_original = 0) 

reg pts i.treatment i.order if sample == "Prolific" & flag_attention_check == 0 & consistent_b == 1 & high_pts == 0, vce(cluster id)
test (2.order = 0) (3.order = 0) 

gen hold = .
label variable hold "Order"
foreach smpl in Prolific Berkeley BU {
	foreach x in order order_original {
		replace hold = `x'
		reg pts i.treatment i.hold if sample == "`smpl'" & flag_attention_check == 0 & consistent_b == 1 & high_pts == 0, vce(cluster id)
		eststo reg_`smpl'_`x' 
		qui sum pts if round == "anom" & e(sample)
		estadd local control_mean = round(r(mean), 0.001) 
		estadd local orderFE "Yes"
		estadd local sample "`smpl'"
		test (2.hold = 0) (3.hold = 0) 
		local p : di %4.3f r(p)
		estadd local pval = `p' 
			
		if "`smpl'" == "Prolific" {
			*** Run additional regression using the prolific sample with group size interactions. 
			reg pts i.treatment i.hold group300 1.group300#2.treatment 1.group300#3.treatment  group15 1.group15#2.treatment 1.group15#3.treatment if sample == "`smpl'" & flag_attention_check == 0 & consistent_b == 1 & high_pts == 0, vce(cluster id)
			eststo gr_`smpl'_`x'
			qui sum pts if round == "anom" & e(sample)
			estadd local control_mean = round(r(mean), 0.001) 
			estadd local orderFE "Yes"
			estadd local sample "`smpl'"
			test (2.hold = 0) (3.hold = 0) 
			local p : di %4.3f r(p)
			estadd local pval = `p'  
		}
		
	}
}

esttab reg_Prolific_order_original reg_Prolific_order reg_Berkeley_order_original reg_Berkeley_order reg_BU_order_original reg_BU_order gr_Prolific_order_original gr_Prolific_order using additional_analysis/tables/table5_order.tex, replace /// 
booktabs obslast /// 
scalars("control_mean Control mean" "pval Order dummies F-test" "sample Sample" "nex \hline") /// 
mgroups("Table 5 column (1)" "Table 5 column (2)" "Table 5 column (3)" "Table 5 column (4)", pattern(1 0 1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span) ///
mtitles("Original" "Corrected" "Original" "Corrected" "Original" "Corrected" "Original" "Corrected") nonumbers nobaselevels ///
star(* 0.1 ** 0.05 *** 0.01) se nonotes label drop(_cons)


* Replicate Table 5 using the original and new coherency criterion. 
gen coherent_original = consistent_b == 1
gen coherent_alt = incoherent == 0
foreach smpl in Prolific Berkeley BU {
	foreach coh in coherent_original coherent_alt {
		reg pts i.treatment i.order if sample == "`smpl'" & flag_attention_check == 0 & `coh' == 1 & high_pts == 0, vce(cluster id)
		eststo `smpl'_`coh' 
		qui sum pts if round == "anom" & e(sample)
		estadd local control_mean = round(r(mean), 0.001) 
		estadd local orderFE "Yes"
		estadd local sample "`smpl'"
		test (2.order = 0) (3.order = 0) 
		local p : di %4.3f r(p)
		estadd local pval = `p' 
			
		if "`smpl'" == "Prolific" {
			*** Run additional regression using the prolific sample with group size interactions. 
			reg pts i.treatment i.order group300 1.group300#2.treatment 1.group300#3.treatment  group15 1.group15#2.treatment 1.group15#3.treatment if sample == "`smpl'" & flag_attention_check == 0 & `coh' == 1 & high_pts == 0, vce(cluster id)
			eststo X`smpl'_`coh'
			qui sum pts if round == "anom" & e(sample)
			estadd local control_mean = round(r(mean), 0.001) 
			estadd local orderFE "Yes"
			estadd local sample "`smpl'"
			test (2.order = 0) (3.order = 0) 
			local p : di %4.3f r(p)
			estadd local pval = `p'  
		}
		
	}
}

esttab Prolific_coherent_original Prolific_coherent_alt Berkeley_coherent_original Berkeley_coherent_alt BU_coherent_original BU_coherent_alt XProlific_coherent_original XProlific_coherent_alt using additional_analysis/tables/table5_coh.tex, replace /// 
booktabs obslast /// 
scalars("control_mean Control mean" "orderFE Order dummies" "pval Order dummies F-test" "sample Sample" "nex \hline") /// 
mgroups("Table 5 column (1)" "Table 5 column (2)" "Table 5 column (3)" "Table 5 column (4)", pattern(1 0 1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span) ///
mtitles("Original" "Alternative" "Original" "Alternative" "Original" "Alternative" "Original" "Alternative") nonumbers nobaselevels ///
star(* 0.1 ** 0.05 *** 0.01) se nonotes label drop(_cons *.order)
