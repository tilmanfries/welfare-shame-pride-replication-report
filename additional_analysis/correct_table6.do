clear
use replication_code_data/Prepped_Data/charity_cleaned_data.dta

** The original paper drops observations if points are too high. 
gen high_pts = 0
foreach round in anom earn recog{
	replace high_pts = 1 if `round'pts > 3000
	}

* Identify incoherent wtps using stated WTPs.
gen switch = 0
forvalues p = 3 / 17 {
    local j = `p' - 1
	local k = `p' - 2
	replace switch = switch + 1 if wtp`p' > wtp`j' & wtp`j' < wtp`k'
	replace switch = switch + 1 if wtp`p' < wtp`j' & wtp`j' > wtp`k'
}
gen incoherent = switch > 1
gen coherent_alt = 1 - incoherent
gen coherent_original = consistent_b

** Define programs to generate the curvature confidence intervals **
capture program drop get_ci 
capture program drop get_ci_new

*** This is the program used in the original code of the paper.
program define get_ci, rclass

	nlcom -2*_b[c.interval#c.interval]/(_b[interval]+2*_b[c.interval#c.interval]*`2') // measure of curvature
	
	matrix tempb = r(b)
	matrix tempv = r(V)
	loc x : di %4.3f tempb[1,1]
	loc lb : di %4.3f tempb[1,1] - (invnormal(.975)*(sqrt(tempv[1,1])))
	loc ub : di %4.3f tempb[1,1] + (invnormal(.975)*(sqrt(tempv[1,1])))
	
	return loc alpha "`x'"
	return loc ci : di "[`lb', `ub']"
	
	* Multiply estimate by SD of points in the anonymous round
	nlcom -2*`1'*_b[c.interval#c.interval]/_b[interval]
	
	matrix tempb = r(b)
	matrix tempv = r(V)
	loc x : di %4.3f tempb[1,1]
	loc lb : di %4.3f tempb[1,1] - (invnormal(.975)*(sqrt(tempv[1,1])))
	loc ub : di %4.3f tempb[1,1] + (invnormal(.975)*(sqrt(tempv[1,1])))
	
	return loc alpha2 "`x'"
	return loc ci2 : di "[`lb', `ub']"
	
end	

*** This is new program provides the correct unitless curvature measure. 
program define get_ci_new, rclass

	nlcom -2*_b[c.interval#c.interval]/(_b[interval]+2*_b[c.interval#c.interval]*`2') // measure of curvature
	
	matrix tempb = r(b)
	matrix tempv = r(V)
	loc x : di %4.3f tempb[1,1]
	loc lb : di %4.3f tempb[1,1] - (invnormal(.975)*(sqrt(tempv[1,1])))
	loc ub : di %4.3f tempb[1,1] + (invnormal(.975)*(sqrt(tempv[1,1])))
	
	return loc alpha "`x'"
	return loc ci : di "[`lb', `ub']"
		
	local a2: di %4.3f `x'*`1'
	local lb2: di %4.3f `lb'*`1'
	local ub2: di %4.3f `ub'*`1'
	return loc alpha2 "`a2' "
	return loc ci2 : di "[`lb2', `ub2']"
	
end	

foreach smpl in Prolific Berkeley BU { 

	sum anompts if coherent_original == 1 & flag_attention_check == 0 & high_pts == 0 & sample == "Prolific"
	local pts_mean = r(mean) / 100
	local pts_sd = r(sd) / 100
	
	preserve
	reshape long wtp nwtp, i(id) j(interval)

	* Take the midpoint of each interval. 
	replace interval = interval + .5
	gen interval_sq = interval^2
	
	label variable interval "Points (100s)"
	label variable interval_sq "Points (100s) sqd."

	reg wtp c.interval c.interval#c.interval if sample == "`smpl'" & interval < 17 & coherent_original == 1 & flag_attention_check == 0 & high_pts == 0, vce(cluster id)	
	eststo orig_`smpl'
	get_ci `pts_sd' `pts_mean'
	estadd local alpha1 "`r(alpha)'"
	estadd local ci1 "`r(ci)'"
	estadd local alpha2 "`r(alpha2)'"
	estadd local ci2 "`r(ci2)'"
	estadd local sample "`smpl'"

	reg wtp c.interval c.interval#c.interval if sample == "`smpl'" & interval < 17 & coherent_original == 1 & flag_attention_check == 0 & high_pts == 0, vce(cluster id)	
	eststo new_`smpl'
	get_ci_new `pts_sd' `pts_mean'
	estadd local alpha1 "`r(alpha)'"
	estadd local ci1 "`r(ci)'"
	estadd local alpha2 "`r(alpha2)'"
	estadd local ci2 "`r(ci2)'"
	estadd local sample "`smpl'"
	restore
	
}

esttab orig_Prolific new_Prolific orig_Berkeley new_Berkeley orig_BU new_BU using additional_analysis/tables/table6_new.tex, replace /// 
booktabs obslast /// 
scalars("alpha1 \$-R''/R'(\bar{a}_{pop})\$" "ci1 95 percent CI" "alpha2 \$-R''/R'(\bar{a}_{pop})\times SD \$" "ci2 95 percent CI" "sample Sample" "nex \hline") substitute(\_ \) /// 
mgroups("Table 6 column (2)" "Table 6 column (4)" "Table 6 column (6)", pattern(1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span) ///
mtitles("Original" "Corrected" "Original" "Corrected" "Original" "Corrected") nonumbers nobaselevels ///
star(* 0.1 ** 0.05 *** 0.01) se nonotes ///
coeflabels(interval "Points (100s)" c.interval#c.interval "Points (100s) sqd." _cons Constant)
