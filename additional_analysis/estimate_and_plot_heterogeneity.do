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

encode sample, gen(sample_nr)

** The original paper drops observations if points are too high. 
gen high_pts = 0
foreach round in anom earn recog{
	replace high_pts = 1 if `round'pts > 3000
	}


* Reshape the data. 
reshape long wtp nwtp, i(id) j(interval)

replace interval = interval + .5
replace anompts = anompts / 100

* Define a new id variable that runs without gaps from 1 to N.
egen idnorm = group(id sample)

* Classify participants as insensitive who always make the same wtp statement.
by idnorm (wtp), sort: gen insensitive = wtp[1] == wtp[_N]

* Loop through all participants in the data sets and recover individual-level 
* OLS parameters
tempname tmpnm 
tempfile tmpfl
postfile `tmpnm' c a b idnorm using `tmpfl'

qui sum idnorm
forvalues p = `r(min)' / `r(max)' {
	qui sum sample_nr if idnorm == `p'
	local smpl = r(mean)
	qui reg wtp c.interval c.interval#c.interval if idnorm == `p' & interval < 17, clus(id)
	
 	post `tmpnm' (_b[_cons]) (_b[interval]) (_b[c.interval#c.interval]) (`p')
	
	}

postclose `tmpnm'

merge n:1 idnorm using `tmpfl'
drop _merge

* Classify types. 
gen convex = b > 0 
gen extreme_point = -.5 * a / b 
gen extreme_point_interior = inrange(extreme_point, .5, 16.5)
gen extreme_point_left = extreme_point < .5 
gen extreme_point_right = extreme_point > 16.5 

gen inverse_u = convex * extreme_point_interior
gen hump_shaped = (1-convex)*extreme_point_interior
gen decreasing = convex * extreme_point_right + (1-convex)*extreme_point_left
gen increasing = convex * extreme_point_left + (1-convex)*extreme_point_right

gen type = 0 
replace type = 1 if increasing == 1
replace type = 2 if decreasing == 1
replace type = 3 if hump_shaped == 1
replace type = 4 if inverse_u == 1
replace type = 5 if insensitive == 1 
replace type = 6 if incoherent == 1

forvalues p = 1 / 6 {
	gen share`p' = type == `p'
}

* Bar graph of the type distributions in the different samples.
preserve 
collapse (mean) share1-share6, by(sample) 

reshape long share, i(sample) j(type) 

drop if sample == "BU QM221"

replace type = type - .2 if sample == "Prolific"
replace type = type + .2 if sample == "BU"

label define tp 1 "Increasing" 2 "Decreasing" 3 "Hump-shaped" 4 "Inverse-U" 5 "Insensitive" 6 "Incoherent"
label values type tp

twoway (bar share type if sample == "Prolific",horizontal barwidth(.2) fcolor(gs12) lcolor(black)) ///
(bar share type if sample == "Berkeley",horizontal barwidth(.2) fcolor(midblue) lcolor(black)) ///
(bar share type if sample == "BU",horizontal barwidth(.2) fcolor(black) lcolor(black)) ///
, ylabels(1 2 3 4 5 6 7, valuelabel)  legend(order(1 "Prolific" 2 "Berkeley" 3 "BU") pos(6) cols(3)) xtitle("Fraction") ytitle(" ")
graph export additional_analysis/figures/type_distribution.png, replace

restore 

* Binned scatter plots of the PRU by PRU type.
replace wtp = wtp / 25 if sample != "Prolific"
replace wtp = wtp / 10 if sample == "Prolific"
replace interval = interval * 100

binscatter wtp interval if type == 1, xlabel(50(400)1650) ytitle("Normalized WTP") xtitle("Point score") /// 
title("Increasing") mcolor(red) lcolor(blue) xline(850) yline(0) name(g1, replace) linetype(qfit)
binscatter wtp interval if type == 2, xlabel(50(400)1650) ytitle("Normalized WTP") xtitle("Point score") /// 
title("Decreasing") mcolor(red) lcolor(blue) xline(850) yline(0) name(g2, replace) linetype(qfit)
binscatter wtp interval if type == 3, xlabel(50(400)1650) ytitle("Normalized WTP") xtitle("Point score") /// 
title("Hump-shaped") mcolor(red) lcolor(blue) xline(850) yline(0) name(g3, replace) linetype(qfit)
binscatter wtp interval if type == 4, xlabel(50(400)1650) ytitle("Normalized WTP") xtitle("Point score") /// 
title("Inverse-U") mcolor(red) lcolor(blue) xline(850) yline(0) name(g4, replace) linetype(qfit)

graph combine g1 g2 g3 g4, ycommon
graph export additional_analysis/figures/type_distribution_binscatter.png, replace

