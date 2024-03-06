* New order variable

gen order = .
* Define an order variable 
forvalues p = 1 / 3 {
	replace order = `p' if round == "anom" & substr(condition,`p',1) =="A"
	replace order = `p' if round == "recog" & substr(condition,`p',1) =="R"
	replace order = `p' if round == "earn" & substr(condition,`p',1) =="E"
}
