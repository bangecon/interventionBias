use "D:\Google Drive\Research - Health\Readmissions\PCORI\interventionBias\data\data.dta", clear
replace read30_01 = read30_01 - 1
logit read30_01 i.hosp i.age_cats i.medsugpt i.icustay i.gender i.race i.ethnicity i.maritals i.mortrisk i.severity lnlos i.mdc
outreg2 using stataResults, replace excel ctitle("Full Sample - Betas")
margins, dydx(_all) post
outreg2 using stataResults, excel ctitle("Full Sample - Margins")
use "D:\Google Drive\Research - Health\Readmissions\PCORI\interventionBias\data\bs.dta", clear
replace read30_01 = read30_01 - 1
logit read30_01 i.hosp i.age_cats i.medsugpt i.icustay i.gender i.race i.ethnicity i.maritals i.mortrisk i.severity lnlos i.mdc
outreg2 using stataResults, excel ctitle("BS Sample - Betas")
margins, dydx(_all) post
outreg2 using stataResults, excel ctitle("BS Sample - Margins")