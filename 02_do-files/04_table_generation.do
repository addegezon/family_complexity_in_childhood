*===================================================*

*****			TABLE GENERATION				*****

*===================================================*

// Use data from "03_descriptive_analysis"
use "$ggstemp/descriptives_reshaped.dta", clear

********************************************************************************
********************************************************************************



// Summary
tabout COUNTRY using "$tables/summary.tex", replace 							///
style(tex) font(bold) twidth(14) sum f(0) 	font(bold)							///
c(min YEAR_S max YEAR_S min KID_BYEAR max KID_BYEAR N childID N id)					///
h2c(2 2 1 ) h2(Survey_years Child_cohorts Total_children Total_mothers) 			///
clab(_ _ _ _ _ )																///
title(Summary statistics of study population) caplab(summary)

* =========================================
* UNION
* =========================================


// Number of parents with quantiles
tabout COUNTRY using "$tables/num_parents.tex", replace 						///
style(tex) font(bold) twidth(14) sum f(2 0 0 0 0) 	font(bold)					///
c(mean num_partners p5 num_partners p25 num_partners p75 num_partners p95 num_partners)					///
h2c(1 1 1 1 1) h2(Mean P5 P25 P75 P95) 			///
clab(_ _ _ _ _ )																///
title(Average number of partners to mother) caplab(num_parents)


*=============
*= Ever lived:

tabout COUNTRY using "$tables/ever_lived.tex", replace 							///
style(tex) font(bold) twidth(17) sum f(1) 									///
c(mean lone_mother mean ever_sep mean ever_1step mean ever_2step)				///															///
clab(% % % % ) 	h2c(1 1 1 1) 													///
h2(Ever_single_mother Ever_separated_parent Ever_1_step-parents 				///
Ever_2_step-parents) caplab(ever_lived)											///
title(Percentage who ever experienced having a single mother, parental 			///
	separation and 1-2+ step parents)											///
fn(Source: Harmonized Histories)

// Country and age specific
levelsof COUNTRY, local(levels) 
foreach country of local levels {

local country_name : label countryl `country'
tabout COUNTRY using "$tables/ever_lived_`country'.tex", replace 				///
style(tex) font(bold) twidth(17) sum f(2 2 ) 									///
c(mean ever_single mean ever_sep mean ever_1step mean ever_2step)				///
clab(% % % % ) 	h2c(1 1 1 1) 													///
h2(Ever_single_mother Ever_separated_parent Ever_1_step-parents 				///
Ever_2_step-parents) caplab(ever_lived_`country')								///
title(Percentage who ever experienced having a single mother, parental 			///
	separation and 1-2+ step parents in `country_name')							///
fn(Source: Harmonized Histories)
}


*=============
*=Family types:

tabout COUNTRY family_types  using "$tables/family_types.tex", replace 			///
style(tex) font(bold) twidth(25) c(row) f(1) 									///
title(Percentage different family types) caplab(family_types)					///
fn(Source: Harmonized Histories)


tab family_types, gen(family_type_)


label variable family_type_1 "P"
label variable family_type_2 "P-Si"
label variable family_type_3 "Si"
label variable family_type_4 "P-Si-St"
label variable family_type_5 "Si-St"
label variable family_type_6 "P-St"
label variable family_type_7 "Si-St-Si"
label variable family_type_8 "P-Si-St-Si"
label variable family_type_9 "Other"

	

graph bar family_type_1-family_type_9 if COUNTRY==7521 & COHORT==1990, over(age, label(angle(45)))  stack
graph export "$graphs/family_types.eps", replace



// Lone mother, separation
tabout COUNTRY using "$tables/single.tex", replace 								///
style(tex) font(bold) twidth(17) sum f(2 2 ) 	font(bold)						///
c(mean lone_mother mean sep_exp) 												///  
clab(Proportion Proportion) 													///
h2c(1 1) h2(Born_to_lone_mother Experienced_separation)	caplab(single)		///
title(Childrens experience of lone motherhood and union disruption)			///
fn(Source: Harmonized Histories)

// Time with parents, in step family & with lone mother
tabout COUNTRY using "$tables/parents_step.tex", replace 						///
style(tex) font(bold) twidth(17) sum f(1) 	font(bold)							///
c(mean time_parents p25 time_parents p75 time_parents 							///
	mean time_step p25 time_step p75 time_step									///
	mean time_single p25 time_single p75 time_single) caplab(parents)			///
clab(Mean Q1 Q3 Mean Q1 Q3 Mean Q1 Q3) h2c(3 3 3) 								///
h2(With_both_parents In_step_family With_single_mother) 						/// 
title(Years spent in different family forms)									///
fn(Source: Harmonized Histories)


* =========================================
* SIBLINGS
* =========================================


// Number of full-siblings
tabout  COUNTRY R_total_fullsibs if age==15 & COHORT>=1980	///
using "$tables/fullsibs.tex", replace 										 ///
style(tex) font(bold) twidth(17)												///
c(row) f(1) layout(rb)	h1(Proportion with number of full-siblings)													///
title(Full-sibling composition) caplab(fullsibs)								///
fn(Source: Harmonized Histories)

// Number of half-siblings
tabout  COUNTRY R_total_halfsibs if age==15 & COHORT>=1980 										///
using "$tables/halfsibs.tex", replace 											///
style(tex) font(bold) twidth(17)												///
c(row) f(1) layout(rb)	h1(Proportion with number of half-siblings)				///
title(Half-sibling composition) caplab(halfsibs)								///
fn(Source: Harmonized Histories)

// Sibling position relative to full-siblings
tabout COUNTRY full_sibling_position if age==15 & COHORT>=1980 								///
using "$tables/full_sibling_position.tex", replace 								///
style(tex) font(bold) twidth(14) c(row) f(1) 									///
h1(Position relative to full-sibling(s))										///
title(Full-sibling position) caplab(siblingposfull)


// Sibling position relative to half-siblings
tabout COUNTRY half_sibling_position if age==15 & COHORT>=1980 								///
using "$tables/half_sibling_position.tex", replace ///
style(tex) font(bold) twidth(14) c(row) f(1) 									///
h1(Position relative to half-sibling(s))										///
title(Half-sibling position) caplab(siblingposhalf)

// Average age differences
tabout COUNTRY if age==15 & COHORT>=1980 	using "$tables/age_diff.tex", replace 							///
style(tex) font(bold) twidth(17) sum f(1) font(bold)							///
c(median age_diff_fullsib p25 age_diff_fullsib p75 age_diff_fullsib 				///
	median age_diff_halfsib p25 age_diff_halfsib p75 age_diff_halfsib)  			///
clab(Median Q1 Q3 Median Q1 Q3) h2c(3 3) h2(Closest_full-sibling_(years) 			///
										Closest_half_sibling_(years)) 			///
title(Age difference to closest full- and half-sibling) caplab(agediff)			///
fn(Source: Harmonized Histories)



************************TEST*****************************************************





tabout age COHORT  if COUNTRY==7521 using "$tables/test.tex", replace ///
style(tex) font(bold) c(mean ever_sep) f(0c) sum clab(%) ///
twidth(9) h1(Proportion ever experienced parental separation) h3(nil) ///
title(Sweden: Proportion ever experienced parental separation) ///
fn(Harmonized histories) caplab(test)

