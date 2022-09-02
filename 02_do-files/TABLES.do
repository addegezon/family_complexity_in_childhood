*===================================================*

*****			TABLE GENERATION				*****

*===================================================*

// Use data from "03_descriptive_analysis"
use "$ggstemp/descriptives_reshaped.dta", clear


keep if inlist(age, 0, 1, 2, 3, 4, 6, 9, 12, 15)

* =========================================
* UNION
* =========================================

*===============
*= Descriptives

// Ever lived
tabout COUNTRY  if (age == 15 & COHORT >= 1980)									///
using "$tables/ever_lived.tex", replace 										///
style(tex) font(bold) twidth(17) sum f(1) 										///
c(mean lone_mother mean ever_sep mean ever_1step)								///
clab(% % %) 	h2c(1 1 1) 														///
h2(Born_to_lone_mother Ever_separated_parents Ever_lived_with_step_parent) 		///
caplab(ever_lived)	dropr(22)													///
title(Percentage born to lone mother, experiencing parental 					///
	separation and those with at least one step parent)							///
fn(Source: Harmonized Histories)

// Family types
tabout COUNTRY family_types if (age == 15 & COHORT >= 1980)						///	 
using "$tables/family_types.tex", replace 										///
style(tex) font(bold) twidth(22.5) c(row) f(1) 									///
h1(nil) dropr(22)																///						
title(Percentage different family types) caplab(family_types)					///
fn(Abbrevations: P = living with both parents, Si = living with a single mother	///
St = living with mother and step-parent)


* =========================================
* SIBLINGS
* =========================================


// Number of full-siblings
tabout  family_types R_total_fullsibs	///
using "$tables/fullsibs.tex", replace 										 ///
style(tex) font(bold) twidth(17)												///
c(row) f(1) layout(rb)	h1(Proportion with number of full-siblings)				///
title(Full-sibling composition) caplab(fullsibs)								///
fn(Source: Harmonized Histories)

// Number of half-siblings
tabout  family_types R_total_halfsibs 									///
using "$tables/halfsibs.tex", replace 											///
style(tex) font(bold) twidth(17)												///
c(row) f(1) layout(rb)	h1(Proportion with number of half-siblings)				///
title(Half-sibling composition) caplab(halfsibs)								///
fn(Source: Harmonized Histories)

// Sibling position relative to full-siblings
tabout family_types full_sibling_position 							///
using "$tables/full_sibling_position.tex", replace 								///
style(tex) font(bold) twidth(14) c(row) f(1) 									///
h1(Position relative to full-sibling(s))										///
title(Full-sibling position) caplab(siblingposfull)


// Sibling position relative to half-siblings
tabout family_types half_sibling_position 							///
using "$tables/half_sibling_position.tex", replace ///
style(tex) font(bold) twidth(14) c(row) f(1) 									///
h1(Position relative to half-sibling(s))										///
title(Half-sibling position) caplab(siblingposhalf)

// Average age differences
tabout family_types  using "$tables/age_diff.tex", replace 							///
style(tex) font(bold) twidth(17) sum f(1) font(bold)							///
c(median age_diff_fullsib p25 age_diff_fullsib p75 age_diff_fullsib 				///
	median age_diff_halfsib p25 age_diff_halfsib p75 age_diff_halfsib)  			///
clab(Median Q1 Q3 Median Q1 Q3) h2c(3 3) h2(Closest_full-sibling_(years) 			///
										Closest_half_sibling_(years)) 			///
title(Age difference to closest full- and half-sibling) caplab(agediff)			///
fn(Source: Harmonized Histories)
