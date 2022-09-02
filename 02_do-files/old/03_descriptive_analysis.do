*===================================================*

*****			DESCRIPTIVE ANALYSIS			*****

*===================================================*

// Use data from "02_sequence_generation.do"
use "$ggstemp/state_sequence_DSS.dta", replace


recode child 	(1 = 1 "1st born") ///
				(2 = 2 "2nd born") ///
				(3/16 = 3 "Third born or higher") ///
				(else=.), gen(parity)
				


* =========================================
* Union descriptives
* =========================================

// Number of partner's mother have
/*
egen last_partner = rowmax(PARTNER_STATE*)
egen first_partner = rowmin(PARTNER_STATE*)


gen num_partner = last_partner - first_partner + 1
replace num_partner = 0 if missing(num_partner)
*/

// Time with each partner
egen last_partner = rowmax(PARTNER_STATE*)
sum last_partner
local x = r(max)

forvalues i = 1/`x'{

	gen time_partner`i' = 0
	
	forvalues j = 0/167{
	
	replace time_partner`i' = time_partner`i' + 1 if PARTNER_STATE`j' == `i'
	
	}
}

// Total time partnered
egen time_partnered = rowtotal(time_partner*)
replace time_partnered = time_partnered/12

// Time with both parents
gen time_parents = 0
replace time_parents = time_partner1 /12 if PARTNER_STATE0 == 1


// Time spent with single mother
gen time_single = 0 

forvalues i = 0/167{

	replace time_single = time_single + 1/12 if PARTNER_STATE`i'==0 

}


// Time spent in stepfamily
gen time_step = 14
replace time_step = time_step - time_parents - time_single


// Experience separation at least once
gen sep_exp = 0 

forvalues current = 1/167{
	
	local previous = `current' - 1
	replace sep_exp = 1 if PARTNER_STATE`current' > PARTNER_STATE`previous' 
	
}

// Born to lone mother
gen lone_mother = 0
replace lone_mother = 1 if PARTNER_STATE0==0



*=============
*= Ever lived:

* Ever lived w/ single mother
gen pc_ever_single =0
forvalues j = 0/167{

replace pc_ever_single = 100 if PARTNER_STATE`j' ==0 & pc_ever_single== 0

}

* Ever lived w/ 1+ step-parent
gen pc_ever_1step =0
forvalues j = 0/167{

replace pc_ever_1step = 100 if 	((PARTNER_STATE0 ==0 & PARTNER_STATE`j' == 1) |	///
								(PARTNER_STATE0 ==1 & PARTNER_STATE`j' == 2)) &	///
								pc_ever_1step == 0

}

* Ever lived w/ 2+ step-parents
gen pc_ever_2step =0
forvalues j = 0/167{

replace pc_ever_2step = 100 if 	((PARTNER_STATE0 ==0 & PARTNER_STATE`j' == 2) |	///
								(PARTNER_STATE0 ==1 & PARTNER_STATE`j' == 3)) &	///
								pc_ever_2step == 0

}

* Ever lived w/ 3+ step parents
gen pc_ever_3step =0
forvalues j = 0/167{

replace pc_ever_3step = 100 if 	((PARTNER_STATE0 ==0 & PARTNER_STATE`j' == 3) |	///
								(PARTNER_STATE0 ==1 & PARTNER_STATE`j' == 4)) &	///
								pc_ever_3step == 0

}
* Ever experienced parental separation if born in union
gen pc_ever_sep = .
replace pc_ever_sep = 0 if PARTNER_STATE0==1

forvalues j = 0/167{

replace pc_ever_sep = 100 if 	PARTNER_STATE0==1 & PARTNER_STATE`j' > 1 &		///
								pc_ever_sep==0

}


*=============
*=Family types:

egen single_count = anycount(PARTNER_STATE0-PARTNER_STATE167), values(0)
egen parent_count = anycount(PARTNER_STATE0-PARTNER_STATE167), values(1)
egen partner_count = 

gen family_type = .
replace family_type = 1 if single_count == 168
replace family_type = 2 if parent_count == 168
replace family_type = 3 if PARTNER_STATE0== 1 & single_count > 0 &				///
							last_partner==1 
replace family_type = 4 if PARTNER_STATE0==0 & last_partner==1 &				///
								PARTNER_STATE167==0
replace family_type = 5 if	PARTNER_STATE0 ==1 & single_count > 0 &				///
										last_partner==2 & PARTNER_STATE167==2
replace family_type = 6 if PARTNER_STATE0 == 0 & last_partner==1 &	///
										PARTNER_STATE167 == 0
replace family_type = 7 if PARTNER_STATE0 ==1 & 								///
						single_count > 0 & last_partner==2 & PARTNER_STATE167==0
replace family_type = 8 if PARTNER_STATE0 == 0 & last_partner==2 &	///
		PARTNER_STATE167 == 2


label define family_typel 1 "Single" 2 "Parents" 								///
3 "Parents, single" 4 "Single, step" 5 "Parents - single - step"				///
6 "Single, step - single" 7 "Parents, single, step, single"						///
8 "Single, step, single, step"


label values family_type family_typel  







* Only single mother
gen pc_only_single = .
replace pc_only_single = 0 if single_count < 168
replace pc_only_single = 100 if single_count == 168

* Only parents
gen pc_only_parents = .
replace pc_only_parents = 0 if parent_count < 168
replace pc_only_parents = 100 if parent_count == 168

* Parents, then single mother
gen pc_parents_single = 0
replace pc_parents_single = 100 if 	PARTNER_STATE0== 1 & single_count > 0 &		///
									last_partner==1 

* Single mother then one step parent
gen pc_single_step = 0
replace pc_single_step = 100 if PARTNER_STATE0==0 & last_partner==1 &			///
								PARTNER_STATE167==0

* Parents, single mother and one step parent
gen pc_parent_single_step = 0
replace pc_parent_single_step = 100 if 	PARTNER_STATE0 ==1 & single_count > 0 &	///
										last_partner==2 & PARTNER_STATE167==2

* Single, step, single
gen pc_single_step_single = 0
replace pc_single_step_single = 100 if PARTNER_STATE0 == 0 & last_partner==1 &	///
										PARTNER_STATE167 == 0

* Parents, single, step, single	
gen pc_parent_single_step_single = 0
replace pc_parent_single_step_single = 100 if PARTNER_STATE0 ==1 & 				///
		single_count > 0 & last_partner==2 & PARTNER_STATE167==0

* Single, step, single, step
gen pc_single_step_single_step = 0
replace pc_single_step_single_step = 100 if PARTNER_STATE0 == 0 & last_partner==2 &	///
		PARTNER_STATE167 == 2
										
egen summering = rowtotal(pc_only* pc_sing* pc_par*)

* =========================================
* Sibling descriptives
* =========================================

			***		NUMBERS		***

// Number of full siblings. Older, younger and total
gen older_fullsibs 			= .
replace older_fullsibs 		= FULLSIB_STATE0


gen younger_fullsibs 		= .
replace younger_fullsibs	= FULLSIB_STATE167 - FULLSIB_STATE0

gen total_fullsibs			= FULLSIB_STATE167

// Number of half siblings. Older, younger and total
gen older_halfsibs 			= .
replace older_halfsibs 		= HALFSIB_STATE0

gen younger_halfsibs 		= .
replace younger_halfsibs	= HALFSIB_STATE167 - HALFSIB_STATE0

gen total_halfsibs			= HALFSIB_STATE167

// Number of siblings. Older, younger and total
gen younger_sibs 			= younger_fullsibs + younger_halfsibs
gen older_sibs				= older_fullsibs + older_halfsibs
gen total_sibs 				= total_fullsibs + total_halfsibs


			***		PROPORTIONS		***

// Proportion with at least one sibling. Total, older and younger
gen one_sib = 0
replace one_sib = 1 if total_sibs > 0 

gen one_older_sib = 0 
replace one_older_sib = 1 if older_sibs > 0

gen one_younger_sib = 0
replace one_younger_sib = 1 if younger_sibs > 0

// Proportion with at least one full sibling. Total, older and younger
gen one_fullsib = 0
replace one_fullsib = 1 if total_fullsibs > 0 

gen one_older_fullsib = 0 
replace one_older_fullsib = 1 if older_fullsibs > 0

gen one_younger_fullsib = 0
replace one_younger_fullsib = 1 if younger_fullsibs > 0

// Proportion with at least one half sibling. Total, older and younger
gen one_halfsib = 0
replace one_halfsib = 1 if total_halfsibs > 0 

gen one_older_halfsib = 0 
replace one_older_halfsib = 1 if older_halfsibs > 0

gen one_younger_halfsib = 0
replace one_younger_halfsib = 1 if younger_halfsibs > 0


			***		AGE DIFFERENCES		***

// Age difference to closest full sibling
gen age_diff_fullsib = .
forvalues j = 1/16{

replace age_diff_fullsib = 	abs(KIDBORN_YM - IKID_YM`j')/12 if 		///
			abs(KIDBORN_YM - IKID_YM`j') < age_diff_fullsib	 &	///
			(PARTNER_STATE0 	== 	PARENT_KID`j')	&	///
			(PARTNER_STATE0 !=0 | PARENT_KID`j' !=.)

}

// Age difference to closest half-sibling
gen age_diff_halfsib = .
forvalues j = 1/16{

replace age_diff_halfsib = 	abs(KIDBORN_YM - IKID_YM`j')/12 if 					///
							abs(KIDBORN_YM - IKID_YM`j') < age_diff_halfsib	 &	///
							((PARTNER_STATE0 			!= 	PARENT_KID`j') |	///
							(PARTNER_STATE0 ==0 | PARENT_KID`j' ==.))

}




save "$ggstemp/descriptives.dta", replace

