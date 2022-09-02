*===================================================*

*****			DESCRIPTIVE ANALYSIS			*****

*===================================================*

// Use data from "02_sequence_generation.do"
use "$ggstemp/state_sequence_set.dta", clear


recode child 	(1 = 1 "1st born") ///
				(2 = 2 "2nd born") ///
				(3/16 = 3 "Third born or higher") ///
				(else=.), gen(parity)

* =========================================
* Union descriptives
* =========================================

// Number of step parents 
egen num_step0 = noccur(DSS0), string(2)
forvalues j = 11(12)179{

	local year = (`j' + 1)/12
	egen num_step`year' = noccur(DSS`j'), string(2)

}

// Number of partners
gen num_partners0 = num_step0
replace num_partners0 = num_step0 + 1 if FAMILY_STATE0 == 1
forvalues j = 11(12)179{

	local year = (`j' + 1)/12

	gen num_partners`year' = num_step`year'
	replace num_partners`year' = num_step`year' + 1 if FAMILY_STATE0 == 1

}



*=============
*= Ever lived:

* Ever lived w/ single mother
gen ever_single0 = 0
replace ever_single0 = 100 if strpos(DSS0,"0") > 0
forvalues j = 11(12)179{

	local year = (`j' + 1)/12

	gen ever_single`year' = 0
	replace ever_single`year' = 100 if strpos(DSS`j',"0") > 0
}

gen lone_mother = ever_single0

* Ever lived w/ 1+ step-parent if born to lone mother or experienced parental union disruption
gen ever_1step0 = .
replace ever_1step0 = 0 if ever_single0 >= 1
replace ever_1step0 = 100 if num_step0 >= 1

forvalues year = 1/15{

	gen ever_1step`year' =0
	replace ever_1step`year' = 100 if num_step`year' >= 1
}


* Ever lived w/ 2+ step-parents
gen ever_2step0 =0
replace ever_2step0 = 100 if num_step0 >= 2
forvalues year = 1/15{

	gen ever_2step`year' =0
	replace ever_2step`year' = 100 if num_step`year' >= 2
}

* Ever lived w/ 3+ step parents
gen ever_3step0 =0
replace ever_3step0 = 100 if num_step0 >= 3
forvalues year = 1/15{

	gen ever_3step`year' =0
	replace ever_3step`year' = 100 if num_step`year' >= 3
}


* Ever experienced parental separation if born in union
gen ever_sep0 = .
replace ever_sep0 = 0 if FAMILY_STATE0==1
replace ever_sep0 = 100 if DSS0 > "1"
forvalues j = 11(12)179{

	local year = (`j' + 1)/12

	gen ever_sep`year' = .
	replace ever_sep`year' = 0 if FAMILY_STATE0==1
	replace ever_sep`year' = 100 if DSS`j' > "1"
}


*=============
*=Family types:

label define familyl 1 "P" 2 "P-Si" 3 "Si" 4 "P-Si-St" 5 "Si-St" 6 "P-St"		///
	7 "Si-St-Si" 8 "P-Si-St-Si" 99 "Other"
	
gen family_types0 = .
replace family_types0 = 1 if DSS0 == "1"
replace family_types0 = 2 if DSS0 == "10"
replace family_types0 = 3 if DSS0 == "0"
replace family_types0 = 4 if DSS0 == "102"
replace family_types0 = 5 if DSS0 == "02"
replace family_types0 = 6 if DSS0 == "12"
replace family_types0 = 7 if DSS0 == "020"
replace family_types0 = 8 if DSS0 == "1020"
replace family_types0 = 99 if family_types0 == .
	
label values family_types0 familyl

forvalues j = 11(12)179{

	local year = (`j' + 1)/12		

	gen family_types`year' = .
	replace family_types`year' = 1 if DSS`j' == "1"
	replace family_types`year' = 2 if DSS`j' == "10"
	replace family_types`year' = 3 if DSS`j' == "0"
	replace family_types`year' = 4 if DSS`j' == "102"
	replace family_types`year' = 5 if DSS`j' == "02"
	replace family_types`year' = 6 if DSS`j' == "12"
	replace family_types`year' = 7 if DSS`j' == "020"
	replace family_types`year' = 8 if DSS`j' == "1020"
	replace family_types`year' = 99 if family_types`year' == .
	
	label values family_types`year' familyl
}


* =========================================
* Sibling descriptives
* =========================================

			***		NUMBERS		***
			
label define siblingl 0 "None" 1 "One" 2 "Two" 3 "Three or more"

// Number of full siblings. Older, younger and total
gen older_fullsibs 		= FULLSIB_STATE0
replace older_fullsibs	= 3 if older_fullsibs > 3 & !missing(older_fullsibs)

gen younger_fullsibs0	= FULLSIB_STATE0 - FULLSIB_STATE0

forvalues j = 11(12)179{

	local year = (`j' + 1)/12		
	gen younger_fullsibs`year'	= FULLSIB_STATE`j' - FULLSIB_STATE0
}

gen total_fullsibs0	= FULLSIB_STATE0
forvalues j = 11(12)179{

	local year = (`j' + 1)/12	
	gen total_fullsibs`year'	= FULLSIB_STATE`j'
}

// Number of half siblings. Older, younger and total
gen older_halfsibs 			= .
replace older_halfsibs 		= HALFSIB_STATE0

gen younger_halfsibs0 = HALFSIB_STATE0 - HALFSIB_STATE0
forvalues j = 11(12)179{

	local year = (`j' + 1)/12		
	gen younger_halfsibs`year'	= HALFSIB_STATE`j' - HALFSIB_STATE0
}

gen total_halfsibs0	= HALFSIB_STATE0
forvalues j = 11(12)179{

	local year = (`j' + 1)/12	
	gen total_halfsibs`year'	= HALFSIB_STATE`j'
}

// Number of siblings. Older, younger and total

gen older_sibs				= older_fullsibs + older_halfsibs

gen younger_sibs0 	= younger_fullsibs0 + younger_halfsibs0
forvalues j = 11(12)179{

	local year = (`j' + 1)/12	
	gen younger_sibs`year' 	= younger_fullsibs`year' + younger_halfsibs`year'
}

gen total_sibs0 	= total_fullsibs0 + total_halfsibs0
forvalues j = 11(12)179{

	local year = (`j' + 1)/12	
	gen total_sibs`year' 	= total_fullsibs`year' + total_halfsibs`year'
}


						


// Recode siblings
recode *sibs*		 	(0 = 0 "None") ///
						(1 = 1 "One")	///
						(2 = 2 "Two")	///
						(3/16 = 3 "Three or more") ///
						(else=.),pre(R_) label(siblingl)

						
// Sibling position relative to full- and half-siblings
label define sib_positionl 1 "Older" 2 "Younger" 3 "Older and younger"

forvalues year = 0/15{
gen full_sibling_position`year' = .
replace full_sibling_position`year' = 1 if older_fullsibs == 0 & 				///
											younger_fullsibs`year' > 0
replace full_sibling_position`year' = 2 if older_fullsibs > 0 & 				///
											younger_fullsibs`year' == 0
replace full_sibling_position`year' = 3 if older_fullsibs > 0 & 				///
											younger_fullsibs`year' > 0

label value full_sibling_position`year' sib_positionl
}


forvalues year = 0/15{
gen half_sibling_position`year' = .
replace half_sibling_position`year' = 1 if older_halfsibs == 0 & 				///
											younger_halfsibs`year' > 0
replace half_sibling_position`year' = 2 if older_halfsibs > 0 & 				///
											younger_halfsibs`year' == 0
replace half_sibling_position`year' = 3 if older_halfsibs > 0 & 				///
											younger_halfsibs`year' > 0

label value half_sibling_position`year' sib_positionl
}


			***		PROPORTIONS		***

// Proportion with at least one sibling. Total, older and younger
forvalues year = 0/15{
	gen one_sib`year' = 0
	replace one_sib`year' = 100 if total_sibs`year' > 0 
}

gen one_older_sib = 0 
replace one_older_sib = 100 if older_sibs > 0

forvalues year = 0/15{
	gen one_younger_sib`year' = 0
	replace one_younger_sib`year' = 100 if younger_sibs`year' > 0 
}

// Proportion with at least one full fullsibling. Total, older and younger
forvalues year = 0/15{
	gen one_fullsib`year' = 0
	replace one_fullsib`year' = 100 if total_fullsibs`year' > 0 
}

gen one_older_fullsib = 0 
replace one_older_fullsib = 100 if older_fullsibs > 0

forvalues year = 0/15{
	gen one_younger_fullsib`year' = 0
	replace one_younger_fullsib`year' = 100 if younger_fullsibs`year' > 0 
}

// Proportion with at least one half sibling. Total, older and younger
forvalues year = 0/15{
	gen one_halfsib`year' = 0
	replace one_halfsib`year' = 100 if total_halfsibs`year' > 0 
}

gen one_older_halfsib = 0 
replace one_older_halfsib = 100 if older_halfsibs > 0

forvalues year = 0/15{
	gen one_younger_halfsib`year' = 0
	replace one_younger_halfsib`year' = 100 if younger_halfsibs`year' > 0 
}

label define proportionl 100 "Yes" 0 "No" 
label value ever_* one_* proportionl


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

// Reshape

drop *_STATE*
drop DSS*

reshape long num_step num_partners ever_single ever_1step ever_2step ever_3step	///
		ever_sep family_types younger_fullsibs R_younger_fullsibs 				///
		total_fullsibs R_total_fullsibs younger_halfsibs R_younger_halfsibs		///
		total_halfsibs R_total_halfsibs younger_sibs R_younger_sibs total_sibs 	///
		R_total_sibs one_sib one_younger_sib one_fullsib one_younger_fullsib 	///
		one_halfsib one_younger_halfsib full_sibling_position 					///
		half_sibling_position, i(childID) j(age)

label variable age "Age of child"
label val age
label variable COHORT "Birth cohort"

		
save "$ggstemp/descriptives_reshaped.dta", replace


