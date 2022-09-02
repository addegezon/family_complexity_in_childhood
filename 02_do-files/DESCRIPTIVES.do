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
egen num_step = noccur(DSS179), string(2)


// Number of partners
gen num_partners = num_step
replace num_partners = num_step + 1 if FAMILY_STATE0 == 1



*=============
*= Ever lived:

* Ever lived w/ single mother
gen ever_single = 0
replace ever_single = 100 if strpos(DSS179,"0") > 0

gen lone_mother = 0
replace lone_mother = 100 if strpos(DSS0,"0") > 0

* Ever lived w/ 1+ step-parent if born to lone mother or experienced parental union disruption
gen ever_1step = .
replace ever_1step = 0 if ever_single >= 1
replace ever_1step = 100 if num_step >= 1


* Ever experienced parental separation if born in union
gen ever_sep = .
replace ever_sep = 0 if FAMILY_STATE0==1
replace ever_sep = 100 if DSS179 > "1"


*=============
*=Family types:

label define familyl 1 "P" 2 "P-Si" 3 "Si" 4 "P-Si-St" 5 "Si-St" 6 "P-St"		///
	7 "Si-St-Si" 8 "P-Si-St-Si" 99 "Other"
	
gen family_types = .
replace family_types = 1 if DSS179 == "1"
replace family_types = 2 if DSS179 == "10"
replace family_types = 3 if DSS179 == "0"
replace family_types = 4 if DSS179 == "102"
replace family_types = 5 if DSS179 == "02"
replace family_types = 6 if DSS179 == "12"
replace family_types = 7 if DSS179 == "020"
replace family_types = 8 if DSS179 == "1020"
replace family_types = 99 if family_types == .
	
label values family_types familyl

*================
*=Time in states:

gen time_in_P = 0
gen time_in_Si = 0
gen time_in_St = 0
gen time_in_state = 0
forvalues j = 0/179{
replace time_in_Si = time_in_Si + 1/12 if FAMILY_STATE`j' == 0
replace time_in_P = time_in_P + 1/12 if FAMILY_STATE`j' == 1
replace time_in_St = time_in_St + 1/12  if FAMILY_STATE`j' == 2
}
replace 

* =========================================
* Sibling descriptives
* =========================================

			***		NUMBERS		***
			
label define siblingl 0 "None" 1 "One" 2 "Two" 3 "Three or more"

// Number of full siblings. Older, younger and total
gen older_fullsibs 		= FULLSIB_STATE0
replace older_fullsibs	= 3 if older_fullsibs > 3 & !missing(older_fullsibs)

gen younger_fullsibs	= FULLSIB_STATE179 - FULLSIB_STATE0

gen total_fullsibs	= FULLSIB_STATE179


// Number of half siblings. Older, younger and total
gen older_halfsibs 			= .
replace older_halfsibs 		= HALFSIB_STATE0

gen younger_halfsibs = HALFSIB_STATE179 - HALFSIB_STATE0


gen total_halfsibs	= HALFSIB_STATE179


// Number of siblings. Older, younger and total

gen older_sibs	= older_fullsibs + older_halfsibs

gen younger_sibs = younger_fullsibs + younger_halfsibs

gen total_sibs 	= total_fullsibs + total_halfsibs


// Recode siblings
recode *sibs*		 	(0 = 0 "None") ///
						(1 = 1 "One")	///
						(2 = 2 "Two")	///
						(3/16 = 3 "Three or more") ///
						(else=.),pre(R_) label(siblingl)

						
// Sibling position relative to full- and half-siblings
label define sib_positionl 1 "Older" 2 "Younger" 3 "Older and younger"


gen full_sibling_position = .
replace full_sibling_position = 1 if older_fullsibs == 0 & 				///
											younger_fullsibs > 0
replace full_sibling_position = 2 if older_fullsibs > 0 & 				///
											younger_fullsibs == 0
replace full_sibling_position = 3 if older_fullsibs > 0 & 				///
											younger_fullsibs > 0

label value full_sibling_position sib_positionl


gen half_sibling_position = .
replace half_sibling_position = 1 if older_halfsibs == 0 & 				///
											younger_halfsibs > 0
replace half_sibling_position = 2 if older_halfsibs > 0 & 				///
											younger_halfsibs == 0
replace half_sibling_position = 3 if older_halfsibs > 0 & 				///
											younger_halfsibs > 0

label value half_sibling_position sib_positionl

			***		PROPORTIONS		***

// Proportion with at least one sibling. Total, older and younger

gen one_sib = 0
replace one_sib = 100 if total_sibs > 0 


gen one_older_sib = 0 
replace one_older_sib = 100 if older_sibs > 0

gen one_younger_sib = 0
replace one_younger_sib = 100 if younger_sibs > 0 


// Proportion with at least one full fullsibling. Total, older and younger

gen one_fullsib = 0
replace one_fullsib = 100 if total_fullsibs > 0 


gen one_older_fullsib = 0 
replace one_older_fullsib = 100 if older_fullsibs > 0

gen one_younger_fullsib = 0
replace one_younger_fullsib = 100 if younger_fullsibs > 0 


// Proportion with at least one half sibling. Total, older and younger

gen one_halfsib = 0
replace one_halfsib = 100 if total_halfsibs > 0 


gen one_older_halfsib = 0 
replace one_older_halfsib = 100 if older_halfsibs > 0


gen one_younger_halfsib = 0
replace one_younger_halfsib = 100 if younger_halfsibs > 0 


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

* =========================================
* Effect of education
* =========================================

mlogit family_types ib2.EDU_3 if COUNTRY!=8402
margins, dydx(ib2.EDU_3) post
eststo edu_pp
marginsplot, yline(0) xlabel(1 "High" 3 "Low")									///
legend(order(1 "P" 2 "P-Si" 3 "Si" 4 "P-Si-St" 5 "Si-St" 6 "P-St"				///
	7 "Si-St-Si" 8 "P-Si-St-Si" 99 "Other"))
graph export "$graphs/education_pp.eps", replace


