*===================================================*

*****	GENERATE PARTNER & SIBLING SEQUENCES	*****

*===================================================*

// Use data from "01_data_preparation.do"
use "$ggstemp/long_HH.dta", replace



* =========================================
* Generate all state variables
* =========================================


forvalues month = 0/179{		//Partner

	gen PARTNER_STATE`month'= .
	
}

forvalues month = 0/179{		//Full siblings

	gen FULLSIB_STATE`month' = 0
	
}

forvalues month = 0/179{		//Half siblings

	gen HALFSIB_STATE`month' = 0
	
}

********************************************************************************
********************************************************************************



* =========================================
* Set union and sibling states
* =========================================


// Generate union states and marriage states for each month
forvalues month = 0/179{

di "Partner month" + `month'

quietly{
	forvalues i = 1/10{		// Loop through each partner
		

			// Set partner state to partner number if month is during union
		replace PARTNER_STATE`month' 		= `i' if 								///
				(KIDBORN_YM + `month' 	>= IPARTNER_YM`i') & 						///
				((KIDBORN_YM + `month' 	<= ISPLIT_YM`i') | SPLIT_`i'==0)
				
	}
}
}

// Generate full-sibling and half-sibling states for each month
forvalues month = 0/179{

di "Sibling month" + `month'

quietly{
	forvalues sib = 1/16{
				
		replace FULLSIB_STATE`month' 	=	FULLSIB_STATE`month' + 1 		if	///
			 	(KIDBORN_YM + `month')	>= 	IKID_YM`sib'				&		///
				PARTNER_STATE0 			== 	PARENT_KID`sib'				&		///
				(PARTNER_STATE0 !=. | PARENT_KID`sib' !=.)
				
		replace HALFSIB_STATE`month' 	=	HALFSIB_STATE`month' + 1	if		///
			 	(KIDBORN_YM + `month')	>= 	IKID_YM`sib'				&		///
				((PARTNER_STATE0 			!= 	PARENT_KID`sib')			|		///
				(PARTNER_STATE0 ==. | PARENT_KID`sib' ==.))
	}
}
}

quietly{
********************************************************************************
********************************************************************************

/*
* =========================================
* Recode state number of partners so that 
* they represent the order in the child's
* life rather than the mothers
* =========================================


// Find the first partner
egen partner_state_min = rowmin(PARTNER_STATE*) 


// Get highest ranking partner
sum PARTNER_STATE179 if PARTNER_STATE179 != .
local highest_partner = r(max)



forvalues month = 0/179{
	
	// Set the partner state to be the first partner if the first partner
	// is equal to the highest ranking partner
	replace PARTNER_STATE`month'	= 1 if 										///
		partner_state_min == `highest_partner' 

	// Set partner to zero if partner state is the highest ranking partner
	// (This will be replaced by the highest ranking partner in the next loop)
	replace PARTNER_STATE`month' = 0 if PARTNER_STATE`month' == `highest_partner'


	// For all other cases, set the partner state to the current state minus
	// the first state plus one
	replace PARTNER_STATE`month' = PARTNER_STATE`month' - partner_state_min + 1 if 	///
		PARTNER_STATE`month' > 1 & PARTNER_STATE`month' != . & 					///
		PARTNER_STATE`month'!= `highest_partner'
	
	
}

// Find the last partner 
egen partner_state_max = rowmax(PARTNER_STATE*)


forvalues month = 0/179{

	// Set partner state to the highest ranking partner 
	// (i.e. substitute the zero previously generated)
	replace PARTNER_STATE`month' = partner_state_max + 1 if 						///
	PARTNER_STATE`month' == 0 
	
	
	
	// Substitute back 0 for single
	replace PARTNER_STATE`month' = 0 if PARTNER_STATE`month' == .
	

}
*/
********************************************************************************
********************************************************************************
}

********************************************************************************
********************************************************************************


* ======================
* Set family type state
* ======================


label define family_statel 0 "Single" 1 "Parents" 2 "Step"

forvalues month =  0/179{

di "Family month" + `month'

quietly{
gen FAMILY_STATE`month' = .



replace FAMILY_STATE`month' = 0 if missing(PARTNER_STATE`month')
replace FAMILY_STATE`month' = 1 if PARTNER_STATE`month' == PARTNER_STATE0 &		///
									!missing(PARTNER_STATE`month')
replace FAMILY_STATE`month' = 2 if PARTNER_STATE`month' != PARTNER_STATE0 &		///
									!missing(PARTNER_STATE`month')
label values FAMILY_STATE`month' family_statel
}

}

********************************************************************************
********************************************************************************

* =============
* Generate DSS
* =============

// Generate
tostring FAMILY_STATE0, gen(DSS0)

// Generate Distinct-Succesive-States for each month
forvalues month = 1/179{
di "DSS month" + `month'

quietly{
local last = `month' - 1

// Set DSS of the month to previous DSS
gen DSS`month' = DSS`last'

// Add new state if current month state is not equal to the last
replace DSS`month' = DSS`last' + strofreal(FAMILY_STATE`month') if				///
		strrpos(DSS`last',strofreal(FAMILY_STATE`month')) != strlen(DSS`last')

}
}


drop _merge 
save "$ggstemp/state_sequence_set.dta", replace


