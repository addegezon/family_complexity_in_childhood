*===================================================*

*****	GENERATE PARTNER & SIBLING SEQUENCES	*****

*===================================================*

// Use data from "01_data_preparation.do"
use "$ggstemp/long_HH.dta", replace



* =========================================
* Generate all state variables
* =========================================

forvalues month = 0/167{		//Union	
		
	gen UNION_STATE`month'= .
	
}

forvalues month = 0/167{		//Marriage	
		
	gen MARR_STATE`month'= .
	
}

forvalues month = 0/167{		//Marriage-union

	gen MU_STATE`month'= .
	
}

forvalues month = 0/167{		//Partner

	gen PARTNER_STATE`month'= .
	
}

forvalues month = 0/167{		//Full siblings

	gen FULLSIB_STATE`month' = 0
	
}

forvalues month = 0/167{		//Half siblings

	gen HALFSIB_STATE`month' = 0
	
}

********************************************************************************
********************************************************************************



* =========================================
* Set union and sibling states
* =========================================

// Generate union states and marriage states for each month
forvalues month = 0/167{


	forvalues i = 1/10{		// Loop through each partner
		

			// Set union state to partner number if month is during union
		replace UNION_STATE`month' 		= `i' if 								///
				(KIDBORN_YM + `month' 	>= IUNION_YM`i') & 						///
				((KIDBORN_YM + `month' 	<= ISEP_YM`i') | SEP_`i'==0)
				
				// Set marriage state to partner number if month is during marriage
		replace MARR_STATE`month' 		= 	`i' if 								///
				(KIDBORN_YM + `month'	>= IMARR_YM`i') & 						///
				((KIDBORN_YM + `month' 	< IDIV_YM`i') | DIV_`i'==0)
		
	}
}

// Generate full-sibling and half-sibling states for each month
forvalues month = 0/167{

	forvalues sib = 1/16{
				
		replace FULLSIB_STATE`month' 	=	FULLSIB_STATE`month' + 1 		if	///
			 	(KIDBORN_YM + `month')	>= 	IKID_YM`sib'				&		///
				UNION_STATE0 			== 	PARENT_KID`sib'				&		///
				(UNION_STATE0 !=. | PARENT_KID`sib' !=.)
				
		replace HALFSIB_STATE`month' 	=	HALFSIB_STATE`month' + 1	if		///
			 	(KIDBORN_YM + `month')	>= 	IKID_YM`sib'				&		///
				((UNION_STATE0 			!= 	PARENT_KID`sib')			|		///
				(UNION_STATE0 ==. | PARENT_KID`sib' ==.))
	} 
}


********************************************************************************
********************************************************************************


* =========================================
* Recode state number of partners so that 
* they represent the order in the child's
* life rather than the mothers
* =========================================


// Find the first partner for both union and marriage
egen union_state_min = rowmin(UNION_STATE*) 
egen marr_state_min = rowmin(MARR_STATE*) 


// Get highest ranking union
sum UNION_STATE167 if UNION_STATE167 != .
local highest_union = r(max)

sum MARR_STATE167 if MARR_STATE167 != .
local highest_marr = r(max)



forvalues month = 0/167{
	
	// Set the union state to be the first partner if the first partner
	// is equal to the highest ranking union
	replace UNION_STATE`month'	= 1 if union_state_min 	== `highest_union'
	replace MARR_STATE`month'	= 1 if marr_state_min 	== `highest_marr'

	// Set union to zero if union state is the highest ranking union
	// (This will be replaced by the highest ranking union in the next loop)
	replace UNION_STATE`month' 	= 0 if UNION_STATE`month' 	== `highest_union'
	replace MARR_STATE`month' 	= 0 if MARR_STATE`month' 	== `highest_marr'



	// For all other cases, set the union state to the current state minus
	// the first state plus one
	replace UNION_STATE`month' = UNION_STATE`month' - union_state_min + 1 if 	///
		UNION_STATE`month' > 1 & UNION_STATE`month' != . & 					///
		UNION_STATE`month'!= `highest_union'
	
	replace MARR_STATE`month' = MARR_STATE`month' - marr_state_min + 1 if 	///
		MARR_STATE`month' > 1 & MARR_STATE`month' != . & 					///
		MARR_STATE`month'!= `highest_marr'
	
}

// Find the last partner 
egen union_state_max = rowmax(UNION_STATE*)
egen marr_state_max = rowmax(MARR_STATE*)


forvalues month = 0/167{

	// Set union state to the highest ranking partner 
	// (i.e. substitute the zero previously generated)
	replace UNION_STATE`month' = union_state_max + 1 if 						///
	UNION_STATE`month' == 0 
	
	replace MARR_STATE`month' = marr_state_max + 1 if 						///
	MARR_STATE`month' == 0 
	
	/*
	// Substitute back 888 for single
	replace UNION_STATE`month' = 0 if UNION_STATE`month' == .
	replace MARR_STATE`month' = 0 if MARR_STATE`month' == .
	*/

}

/*
forvalues month = 0/167{		//Marriage-union

	replace MU_STATE`month'= UNION_STATE`month' +100 if UNION_STATE`month' < 888 & MARR_STATE`month' < 888
	replace MU_STATE`month' = UNION_STATE`month' if UNION_STATE`month'<888 & MARR_STATE`month'==888
	replace MU_STATE`month' = MARR_STATE`month' + 100 if UNION_STATE`month'==888 & MARR_STATE`month'<888
	
}
*/

********************************************************************************
********************************************************************************

drop _merge 
save "$ggstemp/state_sequence_set.dta", replace


