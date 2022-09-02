*===================================================*

*****				DATA PREPARATION			*****

*===================================================*


// Use harmonized history data
use "$ggs/HARMONIZED-HISTORIES.dta", clear

keep if  SEX == 2 	// Keep women
gen id = _n			// Generate unique ID



* =========================================
* Main drops
* =========================================


// Drop  Uruguay, Kazakhstan and Germany
 drop if COUNTRY==8581 | COUNTRY==8601 | COUNTRY==2761 | COUNTRY==2762
			


// Combine waves from Spain, Netherland, Canada and US
replace COUNTRY = 7242 if COUNTRY == 7241
replace COUNTRY = 5282 if COUNTRY == 5281
replace COUNTRY = 1242 if COUNTRY == 1241
replace COUNTRY = 8402 if COUNTRY == 8401


// Drop if unknown (.a) or not available in survey (.c) for partner 1
drop if 	SEP_Y1 == .a 	| SEP_Y1 == .c 		| 								///
			ISEP_M1 == .a 	| ISEP_M1 == .c
		
drop if 	UNION_Y1 == .a 	| UNION_Y1 == .c 	| 								///
			IUNION_M1 == .a | IUNION_M1 == .c

drop if 	MARR_1 == .a 	| MARR_1 == .c 

// Drop if unknown for the rest of the partners
forvalues j = 2/10{

	drop if SEP_Y`j' == .a 	| ISEP_M`j' == .a
	drop if UNION_Y`j' ==.a	| IUNION_M`j' == .a
	drop if MARR_`j' == .a
}

// Drop if immigrated after age 15
drop if ((ym(MIG_Y,IMIG_M) - ym(BORN_Y, IBORN_M)) > 168) & NATIVE==2
drop if NATIVE==2 & missing(IMIG_M)

********************************************************************************
********************************************************************************



* =========================================
* Generate year-month variables and drop
* superflous variables
* =========================================


// Union
forvalues j = 1/10{
	gen IUNION_YM`j'	= ym(UNION_Y`j',IUNION_M`j')
}

// Separation
forvalues j = 1/10{
	gen ISEP_YM`j'		= ym(SEP_Y`j',ISEP_M`j')
}

// Marriage
forvalues j = 1/10{
	gen IMARR_YM`j'		= ym(MARR_Y`j',IMARR_M`j')
}
// Divorce
forvalues j = 1/10{
	gen IDIV_YM`j'		= ym(DIV_Y`j',IDIV_M`j')
}

// Child birth
forvalues j = 1/16{
	gen IKID_YM`j'		= ym(KID_Y`j',IKID_M`j') 	
}

// Child death
forvalues j = 1/16{
	gen IKID_DYM`j' 	= ym(KID_DY`j',IKID_DM`j') 
}

// Child left home
forvalues j = 1/16{
	gen IKID_LYM`j' 	= ym(KID_LY`j',IKID_LM`j') 
}

// Drop if missing information on recorded partnerships
forvalues j = 1/10{

drop if missing(IUNION_YM`j') & UNION_`j' == 1
drop if missing(ISEP_YM`j') & (SEP_`j' == 1 | SEP_`j' == 2)

drop if missing(IMARR_YM`j') & MARR_`j' == 1
drop if missing(IDIV_YM`j') & (DIV_`j' == 1)


}


// Generate partner variables 
forvalues j = 1/10{

gen PARTNER_`j' = .
replace PARTNER_`j' = 0 if (UNION_`j' == 0) & (MARR_`j' == 0)
replace PARTNER_`j' = 1 if (UNION_`j' == 1) | (MARR_`j' == 1)

}

forvalues j = 1/10{

gen SPLIT_`j' = .
replace SPLIT_`j' = 0 if (SEP_`j' == 0) & (DIV_`j' == 0)
replace SPLIT_`j' = 1 if (SEP_`j' == 1) | (DIV_`j' == 1)

}



forvalues j = 1/10{

gen IPARTNER_YM`j' 		= .
replace IPARTNER_YM`j' 	=	IUNION_YM`j' if UNION_`j' == 1 &					///
							((IUNION_YM`j' <= IMARR_YM`j') | MARR_`j' == 0)
							
replace IPARTNER_YM`j'	=	IMARR_YM`j' if MARR_`j' == 1 &						///
							((IMARR_YM`j' <= IUNION_YM`j'))

}

forvalues j = 1/10{

gen ISPLIT_YM`j'		= .

replace ISPLIT_YM`j'	= 	ISEP_YM`j' if SEP_`j' == 1 &						///
							((ISEP_YM`j' <= IDIV_YM`j') | DIV_`j' == 0)
							
replace ISPLIT_YM`j'	=	IDIV_YM`j' if DIV_`j' == 1 &						///
							((IDIV_YM`j' <= ISEP_YM`j'))


}

// Generate variables coding for all the kids parents

forvalues sib = 1/16{

	gen PARENT_KID`sib' = .

	forvalues i = 1/10{
	
	replace PARENT_KID`sib' 	= `i' if 									///
			(IKID_YM`sib' 			>= IPARTNER_YM`i') 	& 						///
			((IKID_YM`sib'		 	<= ISPLIT_YM`i') | SPLIT_`i'==0) &				///
			PARTNER_`i' == 1 & KID_`sib' == 1
		
	
	
	/*	CODE USING SEPARATE UNION/MARRIAGE VARIABLES				
	
		replace PARENT_KID`sib' 	= `i' if 									///
			(IKID_YM`sib' 			>= IUNION_YM`i') 	& 						///
			((IKID_YM`sib'		 	<= ISEP_YM`i') | SEP_`i'==0) &				///
			UNION_`i' == 1 & KID_`sib' == 1
		
		replace PARENT_KID`sib' 	= `i' if 									///
			(IKID_YM`sib' 			>= IMARR_YM`i') 	& 						///
			((IKID_YM`sib'		 	<= IDIV_YM`i') | DIV_`i'==0) &				///
			MARR_`i' == 1 & KID_`sib' == 1
	*/

	}

}




// Keep only relevant variables
keep 	IKID_*YM* IUNION_YM* ISEP_YM* SEP_1-SEP_10 	UNION_1-UNION_10			///
		COUNTRY YEAR_S MONTH_S id  MARR_1-MARR_10 IMARR_YM* IDIV_YM*			///
		DIV_1-DIV_10 PARENT_KID* PARTNER_1-PARTNER_10 IPARTNER_YM* ISPLIT_YM*	///
		SPLIT_1-SPLIT_10
		
********************************************************************************
********************************************************************************


* =========================================
* Create dataset to be merged later
* =========================================

preserve
	keep id IKID_YM* IKID_DYM* IKID_LYM* 

	save "$ggstemp/child_info.dta",replace
restore
********************************************************************************
********************************************************************************


* =========================================
* Reshape and variable manipulation
* =========================================

// Reshape using child and birth date
reshape long IKID_YM IKID_DYM IKID_LYM, i(id) j(child)

// Generate unique child ID
gen childID = _n

// Drop if missing
drop if missing(IKID_YM)

// Keep those older than 15
keep if (ym(YEAR_S,MONTH_S) - IKID_YM) > 168

// Drop if dead before 15
drop if (IKID_DYM - IKID_YM) < 168

// Drop if left home before age 15
drop if (IKID_LYM - IKID_YM) < 168

// Gen birth year and cohort variables
gen KID_BYEAR = year(dofm(IKID_YM))

/*
recode KID_BYEAR	(1950/1959 = 1 "1950-59")									///
					(1960/1969 = 2 "1960-69")									///
					(1970/1979 = 3 "1970-79")									///
					(1980/1989 = 4 "1980-89")									///
					(1990/1999 = 5 "1990-99") 									///
					(2000/2010 = 6 "2000-2010") 								///
					(else = .), gen(COHORT)

drop if missing(COHORT)
*/

// Merge with saved child variables
merge m:1 id using "$ggstemp/child_info.dta"
drop if missing(childID)

// Set missing value to child variables which is the same as observation
forvalues j = 1/10{

replace IKID_YM`j' 		= . if child == `j'
replace IKID_DYM`j' 	= . if child == `j'
replace IKID_LYM`j' 	= . if child == `j'
replace PARENT_KID`j'	= . if child == `j'

}

// Rename anchor variables
rename IKID_YM 	KIDBORN_YM
rename IKID_DYM KIDDEAD_YM
rename IKID_LYM KIDLEFT_YM
********************************************************************************
********************************************************************************


* =========================================
* Export the results
* =========================================

save "$ggstemp/long_HH", replace


