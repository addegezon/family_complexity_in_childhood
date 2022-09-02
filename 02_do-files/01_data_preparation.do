*===================================================*

*****				DATA PREPARATION			*****

*===================================================*


// Use harmonized history data
use "$ggs/HARMONIZED-HISTORIES.dta", clear


* =========================================
* Main drops
* =========================================


// Drop  Uruguay, Kazakhstan, Germany and Austria
 drop if COUNTRY==8581 | COUNTRY==8601 | COUNTRY==2761 | COUNTRY==2762 |		///
COUNTRY == 8401 | COUNTRY == 8402 | COUNTRY == 1242 | COUNTRY == 1241


// Combine waves from Spain, Netherland, Canada and US
replace COUNTRY = 7242 if COUNTRY == 7241
replace COUNTRY = 5282 if COUNTRY == 5281
*replace COUNTRY = 1242 if COUNTRY == 1241
replace COUNTRY = 8402 if COUNTRY == 8401

// Re-define country labels

label define countryl 401 `"Austria"', modify
label define countryl 561 `"Belgium"', modify
label define countryl 1001 `"Bulgaria"', modify
label define countryl 1121 `"Belarus"', modify
label define countryl 1242 `"Canada"', modify
label define countryl 2031 `"Czech Republic"', modify
label define countryl 2331 `"Estonia"', modify
label define countryl 2501 `"France"', modify
label define countryl 2681 `"Georgia"', modify
label define countryl 3481 `"Hungary"', modify
label define countryl 3801 `"Italy"', modify
label define countryl 4401 `"Lithuania"', modify
label define countryl 4981 `"Moldova"', modify
label define countryl 5282 `"Netherlands"', modify
label define countryl 5781 `"Norway"', modify
label define countryl 6162 `"Poland"', modify
label define countryl 6421 `"Romania"', modify
label define countryl 6431 `"Russia"', modify
label define countryl 7242 `"Spain"', modify
label define countryl 7521 `"Sweden"', modify
label define countryl 8261 `"UK"', modify
label define countryl 8402 `"USA"', modify

label variable COUNTRY "Country"
label values COUNTRY countryl




// Drop counts
quietly{
tabulate COUNTRY, matcell(freq)
putexcel set "$tables/drops", sheet("drops") modify
putexcel A1=("Total")
putexcel A2=matrix(freq) 
}

keep if  SEX == 2 	// Keep women

					// Drop counts
quietly{
tabulate COUNTRY, matcell(freq)
putexcel set "$tables/drops", sheet("drops") modify
putexcel B1=("Keep women")
putexcel B2=matrix(freq) 
}


// Drop childless women
egen kids = rowmax(KID_1-KID_16)

drop if kids == 0
drop kids

quietly{
tabulate COUNTRY, matcell(freq)
putexcel set "$tables/drops", sheet("drops") modify
putexcel C1=("With children")
putexcel C2=matrix(freq) 
}


gen id = _n			// Generate unique ID




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

// Drop counts
quietly{
tabulate COUNTRY, matcell(freq)
putexcel set "$tables/drops", sheet("drops") modify
putexcel D1=("With union history")
putexcel D2=matrix(freq) 
}


// Drop if immigrated after age 15
drop if ((ym(MIG_Y,IMIG_M) - ym(BORN_Y, IBORN_M)) > 168) & NATIVE==2
drop if NATIVE==2 & missing(IMIG_M)

// Drop counts
quietly{
tabulate COUNTRY, matcell(freq)
putexcel set "$tables/drops", sheet("drops") modify
putexcel E1=("Natives/immigrated before 15")
putexcel E2=matrix(freq) 
}

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


// Generate partner variables 

// Binary indicator of partner
forvalues j = 1/10{

gen PARTNER_`j' = .
replace PARTNER_`j' = 0 if (UNION_`j' == 0) & (MARR_`j' == 0)
replace PARTNER_`j' = 1 if (UNION_`j' == 1) | (MARR_`j' == 1)

}

// Binary indicator of splitting up
forvalues j = 1/10{

gen SPLIT_`j' = .
replace SPLIT_`j' = 0 if (SEP_`j' == 0) & (DIV_`j' == 0)
replace SPLIT_`j' = 1 if (SEP_`j' == 1) | (DIV_`j' == 1)

}


// Year-month of partnering
forvalues j = 1/10{

gen IPARTNER_YM`j' 		= .
replace IPARTNER_YM`j' 	=	IUNION_YM`j' if UNION_`j' == 1 &					///
							((IUNION_YM`j' <= IMARR_YM`j') | MARR_`j' == 0)
							
replace IPARTNER_YM`j'	=	IMARR_YM`j' if MARR_`j' == 1 &						///
							((IMARR_YM`j' <= IUNION_YM`j'))

}

// Year-month of splitting up
forvalues j = 1/10{

gen ISPLIT_YM`j'		= .

replace ISPLIT_YM`j'	= 	ISEP_YM`j' if SEP_`j' == 1 &						///
							((ISEP_YM`j' <= IDIV_YM`j') | DIV_`j' == 0)
							
replace ISPLIT_YM`j'	=	IDIV_YM`j' if DIV_`j' == 1 &						///
							((IDIV_YM`j' <= ISEP_YM`j'))


}


// Drop if missing information on recorded partnerships
forvalues j = 1/10{

drop if missing(IPARTNER_YM`j') & PARTNER_`j' == 1
drop if missing(ISPLIT_YM`j') & (SPLIT_`j' == 1)

}

// Drop counts
quietly{
tabulate COUNTRY, matcell(freq)
putexcel set "$tables/drops", sheet("drops") modify
putexcel F1=("Info on partner sequence")
putexcel F2=matrix(freq) 
}

// Drop if overlapping relationships
forvalues j = 1/9{

local x = `j' + 1

drop if (ISPLIT_YM`j' > IPARTNER_YM`x') & SPLIT_`j' == 1

}


// Drop counts
quietly{
tabulate COUNTRY, matcell(freq)
putexcel set "$tables/drops", sheet("drops") modify
putexcel G1=("Non-overlapping relationships")
putexcel G2=matrix(freq) 
}


// Generate variables coding for all the kids parents

forvalues sib = 1/16{

	gen PARENT_KID`sib' = .

	forvalues i = 1/10{
	
	replace PARENT_KID`sib' 	= `i' if 										///
			(IKID_YM`sib' 			>= IPARTNER_YM`i') 	& 						///
			((IKID_YM`sib'		 	<= ISPLIT_YM`i') | SPLIT_`i'==0) &			///
			PARTNER_`i' == 1 & KID_`sib' == 1
		
	}

}


// Keep only relevant variables
keep 	IKID_*YM* PARENT_KID* PARTNER_1-PARTNER_10 IPARTNER_YM* ISPLIT_YM*	///
		COUNTRY YEAR_S MONTH_S id SPLIT_1-SPLIT_10 PERSWGT EDU_3
		 
		
		
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

quietly{
tabulate COUNTRY, matcell(freq)
putexcel set "$tables/drops", sheet("drops") modify
putexcel H1=("Children")
putexcel H2=matrix(freq) 
}


// Generate unique child ID
gen childID = _n

// Drop if missing
drop if missing(IKID_YM)

quietly{
tabulate COUNTRY, matcell(freq)
putexcel set "$tables/drops", sheet("drops") modify
putexcel I1=("Info on birth")
putexcel I2=matrix(freq) 
}

// Keep those older than 15
drop if (ym(YEAR_S,MONTH_S) - IKID_YM) < 168

quietly{
tabulate COUNTRY, matcell(freq)
putexcel set "$tables/drops", sheet("drops") modify
putexcel J1=("Older than 15")
putexcel J2=matrix(freq) 
}


// Drop if dead before 15
drop if (IKID_DYM - IKID_YM) < 168

quietly{
tabulate COUNTRY, matcell(freq)
putexcel set "$tables/drops", sheet("drops") modify
putexcel K1=("Alive until 15")
putexcel K2=matrix(freq) 
}


// Drop if left home before age 15
drop if (IKID_LYM - IKID_YM) < 168

quietly{
tabulate COUNTRY, matcell(freq)
putexcel set "$tables/drops", sheet("drops") modify
putexcel L1=("Lived at home until 15")
putexcel L2=matrix(freq) 
}





// Gen birth year and cohort variables
gen KID_BYEAR = year(dofm(IKID_YM))

/*
recode KID_BYEAR	(1950/1959 = 1950 "1950-59")									///
					(1960/1969 = 1960 "1960-69")									///
					(1970/1979 = 1970 "1970-79")									///
					(1980/1989 = 1980 "1980-89")									///
					(1990/1999 = 1990 "1990-99") 									///
					(else = .), gen(COHORT)

drop if missing(COHORT)
*/
drop if KID_BYEAR<1980
quietly{
tabulate COUNTRY, matcell(freq)
putexcel set "$tables/drops", sheet("drops") modify
putexcel M1=("Born between 1990-2000")
putexcel M2=matrix(freq) 
}




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


