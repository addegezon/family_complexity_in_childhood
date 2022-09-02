*===================================================*

*****				R IMPORT					*****

*===================================================*

* =========================================
* Import DSS from R
* =========================================

infile childID ST1:ST1_fmt ST2:ST2_fmt ST3:ST3_fmt ST4:ST4_fmt ST5:ST5_fmt ST6:ST6_fmt ST7:ST7_fmt ST8:ST8_fmt ST9:ST9_fmt ST10:ST10_fmt ST11:ST11_fmt ST12:ST12_fmt  using  "$ggstemp/dss.csv" , clear automatic

// Recode % as missing

forvalues state = 2/12{

replace ST`state' = . if ST`state' == 1

}

// Generate DSS variable
gen DSS = ""
forval j = 1/12 { 
    replace DSS = DSS + string(ST`j') if ST`j' < . 
} 

// Drop ST-variables
drop ST*

// Merge with main dataset
merge 1:1 childID using "$ggstemp/state_sequence_set.dta"

// Export
save "$ggstemp/state_sequence_DSS.dta", replace
