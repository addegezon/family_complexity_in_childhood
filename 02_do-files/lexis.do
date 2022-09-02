drop COHORT
keep if COUNTRY ==7521

recode KID_BYEAR	(1950/1954 = 1950 "1950-54")									///
					(1955/1959 = 1955 "1955-59")									///
					(1960/1964 = 1960 "1960-64")									///
					(1965/1969 = 1965 "1965-69")									///
					(1970/1974 = 1970 "1970-74")									///
					(1975/1979 = 1975 "1975-79")									///
					(1980/1984 = 1980 "1980-84")									///
					(1985/1989 = 1985 "1985-89")									///
					(1990/1994 = 1990 "1990-94") 									///
					(1995/1999 = 1995 "1995-99")									///
					(else = .), gen(COHORT)

bysort COHORT age: egen one_halfsib_mean = mean(one_halfsib)
bysort COHORT age: egen one_step_mean = mean(ever_1step)


levelsof COHORT, local(levels) 
foreach cohort of local levels {
twoway area one_step_mean age if COHORT==`cohort', sort ylabel(0(10)100)
graph export "/home/andreas/temp/cohort_`cohort'.eps", replace

}