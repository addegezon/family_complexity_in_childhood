************************************************************************************
************************************************************************************

version 16 
set more off, perm 
clear all
set max_memory 7g, permanently

capture log close 

************************************************************************************
************************************************************************************

set seed 123454321

************************************************************************************
************************************************************************************

* =========================================
* Global Macros for Paths
* =========================================

global workingDir	"/home/andreas/Documents/skola/master/work/analysis"

global data 		"${workingDir}/01_data"					// Data
global temp 		"${data}/01_temp"						// Temporary Data
	
global ggs			"${data}/00_ggs" 						// Original GGS Data	
global ggstemp	 	"${ggs}/01_temp"						// Temporary GGS Data 

global dofiles 		"${workingDir}/02_do-files"				// do-files
global log 			"${workingDir}/03_logs"					// log-files
global graphs 		"${workingDir}/04_graphs" 				// graphs
global tables 		"${workingDir}/05_tables" 				// tabels

************************************************************************************
************************************************************************************

* =========================================
* Run all do-files
* =========================================

do "${dofiles}/01_data_preparation.do"						// Prepare data for sequence_generation
do "${dofiles}/02_sequence_generation.do"					// Generate sequences
do "${dofiles}/03_descriptive_analysis.do"					// Run main analysis





************************************************************************************
************************************************************************************

exit