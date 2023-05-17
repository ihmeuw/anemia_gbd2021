/***********************************************************************************************************
 Date: 02/10/2020
 Project: ubCov
 Purpose: Run Script
***********************************************************************************************************/


//////////////////////////////////
// Setup
//////////////////////////////////

if c(os) == "Unix" {
    local j "FILEPATH"
	local h "FILEPATH"
	local l "FILEPATH"
    set odbcmgr unixodbc
	local central_root "FILEPATH"
}
else if c(os) == "Windows" {
    local j "FILEPATH"
	local h "FILEPATH"
	local l "FILEPATH"
	local central_root "FILEPATH"
}

clear all
set more off
set obs 1

///////////////////////////////////////////////////////////////////////
/*  Arguments:
		- topic: your research topic
        - array: Ubcov_id. The id of the codebook row
		- outpath_L: output file path for limited drive files
		- outpath_J: output file path for general files
    Optional:
        - keep: Keeps both raw data and extracted data, allows manual extraction check before final output.
        - bypass: Skips the extraction check, output the data 
        - run_all: Loops through all ubcov_ids in the codebook.
*/
////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////
//Extraction
//////////////////////////////////////////////////////////////////////


// Load functions
cd "`central_root'"
do "`central_root'/FILEPATH/load.do"

// Load the base code for ubCov
local paths  `central_root'/FILEPATH/
foreach path in `paths' {
    local files : dir "`path'" files "*.do"
    foreach file in `files' {
        if "`file'" != "run.do" do "`path'/`file'"
    }
}

// Make sure you're in central
cd `central_root'

// Initialize the system
ubcov_path
init, topics(`topics')

local topics anemia
local array 7198
local outpath_L "FILEPATH/"
local outpath_J "FILEPATH/"
local options bypass 



// Launches extract
foreach number in `array'{
    local uid `number'
    run_extract `uid', `options'
    tostring year_start, gen(year)
    tostring year_end, gen(end_year)
    tostring nid, gen(nid_n)
    local filename = survey_name + "_" + nid_n + "_" + survey_module + "_" + ihme_loc_id + "_" + year + "_" + end_year
    local filename = subinstr("`filename'", "/", "_",.)
    drop year end_year nid_n
	if (strpos("$file_path", "LIMITED_USE")|strpos("$file_path", "IDENT")){
		local outpath = "`outpath_L'"
	}
	else{
		local outpath = "`outpath_J'"
	}
	cd  "`outpath'"
    save "`filename'", replace
}
