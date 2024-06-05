# MaternalPhoneUse


Three R scripts used for preprocessing and analysis:

	ITSbin.R
	beiwe_lena_preprocessing.R (Part 1 and 2)
 	beiwe_lena_analysis.R


Data_Raw: folder contains raw outputs of Beiwe mobile and LENA audio device; used for ITSbin.R and beiwe_lena_preprocessing.R (Part 1)
       
	Beiwe_raw_infants
 		- each participant’s folder has a power_state folder which contains all phone use events and timestamps
   		- there is one power state log for every hour during which phone use data was collected (time in UTC)
	LENA_raw_ITS_infants
 		- ITS outputs from LENA recordings
   	Sleep_movi_infants
 		- Sleep episodes identified using Sadeh sleep algorithm


Date_Binned: folder contains 1-minute binned timeseries outputs from ITSbin.R and beiwe_lena_preprocessing.R (Part 1); used for part 2 of beiwe_lena_preprocessing.R
      		
	Beiwe_binned_infants
 		- P##.csv for each participant: phone state (on or off) for every minute of participant's Beiwe recording
	LENA_binned_infants
 		- P## folder for each participant: contains outputs from ITSbin.R
   			- 1mins_midnight_CRTRUE_seq_alldata folder contains csv files with 1-minute binned word counts
      

Data_Ready_for_Analysis: folder contains outputs from beiwe_lena_preprocessing.R (part 2); used for beiwe_lena_analysis.R

	P##.csv for each participant
		- files containing 1-minute binned Beiwe and LENA (infant sleep removed) data for each participant
	all_P.csv
 		- one file containing 1-minute binned Beiwe and LENA (infant sleep removed) data for all participants
	all_P_day_dem.csv
 		- file containing 1-minute binned Beiwe and LENA (infant sleep removed) data with de-identified demographics for all participants
