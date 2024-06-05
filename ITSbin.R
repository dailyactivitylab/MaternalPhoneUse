# ITSbin package and functions created by Helen Anderson.
# Heather Anderson. (2021). htanderson/ITSbin: Second release of ITSbin (v1.0.1). Zenodo. https://doi.org/10.5281/zenodo.4546487

# Note: Data_Preprocessing folder contains the outputs for this code: LENA_binned_infants

library(ITSbin)

# function for converting ITS files to csv outputs with 1 sec and 1 min binned summary measures
# P = participant number (P##), p_dir = folder where ITS files are located (each participant has a folder and each folder has ITS files), output_dir = where you want your CSV files output

ITS_to_bins <- function(P, p_dir, output_dir) {
  
  if (dir.exists(paste(p_dir, P, sep=""))) {
    p_ITS_dir <- paste(p_dir, P, sep="")
    p_output_dir_str <- paste(output_dir, P, sep = "")
    print(p_output_dir_str)
    p_output_dir <- dir.create(p_output_dir_str)  # create new output folder for participant

    check_multiday(
      ITS.dir = p_ITS_dir,
      CSV.dir = p_output_dir_str,
      time.zone = "Chicago",
      write.all.recordings = FALSE
    )

    ITS_to_seconds(
      ITS.dir = p_ITS_dir,
      CSV.dir = p_output_dir_str,
      time.zone = "Chicago")
    
    seconds_dir_str <- paste(p_output_dir_str, "/seconds/", sep = "")
    print("Seconds file is done.")

    
    # 1 minute bins
    bin_seconds(
      seconds.dir = seconds_dir_str,
      output.dir = p_output_dir_str,
      bin.to.mins = 1,
      align.rows = "midnight")
  }
  else {
    print(paste("There is no LENA folder with ITS files for ", P, sep=""))
  }
}

### MAIN ###

p_dir <- "../Data_Raw/LENA_raw_ITS_infants/"
output_dir <- "../Data_Binned/LENA_binned_infants/"

PIDs <- list.files(p_dir)

# loop through all participants and call ITS_to_bins function for each
for (P in PIDs) {
  ITS_to_bins(P, p_dir, output_dir)
}
