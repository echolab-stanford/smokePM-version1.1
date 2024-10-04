# Set the location of Dropbox and GitHub folders
path_dropbox = "~/Documents/smokePM-prediction/Local_dropbox"# "INSERT PATH TO DROPBOX FOLDER HERE"
path_github = "~/Documents/smokePM-prediction/Local_github"# "INSERT PATH TO GITHUB REPO HERE"

path_dropbox_sherlock = "/home/groups/mburke/smokePM-prediction/"# "INSERT PATH TO DROPBOX FOLDER ON HPC HERE"
path_github_sherlock = "/scratch/groups/mburke/smokePM-prediction/"# "INSERT PATH TO GITHUB REPO ON HPC HERE"

# File paths based on root folders above
path_data = file.path(path_dropbox, "data")
path_output = file.path(path_dropbox, "output")
path_final = file.path(path_dropbox, "final")
path_tables = file.path(path_github, "tables", "raw")
path_figures = file.path(path_github, "figures", "raw")
path_setup = file.path(path_github, "scripts", "setup")
path_main = file.path(path_github, "scripts", "main")
path_supplementary = file.path(path_github, "scripts", "supplementary")

path_data_sherlock = file.path(path_dropbox_sherlock, "data")
path_output_sherlock = file.path(path_dropbox_sherlock, "output")
path_final_sherlock = file.path(path_dropbox_sherlock, "final")
path_tables_sherlock = file.path(path_github_sherlock, "tables", "raw")
path_figures_sherlock = file.path(path_github_sherlock, "figures", "raw")
path_setup_sherlock = file.path(path_github_sherlock, "scripts", "setup")
path_main_sherlock = file.path(path_github_sherlock, "scripts", "main")
path_supplementary_sherlock = file.path(path_github_sherlock, "scripts", "supplementary")

# File paths for raw datasets spanning many files
path_epa = file.path(path_data, "EPA", "raw") 
path_fire = file.path(path_data, "fire", "raw")
path_smoke = file.path(path_data, "smoke", "raw") 
path_era5 = file.path(path_data, "ERA5_variables") 
path_merra2 = file.path(path_data, "MERRA2_AOT", "total_aot")
