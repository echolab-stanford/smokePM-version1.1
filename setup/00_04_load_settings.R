if (!("sherlock" %in% ls())) sherlock = F
if (sherlock) {
  
  # Parallel computing -----------------------------------------------------------
  # Stanford University Net ID
  SUNetID = "INSERT YOUR SUNET ID HERE"

  # Set the number of cores to use in parallel computing
  num_cores = Sys.getenv("SLURM_CPUS_PER_TASK")
  if (nchar(num_cores) > 0) {
    num_cores = as.integer(num_cores) - 1
  } else {
    num_cores = 1 # default sequential
  }
  
} else {
  dotenv::load_dot_env()
  
  # Google Earth Engine ----------------------------------------------------------
  # Provide your Google Earth Engine email
  gee_email = Sys.getenv('GEE_EMAIL')# "INSERT YOUR GEE EMAIL HERE, e.g. jdoe@stanford.edu"
  gee_user = strsplit(gee_email, "@")[[1]][1]
  library(rgee)
  try(ee_Initialize(user = gee_user))
  

  # US Census Bureau -------------------------------------------------------------
  # Provide your US Census API Key
  census_key <- Sys.getenv('US_CENSUS_API')# "INSERT YOUR US CENSUS BUREAU API KEY HERE"
  try(census_api_key(census_key))
  
}

# Other ------------------------------------------------------------------------
nonContig_stateFIPS <- c("02","60","66","15","72","78","69")
