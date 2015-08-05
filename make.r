# hi Slovakia

source("load.r")
source("functions.r")
source("parties.r")

# folders

dir.create("data"   , showWarnings = FALSE)
dir.create("photos" , showWarnings = FALSE)
dir.create("plots"  , showWarnings = FALSE)
dir.create("raw"    , showWarnings = FALSE)

# parameters

plot = TRUE
gexf = TRUE
mode = "fruchtermanreingold"
meta = c(
  "cty" = "Slovakia",
  "lang" = "sk", # Wikipedia language for chamber and constituencies
  "ch" = "Národná_rada_Slovenskej_republiky",
  "type" = "Unicameral",
  "ipu" = 2285,
  "seats" = 150
)

# build routine

source("data.r")  # scrape bills and sponsors
source("build.r") # assemble the networks
source("comm.r")  # add committee co-membership

# have a nice day
