
# make sure packrat is working
if(!packrat:::isPackratModeOn()) packrat::on()

require(magrittr)

# load functions
f <- lapply(list.files("code", full.names = T), source)

read_data_plan <- drake::drake_plan(
  metadata = read_metadata(drake::file_in("data/ntw_info.csv")),
  bartomeus_networks = preprocess_bartomeus(drake::file_in("data/raw/Bartomeus_Ntw_nceas.txt")),
  lopezaraiza_networks = preprocess_lopezaraiza(
    drake::file_in("data/raw/networks_lopezaraiza/AC ctl a.txt"),
    drake::file_in("data/raw/networks_lopezaraiza/AC ctl b.txt"),
    drake::file_in("data/raw/networks_lopezaraiza/AC exp a.txt"),
    drake::file_in("data/raw/networks_lopezaraiza/AC exp b.txt"),
    drake::file_in("data/raw/networks_lopezaraiza/CD ctl a.txt"),
    drake::file_in("data/raw/networks_lopezaraiza/CD ctl b.txt"),
    drake::file_in("data/raw/networks_lopezaraiza/CD exp a.txt"),
    drake::file_in("data/raw/networks_lopezaraiza/CD exp b.txt"),
    drake::file_in("data/raw/networks_lopezaraiza/RF ctl a.txt"),
    drake::file_in("data/raw/networks_lopezaraiza/RF ctl b.txt"),
    drake::file_in("data/raw/networks_lopezaraiza/RF exp a.txt"),
    drake::file_in("data/raw/networks_lopezaraiza/RF exp b.txt"),
    drake::file_in("data/raw/networks_lopezaraiza/SA ctl a.txt"),
    drake::file_in("data/raw/networks_lopezaraiza/SA ctl b.txt"),
    drake::file_in("data/raw/networks_lopezaraiza/SA exp a.txt"),
    drake::file_in("data/raw/networks_lopezaraiza/SA exp b.txt")),
  ballantyne_networks = preprocess_ballantyne(
    drake::file_in("data/raw/Ballantyne-2015-raw_data.csv")),
  networks = c(bartomeus_networks, lopezaraiza_networks, ballantyne_networks),
  strings_in_dots = "literals"
)

project_plan <- rbind(read_data_plan)
drake::make(project_plan)
