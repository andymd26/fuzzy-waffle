install.packages("R.utils")
install.packages("dplyr")
options(scipen=999)
require(R.utils)
require(dplyr)

path_data = "/Users/bloh356/Documents/fuzzy-waffle/data/"
cap.raw = read.table(paste(path_data, "capacity_eia.txt.gz", sep=""), sep="\t", header=TRUE, comment.char="")
cap.raw$summer_capacity = as.numeric(as.character(cap.raw$summer_capacity))

eia.dict.1 = data.frame(status_code_1 = c("BU", "OA", "OP", "OS", "RE", "SB", "SC", "SD", "TS", "A", "CN", "CO", "D", "FC", "IP", "L", 
                                  "LE", "M", "MO", "OT", "P", "PL", "RA", "RP", "RT", "T", "U", "V"), 
                         status_code_1_text = c("backup", "out of service but expected to return this year", "operating", "out of service > 365 days not expected to return this year",
                                                "retired", "standby or backup", "cold shutdown", "sold to nonutility", "construction complete but not in commercial operation", 
                                                "proposed increase in generator capability", "cancelled", "under construction", "proposed decrease in generator capability", 
                                                "planned fuel conversion", "planned, indefinitely postponed", "regulatory approval pending but site prep potentially underway", 
                                                "environmental, site or legal challenges", "proposed deactivated shutdown", "modification planned for existing generator", "other",
                                                "planned but no regulatory approvals", "planned not under construction", "planned reactivation of previously retired or deactivated unit",
                                                "proposed repowering", "scheduled for retirement", "regulatory approved but not under construction", "under construction < 50 percent complete", 
                                                "under construction > 50 percent complete")) 

eia.dict.2 = data.frame(status_code_2 = c("BU", "OA", "OP", "OS", "RE", "SB", "SC", "SD", "TS", "A", "CN", "CO", "D", "FC", "IP", "L", 
                                          "LE", "M", "MO", "OT", "P", "PL", "RA", "RP", "RT", "T", "U", "V"), 
                        status_code_2_text = c("backup", "out of service but expected to return this year", "operating", "out of service > 365 days not expected to return this year",
                                               "retired", "standby or backup", "cold shutdown", "sold to nonutility", "construction complete but not in commercial operation", 
                                               "proposed increase in generator capability", "cancelled", "under construction", "proposed decrease in generator capability", 
                                               "planned fuel conversion", "planned, indefinitely postponed", "regulatory approval pending but site prep potentially underway", 
                                               "environmental, site or legal challenges", "proposed deactivated shutdown", "modification planned for existing generator", "other",
                                               "planned but no regulatory approvals", "planned not under construction", "planned reactivation of previously retired or deactivated unit",
                                               "proposed repowering", "scheduled for retirement", "regulatory approved but not under construction", 
                                               "under construction < 50 percent complete", "under construction > 50 percent complete")) 


eia.dict.3 = data.frame(prime_mover = c("AB", "BA", "BT", "CA", "CC", "CE", "CG", "CH", "CP", "CS", "CT", "CW", "FC", "FW", "GE", "GT", "HC", "HL", "HR",
                                 "HY", "IC", "IG", "JE", "NB", "NG", "NH", "NP", "OC", "PB", "PS", "PV", "SP", "SS", "ST", "VR", "WT", "OT", "NA"),
                        prime_mover_text = c("Atmospheric Fluidized Bed", "Battery energy storage", "turbines used in a binary cycle", "Combined Cycle Steam Turbine with Supplemental Firing", 
                                 "Combine Cycle - Total Unit", "Compressed Air Energy Storage", "not defined", "Steam Turbine, Common Header", "Concentrated solar power energy storage", 
                                 "Combined Cycle Single Shaft (gas turbine and steam turbine share a single generator)", "Combined Cycle Combustion Turbine Part", 
                                 "Combined Cycle Steam Turbine Part (Waste Heat Boiler Only)", "Fuel Cell (electrochemical)", "Flywheel energy storage", "Steam Turbine (geothermal)", 
                                 "Combustion (gas) Turbine",  "Hydraulic Turbine (conventional)",
                                 "Hydraulic Turbine (pipeline)", "Hydraulic Turbine (reversible)",
                                 "Hydraulic Turbine (conventional)", "Internal Combustion", "Integrated Coal Gasification Combined Cycle",
                                 "Jet Engine", "Steam Turbine (Boiling Water Nuclear Reactor)", "Steam Turbine (Graphite Nuclear Reactor)",
                                 "Steam Turbine (High-Temperature Gas-Cooled Nuclear Reactor)", "Steam Turbine (Pressurized Water Nuclear Reactor)",
                                 "Ocean Thermal Turbine", "Pressurized Fluidized Bed Combustion", "Hydraulic Turbine Reversible (pumped storage)",
                                 "Photovoltaic","Photovoltaic", "Steam Turbine (Solar)", "Steam Turbine (Boiler)", "Various Types", "Wind Turbine", "Other (Explained in Notes)",
                                 "Unknown at this Time"))

eia.dict.4 = data.frame(fuel_1 = c("AB", "ANT", "BFG", "BIO", "BIT", "BL", "BLQ", "COG", "COL", "COM", "CRU", "CWM", "DFO", "FO1", "FO2", "FO3", "FO4",
                                 "FO5", "FO6", "GAS", "GEO", "GST", "JF", "KER", "LFG", "LIG", "LNG", "LPG", "MF", "MSW", "MTE", "MWH", "MTH", "NG", "NUC", 
                                 "OBG", "OBL", "OBS", "OG", "OO", "OTH", "PC", "PET", "PL", "PRO", "REF", "RG", "RRO", "SNG", "STM", "SUB", "SUN", "TH", "TOP", 
                                 "UR", "WAT", "WC", "WD", "WH", "WND", "OT", "NA", "PG", "PUR", "RC", "RFO", "SC", "SG", "SGC", "SGP", 'SLW', 'SU', 'TDF', 'UNK', 'WDL',
                                 "WDS", "WO", "WOC"),
                        fuel_1_text = c("Agriculture crop byproducts, straw, energy crops", "Anthracite", "blast furnace gas", 'biomass generic', 'bituminous coal', 'not defined',
                                        'black liquor', 'coke oven gas', 'coal generic', 'coal oil mixture', 'crude oil', 'coal water mixture', 'distillate fuel oil', 'no 1 fuel oil', 
                                        'no 2 fuel oil', 'no 3 fuel oil', 'no 4 fuel oil', 'no 5 fuel oil', 'no 6 fuel oil', 'gas generic', 'geothermal', 'geothermal steam', 
                                        'jet fuel', 'kerosene', 'landfill gas', 'lignite', 'liquified natural gas', 'liquified propane gas', 'multifueled', 'municipal solid waste',
                                        'methane', 'megawatt hour', 'methanol', 'natural gas', 'nuclear', 'other biomass gases', 'other biomass liquids', 'other biomass solids', 'other gas', 
                                        'not defined', "other",  'petroleum coke', 'petroleum generic', 'plutonium', 'propane', 'refuse, bagasse and all other nonwood waste', 
                                        'refinery gas', 're refined motor oil', 'synthetic natural gas', 'steam', 'subbituminous coal', 'sun', 'thorium',  'topped crude oil', 'uranium', 
                                        'water', 'waste coal', 'wood and wood waste', 'waste heat', 'wind', 'other', 'not available', 'propane', 'purchased steam', 'refined coal', 
                                        "residual fuel oil", 'coal synfuel', 'synthetic gas other than coal derived', 'coal-derived synthetic gas', 'synthetic gas from petroleum coke', 
                                        'sludge waste', 'not defined', 'tires', 'not defined', 'wood waste liquids', 'wood and wood waste solids', 'oil-other and waste oil', 'not defined'))

eia.dict.5 = data.frame(fuel_2 = c("AB", "ANT", "BFG", "BIO", "BIT", "BL", "BLQ", "COG", "COL", "COM", "CRU", "CWM", "DFO", "FO1", "FO2", "FO3", "FO4",
                                     "FO5", "FO6", "GAS", "GEO", "GST", "JF", "KER", "LFG", "LIG", "LNG", "LPG", "MF", "MSW", "MTE", "MWH", "MTH", "NG", "NUC", 
                                     "OBG", "OBL", "OBS", "OG", "OO", "OTH", "PC", "PET", "PL", "PRO", "REF", "RG", "RRO", "SNG", "STM", "SUB", "SUN", "TH", "TOP", 
                                     "UR", "WAT", "WC", "WD", "WH", "WND", "OT", "NA", "PG", "PUR", "RC", "RFO", "SC", "SG", "SGC", "SGP", 'SLW', 'SU', 'TDF', 'UNK', 'WDL',
                                     "WDS", "WO", "WOC"),
                        fuel_2_text = c("Agriculture crop byproducts, straw, energy crops", "Anthracite", "blast furnace gas", 'biomass generic', 'bituminous coal', 'not defined',
                                        'black liquor', 'coke oven gas', 'coal generic', 'coal oil mixture', 'crude oil', 'coal water mixture', 'distillate fuel oil', 'no 1 fuel oil', 
                                        'no 2 fuel oil', 'no 3 fuel oil', 'no 4 fuel oil', 'no 5 fuel oil', 'no 6 fuel oil', 'gas generic', 'geothermal', 'geothermal steam', 
                                        'jet fuel', 'kerosene', 'landfill gas', 'lignite', 'liquified natural gas', 'liquified propane gas', 'multifueled', 'municipal solid waste',
                                        'methane', 'megawatt hour', 'methanol', 'natural gas', 'nuclear', 'other biomass gases', 'other biomass liquids', 'other biomass solids', 'other gas', 
                                        'not defined', "other",  'petroleum coke', 'petroleum generic', 'plutonium', 'propane', 'refuse, bagasse and all other nonwood waste', 
                                        'refinery gas', 're refined motor oil', 'synthetic natural gas', 'steam', 'subbituminous coal', 'sun', 'thorium',  'topped crude oil', 'uranium', 
                                        'water', 'waste coal', 'wood and wood waste', 'waste heat', 'wind', 'other', 'not available', 'propane', 'purchased steam', 'refined coal', 
                                        "residual fuel oil", 'coal synfuel', 'synthetic gas other than coal derived', 'coal-derived synthetic gas', 'synthetic gas from petroleum coke', 
                                        'sludge waste', 'not defined', 'tires', 'not defined', 'wood waste liquids', 'wood and wood waste solids', 'oil-other and waste oil', 'not defined'))

eia.dict.6 = data.frame(fuel_3 = c("AB", "ANT", "BFG", "BIO", "BIT", "BL", "BLQ", "COG", "COL", "COM", "CRU", "CWM", "DFO", "FO1", "FO2", "FO3", "FO4",
                                   "FO5", "FO6", "GAS", "GEO", "GST", "JF", "KER", "LFG", "LIG", "LNG", "LPG", "MF", "MSW", "MTE", "MWH", "MTH", "NG", "NUC", 
                                   "OBG", "OBL", "OBS", "OG", "OO", "OTH", "PC", "PET", "PL", "PRO", "REF", "RG", "RRO", "SNG", "STM", "SUB", "SUN", "TH", "TOP", 
                                   "UR", "WAT", "WC", "WD", "WH", "WND", "OT", "NA", "PG", "PUR", "RC", "RFO", "SC", "SG", "SGC", "SGP", 'SLW', 'SU', 'TDF', 'UNK', 'WDL',
                                   "WDS", "WO", "WOC"),
                        fuel_3_text = c("Agriculture crop byproducts, straw, energy crops", "Anthracite", "blast furnace gas", 'biomass generic', 'bituminous coal', 'not defined',
                                        'black liquor', 'coke oven gas', 'coal generic', 'coal oil mixture', 'crude oil', 'coal water mixture', 'distillate fuel oil', 'no 1 fuel oil', 
                                        'no 2 fuel oil', 'no 3 fuel oil', 'no 4 fuel oil', 'no 5 fuel oil', 'no 6 fuel oil', 'gas generic', 'geothermal', 'geothermal steam', 
                                        'jet fuel', 'kerosene', 'landfill gas', 'lignite', 'liquified natural gas', 'liquified propane gas', 'multifueled', 'municipal solid waste',
                                        'methane', 'megawatt hour', 'methanol', 'natural gas', 'nuclear', 'other biomass gases', 'other biomass liquids', 'other biomass solids', 'other gas', 
                                        'not defined', "other",  'petroleum coke', 'petroleum generic', 'plutonium', 'propane', 'refuse, bagasse and all other nonwood waste', 
                                        'refinery gas', 're refined motor oil', 'synthetic natural gas', 'steam', 'subbituminous coal', 'sun', 'thorium',  'topped crude oil', 'uranium', 
                                        'water', 'waste coal', 'wood and wood waste', 'waste heat', 'wind', 'other', 'not available', 'propane', 'purchased steam', 'refined coal', 
                                        "residual fuel oil", 'coal synfuel', 'synthetic gas other than coal derived', 'coal-derived synthetic gas', 'synthetic gas from petroleum coke', 
                                        'sludge waste', 'not defined', 'tires', 'not defined', 'wood waste liquids', 'wood and wood waste solids', 'oil-other and waste oil', 'not defined'))
 # Abbreviations explained

cap.eia = cap.raw %>%
  filter(summer_capacity != 0) %>%
  # Remove plants with no summer time capacity (not sure why they exist in the data anyway)
  filter(status_code_1 != "RE" & status_code_2 != "RE") %>%
  # Remove retired plants
  filter(status_code_2 != "CN") %>%
  # Remove previously planned but now canceled projects
  filter(status_code_2 != "PL" & status_code_2 != "P" & status_code_2 != "IP") %>%
  # Remove planned projects (P and PL) and indefinitely postponed, 
  filter(status_code_2 != "CO" & status_code_2 != "L" & status_code_2 != "T") %>%
  # Remove units under construction, units awaiting regulatory approval, and units with regulatory approval but not under construciton
  filter(status_code_2 != "U" & status_code_2 != "V" & status_code_2 != "TS" & status_code_1 != "TS") %>%
  # Remove additional plants under construction (U and V) and constructed power plants not yet in operation
  filter(status_code_2 != "LE") %>%
  # Remove sites that are not constructed (legal delays)
  mutate(status_fulltext = 2) %>%
  mutate(age = year - in_service) %>%
  # Calculate the age of the plant
  left_join(eia.dict.3) %>%
  # Add a new column that includes an explanation of the prime mover variable
  left_join(eia.dict.1) %>%
  # Add a new column that includes an explanation of the status_code_1
  left_join(eia.dict.2) %>%
  # Add a column that includes an explanation of the status_code_2
  left_join(eia.dict.4) %>%
  left_join(eia.dict.5) %>%
  left_join(eia.dict.6)
  # Add three columns for the primary, secondary, and tertiary fuel used by each unit

gz1 = gzfile(paste(path_data,"capacity_eia_fueluse_productiontechnology_1990_2014.txt.gz", sep=""), "w")
write.table(cap.eia, file = gz1, sep="\t",col.names = TRUE, row.names = FALSE)
close(gz1)
# Output the cleaned up data to the data folder as a .txt.gz file

cap.eia.total = cap.eia %>%
  group_by(year, prime_mover, fuel_1, prime_mover_text, fuel_1_text) %>%
  # Add three new columns explaining the fuel type abbreviation
  summarize(total_summer_capacity = sum(summer_capacity, na.rm=TRUE))
  # Generate a new dataframe that is the sum of summer capacity by primary fuel and production technology

gz1 = gzfile(paste(path_data,"total_annual_capacity_eia_fueluse_technology_1990_2014.txt.gz", sep=""), "w")
write.table(cap.eia.total, file = gz1, sep="\t", col.names=TRUE, row.names=FALSE)
close(gz1)
# Output the total capacity by fuel and production technology to the data folder as a .txt.gz file

