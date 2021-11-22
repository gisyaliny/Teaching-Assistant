data(shpSEA)

shpSEA$REGIONS <- car::recode(shpSEA$STATE,'
                              c("AK","WA","OR","CA","HI") = "Pacific";
                              c("MT","ID","WY","NV","UT","CO","AZ","NM") = "Mountain";
                              c("ND","SD","MN","NE","IA","KS","MO") = "West North Central";
                              c("TX","OK","AR","LA") = "West South Central";
                              c("WI","IL","IN","OH","MI") = "East North Central";
                              c("MS","AL","TN","KY") = "East South Central";
                              c("FL","GA","SC","NC","VA","WV","DC","MD","DE") = "South Atlantic";
                              c("PA","NJ","NY") = "Middle Atlantic";
                              c("CT","RI","MA","NH","VT","ME") = "New England";
                              else = NA
                              ')
table(shpSEA$REGIONS)
sum(table(shpSEA$REGIONS))
