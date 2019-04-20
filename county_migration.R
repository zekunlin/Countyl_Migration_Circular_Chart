library(data.table)
library(stringr)


# read data in
migratCsvAddres <- "E:/slr_project_data/us_revenue_migration_data/countyinflow1516.csv"
affected_cnty <- "E:/slr_project_data/counties_affected_6ftslr.txt"
cntyflowin <- fread(migratCsvAddres, check.names = F, stringsAsFactors = F)
cntyAffected <- fread(affected_cnty, check.names = F, stringsAsFactors = F)
cntyAffectedFips <- cntyAffected$COUNTYFP
nc_cnty <- data.table(y2_countyfips=cntyflowin$y1_countyfips, y2_countyname=cntyflowin$y1_countyname)

# consider the county level redistribution only (only happen inside the county)
nc_cnty_flowin <- cntyflowin[cntyflowin$y1_statefips==37 & cntyflowin$y2_statefips==37,] 
AffecMigrat <- nc_cnty_flowin[nc_cnty_flowin$y1_countyfips %in% cntyAffectedFips | 
                                nc_cnty_flowin$y2_countyfips %in% cntyAffectedFips,]
AffecSumary <- AffecMigrat[AffecMigrat$y1_countyfips == AffecMigrat$y2_countyfips,]
AffecMigratDetail <- AffecMigrat[, AffecMigrat[!AffecMigrat$y1_countyfips == 
                                                 AffecMigrat$y2_countyfips,],]
#id <- as.character(AffecMigratDetail$y1_countyname)
#AffecMigratDetail <- cbind(id,AffecMigratDetail)

outflowAddres <- "E:/slr_project_data/us_revenue_migration_data/countyoutflow1516.csv"
outflow <- fread(outflowAddres)
nc_outflow <- outflow[outflow$y1_statefips==37 & outflow$y2_statefips==37,] 
affectOutflow <- nc_outflow[nc_outflow$y1_countyfips %in% cntyAffectedFips | nc_outflow$y2_countyfips
                            %in% cntyAffectedFips,]
affectOutflowDetail <- affectOutflow[, affectOutflow[!affectOutflow$y1_countyfips == 
                                                       affectOutflow$y2_countyfips,],]

y2_names <- data.table(y2_countyname=affectOutflowDetail$y2_countyname, 
                       y2_countyfips=affectOutflowDetail$y2_countyfips)

y2_names <- y2_names[order(y2_names$y2_countyfips),]
AffecMigratDetail <- AffecMigratDetail[order(AffecMigratDetail$y2_countyfips),]
table2write <- data.frame(AffecMigratDetail, y2_countyname=as.character(y2_names$y2_countyname))
id=as.character(table2write$y2_countyname)
table2write <- cbind(id, table2write, group=links_table$group)
table2write$y2_countyname <- str_remove_all(table2write$y2_countyname, " County")
table2write$y1_countyname <- str_remove_all(table2write$y1_countyname, " County")
fwrite(table2write, file = "E:/slr_project_data/nc_revenue_migrat_cnty_6ftslr.csv",
      col.names = T, row.names = F, sep = ",")


# lets get the nodes table:
nodes_part1 <- as.data.frame(cbind(table2write$y1_countyfips, 
                                   as.character(table2write$y1_countyname), table2write$agi))
nodes_part2 <- as.data.frame(cbind(table2write$y2_countyfips, 
                                   as.character(table2write$y2_countyname), table2write$agi))

nodes_names <- rbind(nodes_part1, nodes_part2)
nodes_names<- nodes_names[!duplicated(nodes_names[,1]),]
dim(nodes_names)
nodes_names$group <- 0
nodes_names$group[nodes_names$V1 %in% cntyAffectedFips] <- "coastal"
nodes_names$group[nodes_names$group != "coastal"] <- "inland"

considred_fips <- sort(unique(c(table2write$y1_countyfips, table2write$y2_countyfips)))

nodes <- nodes_names
colnames(nodes) <- c("id", "county", "agi", "group")
nodes$county <- str_remove_all(nodes$county," County")
fwrite(nodes, file="E:/slr_project_data/nc_revenue_nodes.csv", col.names = T, row.names = F,
       sep=",")

links_table <- data.frame(source_id=table2write$y1_countyfips, 
                          target_id=table2write$y2_countyfips,
                          return=table2write$n1, agi=table2write$agi)
links_table$group <- 0
links_table$group[links_table$target_id %in% cntyAffectedFips] <- "coastal"
links_table$group[links_table$group != "coastal"] <- "inland"

fwrite(links_table, file = "E:/slr_project_data/nc_revenue_links.csv", 
       col.names = T, row.names = F, sep = ",")

