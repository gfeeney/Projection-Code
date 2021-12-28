# Purpose is to illustrate the table we want from the 2009 census projections
# by showing the table for an early version of the 2019 census projections
# See also the email to which the resulting .xlsx file is attached and 
# Feeney-2017-IUSSP-Capetown.pdf Section 4, pages 7-8.

rm(list = ls())
x <- readRDS("../projection-outputs/ProjectionList-MIGRATION.rds")
names(x)
names(x[[2]])
pframe <- x$Mombasa$pcycle1
pframe

atp <- readRDS("../projection-inputs/AgeTimePlace.rds")
atp$Age
ASDrows <- atp$Age$ASDrows
pframe[ASDrows, "NIM"]

nmt <- round(as.matrix(do.call(cbind, lapply(x, function(x){x$pcycle1[ASDrows, "NIM"]}))), 0)
View(nmt)
write.csv(nmt, "../projection-outputs/internal-migration/net-migration-adjustments.csv")
