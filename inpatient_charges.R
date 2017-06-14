# Amber Garner

#************SETUP***************************************

# Install and load needed packages
install.packages("discretization")
install.packages("arules")
install.packages("sqldf")
install.packages("outliers")
install.packages("ggplot2")
library(discretization)
library(arules)
library(sqldf)
library(outliers)
library(ggplot2)

# Read in csv file called inpatientCharges with headers and seperated by comma
charges <- read.csv("inpatient_charges.csv", header = TRUE, sep = ",")


#**********PRE-PROCESSED DATA DESCRIPTION***********

# Print first 4 rows of data set
head(charges, 6)

# Print structure of data set
str(charges)

# Print summary of data set
summary(charges)

# View the data set in a table
View(charges)

#**************DATA PROCESSING****************

# change names of columns
names(charges)<-c("drg_def", "provider_id", "provider_name", "st_add", 
"city", "state", "zipcode", "referral_region_desc", "total_discharges", 
"avg_cov_charges", "avg_total_payments", "avg_medicare_payments")

# Verify names were changes correctly
names(charges)

# create new variable named creg
# subset charges to only include records of hospitals in the south
creg <- subset(charges, subset= state %in% c("FL", "GA", "MD", "DC","NC","SC", "VA", "WV", "DE", "AL", "KY", "MS","TN", "AR", "LA", "OK", "TX"))

#remove unused levels
creg <- droplevels(creg)

# Verify subset worked properly
summary(creg$state)

# create new column for divisions based on state of provider
creg$division<-ifelse(creg$state %in% c("DC","DE", "FL", "GA", "MD","NC","SC", "VA", "WV"),"SOUTH ATLANTIC",
ifelse(creg$state %in% c("AL","KY","MS", "TN"), "EAST SOUTH CENTRAL",
ifelse(creg$state %in% c("AR","LA", "OK", "TX"),"WEST SOUTH CENTRAL","OTHER")))

# convert from division character to factor type
creg$division <- factor(creg$division)

# verify new attribute is there and is factor
summary(creg$division)
str(creg$division)

# Convert average covered charges, average total payments, & average medicare payments to numeric
convert <- strsplit(x=as.character(creg$avg_cov_charges), split = "$", fixed = T)
creg$avg_cov_charges <- as.numeric(sapply(convert, "[[",2))
convert <- strsplit(x=as.character(creg$avg_total_payments), split = "$", fixed = T)
creg$avg_total_payments <- as.numeric(sapply(convert, "[[",2))
convert <- strsplit(x=as.character(creg$avg_medicare_payments), split = "$", fixed = T)
creg$avg_medicare_payments <- as.numeric(sapply(convert, "[[",2))

# remove convert 
rm(convert)

# verify the average columns were converted to numeric
str(creg[,10:12])

# subset to only one dfg definition
small <- subset(creg, subset= creg$drg_def=="190 - CHRONIC OBSTRUCTIVE PULMONARY DISEASE W MCC")

# check for inconsistencies in smaller set
# find length of unique values in id, name, & address columns
length(unique(small$provider_id))

length(unique(small$provider_name))

length(unique(small$st_add))

# return rows in view with duplicate names
View(sqldf("SELECT provider_id, provider_name, st_add, city FROM small AS T1 
WHERE EXISTS (
           SELECT provider_name, COUNT(*) AS [Count] 
           FROM small 
           GROUP BY provider_name 
           HAVING (COUNT(*) > 1) AND (provider_name = T1.provider_name))
           ORDER BY provider_name"))

# returns rows with duplicate address
View(sqldf("SELECT provider_id, provider_name, st_add, city FROM small AS T1 
WHERE EXISTS (
      SELECT st_add, COUNT(*) AS [Count] 
      FROM small 
      GROUP BY st_add 
      HAVING (COUNT(*) > 1) AND (st_add = T1.st_add))
      ORDER BY st_add"))
# remove small dataframe 
rm(small)

# Remove the 2nd column- Provider.ID from the data set
creg <- creg[,-2]

# Print names to verify delete for Provider.ID
names(creg)

# Look at distribution of total discharges variable
summary(creg$total_discharges)

# Find outliers
boxplot.stats(creg$total_discharges, coef = 3)
boxplot.stats(creg$avg_cov_charges, coef = 3)
boxplot.stats(creg$avg_total_payments, coef = 3)
boxplot.stats(creg$avg_medicare_payment, coef = 3)

# remove outliers
new_creg <- creg[creg$total_discharges <149,]

#************1st VISUAL****************
# Scatterplot before outlier removal of total discharges
plot(creg$total_discharges, col = 3, ylab = "total discharges", las=1, cex=0.5, main = "Total Discharges Before")
# Scatterpolot after outlier removal of total discharges
plot(new_creg$total_discharges, col = 4, ylab = "total discharges", las=1, cex=0.5, main="Total Discharges After")

#************2ND VISUAL****************
# get sum of total discharges for each drg def
agg <- aggregate(new_creg$total_discharges, by=list(new_creg$drg_def), sum)

# change names of columns
names(agg)<-c("drg_def", "discharge_total")

# Sort descending by discharge
sortagg <- agg[order(agg$discharge_total, decreasing = T),]

# Find top 10 and bottom 10
topten <- head(sortagg, 10)
bottomten <- tail(sortagg, 10)

# Barchart top 10
ggplot(topten, aes(x=reorder(drg_def,discharge_total), y=discharge_total, fill=drg_def)) + geom_bar(,stat="identity") + coord_flip()+ theme(text = element_text(size=8), legend.position="none") + ggtitle("Top DRG by Discharge") + xlab("DRG Definition") + ylab("Total Discharges")

# Barchart bottom 10
ggplot(bottomten, aes(x=reorder(drg_def,discharge_total), y=discharge_total, fill=drg_def)) + geom_bar(,stat="identity") + coord_flip()+ theme(text = element_text(size=8), legend.position="none") + ggtitle("Bottom DRG by Discharge") + xlab("DRG Definition") + ylab("Total Discharges")

#*************CONTINUE DATA PROCESSING****************
# Convert to factor variable with cluster method & 12 categories
new_creg$total_discharges <- discretize(new_creg$total_discharges, "cluster", categories = 12)

# Verify that variable is now a factor 
summary(new_creg$total_discharges)

#**********POST-PROCESSED DATA DESCRIPTION***************
# Summary Statistics after changes
head(new_creg)
summary(new_creg)
str(new_creg)

#**************3RD VISUAL********************************

# Create boxplot of average total charges by division
ggplot(new_creg, aes(division, avg_total_payments, fill=division)) + geom_boxplot(outlier.shape=1) + ggtitle("Average Total Payments by Division") + theme(axis.text.x=element_text(angle=90), legend.position="none") + coord_cartesian(ylim=c(0,25000))

#***************THE END**********************************