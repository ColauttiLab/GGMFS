##########################################################
### This script counts basic info about the project    ###
### Number of sites, measurements, etc.                ###
### No output files; all results shown in command line ###
##########################################################

###########################################################################################
#Reading in data and creating indices
###########################################################################################
PopData<-read.csv("CorrectedDataAll.csv",as.is=T)

# Creating an index for columns that indicate the number of individuals in each plot. 
Counts<-grep("P[0-9]{1,2}Adult+$|P[0-9]{1,2}Ros+$",names(PopData))

#' Creating an index for columns that describe data on a plant individual 
#' (i.e., size, number of leaves, fecundity, leaf damage and fungal damage). 
Measures<-grep("P[0-9]{1,2}[A-z]+[0-9]{1,2}",names(PopData))
colnames(PopData[,Measures])

#Creating an index for all other columns
Other<-grep("Pop_Size|Pct_Canopy_Cover|Population_Type|None_Known|Collection_Date|Region",names(PopData))
###########################################################################################



###########################################################################################
# Some basic statistics on the success of the project
###########################################################################################

# Number of measurements
sum(colSums(!is.na(PopData[,Measures])))
# Number of plants counted
sum(colSums(PopData[,Counts],na.rm=T))
# Number of Height Measurements
sum(colSums(!is.na(PopData[,grep("Adult[0-9]+",names(PopData))])))
# Number of Fruit Measurements
sum(colSums(!is.na(PopData[,grep("Fruits[0-9]+",names(PopData))])))
# Herbivore Damage
sum(colSums(!is.na(PopData[,grep("Dmg[0-9]+",names(PopData))])))
# Fungal Damage
sum(colSums(!is.na(PopData[,grep("Fung[0-9]+",names(PopData))])))

# Find Unique Sites
UniPops<-PopData[!duplicated(PopData[,c("Latitude","Longitude")]),]




# Number of unique population locations
nrow(unique(UniPops))
# Number of unique North American Populations
nrow(unique(UniPops[UniPops$Region=="NorthAm",]))
# Number of unique European Populations
nrow(unique(UniPops[UniPops$Region=="Europe",]))
###########################################################################################






