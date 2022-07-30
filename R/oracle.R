cat("\n\n################################################################")
cat("\n# START EXECUTE ORACLE ECC                                       #")
cat("\n##################################################################\n\n")


# clean
rm(list=ls())


###############################################################################
# Oracle Partitions with Ensemble of Classifier Chain                         #
# Copyright (C) 2022                                                          #
#                                                                             #
# This code is free software: you can redistribute it and/or modify it under  #
# the terms of the GNU General Public License as published by the Free        #
# Software Foundation, either version 3 of the License, or (at your option)   #
# any later version. This code is distributed in the hope that it will be     #
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of      #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General    #
# Public License for more details.                                            #
#                                                                             #
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri Ferrandin  #
# Federal University of Sao Carlos (UFSCar: https://www2.ufscar.br/) |        #
# Campus Sao Carlos | Computer Department (DC: https://site.dc.ufscar.br/)    #
# Program of Post Graduation in Computer Science                              #
# (PPG-CC: http://ppgcc.dc.ufscar.br/) | Bioinformatics and Machine Learning  #
# Group (BIOMAL: http://www.biomal.ufscar.br/)                                #
###############################################################################



###############################################################################
# SET WORKSAPCE                                                               #
###############################################################################
FolderRoot = "~/Oracle-ECC"
FolderScripts = paste(FolderRoot, "/R", sep="")


###############################################################################
# LOAD SOURCES
###############################################################################
setwd(FolderScripts)
source("libraries.R")

setwd(FolderScripts)
source("utils.R")

setwd(FolderScripts)
source("buildAndTestPartitions.R")

setwd(FolderScripts)
source("evaluation.R")

setwd(FolderScripts)
source("BestWorstPartitions.R")

setwd(FolderScripts)
source("run.R")


###############################################################################
# R Options Configuration                                                     #
###############################################################################
options(java.parameters = "-Xmx64g")  # JAVA
options(show.error.messages = TRUE)   # ERROR MESSAGES
options(scipen=20)                    # number of places after the comma



###############################################################################
# Reading the "datasets-original.csv" file to get dataset information         #
# for code execution!                                                         #
###############################################################################
setwd(FolderRoot)
datasets <- data.frame(read.csv("datasets-original.csv"))



###############################################################################
# ARGS COMMAND LINE                                                          #
###############################################################################
cat("\n#####################################")
cat("\n# GET ARGUMENTS FROM COMMAND LINE   #")
cat("\n#####################################\n\n")
args <- commandArgs(TRUE)



###############################################################################
# FIRST ARGUMENT: getting specific dataset information being processed        #
# from csv file                                                               #
###############################################################################

#config_file = "/home/cissa/Oracle-ECC/O-Config-Files/O-GpositiveGO.csv"

config_file <- args[1]


if(file.exists(config_file)==FALSE){
  cat("\n################################################################")
  cat("#\n Missing Config File! Verify the following path:              #")
  cat("#\n ", config_file, "                                            #")
  cat("#################################################################\n\n")
  break
} else {
  cat("\n########################################")
  cat("\n# Properly loaded configuration file!  #")
  cat("\n########################################\n\n")
}


cat("\n########################################")
cat("\n# Config File                          #\n")
config = data.frame(read.csv(config_file))
print(config)
cat("\n########################################\n\n")

dataset_path = toString(config$Value[1])
dataset_path = str_remove(dataset_path, pattern = " ")

folderResults = toString(config$Value[2])
folderResults = str_remove(folderResults, pattern = " ")

bell_partitions_path = toString(config$Value[3])
bell_partitions_path = str_remove(bell_partitions_path , pattern = " ")

dataset_name = toString(config$Value[4])
dataset_name = str_remove(dataset_name, pattern = " ")

number_dataset = as.numeric(config$Value[5])
number_folds = as.numeric(config$Value[6])
number_cores = as.numeric(config$Value[7])

ds = datasets[number_dataset,]


cat("\n################################################################\n")
print(ds)
cat("\n# DATASET PATH: \t", dataset_path)
cat("\n# TEMPORARY PATH: \t", folderResults)
cat("\n# BELL PARTITIONS PATH: \t", bell_partitions_path)
cat("\n# DATASET NAME:  \t", dataset_name)
cat("\n# NUMBER DATASET: \t", number_dataset)
cat("\n# NUMBER X-FOLDS CROSS-VALIDATION: \t", number_folds)
cat("\n# NUMBER CORES: \t", number_cores)
cat("\n################################################################\n\n")


###############################################################################
# Creating temporary processing folder                                        #
###############################################################################
if (dir.exists(folderResults) == FALSE) {dir.create(folderResults)}


###############################################################################
# Creating all directories that will be needed for code processing            #
###############################################################################
cat("\n######################")
cat("\n# Get directories    #")
cat("\n######################\n")
diretorios <- directories(dataset_name, folderResults)
print(diretorios)
cat("\n\n")


###############################################################################
# Copying datasets from ROOT folder on server                                 #
###############################################################################

cat("\n####################################################################")
cat("\n# Checking the dataset tar.gz file                                 #")
cat("\n####################################################################\n\n")
str00 = paste(dataset_path, "/", ds$Name,".tar.gz", sep = "")
str00 = str_remove(str00, pattern = " ")

if(file.exists(str00)==FALSE){

  cat("\n######################################################################")
  cat("\n# The tar.gz file for the dataset to be processed does not exist!    #")
  cat("\n# Please pass the path of the tar.gz file in the configuration file! #")
  cat("\n# The path entered was: ", str00, "                                  #")
  cat("\n######################################################################\n\n")
  break

} else {

  cat("\n####################################################################")
  cat("\n# tar.gz file of the dataset loaded correctly!                     #")
  cat("\n####################################################################\n\n")

  # COPIANDO
  str01 = paste("cp ", str00, " ", diretorios$folderDataset, sep = "")
  res = system(str01)
  if (res != 0) {
    cat("\nError: ", str01)
    break
  }

  # DESCOMPACTANDO
  str02 = paste("tar xzf ", diretorios$folderDataset, "/", ds$Name,
                ".tar.gz -C ", diretorios$folderDataset, sep = "")
  res = system(str02)
  if (res != 0) {
    cat("\nError: ", str02)
    break
  }

  #APAGANDO
  str03 = paste("rm ", diretorios$folderDataset, "/", ds$Name,
                ".tar.gz", sep = "")
  res = system(str03)
  if (res != 0) {
    cat("\nError: ", str03)
    break
  }

}



###############################################################################
# copying partitions from ROOT folder on server                               #
###############################################################################

cat("\n####################################################################")
cat("\n# Checking the all partitions (bell partitions) tar.gz file        #")
cat("\n####################################################################\n\n")
str00 = paste(bell_partitions_path, "/", ds$Name,".tar.gz", sep = "")
str00 = str_remove(str00, pattern = " ")


if(file.exists(str00)==FALSE){

  cat("\n######################################################################")
  cat("\n# The tar.gz file for the partitions to be processed does not exist! #")
  cat("\n# Please pass the path of the tar.gz file in the configuration file! #")
  cat("\n# The path entered was: ", str00,  "                                 #")
  cat("\n######################################################################\n\n")
  break

} else{

  cat("\n####################################################################")
  cat("\n# tar.gz file of the partitions loaded correctly!                  #")
  cat("\n####################################################################\n\n")

  # COPIANDO
  str01 = paste("cp ", str00, " ", diretorios$folderResults, sep = "")
  res = system(str01)
  if (res != 0) {
    cat("\nError: ", str01)
    break
  }

  # DESCOMPACTANDO
  str02 = paste("tar xzf ", diretorios$folderResults, "/", ds$Name,
                ".tar.gz -C ", diretorios$folderBellPart, sep = "")
  res = system(str02)
  if (res != 0) {
    cat("\nError: ", str02)
    break
  }

  # APAGANDO
  str03 = paste("rm ", diretorios$folderResults, "/",
                ds$Name, ".tar.gz", sep = "")
  res = system(str03)
  if (res != 0) {
    cat("\nError: ", str03)
    break
  }

}



###############################################################################
# Getting the total number of possible partitions for the specific dataset    #
###############################################################################
setwd(diretorios$folderBellPartDataset)
str_ = paste(dataset_name, "-groupsPerPartitions.csv", sep = "")
bell = data.frame(read.csv(str_))
n = nrow(bell)


###############################################################################
#
###############################################################################
apagar = c(0)
result = data.frame(apagar)
namesMeasures = c("")


###############################################################################
#
###############################################################################
count = 2
id_part = 2
while (id_part <= n) {

  #number_cores = 1

  diretorios <- directories(dataset_name, folderResults)

  FolderPartition = paste(diretorios$folderTest, "/Partition-",
                          id_part, sep="")

  cat("\nPARTITION: ", id_part, "\n")

  ############################################################################
  # execute oracle experiment
  timeEP = system.time(res <- oraclePartitions(id_part,
                                               ds,
                                               dataset_name,
                                               number_dataset,
                                               number_cores,
                                               number_folds,
                                               folderResults))

  result_set <- t(data.matrix(timeEP))
  setwd(diretorios$folderTest)
  write.csv(result_set, paste("Partition-", id_part, "-Runtime.csv"))

  ###########################################################################
  # compress files
  cat("\n Compress folders and files")
  str3a <- paste( "tar -zcf ", diretorios$folderTest, "/", dataset_name,
                  "-Partition-", id_part, "-results.tar.gz " ,
                  diretorios$folderTest, sep = "")
  system(str3a)

  ###########################################################################
  cat("\n Copy to google drive")
  origem = paste(diretorios$folderTest, "/", dataset_name,
                 "-Partition-", id_part, "-results.tar.gz", sep = "")
  destino = paste("nuvem:[2022]ResultadosExperimentos/ECC/Oracle/",
                  dataset_name, sep = "")
  comando = paste("rclone -P copy ", origem, " ", destino, sep = "")
  cat("\n", comando, "\n")
  a = print(system(comando))
  a = as.numeric(a)
  if (a != 0) {
    stop("Erro RCLONE")
    quit("yes")
  }

  ###########################################################################
  cat("\n delete files")
  comando2 = paste("rm -r ", origem)
  print(system(comando2))

  comando3 = paste("rm -r ", FolderPartition, sep="")
  print(system(comando3))

  ###########################################################################
  # count
  id_part = id_part + 1
  count = count + 1
  cat("\n")
  gc()

}



cat("\n###################################################################")
cat("\n# COPY TEST RESULTS TO GOOGLE DRIVE                               #")
cat("\n###################################################################\n\n")
origem = diretorios$folderTest
destino = paste("nuvem:[2022]ResultadosExperimentos/ECC/Oracle/",
                dataset_name, sep = "")
comando = paste("rclone -P copy ", origem, " ", destino, sep = "")
cat("\n", comando, "\n")
a = print(system(comando))
a = as.numeric(a)
if (a != 0) {
  stop("Erro RCLONE")
  quit("yes")
}



cat("\n###################################################################")
cat("\n# DELETE TEST FOLDER                                              #")
cat("\n###################################################################\n\n")
print(system(paste("rm -r ", diretorios$folderTest, sep="")))



cat("\n#######################################################################")
cat("\nCompute Best Measures Partitions per Fold                            #")
cat("\n#######################################################################\n\n")
timeBMP = system.time(bestMeasuresPartitions(id_part,
                                             ds,
                                             dataset_name,
                                             number_dataset,
                                             number_cores,
                                             number_folds,
                                             folderResults,
                                             namesLabels))



cat("\n######################################################################")
cat("\nCompute Best and Worst partition                                     #")
cat("\n######################################################################\n\n")
timeBWP = system.time(bwPartition())


cat("\n######################################################################")
cat("\n# SAVE RUNTIME                                                       #")
cat("\n######################################################################\n\n")
runtime = rbind(timeBMP, timeBWP)
setwd(diretorios$folderTempResults)
write.csv(runtime, "Other-Runtime.csv")


cat("\n######################################################################")
cat("\n# Copy to google drive                                               #")
cat("\n######################################################################\n\n")
origem = diretorios$folderTempResults
destino = paste("nuvem:[2022]ResultadosExperimentos/ECC/Oracle/",
               dataset_name, sep = "")
comando = paste("rclone -P copy ", origem, " ", destino, sep = "")
cat("\n", comando, "\n")
a = print(system(comando))
a = as.numeric(a)
if (a != 0) {
 stop("Erro RCLONE")
 quit("yes")
}



cat("\n######################################################################")
cat("\n# COPY TO FOLDER REPORTS                                             #")
cat("\n######################################################################\n\n")
origem = paste(diretorios$folderTempResults, "/*", sep="")
destino = diretorios$folderReportsD
print(system(paste("cp -r ", origem, " ", destino, sep="")))



cat("\n######################################################################")
cat("\n# DELETE FOLDERS                                                     #")
cat("\n######################################################################\n\n")
system(paste("rm -r ", diretorios$folderResults, sep = ""))



gc()
cat("\n##################################################################################################")
cat("\n# END OF ORACLE PARTITIONS. Thanks God!                                                          #")
cat("\n##################################################################################################")
cat("\n\n\n\n")

rm(list = ls())


##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
