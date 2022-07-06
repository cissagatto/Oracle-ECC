##################################################################################################
# ORACLE PARTITIONS WITH ECC                                                                     #
# Copyright (C) 2022                                                                             #
#                                                                                                #
# This code is free software: you can redistribute it and/or modify it under the terms of the    #
# GNU General Public License as published by the Free Software Foundation, either version 3 of   #
# the License, or (at your option) any later version. This code is distributed in the hope       #
# that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of         #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for    #
# more details.                                                                                  #
#                                                                                                #
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri Ferrandin                     #
# Federal University of Sao Carlos (UFSCar: https://www2.ufscar.br/) Campus Sao Carlos           #
# Computer Department (DC: https://site.dc.ufscar.br/)                                           #
# Program of Post Graduation in Computer Science (PPG-CC: http://ppgcc.dc.ufscar.br/)            #
# Bioinformatics and Machine Learning Group (BIOMAL: http://www.biomal.ufscar.br/)               #
#                                                                                                #
##################################################################################################


##################################################################################################
# Script 6 - Run                                                                                 #
##################################################################################################



##################################################################################################
# Configures the workspace according to the operating system                                     #
##################################################################################################
FolderRoot = "~/Oracle-ECC"
FolderScripts = paste(FolderRoot, "/R", sep="")


##################################################################################################
# LOAD INTERNAL LIBRARIES                                                                        #
##################################################################################################
cat("\nLoad Sources")

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


##################################################################################################
# n_dataset: number of the dataset in the "datasets.csv"                                         #
# number_cores: number of cores to paralell                                                      #
# number_folds: number of folds for cross validation                                             #
##################################################################################################
oraclePartitions <- function(number_dataset, number_cores, number_folds, id_part, folderResults){

  diretorios <- directories(dataset_name, folderResults)

  if(number_cores == 0){
    cat("\nZero is a disallowed value for number_cores. Please choose a value greater than or equal to 1.")
  } else {
    cl <- parallel::makeCluster(number_cores)
    doParallel::registerDoParallel(cl)
    print(cl)

    if(number_cores==1){
      cat("\n\n################################################################################################")
      cat("\n# Running Sequentially!                                                                          #")
      cat("\n##################################################################################################\n\n")
    } else {
      cat("\n\n################################################################################################")
      cat("\n# Running in parallel with ", number_cores, " cores!                                             #")
      cat("\n##################################################################################################\n\n")
    }
  }
  cl = cl

  retorno = list()

  cat("\n\n################################################################################################")
  cat("\n# RUN: Get dataset information: ", number_dataset, "                                             #")
  ds = datasets[number_dataset,]
  names(ds)[1] = "Id"
  dataset_name = toString(ds$Name)
  cat("\n# Dataset: ", dataset_name, "                                                                     #")
  cat("\n\n################################################################################################")

  cat("\n\n################################################################################################")
  cat("\n# Run: get labels")
  arquivo = paste(diretorios$folderNamesLabels, "/" ,
                  dataset_name, "-NamesLabels.csv", sep="")
  namesLabels = data.frame(read.csv(arquivo))
  colnames(namesLabels) = c("id", "labels")
  namesLabels = c(namesLabels$labels)
  cat("\n\n################################################################################################")

  cat("\n\n################################################################################################")
  cat("\n# Run: get bell partition information")
  info <- infoPartitions(id_part, dataset_name, folderResults)
  cat("\n\n################################################################################################")

  cat("\n\n################################################################################################")
  cat("\n# RUN: Build and Test Bell Partitions                                                            #")
  timeComPart = system.time(resPart <- partition(id_part,
                                                 ds,
                                                 dataset_name,
                                                 number_folds,
                                                 namesLabels,
                                                 folderResults))
  cat("\n\n################################################################################################")

  cat("\n\n################################################################################################")
  cat("\n# RUN: Matrix Correlation                                                                        #")
  timeGather = system.time(resGather <- gather(id_part, ds, dataset_name, number_folds,
                                               namesLabels, folderResults))
  cat("\n\n################################################################################################")

  cat("\n\n################################################################################################")
  cat("\n# Run: Evaluation Fold                                                                            #")
  timeEval = system.time(resEval <- eval(id_part, ds, dataset_name, number_folds,
                                         namesLabels, folderResults))
  cat("\n\n################################################################################################")

  cat("\n\n################################################################################################")
  cat("\n# Run: Gather Evaluation                                                                          #")
  timeGE = system.time(resGE <- gatherEvaluation(id_part, ds, dataset_name, number_folds,
                                                 namesLabels, folderResults))
  cat("\n\n################################################################################################")

  cat("\n\n################################################################################################")
  cat("\n# Run: Save Runtime                                                                               #")
  Runtime = rbind(timeComPart, timeGather, timeEval, timeGE)
  Folder <- paste(diretorios$folderResultDataset, "/Partition-", id_part, sep="")
  setwd(Folder)
  name = paste("Partition-", info$numberOfPartition ,"-Runtime-run.csv", sep="")
  write.csv(Runtime, name)
  cat("\n\n################################################################################################")

  cat("\n\n################################################################################################")
  setwd(diretorios$folderReportsDataset)
  name = paste("Partition-", info$numberOfPartition ,"-Runtime-run.csv", sep="")
  write.csv(Runtime, name)
  cat("\n\n################################################################################################")

  cat("\n# Run: Stop Parallel                                                                              #")
  parallel::stopCluster(cl)

  gc()
}


##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
