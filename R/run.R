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



##################################################################################################
# n_dataset: number of the dataset in the "datasets.csv"                                         #
# number_cores: number of cores to paralell                                                      #
# number_folds: number of folds for cross validation                                             #
##################################################################################################
oraclePartitions <- function(id_part,
                             ds,
                             dataset_name,
                             number_dataset,
                             number_cores,
                             number_folds,
                             folderResults){

  diretorios <- directories(dataset_name, folderResults)

  if(number_cores == 0){

    cat("\n\n##########################################################")
    cat("\n# Zero is a disallowed value for number_cores. Please      #")
    cat("\n# choose a value greater than or equal to 1.               #")
    cat("\n############################################################\n\n")

  } else {

    cl <- parallel::makeCluster(number_cores)
    doParallel::registerDoParallel(cl)
    print(cl)

    if(number_cores==1){
      cat("\n\n##########################################################")
      cat("\n# Running Sequentially!                                    #")
      cat("\n############################################################\n\n")
    } else {
      cat("\n\n############################################################")
      cat("\n# Running in parallel with ", number_cores, " cores!         #")
      cat("\n##############################################################\n\n")
    }
  }
  cl = cl

  retorno = list()

  cat("\n\n##########################################################")
  cat("\n# Run: get labels                                          #")
  cat("\n##############################################################\n\n")
  arquivo = paste(diretorios$folderNamesLabels, "/" ,
                  dataset_name, "-NamesLabels.csv", sep="")
  namesLabels = data.frame(read.csv(arquivo))
  colnames(namesLabels) = c("id", "labels")
  namesLabels = c(namesLabels$labels)


  cat("\n\n##########################################################")
  cat("\n# Run: get bell partition information                      #")
  cat("\n##############################################################\n\n")
  info <- infoPartitions(id_part, dataset_name, folderResults)

  # criando a pasta específica da partição
  FolderPartition = paste(diretorios$folderTest, "/Partition-",
                          id_part, sep="")
  if(dir.exists(FolderPartition)==FALSE){dir.create(FolderPartition)}


  cat("\n\n##########################################################")
  cat("\n# Run: Build and Test Bell Partitions                      #")
  cat("\n##############################################################\n\n")
  timeComPart = system.time(resPart <- partition(id_part,
                                                 ds,
                                                 dataset_name,
                                                 number_dataset,
                                                 number_cores,
                                                 number_folds,
                                                 folderResults,
                                                 namesLabels))


  cat("\n\n##########################################################")
  cat("\n# Run: Matrix Correlation                                  #")
  cat("\n##############################################################\n\n")
  timeGather = system.time(resGather <- gather(id_part,
                                               ds,
                                               dataset_name,
                                               number_dataset,
                                               number_cores,
                                               number_folds,
                                               folderResults,
                                               namesLabels))


  cat("\n\n##########################################################")
  cat("\n# Run: Evaluation Fold                                     #")
  cat("\n##############################################################\n\n")
  timeEval = system.time(resEval <- eval(id_part,
                                         ds,
                                         dataset_name,
                                         number_dataset,
                                         number_cores,
                                         number_folds,
                                         folderResults,
                                         namesLabels))


  cat("\n\n##########################################################")
  cat("\n# Run: Gather Evaluation                                   #")
  cat("\n##############################################################\n\n")
  timeGE = system.time(resGE <- gatherEvaluation(id_part,
                                                 ds,
                                                 dataset_name,
                                                 number_dataset,
                                                 number_cores,
                                                 number_folds,
                                                 folderResults,
                                                 namesLabels))


  cat("\n\n##########################################################")
  cat("\n# Run: Save Runtime                                       #")
  cat("\n##############################################################\n\n")
  Runtime = rbind(timeComPart, timeGather, timeEval, timeGE)
  setwd(FolderPartition)
  name = paste("Partition-", info$numberOfPartition ,"-Runtime-run.csv", sep="")
  write.csv(Runtime, name)




  cat("\n\n##########################################################")
  cat("\n# Run: Stop Parallel                                       #")
  cat("\n##############################################################\n\n")
  parallel::stopCluster(cl)

  cat("\n\n##########################################################")
  cat("\n# RUN FINISH                                               #")
  cat("\n##############################################################\n\n")

  gc()
}


##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
