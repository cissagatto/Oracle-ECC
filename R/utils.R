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
# Script 2 - Utils
##################################################################################################


##################################################################################################
# Configures the workspace according to the operating system                                     #
##################################################################################################
FolderRoot = "~/Oracle-ECC"
FolderScripts = paste(FolderRoot, "/R", sep="")


##################################################################################################
# FUNCTION DIRECTORIES                                                                           #
#   Objective:                                                                                   #
#      Creates all the necessary folders for the project. These are the main folders that must   #
#      be created and used before the script starts to run                                       #
#   Parameters:                                                                                  #
#      None                                                                                      #
#   Return:                                                                                      #
#      All path directories                                                                      #
##################################################################################################
directories <- function(dataset_name, folderResults){

  retorno = list()

  # FOLDER RESULTS
  if(dir.exists(folderResults) == TRUE){
    setwd(folderResults)
    setwd(folderResults)
    dir_folderResults = dir(folderResults)
    n_folderResults = length(dir_folderResults)
  } else {
    dir.create(folderResults)
    setwd(folderResults)
    dir_folderResults = dir(folderResults)
    n_folderResults = length(dir_folderResults)
  }

  # ~/Oracle-ECC/Reports/
  folderReports = paste(FolderRoot, "/Reports", sep="")
  if(dir.exists(folderReports) == TRUE){
    setwd(folderReports)
    dirReports = dir(folderReports)
    n_Reports = length(folderReports)
  } else {
    dir.create(folderReports)
    setwd(folderReports)
    dirReports = dir(folderReports)
    n_Reports = length(dirReports)
  }

  # ~/Oracle-ECC/Reports/GpositiveGO
  folderReportsDataset = paste(folderReports, "/", dataset_name, sep="")
  if(dir.exists(folderReportsDataset) == TRUE){
    setwd(folderReportsDataset)
    dir_folderReportsDataset = dir(folderReportsDataset)
    n_folderReportsDataset = length(dir_folderReportsDataset)
  } else {
    dir.create(folderReportsDataset)
    setwd(folderReportsDataset)
    dir_folderReportsDataset = dir(folderReportsDataset)
    n_folderReportsDataset = length(dir_folderReportsDataset)
  }

  # "~/Oracle-ECC/Reports/GpositiveGO"
  folderUtils = paste(FolderRoot, "/utils", sep="")
  if(dir.exists(folderUtils) == TRUE){
    setwd(folderUtils)
    dirUtils = dir(folderUtils)
    n_Utils = length(folderUtils)
  } else {
    dir.create(folderUtils)
    setwd(folderUtils)
    dirUtils = dir(folderUtils)
    n_Utils = length(dirUtils)
  }

  # "/dev/shm/res/datasets"
  folderDatasets = paste(folderResults, "/datasets", sep="")
  if(dir.exists(folderDatasets) == TRUE){
    setwd(folderDatasets)
    dirDatasets = dir(folderDatasets)
    n_Datasets = length(dirDatasets)
  } else {
    dir.create(folderDatasets)
    setwd(folderDatasets)
    dirDatasets = dir(folderDatasets)
    n_Datasets = length(dirDatasets)
  }

  # "/dev/shm/res/datasets/GpositiveGO"
  folderDatasetFolds = paste(folderDatasets, "/", dataset_name, sep="")
  if(dir.exists(folderDatasetFolds) == TRUE){
    setwd(folderDatasetFolds)
    dir_folderDatasetFolds = dir(folderDatasetFolds)
    n_folderDatasetFolds = length(dir_folderDatasetFolds)
  } else {
    dir.create(folderDatasetFolds)
    setwd(folderDatasetFolds)
    dir_folderDatasetFolds = dir(folderDatasetFolds)
    n_folderDatasetFolds = length(dir_folderDatasetFolds)
  }

  # "/dev/shm/res/BellPartitions"
  folderBellPart = paste(folderResults, "/BellPartitions", sep="")
  if(dir.exists(folderBellPart) == TRUE){
    setwd(folderBellPart)
    dirBellPart = dir(folderBellPart)
    n_BellPart = length(dirBellPart)
  } else {
    dir.create(folderBellPart)
    setwd(folderBellPart)
    dirBellPart = dir(folderBellPart)
    n_BellPart = length(dirBellPart)
  }

  # "/dev/shm/res/GpositiveGO"
  folderResultDataset = paste(folderResults, "/", dataset_name, sep="")
  if(dir.exists(folderResultDataset) == TRUE){
    setwd(folderResultDataset)
    dir_folderResultDataset = dir(folderResultDataset)
    n_folderResultDataset = length(dir_folderResultDataset)
  } else {
    dir.create(folderResultDataset)
    setwd(folderResultDataset)
    dir_folderResultDataset = dir(folderResultDataset)
    n_folderResultDataset = length(dir_folderResultDataset)
  }

  # "/dev/shm/res/datasets/GpositiveGO/CrossValidation/"
  folderCV = paste(folderDatasetFolds, "/CrossValidation/", sep="")
  if(dir.exists(folderCV) == TRUE){
    setwd(folderCV)
    dir_folderCV = dir(folderCV)
    n_folderCV = length(dir_folderCV)
  } else {
    dir.create(folderCV)
    setwd(folderCV)
    dir_folderCV = dir(folderCV)
    n_folderCV = length(dir_folderCV)
  }

  # "/dev/shm/res/datasets/GpositiveGO/CrossValidation/Tr"
  folderCVTR = paste(folderDatasetFolds, "/CrossValidation/Tr", sep="")
  if(dir.exists(folderCVTR) == TRUE){
    setwd(folderCVTR)
    dir_folderCVTR = dir(folderCVTR)
    n_folderCVTR = length(dir_folderCVTR)
  } else {
    dir.create(folderCVTR)
    setwd(folderCVTR)
    dir_folderCVTR = dir(folderCVTR)
    n_folderCVTR = length(dir_folderCVTR)
  }

  #  "/dev/shm/res/datasets/GpositiveGO/CrossValidation/Ts"
  folderCVTS = paste(folderDatasetFolds, "/CrossValidation/Ts", sep="")
  if(dir.exists(folderCVTS) == TRUE){
    setwd(folderCVTS)
    dir_folderCVTS = dir(folderCVTS)
    n_folderCVTS = length(dir_folderCVTS)
  } else {
    dir.create(folderCVTS)
    setwd(folderCVTS)
    dir_folderCVTS = dir(folderCVTS)
    n_folderCVTS = length(dir_folderCVTS)
  }

  # "/dev/shm/res/datasets/GpositiveGO/CrossValidation/Vl"
  folderCVVL = paste(folderDatasetFolds, "/CrossValidation/Vl", sep="")
  if(dir.exists(folderCVVL) == TRUE){
    setwd(folderCVVL)
    dir_folderCVVL = dir(folderCVVL)
    n_folderCVVL = length(dir_folderCVVL)
  } else {
    dir.create(folderCVVL)
    setwd(folderCVVL)
    dir_folderCVVL = dir(folderCVVL)
    n_folderCVVL = length(dir_folderCVVL)
  }

  #  "/dev/shm/res/datasets/NamesLabels"
  folderNamesLabels = paste(folderDatasetFolds, "/NamesLabels", sep="")
  if(dir.exists(folderNamesLabels) == TRUE){
    setwd(folderNamesLabels)
    dir_folderNamesLabels = dir(folderNamesLabels)
    n_folderNamesLabels = length(dir_folderNamesLabels)
  } else {
    dir.create(folderNamesLabels)
    setwd(folderNamesLabels)
    dir_folderNamesLabels = dir(folderNamesLabels)
    n_folderNamesLabels = length(dir_folderNamesLabels)
  }

  # "/dev/shm/res/datasets/LabelSpace"
  folderLabelSpace = paste(folderDatasetFolds, "/LabelSpace", sep="")
  if(dir.exists(folderLabelSpace) == TRUE){
    setwd(folderLabelSpace)
    dir_folderLabelSpace = dir(folderLabelSpace)
    n_folderLabelSpace = length(dir_folderLabelSpace)
  } else {
    dir.create(folderLabelSpace)
    setwd(folderLabelSpace)
    dir_folderLabelSpace = dir(folderLabelSpace)
    n_folderLabelSpace = length(dir_folderLabelSpace)
  }

  # "/dev/shm/res/Partition"
  folderPartition = paste(folderResults, "/Partition", sep="")
  if(dir.exists(folderPartition) == TRUE){
    setwd(folderPartition)
    dir_folderPartition = dir(folderPartition)
    n_folderPartition = length(dir_folderPartition)
  } else {
    dir.create(folderPartition)
    setwd(folderPartition)
    dir_folderPartition = dir(folderPartition)
    n_folderPartition = length(dir_folderPartition)
  }

  # "/dev/shm/res/Community"
  folderCommunity = paste(folderResults, "/Community", sep="")
  if(dir.exists(folderCommunity) == TRUE){
    setwd(folderCommunity)
    dir_folderCommunity = dir(folderCommunity)
    n_folderCommunity = length(dir_folderCommunity)
  } else {
    dir.create(folderCommunity)
    setwd(folderCommunity)
    dir_folderCommunity = dir(folderCommunity)
    n_folderCommunity = length(dir_folderCommunity)
  }


  # return folders
  retorno$folderResults = folderResults
  retorno$folderReports = folderReports
  retorno$folderReportsDataset = folderReportsDataset
  retorno$folderUtils = folderUtils
  retorno$folderDatasets = folderDatasets
  retorno$folderBellPart = folderBellPart
  retorno$folderDatasetFolds = folderDatasetFolds
  retorno$folderResultDataset = folderResultDataset
  retorno$folderLabelSpace = folderLabelSpace
  retorno$folderNamesLabels = folderNamesLabels
  retorno$folderPartition = folderPartition
  retorno$folderCommunity = folderCommunity
  retorno$folderCV = folderCV
  retorno$folderCVTR = folderCVTR
  retorno$folderCVTS = folderCVTS
  retorno$folderCVVL = folderCVVL


  # return of folder contents
  retorno$dirResults = dir_folderResults
  retorno$dirReports = dirReports
  retorno$dir_folderReportsDataset = dir_folderReportsDataset
  retorno$dirUtils = dirUtils
  retorno$dirDatasets = dirDatasets
  retorno$dirBellPartitions = dirBellPart
  retorno$dir_folderDatasetFolds = dir_folderDatasetFolds
  retorno$dir_folderResultDataset = dir_folderResultDataset
  retorno$dir_folderCV = dir_folderCV
  retorno$dir_folderCVTR = dir_folderCVTR
  retorno$dir_folderCVTS = dir_folderCVTS
  retorno$dir_folderCVVL = dir_folderCVVL


  # return of the number of objects inside the folder
  retorno$n_folderResults = n_folderResults
  retorno$n_Reports = n_Reports
  retorno$n_folderReportsDataset = n_folderReportsDataset
  retorno$n_Utils = n_Utils
  retorno$n_Datasets = n_Datasets
  retorno$n_BellPartitions = n_BellPart
  retorno$n_folderDatasetFolds = n_folderDatasetFolds
  retorno$n_folderResultDataset = n_folderResultDataset
  retorno$n_folderCV = n_folderCV
  retorno$n_folderCVTS = n_folderCVTR
  retorno$n_folderCVTS = n_folderCVTS
  retorno$n_folderCVVL = n_folderCVVL

  return(retorno)
  gc()
}


##################################################################################################
# FUNCTION CONVERT TO ARFF                                                                       #
#     Objective:                                                                                 #
#        Convert csv file correctly to arff file                                                 #
#     Parameters                                                                                 #
#        arg 1: existing csv file name                                                           #
#        arg 2: name of the arff file to be created                                              #
#        arg 3: specific number of labels that are part of the file. Example: starts at label    #
#        30 and ends at label 50.                                                                #
#     Return:                                                                                    #
#        The arff file in the specific folder                                                    #
##################################################################################################
converteArff <- function(arg1, arg2, arg3, FolderUtils){
  str = paste("java -jar ", FolderUtils, "/R_csv_2_arff.jar ", arg1, " ", arg2, " ", arg3, sep="")
  print(system(str))
  cat("\n\n")
}



##################################################################################################
# FUNCTION INFO DATA SET                                                                         #
#  Objective                                                                                     #
#     Gets the information that is in the "datasets.csv" file.                                    #
#  Parameters                                                                                    #
#     dataset: the specific dataset                                                              #
#  Return                                                                                        #
#     Everything in the spreadsheet                                                              #
##################################################################################################
infoDataSet <- function(dataset){
  retorno = list()
  retorno$id = dataset$ID
  retorno$name = dataset$Name
  retorno$domain = dataset$Domain
  retorno$instances = dataset$Instances
  retorno$attributes = dataset$Attributes
  retorno$inputs = dataset$Inputs
  retorno$labels = dataset$Labels
  retorno$LabelsSets = dataset$LabelsSets
  retorno$single = dataset$Single
  retorno$maxfreq = dataset$MaxFreq
  retorno$card = dataset$Card
  retorno$dens = dataset$Dens
  retorno$mean = dataset$MeanIR
  retorno$scumble = dataset$Scumble
  retorno$tcs = dataset$TCS
  retorno$attStart = dataset$AttStart
  retorno$attEnd = dataset$AttEnd
  retorno$labStart = dataset$LabelStart
  retorno$labEnd = dataset$LabelEnd
  return(retorno)
  gc()
}

infoPartitions <- function(id_part, dataset_name, folderResults){

  retorno = list()

  # setando o diretório específico
  diretorios = directories(dataset_name, folderResults)

  # obtendo a lista de partições
  nome = paste(diretorios$folderBellPart,
               "/", dataset_name, "-partitions.csv", sep="")
  bell = data.frame(read.csv(nome))

  # obtendo o total de grupos por cada partição
  nome2 = paste(diretorios$folderBellPart,
                "/", dataset_name, "-groupsPerPartitions.csv", sep="")
  groupsPerPartitions = data.frame(read.csv(nome2))

  # obtendo o formato da partição
  specificPartition = bell %>% filter(., bell$part == id_part)

  # obtendo o total de grupos da partição
  groupSecificPartition = groupsPerPartitions %>% filter(., groupsPerPartitions$part == id_part)

  # obtendo o número da partição
  numeroDaParticao = groupSecificPartition$part

  # obtendo o número de grupos da partição
  numeroDeGruposDaParticao = groupSecificPartition$totalGroups

  # criando a pasta específica da partição
  FolderPartition = paste(diretorios$folderResultDataset, "/Partition-", numeroDaParticao, sep="")
  if(dir.exists(FolderPartition)==FALSE){dir.create(FolderPartition)}

  #retorno$FolderBP = FolderBP
  retorno$bell = bell
  retorno$groupsPerPartitions = groupsPerPartitions
  retorno$specificPartition = specificPartition
  retorno$specificPartition = specificPartition
  retorno$numberOfPartition = numeroDaParticao
  retorno$numberGroupsOfPartition = numeroDeGruposDaParticao
  retorno$FolderPartition = FolderPartition

  return(retorno)

}


##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
