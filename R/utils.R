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

  #############################################################################
  # RESULTS FOLDER:                                                           #
  # Parameter from command line. This folder will be delete at the end of the #
  # execution. Other folder is used to store definitely the results.          #
  # Example: "/dev/shm/result"; "/scratch/result"; "/tmp/result"              #
  #############################################################################
  if(dir.exists(folderResults) == TRUE){
    setwd(folderResults)
    dir_folderResults = dir(folderResults)
    n_folderResults = length(dir_folderResults)
  } else {
    dir.create(folderResults)
    setwd(folderResults)
    dir_folderResults = dir(folderResults)
    n_folderResults = length(dir_folderResults)
  }

  #############################################################################

  #############################################################################
  folderReports = paste(FolderRoot, "/Reports", sep="")
  if(dir.exists(folderReports) == TRUE){
    setwd(folderReports)
    dir_folderReports = dir(folderReports)
    n_folderReports = length(dir_folderReports)
  } else {
    dir.create(folderReports)
    setwd(folderReports)
    dir_folderReports = dir(folderReports)
    n_folderReports = length(dir_folderReports)
  }

  #############################################################################

  #############################################################################
  folderReportsD = paste(folderReports, "/", dataset_name, sep="")
  if(dir.exists(folderReportsD) == TRUE){
    setwd(folderReportsD)
    dir_folderReportsD = dir(folderReportsD)
    n_folderReportsD = length(dir_folderReportsD)
  } else {
    dir.create(folderReportsD)
    setwd(folderReportsD)
    dir_folderReportsD = dir(folderReportsD)
    n_folderReportsD = length(dir_folderReportsD)
  }


  #############################################################################
  # TEST FOLDER: "/dev/shm/results/Test"                                      #
  #         Folder that will temporarily store the files and folders needed   #
  #     for code processing in the test phase                                 #
  #############################################################################
  folderTest = paste(folderResults, "/Test", sep="")
  if(dir.exists(folderTest) == TRUE){
    setwd(folderTest)
    dir_folderTest = dir(folderTest)
    n_folderTest = length(dir_folderTest)
  } else {
    dir.create(folderTest)
    setwd(folderTest)
    dir_folderTest = dir(folderTest)
    n_folderTest = length(dir_folderTest)
  }



  #############################################################################
  # TEST FOLDER: "/dev/shm/results/Test"                                      #
  #         Folder that will temporarily store the files and folders needed   #
  #     for code processing in the test phase                                 #
  #############################################################################
  folderTempResults = paste(folderResults, "/Results", sep="")
  if(dir.exists(folderTempResults) == TRUE){
    setwd(folderTempResults)
    dir_folderTempResults = dir(folderTempResults)
    n_folderTempResults = length(dir_folderTempResults)
  } else {
    dir.create(folderTempResults)
    setwd(folderTempResults)
    dir_folderTempResults = dir(folderTempResults)
    n_folderTempResults = length(dir_folderTempResults)
  }


  #############################################################################
  # BELL PARTITIONS FOLDER: "/dev/shm/results/BellPartitions"                 #
  #         Folder that will temporarily store the files that contain the     #
  #    information of all possible partitions                                 #                                           #
  #############################################################################
  folderBellPart = paste(folderResults, "/BellPartitions", sep="")
  if(dir.exists(folderBellPart) == TRUE){
    setwd(folderBellPart)
    dir_folderBellPart = dir(folderBellPart)
    n_folderBellPart = length(dir_folderBellPart)
  } else {
    dir.create(folderBellPart)
    setwd(folderBellPart)
    dir_folderBellPart = dir(folderBellPart)
    n_folderBellPart = length(dir_folderBellPart)
  }

  #############################################################################
  # BELL PARTITIONS FOLDER: "/dev/shm/results/BellPartitions/GpositiveGO"     #
  #         Folder that will temporarily store the files that contain the     #
  #    information of all possible partitions for the dataset                 #
  #############################################################################
  folderBellPartDataset = paste(folderBellPart, "/", dataset_name, sep="")
  if(dir.exists(folderBellPartDataset) == TRUE){
    setwd(folderBellPartDataset)
    dir_folderBellPartDataset = dir(folderBellPartDataset)
    n_folderBellPartDataset = length(dir_folderBellPartDataset)
  } else {
    dir.create(folderBellPartDataset)
    setwd(folderBellPartDataset)
    dir_folderBellPartDataset = dir(folderBellPartDataset)
    n_folderBellPartDataset = length(dir_folderBellPartDataset)
  }

  #############################################################################
  # DATASET FOLDER: "/dev/shm/results/Dataset"                                #
  #         Folder that will temporarily store the dataset files and folders  #
  #############################################################################
  folderDataset = paste(folderResults, "/Dataset", sep="")
  if(dir.exists(folderDataset) == TRUE){
    setwd(folderDataset)
    dir_folderDataset = dir(folderDataset)
    n_folderDataset = length(dir_folderDataset)
  } else {
    dir.create(folderDataset)
    setwd(folderDataset)
    dir_folderDataset = dir(folderDataset)
    n_folderDataset = length(dir_folderDataset)
  }


  #############################################################################
  # SPECIFIC DATASET FOLDER:                                                  #
  #         "/dev/shm/results/Dataset/GpositiveGO"                            #
  #         Folder that will temporarily store the dataset files and folders  #
  #############################################################################
  folderDatasetX = paste(folderDataset, "/", dataset_name, sep="")
  if(dir.exists(folderDatasetX) == TRUE){
    setwd(folderDatasetX)
    dir_folderDatasetX = dir(folderDatasetX)
    n_folderDatasetX = length(dir_folderDatasetX)
  } else {
    dir.create(folderDatasetX)
    setwd(folderDatasetX)
    dir_folderDatasetX = dir(folderDatasetX)
    n_folderDatasetX = length(dir_folderDatasetX)
  }

  #############################################################################
  # CROSS VALIDATION FOLDER:                                                  #
  #         "/dev/shm/results/Dataset/GpositiveGO/CrossValidation"            #
  #         Folder that will temporarily store the dataset files and folders  #
  #############################################################################
  folderCV = paste(folderDatasetX, "/CrossValidation", sep="")
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

  #############################################################################
  # CROSS VALIDATION TRAIN FILES/FOLDER:                                      #
  #         "/dev/shm/results/Dataset/GpositiveGO/CrossValidation/Tr"         #
  #         Folder that will temporarily store the dataset files and folders  #
  #############################################################################
  folderCVTR = paste(folderCV, "/Tr", sep="")
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

  #############################################################################
  # CROSS VALIDATION TEST FILES/FOLDER:                                       #
  #          "/dev/shm/results/Dataset/GpositiveGO/CrossValidation/Ts"        #
  #         Folder that will temporarily store the dataset files and folders  #
  #############################################################################
  folderCVTS = paste(folderCV, "/Ts", sep="")
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

  #############################################################################
  # CROSS VALIDATION VALIDATION FILES/FOLDER:                                 #
  #         "/dev/shm/results/Dataset/GpositiveGO/CrossValidation/Vl"         #
  #         Folder that will temporarily store the dataset files and folders  #
  #############################################################################
  folderCVVL = paste(folderCV, "/Vl", sep="")
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

  #############################################################################
  # CROSS VALIDATION LABEL SPACE FILES/FOLDER:                                #
  #         "/dev/shm/results/Dataset/GpositiveGO/LabelSpace"                 #
  #         Folder that will temporarily store the dataset files and folders  #
  #############################################################################
  folderLabelSpace = paste(folderDatasetX, "/LabelSpace", sep="")
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

  #############################################################################
  # CROSS VALIDATION LABELS NAMES FILES/FOLDER:                               #
  #        "/dev/shm/results/Dataset/GpositiveGO/NamesLabels"                 #
  #         Folder that will temporarily store the dataset files and folders  #
  #############################################################################
  folderNamesLabels = paste(folderDatasetX, "/NamesLabels", sep="")
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


  # return folders
  retorno$folderTempResults = folderTempResults
  retorno$folderResults = folderResults
  retorno$folderReports = folderReports
  retorno$folderReportsD = folderReportsD
  retorno$folderResults = folderResults
  retorno$folderTest = folderTest
  retorno$folderBellPart = folderBellPart
  retorno$folderBellPartDataset = folderBellPartDataset
  retorno$folderDataset = folderDataset
  retorno$folderDatasetX = folderDatasetX
  retorno$folderCV = folderCV
  retorno$folderCVTR = folderCVTR
  retorno$folderCVTS = folderCVTS
  retorno$folderCVVL = folderCVVL
  retorno$folderLabelSpace = folderLabelSpace
  retorno$folderNamesLabels = folderNamesLabels

  # return folder contents
  retorno$dir_folderResults = dir_folderResults
  retorno$dir_folderTempResults = dir_folderTempResults
  retorno$dir_folderReports = dir_folderReports
  retorno$dir_folderReportsD = dir_folderReportsD
  retorno$dir_folderResults = dir_folderResults
  retorno$dir_folderTest = dir_folderTest
  retorno$dir_folderBellPart = dir_folderBellPart
  retorno$dir_folderBellPartDataset = dir_folderBellPartDataset
  retorno$dir_folderDataset = dir_folderDataset
  retorno$dir_folderDatasetX = dir_folderDatasetX
  retorno$dir_folderCV = dir_folderCV
  retorno$dir_folderCVTR = dir_folderCVTR
  retorno$dir_folderCVTS = dir_folderCVTS
  retorno$dir_folderCVVL = dir_folderCVVL
  retorno$dir_folderLabelSpace = dir_folderLabelSpace
  retorno$dir_folderNamesLabels = dir_folderNamesLabels

  # return of the number of objects inside the folder
  retorno$n_folderResults = n_folderResults
  retorno$n_folderTempResults = n_folderTempResults
  retorno$n_folderReports = n_folderReports
  retorno$n_folderReportsD = n_folderReportsD
  retorno$n_folderResults = n_folderResults
  retorno$n_folderTest = n_folderTest
  retorno$n_folderBellPart = n_folderBellPart
  retorno$n_folderBellPartDataset = n_folderBellPartDataset
  retorno$n_folderDataset = n_folderDataset
  retorno$n_folderDatasetX = n_folderDatasetX
  retorno$n_folderCV = n_folderCV
  retorno$n_folderCVTR = n_folderCVTR
  retorno$n_folderCVTS = n_folderCVTS
  retorno$n_folderCVVL = n_folderCVVL
  retorno$n_folderLabelSpace = n_folderLabelSpace
  retorno$n_folderNamesLabels = n_folderNamesLabels

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
  nome = paste(diretorios$folderBellPartDataset,
               "/", dataset_name, "-partitions.csv", sep="")
  bell = data.frame(read.csv(nome))

  # obtendo o total de grupos por cada partição
  nome2 = paste(diretorios$folderBellPartDataset,
                "/", dataset_name, "-groupsPerPartitions.csv", sep="")
  groupsPerPartitions = data.frame(read.csv(nome2))

  # obtendo o formato da partição
  specificPartition = bell %>% filter(., bell$part == id_part)

  # obtendo o total de grupos da partição
  groupSecificPartition = groupsPerPartitions %>%
    filter(., groupsPerPartitions$part == id_part)

  # obtendo o número da partição
  numeroDaParticao = groupSecificPartition$part

  # obtendo o número de grupos da partição
  numeroDeGruposDaParticao = groupSecificPartition$totalGroups

  #retorno$FolderBP = FolderBP
  retorno$bell = bell
  retorno$groupsPerPartitions = groupsPerPartitions
  retorno$specificPartition = specificPartition
  retorno$specificPartition = specificPartition
  retorno$numberOfPartition = numeroDaParticao
  retorno$numberGroupsOfPartition = numeroDeGruposDaParticao

  return(retorno)

}


##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
