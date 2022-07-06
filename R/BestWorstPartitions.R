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
#
##################################################################################################
bestMeasuresPartitions <- function(dataset_name, number_folds, folderResults){

  retorno = list()

  diretorios = directories(dataset_name, folderResults)

  ##################################################################################################
  cat("\nGet the number of bell partitions")
  setwd(diretorios$folderBellPart)
  str = paste(dataset_name, "-groupsPerPartitions.csv", sep="")
  bell = data.frame(read.csv(str))
  n = nrow(bell)
  cat("\nNumber Partitions: ", n)

  measures2 = c("accuracy","average-precision","clp","coverage","F1","hamming-loss","macro-AUC",
               "macro-F1","macro-precision","macro-recall","margin-loss","micro-AUC","micro-F1",
               "micro-precision","micro-recall","mlp","one-error","precision","ranking-loss",
               "recall","subset-accuracy","wlp")
  n_m = length(measures2)


  ##################################################################################################
  # 2.º abrir o arquivo com o resultado FOLDS x MEDIDAS
  # C:\Users\elain\ExhaustiveSHM\results\emotions\Partition-2
  # Partition-2-Evaluated.csv

  x = 1
  while(x<=n_m){

    cat("\n\nMeasure: ", toString(measures2[x]))

    partition = c(0)
    measures = c("")
    Fold.1 = c(0)
    Fold.2 = c(0)
    Fold.3 = c(0)
    Fold.4 = c(0)
    Fold.5 = c(0)
    Fold.6 = c(0)
    Fold.7 = c(0)
    Fold.8 = c(0)
    Fold.9 = c(0)
    Fold.10 = c(0)
    all = data.frame(partition, measures, Fold.1, Fold.2, Fold.3,
                     Fold.4, Fold.5, Fold.6, Fold.7, Fold.8, Fold.9, Fold.10)

    cat("\n##########################################################")
    i = 2
    while(i<=n){
      cat("\nPartition ", i)
      setwd(diretorios$folderReportsDataset)
      nome = paste("Partition-", i, "-Evaluated.csv", sep="")
      arquivo = data.frame(read.csv(nome))
      result = arquivo[x,]
      partition = i
      allMeasure = cbind(partition, result)
      all = rbind(all, allMeasure)
      #unlink(nome)
      i = i + 1
      gc()
    } # fim partições

    cat("\n")

    all = all[-1,-2]

    #setwd(diretorios$folderResultDataset)
    #write.csv(all, paste(dataset_name, "-All-", measures[x], "-Oracle.csv", sep=""), row.names = FALSE)

    setwd(diretorios$folderReportsDataset)
    write.csv2(all, paste(dataset_name, "-All-", toString(measures2[x]), "-Oracle.csv", sep=""), row.names = FALSE)

    cat("\n##########################################################")
    cat("\n Depois escolher a partição com a melhor Macro-F1")
    cat("\n de P2 até PN, qual obteve a maior Macro-f1 no Fold-x?")

    fold = c(0)
    partition = c(0)
    value = c(0)
    result2 = data.frame(fold, partition, value)

    all[is.na(all)] <- 0

    j = 1
    while(j<=number_folds){

      cat("\nFold", j)

      a = j + 1
      value_max = as.numeric(max(all[,a]))
      index_max = which.max(all[,a])

      fold = j
      partition = index_max+1
      value = value_max
      result = data.frame(fold, partition, value)
      result2 = rbind(result2, result)

      j = j + 1
      gc()
    }

    cat("\n")

    #setwd(diretorios$folderResultDataset)
    #write.csv(result2[-1,], paste(dataset_name, "-Best-Macro-Partitions-Oracle.csv", sep=""), row.names = FALSE)

    setwd(diretorios$folderReportsDataset)
    write.csv2(result2[-1,], paste(dataset_name, "-Best-", toString(measures2[x]) ,"-Fold-Partitions-Oracle.csv", sep=""), row.names = FALSE)

    x = x + 1
    gc()
  } # fim medidas


  gc()
  cat("\n##################################################################################################")
  cat("\n# Best Partitions: END                                                                           #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")

}


###########################################################
bwPartition <- function(){

  diretorios = directories(dataset_name, folderResults)

  ################################################################################################
  partition = c(0)
  value = c(0)
  Accuracy2 = data.frame(partition, value)
  AveragePrecision2 = data.frame(partition, value)
  CLP2 = data.frame(partition, value)
  Coverage2 = data.frame(partition, value)
  F12 = data.frame(partition, value)
  HammingLoss2 = data.frame(partition, value)
  MacroAUC2 = data.frame(partition, value)
  MacroF12 = data.frame(partition, value)
  MacroPrecision2 = data.frame(partition, value)
  MacroRecall2 = data.frame(partition, value)
  MarginLoss2 = data.frame(partition, value)
  MicroAUC2 = data.frame(partition, value)
  MicroF12 = data.frame(partition, value)
  MicroPrecision2 = data.frame(partition, value)
  MicroRecall2 = data.frame(partition, value)
  MLP2 = data.frame(partition, value)
  OneError2 = data.frame(partition, value)
  Precision2 = data.frame(partition, value)
  RankingLoss2 = data.frame(partition, value)
  Recall2 = data.frame(partition, value)
  SubsetAccuracy2 =data.frame(partition, value)
  WLP2 = data.frame(partition, value)

  if(number_folds==1){
    cat("\nFold Único")

    p = 2
    while(p<=n){
      setwd(diretorios$folderReportsDataset)
      resultados = data.frame(read.csv(paste("Partition-",p,"-Evaluated.csv", sep="")))

      # partition
      partition = p

      # 1. ACCURACY
      Accuracy = data.frame(resultados2[1,])
      names(Accuracy) = "valores"
      Accuracy[is.na(Accuracy)] <- 0
      value = as.numeric(Accuracy$valores)
      Accuracy2 = rbind(Accuracy2, data.frame(partition, value))

      # 2. AVERAGE PRECISION
      AveragePrecision = data.frame(resultados2[2,])
      names(AveragePrecision) = "valores"
      AveragePrecision[is.na(AveragePrecision)] <- 0
      value = as.numeric(AveragePrecision$valores)
      AveragePrecision2 = rbind(AveragePrecision2, data.frame(partition, value))

      # 3. CLP
      CLP = data.frame(resultados2[3,])
      names(CLP) = "valores"
      CLP[is.na(CLP)] <- 0
      value = as.numeric(CLP$valores)
      CLP2 = rbind(CLP2, data.frame(partition, value))

      # 4. COVERAGE
      Coverage = data.frame(resultados2[4,])
      names(Coverage) = "valores"
      Coverage[is.na(Coverage)] <- 0
      value = as.numeric(Coverage$valores)
      Coverage2 = rbind(Coverage2, data.frame(partition, value))

      # 5. F1
      F1 = data.frame(resultados2[5,])
      names(F1) = "valores"
      F1[is.na(F1)] <- 0
      value = as.numeric(F1$valores)
      F12 = rbind(F12, data.frame(partition, value))

      # 6. HAMMING LOSS
      HammingLoss = data.frame(resultados2[6,])
      names(HammingLoss) = "valores"
      HammingLoss[is.na(HammingLoss)] <- 0
      value = as.numeric(HammingLoss$valores)
      HammingLoss2 = rbind(HammingLoss2, data.frame(partition, value))

      # 7. MACRO AUC
      MacroAUC = data.frame(resultados2[7,])
      names(MacroAUC) = "valores"
      MacroAUC[is.na(MacroAUC)] <- 0
      value = as.numeric(MacroAUC$valores)
      MacroAUC2 = rbind(MacroAUC2, data.frame(partition, value))

      # 8. MACRO F1
      MacroF1 = data.frame(resultados2[8,])
      names(MacroF1) = "valores"
      MacroF1[is.na(MacroF1)] <- 0
      value = as.numeric(MacroF1$valores)
      MacroF12 = rbind(MacroF12, data.frame(partition, value))

      # 9. MACRO PRECISION
      MacroPrecision = data.frame(resultados2[9,])
      names(MacroPrecision) = "valores"
      MacroPrecision[is.na(MacroPrecision)] <- 0
      value = as.numeric(MacroPrecision$valores)
      MacroPrecision2 = rbind(MacroPrecision2, data.frame(partition, value))

      # 10. MACRO RECALL
      MacroRecall = data.frame(resultados2[10,])
      names(MacroRecall) = "valores"
      MacroRecall[is.na(MacroRecall)] <- 0
      value = as.numeric(MacroRecall$valores)
      MacroRecall2 = rbind(MacroRecall2, data.frame(partition, value))

      # 11. MARGIN LOSS
      MarginLoss = data.frame(resultados2[11,])
      names(MarginLoss) = "valores"
      MarginLoss[is.na(MarginLoss)] <- 0
      value = as.numeric(MarginLoss$valores)
      MarginLoss2 = rbind(MarginLoss2, data.frame(partition, value))

      # 12. MICRO AUC
      MicroAUC = data.frame(resultados2[12,])
      names(MicroAUC) = "valores"
      MicroAUC[is.na(MicroAUC)] <- 0
      value = as.numeric(MicroAUC$valores)
      MicroAUC2 = rbind(MicroAUC2, data.frame(partition, value))

      # 13. MICRO F1
      MicroF1 = data.frame(resultados2[13,])
      names(MicroF1) = "valores"
      MicroF1[is.na(MicroF1)] <- 0
      value = as.numeric(MicroF1$valores)
      MicroF12 = rbind(MicroF12, data.frame(partition, value))

      # 14. MICRO PRECISION
      MicroPrecision = data.frame(resultados2[14,])
      names(MicroPrecision) = "valores"
      MicroPrecision[is.na(MicroPrecision)] <- 0
      value = as.numeric(MicroPrecision$valores)
      MicroPrecision2 = rbind(MicroPrecision2, data.frame(partition, value))

      # 15. MICRO RECALL
      MicroRecall = data.frame(resultados2[15,])
      names(MicroRecall) = "valores"
      MicroRecall[is.na(MicroRecall)] <- 0
      value = as.numeric(MicroRecall$valores)
      MicroRecall2 = rbind(MicroRecall2, data.frame(partition, value))

      # 16. MLP
      MLP = data.frame(resultados2[16,])
      names(MLP) = "valores"
      MLP[is.na(MLP)] <- 0
      value = as.numeric(MLP$valores)
      MLP2 = rbind(MLP2, data.frame(partition, value))

      # 17. ONE ERROR
      OneError = data.frame(resultados2[17,])
      names(OneError) = "valores"
      OneError[is.na(OneError)] <- 0
      value = as.numeric(OneError$valores)
      OneError2 = rbind(OneError2, data.frame(partition, value))

      # 18. PRECISION
      Precision = data.frame(resultados2[18,])
      names(Precision) = "valores"
      Precision[is.na(Precision)] <- 0
      value = as.numeric(Precision$valores)
      Precision2 = rbind(Precision2, data.frame(partition, value))

      # 19. RANKING LOSS
      RankingLoss = data.frame(resultados2[19,])
      names(RankingLoss) = "valores"
      RankingLoss[is.na(RankingLoss)] <- 0
      value = as.numeric(RankingLoss$valores)
      RankingLoss2 = rbind(RankingLoss2, data.frame(partition, value))

      # 20. RECALL
      Recall = data.frame(resultados2[20,])
      names(Recall) = "valores"
      Recall[is.na(Recall)] <- 0
      value = as.numeric(Recall$valores)
      Recall2 = rbind(Recall2, data.frame(partition, value))

      # 21. SUBSET ACCURACY
      SubsetAccuracy = data.frame(resultados2[21,])
      names(SubsetAccuracy) = "valores"
      SubsetAccuracy[is.na(SubsetAccuracy)] <- 0
      value = as.numeric(SubsetAccuracy$valores)
      SubsetAccuracy2 = rbind(SubsetAccuracy2, data.frame(partition, value))

      # 22. WLP
      WLP = data.frame(resultados2[22,])
      names(WLP) = "valores"
      WLP[is.na(WLP)] <- 0
      value = as.numeric(WLP$valores)
      WLP2 = rbind(WLP2, data.frame(partition, value))

      p = p + 1
      gc()
    }

  } else {

    cat("\n MAIS DE UM FOLD")

    p = 2
    while(p<=n){

      cat("\n\nPartition: ", p)

      setwd(diretorios$folderReportsDataset)
      resultados = data.frame(read.csv(paste("Partition-",p,"-Evaluated.csv", sep="")))
      resultados1 = resultados[,-1]
      resultados2 = data.frame(apply(resultados1,1,mean))
      names(resultados2) = "average"

      # partição
      partition = p

      # 1. ACCURACY
      Accuracy = data.frame(resultados2[1,])
      names(Accuracy) = "valores"
      Accuracy[is.na(Accuracy)] <- 0
      value = as.numeric(Accuracy$valores)
      Accuracy2 = rbind(Accuracy2, data.frame(partition, value))

      # 2. AVERAGE PRECISION
      AveragePrecision = data.frame(resultados2[2,])
      names(AveragePrecision) = "valores"
      AveragePrecision[is.na(AveragePrecision)] <- 0
      value = as.numeric(AveragePrecision$valores)
      AveragePrecision2 = rbind(AveragePrecision2, data.frame(partition, value))

      # 3. CLP
      CLP = data.frame(resultados2[3,])
      names(CLP) = "valores"
      CLP[is.na(CLP)] <- 0
      value = as.numeric(CLP$valores)
      CLP2 = rbind(CLP2, data.frame(partition, value))

      # 4. COVERAGE
      Coverage = data.frame(resultados2[4,])
      names(Coverage) = "valores"
      Coverage[is.na(Coverage)] <- 0
      value = as.numeric(Coverage$valores)
      Coverage2 = rbind(Coverage2, data.frame(partition, value))

      # 5. F1
      F1 = data.frame(resultados2[5,])
      names(F1) = "valores"
      F1[is.na(F1)] <- 0
      value = as.numeric(F1$valores)
      F12 = rbind(F12, data.frame(partition, value))

      # 6. HAMMING LOSS
      HammingLoss = data.frame(resultados2[6,])
      names(HammingLoss) = "valores"
      HammingLoss[is.na(HammingLoss)] <- 0
      value = as.numeric(HammingLoss$valores)
      HammingLoss2 = rbind(HammingLoss2, data.frame(partition, value))

      # 7. MACRO AUC
      MacroAUC = data.frame(resultados2[7,])
      names(MacroAUC) = "valores"
      MacroAUC[is.na(MacroAUC)] <- 0
      value = as.numeric(MacroAUC$valores)
      MacroAUC2 = rbind(MacroAUC2, data.frame(partition, value))

      # 8. MACRO F1
      MacroF1 = data.frame(resultados2[8,])
      names(MacroF1) = "valores"
      MacroF1[is.na(MacroF1)] <- 0
      value = as.numeric(MacroF1$valores)
      MacroF12 = rbind(MacroF12, data.frame(partition, value))

      # 9. MACRO PRECISION
      MacroPrecision = data.frame(resultados2[9,])
      names(MacroPrecision) = "valores"
      MacroPrecision[is.na(MacroPrecision)] <- 0
      value = as.numeric(MacroPrecision$valores)
      MacroPrecision2 = rbind(MacroPrecision2, data.frame(partition, value))

      # 10. MACRO RECALL
      MacroRecall = data.frame(resultados2[10,])
      names(MacroRecall) = "valores"
      MacroRecall[is.na(MacroRecall)] <- 0
      value = as.numeric(MacroRecall$valores)
      MacroRecall2 = rbind(MacroRecall2, data.frame(partition, value))

      # 11. MARGIN LOSS
      MarginLoss = data.frame(resultados2[11,])
      names(MarginLoss) = "valores"
      MarginLoss[is.na(MarginLoss)] <- 0
      value = as.numeric(MarginLoss$valores)
      MarginLoss2 = rbind(MarginLoss2, data.frame(partition, value))

      # 12. MICRO AUC
      MicroAUC = data.frame(resultados2[12,])
      names(MicroAUC) = "valores"
      MicroAUC[is.na(MicroAUC)] <- 0
      value = as.numeric(MicroAUC$valores)
      MicroAUC2 = rbind(MicroAUC2, data.frame(partition, value))

      # 13. MICRO F1
      MicroF1 = data.frame(resultados2[13,])
      names(MicroF1) = "valores"
      MicroF1[is.na(MicroF1)] <- 0
      value = as.numeric(MicroF1$valores)
      MicroF12 = rbind(MicroF12, data.frame(partition, value))

      # 14. MICRO PRECISION
      MicroPrecision = data.frame(resultados2[14,])
      names(MicroPrecision) = "valores"
      MicroPrecision[is.na(MicroPrecision)] <- 0
      value = as.numeric(MicroPrecision$valores)
      MicroPrecision2 = rbind(MicroPrecision2, data.frame(partition, value))

      # 15. MICRO RECALL
      MicroRecall = data.frame(resultados2[15,])
      names(MicroRecall) = "valores"
      MicroRecall[is.na(MicroRecall)] <- 0
      value = as.numeric(MicroRecall$valores)
      MicroRecall2 = rbind(MicroRecall2, data.frame(partition, value))

      # 16. MLP
      MLP = data.frame(resultados2[16,])
      names(MLP) = "valores"
      MLP[is.na(MLP)] <- 0
      value = as.numeric(MLP$valores)
      MLP2 = rbind(MLP2, data.frame(partition, value))

      # 17. ONE ERROR
      OneError = data.frame(resultados2[17,])
      names(OneError) = "valores"
      OneError[is.na(OneError)] <- 0
      value = as.numeric(OneError$valores)
      OneError2 = rbind(OneError2, data.frame(partition, value))

      # 18. PRECISION
      Precision = data.frame(resultados2[18,])
      names(Precision) = "valores"
      Precision[is.na(Precision)] <- 0
      value = as.numeric(Precision$valores)
      Precision2 = rbind(Precision2, data.frame(partition, value))

      # 19. RANKING LOSS
      RankingLoss = data.frame(resultados2[19,])
      names(RankingLoss) = "valores"
      RankingLoss[is.na(RankingLoss)] <- 0
      value = as.numeric(RankingLoss$valores)
      RankingLoss2 = rbind(RankingLoss2, data.frame(partition, value))

      # 20. RECALL
      Recall = data.frame(resultados2[20,])
      names(Recall) = "valores"
      Recall[is.na(Recall)] <- 0
      value = as.numeric(Recall$valores)
      Recall2 = rbind(Recall2, data.frame(partition, value))

      # 21. SUBSET ACCURACY
      SubsetAccuracy = data.frame(resultados2[21,])
      names(SubsetAccuracy) = "valores"
      SubsetAccuracy[is.na(SubsetAccuracy)] <- 0
      value = as.numeric(SubsetAccuracy$valores)
      SubsetAccuracy2 = rbind(SubsetAccuracy2, data.frame(partition, value))

      # 22. WLP
      WLP = data.frame(resultados2[22,])
      names(WLP) = "valores"
      WLP[is.na(WLP)] <- 0
      value = as.numeric(WLP$valores)
      WLP2 = rbind(WLP2, data.frame(partition, value))

      p = p + 1
      gc()
    }

  }

  #########################################################################
  # 1. ACCURACY
  Accuracy = Accuracy2[-1,]
  value_max = as.numeric(max(Accuracy[,2]))
  accuracy_max_index = which.max(Accuracy[,2])
  partition = Accuracy[accuracy_max_index,]
  partition = as.numeric(partition$partition)
  BestAccuracy = data.frame(partition, value_max)

  value_min = as.numeric(min(Accuracy[,2]))
  accuracy_min_index = which.min(Accuracy[,2])
  partition = Accuracy[accuracy_min_index,]
  partition = as.numeric(partition$partition)
  WorstAccuracy = data.frame(partition, value_min)

  #########################################################################
  # 2. AVERAGE PRECISION
  AveragePrecision = AveragePrecision2[-1,]
  value_max = as.numeric(max(AveragePrecision[,2]))
  average_precision_max_index = which.max(AveragePrecision[,2])
  partition = AveragePrecision[average_precision_max_index,]
  partition = as.numeric(partition$partition)
  BestAveragePrecision = data.frame(partition, value_max)

  value_min = as.numeric(min(AveragePrecision[,2]))
  average_precision_min_index = which.min(AveragePrecision[,2])
  partition = AveragePrecision[average_precision_min_index,]
  partition = as.numeric(partition$partition)
  WorstAveragePrecision = data.frame(partition, value_min)

  #########################################################################
  # 3. CLP
  CLP = CLP2[-1,]
  value_min = as.numeric(max(CLP[,2]))
  clp_max_index = which.max(CLP[,2])
  partition = CLP[clp_max_index,]
  partition = as.numeric(partition$partition)
  WorstClp = data.frame(partition, value_min)

  value_max = as.numeric(min(CLP[,2]))
  clp_min_index = which.min(CLP[,2])
  partition = CLP[clp_min_index,]
  partition = as.numeric(partition$partition)
  BestClp = data.frame(partition, value_max)

  #########################################################################
  # 4. COVERAGE
  Coverage = Coverage2[-1,]
  value_min = as.numeric(max(Coverage[,2]))
  coverage_max_index = which.max(Coverage[,2])
  partition = Coverage[coverage_max_index,]
  partition = as.numeric(partition$partition)
  WorstCoverage = data.frame(partition, value_min)

  value_max = as.numeric(min(Coverage[,2]))
  coverage_min_index = which.min(Coverage[,2])
  partition = Coverage[coverage_min_index,]
  partition = as.numeric(partition$partition)
  BestCoverage = data.frame(partition, value_max)

  #########################################################################
  # 5. F1
  F1 = F12[-1,]
  value_max = as.numeric(max(F1[,2]))
  f1_max_index = which.max(F1[,2])
  partition = F1[f1_max_index,]
  partition = as.numeric(partition$partition)
  BestF1 = data.frame(partition, value_max)

  value_min = as.numeric(min(F1[,2]))
  f1_min_index = which.min(F1[,2])
  partition = F1[f1_min_index,]
  partition = as.numeric(partition$partition)
  WorstF1 = data.frame(partition, value_min)

  #########################################################################
  # 6. HAMMING LOSS
  HammingLoss = HammingLoss2[-1,]
  value_min = as.numeric(max(HammingLoss[,2]))
  hamming_loss_max_index = which.max(HammingLoss[,2])
  partition = HammingLoss[hamming_loss_max_index,]
  partition = as.numeric(partition$partition)
  WorstHammingLoss = data.frame(partition, value_min)

  value_max = as.numeric(min(HammingLoss[,2]))
  hamming_loss_min_index = which.min(HammingLoss[,2])
  partition = HammingLoss[hamming_loss_min_index,]
  partition = as.numeric(partition$partition)
  BestHammingLoss = data.frame(partition, value_max)

  #########################################################################
  # 7. MACRO AUC
  is.na(MacroAUC2)
  MacroAUC = MacroAUC2[-1,]
  value_max = as.numeric(max(MacroAUC[,2]))
  macro_auc_max_index = which.max(MacroAUC[,2])
  partition = MacroAUC[macro_auc_max_index,]
  partition = as.numeric(partition$partition)
  BestMacroAUC= data.frame(partition, value_max)

  value_min = as.numeric(min(MacroAUC[,2]))
  macro_auc_min_index = which.min(MacroAUC[,2])
  partition = MacroAUC[macro_auc_min_index,]
  partition = as.numeric(partition$partition)
  WorstMacroAUC = data.frame(partition, value_min)

  #########################################################################
  # 8. MACRO F1
  MacroF1 = MacroF12[-1,]
  value_max = as.numeric(max(MacroF1[,2]))
  macro_f1_max_index = which.max(MacroF1[,2])
  partition = MacroF1[macro_f1_max_index,]
  partition = as.numeric(partition$partition)
  BestMacroF1 = data.frame(partition, value_max)

  value_min = as.numeric(min(MacroF1[,2]))
  macro_f1_min_index = which.min(MacroF1[,2])
  partition = MacroF1[macro_f1_min_index,]
  partition = as.numeric(partition$partition)
  WorstMacroF1 = data.frame(partition, value_min)

  #########################################################################
  # 9. MACRO PRECISION
  MacroPrecision = MacroPrecision2[-1,]
  value_max = as.numeric(max(MacroPrecision[,2]))
  macro_precision_max_index = which.max(MacroPrecision[,2])
  partition = MacroPrecision[macro_precision_max_index,]
  partition = as.numeric(partition$partition)
  BestMacroPrecision = data.frame(partition, value_max)

  value_min = as.numeric(min(MacroPrecision[,2]))
  macro_precision_min_index = which.min(MacroPrecision[,2])
  partition = MacroPrecision[macro_precision_min_index,]
  partition = as.numeric(partition$partition)
  WorstMacroPrecision = data.frame(partition, value_min)

  #########################################################################
  # 10. MACRO RECALL
  MacroRecall = MacroRecall2[-1,]
  value_max = as.numeric(max(MacroRecall[,2]))
  macro_recall_max_index = which.max(MacroRecall[,2])
  partition = MacroRecall[macro_recall_max_index,]
  partition = as.numeric(partition$partition)
  BestMacroRecall = data.frame(partition, value_max)

  value_min = as.numeric(min(MacroRecall[,2]))
  macro_recall_min_index = which.min(MacroRecall[,2])
  partition = MacroRecall[macro_recall_min_index,]
  partition = as.numeric(partition$partition)
  WorstMacroRecall = data.frame(partition, value_min)

  #########################################################################
  # 11. MARGIN LOSS
  MarginLoss = MarginLoss2[-1,]
  value_min = as.numeric(max(MarginLoss[,2]))
  margin_loss_max_index = which.max(MarginLoss[,2])
  partition = MarginLoss[margin_loss_max_index,]
  partition = as.numeric(partition$partition)
  WorstMarginLoss = data.frame(partition, value_min)

  value_max = as.numeric(min(MarginLoss[,2]))
  margin_loss_min_index = which.min(MarginLoss[,2])
  partition = MarginLoss[margin_loss_min_index,]
  partition = as.numeric(partition$partition)
  BestMarginLoss = data.frame(partition, value_max)


  #########################################################################
  # 7. MICRO AUC
  MicroAUC = MicroAUC2[-1,]
  value_max = as.numeric(max(MicroAUC[,2]))
  micro_auc_max_index = which.max(MicroAUC[,2])
  partition = MicroAUC[micro_auc_max_index,]
  partition = as.numeric(partition$partition)
  BestMicroAUC = data.frame(partition, value_max)

  value_min = as.numeric(min(MicroAUC[,2]))
  micro_auc_min_index = which.min(MicroAUC[,2])
  partition = MicroAUC[micro_auc_min_index,]
  partition = as.numeric(partition$partition)
  WorstMicroAUC = data.frame(partition, value_min)

  #########################################################################
  # 8. MACRO F1
  MicroF1 = MicroF12[-1,]
  value_max = as.numeric(max(MicroF1[,2]))
  micro_f1_max_index = which.max(MicroF1[,2])
  partition = MicroF1[micro_f1_max_index,]
  partition = as.numeric(partition$partition)
  BestMicroF1 = data.frame(partition, value_max)

  value_min = as.numeric(min(MicroF1[,2]))
  micro_f1_min_index = which.min(MacroF1[,2])
  partition = MicroF1[micro_f1_min_index,]
  partition = as.numeric(partition$partition)
  WorstMicroF1 = data.frame(partition, value_min)

  #########################################################################
  # 9. MACRO PRECISION
  MicroPrecision = MicroPrecision2[-1,]
  value_max = as.numeric(max(MicroPrecision[,2]))
  micro_precision_max_index = which.max(MicroPrecision[,2])
  partition = MicroPrecision[micro_precision_max_index,]
  partition = as.numeric(partition$partition)
  BestMicroPrecision = data.frame(partition, value_max)

  value_min = as.numeric(min(MicroPrecision[,2]))
  micro_precision_min_index = which.min(MicroPrecision[,2])
  partition = MicroPrecision[micro_precision_min_index,]
  partition = as.numeric(partition$partition)
  WorstMicroPrecision = data.frame(partition, value_min)

  #########################################################################
  # 10. MACRO RECALL
  MicroRecall = MicroRecall2[-1,]
  value_max = as.numeric(max(MicroRecall[,2]))
  micro_recall_max_index = which.max(MicroRecall[,2])
  partition = MicroRecall[micro_recall_max_index,]
  partition = as.numeric(partition$partition)
  BestMicroRecall = data.frame(partition, value_max)

  value_min = as.numeric(min(MicroRecall[,2]))
  micro_recall_min_index = which.min(MicroRecall[,2])
  partition = MicroRecall[micro_recall_min_index,]
  partition = as.numeric(partition$partition)
  WorstMicroRecall = data.frame(partition, value_min)

  #########################################################################
  # 16. MLP
  MLP = MLP2[-1,]
  value_min = as.numeric(max(MLP[,2]))
  mlp_max_index = which.max(MLP[,2])
  partition = MLP[mlp_max_index,]
  partition = as.numeric(partition$partition)
  WorstMlp = data.frame(partition, value_min)

  value_max = as.numeric(min(MLP[,2]))
  mlp_min_index = which.min(MLP[,2])
  partition = MLP[mlp_min_index,]
  partition = as.numeric(partition$partition)
  BestMlp = data.frame(partition, value_max)

  #########################################################################
  # 17. ONE ERROR
  OneError = OneError2[-1,]
  value_min = as.numeric(max(OneError[,2]))
  one_error_max_index = which.max(OneError[,2])
  partition = OneError[one_error_max_index,]
  partition = as.numeric(partition$partition)
  WorstOneError = data.frame(partition, value_min)

  value_max = as.numeric(min(OneError[,2]))
  one_error_min_index = which.min(OneError[,2])
  partition = OneError[one_error_min_index,]
  partition = as.numeric(partition$partition)
  BestOneError = data.frame(partition, value_max)

  #########################################################################
  # 18. PRECISION
  Precision = Precision2[-1,]
  value_max = as.numeric(max(Precision[,2]))
  precision_max_index = which.max(Precision[,2])
  partition = Precision[precision_max_index,]
  partition = as.numeric(partition$partition)
  BestPrecision = data.frame(partition, value_max)

  value_min = as.numeric(min(Precision[,2]))
  precision_min_index = which.min(Precision[,2])
  partition = Precision[precision_min_index,]
  partition = as.numeric(partition$partition)
  WorstPrecision = data.frame(partition, value_min)

  #########################################################################
  # 19. RANKING LOSS
  RankingLoss = RankingLoss2[-1,]
  value_min = as.numeric(max(RankingLoss[,2]))
  ranking_loss_max_index = which.max(RankingLoss[,2])
  partition = RankingLoss[ranking_loss_max_index,]
  partition = as.numeric(partition$partition)
  WorstRankingLoss = data.frame(partition, value_min)

  value_max = as.numeric(min(RankingLoss[,2]))
  ranking_loss_min_index = which.min(RankingLoss[,2])
  partition = RankingLoss[ranking_loss_min_index,]
  partition = as.numeric(partition$partition)
  BestRankingLoss = data.frame(partition, value_max)

  #########################################################################
  # 20. RECALL
  Recall = Recall2[-1,]
  value_max = as.numeric(max(Recall[,2]))
  recall_max_index = which.max(Recall[,2])
  partition = Recall[recall_max_index,]
  partition = as.numeric(partition$partition)
  BestRecall = data.frame(partition, value_max)

  value_min = as.numeric(min(Recall[,2]))
  recall_min_index = which.min(Recall[,2])
  partition = Recall[recall_min_index,]
  partition = as.numeric(partition$partition)
  WorstRecall = data.frame(partition, value_min)

  #########################################################################
  # 21. SUBSET ACCURACY
  SubsetAccuracy = SubsetAccuracy2[-1,]
  value_max = as.numeric(max(SubsetAccuracy[,2]))
  subset_accuracy_max_index = which.max(SubsetAccuracy[,2])
  partition = SubsetAccuracy[subset_accuracy_max_index,]
  partition = as.numeric(partition$partition)
  BestSubsetAccuracy = data.frame(partition, value_max)

  value_min = as.numeric(min(SubsetAccuracy[,2]))
  subset_accuracy_min_index = which.min(SubsetAccuracy[,2])
  partition = SubsetAccuracy[subset_accuracy_min_index,]
  partition = as.numeric(partition$partition)
  WorstSubsetAccuracy = data.frame(partition, value_min)

  #########################################################################
  # 22. WLP
  WLP = WLP2[-1,]
  value_min = as.numeric(max(WLP[,2]))
  wlp_max_index = which.max(WLP[,2])
  partition = WLP[wlp_max_index,]
  partition = as.numeric(partition$partition)
  WorstWlp = data.frame(partition, value_min)

  value_max = as.numeric(min(WLP[,2]))
  wlp_min_index = which.min(WLP[,2])
  partition = WLP[wlp_min_index,]
  partition = as.numeric(partition$partition)
  BestWlp = data.frame(partition, value_max)


  ###################################################################################################################
  BestResults = rbind(BestAccuracy, BestAveragePrecision, BestClp, BestCoverage,
                      BestF1, BestHammingLoss, BestMacroAUC, BestMacroF1, BestMacroPrecision,
                      BestMacroRecall, BestMarginLoss, BestMicroAUC, BestMicroF1,
                      BestMicroPrecision, BestMicroRecall, BestMlp, BestOneError, BestPrecision,
                      BestRankingLoss, BestRecall, BestSubsetAccuracy, BestWlp)

  #########################################################################################################################
  WorstResults = rbind(WorstAccuracy, WorstAveragePrecision, WorstClp, WorstCoverage,
                       WorstF1, WorstHammingLoss, WorstMacroAUC, WorstMacroF1, WorstMacroPrecision,
                       WorstMacroRecall, WorstMarginLoss, WorstMicroAUC, WorstMicroF1,
                       WorstMicroPrecision, WorstMicroRecall, WorstMlp, WorstOneError,
                       WorstPrecision, WorstRankingLoss, WorstRecall, WorstSubsetAccuracy, WorstWlp)


  #########################################################################################################################

  medidas = c(resultados$measures)

  br = cbind(medidas, BestResults)
  names(br) = c("measure", "partition", "value")

  wr = cbind(medidas, WorstResults)
  names(wr) = c("measure", "partition", "value")

  #cat("Save Best and Worst Partitions")
  setwd(diretorios$folderReportsDataset)
  write.csv2(br, paste(dataset_name, "-Results-Best.csv", sep=""))
  write.csv2(wr, paste(dataset_name, "-Results-Worst.csv", sep=""))

  library(dplyr)
  frequencia1 = count(br, vars= "partition")
  names(frequencia1) = c("partition", "frequency")

  frequencia2 = count(wr, vars = "partition")
  names(frequencia2) = c("partition", "frequency")

  setwd(diretorios$folderReportsDataset)
  write.csv2(frequencia1, paste(dataset_name, "-frequency-results-best.csv", sep=""))
  write.csv2(frequencia2, paste(dataset_name, "-frequency-results-worst.csv", sep=""))

}





##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
