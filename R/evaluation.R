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
# Script 4 - Evaluation                                                                          #
##################################################################################################


##################################################################################################
# Configures the workspace according to the operating system                                     #
##################################################################################################
FolderRoot = "~/Oracle-ECC"
FolderScripts = paste(FolderRoot, "/R", sep="")



##################################################################################################
# FUNCTION GATHER PREDICTS HYBRID PARTITIONS                                                     #
#   Objective                                                                                    #
#      From the file "test.pred.arff", separates the real labels and the predicted labels to     #
#      generate the confusion matrix to evaluate the partition.                                  #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       FolderHybrid: path of hybrid partition results                                           #
#   Return                                                                                       #
#       true labels and predicts labels                                                          #
##################################################################################################
gather <-
  function(id_part,
           ds,
           dataset_name,
           number_folds,
           namesLabels,
           folderResults) {
    diretorios = directories(dataset_name, folderResults)
    info <- infoPartitions(id_part, dataset_name, folderResults)

    if (number_folds == 1) {
      cat("\nUm único fold")

      # data frame
      apagar = c(0)
      y_true = data.frame(apagar)
      y_pred = data.frame(apagar)

      FolderSplit = paste(
        diretorios$folderResultDataset,
        "/Partition-",
        info$numberOfPartition,
        "/Split-1",
        sep = ""
      )

      g = 1
      while (g <= info$numberGroupsOfPartition) {
        cat("\n\tGroup: ", g)

        FolderGroup = paste(FolderSplit, "/Group-", g, sep = "")

        #cat("\nGather y_true ", g, "\n")
        setwd(FolderGroup)
        y_true_gr = data.frame(read.csv("y_true.csv"))
        y_true = cbind(y_true, y_true_gr)
        #print(nrow(y_true))

        #cat("\nGather y_predict ", g, "\n")
        y_pred_gr = data.frame(read.csv("y_predict.csv"))
        y_pred = cbind(y_pred, y_pred_gr)
        #print(nrow(y_pred))

        # deleting files
        unlink("y_true.csv", recursive = TRUE)
        unlink("y_predict.csv", recursive = TRUE)
        unlink("inicioFimRotulos.csv", recursive = TRUE)

        g = g + 1
        gc()
      }

      #cat("\nSave files ", g, "\n")
      FolderSplit = paste(
        diretorios$folderResultDataset,
        "/Partition-",
        info$numberOfPartition,
        "/Split-1",
        sep = ""
      )
      setwd(FolderSplit)
      y_pred = y_pred[, -1]
      y_true = y_true[, -1]
      write.csv(y_pred, "y_predict.csv", row.names = FALSE)
      write.csv(y_true, "y_true.csv", row.names = FALSE)

      gc()

    } else {
      # start build partitions
      # do fold 1 até o último fold
      f = 1
      gatherParal <- foreach(f = 1:number_folds) %dopar% {
        # data frame
        apagar = c(0)
        y_true = data.frame(apagar)
        y_pred = data.frame(apagar)

        cat("\nFold: ", f)

        FolderSplit = paste(
          diretorios$folderResultDataset,
          "/Partition-",
          info$numberOfPartition,
          "/Split-",
          f,
          sep = ""
        )

        g = 1
        while (g <= info$numberGroupsOfPartition) {
          cat("\n\tGroup: ", g)

          FolderGroup = paste(FolderSplit, "/Group-", g, sep = "")

          #cat("\nGather y_true ", g, "\n")
          setwd(FolderGroup)
          y_true_gr = data.frame(read.csv("y_true.csv"))
          y_true = cbind(y_true, y_true_gr)
          #print(nrow(y_true))

          #cat("\nGather y_predict ", g, "\n")
          y_pred_gr = data.frame(read.csv("y_predict.csv"))
          y_pred = cbind(y_pred, y_pred_gr)
          #print(nrow(y_pred))

          # deleting files
          print(system(paste("rm -r ", FolderGroup, sep="")))

          g = g + 1
          gc()
        }

        #cat("\nSave files ", g, "\n")
        FolderSplit = paste(
          diretorios$folderResultDataset,
          "/Partition-",
          info$numberOfPartition,
          "/Split-",
          f,
          sep = ""
        )
        setwd(FolderSplit)
        y_pred = y_pred[, -1]
        y_true = y_true[, -1]
        write.csv(y_pred, "y_predict.csv", row.names = FALSE)
        write.csv(y_true, "y_true.csv", row.names = FALSE)

        gc()
      } # fim do foreach
    }

    gc()
    cat(
      "\n##################################################################################################"
    )
    cat("\n# Gather Predicts: END                                                                           #")
    cat(
      "\n##################################################################################################"
    )
    cat("\n\n\n\n")

  } # fim da função




##################################################################################################
# FUNCTION EVALUATION HYBRID PARTITIONS                                                          #
#   Objective                                                                                    #
#      Evaluates the hybrid partitions                                                           #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       FolderHybrid: path of hybrid partition results                                           #
#   Return                                                                                       #
#       Assessment measures for each hybrid partition                                            #
##################################################################################################
eval <-
  function(id_part,
           ds,
           dataset_name,
           number_folds,
           namesLabels,
           folderResults) {
    diretorios = directories(dataset_name, folderResults)
    info <- infoPartitions(id_part, dataset_name, folderResults)

    if (number_folds == 1) {
      library("mldr")
      library("utiml")

      cat("\nFold Único")
      # specifyin folder for the fold
      FolderSplit = paste(
        diretorios$folderResultDataset,
        "/Partition-",
        info$numberOfPartition,
        "/Split-1",
        sep = ""
      )

      # get the true and predict lables
      setwd(FolderSplit)
      y_true = data.frame(read.csv("y_true.csv"))
      y_pred = data.frame(read.csv("y_predict.csv"))

      # compute measures multilabel
      y_true2 = data.frame(sapply(y_true, function(x)
        as.numeric(as.character(x))))
      y_true3 = mldr_from_dataframe(y_true2 ,
                                    labelIndices = seq(1, ncol(y_true2)),
                                    name = "y_true2")
      y_pred2 = sapply(y_pred, function(x)
        as.numeric(as.character(x)))

      #cat("\n\t\tSave Confusion Matrix")
      setwd(FolderSplit)
      sink(file = "Conf-Mat-Fold-1.txt", type = "output")
      confmat = multilabel_confusion_matrix(y_true3, y_pred2)
      print(confmat)
      sink()

      # creating a data frame
      confMatPart = multilabel_evaluate(confmat)
      confMatPart = data.frame(confMatPart)
      names(confMatPart) = "Fold-1"
      write.csv(confMatPart, "Split-1-Evaluated.csv")

      # delete files
      setwd(FolderSplit)
      unlink("y_true.csv", recursive = TRUE)
      unlink("y_predict.csv", recursive = TRUE)

      gc()

    } else {
      # from fold = 1 to number_folder
      f = 1
      evalParal <- foreach(f = 1:number_folds) %dopar% {
        library("mldr")
        library("utiml")

        cat("\nFold: ", f)

        # data frame
        apagar = c(0)
        confMatPartitions = data.frame(apagar)
        partitions = c()

        # specifyin folder for the fold
        FolderSplit = paste(
          diretorios$folderResultDataset,
          "/Partition-",
          info$numberOfPartition,
          "/Split-",
          f,
          sep = ""
        )

        # get the true and predict lables
        setwd(FolderSplit)
        y_true = data.frame(read.csv("y_true.csv"))
        y_pred = data.frame(read.csv("y_predict.csv"))

        # compute measures multilabel
        y_true2 = data.frame(sapply(y_true, function(x)
          as.numeric(as.character(x))))
        y_true3 = mldr_from_dataframe(y_true2 ,
                                      labelIndices = seq(1, ncol(y_true2)),
                                      name = "y_true2")
        y_pred2 = sapply(y_pred, function(x)
          as.numeric(as.character(x)))

        #cat("\n\t\tSave Confusion Matrix")
        setwd(FolderSplit)
        salva3 = paste("Conf-Mat-Fold-", f, ".txt", sep = "")
        sink(file = salva3, type = "output")
        confmat = multilabel_confusion_matrix(y_true3, y_pred2)
        print(confmat)
        sink()

        # creating a data frame
        confMatPart = multilabel_evaluate(confmat)
        confMatPart = data.frame(confMatPart)
        names(confMatPart) = paste("Fold-", f, sep = "")
        namae = paste("Split-", f, "-Evaluated.csv", sep = "")
        write.csv(confMatPart, namae)

        # delete files
        setwd(FolderSplit)
        unlink("y_true.csv", recursive = TRUE)
        unlink("y_predict.csv", recursive = TRUE)

        gc()
      } # end folds
    }

    gc()
    cat(
      "\n##################################################################################################"
    )
    cat("\n# Evaluation Folds: END                                                                          #")
    cat(
      "\n##################################################################################################"
    )
    cat("\n\n\n\n")
  }


##################################################################################################
# FUNCTION GATHER EVALUATIONS                                                                    #
#   Objective                                                                                    #
#       Gather metrics for all folds                                                             #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       FolderHybrid: path of hybrid partition results                                           #
#   Return                                                                                       #
#       Assessment measures for all folds                                                        #
##################################################################################################
gatherEvaluation <-
  function(id_part,
           ds,
           dataset_name,
           number_folds,
           namesLabels,
           folderResults) {
    diretorios = directories(dataset_name, folderResults)
    info <- infoPartitions(id_part, dataset_name, folderResults)
    FolderPartition = paste(diretorios$folderResultDataset,
                            "/Partition-",
                            info$numberOfPartition,
                            sep = "")

    # vector with names
    measures = c(
      "accuracy",
      "average-precision",
      "clp",
      "coverage",
      "F1",
      "hamming-loss",
      "macro-AUC",
      "macro-F1",
      "macro-precision",
      "macro-recall",
      "margin-loss",
      "micro-AUC",
      "micro-F1",
      "micro-precision",
      "micro-recall",
      "mlp",
      "one-error",
      "precision",
      "ranking-loss",
      "recall",
      "subset-accuracy",
      "wlp"
    )

    # data frame
    apagar = c(0)
    avaliado4 = data.frame(apagar)
    folds = c(0)
    nomesFolds = c(0)

    if (number_folds == 1) {
      cat("Fold Único")

      # specifying folder for the fold
      FolderSplit = paste(FolderPartition, "/Split-1", sep = "")
      setwd(FolderSplit)
      avaliado = data.frame(read.csv("Split-1-Evaluated.csv"))
      names(avaliado) = c("medidas", "valores")

      setwd(diretorios$folderReportsDataset)
      write.csv(avaliado,
                paste("Partitions-", id_part, "-Evaluated.csv", sep = ""))

    } else {
      # from fold = 1 to number_folders
      f = 1
      while (f <= number_folds) {
        cat("\nFold: ", f)

        # specifying folder for the fold
        FolderSplit = paste(FolderPartition, "/Split-", f, sep = "")
        setwd(FolderSplit)
        str = paste("Split-", f, "-Evaluated.csv", sep = "")
        avaliado = data.frame(read.csv(str))
        names(avaliado)[1] = "medidas"
        avaliado2 = data.frame(avaliado[order(avaliado$medidas, decreasing = FALSE), ])
        avaliado3 = data.frame(avaliado2[, -1])
        avaliado4 = cbind(avaliado4, avaliado3)
        #names(avaliado4)[f+1] = paste("Fold-", f, sep="")
        nomesFolds[f] = paste("Fold-", f, sep = "")

        setwd(FolderSplit)
        unlink(str)

        f = f + 1
        gc()

      } # end folds

      #cat("\nSAVE MEASURES")
      avaliado4$apagar = measures
      colnames(avaliado4) = c("measures", nomesFolds)

      setwd(FolderPartition)
      nome3 = paste("Partition-",
                    info$numberOfPartition,
                    "-Evaluated.csv",
                    sep = "")
      write.csv(avaliado4, nome3, row.names = FALSE)

      setwd(diretorios$folderReportsDataset)
      nome3 = paste("Partition-",
                    info$numberOfPartition,
                    "-Evaluated.csv",
                    sep = "")
      write.csv(avaliado4, nome3, row.names = FALSE)

      setwd(diretorios$folderResultDataset)
      nome3 = paste("Partition-",
                    info$numberOfPartition,
                    "-Evaluated.csv",
                    sep = "")
      write.csv(avaliado4, nome3, row.names = FALSE)

      nome4 = paste("Partition-",
                    info$numberOfPartition,
                    "-Sum-Eval.csv",
                    sep = "")
      avaliado5 = avaliado4[, -1]
      avaliado6 = data.frame(apply(avaliado5, 1, mean))
      colnames(avaliado6) = "Mean-10Folds"
      avaliado7 = cbind(measures, avaliado6)

      setwd(FolderPartition)
      write.csv(avaliado7, nome4)

      setwd(diretorios$folderResultDataset)
      write.csv(avaliado7, nome4)

      setwd(diretorios$folderReportsDataset)
      write.csv(avaliado7, nome4)
    }

    gc()
    cat(
      "\n##################################################################################################"
    )
    cat("\n# Evaluated Partition: END                                                                       #")
    cat(
      "\n##################################################################################################"
    )
    cat("\n\n\n\n")
  }

##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
