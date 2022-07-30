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
#
###############################################################################
partition <- function(id_part,
                      ds,
                      dataset_name,
                      number_dataset,
                      number_cores,
                      number_folds,
                      folderResults,
                      namesLabels){

  retorno = list()

  diretorios = directories(dataset_name, folderResults)

  #cat("\nGet bell partition information")
  info <- infoPartitions(id_part, dataset_name, folderResults)

  #cat("\nCreate the specific folder")
  FolderPartition = paste(diretorios$folderTest, "/Partition-",
                          id_part, sep="")
  if(dir.exists(FolderPartition)==FALSE){dir.create(FolderPartition)}

  # from fold 1 to last partition
  #cat("\nStart build partitions")
  f = 1
  buildBellPartitions <- foreach(f = 1:number_folds) %dopar% {
  # while(f<=10){

    cat("\n\nFold: ", f)

    #####################################################################
    FolderRoot = "~/Oracle-ECC"
    FolderScripts = paste(FolderRoot, "/R", sep="")

    #####################################################################
    # LOAD LIBRARIES
    setwd(FolderScripts)
    source("libraries.R")

    setwd(FolderScripts)
    source("utils.R")

    #####################################################################
    diretorios = directories(dataset_name, folderResults)

    #####################################################################
    FolderSplit = paste(FolderPartition, "/Split-", f, sep="")
    if(dir.exists(FolderSplit)==FALSE){dir.create(FolderSplit)}

    # get bell partition information
    info = infoPartitions(id_part, dataset_name, folderResults)

    #####################################################################
    #cat("\nOpen Train file ", f, "\n")
    setwd(diretorios$folderCVTR)
    nome_arq_tr = paste(dataset_name, "-Split-Tr-", f, ".csv", sep="")
    arquivo_tr = data.frame(read.csv(nome_arq_tr))

    #####################################################################
    #cat("\nOpen Validation file ", f, "\n")
    setwd(diretorios$folderCVVL)
    nome_arq_vl = paste(dataset_name, "-Split-Vl-", f, ".csv", sep="")
    arquivo_vl = data.frame(read.csv(nome_arq_vl))

    #####################################################################
    #cat("\nOpen Test file ", f, "\n")
    setwd(diretorios$folderCVTS)
    nome_arq_ts = paste(dataset_name, "-Split-Ts-", f, ".csv", sep="")
    arquivo_ts = data.frame(read.csv(nome_arq_ts))

    #####################################################################
    # juntando treino e validação
    arquivo_tr2 = rbind(arquivo_tr, arquivo_vl)

    #####################################################################
    # separando os rótulos verdadeiros
    y_true = arquivo_ts[,ds$LabelStart:ds$LabelEnd]

    # gerando indices
    number = seq(ds$LabelStart, ds$LabelEnd, by=1)

    # transformando treino em mldr
    ds_train = mldr_from_dataframe(arquivo_tr2, labelIndices = number)
    br_train = mldr_transform(ds_train, type = "BR")

    # transformando test em mldr
    ds_test = mldr_from_dataframe(arquivo_ts, labelIndices = number)
    br_test = mldr_transform(ds_test, type = "BR")

    # aplicando modelo br
    brmodel = br(ds_train, "C5.0", seed=123, cores = number_cores)

    # testando modelo br
    predict_tbr <- predict(brmodel, ds_test,
                           cores=number_cores)

    # compute a threshold
    thresholds_br <- scut_threshold(predict_tbr, ds_test,
                                    cores = number_cores)

    # applying a threshold
    new.test.br <- fixed_threshold(predict_tbr, thresholds_br)

    # convert into matrix
    new.test.br2 = as.matrix(new.test.br)

    # convert into dataframe
    new.test.br3 = data.frame(new.test.br2)

    # saving
    setwd(FolderSplit)
    write.csv(new.test.br3, "br-predict.csv")

    g = 1
    while(g<=info$numberGroupsOfPartition){

      ####################################################################################
      nome_particao = paste("partition_", info$numberOfPartition, sep="")
      nome_grupo = paste("group_", g, sep="")

      ####################################################################################
      nome_grupo_2 = paste("Group-", g, sep="")
      FolderGroup = paste(FolderSplit , "/", nome_grupo_2, sep="")
      if(dir.exists(FolderGroup)==FALSE){dir.create(FolderGroup)}


      ####################################################################################
      # get the labels of this group
      specificPartition = info$specificPartition
      specificGroup = specificPartition %>%
        filter(., specificPartition$group == g)

      ####################################################################################
      # total de rótulos neste grupo
      totalLabelsThisGr = nrow(specificGroup)

      ######################################################################################################################
      #cat("\nTRAIN: Mount Group ", g)
      atributos_tr = arquivo_tr2[ds$AttStart:ds$AttEnd]
      n_a = ncol(atributos_tr)
      classes_tr = select(arquivo_tr2, specificGroup$labels)
      n_c = ncol(classes_tr)
      grupo_tr = cbind(atributos_tr, classes_tr)
      n_g = ncol(grupo_tr)
      inicio_tr = n_a+1
      fim_tr = ncol(grupo_tr)

      ######################################################################################################################
      #cat("\nTEST: Mount Group: ", g)
      atributos_ts = arquivo_ts[ds$AttStart:ds$AttEnd]
      n_a = ncol(atributos_ts)
      classes_ts = select(arquivo_ts, specificGroup$labels)
      n_c = ncol(classes_ts)
      grupo_ts = cbind(atributos_ts, classes_ts)
      n_g = ncol(grupo_ts)
      inicio_ts = n_a+1
      fim_ts = ncol(grupo_ts)

      # indices
      indices = seq(inicio_tr, fim_tr, by=1)

      # total de rótulos no grupo
      e = as.numeric(length(indices))

      if(e<=1){
        cat("\nUM RÓTULO")

        y_predict = select(new.test.br3, specificGroup$labels)
        setwd(FolderGroup)
        write.csv(y_predict, "y_predict.csv", row.names = FALSE)

        y_true2 = select(y_true, specificGroup$labels)
        setwd(FolderGroup)
        write.csv(y_true2, "y_true.csv", row.names = FALSE)

      } else {
        cat("\nMAIS DE UM RÓTULO")

        # treino
        ds_train_g = mldr_from_dataframe(grupo_tr,
                                       labelIndices = indices)
        #ds_train_g$labels

        # teste
        ds_test_g = mldr_from_dataframe(grupo_ts,
                                      labelIndices = indices)
        #ds_test_g$labels

        # Create an Ensemble of Classifier Chains using
        # Random Forest (randomForest package)
        ecc_model_g <- ecc(ds_train_g, "C5.0", seed=123, cores = number_cores)

        # Predict
        test_ecc_g <- predict(ecc_model_g, ds_test_g, cores = number_cores)

        # Apply a threshold
        thresholds_ecc_g <- scut_threshold(test_ecc_g,
                                           ds_test_g,
                                           cores = number_cores)

        new.test.ecc.g <- fixed_threshold(test_ecc_g, thresholds_ecc_g)

        setwd(FolderGroup)
        write.csv(new.test.ecc.g, "y_predict.csv", row.names = FALSE)

        y_true2 = select(y_true, specificGroup$labels)
        setwd(FolderGroup)
        write.csv(y_true2, "y_true.csv", row.names = FALSE)

      }


      ########################################################################################################################
      # count
      g = g + 1

      gc()
    } # END GROUP

    f = f + 1
    gc()
  }

  ########################################################################################################################
  # return
  retorno$id_part = id_part
  retorno$ds = ds
  retorno$dataset_name = dataset_name
  retorno$number_folds = number_folds
  retorno$namesLabels = namesLabels
  retorno$infoPartition = info
  retorno$FolderPartition = FolderPartition
  return(retorno)

  gc()
  cat("\n##################################################################################################")
  cat("\n# Build and Test Partitions: END                                                                 #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
