##################################################################################################
# Copyright (C) 2021                                                                             #
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
# Configures the workspace according to the operating system                                     #
##################################################################################################
FolderRoot = "~/Oracle-ECC"
FolderScripts = paste(FolderRoot, "/R/", sep="")

library(stringr)

setwd(FolderRoot)
datasets = data.frame(read.csv("datasets-originals.csv"))
n = nrow(datasets)

FolderJob = paste(FolderRoot, "/Jobs", sep="")
if(dir.exists(FolderJob)==FALSE){dir.create(FolderJob)}

  i = 1
  while(i<=n){

    dataset = datasets[i,]
    cat("\n\tDataset:", dataset$Name)

    nome_job = paste("O-", dataset$Name, sep="")
    nome_pasta1 = paste("/scratch/", nome_job, sep="")
    nome_pasta2 = paste("/dev/shm/", nome_job, sep="")

    nome = paste(FolderJob, "/", nome_job, ".sh", sep="")
    output.file <- file(nome, "wb")

    write("#!/bin/bash", file = output.file)

    str1 = paste("#SBATCH -J ", nome_job, sep="")
    write(str1, file = output.file, append = TRUE)

    write("#SBATCH -o %j.out", file = output.file, append = TRUE)

    write("#SBATCH -n 1", file = output.file, append = TRUE)

    write("#SBATCH -c 10", file = output.file, append = TRUE)

    #write("#SBATCH --partition slow", file = output.file, append = TRUE)

    #write("#SBATCH -t 720:00:00", file = output.file, append = TRUE)

    write("#SBATCH -t 128:00:00", file = output.file, append = TRUE)

    #write("#SBATCH --mem=0", file = output.file, append = TRUE)

    write("#SBATCH --mem-per-cpu=20GB", file = output.file, append = TRUE)

    write("#SBATCH --mail-user=elainegatto@estudante.ufscar.br",
          file = output.file, append = TRUE)

    write("#SBATCH --mail-type=ALL", file = output.file, append = TRUE)
    write("", file = output.file, append = TRUE)


    str2 = paste("local_job1=",  "\"/dev/shm/", nome_job, "\"", sep="")
    write(str2, file = output.file, append = TRUE)
    str2 = paste("local_job2=",  "\"/scratch/", nome_job, "\"", sep="")
    write(str2, file = output.file, append = TRUE)
    write("function clean_job(){", file = output.file, append = TRUE)
    str3 = paste(" echo", "\"Limpando ambiente...\"", sep=" ")
    write(str3, file = output.file, append = TRUE)
    str4 = paste(" rm -rf ", "\"${local_job1}\"", sep="")
    write(str4, file = output.file, append = TRUE)
    str4 = paste(" rm -rf ", "\"${local_job2}\"", sep="")
    write(str4, file = output.file, append = TRUE)
    write("}", file = output.file, append = TRUE)
    write("trap clean_job EXIT HUP INT TERM ERR",
          file = output.file, append = TRUE)
    write("", file = output.file, append = TRUE)


    write("set -eE", file = output.file, append = TRUE)
    write("umask 077", file = output.file, append = TRUE)


    write("", file = output.file, append = TRUE)
    str5 = paste("echo RUN ", nome_job, sep="")
    write(str5, file = output.file, append = TRUE)
    write("", file = output.file, append = TRUE)


    write("", file = output.file, append = TRUE)
    write("echo APAGANDO PASTA", file = output.file, append = TRUE)
    str11 = paste("rm -rf ", nome_pasta1, sep="")
    write(str11, file = output.file, append = TRUE)
    str11 = paste("rm -rf ", nome_pasta2, sep="")
    write(str11, file = output.file, append = TRUE)


    write("", file = output.file, append = TRUE)
    write("echo CRIANDO PASTA", file = output.file, append = TRUE)
    str11 = paste("mkdir ", nome_pasta1, sep="")
    write(str11, file = output.file, append = TRUE)


    write("", file = output.file, append = TRUE)
    write("echo COPIANDO MINICONDA", file = output.file, append = TRUE)
    str20 = paste("cp /home/u704616/miniconda3.tar.gz ", 
                  nome_pasta1, sep="")
    write(str20 , file = output.file, append = TRUE)


    write(" ", file = output.file, append = TRUE)
    write("echo DESCOMPACTANDO MINICONDA", file = output.file, append = TRUE)
    str22 = paste("tar xzf ", nome_pasta1,
                  "/miniconda3.tar.gz -C ", nome_pasta1, sep="")
    write(str22 , file = output.file, append = TRUE)



    write(" ", file = output.file, append = TRUE)
    write("echo SOURCE", file = output.file, append = TRUE)
    str21 = paste("source ", nome_pasta1,
                  "/miniconda3/etc/profile.d/conda.sh ", sep="")
    write(str21, file = output.file, append = TRUE)

    write(" ", file = output.file, append = TRUE)
    write("echo ATIVANDO CONDA", file = output.file, append = TRUE)
    write("conda activate AmbienteTeste", file = output.file, append = TRUE)
    write(" ", file = output.file, append = TRUE)


    write("echo EXECUTANDO", file = output.file, append = TRUE)
    str7 = paste("Rscript /home/u704616/Oracle-ECC/R/oracle.R ",
                 dataset$Id, " 10 10 \"/scratch/", 
                 nome_job, "\"", sep="")
    write(str7, file = output.file, append = TRUE)
    write(" ", file = output.file, append = TRUE)


    write("echo APAGANDO PASTA DO JOB", file = output.file, append = TRUE)
    str11 = paste("rm -rf /scratch/", nome_job, sep="")
    write(str11, file = output.file, append = TRUE)

    close(output.file)

    i = i + 1
    gc()
  }
  