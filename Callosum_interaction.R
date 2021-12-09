getwd()
setwd("S:/PhD/Tesisti/Improta/attachments")

# aprire file database in formato txt

data <- read.table("BSC_CC.csv",
              sep = ",", na.string="NA", header = T)

# visualizzare pattern di dati mancanti (NA)

library(mice)
md.pattern(data)

# non ci sono dati mancanti, quindi proseguo
# per visualizzare i nomi delle colonne -> names(data2)


# la funzione lapply applica iterativamente il modello di regressione multipla
# sostituendo a x di volta in volta le colonne da 32 a 35 (i quattro valori di DTI dell'arcuato)
#model1

models.1 <- lapply(data[,41:54],
                        function(x) lm(x ~ L2_AoA + Language_Entropy  + cambridge._score + sex_dic + education_yrs + TIB, data = data))



# mostra i risultati di ciascun fattore

summaries.models.1 <- lapply(models.1, summary)
summaries.models.1


#AoA X EXP

models.inter1 <- lapply(data[,41:54],
 function(x) lm(x ~ L2_AoA*Language_Entropy  + cambridge._score + sex_dic + education_yrs + TIB, data = data))



# mostra i risultati di ciascun fattore

summaries.inter1 <- lapply(models.inter1, summary)
summaries.inter1
                                                 

# sposta i p-values in un vettore

p_value.AoAxExp <- c()


for (i in 1:4){
  matrix.inter1 <- summaries.inter1[[i]][4][[1]]
  vettore.AoAxExp <- c(matrix.inter1[2,4])
  for (ii in 1:length(vettore.AoAxExp)){
    p_value.AoAxExp <- c(p_value.AoAxExp, round(vettore.AoAxExp[ii], digits = 3))
  }
}

p_value.AoAxExp


#correzione per confronti multipli.

p.adjust(p_value.AoAxExp, method = "fdr", n=length(p_value.AoAxExp))


#AoA X PROF

models.inter2 <- lapply(data[,41:54],
 function(x) lm(x ~ L2_AoA*cambridge._score + Language_Entropy  + sex_dic + education_yrs + TIB, data = data))



# mostra i risultati di ciascun fattore

summaries.inter2 <- lapply(models.inter2, summary)
summaries.inter2
                                                 

# sposta i p-values in un vettore

p_value.AoAxProf <- c()


for (i in 1:4){
  matrix.inter2 <- summaries.inter2[[i]][4][[1]]
  vettore.AoAxProf <- c(matrix.inter2[2,4])
  for (ii in 1:length(vettore.AoAxProf)){
    p_value.AoAxProf <- c(p_value.AoAxProf, round(vettore.AoAxProf[ii], digits = 3))
  }
}

p_value.AoAxProf


#correzione per confronti multipli.

p.adjust(p_value.AoAxProf, method = "fdr", n=length(p_value.AoAxProf))



#Exp X PROF

models.inter3 <- lapply(data[,41:54],
 function(x) lm(x ~ cambridge._score*Language_Entropy + L2_AoA + sex_dic + education_yrs + TIB, data = data))



# mostra i risultati di ciascun fattore

summaries.inter3 <- lapply(models.inter3, summary)
summaries.inter3
                                                 

# sposta i p-values in un vettore

p_value.ExpxProf <- c()


for (i in 1:4){
  matrix.inter3 <- summaries.inter3[[i]][4][[1]]
  vettore.ExpxProf <- c(matrix.inter3[2,4])
  for (ii in 1:length(vettore.ExpxProf)){
    p_value.ExpxProf <- c(p_value.ExpxProf, round(vettore.ExpxProf[ii], digits = 3))
  }
}

p_value.ExpxProf


#correzione per confronti multipli.

p.adjust(p_value.ExpxProf, method = "fdr", n=length(p_value.ExpxProf))


p_value.interactions <- c(p_value.AoAxExp, p_value.AoAxProf, p_value.ExpxProf)
p.adjust(p_value.interactions, method = "fdr", n=length(p_value.interactions))

