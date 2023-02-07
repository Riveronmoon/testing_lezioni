#######################################################
### Testing psicologico (PSP6075525)
### A.A. 2022/2023
### prof. Antonio Calcagnì (antonio.calcagni@unipd.it)
#######################################################

## CONTENUTO DEL CODICE ##################################
# (A) Covarianza e correlazione: breve intro
# (B) Grafici per matrici di correlazione
# (C) Comparare matrici di correlazione
##########################################################


# Inizializzazione ambiente di lavoro -------------------------------------
rm(list=ls()); graphics.off()
setwd("~/MEGA/Lavoro_sync/Didattica/2022_2023/testing_psicologico/")
library(psych)


# (A) Covarianza e correlazione: breve intro ------------------------------

# Utilizziamo il dataset Interest della libreria hemp che contiene risposte ad un questionario con scale sugli aspetti cognitivi e di personalità.
# Maggiori info sulle variabili del dataset: ?hemp::interest oppure https://github.com/cddesja/hemp/blob/master/R/data.R
load("laboratorio/data/interest.RData")
str(interest)
interest$gender = as.factor(interest$gender)
head(interest)

# Il dataset presenta tre variabili iniziali (gender,educ, age) che non riguardano le scale del questionario. Inoltre ci sono dim(interest)[2] = 30 variabili in tutto, troppe.
# Prendiamo un sottoinsieme con alcune scale interessanti:
interest.scales = interest[,c("vocab","reading","sentcomp","geometry","analyrea","sociabty","worry")]
# Mettiamo le variabili concomitanti age,educ,gender in un secondo dataframe:
interest.cmts = interest[,c(1:3)]

# Le variabili riferite alle scale sono già standardizzate, infatti:
apply(X = interest.scales,MARGIN = 2,FUN = mean) #approx 0
apply(X = interest.scales,MARGIN = 2,FUN = var) #approx 1

# Correlazione e covarianza sono difatti simili
cor(interest.scales)
cov(interest.scales)

# Immaginiamo di conoscere le varianze delle variabili/scale, ad esempio:
# Nota: in questo caso stiamo generando le varianze in maniera casuale da una densità Uniforme, ossia var ~ U(2.34,7.78)
set.seed(123)
vars.scales = runif(n = NCOL(interest.scales),min = 2.34,max = 7.78) #campioniamo NCOL(interest.scales) = 7 varianze da una distribuzione uniforme in [2.34,7.78]

# e otteniamo la matrice di covarianza da quella di correlazione:
cov.scales = lavaan::cor2cov(R = cor(interest.scales),sds = sqrt(vars.scales))
cor.scales = cov2cor(V = cov.scales)

# Prima rappresentazione grafica delle correlazioni a coppia:
# Nota: questo grafico è pensato per variabili continue!
psych::pairs.panels(x = interest.scales)
# Lungo la diagonale del grafico abbiamo l'istogramma di frequenze e quello perequato delle variabili/scale, nel triangolo inferiore del grafico abbiamo i grafici a dispersione bivariati 
# con la stima del modello di relazione, infine nel triangolo superiore abbiamo i coeff di correlazione bivariati (calcolati con la formula di Pearson)

# Per quantificare l'associazione quando le variabili non sono continue, possiamo usare le misure di correlazione per ranghi:
# Spearman's rank correlation coefficient and Kendall's rank correlation coefficient (τ) measure the extent to which, as one variable increases, the other variable tends to increase, 
# without requiring that increase to be represented by a linear relationship. If, as the one variable increases, the other decreases, the rank correlation coefficients will be negative.
# https://en.wikipedia.org/wiki/Correlation_and_dependence#Rank_correlation_coefficients
# Facciamo un esempio prendendo le variabili concomitanti del nostro dataset:
cor(x = interest.cmts$educ,interest.cmts$age,method = "spearman")
cor(x = interest.cmts$educ,interest.cmts$age,method = "kendall")


# (B) Grafici per matrici di correlazione -------------------------------------

# Un modo alternativo e grafico per rappresentare matrici di correlazioni (soprattuto quando le dimensioni sono grandi)
library(corrplot)

corrplot(cor.scales, method = "circle")
# Grandezza dei cerchi e colore sono in relazione alla magnitudine della correlazione

corrplot(cor.scales, method = "square") #..usando i quadratini
# Grandezza dei quadrati e colore sono in relazione alla magnitudine della correlazione

corrplot(cor.scales, method = "color") #..usando solo il colore
# Il colore sono in relazione alla magnitudine della correlazione

# In generale notiamo che le variabili {vocab,reading,sentcomp} sono tra loro abbastanza correlate (colore blu scuro), analogamente {geometry,analyrea}.
# Le variabili sociabty e worry invece non correlano con il resto delle variabili (colore chiaro tendente al bianco).

corrplot.mixed(corr = cor.scales, upper = "circle") #..usando cerchi e numeri

# E' possibile anche ordinare le correlazioni graficamente in modo che quelle più alte siano più vicine tra loro:
corrplot(cor.scales, method = "circle",order="hclust") #..usando un metodo di clustering gerarchico per l'ordinamento
corrplot(cor.scales, method = "circle",order="FPC") #..usando un metodo di decomposizione in componenti principali (PCA) per l'ordinamento

# oppure semplicemente usando l'ordine alfabetico (senza tener conto della magnitudine delle correlazioni):
corrplot(cor.scales, method = "circle",order="alphabet") 

# Le visualizzazioni possono essere personalizzate in modi diversi. Si veda il tutorial della libreria:
# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html

# Un modo ulteriore è quello che combina il grafico heatmap insieme al clustering gerarchico:
heatmap(x = cor.scales,symm = TRUE)

# ..cambiando il gradiente del colore:
col = colorRampPalette(c("white", "red", "orange"))(NCOL(cor.scales)) #white: zero cor; red: negative cor; organe: positive cor
heatmap(x = cor.scales,symm = TRUE,col=col)

# Il dendrogramma posto sopra i margini del grafico indica i raggruppamenti (cluster) ottenuti sulle similarità delle correlazioni mediante clustering

# E' possibile anche visualizzare una matrice di correlazione mediante un grafico a rete
library(qgraph)
qgraph(input = cor.scales,minimum=0.05,vsize=10,legend=FALSE,borders=TRUE) 

# I nodi connessi indicano presenza di correlazione, colore e spessore dei nodi rappresentano la magnitudine della correlazione
# Possiamo anche raggruppare i nodi della rete rispetto al gruppo di appartenenza delle variabili, ad esempio rispetto alle aree di indagine delle scale.
# Poniamo: {vocab,reading,sentcomp} = area_A, {geometry,analyrea} = area_B, {worry,sociabty} = area_C

groups = list(A=c(1:3),B=c(4:5),C=c(6:7))
qgraph(input = cor.scales,minimum=0.05,vsize=10,legend=TRUE,borders=TRUE,groups=groups) #raggruppamenti per colore

# ..oppure possiamo usare un raggruppamento basato sul clustering gerarchico delle variabili con metodo di Ward
hclust.scales = hclust(d = dist(cor.scales),method = "ward.D2")
plot(hclust.scales) #dendrogramma del clustering effettuato sulla matrice delle distanze tra le variabili (trasformazione della matrice di correlazione)
groups2 = cutree(hclust.scales,k = 2) #decidiamo 2 gruppi

groups2 = list(A=which(groups2==1),B=which(groups2==2)) #creiamo la lista dei gruppi per la funzione qgraph
qgraph(input = cor.scales,minimum=0.05,vsize=10,legend=TRUE,borders=FALSE,groups=groups2,theme="classic") #raggruppamenti per colore

# Grafico finale per le due reti
par(mfrow=c(1,2))
qgraph(input = cor.scales,minimum=0.0,vsize=10,legend=TRUE,borders=TRUE,groups=groups,layout="spring")
qgraph(input = cor.scales,minimum=0.0,vsize=10,legend=TRUE,borders=FALSE,groups=groups2,layout="spring") # le due reti differiscono per raggruppamento


# (C) Comparare matrici di correlazione -----------------------------------
library(dendextend)
# https://cran.r-project.org/web/packages/dendextend/vignettes/introduction.html
# La comparazione grafica tra matrici di correlazione può avvenire in diversi modi. Qui utilizzeremo la comparazione tra dendrogrammi ottenuti
# sulle matrici di correlazione.
# Compariamo le matrici di correlazione sulle alcune variabili (p=22) del nostro dataset interest, classificate per maschi e femmine 

matA = cor(interest[interest$gender==1,c(2:22)]) #matrice di correlazione per femmine
matB = cor(interest[interest$gender==2,c(2:22)]) #matrice di correlazione per maschi

# Clustering gerarchico con il metodo di Ward
hclust.matA = hclust(d = dist(matA),method = "ward.D2") 
hclust.matB = hclust(d = dist(matB),method = "ward.D2")

# Creazione dei dendrogrammi
dendA = as.dendrogram(hclust.matA)
dendB = as.dendrogram(hclust.matB)

# Visualizzazione del tanglegram
# A tanglegram plot gives two dendrogram (with the same set of labels), one facing the other, 
# and having their labels connected by lines. Tanglegram can be used for visually comparing two methods of Hierarchical clustering.
dendextend::tanglegram(dendA,dendB,main_left = "femmine",main_right = "maschi")

# ..quanto sono allineati/simili i nostri dendrogrammi?
dendextend::entanglement(dendA,dendB) # approx 1: scarso allineamento (approx 0: ottimo allineamento)

# Le strutture che sottendono le nostre variabili tra femmine e maschi sono poco simili, come evidenziato dal basso indice di entanglement e dal
# grafico tipo tanglegram (gli archi che connettono le variabili del dendrogramma si incrociano).
# Le matrici di correlazione tra femmine e maschi sulle variabili cognitive e di personalità sono differenti.

# Ulteriore confronto con qgraph()
# E' possibile visualizzare matrici di correlazione di maschi e femmine a confronto usando la visualizzazione a rete, come visto nella sezione [C]:
par(mfrow=c(1,2))
qgraph(input = matA,minimum=0.0,vsize=10,legend=FALSE,borders=TRUE,layout="spring",title="Femmine")
qgraph(input = matB,minimum=0.0,vsize=10,legend=FALSE,borders=TRUE,layout="spring",title="Maschi") 


# (D) Trasformazioni di scala per variabili categoriali/ordinali ----------
# Molti modelli e metodi statistici per dati (multivariati) derivanti da questionari e test presuppongono che le variabili misurate siano continue.
# Una pratica comune, che però può distorcere i risultati, è quella di considerare le categorie ordinate di risposta (es.: scala Likert) come continue.
# Un altro approccio considera invece lo sviluppo di metodi che tengano conto della natura del dato in questione (ordinale o categoriale). 
# Una terza via è quella che considera la trasformazione del dato categoriale o ordinale in continuo, utilizzando procedure ad-hoc che ne preservino l'informazione.
# I metodi che ricadono in quest'ultima categoria sono detti metodi di "optimal scaling".
# The idea behind OPTIMAL SCALING is to assign numerical quantifications to the categories of each variable, thus allowing standard procedures to be used to obtain a solution on the quantified variables.
# The optimal scale values are assigned to categories of each variable based on the optimizing criterion of the procedure in use. Unlike the original labels of the nominal or ordinal variables in the analysis, these scale values have metric properties.
# In most Categories procedures, the optimal quantification for each scaled variable is obtained through an iterative method called alternating least squares in which, after the current quantifications are used to find a solution, the quantifications are updated using that solution. The updated quantifications are then used to find a new solution, 
# which is used to update the quantifications, and so on, until some criterion is reached that signals the process to stop (source: https://www.ibm.com/support/knowledgecenter/en/SSLVMB_23.0.0/spss/categories/optimal_scaling_whatis.html)

# Una tecnica nota per calcolare matrici di correlazione da dati ordinali è nota come LINEALS
# de Leeuw, J. (1988). Multivariate analysis with linearizable regressions. Psychometrika, 53, 437-454. 
# Usiamo la funzione lineals() della libreria aspect
library(aspect)

# Quantificazione della correlazione lineare tra variabili ordinali e nominali
data(package="aspect",dataset="galo") # carichiamo il dataset galo che contiene un misto di variabili categoriali (es.: "gender") e ordinali (es.: "IQ")
head(galo)
R_galo = aspect::lineals(data = galo) # calcolo dell'associazione lineare tra le variabili miste
print(R_galo)

# Quantificazione della correlazione lineare tra variabili ordinali
# Consideriamo il dataset "Environment" che contiene item di tipo politomico
datax_poly = ltm::Environment
head(datax_poly)
str(datax_poly)
datax_poly = sapply(X = datax_poly,FUN = as.numeric) #convertiamo ciascuna colonna (item) da variabile categoriale a numerica. Nota: sapply() funziona come il ciclo for()

R_poly = aspect::lineals(data = datax_poly,level = rep("ordinal",NCOL(datax_poly)))
print(R_poly)
cor(datax_poly,method = "spearman") #in questo caso sono molto simili i risultati

# Le matrici di correlazioni così ottenute possono essere utilizzate per analisi multidimensionali successive, 
# utilizzando la quantificazione ottenuta mediante optimal scaling.


# Un altro modo per calcolare una statistica di associazione lineare (come la correlazione) tra variabili ordinali è
# quello di ricorrere al metodo della correlazione policorica che, date due o più variabili osservate ordinali, permette di stimarne
# la correlazione continua sottesa. Info: https://en.wikipedia.org/wiki/Polychoric_correlation
# Note: John Uebersax (2015) makes the interesting point that both polychoric and tetrachoric correlations should be called "latent correlations" or 
# "latent continuous correlations" because of the way they are found and not tetrachoric or polychoric which is the way they were found in the past. 
# That is, what is the correlation between two latent variables that when artificially broken into two (tetrachoric) or more (polychoric) values produces 
# the n x n table of observed frequencies.

R_polyc = psych::polychoric(x = datax_poly)
print(R_polyc$rho) #latent correlation matrix
print(R_polyc$tau) #latent continuous thresholds corresponding to observed categories
# Note: a variable with K observed levels (in the current example, K=3) admits K-1 latent thresholds. Thus, in the current example,
# for the variable "LeadPetrol" the continuous levels of the observed (1,2,3) levels are: 
# 1 <-> -inf
# 2 <-> 0.29
# 3 <-> 1.56

# If we compare polychoric with lineals, we found that they are different.
print(R_polyc$rho)
print(R_poly$cormat)
# This is because LINEALS works by linearizing the non-linear association among the observed variables whereas POLYCHORIC estimates a continuous
# correlation matrix among latent normally distributed variables underlying the observed ones.




