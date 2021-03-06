library(CreditMetrics)
lgd <- 0.45
# matrice de migration empirique sur un an � partir du site web de standard&poors
rc <- c("AAA", "AA", "A", "BBB", "BB", "B", "CCC", "D")
N <- 3
n <- 50000
r <- 0.03
ead <- c(4000000, 1000000, 10000000)	
rating <- c("BBB", "AA", "B")	
firmnames <- c("firm 1", "firm 2", "firm 3")
alpha <- 0.99

M <- matrix(c(90.81,  8.33,  0.68,  0.06,  0.08,  0.02,  0.01,   0.01,
              0.70, 90.65,  7.79,  0.64,  0.06,  0.13,  0.02,   0.01,
              0.09,  2.27, 91.05,  5.52,  0.74,  0.26,  0.01,   0.06,
              0.02,  0.33,  5.95, 85.93,  5.30,  1.17,  1.12,   0.18,
              0.03,  0.14,  0.67,  7.73, 80.53,  8.84,  1.00,   1.06,
              0.01,  0.11,  0.24,  0.43,  6.48, 83.46,  4.07,   5.20,
              0.21,     0,  0.22,  1.30,  2.38, 11.24, 64.86,  19.79,
              0,     0,     0,     0,     0,     0,     0, 100
)/100, 8, 8, dimnames = list(rc, rc), byrow = TRUE)

#cm.cs(M, lgd)

# calcule la valeur de cr�dit � risque pour les profits et pertes simul�s.

# correlation matrix
rho<- matrix(c(  1, 0.4, 0.6,
                  0.4,   1, 0.5,
                  0.6, 0.5,   1), 3, 3, dimnames = list(firmnames, firmnames),
              byrow = TRUE) 
rho

 cm.CVaR(M, lgd, ead, N, n, r, rho, alpha, rating)
 
# cm.gain calcule les profits ou les pertes, ceci est fait en construisant la diff�rence par
# rapport � la valeur de r�f�rence et aux valeurs de portefeuille simul�es des positions de 
# cr�dit.
 
 cm.gain(M, lgd, ead, N, n, r, rho, rating)

 # trace un histogramme pour la distribution simul�e des profits et pertes.
 
 cm.hist(M, lgd, ead, N, n, r, rho, rating,
         col = "steelblue4", main = "Profit / Loss Distribution",
         xlab = "profit / loss", ylab = "frequency")

 # cm.matrix teste si la matrice M donn�e est une matrice de migration. Les dimensions de la 
 # matrice de migration doivent donc �tre au moins 2 fois 2 et les dimensions des lignes et
 # des colonnes doivent �tre �gales. De plus, les valeurs de la matrice de migration doivent 
 # �tre comprises entre 0 et 1, et la somme de chaque ligne doit �tre de 1 
 
 cm.matrix(M)

 # cm.portfolio calcule les valeurs de portefeuille simul�es en utilisant la fonction cm.val.
 cm.portfolio(M, lgd, ead, N, n, r, rho, rating)
 
 # cm.quantile calcule les quantit�s de migration empirique pour chaque cote d'une
 # matrice de migration empirique d'un an. La limite de d�faillance est le quantile de 
 # la probabilit� de d�faillance.
 
 cm.quantile(M) 
 
 