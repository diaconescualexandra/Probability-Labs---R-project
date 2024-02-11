# Partea I

# I.a
frepcomgen <- function(n, m) {  
  # Inițializare tabel cu repartiție comună incompletă
  repartitie_comuna <- matrix(NA, nrow = n + 1, ncol = m + 1)
  
  # Etichete pentru variabilele aleatoare X și Y
  etichete_x <- sort(round(runif(n, min = -3, max = 3), 2))
  etichete_x <- c(etichete_x, "qj")
  etichete_y <- sort(round(runif(m, min = -3, max = 3), 2))
  etichete_y <- c(etichete_y, "pi")
  
  # Setarea etichetelor pentru coloane și rânduri
  colnames(repartitie_comuna) <- etichete_y
  rownames(repartitie_comuna) <- etichete_x
  
  # Normalize the values to make their sum equal to 1
  random_values <- runif(n)
  normalized_values_row <- random_values / sum(random_values)
  
  # Adaug valori random (a caror suma este 1) pe coloana "pi"
  repartitie_comuna[1:n, (m + 1)] <- normalized_values_row
  
  # Normalize the values to make their sum equal to 1
  random_values <- runif(m)
  normalized_values_col <- random_values / sum(random_values)
  
  # Adaug valori random (a caror suma este 1) pe randul "qj"
  repartitie_comuna[(n + 1), 1:m] <- normalized_values_col
  
  # Adaug valori in tabel astfel incat sa poata fi determinata repartitia
  # comuna a celor doua v.a.
  for (i in 1:n) {
    # Voi alege pentru fiecare rand, la intamplare, o coloana in care sa nu adaug
    # nicio valoare, urmand ca aceasta sa fie completata folosind functia de la b)
    random_index <- sample(1:m, 1)
    for (j in 1:m) {
      if (j == random_index) {
        next
      }
      else {
        repartitie_comuna[i, j] <- normalized_values_row[i] * normalized_values_col[j]
      }
    }
  }
  
  repartitie_comuna[(n + 1), (m + 1)] <- 1
  
  # Returnarea tabelului pentru a fi completat ulterior
  return(repartitie_comuna)
}

# Exemplu
repartitie_comuna_incompleta <- frepcomgen(3, 4)
print(repartitie_comuna_incompleta)

repartitie_comuna_incompleta <- frepcomgen(7, 10)
print(repartitie_comuna_incompleta)

# I.b
fcomplrepcom <- function(repartitie_comuna) {
  # Obtin n si m
  matrix_dimensions <- dim(repartitie_comuna)
  
  n <- matrix_dimensions[1] - 1
  m <- matrix_dimensions[2] - 1
  
  # Completez tabelul
  for (i in 1:n) {
    # Stiu ca pe fiecare rand exista o celula in tabel care nu este completata
    # Cand gasesc aceasta celula, memorez index-ul coloanei
    # Pentru fiecare rand i voi calcula suma valorilor existente (row_sum)
    # (mai putin ultima, care este pi)
    # Orice valoare NA va fi inlocuita cu:
    # repartitie_comuna[i, m + 1] - row_sum
    row_sum <- 0
    for (j in 1:m) {
      if (is.na(repartitie_comuna[i, j])) {
        index <- j
      }
      else {
        row_sum <- row_sum + repartitie_comuna[i, j]
      }
    }
    repartitie_comuna[i, index] <- repartitie_comuna[i, m + 1] - row_sum
  }
  
  # Returnez tabelul completat
  return(repartitie_comuna)
}

# Exemplu
repartitie_comuna_completa <- fcomplrepcom(repartitie_comuna_incompleta)
print(repartitie_comuna_completa)

# I.c
frepmarginal <- function(repartitie_comuna) {
  # Obține dimensiunile repartiției comune
  n <- nrow(repartitie_comuna) - 1
  m <- ncol(repartitie_comuna) - 1
  
  # Initializez repartițiile marginale
  repartitie_marginala_X <- matrix(NA, nrow = 1, ncol = n)
  repartitie_marginala_Y <- matrix(NA, nrow = 1, ncol = m)
  
  # Valorile variabilelor aleatoare X și Y
  etichete_x <- rownames(repartitie_comuna)[1:n]
  etichete_y <- colnames(repartitie_comuna)[1:m]
  
  # colnames(repartitie_marginala_X) <- paste0(etichete_x)
  colnames(repartitie_marginala_X) <- etichete_x
  rownames(repartitie_marginala_X) <- "pi"
  # colnames(repartitie_marginala_Y) <- paste0(etichete_y)
  colnames(repartitie_marginala_Y) <- etichete_y
  rownames(repartitie_marginala_Y) <- "qj"
  
  # Adaug probabilitatile in repartitiile marginale
  for (i in 1:n) {
    repartitie_marginala_X[i] <- repartitie_comuna[i, m + 1]
  }
  for (j in 1:m) {
    repartitie_marginala_Y[j] <- repartitie_comuna[n + 1, j]
  }
  
  # Returnez repartiile
  return(list(repartitie_marginala_X = repartitie_marginala_X, 
              repartitie_marginala_Y = repartitie_marginala_Y))
}

# Exemplu
repartitii_marginale <- frepmarginal(repartitie_comuna_completa)

# Afișează repartițiile marginale pentru X și Y
print("Repartitie marginala pentru X:")
print(repartitii_marginale$repartitie_marginala_X)
print("Repartitie marginala pentru Y:")
print(repartitii_marginale$repartitie_marginala_Y)

# I.d
fpropcov <- function(X, pX, Y, pY, a,b,c,d) # valori pentru x , y probabilitatile lor si parametri a,b,c,d
{
  E_X <- sum ( X * pX) # calculeaza media lui x 
  E_Y <- sum (Y * pY) #media lui y
  E_Z <- a * E_X + b * E_Y # media lui Z=aX+bY
  E_T <- c * E_X + d * E_Y # media lui T
  
  E_ZT <- sum ( (a* X + b*Y) * ( c*X + d*Y)*pX* pY )  #Z*T = (aX+bY) * (cX+dY), E(z*T) = Z*T *pX*PY
  
  covZT <- E_ZT -( E_Z*E_T ) #covarianta este media inmultirii - mediile inmultite
  
  return(covZT)
  
}

#exemplu 

X <- c(1, 2)
pX <- c(0.5, 0.5)
Y <- c(3, 4)
pY <- c(0.5, 0.5)
a <- 1
b <- 2
c <- 1
d <- 3 

covarianța_ZT <- fpropcov(X, pX, Y, pY, a, b, c, d)
print(covarianța_ZT)

# I.e
fPcond <- function(repartitie_comuna, x_cond, y_cond) {
  # Obțin indexul corespunzător valorii x_cond în repartitia_comuna
  index_x <- which(rownames(repartitie_comuna) == as.character(x_cond))
  
  # Obțin indexul corespunzător valorii y_cond în repartitia_comuna
  index_y <- which(colnames(repartitie_comuna) == as.character(y_cond))
  
  # Verifică dacă valorile date sunt prezente în repartitia_comuna
  if (length(index_y) == 0 || length(index_x) == 0) {
    stop("Valorile date nu se găsesc în repartitia_comuna.")
  }
  
  # Calculez probabilitatea
  prob <- repartitie_comuna[index_x, index_y] / 
    repartitie_comuna[nrow(repartitie_comuna), index_y]
  
  return(prob)
}

# Exemplu
prob <- fPcond(repartitie_comuna_completa, 1.72, -2.75)
print(prob)

# I.f
fPcomun <- function(repartitie_comuna, valoare_X, valoare_Y) {
  # Găsirea indicilor corespunzători pentru valorile date ale variabilelor X și Y
  indice_X <- match(as.character(valoare_X), rownames(repartitie_comuna))
  indice_Y <- match(as.character(valoare_Y), colnames(repartitie_comuna))
  
  # Verificare dacă valorile sunt valide
  if (is.na(indice_X) || is.na(indice_Y)) {
    stop("Valorile date pentru X sau Y nu sunt în repartiția comună.")
  }
  
  # Calculul probabilității pentru perechea (X, Y)
  probabilitate <- repartitie_comuna[indice_X, indice_Y]
  
  return(probabilitate)
}

# Exemplu
valoare_X <- as.character(1.72)
valoare_Y <- as.character(-2.75)
probabilitate_valori <- fPcomun(repartitie_comuna_completa, valoare_X, valoare_Y)
print(probabilitate_valori)


# I.g.1
#se preiau indicii matricei
n <- 3 
m <- 4

#se genereaza repartitia comuna completa
repartitie_comuna_completa <- fcomplrepcom(repartitie_comuna_incompleta)

# preiau primele n coloane si m linii
# preiau indicii liniilor selectate mai sus si apoi inmultesc matricele
# rezultatul se inmulteste cu probabilitatile lui X si Y
cov_xy <- sum(sum(repartitie_comuna_completa[1:n, 1:m] * outer(as.numeric(rownames(repartitie_comuna_completa[1:n, 1:m])), 
                                                               as.numeric(colnames(repartitie_comuna_completa[1:n, 1:m])), "*")))

cov <- 5 * (-3) * cov_xy
print(cov)

# I.g.2
gEx2 <- function(repartitie_comuna) {
  
  # P(0<X<0.8|Y>0.3)
  lim_inf_x = -2
  lim_sup_x = 0.8
  lim_inf_y = -2
  
  # Obține dimensiunile repartiției comune
  n <- nrow(repartitie_comuna) - 1
  m <- ncol(repartitie_comuna) - 1
  
  # Valorile variabilelor aleatoare X și Y
  etichete_x <- rownames(repartitie_comuna)[1:n]
  etichete_y <- colnames(repartitie_comuna)[1:m]
  
  # Probabilitatea ceruta este initial 0
  prob <- 0
  
  # Adun probabilitatile
  for (i in seq_along(etichete_x)) {
    if (lim_inf_x < as.numeric(etichete_x[i]) && as.numeric(etichete_x[i]) < lim_sup_x) {
      for (j in seq_along(etichete_y)) {
        if (lim_inf_y < as.numeric(etichete_y[j])) {
          prob <- prob + fPcond(repartitie_comuna, etichete_x[i], etichete_y[j])
        }
      }
    }
  }
  return(prob)
}

# Exemplu
prob_g2 <- gEx2(repartitie_comuna_completa)
print(prob_g2)

# 1.g.3
gEx3 <- function(repartitie_comuna) {
  # P(X>0.2,Y<1.7)
  lim_inf_x = 0
  lim_sup_y = -2
  
  # Obține dimensiunile repartiției comune
  n <- nrow(repartitie_comuna) - 1
  m <- ncol(repartitie_comuna) - 1
  
  # Valorile variabilelor aleatoare X și Y
  etichete_x <- rownames(repartitie_comuna)[1:n]
  etichete_y <- colnames(repartitie_comuna)[1:m]
  
  # Probabilitatea ceruta este initial 0
  prob <- 0
  
  # Adun probabilitatile
  for (i in seq_along(etichete_x)) {
    if (lim_inf_x < as.numeric(etichete_x[i])) {
      for (j in seq_along(etichete_y)) {
        if (as.numeric(etichete_y[j]) < lim_sup_y) {
          prob <- prob + repartitie_comuna[etichete_x[i], etichete_y[j]]
        }
      }
    }
  }
  
  # Metoda 2
  # indices_x <- which(as.numeric(etichete_x) > 0)
  # indices_y <- which(as.numeric(etichete_y) < -2)
  # prob <- sum(repartitie_comuna[indices_x, indices_y])
  
  return(prob)
}

# Exemplu
prob_g3 <- gEx3(repartitie_comuna_completa)
print(prob_g3)

# I.h
#independente
fverind <-  function (n,m) 
{
  # repartitia comuna completa 
  repartitie_comuna_incompleta <-  frepcomgen(n,m)
  repartitie_comuna_completa <- fcomplrepcom(repartitie_comuna_incompleta)
  
  #parcurg matricele
  for ( i in 1:n) {
    for ( j in 1:m)
    {
      prob_comuna_estimata <- repartitie_comuna_completa[i, j] # P(X=x, Y=y), probabilitatea din tabelul cu repartitia comuna
      prob_marginala_x <- repartitie_comuna_completa[i, m+1] #probabilitatile marginale ale lui X, adica ultima coloana
      prob_marginala_y <- repartitie_comuna_completa[n+1, j] ##probabilitatile marginale ale lui y, adica ultima linie
      
      if (prob_comuna_estimata != prob_marginala_y*prob_marginala_x ) # daca probabilitatea deja existenta in tabel este diferita de cea calculata inmultind probabilitatile
        #corespunzatoare 
      {
        return("x si y nu sunt independente")
        
      }
    }
  }
  
  return ( "x si y sunt independente ")
  
}
#exemplu
rezultat <- fverind(3, 4)
print(rezultat)

#necorelate daca coeficientul de corelatie = 0, adica daca cov(x,y) = 0
fvernecor <- function(n, m) {
  
  #repartitia comuna completa
  repartitie_comuna_incompleta <- frepcomgen(n, m)
  repartitie_comuna_completa <- fcomplrepcom(repartitie_comuna_incompleta)
  
  # preiau primele n coloane si m linii
  # preiau indicii liniilor selectate mai sus si apoi inmultesc matricele
  # rezultatul se inmulteste cu probabilitatile lui X si Y
  cov_xy <- sum(sum(repartitie_comuna_completa[1:n, 1:m] * outer(as.numeric(rownames(repartitie_comuna_completa[1:n, 1:m])), 
                                                                 as.numeric(colnames(repartitie_comuna_completa[1:n, 1:m])), "*")))
  
  
  if ( cov_xy == 0)
  {cat("variabilele x si y sunt necorelate \n")
    return (TRUE) }
  else{
    cat (" altceva \n")
    return ( FALSE) }
}

rezultat <- fvernecor(3, 3)

# Partea II

library(pracma)
library(plotly)

# Functia ######################################################################
################################################################################
# Adaug functia dorita
f <- function(x, y) {
  # return (1/13 * x * y^2)
  # return (1 + 4*x*y)
  return (x^2 + y^2)
}

# Adaug intervalele pentru x si y
inf_x <- 0; sup_x <- 1
inf_y <- 0; sup_y <- 1
################################################################################

# II.a
verificare_fubini <- function(f, inf_x, sup_x, inf_y, sup_y) {
  
  # integral2: folosita pentru a calcula integrala dubla a unei functii pe un
  #     anumit interval
  # $message: integral2 returneaza un obiect care contine diverse informatii
  #     despre rezultatul integrarii duble. Folosim "$message" pentru a verifica
  #     daca integrarea a fost realizata cu succes sau nu
  
  if (is.null(integral2(f, inf_x, sup_x, inf_y, sup_y)$message)) {
    cat("Integrala dublă poate fi calculată folosind teorema lui Fubini.\n")
    return(TRUE)
  } else {
    cat("Integrala dublă nu poate fi calculată folosind teorema lui Fubini.\n")
    return(FALSE)
  }
}

integrala <- function(f, inf_x, sup_x, inf_y, sup_y) {
  # Se calculează integrala dublă folosind teorema lui Fubini
  result <- integral2(f, inf_x, sup_x, inf_y, sup_y)$Q
  # result <- integral2(f, inf_x, sup_x, inf_y, sup_y)$value
  # result <- integral2(f, inf_x, sup_x, inf_y, sup_y)$result
  
  return(result)
}

if(verificare_fubini(f, inf_x, sup_x, inf_y, sup_y)) {
  # Daca putem aplica teorema lui Fubini, calculam integrala dubla si o afisam
  result <- integrala(f, inf_x, sup_x, inf_y, sup_y)
  cat("Valoarea integralei duble este:", result, "\n")
}

# II.b
interpretare_geometrica <- function() {
  # Generarea unui set de puncte
  x <- seq(0, 5, length.out = 100)
  y <- seq(2, 5, length.out = 100)
  
  # Generare valori aplicand functi f la toate combinatiile de x si y
  z <- outer(x, y, function(x, y) 1 + 4*x*y)
  
  # Interpretare geometrica a integralei duble
  plot_ly(x = x, y = y, z = z, type = "surface")
  persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")
}

# II.c
dens_de_prob_verif1 <- function(f, inf_x, sup_x, inf_y, sup_y) {
  # Generarea unui set de puncte din domeniu
  x_values <- seq(inf_x, sup_x, length.out = 100)
  y_values <- seq(inf_y, sup_y, length.out = 100)
  # Creez perechi de valori (x,y)
  puncte <- expand.grid(x = x_values, y = y_values)
  
  # Evaluarea funcției în punctele generate
  values <- apply(puncte, 1, function(pct) f(pct[1], pct[2]))
  # Verific daca toate valorile sunt mai mari sau egale cu 0
  verif1 <- all(values >= 0)
  
  if (verif1) {
    print("f(x, y) >= 0 pe tot domeniul")
  } else {
    print("Funcția f(x, y) are valori negative")
  }
  return(verif1)
}

dens_de_prob_verif2 <- function(f, inf_x, sup_x, inf_y, sup_y) {
  # Calculez integrala pe "R^2"
  # value <- integrala(f, -Inf, Inf, -Inf, Inf)
  value <- integrala(f, -100, 100, -100, 100)
  
  # Verific daca rezultatul este apeoximativ 1
  tolerance <- 0.001
  verif2 <- abs(value - 1) < tolerance
  
  if (verif2) {
    print("Integrala dubla din f(x,y) este aproximativ egală cu 1.")
  } else {
    print("Integrala dubla din f(x,y) nu este aproximativ egală cu 1.")
  }
  
  return(verif2)
}

densitate_de_probabilitate <- function(f, inf_x, sup_x, inf_y, sup_y) {
  # Daca f(x,y) >= 0 si integrala dubla din f(x,y) este 1 atunci functia este
  # densitate de probabilitate
  if (dens_de_prob_verif1(f, inf_x, sup_x, inf_y, sup_y) &&
      dens_de_prob_verif2(f, inf_x, sup_x, inf_y, sup_y)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

result <- densitate_de_probabilitate(f, inf_x, sup_x, inf_y, sup_y)
if (result) {
  print("Functia f(x,y) este densitate de probabilitate")
} else {
  print("Functia f(x,y) nu este densitate de probabilitate")
}

# II.f
# Funcția pentru a reprezenta densitatea și funcția de repartiție : caz unidimensional

plot_density_cdf <- function(mean, sd) {
  x <- seq(-5, 5, length.out = 1000) #valorile lui x
  y_density <- dnorm(x, mean, sd) #densitatea
  y_cdf <- pnorm(x, mean, sd) #functia de repartitie
  
  return(list(x = x, y_density = y_density, y_cdf = y_cdf))
}

device <- plot_density_cdf(1, 2)

# plot gol pentru initializare cu valorile lui x din device, densitatea 
#pune limita axei y
plot(device$x, device$y_density, type = "l", col = "blue", ylim = c(0, max(device$y_density, device$y_cdf)),
     main = "Densitatea si Functia de repartitie", xlab = "Value", ylab = "Probabilitatea")
#adauga valorile functiei de repartitie
lines(device$x, device$y_cdf, col = "red")

# in exemplu se parcurge de la 0.5 la 2 din 0.1 in 0.1
for (i in seq(0.5, 2, by = 0.1)) {
  device <- plot_density_cdf(1, i)  # updateaza valorile lui y pt diferite sd
  
  # se updateaza fct de repartitie si densitatea
  lines(device$x, device$y_density, col = "blue")
  lines(device$x, device$y_cdf, col = "red")
  
  Sys.sleep(0.5)
}


# f bidimensionale, repartitie normala

library(mvtnorm) # pentru bidimensionale
plot_density_cdf_2d <- function(mean, sigma) {
  n <- 100 #numarul de puncte, pentru generarea graficului
  x <- seq(-5, 5, length.out = n)
  y <- seq(-5, 5, length.out = n)
  z_density <- matrix(0, n, n) #initializeaza densitatea cu dimensiunea n
  z_cdf <- matrix(0, n, n) #initializeaza functia de repartitie cu dimensiunea n
  #pentru fiecare punct (x[i], y[j]), calculeaza densitatea si fct de repartitie 
  for (i in 1:n) {
    for (j in 1:n) {
      z_density[i, j] <- dmvnorm(c(x[i], y[j]), mean, sigma)
      z_cdf[i, j] <- pmvnorm(lower = c(-Inf, -Inf), upper = c(x[i], y[j]), mean = mean, sigma = sigma)
    }
  }
  
  return(list(x = x, y = y, z_density = z_density, z_cdf = z_cdf))
}

# apeleaza functia pentru c(1,1) si matricea (c(2, 1, 1, 2) )
device_2d <- plot_density_cdf_2d(mean = c(1, 1), sigma = matrix(c(2, 1, 1, 2), nrow = 2))
# un plot 3D
persp(device_2d$x, device_2d$y, device_2d$z_density, col = "lightsalmon", main = "2D Density and CDF", xlab = "X", ylab = "Y", zlab = "Density")
# loop pentru a itera prin diferite sd uri
for (i in seq(0.5, 2, by = 0.1)) {
  sigma <- matrix(c(i, 0, 0, i), nrow = 2)  # updateaza matricea cu noua sd
  #se updateaza plot ul cu noile valori
  device_2d <- plot_density_cdf_2d(mean = c(1, 1), sigma = sigma)
  persp(device_2d$x, device_2d$y, device_2d$z_density, col = "lightsalmon", main = "2D Density and CDF", xlab = "X", ylab = "Y", zlab = "Density")
  Sys.sleep(0.5)
}

