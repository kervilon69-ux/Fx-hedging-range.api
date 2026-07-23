library(plumber)



#* @filter cors
function(req, res) {
 
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  plumber::forward()
}

#* @apiTitle FX-Hedging API

#* Calcul de la matrice
#* @post /calculer
#* @serializer json

#colB = spot
#colC = Fw
#colD = Foreign value
#colE = Domestic value calculated with spot only (i.e colD/colB)
#COL_F = minimum value in domestic ccy / COL_F <- (marge/100)*colD/colB[1]
#colG = cushion
#colH = Domestic value calculated with spot and with hedging (Fw)
#colI = exposure 
#colJ = exposure controled
#colK = Hedged
#colL = Hedged change
#colM = linear progression



function(req) {

  body <- jsonlite::fromJSON(req$postBody)
  colD <- as.numeric(body$colD)
  marge     <- as.numeric(body$marge)
  tolerance <- as.integer(body$tolerance)
  colB <- as.numeric(unlist(strsplit(body$colB, ",")))
  colC <- as.numeric(unlist(strsplit(body$colC, ",")))
  vol   <- if (!is.null(body$vol)   && body$vol   != "") as.numeric(strsplit(body$vol,   ",")[[1]]) else rep(NA, length(colB))
  txEtr <- if (!is.null(body$txEtr) && body$txEtr != "") as.numeric(strsplit(body$txEtr, ",")[[1]]) else rep(NA, length(colB))
  txDom <- if (!is.null(body$txDom) && body$txDom != "") as.numeric(strsplit(body$txDom, ",")[[1]]) else rep(NA, length(colB))
  range <- if (as.character(body$range) != "no") {as.numeric(body$range)}
  


##############################################
##############      IF     ###################
##############################################

if (as.character(body$range) == "no"){

  PARAM <- 9
  COL_F <- (marge/100)*colD/colB[1]
  n     <- length(colB)

  matrice <- matrix(NA, nrow = n, ncol = 13)
  colnames(matrice) <- c("colA","colB","colC","colD","colE","colF",
                          "colG","colH","colI","colJ","colK","colL","colM")

  for (i in 1:n) {
    colE <- colD / colB[i]
    colG <- colE - COL_F
    print(COL_F)

    # calcul lambda ici

    # calcul extrema ici

    if (i == 1) {
      colH <- colE
    } else if (i == 2) {
      colH <- matrice[i-1, "colJ"] * colE + matrice[i-1, "colK"] * colD / colC[i-1]
    } else {
      colH <- matrice[i-1, "colJ"] * colE + sum(matrice[1:(i-1), "colL"] * colD / colC[1:(i-1)])
    }

    # Calcul paramètre lambda
    if (i == 1) {
      tradeoff <- -1*((txEtr[i]/100)-(txDom[i]/100)) + (tolerance+2)*1*(vol[i]/100) # poids à 1 (100%) car hedge = 0 quand i = 0
      } else {
      tradeoff <- -matrice[i-1, "colJ"]*((txEtr[i]/100)-(txDom[i]/100)) + (tolerance+2)*matrice[i-1, "colJ"]*(vol[i]/100)
      }
     print(tradeoff)

     if(is.na(tradeoff)){PARAM=9}
    else {
    PARAM <- 9+(((3-9)/(0.29-0.21))*(tradeoff-0.21)) # parametrage dans D:\DD\Documents\Chaire\Documentation technique\Prog\Fx-hedging-control\FX_process_test.xlsx
    PARAM <- max(3, min(PARAM,9))}
    print(PARAM)

    colI <- ifelse(is.na(colH), NA, min(max(colG * PARAM / colH, 0), 1))

    if (i == 1) {
      colJ <- colI
    } else if (is.na(colI)) {
      colJ <- matrice[i-1, "colJ"]
    } else {
      colJ <- min(colI, matrice[i-1, "colJ"], na.rm = TRUE)
    }

    colK <- 1 - colJ
    colL <- if (i == 1) colK else colK - matrice[i-1, "colK"]

     # Hedge incrément : somme cumulée de 10% (10%, 20%, 30%, ...)
  colM <- i * 100/n

    matrice[i, ] <- c(i, colB[i], colC[i], colD, colE, COL_F,
                       colG, colH, colI, colJ, colK, colL, colM)
  }

  list(
    colA = as.vector(matrice[, "colA"]),
    colE = as.vector(matrice[, "colE"]),
    colH = as.vector(matrice[, "colH"]),
    colK = as.vector(matrice[, "colK"]),
    colM = as.vector(matrice[, "colF"])
  )

  

}

###########################################
#############   ELSE   ####################
###########################################


else {
  
  PARAM <- 9
  COL_F <- (marge/100)*colD/colB[1]
  COL_FF <- (1+(1-(marge/100)))*colD/colB[1]
  n     <- length(colB)
  



  matrice <- matrix(NA, nrow = n, ncol = 16)
  colnames(matrice) <- c("colA","colB","colC","colD","colE","colF",
                          "colG","colH","colI","colJ","colK","colL","colM","col_FF", "colGG", "colGGG" )

    
  for (i in 1:n) {
    
   # calcul cushion baisse 
    colE <- colD / colB[i]
    colG <- colE - COL_F
    

  # calcul cushion hausse
    colE <- colD / colB[i]
    colGG <- COL_FF - colE
    
  # cushion
    colGGG <- min(colG,colGG)

    # Calcul paramètre lambda
    if (i == 1) {
      tradeoff <- -1*((txEtr[i]/100)-(txDom[i]/100)) + (tolerance+2)*1*(vol[i]/100) # poids à 1 (100%) car hedge = 0 quand i = 0
      } else {
      tradeoff <- -matrice[i-1, "colJ"]*((txEtr[i]/100)-(txDom[i]/100)) + (tolerance+2)*matrice[i-1, "colJ"]*(vol[i]/100)
      }
     print(tradeoff)

     if(is.na(tradeoff)){PARAM=9}
    else {
    PARAM <- 9+(((3-9)/(0.29-0.21))*(tradeoff-0.21)) # parametrage dans D:\DD\Documents\Chaire\Documentation technique\Prog\Fx-hedging-control\FX_process_test.xlsx
    PARAM <- max(3, min(PARAM,9))}
    print(PARAM)

    


# calcul deal value spot + hedged
    if (i == 1) {
      colH <- colE
    } else if (i == 2) {
      colH <- matrice[i-1, "colJ"] * colE + matrice[i-1, "colK"] * colD / colC[i-1]
    } else {
      colH <- matrice[i-1, "colJ"] * colE + sum(matrice[1:(i-1), "colL"] * colD / colC[1:(i-1)])
    }

    
# calcul position avec cushion bas ou cushion haut (min des deux)
    colI <- ifelse(is.na(colH), NA, min(max(colGGG * PARAM / colH, 0), 1))

    colI<- ifelse (colB[i]<=(colB[1]*(1+range/100)) & colB[i]>=(colB[1]*(1-range/100)),1, colI) # à l'intérieur du range l'expo est de 1 sinon expo

print(colI)

   # ajustement sortie de range à 0% hedge
  #  expo_top<-((colD/(colB[1]*(1+range/100))-COL_F)*PARAM)/(colD/(colB[1]*(1+range/100)))
  #  expo_low<-(COL_FF-colD/(colB[1]*(1-range/100)))*PARAM/(colD/(colB[1]*(1-range/100)))
#print (expo_top)
#print (expo_low)
    
 #   colI<- ifelse(colB[i]>=(colB[1]*(1+range/100)),colI+(1-expo_top),colI)
  #  colI<- ifelse(colB[i]<=(colB[1]*(1-range/100)),colI+(1-expo_low),colI)
  #  colI<-min(max(colI,0),1)
   #  print(colI)


    if (i == 1) {      # l'expo ne peut pas augmenter lorsqu'elle a baissé
      colJ <- colI
    } else if (is.na(colI)) {
      colJ <- matrice[i-1, "colJ"]
    } else {
      colJ <- min(colI, matrice[i-1, "colJ"], na.rm = TRUE)
    }

    colK <- 1 - colJ
    
 

    # delta de hedging
    colL <- if (i == 1) colK else colK - matrice[i-1, "colK"]

     # Hedge incrément : somme cumulée de 10% (10%, 20%, 30%, ...)
  colM <- i * 100/n

    matrice[i, ] <- c(i, colB[i], colC[i], colD, colE, COL_F,
                       colG, colH, colI, colJ, colK, colL, colM, COL_FF, colGG, colGGG)
  }

  list(
    colA = as.vector(matrice[, "colA"]),
    colE = as.vector(matrice[, "colE"]),
    colH = as.vector(matrice[, "colH"]),
    colK = as.vector(matrice[, "colK"]),
    colM = as.vector(matrice[, "colF"])
  )

}


} # end function
