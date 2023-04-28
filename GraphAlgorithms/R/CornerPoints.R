CornerPoints <- function(X,Y,Cls = rep(1, length(X))){

  # Ergaenzt Punkte an den Ecken, die einen Datensatz toroid machen.
  #
  # Input:
  # X, Y: Vektoren mit koordinaten der Datenpunkte
  # Cls: Vektor mit der Klasse der Datenpunkte, falls nicht gegeben, sind alle Daten Cls = 1.
  # Output:
  # XplusCorner, YplusCorner, ClsPlusCorner: Vektoren X, Y und CLS, an deren Ende jeweils die zus?tzlichen Punkte, bzw. deren Klasse angef?gt wurde.
  # CornerX, CornerY, CornerCLS: Zus?tzliche Punkte f?r X, Y und deren Klasse in CLS
  # VoronoiRandLimits: Die Limits der Voronoilinien an den Ecken.

  RandDistanzFaktor    <-   1;  # so weit weg liegen die Randpunkte
  VoronoiLimitFaktor   <- 0.1; # so weit weg liegen die Voronoigrenzen an den Randpunkten
  VoronoiRandLimits    <- vector(length = 4)
  minX                 <- min(X);
  maxX                 <- max(X);
  minY                 <- min(Y);
  maxY                 <- max(Y);
  rangeX               <- maxX - minX;
  rangeY               <- maxY - minY;
  distX                <- rangeX / 3;
  distY                <- rangeY / 3;
  left                 <- (minX - (rangeX * RandDistanzFaktor));
  VoronoiRandLimits[1] <- (minX - (rangeX * VoronoiLimitFaktor)); 
  right                <- (maxX + (rangeX * RandDistanzFaktor));
  VoronoiRandLimits[2] <- (maxX + (rangeX * VoronoiLimitFaktor));
  down                 <- (minY - (rangeY * RandDistanzFaktor));
  VoronoiRandLimits[3] <- (minY - (rangeY * VoronoiLimitFaktor));
  up                   <- (maxY + (rangeY * RandDistanzFaktor));
  VoronoiRandLimits[4] <- (maxY + (rangeY * VoronoiLimitFaktor));
  leftCornerX          <- rep(left, 4); 
  leftCornerY          <- seq(minY, up, by = distY)[1:4];
  rightCornerX         <- rep(right, 4);
  rightCornerY         <- seq(minY, up, by = distY)[1:4];
  lowerCornerY         <- rep(down, 4);
  lowerCornerX         <- seq(minX, right, by = distX)[1:4];
  upperCornerY         <- rep(up, 4);
  upperCornerX         <- seq(minX, right, by = distX)[1:4];
  CornerX              <- c(leftCornerX, rightCornerX, lowerCornerX, upperCornerX);
  CornerY              <- c(leftCornerY, rightCornerY, lowerCornerY, upperCornerY);
  XPlusCorner          <- c(X, CornerX);
  YPlusCorner          <- c(Y, CornerY);
  ClsCorner            <- c(rep(1,4), rep(2,4), rep(3,4), rep(4,4))*10000; # Yes, that's absolutely inefficient and makes no sense but that's how the mathlab code looks like.
  ClsPlusCorner        <- c(Cls, ClsCorner);
  
  return(list("XPlusCorner"       = XPlusCorner,
              "YPlusCorner"       = YPlusCorner,
              "ClsPlusCorner"     = ClsPlusCorner,
              "CornerX"           = CornerX,
              "CornerY"           = CornerY,
              "ClsCorner"         = ClsCorner,
              "VoronoiRandLimits" = VoronoiRandLimits))
}