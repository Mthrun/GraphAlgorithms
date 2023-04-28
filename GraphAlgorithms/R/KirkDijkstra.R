KirkDijkstra=function(AorV,xyCorE, SID = NULL, FID = NULL, verbose = F,waitbar=F){
# r = KirkDijkstra(AorV,xyCorE,SID,FID) 
# Der Dijkstra Algorithmus zum finden der kuerzesten Pfade und deren Kosten
#
###INPUT
# AorV				A oder V mit
#		A 				NxN Adjazenzmatrix, mit A(I,J) != 0 genau dann wenn I und J verbunden sind
#       V 				Nx2 (oder Nx3) Matrix mit x,y,(z) Koordinaten
# xyCorE			xy oder C oder E mit
#		xy 				Nx2 ( oder Nx3) Matrix mit x,y,(z) Koordinaten. Nur falls A erstes Argument ist
#		C 				NxN Kostenmatrix, C(I,J) enthaelt die Kosten f?r die Bewegung von I nach J
#		E 				Px2 Matrix die eine Liste der Kanten in den ersten beiden Spalten enth?lt
#		E3				Px3 Matrix die eine Lister der Kanten in den ersten beiden Spalten und ihre
#						Gewichtungen in der dritten Spalte enth?lt
# SID 				1xL Vektor der Startpunkte. Wenn nicht angegeben, werden minimale Pfade f?r alle
#						N Punkte zu den Zielpunkten berechnet
# FID               1xM Vektor der Zielpunkte. Wenn nicht angegeben, werden minimale Pfade f?r die Startpunkte	
#						zu allen N  Punkten berechnet
# verbose   Falls TRUE werden, werden Statusnachrichten ausgegeben
# waitbar   Fals TRUE wird ein Ladebalken angezeigt, nur unter Windows moeglich; nur bei verbose=TRUEE
#
###OUTPUT
# costs             LxM Matrix der Minimalen Kosten der Minimalen Pfade
# paths 			LxM Array, dass die k?rzesten Pfade als Listen enth?lt
# Author: Joseph Kirk (in Matlab), in R implementiert durch: Florian Lerch
# 1. Editor: MT 11/2014
# 2. Editor: MT 01/2015: Algorithmus bricht ab, wenn nicht von jedem Punkt zu jedem Punkt min. 1 Pfad exisitert 
# 3. Editor: FL 06/2015: Algorithmus bricht jetzt nicht mehr ab, wenn eine Verbindung nicht existiert
# 4. Editor: MT 08/2016:  hilfsfunktionen durch Rinterne funktionen ersetzt (zeros->matrix, error->stop)
  
	# Verarbeite die Eingabe
	processInputs = function(AorV,xyCorE){
		C = matrix(0,n,n)
    
		# H?he und Breite von AorV muss gleich sein
		if(n == nc){
			# H?he und Breite der Kostenmatrix sind gleich
			if( m == n){
				# Falls AorV und Kostenmatrix die selben Dimensionen haben
				if(m == mc){
					A = AorV
					# setze die Diagonale von A auf 0
					A = A - diag(diag(A))
					C = xyCorE   
					all_positive = TRUE
					# f?r alle Zeilen und Spalten
					for(i in 1:nrow(A))
					for(j in 1:ncol(A)) 
					# wenn Punkt nicht 0, also i und j verbunden
					if(A[i,j] != 0)
						# wenn Kosten f?r den Pfad nicht gr??er 0  -> es sind nicht alle Kosten positiv
						if( ! (C[i,j] > 0) ) all_positive = FALSE
						# Mache aus A eine Matrix mit Kanten und Gewichtungen
						E = a2e(A)
				}
				# AorV und Kostenmatrix haben unterschiedlich viele Dimensionen
				else {
					A = AorV
					# setze die Diagonale von A auf 0
					A = A - diag(diag(A))
					xy = xyCorE
					# Mache aus A eine Matrix mit Kanten und Gewichtungen
					E = a2e(A)
					# bilde die eukklidischen Distanzen
					D = ve2d(xy,E)
					all_positive = all(D > 0)	

					for(row in 1:length(D))
						C[E[row,1],E[row,2]] = D[row]
				}
			} # end if( m == n )
			else stop("Invalid [A,xy] or [A,cost] inputs.")
		} # end if(n == nc)

		# H?he und breite der Kostenmatrix sind nicht gleich
		else{
			# xyCorE ist die Matrix der Kanten ohne Gewichtungen (E)
			if(mc == 2){
				V = AorV
				E = xyCorE

				# Kosten der einzelnen Kanten in E durch euklidische Distanz
				D = ve2d(V,E)
				all_positive = all(D > 0)

				# f?r jede Kante in E wird die zugeh?rige euklidische Distanz in der Kostenmatrix gespeichert
				for(row in 1:m)
					C[E[row,1],E[row,2]] = D[row]
			}
			# xyCorE ist die Matrix der Kanten mit Gewichtungen (E3)
			else if(mc == 3){
				E3 = xyCorE
				all_opositive = all(E3 > 0)

				# E entspricht den ersten beiden spalten von E3
				E = E3[,c(1,2)]

				# f?r jede Kande in E wird die zugeh?rige euklidische Distanz in der Kostenmatrix gespeichert
				for(row in 1:m)
					C[E3[row,1],E3[row,2]] = E3[row,3]
			}
			else stop('Invalid [V,E] inputs.')
		} # end else 
		return( list(E,C) )
	} # end process_inputs

	# Convert Adjacency Matrix to Edge List
	a2e = function(A){
		E = which(A != 0, arr.ind = T,useNames = FALSE)

		return(E)
	} # end a2e

	# Compute Euclidean Distance for Edges
	ve2d = function(V,E){
		# Liste der Koordinaten der Startpunkte von Kanten
		VI = V[E[,1], ]
		# Liste der Koordinaten der Endpunkte von Kanten
		VJ = V[E[,2], ]

		# die euklidischen Distanzen zwischen den Punkten, also L?nge der einzelnen Kanten
		D = sqrt(rowSums((VI - VJ) * (VI - VJ)))
		return(D)
	} # end ve2d

  # -------------------------------------------------------------------
	progress = 0 # Speichert den prozentualen Fortschritt
  	# Alle Kosten f?r Pfade sind positiv
	all_positive = 1

	# lese Dimensionen der Matrizen aus	
	n = dim(AorV)[1]
	nc = dim(AorV)[2]

	m = dim(xyCorE)[1]
	mc = dim(xyCorE)[2]
  
	# generiere eine Kantenmatrix E und Kostenmatrix cost
	r = processInputs(AorV, xyCorE)
	E = r[[1]]
	cost = r[[2]]

	# Liste der Start und Endpunkte initialisieren
	if (is.null(SID)) SID = 1:n
	if (is.null(FID)) FID = 1:n

	if(max(SID) > n || min(SID) <1) stop("Invald [SID] input.")
	if(max(FID) > n || min(FID) < 1) stop("Invalid [FID] input.")

	# falls mehr Endpunkte als Startpunkte, vertausche diese
	isreversed = 0
	if(length(FID) < length(SID)){
		# vertausche auch die ersten beiden Spalten der Kantenmatrix
		E = cbind(E[,2] , E[,1])
		cost = t(cost)
		tmp = SID
		SID = FID
		FID  = tmp
		isreversed = 1
	}

	L = length(SID)
	M = length(FID)

	costs = matrix(0, L, M)
	# LxM Array dessen Elemente leere Listen sind
	paths = array(list(NaN), c(L,M))

	if(verbose) print('Dijkstra begonnen')
  
  if(all_positive) emptyMatrix = matrix(0, 1, n)
  else emptyMatrix = matrix(NaN,1,n)
  emptyArray = array(list(NaN), c(1,n))
  

  
  #Alle Starktpunkte und alle Endpunkte der Kanten
  startPoints = E[,1]
  endPoints = E[,2]
  
  # bestimme alle Indizes von Kanten, die mit einem bestimmten Punkt starten
	indexOfEdgesWithStartingPoint = array(list(NaN), c(1,L))
	for(k in 1 : L){
	  indexOfEdgesWithStartingPoint[[k]] = which(startPoints == k)
	}
  
	# Finde die kleinsten Kosten und Pfade durch den Dijkstra Algorithmus  
	# iteriere durch alle Startpunkte
  if(waitbar){
    Tacho <- waitbar(0,'Processing Details')
  }
  
	for(k in 1:L){
	  #PROFILING: Rprof("out.out", line.profiling=TRUE)
    
		# wenn alle Pfadkosten positiv sind, initialisieren mit 0, ansonsten mit NaN als Platzhalter
	  TBL = emptyMatrix

		# setze minimale Kosten auf Unendlich als Platzhalter
		min_cost = matrix(Inf,1,n)
		# setze abgearbeitete Punkte auf 0
		settled = matrix(0, 1, n)
		# f?r jeden abgearbeiteten Punkt, m?ssen n Pfade (Listen) gefunden werden
		path = emptyArray
    
		I = SID[k]
		min_cost[I] = 0
		TBL[I] = 0

		# Pfad von Punkt zu sich selbst ist trivial
		settled[I] = 1
		path[[I]] = list(I)
    
		# alle Kanten die mit dem Punkt I beginnen
		nids = indexOfEdgesWithStartingPoint[[I]]
    endPointsForI = endPoints[nids]
    
		# solange noch Endpunkte existieren, die nicht erreicht wurden
		while( any(settled[FID] == 0) ){
  			# Update the Table
  			TAB = TBL
  			# wenn alle Pfadkosten positiv sind, setze Pfadkosten auf 0, andernfalls auf NaN als Platzhalter
  			if(all_positive) TBL[I] = 0
  			else TBL[I] = NaN	
  			
			# Calculate the Costs to the Neighbor Points and Record Paths
			# alle Endpunkte der Kanten die mit I beginnen
      for(J in endPointsForI){
        
				# falls der Endpunkt noch nicht abgearbeitet ist
				if(settled[J] == 0){
					# Kosten f?r den Pfad
					c = cost[I,J]

					# empty = False falls f?r den Endpunkt bereits ein Kostenwert vorhanden ist
					if(all_positive) empty = !TAB[J]
					else empty = is.nan(TAB[J])

					# Falls noch kein Kostenwert vorhanden ist, oder der neue kleiner ist
					if(empty || (TAB[J] > (TAB[I] + c))) {
						# setze neuen Kostenwert
						TBL[J] = TAB[I] + c

						# Speichere den neuen Pfad
						if(isreversed) path[[J]] = c(list(J), path[[I]])
						else path[[J]] = c(path[[I]], list(J))
					}
					# der letzte Kostenwert bleibt
					else TBL[J] = TAB[J]
  			}
  		} # for (kk in 1:length(nids))
      
  		# alle Verbindungen die gefunden wurden (0 oder NaN wurde ?berschrieben)
  		if(all_positive) K = which(TBL!=0)
  		else K = which(! is.nan(TBL) )

  		# Find the Minimum Value in the Table
      
      # Falls kein Pfad zu diesem Punkt existiert, breche den Durchlauf hier ab
      if(length(TBL[K])==0) break
  		N = which( TBL[K] == min(TBL[K])  )
      
  		# Settle the Minimum Value
			if (length(N) == 0) break
			else {
				I = K[N[1]]
				min_cost[I] = TBL[I]
				settled[I] = 1
        
        # Die Indizes aller Kanten die mit I beginnen
        nids = indexOfEdgesWithStartingPoint[[I]]
				endPointsForI = endPoints[nids]
			}
		} # end while (any(settled[FID] == 0))
    
		# Store Costs and Paths
		costs[k,] = min_cost[FID]
		paths[k,] = path[FID]
    
    
    if(verbose){
      newProgress = round(k/L*100)
        if(waitbar){
            Tacho <- waitbar(newProgress,Header='Dijkstra',Tacho)
      }else{
      if(progress != newProgress) print(sprintf('Dijkstra: %d %%',newProgress))
    }
      progress = newProgress
    }
    
		#PROFILING: Rprof(NULL)
		#PROFILING: print(summaryRprof("out.out", lines="show"))
    
	} # end for (k in 1 : L)
  if(waitbar){
  close(Tacho) 
  }
  if(verbose) print('Dijkstra beendet')
  
	if(isreversed){
		costs = t(costs)
		paths = t(paths)
	}

	if(L == 1 && M == 1) paths = paths[[1]]
	return(list(costs=costs,paths=paths))
}
