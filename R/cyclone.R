
###################################################################################
#' Cyclone Simulation: Barth/Muschelknautz
#'
#' Calculate cyclone collection efficiency according to Barth/Muschelknautz.
#'
#' @param cyclone list of cyclone's geometrical parameters
#' @param fluid list of fluid parameters
#'
#' @return returns a function that calculates the fractional efficiency for the specified diameter, see example.
#'
#' @examples
#' ## get function for specified cyclone and fluid type
#' fun <- calculationBarthMuschelknautz(
#'        cyclone=list(Da=1260,H=2500,Dt=420,Ht=640,Epsilon=13.1340,He=600,Be=200),
#'        fluid=list(Mu=18.5*10^(-6),Vp=5000,Rhop=2000,Rhof=1.204,Croh=50))[[1]]
#' ## specify diameters to be calculated
#' x<-seq(from=0,to=35,by=0.01)*1e-6 
#' ## compute fractional efficiency and plot
#' plot(x,fun(x),type="l",xlab=expression(paste("Particle Size [", mu, "m]")),
#'      ylab="Fractional Efficiency")
#'
#' @keywords internal
#' @export
###################################################################################
calculationBarthMuschelknautz <- function(cyclone, fluid){
	#---------------------------------------------
	#Umrechnung Geometriedaten fuer Modell Barth / geometric data
	#-------------------------------------------
	ra <- cyclone$Da/(2*1000) 				#Zyklonaussenradius /cyclone radius [m]
	ri <- cyclone$Dt/(2*1000)  			#Tauchrohrradius /vortex finder radius [m]
#unused	rx <- ri	       			#Staubaustragsradius / discharge duct radius [m]
#unused	hi <- ((cyclone$H-cyclone$Ht)/1000)   
	h <- (cyclone$H/1000)        	      	#Gesamth?he Zyklon / total cyclone height [m]
	ht <- (cyclone$Ht/1000)       	    		#Tauchrohreintauchtiefe / [m] Parameter 8.4
#unused	epsilon <- cyclone$Epsilon*pi/180  		#Umrechnung in Bogenmass / calculation angle radians            
#unused	hc <- cyclone$Hc							#Hoehe konischer Teil / heigth conical section [m]
	he <- (cyclone$He/1000)       			#Schlitzeinlauhoehe in m / inlet section height [m]
	be <- (cyclone$Be/1000)       			#Schlitzeinlauhoehe in m / inlet section width  [m]
#unused	hk <- (h-he)								#Hoehe konischer Teil / heigth cylindrical section [m]
	  
  vp <- (fluid$Vp/3600)     			#Umrechnung Volumenstrom / calculation volumetric gas flow rate [m/s]
	croh <- fluid$Croh
	rhof <- fluid$Rhof 
	rhop <- fluid$Rhop
	mu <- fluid$Mu

	BE <- be/ra
	re <- ra-be/2  

	Fe <- be*he	            		#Flaeche Schlitzeinlauf  (Eintrittsquerschnitt) / inlet area                
	Fi <- (pi*ri^2)   				#Tauchrohrquerschnittsflaeche (Austrittsquerschnitt) / 
	F <- (Fe/Fi)            			#Verhaeltnis von Eintrittsflaeche Fe zu Tauchrohr     

	B <- croh*10^(-3)/rhof		  	# Gutbeladung 
	lambda_g <- 0.005 
	lambda <- lambda_g*(1+2*sqrt(B))  	# Wandreibungswert
	alpha <- 1.0-(0.54-0.153/F)*BE^(1/3)    
#onlyusedbyunused	ve <- (vp/Fe)	                	# Einlaufgeschwindigkeit 
	vi <- (vp/(pi*ri^2)) 
#onlyusedbyunused	vphia <- ve*((re/ra)*(1/alpha)) 
	vr <- (vp/(2*pi*ri*(h-ht)))    	#Radialgeschwindigkeit am Innenzylinder / radial gas velocity
	U <- 1/(F*alpha*ri/re+lambda*h/ri)   #Umfangsgeschwindigkeit auf dem Aussenradius Gl. 2.18 
	vphii <- U*vi  

  xGr <- (sqrt((18*mu*vr*ri)/((rhop-rhof)*vphii^2)))#*1e6;# Grenzkorn Gl. 2.4 Seite

#onlyusedbyunused	x50 <- xGr*1.3
#unused	BGr <-(lambda*mu*sqrt(ra*ri))/ ((1-ri/ra)*rhop*(x50)^2*sqrt(vphia*vphii)) #Grenzbeladung / critical load 

	Tf <-  function(x){  
			  f <-(1+2/(x/xGr)^(3.564))^(-1.235);  
			  return(f)    
			  }
	#I <- integrate(Tf,xmin,xmax)
	#E <- 1-BGr/BE+(BGr/B)*I*delta				#Gesamtabscheidegrad

#unused	xi1 <- 0
	xi2 <- (U^2*(ri/ra))*(1-lambda*(h/ri)*U)^(-1)	# Widerstandsbeiwert Einlaufzone zwischen 1 und 5
	xi3 <- 2+3*U^(4/3)+U^2   				# Widerstandsbeiwert Einlaufzone zwischen 10 und 50
	deltaP <-(rhof/2)*vi^2*(xi2+xi3)			# N/m^2 = 16.99 hPa

	Calculation <- c(Tf,deltaP)
	return(Calculation)
}


###################################################################################
#' Objective function - Cyclone Simulation: Barth/Muschelknautz
#'
#' Calculate cyclone collection efficiency. A simple, physics-based
#' optimization problem (potentially bi-objective). See the references [1,2].
#'
#' @param x vector of length at least one and up to six, specifying non-default geometrical parameters: Da, H, Dt, Ht, He, Be
#' @param deterministic binary vector. First element specifies whether volume flow is deterministic or not. Second element specifies whether particle density is deterministic or not. Third element specifies whether particle diameters are deterministic or not. Default: All are deterministic (TRUE).
#' @param cyclone list of a default cyclone's geometrical parameters: fluid$Da, fluid$H, fluid$Dt, fluid$Ht, fluid$He and fluid$Be
#' @param fluid list of default fluid parameters: fluid$Mu, fluid$Vp, fluid$Rhop, fluid$Rhof and fluid$Croh
#' @param noiseLevel list of noise levels for volume flow (noiseLevel$Vp) and particle density (noiseLevel$Rhop), only used if non-deterministic.
#'
#' @return returns a function that calculates the fractional efficiency for the specified diameter, see example.
#'
#' @references 
#' [1] Zaefferer, M.; Breiderhoff, B.; Naujoks, B.; Friese, M.; Stork, J.; Fischbach, A.; Flasch, O.; Bartz-Beielstein, T. Tuning Multi-objective Optimization Algorithms for Cyclone Dust Separators Proceedings of the 2014 Conference on Genetic and Evolutionary Computation, ACM, 2014, 1223-1230 \cr\cr
#' [2] Breiderhoff, B.; Bartz-Beielstein, T.; Naujoks, B.; Zaefferer, M.; Fischbach, A.; Flasch, O.; Friese, M.; Mersmann, O.; Stork, J.; Simulation and Optimization of Cyclone Dust Separators Proceedings 23. Workshop Computational Intelligence, 2013, 177-196
#'
#' @examples
#' ## Call directly
#' funCyclone(c(1260,2500))
#' ## create vectorized target funcion, vectorized, first objective only
#' ## Also: negated, since SPOT always does minimization.
#' tfunvecF1 <-function(x){-apply(x,1,funCyclone)[1,]}
#' tfunvecF1(matrix(c(1260,2500,1000,2000),2,2,byrow=TRUE))
#' ## optimize with spot
#' res <- spot(fun=tfunvecF1,lower=c(1000,2000),upper=c(2000,3000),
#'    control=list(modelControl=list(target="ei"),
#'    model=buildKriging,optimizer=optimLBFGSB,plots=TRUE)) 
#' ## best found solution ...
#' res$xbest
#' ## ... and its objective function value
#' res$ybest
#'
#' @export
###################################################################################
funCyclone <- function(x,deterministic=c(T,T,T),
					cyclone=list(Da=1260,H=2500,Dt=420,Ht=650,He=600,Be=200),
					fluid=list(Mu=18.5*10^(-6),Vp=5000,Rhop=2000,Rhof=1.204,Croh=50),
					noiseLevel=list(Vp=0.1,Rhop=0.05)){
	#--------------------------------------------------------------
	#Geometriedaten / geometric data
	#--------------------------------------------------------------
	#Da <- 1260				      # Zyklondurchmesser [mm]/ cyclone diameter [mm] Parameter 8.1
	#H <- 2500          		  # Zyklongesamthoehe  [mm] / total cyclone height [mm] Parameter 8.2
	#Dt <- 420          		  # Tauchrohrinnendurchmesser [mm] / vortex finder diameter [mm] Parameter 8.3
	#Ht <- 640          		  # Tauchrohreintauchtiefe / [mm] Parameter 8.4
	#Xt <- 0            		  # Tauchrohrposition / position vortex finder
	#Phi <- 0             		# Tauchrohrposition / position vortex finder[degree] Parameter 8.5
	#Tt <- 0	           		  # Tauchrohrwanddicke [mm] / thickness wall of vortex finder [mm] Parameter 8.6
	#Epsilon <- 13.1340 		  # Konusneigungwinkel [grad] / angle of cyclone cone [degree] Parameter 8.7 old
	#Hc <- 1900 		          # Konushoehe [grad] / height of cyclone cone [degree] Parameter 8.7 NEW!
	#Dx <- 0              		# Staubaustragsdurchmesser / discharge duct diameter [mm] Parameter 8.8
	#He <- 600           		# Schlitzeinlaufhoehe [mm] ]/inlet section height [mm] Parameter 8.9
	#Be <- 200          		  # Schlitzeinlaufhoehe [mm] / inlet section width [mm] Parameter 8.10
	#	Alpha <- 0         		# Einlaufwinkel [grad] / [degree] Parameter 8.11 
	#---------------------------------------------
	#Betriebsdaten / fluid parameters
	#-------------------------------------------
	#Mu <- 18.5*10^(-6)		  # Viskositaet / viscosity Zaehigkeit Luft
	#Vp <- 5000              # Volumenstrom in m/h / volumetric gas flow rate [m^3/h]
	#Rhop <- 2000.0          # Partikeldichte in kg/m^3 / particle density [kg/m^3]
	#Rhof <- 1.204        	  # Gasdichte kg/m^3
	#Croh <- 50              # Rohgas Konzentration in g / l
	#--------------------------------------------------------------
	if(is.na(x[[1]]))return(rep(NA,2))
	cyclone$Da<-x[1] #Da in mm
	if(length(x)>1)cyclone$H<-x[2]
	if(length(x)>2)cyclone$Dt<-x[3]
	if(length(x)>3)cyclone$Ht<-x[4]
	#if(length(x)>4)cyclone$Hc<-x[5]
	if(length(x)>4)cyclone$He<-x[5]
	if(length(x)>5)cyclone$Be<-x[6]
	#--------------------------------------------------------------
	fluid <-list(Mu=fluid$Mu,
				Vp=if(deterministic[1]) fluid$Vp else fluid$Vp*(1-noiseLevel$Vp) + noiseLevel$Vp * fluid$Vp * 2 * runif(1),
				Rhop=if(deterministic[2]) fluid$Rhop else fluid$Rhop*(1-noiseLevel$Rhop) + noiseLevel$Rhop * fluid$Rhop * 2 * runif(1), #note: NEVER lower than Rhof, else sqrt(-) in model
				Rhof=fluid$Rhof,
				Croh=fluid$Croh)

	#--------------------------------------------------------------
	C <- calculationBarthMuschelknautz(cyclone, fluid)
	CollectionEfficiency <- C[1]
	PressureDrop <- C[2]
	#--------------------------------------------------------------
	xmin <- c(0,2,4,6,8,10,15,20)*1e-6 #TODO these values are completely arbitrary atm...
	xmax <- c(2,4,6,8,10,15,20,30)*1e-6
	delta <- c(0.0,0.02,0.03,0.05,0.1,0.3,0.3,0.2) #TODO sum should be 1 ?
	
	#--------------------------------------------------------------
	if(deterministic[3])
		xmean <- c(1,3,5,7,9,12.5,17.5,25)*1e-6
	else
		xmean <- xmin + runif(length(xmin)) * (xmax - xmin)
	E<- sum(CollectionEfficiency[[1]](xmean)*delta)
	
	as.numeric(c(PressureDrop,-E)) #first target: minimal pressure drop, second target: maximal col.eff. (inverted for minimization)
}

