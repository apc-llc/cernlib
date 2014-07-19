      SUBROUTINE MLPFIT(NPAT,X,Y,EY,NNeu1,NNeu2,init,init0,iprin,nepmax)
C
C     routine called by pafitv for fit of a vector with a MLP like function
C
C     J.Schwindling 26-APR-99      
C
C     Modified 08-JUN-99: possibility to have 2 hidden layers
C 
#include "hbook/hcfits.inc"     
      REAL X(*),Y(*),EY(*)
      INTEGER NNEUR(4) 
      INTEGER NLAYER 
      REAL RIN(1),RANS(1),POND(1)
      REAL mlpepoch
      INTEGER ndf, nweights
C
      LMET = 6 		! BFGS learning method
C
      print *,'     **********************************************'
      print *,'     *                                            *'
      print *,'     *   fit using a Multi-Layer Perceptron       *'
      print *,'     *      (BFGS minimization method)            *'
      print *,'     *                                            *'
      print *,'     **********************************************'
      print *
      IF(Nneu2 .EQ. 0) THEN
         print *,' Number of hidden neurons = ',nneu1,'    -> ',
     >        3*nneu1+1,' parameters'
         Nweights = 3*nneu1+1
      ELSE
         print *,' Number of hidden neurons = ',nneu1,nneu2,'    -> ',
     >        (nneu2+2)*nneu1+2*nneu2+1,' parameters'
         Nweights = (nneu2+2)*nneu1+2*nneu2+1
      ENDIF
      print * 
      ndf = npat-nweights
C
C     define network and learning
C
      nneur(1) = 1
      nneur(2) = nneu1
      IF(nneu2 .EQ. 0) THEN
         nneur(3) = 1 
	 nlayer = 3 
      ELSE
         nneur(3) = nneu2
	 nneur(4) = 1
	 nlayer = 4
      ENDIF
      ierr = mlpsetnet(nlayer,nneur)
      IF(ierr.NE.0) THEN
         PRINT *,'** error while setting the network'
	 RETURN
      ENDIF	  
      ierr = mlpsetlearn(LMET,0.1,.992,0.1,0.,50,3.,1.)
      IF(ierr.NE.0) THEN
         PRINT *,'** error while setting the learning parameters'
	 RETURN
      ENDIF	  
C
C     set examples
C           
      ierr = mlpsetnpat(0,Npat,1,1,1)
      ierr = mlpsetnpat(1,Npat,1,1,1)
      PondSum = 0.
      DO Ipat = 1,Npat
         Rin(1) = X(Ipat)
	 Rans(1) = Y(Ipat)
	 Pond(1) = 1.
	 if(abs(EY(Ipat)).gt.0.000000001) Pond(1) = 1./(EY(Ipat)**2)
	 PondSum = PondSum + Pond(1)
	 ierr = mlpsetpat(0,Ipat,Rin,Rans,Pond)
	 ierr = mlpsetpat(1,Ipat,Rin,Rans,Pond)
      ENDDO
C
C     initialize weights
C
      IF(INIT.EQ.1) THEN
         IINIT = 1
	 IF(INIT0.EQ.1) IINIT = 0
         CALL mlpinitw(IINIT)
      ENDIF 
C
C     minimization
C    
      NRESTART = 0
 10   CONTINUE
      Nep = 0
      DO Iepoch = 1,Nepmax
         Err = mlpepoch(Iepoch)
         Nep = Nep + 1
	 if(iprin.ge.1) print *,Iepoch,Err
      ENDDO
      print *,' Chisquare = ',Err,' (for ',ndf,
     > ' degrees of freedom) after ',Nep,' epochs'
      FITCHI = Err
C
C     if chisquare to bad, automatic restart
C      
      IRESTART = 0
      IF(ndf .ne. 0) THEN
         IF(ERR/NDF .GT. 10 .AND. NRESTART.LT.4) IRESTART = 1
      ELSE
         IF(ERR .GT. 10 .AND. NRESTART.LT.4) IRESTART = 1
      ENDIF	
      IF(IRESTART .EQ. 1) THEN
         CALL mlpinitw(0)
	 NRESTART = NRESTART + 1
	 GOTO 10
      ENDIF
      CALL mlpfree
      CALL mlpprpawf
C                                                   
      END 
