*
* $Id: delsq.F,v 1.1.1.1 1996/03/22 16:42:46 mclareni Exp $
*
* $Log: delsq.F,v $
* Revision 1.1.1.1  1996/03/22 16:42:46  mclareni
* Phtools
*
*
      FUNCTION DELSQ (K1,K2,K3,K4,K5,K6)
C--  CALLED FROM --  USER(OPTIONAL)
C--  PURPOSE -- CALCULATES THE SQUARE OF THE FOUR-MONENTUM TRANSFER
C               FROM THE TARGET PARTICLE TO THE THE GROUP OF PARTICLES
C               WHOSE INDICES ARE INDICATED AS ARGUMENTS
C               **  ALL MOMENTA (PCM)  ARE ASSUMED TO BE IN THE CENTER
C               OF MASS SYSTEM BEFORE THE CALL
      COMMON/UTIL/PLACE(150),WEIGHT(150),NP,TECM,AMASS(18),PCM(5,18),WT
     +/BEAM  / PRM       ,TARGM    ,PBM      ,ETA      ,GAM      ,DPBM
      DIMENSION K(6)
      PXTOT = 0.0
      ETOT = 0.0
      IF(K1.LE.0) GOTO 450
      N = 1
      K(1) = K1
      IF(K2.LE.0) GOTO 400
      N = 2
      K(2) = K2
      IF(K3.LE.0) GOTO 200
      N = 3
      K(3) = K3
      IF(K4.LE.0) GOTO 200
      N = 4
      K(4) = K4
      IF(K5.LE.0) GOTO 200
      N = 5
      K(5) = K5
      IF(K6.LE.0) GOTO 200
      N = 6
      K(6) = K6
  200 CONTINUE
      DO  250  J1= 1, N
      JJ = K(J1)
      PXTOT = PXTOT + PCM(1, JJ)
  250 ETOT  =  ETOT + PCM(4,  JJ)
      ETOT = ETA*PXTOT + GAM*ETOT
      EM1 = TARGM*TARGM
      EM2 = FMASSQ(K1,K2,K3,K4,K5,K6)
      DELSQ = 2.0 *ETOT *TARGM - EM1 - EM2
      GOTO 500
  400 CONTINUE
      ETOT = ETA*PCM(1,K1) + GAM*PCM(4,K1)
      DELSQ = 2.0*ETOT*TARGM -AMASS(K1)*AMASS(K1) - TARGM*TARGM
      GOTO 500
  450 DELSQ = 0.0
  500 RETURN
      END
