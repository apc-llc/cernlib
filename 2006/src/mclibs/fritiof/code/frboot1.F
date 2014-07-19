*
* $Id: frboot1.F,v 1.1.1.1 1996/01/11 14:05:22 mclareni Exp $
*
* $Log: frboot1.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:22  mclareni
* Fritiof
*
*
C********************************* END FRROTAZ ************************
 
C********************************* FRBOOT1 ****************************
 
      SUBROUTINE FRBOOT1(ID,DPV,DBETA)
 
C... TO BOOST AN SINGLE MOMENTA BY A DBETA(3) FACTOR.
C... ID =0, DPV(1-4)=P_X, P_Y, P_Z, E;
C... ID =1, DPV(1-4)=P_X, P_Y, P_, P+
 
      IMPLICIT DOUBLE PRECISION (D)
      DIMENSION DBETA(3), DPV(4), DRV(4)
 
      DBET2 = 0.D0
      DO 10 J = 1, 3
10    DBET2 = DBET2 + DBETA(J)**2
      IF(DBET2.LT.1.D-10) RETURN
      IF(DBET2.GT.1.D0) THEN
      CALL FRMGOUT(0,1,' FRBOOT1: CHECK BETA > 1',
     >    REAL(DBETA(1)),REAL(DBETA(2)),REAL(DBETA(3)),REAL(DBET2),0.)
      ENDIF
      DBET = DSQRT(DBET2)
 
        IF(DBET.GT.0.99999999D0) THEN
      DO 13 J=1, 3
13    DBETA(J) = DBETA(J)* 0.99999999D0/DBET
        DBET=0.99999999D0
      DBET2 = DBET**2
        ENDIF
 
      DGAMA = 1.D0/DFRSQR(1.D0-DBET2, 'UIOP09')
        DEFF = DGAMA/(1.D0+DGAMA)
 
      DRV(1) = DPV(1)
      DRV(2) = DPV(2)
      IF(ID.EQ.0) THEN
      DRV(3) = DPV(3)
      DRV(4) = DPV(4)
      ELSE
      DRV(3) = (DPV(4) - DPV(3))/2D0
      DRV(4) = (DPV(4) + DPV(3))/2D0
      ENDIF
 
       DBP = 0.
       DO 25 I = 1, 3
25     DBP = DBP + DBETA(I)* DRV(I)
 
C..........................................BOOST..............
      DO 30 J = 1, 3
  30    DRV(J) = DRV(J) +(DEFF*DBP -DRV(4))*DGAMA*DBETA(J)
      DRV(4) = DGAMA* (DRV(4)-DBP)
 
      DPV(1) = DRV(1)
      DPV(2) = DRV(2)
      IF(ID.EQ.0) THEN
      DPV(3) = DRV(3)
      DPV(4) = DRV(4)
      ELSE
      DPV(3) = (DRV(4) - DRV(3))
      DPV(4) = (DRV(4) + DRV(3))
      ENDIF
 
 
      RETURN
      END
