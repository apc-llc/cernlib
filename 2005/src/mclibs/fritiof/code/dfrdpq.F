*
* $Id: dfrdpq.F,v 1.1.1.1 1996/01/11 14:05:20 mclareni Exp $
*
* $Log: dfrdpq.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:20  mclareni
* Fritiof
*
*
C********************************* END FRPSOFT ***************************
 
C********************************* FUNCTION FRDPQ *************************
 
      DOUBLE PRECISION FUNCTION DFRDPQ(DPMIN,DPMAX,P0)
 
C......generate PQ (P- or P+ ) according to dPQ/PQ+P0,
C......with Pmin< PQ <Pmax.
 
      IMPLICIT DOUBLE PRECISION (D)
      PARAMETER (KSZ1=20)
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      SAVE /FRPARA1/
 
      DP0 = DBLE(P0)
      IF(DPMIN+DP0.LE.0.) THEN
      DFRDPQ = 0.
      RETURN
      ENDIF
 
        IF(DPMAX.LE.DPMIN) THEN
      DFRDPQ = DPMIN
      ELSE
      DR = DBLE(RLU(0))
      DFRDPQ = (DPMIN+DP0) *((DPMAX+DP0)/(DPMIN+DP0))**DR -DP0
      ENDIF
 
      RETURN
      END
