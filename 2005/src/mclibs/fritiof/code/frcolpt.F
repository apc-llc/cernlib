*
* $Id: frcolpt.F,v 1.1.1.1 1996/01/11 14:05:20 mclareni Exp $
*
* $Log: frcolpt.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:20  mclareni
* Fritiof
*
*
C********************************* END FRVECTC **************************
 
 
C********************************* FRCOLPT ******************************
 
      SUBROUTINE FRCOLPT(PK2M,PX,PY,PX0,PY0)
 
C-----------------------------------------------------------------------
C     GIVING THE EXCITED NUCLEONS gaussian PT:
C      dP ~ exp(-(Px-Px0)**2/sig)*exp(-(Py-Py0)**2/sig)
C     PK2M: the upper cut off for PT^2.
C-----------------------------------------------------------------------
 
      PARAMETER (KSZ1=20)
      implicit DOUBLE PRECISION (D)
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      SAVE /FRPARA1/
 
      PX = 0.
      PY = 0.
      PK0 = PX0**2+PY0**2
      P2MX = PK2M-PK0
      IF(P2MX.LE.1.E-5) RETURN
 
      IF(VFR(6).LE.0.000001) THEN
      PX = PX0
      PY = PY0
 
      ELSE
 
      ITRY = 0
10    CALL FRGAUSS(P, VFR(6), P2MX)
 
      Adelpt=FRSQR(p, 'PIO086')
 
      AFI=2*3.1415926*RLU(0)
      PX=PX0+ADELPT*COS(AFI)
      PY=PY0+ADELPT*SIN(AFI)
        IF(PX**2+PY**2.GT.PK2M) THEN
        ITRY = ITRY+1
        CALL FRLOOPU(*10,ITRY,100,'LPfrcolpt')
        ENDIF
 
      ENDIF
 
      RETURN
      END
