*
* $Id: ardbrb.F,v 1.1.1.1 1996/03/08 16:51:04 mclareni Exp $
*
* $Log: ardbrb.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:04  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: ardbrb.F,v 1.1.1.1 1996/03/08 16:51:04 mclareni Exp $
 
      SUBROUTINE ARDBRB(DTHE,DPHI,DBEX,DBEY,DBEZ,NI,I)
 
C...ARiadne subroutine DouBle precision ROtate and BOost
 
C...Rotates and boost NI particles in /ARPART/ using double precision
C...angles.
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),QEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),INQ(MAXPAR),
     $                XPMU(MAXPAR),XPA(MAXPAR),PT2GG(MAXPAR),IPART
      SAVE /ARPART/
 
      DIMENSION I(NI),BR(3,3),BV(3),DP(4)
 
 
      IF (DTHE**2+DPHI**2.GT.1.0D-20) THEN
 
C...Rotate (typically from z axis to direction theta,phi)
 
        DSP=SIN(DPHI)
        DCP=COS(DPHI)
        DST=SIN(DTHE)
        DCT=COS(DTHE)
 
        BR(1,1)=DCT*DCP
        BR(1,2)=-DSP
        BR(1,3)=DST*DCP
        BR(2,1)=DCT*DSP
        BR(2,2)=DCP
        BR(2,3)=DST*DSP
        BR(3,1)=-DST
        BR(3,2)=0.0
        BR(3,3)=DCT
 
        DO 100 IJ=1,NI
          DO 110 J=1,3
            BV(J)=BP(I(IJ),J)
 110      CONTINUE
          DO 120 J=1,3
            BP(I(IJ),J)=BR(J,1)*BV(1)+BR(J,2)*BV(2)+BR(J,3)*BV(3)
 120      CONTINUE
 100    CONTINUE
 
      ENDIF
 
      DBTOT2=DBEX**2+DBEY**2+DBEZ**2
      IF (DBTOT2.GT.1.0D-20) THEN
        IF (DBTOT2.GE.1.0D0) CALL ARERRM('ARROBO',14,0)
        DGA=1.0D0/DSQRT(1.0D0-DBTOT2)
 
        DO 200 IJ=1,NI
          DO 210 J=1,4
            DP(J)=BP(I(IJ),J)
 210      CONTINUE
          DBEP=DBEX*DP(1)+DBEY*DP(2)+DBEZ*DP(3)
          DGABEP=DGA*(DGA*DBEP/(1.0D0+DGA)+DP(4))
 
          BP(I(IJ),1)=DP(1)+DGABEP*DBEX
          BP(I(IJ),2)=DP(2)+DGABEP*DBEY
          BP(I(IJ),3)=DP(3)+DGABEP*DBEZ
          BP(I(IJ),4)=DGA*(DP(4)+DBEP)
 
 200    CONTINUE
 
      ENDIF
 
      RETURN
 
C**** END OF ARDBRB ****************************************************
      END
