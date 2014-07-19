*
* $Id: arecoi.F,v 1.1.1.1 1996/03/08 16:51:03 mclareni Exp $
*
* $Log: arecoi.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:03  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arecoi.F,v 1.1.1.1 1996/03/08 16:51:03 mclareni Exp $
 
      REAL FUNCTION ARECOI(BE0,DE1,DE2,DE3,BP0,DP1,DP2,DP3,BALP,PT12)
 
C...Ariadne function RECOIl
 
C...Calculates the angle of a recoil gluon according to the new
C...Recoil strategy: p_t1^2*exp(-y_1)=p_t2^2*exp(-y_2)
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE /LUDAT1/
 
C...Calculate the maximum and minimum angle
      PHIL=0.0
      PHIU=PARU(1)-BALP
 
C...Calculate angle of recoil gluon
      BW=DE1+DE2+DE3
      BS=BW**2
      BM3=DE3**2-DP3**2
      S0123=(BW+BE0)**2-BP0**2
      S12=BS-2.0*BW*DE3+BM3
      S23=BS-2.0*BW*DE1
      S13=BS-2.0*BW*DE2
      D01=2.0*S12*DE1*BE0
      D02=2.0*S12*DP1*BP0
      D03=PT12*(S0123-S13-S23+BM3-2.0*DE3*BE0)
      D04=2*DP3*BP0*PT12*COS(BALP)
      D05=2*DP3*BP0*PT12*SIN(BALP)
      D11=D01-D03
      D12=D05
      D13=D04+D02
      D21=(D11**2-D13**2)/(D12**2+D13**2)
      D22=D12*D11/(D12**2+D13**2)
      D31=D22**2-D21
      DSPHI=SQRT(MAX(D31,0.0D0))-D22
      IF (DSPHI.LT.0.0D0) THEN
        PHI=PHIL
      ELSEIF (DSPHI.GE.1.0D0) THEN
        PHI=PHIU
      ELSE
        PHI=MIN(ASIN(DSPHI),DBLE(PARU(1))-BALP)
      ENDIF
 
      ARECOI=PHI
 
      RETURN
 
 
C**** END OF ARECOI ****************************************************
      END
