*
* $Id: armcdi.F,v 1.1.1.1 1996/03/08 16:51:02 mclareni Exp $
*
* $Log: armcdi.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:02  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: armcdi.F,v 1.1.1.1 1996/03/08 16:51:02 mclareni Exp $
 
      SUBROUTINE ARMCDI(ARRNDX,ARRNDY,ARVETO)
 
C...ARiadne subroutine Monte Carlo DIstribution
 
C...Generates x_1 and x_3 for a radiating dipole
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /ARINT1/ BC1,BC3,BZM,BZP,BP1,BM1,BP3,BM3,BPDY,BMDY,
     $                BMRP1,BMR1,BMRP3,BMR3,KQ3,KF3,KQ1,KF1,
     $                B1,B2,B3,XT2,XT,Y,QQ1,QQ3,
     $                QE1,QE3,ALP1,ALP3,XMU1,XMU3,
     $                S,W,C,CN,ALPHA0,XLAM2,IFLG,IFL1,IFL3,
     $                XT2MP,XT2M,XT2C,XTS,XT3,XT1,XT2GG1,XT2GG3,
     $                YINT,YMAX,YMIN,SQ2,YFAC,PTTRUE,
     $                Y1,Y2,Y3,SY1,SY2,SY3,SSY,ZSQEV,
     $                AE1,AE3,NXP1,NXP3,FQ1,FQ3,QFAIL,QEXDY
      SAVE /ARINT1/
 
 
C...Exit if below cut
 100  IF (XT2M.LT.XT2C) GOTO 900
      QFAIL=.FALSE.
 
C...Generate random XT2
      XT2=ARRNDX()
      IF (XT2.LT.XT2C) GOTO 900
      XT=SQRT(XT2)
 
C...Generate rapidity Y
      Y=ARRNDY()
 
C...Calculate energy fractions
      B1=BC1-XT*EXP(Y)
      B3=BC3-XT*EXP(-Y)
      B2=2.0-B1-B3
 
C...Set maximum XT2 for possible next random call (VETO algorithm)
      XT2M=XT2
 
C...Redo random calls according to veto-algorithm
      IF (QFAIL.OR.ARVETO().LE.RLU(IDUM)) GOTO 100
 
C...Check that Current values are kinematically allowed
      CALL ARCHKI(0,IOK)
      IF (IOK.EQ.0) GOTO 100
 
      RETURN
 
C...If below cuts set XT2 to 0
 900  B1=BC1
      B3=BC3
      XT2=0.0
 
      RETURN
 
C**** END OF ARMCDI ****************************************************
      END
