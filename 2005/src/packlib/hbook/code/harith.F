*
* $Id: harith.F,v 1.1.1.1 1996/01/16 17:07:31 mclareni Exp $
*
* $Log: harith.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:31  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 05/03/89  09.22.01  by  Rene Brun
*-- Author :
      SUBROUTINE HARITH (TFUNC,Y,A,DF,PL0,AMX,AMN)
*.==========>
*.           HARITH sets value of y-fitting function
*.           and calculates df=dy/da-derivatives
*..=========> ( I.Ivanchenko )
      DIMENSION A(1),DF(1),PL0(1),AMX(1),AMN(1)
#include "hbook/hcfit2.inc"
      EXTERNAL TFUNC
*
*             10.*MAXIMUM RELATIVE PRECISION
      SAVE RP
      DATA RP/1.E-14/
*.___________________________________________
      IF(IDIMPN.EQ.4)THEN
         XHFIT=XFUMIL(1)
         YHFIT=XFUMIL(2)
         CALL HTFUNC(TFUNC,XHFIT,YHFIT,Y)
      ELSE
         Y=TFUNC(XFUMIL)
      ENDIF
*
      DO 10 I=1,NA
         DF(I)=0.
         IF (PL0(I).LE.0.)GO TO 10
         AI=A(I)
         HI=0.01*PL0(I)
         PI=RP*ABS(AI)
         IF (HI.LE.PI) HI=PI
         A(I)=AI+HI
         IF (A(I)-AMX(I).GT.0.)THEN
            A(I)=AI-HI
            HI=-HI
            IF (A(I)-AMN(I).LT.0.)THEN
               A(I)=AMX(I)
               HI=AMX(I)-AI
               IF (AMN(I)-AI+HI.LT.0.)THEN
                  A(I)=AMN(I)
                  HI=AMN(I)-AI
               ENDIF
            ENDIF
         ENDIF
         IF(IDIMPN.EQ.4)THEN
            CALL HTFUNC(TFUNC,XHFIT,YHFIT,Z)
         ELSE
            Z=TFUNC(XFUMIL)
         ENDIF
         DF(I)=(Z-Y)/HI
         A(I)=AI
  10  CONTINUE
      END
