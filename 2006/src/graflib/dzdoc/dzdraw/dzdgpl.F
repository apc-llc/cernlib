*
* $Id: dzdgpl.F,v 1.1.1.1 1996/03/04 16:13:13 mclareni Exp $
*
* $Log: dzdgpl.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:13  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDGPL(N,X,Y)
      REAL X(9),Y(9)
#include "dzdprm.inc"
#if defined(CERNLIB_BSLASH)
#include "dzdoc/bslash2.inc"
#endif
#if !defined(CERNLIB_BSLASH)
#include "dzdoc/bslash1.inc"
#endif
      CHARACTER*24 CCINT
      N1=IABS(N)
      IF(IFOMED.EQ.3)THEN
         DO 10 I=1,N1-1
         DX=X(I+1)-X(I)
         DY=Y(I+1)-Y(I)
         DL = ABS(DX)*PAGECM
         IF(ABS(DX).LT.0.001)THEN
             IX=0
             IF(DY.GE.0.)THEN
                 IY=1
             ELSE
                 IY=-1
             ENDIF
             DL=ABS(DY)*PAGECM
         ELSEIF(ABS(DY).LT.0.001)THEN
             IY=0
             IF(DX.GE.0.)THEN
                 IX=1
             ELSE
                 IX=-1
             ENDIF
         ELSE
           IF(ABS(DX).GT.ABS(DY))THEN
             ISL=(ABS(DX)+0.01)/ABS(DY)
             IF(ISL.GT.6)ISL=6
             IF(DX.GE.0.)THEN
                 IX=ISL
             ELSE
                 IX=-ISL
             ENDIF
             IF(DY.GE.0.)THEN
                 IY=1
             ELSE
                 IY=-1
             ENDIF
           ELSE
             ISL=(ABS(DY)+0.01)/ABS(DX)
             IF(ISL.GT.6)ISL=6
             IF(DX.GE.0.)THEN
                 IX=1
             ELSE
                 IX=-1
             ENDIF
             IF(DY.GE.0.)THEN
                 IY=ISL
             ELSE
                 IY=-ISL
             ENDIF
           ENDIF
         ENDIF
         IF(I.EQ.N1-1 .AND.N.LT.0)THEN
             WRITE(LUNGRA,'(A,2(F5.2,A),2(I2,A),F5.2,A)')
     &       BS//'p(', X(I)*PAGECM, ',', Y(I)*PAGECM,' ){'//BS//'v(',
     &       IX,',',IY,'){',DL,'}}'
         ELSE
             WRITE(LUNGRA,'(A,2(F5.2,A),2(I2,A),F5.2,A)')
     &       BS//'p(',X(I)*PAGECM,',',Y(I)*PAGECM,'){'//BS//'l(',
     &       IX,',',IY,'){',DL,'}}'
         ENDIF
10       CONTINUE
         GOTO 999
*     PostScript
      ELSEIF(IFOMED.EQ.2)THEN
         IF(PAMM10.LE.0.001)PAMM10=70.
         DO 20 I=1,N1
            IX=X(I)*PAMM10
            IY=Y(I)*PAMM10
            IF(I.EQ.1)THEN
               WRITE(CCINT,'(2I9,A)')IX,IY,' m'
            ELSE
               WRITE(CCINT,'(2I9,A)')IX,IY,' d'
            ENDIF
            CALL DZDPLN(LUNGRA,CCINT,-1)
20       CONTINUE
         CALL DZDPLN(LUNGRA,' s',-1)
      ENDIF
      CALL IPL(N1,X,Y)
999   END
 
************************************************************************
