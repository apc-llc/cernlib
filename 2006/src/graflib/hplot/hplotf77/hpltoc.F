*
* $Id: hpltoc.F,v 1.1.1.1 1996/01/19 10:50:11 mclareni Exp $
*
* $Log: hpltoc.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:11  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.18/00 01/11/93  10.09.36  by  O.Couet
*-- Author :
      SUBROUTINE HPLTOC(X,Y,XCM,YCM,NT)
*.==========>
*.       Converts X and Y coordinates in the current transformation
*.       to cm.
*..=========>
#include "hplot/hpl1.inc"
      DIMENSION RWIN(4),RVP(4)
*._____________________________
*
      CALL IGQWK(1,'NTNB',RNT)
      CALL IGQWK(1,'NTWN',RWIN)
      CALL IGQWK(1,'NTVP',RVP)
      NT=INT(RNT)
*
      IF(NT.EQ.1)THEN
         XCM=X
         YCM=Y
      ELSE
         IF(.NOT.LOGXFL)THEN
            DX=X-RWIN(1)
         ELSE
            IF(X.LE.0.)GOTO 10
            DX=LOG10(X)-RWIN(1)
         ENDIF
         IF(.NOT.LOGYFL)THEN
            DY=Y-RWIN(3)
         ELSE
            IF(Y.LE.0.)GOTO 10
            DY=LOG10(Y)-RWIN(3)
         ENDIF
         XVP=((RVP(2)-RVP(1))*DX)/(RWIN(2)-RWIN(1))+RVP(1)
         YVP=((RVP(4)-RVP(3))*DY)/(RWIN(4)-RWIN(3))+RVP(3)
         CALL ISELNT(1)
         CALL IGQWK(1,'NTWN',RWIN)
         CALL IGQWK(1,'NTVP',RVP)
         XCM=((XVP-RVP(1))*(RWIN(2)-RWIN(1)))/(RVP(2)-RVP(1))+RWIN(1)
         YCM=((YVP-RVP(3))*(RWIN(4)-RWIN(3)))/(RVP(4)-RVP(3))+RWIN(3)
         CALL ISELNT(NT)
      ENDIF
      RETURN
*
   10 CALL HBUG('Illegal coordinate in Log scale','HPLTOC',0)
      END
