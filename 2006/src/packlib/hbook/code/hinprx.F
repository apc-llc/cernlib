*
* $Id: hinprx.F,v 1.1.1.1 1996/01/16 17:07:40 mclareni Exp $
*
* $Log: hinprx.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:40  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 19/08/94  13.51.14  by  Rene Brun
*-- Author :
      SUBROUTINE HINPRX(LABEL,IXY,NUM,NC,XLOW,XUP,LC)
*.==========>
*.           PRINTS INDEX INFO FOR PRO, BAND,SLICE
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcprin.inc"
#include "hbook/hcunit.inc"
      COMMON/HFORM/IA(127),IDU
      CHARACTER*(*) LABEL
*.___________________________________________
      CALL VBLANK(IA(2),12)
      CALL UCTOH(LABEL,IA(2),1,12)
      IF(NUM.NE.0)CALL HBCDI(NUM,3,IA(11))
      NH=NH+1
      NT=IQ(LC-1)+12
      IF(LQ(LC).NE.0)THEN
         LW=LQ(LC)
         NT=NT+IQ(LW-1)+10
      ENDIF
*
      WRITE(LOUT,9700)NH,(IA(I),I=2,13),IQ(LC+KNBIT),IQ(LC+KNOENT),
     +   IDG(2),IXY,NC,XLOW,XUP,LC,NT
 9700 FORMAT(' .',I4,12X,12A1,28X,I2,I9,2(2X,A1),I6,
     *  2(2X,E10.3),3X,I9,I7,1X,'.')
*
      CALL VBLANK(IA(2),123)
      END
