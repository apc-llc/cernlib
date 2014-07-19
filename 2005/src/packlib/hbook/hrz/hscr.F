*
* $Id: hscr.F,v 1.1.1.1 1996/01/16 17:08:08 mclareni Exp $
*
* $Log: hscr.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:08  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/08 06/07/94  19.10.02  by  Rene Brun
*-- Author :
      SUBROUTINE HSCR(IDD,ICYCLE,CHOPT)
*.==========>
*.           To scratch histogram ID from current directory
*.           on the RZ file.
*..=========> ( R.Brun )
#include "hbook/hcdire.inc"
      CHARACTER*(*) CHOPT
      COMMON/QUEST/IQUEST(100)
      DIMENSION KEYRZ(2)
*.___________________________________________
*
      IF(ICHTOP(ICDIR).EQ.0)THEN
         CALL HDELET(IDD)
         GO TO 99
      ENDIF
*
      IF(IDD.EQ.0)THEN
         CALL RZDELK(IDD,ICYCLE,'K')
      ELSE
         IKS=0
  10     IKS=IKS+1
  20     CALL RZINK(IKS,ICYCLE,'S')
         IF(IQUEST(1).NE.0)GO TO 90
         KEYRZ(1)=IQUEST(21)
         KEYRZ(2)=IQUEST(22)
         IF(KEYRZ(1).NE.IDD)GO TO 10
         CALL RZDELK(KEYRZ,ICYCLE,CHOPT)
         IF(IQUEST(1).NE.0)GO TO 99
         GO TO 20
      ENDIF
  90  CALL RZSAVE
  99  RETURN
      END
