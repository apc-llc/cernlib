*
* $Id: iformt.F,v 1.1.1.1 1996/02/15 17:53:26 mclareni Exp $
*
* $Log: iformt.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:26  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION IFORMT (WORD,N1,N2)
C
C
      COMMON/SLATE/IBIN(40)
      DIMENSION LSIGN(3)
      DIMENSION WORD(9)
C
      DIMENSION INT(10)
      DATA  LSIGN/ 1H-, 1H , 1H+/
      DATA  INT / 1H0, 1H1, 1H2, 1H3, 1H4, 1H5, 1H6, 1H7, 1H8, 1H9/
C
      KSIGN=1
      IFORMT=0
      CALL UBLOW (WORD,IBIN,N2)
C
         DO 10 J=N1,N2
      IN=IUCOMP (IBIN(J),INT,10)
      IF (IN.NE.0) GO TO 15
      I=IUCOMP (IBIN(J),LSIGN,3)
      IF (I.EQ.0) GO TO 101
      IF (I.NE.2) GO TO 30
   10 CONTINUE
      GO TO 25
C
   15    DO 20 I=J,N2
      IN=IUCOMP (IBIN(I),INT,10)-1
      IF (IBIN(I).EQ.LSIGN(2)) IN=0
      IF (IN.LT.0) GO TO 101
   20 IFORMT=IN+10*IFORMT
C
   25 IFORMT=KSIGN*IFORMT
      RETURN
C
   30 KSIGN=I-2
      J=J+1
      GO TO 15
C
  101 IFORMT=-999999999
      RETURN
      END
