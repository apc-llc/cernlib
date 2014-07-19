*
* $Id: mcsges.F,v 1.1.1.1 1996/02/26 17:16:50 mclareni Exp $
*
* $Log: mcsges.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:50  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/00 17/02/94  16.56.13  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      FUNCTION MCSGES(JS,NS,JP,NP)
      INTEGER CGCONT
      EXTERNAL CGCONT
#include "comis/csichv.inc"
      MCSGES=0
      J1=JS
      N1=NS
      J2=JP
      N2=NP
      IF(N2.LE.0)GO TO 2
      KCH2=MKCHAR(J2)
      IF(KCH2.NE.ICHBLN)KK= MKBLAN(J1,N1)
 1    IF(N1.LE.0)THEN
         IF(CGCONT(JS,NS).EQ.0)GO TO 3
         J1=JS
         N1=NS
         KK= MKBLAN(J1,N1)
         GO TO  1
      ENDIF
      KCH1=MKCHAR(J1)
      IF(KCH1.NE.KCH2)GO TO 3
      J1=J1+1
      N1=N1-1
      J2=J2+1
      N2=N2-1
      IF(KCH1.EQ.ICHBLN)THEN
         KK1= MKBLAN(J1,N1)
         KK2= MKBLAN(J2,N2)
      ENDIF
      IF(N2.GT.0)THEN
         KCH2=MKCHAR(J2)
         GO TO 1
      ENDIF
      JS=J1
      NS=N1
 2    MCSGES=1
 3    CONTINUE
      RETURN
      END
