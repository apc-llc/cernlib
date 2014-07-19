*
* $Id: mcsident.F,v 1.1.1.1 1996/02/26 17:16:31 mclareni Exp $
*
* $Log: mcsident.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:31  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/00 04/03/94  16.06.44  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      FUNCTION MCSIDENT(J1,N1)
C===============================================
C   PICKS OUT AN IDENTIFIER FROM STRING (J1,N1)
C   AND PLACES IT TO IDEN(8)
C   IF THE IDENTIFIER CONSISTS OF MORE THAN
C   KLENID SYMBOLS, THE LAST SYMBOLS ARE IGNORED.
C   MCSIDENT= IDENTIFIER'S LENGTH, EVEN WHEN IT
C   IS GREATER THAN KLENID.
C   J1 AND N1 MUST BE VARYABLES.
C===============================================
#include "comis/cspar.inc"
#include "comis/cspnts.inc"
#include "comis/csichv.inc"
      MCSIDENT=0
      IF(N1.LE.0)RETURN
**      CALL CSSETC(J2,N2,ICHBLN)
      K=MKCHAR(J1)
**      IF(MLETTE(K).EQ.0)RETURN
      IF(K.GE.ICHLA .AND. K.LE.ICHLZ .OR. K.EQ.ICHSUSC)THEN
        L=1
        J=J1
        J1=J1+1
        N1=N1-1
      ELSE
        RETURN
      ENDIF
    1 IF(N1.LE.0)GO TO 3
      K=MKCHAR(J1)
      IF(   K.GE.ICHLA .AND. K.LE.ICHLZ
     + .OR. K.EQ.ICHSUSC
     + .OR. K.GE.ICHD0 .AND. K.LE.ICHD9 )THEN
        L=L+1
        J1=J1+1
        N1=N1-1
        GO TO 1
      ENDIF
    3 MCSIDENT=L
      L=MIN(L,KLENID)
      NCIDEN=L
      NWIDEN=(L+3)/4
      IDEN(NWIDEN)=KBLN
      IF(NWIDEN.LT.8)IDEN(NWIDEN+1)=KBLN
      CALL CCOPYS(J,JID,L)
      END
