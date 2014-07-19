*
* $Id: mident.F,v 1.1.1.1 1996/02/26 17:16:57 mclareni Exp $
*
* $Log: mident.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:57  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_IBM)||defined(CERNLIB_APOLLO)||!defined(CERNLIB_ASSEMB)
*CMZ :  1.18/00 18/02/94  14.46.03  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      FUNCTION MIDENT(J1,N1,J2,N2)
C===============================================
C   PICKS OUT AN IDENTIFIER FROM STRING (J1,N1)
C   AND PLACES IT TO STRING (J2,N2).
C   IF THE IDENTIFIER CONSISTS OF MORE THAN
C   N2 SYMBOLS, THE LAST SYMBOLS ARE IGNORED.
C   IDENT= IDENTIFIER'S LENGTH, EVEN WHEN IT
C   IS GREATER THAN N2.
C   J1 AND N1 MUST BE VARYABLES.
C   N2 MUST BE GIVEN.
C===============================================
#include "comis/cspar.inc"
#include "comis/cspnts.inc"
#include "comis/csichv.inc"
      MIDENT=0
      IF(N1.LE.0)RETURN
      CALL CSSETC(J2,N2,ICHBLN)
      K=MKCHAR(J1)
      IF(MLETTE(K).EQ.0)RETURN
      L=0
      J=J2
      N=N2
      GO TO 2
    1 IF(N1.LE.0)GO TO 3
      K=MKCHAR(J1)
      IF(MLETTE(K).EQ.1)GO TO 2
      IF(MLDIGI(K).EQ.0)GO TO 3
    2 J1=J1+1
      N1=N1-1
      L=L+1
      IF(N.LE.0)GO TO 1
      CALL CSCHAR(J,K)
      J=J+1
      N=N-1
      GO TO 1
    3 MIDENT=L
      NCIDEN=L
      NWIDEN=(L+3)/4
      RETURN
      END
#endif
