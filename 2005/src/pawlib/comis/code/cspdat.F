*
* $Id: cspdat.F,v 1.1.1.1 1996/02/26 17:16:16 mclareni Exp $
*
* $Log: cspdat.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:16  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.36.33  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CSPDAT(JIB)
***------------------------
*      routine for processing data statement
***------------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstab.inc"
#include "comis/cssysd.inc"
#include "comis/cspnts.inc"
      I=IQ(LDATA)
      LDATA=0
  1   IF(I.EQ.0)RETURN
      IT=IQ(I+3)
      CALL CSLDLI(IT)
      LD=LENEGI*IQ(I+2)
      IF(IABS(ITYPGI).EQ.3)THEN
        IF(NUMGI.EQ.0)THEN
          J=MJCHAR(IQ(JIB+ISHGI))
        ELSE
          J=(IQ(NUMGI)+ISHGI)*NBYTPW
        ENDIF
        J=J+IQ(I+1)
        CALL CCOPYS(MJCHAR(IQ(I+4)),J,LD)
      ELSE
        IS=ISHGI+IQ(I+1)
        IF(NUMGI.EQ.0)THEN
          CALL CCOPYA(IQ(I+4),IQ(JIB+IS),LD)
        ELSE
          J=IQ(NUMGI)+IS
          CALL COPYAJ(LOCF(IQ(I+4)),J,LD)
        ENDIF
      ENDIF
      I1=IQ(I)
      CALL MHFREE(I)
      I=I1
      GO TO 1
      END
