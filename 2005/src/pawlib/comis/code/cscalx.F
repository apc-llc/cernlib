*
* $Id: cscalx.F,v 1.1.1.1 1996/02/26 17:16:28 mclareni Exp $
*
* $Log: cscalx.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:28  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.01  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CSCALX(NAME,NPAR,M)
***---------------------------------
      INTEGER CSADDR
#include "comis/csrec.inc"
           CHARACTER *(*) NAME
           INTEGER M(10)
      J=CSADDR(NAME)
      IF(J.NE.0)THEN
        CALL CSJCAX(J,NPAR,M(1))
      ELSE
        REC1='CSCALL: NO PROGRAM '// NAME
        CALL CSSOUT(REC1)
      ENDIF
      END
