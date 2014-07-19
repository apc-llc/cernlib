*
* $Id: csoutb.F,v 1.1.1.1 1996/02/26 17:16:23 mclareni Exp $
*
* $Log: csoutb.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:23  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.36.57  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CSOUTB
#include "comis/csrec.inc"
#include "comis/cslun.inc"
         COMMON/CSCHBP/NP,NF
      IF(NF.GT.NP)THEN
         CALL CSSOUT(REC1(:NF-1))
         IF(ISTLOG.NE.0)THEN
           WRITE(LUNLOG,77)REC1(1:NF-1)
  77       FORMAT(A)
         ENDIF
      ENDIF
      NF=NP
      REC1(NF:)=' '
      END
