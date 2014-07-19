*
* $Id: cstext.F,v 1.1.1.1 1996/02/26 17:16:25 mclareni Exp $
*
* $Log: cstext.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:25  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/00 18/01/94  08.59.24  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
       SUBROUTINE CSTEXT(PR,TEXT)
***--------------------------------
#include "comis/cspar.inc"
#include "comis/cspnts.inc"
      CHARACTER *(*) PR,TEXT
      IF(JPMB.NE.0)CALL CSLFRE(JPMB)
      NC=LENOCC(TEXT)
      CALL CSPUTL(JPMB,TEXT,NC,0)
      JPMC=JPMB
      KEYRD=-1
      CALL CSPAUS(PR)
      END
