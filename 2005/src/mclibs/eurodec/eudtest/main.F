*
* $Id: main.F,v 1.1.1.1 1996/03/08 16:58:53 mclareni Exp $
*
* $Log: main.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:53  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      PROGRAM EUEXAM
#include "eurodec/eufiles.inc"
C
      COMMON / PAWC / HMEM(50000)
      CALL TIMEST(30.)
      CALL HLIMIT(50000)
C---  Here test fragmentation
C---
      OPEN(10,FILE=EDFILE,STATUS='OLD')
      OPEN(11,FILE=ETFILE,STATUS='OLD')
C
      CALL STEERF
      CALL HDELET(0)
C---  Here test decay
C---
      OPEN(10,FILE=EDFILE,STATUS='OLD')
      OPEN(11,FILE=ETFILE,STATUS='OLD')
C
      CALL STEERD
C
      END
