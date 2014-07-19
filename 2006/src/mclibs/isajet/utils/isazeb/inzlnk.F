*
* $Id: inzlnk.F,v 1.11 2001/10/08 14:03:16 mclareni Exp $
*
* $Log: inzlnk.F,v $
* Revision 1.11  2001/10/08 14:03:16  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE INZLNK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-       Initialize link area /ZLINKA/
C-
C-   Created  28-OCT-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "zebcom.inc"
#include "zlinka.inc"
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        CALL MZLINK(IXCOM,'/ZLINKA/',LSLINK,LRLINK,LRLINK(NRLINK))
        FIRST=.FALSE.
      ENDIF
  999 RETURN
      END
