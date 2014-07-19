*
* $Id: tmread.F,v 1.1.1.1 1996/02/15 17:52:29 mclareni Exp $
*
* $Log: tmread.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:29  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      OPTIONS  -sa
      SUBROUTINE TMREAD (MAXCHL, LINE, NCHL, ISTATL)
C
C CERN PROGLIB#         TMREAD          .VERSION KERNCVX  1.08  910806
C ORIG. 06/08/91, JZ
C
C-    read the next line from stdin :
C-
C-        MAXCH   maxim. # of characters into LINE
C-        NCH     actual # of characters read into LINE
C-        ISTAT   status return, zero : OK  -ve : EoF

      CHARACTER    LINE*(*)

      INTEGER*4    MAXCH, NCH, ISTAT


      MAXCH = MAXCHL
      CALL TMREADI (MAXCH, LINE, NCH, ISTAT)
      NCHL   = NCH
      ISTATL = ISTAT
      RETURN
      END
#ifdef CERNLIB_CCGEN_TMREAD
#undef CERNLIB_CCGEN_TMREAD
#endif
