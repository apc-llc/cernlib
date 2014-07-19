*
* $Id: time.F,v 1.1.1.1 1996/02/15 17:50:05 mclareni Exp $
*
* $Log: time.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:05  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
      SUBROUTINE TIMEST(TIMINT)
C
C CERN PROGLIB# Z007    TIMEST          .VERSION KERNMPW  1.00  891208
C ORIG. 01/12/89  FCA
C MODIF.06/10/92 Cees de Laat  correct TIMED
C
      SAVE HTIME, TIMLIM, TIMLAS
      DATA HTIME, TIMLAS, TIMLIM /0., 0., 10000./
      HTIME = SECNDS(0.)
      TIMLAS = HTIME
      TIMLIM = TIMINT
      RETURN
      ENTRY TIMEX(T)
      T=SECNDS(HTIME)
      RETURN
      ENTRY TIMEL(T)
      T = TIMLIM - SECNDS(HTIME)
      RETURN
      ENTRY TIMED(T)
      T = SECNDS(TIMLAS)
      TIMLAS = TIMLAS + T
      END
