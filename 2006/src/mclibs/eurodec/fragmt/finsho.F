*
* $Id: finsho.F,v 1.1.1.1 1996/03/08 16:58:51 mclareni Exp $
*
* $Log: finsho.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:51  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      SUBROUTINE FINSHO(IP1,IP2)
C.----------------------------------------------------------------------
C.
C.    TREAT PARTONS AT LOCATION IP1 UPTO IP2 IN COMMON /HADGEN/:
C.    AND PERFORM FORWARD PARTON CASCADE (FOX & WOLFRAM)
C.    LAST UPDATE: 10/04/88
C.
C.----------------------------------------------------------------------
#include "eurodec/hadgen.inc"
      IF (IP2.GT.NHMAX) CALL ERRORD(62,' ',FLOAT(NHMAX))
      RETURN
      END
