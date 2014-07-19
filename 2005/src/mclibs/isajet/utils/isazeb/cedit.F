*
* $Id: cedit.F,v 1.10 2001/10/08 14:03:16 mclareni Exp $
*
* $Log: cedit.F,v $
* Revision 1.10  2001/10/08 14:03:16  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION CEDIT(EEM,EHAD,SPHI,CPHI,STH,CTH,ETA)
C======================================================================
C
C   Purpose and Methods :
C   select calorimeter cells to be included in Zebra output
C   called for every cell. If CEDIT=.FALSE. cell will
C   be left out
C
C   Inputs  :
C   EEM = e.m. energy in cell
C   EHAD= had. energy in cell
C   SPHI = sin(phi)   center of cell
C   CPHI = cos(phi)   center of cell
C   STH  = sin(th)         "
C   CTH  = cos(th)         "
C   ETA  = eta             "
C
C   Outputs : NONE
C
C   Created  23-MAY-1987   Serban D. Protopopescu
C
C======================================================================
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      LOGICAL CEDIT
      REAL EEM,EHAD,SPHI,CPHI,STH,CTH,ETA
C======================================================================
      CEDIT=.TRUE.
      IF((EEM+EHAD)*STH.LT..2) CEDIT=.FALSE.
  999 RETURN
      END
