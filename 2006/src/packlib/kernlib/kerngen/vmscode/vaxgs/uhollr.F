*
* $Id: uhollr.F,v 1.1.1.1 1996/02/15 17:50:25 mclareni Exp $
*
* $Log: uhollr.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:25  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
      SUBROUTINE UHOLLR (VECT,NC,WORD)
C
C CERN PROGLIB# M415    UHOLLR          .VERSION KERNVAX  2.04  830531
C ORIG. 01/01/65 JZ, MODIFIED FOR VAX 31/05/83 BY BODINI+CECCHET
C
C-    LOAD THE NC-LONG HOLLERITH STRING IN WORD INTO THE VECTOR VECT
C-    PAD VECT WITH BLANKS TO ITS FULL WORD BOUNDARY
C
      BYTE    VECT(9), WORD(9)
C
      NW = (NC-1)/4 + 1
      NT = 4*NW
C
      CALL UCOPY (WORD,VECT,NW)
C
      IF (NC.EQ.NT)          RETURN
C
      DO 17 K=NC+1,NT
   17 VECT(K)= ' '
C
      RETURN
      END
