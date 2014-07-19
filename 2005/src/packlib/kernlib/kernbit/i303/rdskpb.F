*
* $Id: rdskpb.F,v 1.1.1.1 1996/02/15 17:47:46 mclareni Exp $
*
* $Log: rdskpb.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:46  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      SUBROUTINE RDSKPB
C** SKIPS BLANKS BEGINNING FROM CURRENT CHARACTER AND POSITIONS
C** POINTER ON FIRST NON-BLANK CHARACTER OR ON POSITION 81 IF
C** THERE IS NO NON-BLANCS LEFT
C********************************************************************
#include "i303cd.inc"
    1 CONTINUE
      IF (CURNT .EQ. ' ') THEN
         IF (IBPTR .GE. 80) THEN
            IBPTR=81
            RETURN
         ENDIF
         CALL RDNEXT
         GO TO 1
      ENDIF
      RETURN
      END
