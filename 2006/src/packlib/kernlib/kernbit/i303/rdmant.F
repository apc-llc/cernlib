*
* $Id: rdmant.F,v 1.1.1.1 1996/02/15 17:47:46 mclareni Exp $
*
* $Log: rdmant.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:46  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      SUBROUTINE RDMANT(LENMAN,LENEXP)
C** CALLED AFTER DECIMAL POINT WAS READ TO READ MANTISSA
C**********************************************************************
#include "i303cd.inc"
C*NS  LOGICAL RDIGI,RALPH
      CHARACTER*1 RDUPCH

      CALL RDINT(LENMAN)
      IFPOS=IBPTR
      IF (RDUPCH(CURNT) .EQ. 'E') THEN
         CALL RDNEXT
C        *** SIGN IS THE FIRST CHARACTER
         IF ((CURNT .EQ. '+') .OR. (CURNT .EQ. '-')) THEN
            CALL RDNEXT
         ENDIF
         CALL RDINT(LENGTH)
      ENDIF
      LENEXP=IBPTR-IFPOS
      END
