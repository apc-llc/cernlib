*
* $Id: pfmask.F,v 1.1.1.1 1996/03/01 11:38:46 mclareni Exp $
*
* $Log: pfmask.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:46  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.03/07 19/08/93  16.39.21  by  Fons Rademakers
*-- Author :    Fons Rademakers   19/08/93
      SUBROUTINE PFMASK(NSLAV, MASK, IMODE)
************************************************************************
*                                                                      *
* Set bits into a mask. General routine but specifically used to       *
* set the bits in a mask used by the PSELCT function.                  *
* For IMODE=-1, set the first NSLAV bits in MASK, for IMODE=1 set bit  *
* NSLAV to 1, for IMODE=0 set bit NSLAV to 0.                          *
*                                                                      *
************************************************************************
*
      IF (IMODE .EQ. 0) THEN
         CALL SBIT0(MASK, NSLAV)
      ELSEIF (IMODE .GT. 0) THEN
         CALL SBIT1(MASK, NSLAV)
      ELSE
         MASK = 0
         DO 10 I = 1, NSLAV
            CALL SBIT1(MASK, I)
10       CONTINUE
      ENDIF
*
      END
