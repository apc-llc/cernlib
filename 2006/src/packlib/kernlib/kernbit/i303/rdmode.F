*
* $Id: rdmode.F,v 1.1.1.1 1996/02/15 17:47:46 mclareni Exp $
*
* $Log: rdmode.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:46  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      SUBROUTINE RDMODE(I)
C  TO CHANGE MODE OF WORK OF RDNAME.
C  CALL TO RDMODE(2) WILL FORCE RDNAME READ AS WHOLE NAMES SETS OF
C  ALPHANUMERIC CHARACTERS, SUCH AS ALPHA99.
C  THE USUAL MODE (PURELY ALPHABETIC NAMES) WILL BE RESTORED BY A
C  CALL RDMODE(1)
C********************************************************************
#include "i303cd.inc"
      MODE=I
      END
