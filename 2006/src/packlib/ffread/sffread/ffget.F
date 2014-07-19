*
* $Id: ffget.F,v 1.1.1.1 1996/03/08 11:50:41 mclareni Exp $
*
* $Log: ffget.F,v $
* Revision 1.1.1.1  1996/03/08 11:50:41  mclareni
* Ffread
*
*
#include "ffread/pilot.h"
      SUBROUTINE FFGET (CHOPT, IVALUE)
C
C      FUNCTIONAL DESCRIPTION:
C
C              This routine returns certain internal parameters of FFREAD.
C
C      DUMMY ARGUMENTS:
C
C              CHOPT  - character string specifying which item is requested.
C                       Possible values are:
C                       LENG - # of significant characters on data card
C                       LINP - LUN for input
C                       LOUT - LUN for output
C                       NBIT - # of bits per word        -
C                       NCHW - # of characters per word   ) machine constants
C                       NBCH - # of bits per character   -
C                       NCHK - # of characters per key
C              IVALUE - integer value corresponding to requested item
C
C      IMPLICIT INPUTS:
C
C              For CHOPT = LENG, LINP, LOUT, or NCHK, the corresponding
C              values from common CFREAD.
C              The other values are machine-dependant constants.
C
C      IMPLICIT OUTPUTS:
C
C              NONE
C
C      SIDE EFFECTS:
C
C              NONE
C
#include "ffread/ffmach.inc"
#include "ffread/ffluns.inc"
#include "ffread/cfread.inc"
      CHARACTER CHOPT*(*)
C
C----------------- Beginning of executable statements -------------------------
C
C        Check option and dispatch on it
C
      CALL UCTOH (CHOPT, NCHOPT, NCHWD, MIN (LEN (CHOPT), 4) )
      IOPT = IUCOMP (NCHOPT, IFFOPT(2), 7)
      GO TO (1, 2, 3, 4, 5, 6, 7), IOPT
C
C        Unknown option
C
      WRITE (UNIT=LUNOUT, FMT=1001) NCHOPT
      RETURN
 
   1  IVALUE = LINPUT
      RETURN
 
   2  IVALUE = LUNIN
      RETURN
 
   3  IVALUE = LUNOUT
      RETURN
 
   4  IVALUE = NBITWD
      RETURN
 
   5  IVALUE = NCHWD
      RETURN
 
   6  IVALUE = NBITCH
      RETURN
 
   7  IVALUE = NCHKEY
      RETURN
 
1001  FORMAT
     * (' FFGET  --- UNRECOGNIZED OPTION ''', A4, ''' - TRY AGAIN')
 
      END
