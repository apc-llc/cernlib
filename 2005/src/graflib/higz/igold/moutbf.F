*
* $Id: moutbf.F,v 1.1.1.1 1996/02/14 13:10:48 mclareni Exp $
*
* $Log: moutbf.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:48  mclareni
* Higz
*
*
#if defined(CERNLIB_MSDOS)
#include "higz/pilot.h"
*CMZ :  1.14/11 29/06/92  13.58.00  by  Unknown
*-- Author : V.Fine
      SUBROUTINE MOUTBF
#include "higz/golden.inc"
#if defined(CERNLIB_XIOPAK)
      CALL XOTBYF(LUNG,CHMBUF(:IPOINT-1),IPOINT-1)
#endif
#if defined(CERNLIB_CIPACK)
      CALL CIPUT(LUNG,CHMBUF(:IPOINT-1),IPOINT-1,ISTAT)
      IF (ISTAT .LT. 0) GO TO 10
#endif
#if (!defined(CERNLIB_XIOPAK))&&(!defined(CERNLIB_CIPACK))
      Write (LUNG(1), err=10 , rec=NREC) BUFFER
      CALL VFILL(BUFFER,LBUF/4,0)
#endif
      NREC = NREC + 1
      RETURN
#if !defined(CERNLIB_XIOPAK)
   10 CONTINUE
      CALL IGERR('META FILE WRITE FAULT','MOUTBF')
#endif
      END
#endif
