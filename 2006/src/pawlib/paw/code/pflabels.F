*
* $Id: pflabels.F,v 1.3 1996/09/17 08:26:38 dinofm Exp $
*
* $Log: pflabels.F,v $
* Revision 1.3  1996/09/17 08:26:38  dinofm
* Two unused variables have been removed
*
* Revision 1.2  1996/06/03 13:28:49  couet
* - add pilot.h
*
* Revision 1.1.1.1  1996/06/03 15:09:49 couet
* Paw
*
* 
#include "paw/pilot.h"
      SUBROUTINE PFLABELS (CBUF,ISIZE)
*.==========>
*. Send the labels of a nTuple character variable to the master and get
*. back the Union of all labels detected by slaves
*.
*..=========> ( Dino FM )
*
*
#include "hbook/hcpiaf.inc"
#include "hbook/czbuff.inc"

      CHARACTER*(*) CBUF
      INTEGER LPTR,LENCHUNK,LREM,MAXBUFLEN,ISTAT

      MAXBUFLEN = LEN (CHBUF)

*---- Send the PLABELS pseudo-command 
      WRITE (CHSMPF,'(A,I12)') 'PLABEL', ISIZE
      CALL CZPUTA(CHSMPF,ISTAT)
      IF (ISTAT .NE. 0) GOTO 999

*---- Send the labels; the input buffer is split (if required)
*---- over several physical buffers
      LPTR = 0
      DO 111 LREM = ISIZE, 0, -MAXBUFLEN
         LENCHUNK =  MIN (LREM,MAXBUFLEN)
         CHBUF = CBUF (LPTR + 1 : LPTR + LENCHUNK)
         LPTR = LPTR + LENCHUNK
         CALL CZPUTC (LENCHUNK, ISTAT)
         IF(ISTAT .NE. 0) GOTO 999
 111  CONTINUE

*
*-- get back the Union set as a string
*

*---- Read the new size
      CALL CZGETA(CHSMPF, ISTAT)
      READ(CHSMPF,'(I12)') ISIZE

      LPTR = 0
      DO 112 LREM = ISIZE, 0, -MAXBUFLEN
         LENCHUNK =  MIN (LREM,MAXBUFLEN)
         CALL CZGETC (LENCHUNK, ISTAT)
         CBUF (LPTR + 1 : LPTR + LENCHUNK) = CHBUF (1:LENCHUNK)
         LPTR = LPTR + LENCHUNK
         IF(ISTAT .NE. 0) GOTO 999
 112  CONTINUE



 999  CONTINUE
      
      END

