*
* $Id: hnbuff.F,v 1.1.1.1 1996/01/16 17:07:58 mclareni Exp $
*
* $Log: hnbuff.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:58  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/02 16/12/92  16.29.18  by  Fons Rademakers
*-- Author :    Fons Rademakers   28/04/92
      SUBROUTINE HNBUFF(IDD, FATAL)
*.==========>
*.
*.           Fast finding of buffer manager structure for new
*.           (variable row length) N-tuples. This routine may only
*.           be called by HGNTF and HNBFWR. It assumes that the buffers
*.           have not been changed between calls.
*.           For the data-structure description see routine HBNT.
*.
*.  This routine finds the contents buffers for N-tuple ID.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
*
      LOGICAL  FATAL
*
      IF (LQ(LCDIR-4) .EQ. 0) THEN
*
*-- this may not happen in HNBUFF
*
         IF (FATAL) THEN
            CALL HBUG('Buffer structure not initialized.','HNBUFF',IDD)
         ENDIF
         IERR = 1
         RETURN
      ELSEIF (IQ(LBUF-5) .NE. IDD) THEN
*
*-- find buffer bank for ntuple IDD
*
         LBUF = LQ(LCDIR-4)
   20    IF (IQ(LBUF-5) .EQ. IDD) GOTO 40
         IF (LQ(LBUF) .NE. 0) THEN
            LBUF = LQ(LBUF)
            GOTO 20
         ENDIF
*
*-- this may not happen either
*
         IF (FATAL) THEN
            CALL HBUG('Buffer structure not found.','HNBUFF',IDD)
         ENDIF
         IERR = 1
         RETURN
      ENDIF
*
*-- create and set TMP buffers
*
   40 CALL HNTMPF(IDD, FATAL)
*
      END
 
