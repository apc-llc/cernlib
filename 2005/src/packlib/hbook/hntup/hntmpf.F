*
* $Id: hntmpf.F,v 1.1.1.1 1996/01/16 17:07:58 mclareni Exp $
*
* $Log: hntmpf.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:58  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.20/00 19/06/93  14.01.33  by  Fons Rademakers
*-- Author :    Fons Rademakers   28/04/92
      SUBROUTINE HNTMPF(IDD, FATAL)
*.==========>
*.
*.           Fast finding of temporary buffers for new
*.           (variable row length) N-tuples. This routine may
*.           only be called by HNBUFF. It assumes that no new variables
*.           have been declared since the last call.
*.           For the data-structure description see routine HBNT.
*.
*.  This routine finds the tmp buffers for N-tuple ID.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
*
      LOGICAL  FATAL
*
      IF (LQ(LCDIR-5) .EQ. 0) THEN
*
*-- this may not happen in HNTMPF
*
         IF (FATAL) THEN
            CALL HBUG('Tmp structure not initialized.','HNTMPF',IDD)
         ENDIF
         IERR = 1
         RETURN
*
      ELSEIF (IQ(LTMP-5) .NE. IDD) THEN
*
*-- find tmp bank for ntuple IDD
*
         LTMP = LQ(LCDIR-5)
   20    IF (IQ(LTMP-5) .EQ. IDD) THEN
            LTMP1 = LQ(LTMP-1)
            RETURN
         ENDIF
         IF (LQ(LTMP) .NE. 0) THEN
            LTMP = LQ(LTMP)
            GOTO 20
         ENDIF
*
*-- this may not happen either
*
         IF (FATAL) THEN
            CALL HBUG('Tmp structure not found.','HNTMPF',IDD)
         ENDIF
         IERR = 1
         RETURN
      ENDIF
*
      END
 
