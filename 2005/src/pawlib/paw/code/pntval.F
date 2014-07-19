*
* $Id: pntval.F,v 1.1.1.1 1996/03/01 11:38:46 mclareni Exp $
*
* $Log: pntval.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:46  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.05/15 21/07/94  18.27.05  by  Fons Rademakers
*-- Author :    Fons Rademakers   06/10/92
      SUBROUTINE PNTVAL(IEVT, IVAR, DVAL, IVAL, C32, PNTV)
*.==========>
*.
*.           Returns the value for the IEVT'th event of the IVAR'th
*.           Ntuple column. When the type of IVAR is 5 (char) then
*.           C32 contains on return the string.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcbook.inc"
#include "paw/pawidn.inc"
#include "hbook/hcntpaw.inc"
#include "paw/pntold.inc"
#include "paw/pawcfu.inc"
*
      CHARACTER*(*) C32
      LOGICAL       LOGIC
      INTEGER       ILOGI
      REAL*8        DPR, DVAL
      INTEGER       ISPR(2)
      EQUIVALENCE  (ISPR(1), DPR)
      EQUIVALENCE  (LOGIC, ILOGI)
*
      IF (NTOLD) THEN
         IF (IOFFST(IVAR) .EQ. 0) THEN
            PNTV = X(INDX(IVAR))
         ELSE
            PNTV = Q(IOFFST(IVAR)+IEVT)
         ENDIF
      ELSE
         IF (ITYPE(IVAR) .EQ. 1) THEN
            IF (ISIZE(IVAR) .EQ. 4) THEN
               IF (IOFFST(IVAR) .EQ. 0) THEN
                  PNTV = RVAR4(INDX(IVAR))
               ELSE
                  PNTV = Q(IOFFST(IVAR)+IEVT)
               ENDIF
            ELSEIF (ISIZE(IVAR) .EQ. 8) THEN
               IF (IOFFST(IVAR) .EQ. 0) THEN
                  DVAL = RVAR8(INDX(IVAR))
               ELSE
                  ISPR(1) = IQ(IOFFST(IVAR)+2*(IEVT-1)+1)
                  ISPR(2) = IQ(IOFFST(IVAR)+2*(IEVT-1)+2)
                  DVAL    = DPR
               ENDIF
               PNTV  = SNGL(DVAL)
            ENDIF
         ELSEIF (ITYPE(IVAR).EQ.2 .OR. ITYPE(IVAR).EQ.3) THEN
            IF (IOFFST(IVAR) .EQ. 0) THEN
               IVAL = IVAR4(INDX(IVAR))
            ELSE
               IVAL = IQ(IOFFST(IVAR)+IEVT)
            ENDIF
            PNTV = REAL(IVAL)
         ELSEIF (ITYPE(IVAR) .EQ. 4) THEN
            IF (IOFFST(IVAR) .EQ. 0) THEN
               IF (LVAR4(INDX(IVAR))) THEN
                  IVAL = 1
               ELSE
                  IVAL = 0
               ENDIF
            ELSE
               ILOGI = IQ(IOFFST(IVAR)+IEVT)
               IF (LOGIC) THEN
                  IVAL = 1
               ELSE
                  IVAL = 0
               ENDIF
            ENDIF
            PNTV = REAL(IVAL)
         ELSEIF (ITYPE(IVAR) .EQ. 5) THEN
            IF (IOFFST(IVAR) .EQ. 0) THEN
               C32 = CVAR32(INDX(IVAR))
            ELSE
               MXBY = ISHFT(ISIZE(IVAR),-2)
               C32 = ' '
               CALL UHTOC(IQ(IOFFST(IVAR)+MXBY*(IEVT-1)+1), 4, C32,
     +                    ISIZE(IVAR))
            ENDIF
            PNTV = 0.0
         ENDIF
      ENDIF
*
      END
