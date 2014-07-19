*
* $Id: hntrng.F,v 1.1.1.1 1996/01/16 17:08:00 mclareni Exp $
*
* $Log: hntrng.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:00  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/01 26/10/94  17.36.14  by  Fons Rademakers
*-- Author :    Fons Rademakers   26/10/94
      SUBROUTINE HNTRNG(TVAR, ILOW, IUP, IER)
*.==========>
*.
*.           Return the range of an integer VAR in this N-tuple.
*.           Returns IER=0 if range exists, IER=1, if (unsigned) integer
*.           but no range and IER=2 if not an (unsigned) integer.
*.           ILOW = IUP = 0 in case IER<>0.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcbook.inc"
*
      CHARACTER*(*) TVAR
      CHARACTER*32  VAR, NAME
      LOGICAL       LDUM
*
*-- save block address (not in link area but in this routine no
*-- relocation should happen so it will be ok)
*
      LOLDBL = LBLOK
*
      VAR   = TVAR
      CALL CLTOU(VAR)
      LVAR  = LENOCC(VAR)
      LBLOK = LQ(LCID-1)
      LCHAR = LQ(LCID-2)
      LINT  = LQ(LCID-3)
      LREAL = LQ(LCID-4)
*
      ILOW = 0
      IUP  = 0
      IER  = 0
*
*-- loop over all blocks
*
   10 LNAME = LQ(LBLOK-1)
*
      NDIM = IQ(LBLOK+ZNDIM)
      IOFF = 0
*
      DO 20 I = 1, NDIM
         LL = IQ(LNAME+IOFF+ZLNAME)
         LV = IQ(LNAME+IOFF+ZNAME)
         NAME = ' '
         CALL UHTOC(IQ(LCHAR+LV), 4, NAME, LL)
         CALL CLTOU(NAME)
         IF (VAR(1:LVAR) .EQ. NAME(1:LL)) THEN
*
            CALL HNDESC(IOFF, NSUB, ITYPE, ISIZE, NBITS, LDUM)
*
            IF (ITYPE.EQ.2 .OR. ITYPE.EQ.3) THEN
               LL = IQ(LNAME+IOFF+ZRANGE)
               IF (LL .NE. 0) THEN
                  ILOW = IQ(LINT+LL)
                  IUP  = IQ(LINT+LL+1)
               ELSE
                  IER = 1
               ENDIF
            ELSE
               IER = 2
            ENDIF
*
            GOTO 30
         ENDIF
         IOFF = IOFF + ZNADDR
   20 CONTINUE
*
      LBLOK = LQ(LBLOK)
      IF (LBLOK .NE. 0) GOTO 10
*
   30 LBLOK = LOLDBL
      LNAME = LQ(LBLOK-1)
*
      END
