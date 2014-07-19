*
* $Id: hvxist.F,v 1.1.1.1 1996/01/16 17:07:59 mclareni Exp $
*
* $Log: hvxist.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:59  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/04 06/06/94  20.02.51  by  Fons Rademakers
*-- Author :    Fons Rademakers   19/12/91
      SUBROUTINE HVXIST(TVAR, BLOCK, CIVAR, ITYPE, ISIZE, IELEM)
*.==========>
*.
*.           Checks  if VAR exists in this N-tuple.
*.           Returns IELEM=0 if it does not exist.
*.           Also returns the block, name of index variable, type, size
*.           and array length of the variable.
*.           These fields are ' ' or 0 when var does not exist.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcbook.inc"
*
      CHARACTER*(*) TVAR, BLOCK, CIVAR
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
      BLOCK = ' '
      CIVAR = ' '
      ITYPE = 0
      ISIZE = 0
      IELEM = 0
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
            CALL UHTOC(IQ(LBLOK+ZIBLOK), 4, BLOCK, 8)
*
            CALL HNDESC(IOFF, NSUB, ITYPE, ISIZE, NBITS, LDUM)
            IELEM = 1
            DO 25 J = 1, NSUB
               LP = IQ(LINT+IQ(LNAME+IOFF+ZARIND)+(J-1))
               IF (LP .LT. 0) THEN
                  IE = -LP
               ELSE
                  LL = IQ(LNAME+LP-1+ZRANGE)
                  IE = IQ(LINT+LL+1)
                  LL = IQ(LNAME+LP-1+ZLNAME)
                  LV = IQ(LNAME+LP-1+ZNAME)
                  CALL UHTOC(IQ(LCHAR+LV), 4, CIVAR, LL)
               ENDIF
               IELEM = IELEM*IE
   25       CONTINUE
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
