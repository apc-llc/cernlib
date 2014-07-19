*
* $Id: hntvar.F,v 1.1.1.1 1996/01/16 17:07:58 mclareni Exp $
*
* $Log: hntvar.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:58  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/06 27/06/94  17.41.54  by  Fons Rademakers
*-- Author :    Fons Rademakers   14/08/92
      SUBROUTINE HNTVAR(ID1,IVAR,CHTAG,BLOCK,NSUB,ITYPE,ISIZE,IELEM)
*.==========>
*.
*.           Returns the tag, block, type, size and array length of the
*.           variable with index IVAR in N-tuple ID1.
*.           N-tuple must already be in memory.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
*
      CHARACTER*(*)  CHTAG, BLOCK
      CHARACTER*32   NAME
      LOGICAL        NEWTUP, LDUM
*
      ID    = ID1
      IDPOS = LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
      IF (IDPOS .LE. 0) THEN
         CALL HBUG('Unknown N-tuple','HNTVAR',ID1)
         RETURN
      ENDIF
      LCID  = LQ(LTAB-IDPOS)
      I4 = JBIT(IQ(LCID+KBITS),4)
      IF (I4 .EQ. 0) RETURN
      NEWTUP = .TRUE.
      IF (IQ(LCID-2) .NE. ZLINK) NEWTUP = .FALSE.
*
      CHTAG = ' '
      NAME  = ' '
      BLOCK = ' '
      NSUB  = 0
      ITYPE = 0
      ISIZE = 0
      IELEM = 0
*
      ICNT  = 0
*
      IF (NEWTUP) THEN
*
         IF (IVAR .GT. IQ(LCID+ZNDIM)) RETURN
*
         LBLOK = LQ(LCID-1)
         LCHAR = LQ(LCID-2)
         LINT  = LQ(LCID-3)
         LREAL = LQ(LCID-4)
*
*-- loop over all blocks
*
  5      LNAME = LQ(LBLOK-1)
*
         IOFF = 0
         NDIM = IQ(LBLOK+ZNDIM)
*
         DO 10 I = 1, NDIM
            ICNT = ICNT + 1
            IF (ICNT .EQ. IVAR) THEN
*
               CALL HNDESC(IOFF, NSUB, ITYPE, ISIZE, NBITS, LDUM)
*
               LL = IQ(LNAME+IOFF+ZLNAME)
               LV = IQ(LNAME+IOFF+ZNAME)
               CALL UHTOC(IQ(LCHAR+LV), 4, NAME, LL)
               CALL UHTOC(IQ(LBLOK+ZIBLOK), 4, BLOCK, 8)
*
               IELEM = 1
               DO 25 J = 1, NSUB
                  LP = IQ(LINT+IQ(LNAME+IOFF+ZARIND)+(J-1))
                  IF (LP .LT. 0) THEN
                     IE = -LP
                  ELSE
                     LL = IQ(LNAME+LP-1+ZRANGE)
                     IE = IQ(LINT+LL+1)
                  ENDIF
                  IELEM = IELEM*IE
   25          CONTINUE
*
               CHTAG = NAME
               RETURN
*
            ENDIF
*
            IOFF = IOFF + ZNADDR
  10     CONTINUE
*
         LBLOK = LQ(LBLOK)
         IF (LBLOK .NE. 0) GOTO 5
*
      ELSE
*
         IF (IVAR .GT. IQ(LCID+2)) RETURN
         ITAG1 = IQ(LCID+10)
         CALL UHTOC(IQ(LCID+ITAG1+2*(IVAR-1)), 4, NAME, 8)
         CHTAG = NAME
         ITYPE = 1
         ISIZE = 4
         IELEM = 1
*
      ENDIF
*
      END
