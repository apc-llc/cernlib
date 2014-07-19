*
* $Id: hwbnam.F,v 1.1.1.1 1996/01/16 17:07:59 mclareni Exp $
*
* $Log: hwbnam.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:59  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.20/08 07/09/93  19.32.32  by  Fons Rademakers
*-- Author :    Fons Rademakers   28/01/92
      SUBROUTINE HWBNAM(LUN, ITRUNC)
*.==========>
*.
*.           Write HBNAME calls for the user function.
*.           Truncate variable name to ITRUNC characters.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
*
      CHARACTER*80 BNAM, BNAMC
      CHARACTER*32 NAME
      CHARACTER*8  BLKNAM
      CHARACTER*5  SID
      LOGICAL      LDUM
*
      LBLOK = LQ(LCID-1)
*
      CALL HITOC(ID, SID, LS, IERR)
      WRITE(LUN,1000) '      CALL HBNAME('//SID(1:LS)//','//
     +                ''' '',0,''$CLEAR'')'
*
*-- loop over all blocks
*
  5   LNAME = LQ(LBLOK-1)
*
      IOFF = 0
      NDIM = IQ(LBLOK+ZNDIM)
      CALL UHTOC(IQ(LBLOK+ZIBLOK), 4, BLKNAM, 8)
*
      IFBN = 0
      BNAM = '      CALL HBNAME('//SID(1:LS)//','//''''//
     +       BLKNAM(1:LENOCC(BLKNAM))//''''//','
      LPBN = LENOCC(BNAM)
*
      IFBNC = 0
      BNAMC = '      CALL HBNAMC('//SID(1:LS)//','//''''//
     +        BLKNAM(1:LENOCC(BLKNAM))//''''//','
      LPBNC = LENOCC(BNAMC)
*
      DO 10 I = 1, NDIM
         CALL HNDESC(IOFF, NSUB, ITYPE, ISIZE, NBITS, LDUM)
         LL = IQ(LNAME+IOFF+ZLNAME)
         LV = IQ(LNAME+IOFF+ZNAME)
         NAME = ' '
         CALL UHTOC(IQ(LCHAR+LV), 4, NAME, LL)
         LL = MIN(ITRUNC,LL)
*
*-- construct HBNAME statement
*
         IF (ITYPE .NE. 5) THEN
            IF (IFBN .EQ. 0) THEN
               BNAM = BNAM(1:LPBN)//NAME(1:LL)//',''$SET'')'
               IFBN = 1
            ENDIF
*
*-- construct HBNAMC statement
*
         ELSE
            IF (IFBNC .EQ. 0) THEN
               BNAMC = BNAMC(1:LPBNC)//NAME(1:LL)//',''$SET'')'
               IFBNC = 1
            ENDIF
         ENDIF
*
         IOFF = IOFF + ZNADDR
  10  CONTINUE
*
      IF (IFBN .NE. 0)  WRITE(LUN,1000) BNAM(1:LENOCC(BNAM))
      IF (IFBNC .NE. 0) WRITE(LUN,1000) BNAMC(1:LENOCC(BNAMC))
*
      LBLOK = LQ(LBLOK)
      IF (LBLOK .NE. 0) THEN
         WRITE(LUN,1000) '*'
         GOTO 5
      ENDIF
*
 1000 FORMAT(A)
*
      END
 
