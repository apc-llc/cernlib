*
* $Id: hprnts.F,v 1.1.1.1 1996/01/16 17:07:59 mclareni Exp $
*
* $Log: hprnts.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:59  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.20/08 07/09/93  19.30.59  by  Fons Rademakers
*-- Author :    Fons Rademakers   06/01/92
      SUBROUTINE HPRNTS
*.==========>
*.
*.           Print space usage statistics.
*.           This routine assumes that LCID is correctly set.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcnt.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcunit.inc"
*
      CHARACTER*6  SNPW, SNPWT
      CHARACTER*8  BLKNAM, SNW, SNWT, SNOENT
      LOGICAL      VARSIZ, VARTOT, INDVAR
*
      LBLOK = LQ(LCID-1)
      LCHAR = LQ(LCID-2)
      LINT  = LQ(LCID-3)
      LREAL = LQ(LCID-4)
*
      VARTOT = .FALSE.
      NWTOT  = 0
      NPWTOT = 0
*
      WRITE(LOUT,1000)
*
*-- loop over all blocks
*
  5   LNAME = LQ(LBLOK-1)
*
      NW   = 0
      NPW  = 0
      VARSIZ = .FALSE.
      IOFF = 0
      NDIM = IQ(LBLOK+ZNDIM)
      CALL UHTOC(IQ(LBLOK+ZIBLOK), 4, BLKNAM, 8)
*
      DO 10 I = 1, NDIM
         CALL HNDESC(IOFF, NSUB, ITYPE, ISIZE, NBITS, INDVAR)
*
         IF (INDVAR) THEN
            VARSIZ = .TRUE.
            VARTOT = .TRUE.
         ENDIF
*
         IELEM = 1
         DO 20 J = 1, NSUB
            LP = IQ(LINT+IQ(LNAME+IOFF+ZARIND)+(J-1))
            IF (LP .LT. 0) THEN
               IE = -LP
            ELSE
               LL = IQ(LNAME+LP-1+ZRANGE)
               IE = IQ(LINT+LL+1)
            ENDIF
            IELEM = IELEM*IE
20       CONTINUE
*
         NW = NW + IELEM*ISIZE
*
         NPW = NPW + IELEM*NBITS
*
         IOFF = IOFF + ZNADDR
10    CONTINUE
*
      NPW = FLOAT(NPW)/IBIPB + (IBIPB-1.)/IBIPB
      CALL HITOC(NW, SNW, LL, IERR)
      CALL HITOC(NPW, SNPW, LL, IERR)
      CALL HITOC(IQ(LBLOK+ZNOENT), SNOENT, LL, IERR)
*
      IF (VARSIZ) THEN
         WRITE(LOUT,2000) BLKNAM, SNOENT, SNW, 'Var.  '
      ELSE
         IF (NPW .EQ. 0) THEN
            RP = 0.0
         ELSE
            RP = FLOAT(NW)/NPW
         ENDIF
         WRITE(LOUT,2500) BLKNAM, SNOENT, SNW, SNPW, RP
      ENDIF
*
      NWTOT  = NWTOT + NW
      NPWTOT = NPWTOT + NPW
*
      LBLOK = LQ(LBLOK)
      IF (LBLOK .NE. 0) GOTO 5
*
      CALL HITOC(NWTOT, SNWT, LL, IERR)
      CALL HITOC(NPWTOT, SNPWT, LL, IERR)
      IF (VARTOT) THEN
         WRITE(LOUT,3000) SNWT, 'Var.  '
      ELSE
         IF (NPWTOT .EQ. 0) THEN
            RP = 0.0
         ELSE
            RP = FLOAT(NWTOT)/NPWTOT
         ENDIF
         WRITE(LOUT,3500) SNWT, SNPWT, RP
      ENDIF
*
1000  FORMAT(
     + ' *********************************************************',
     + '*********',/
     + ' *  Block   *  Entries  * Unpacked * Packed *   Packing Fact',
     + 'or    *',/
     + ' *********************************************************',
     + '*********')
2000  FORMAT(
     + ' * ',A,' *  ',A,' * ',A,' * ',A,' *    Variable         *')
2500  FORMAT(
     + ' * ',A,' *  ',A,' * ',A,' * ',A,' *    ',F8.3,'         *')
3000  FORMAT(
     + ' * Total    *    ---    * ',A,' * ',A,' *    Variable',
     + '         *')
3500  FORMAT(
     + ' * Total    *    ---    * ',A,' * ',A,' *    ',F8.3,
     + '         *')
*
      END
