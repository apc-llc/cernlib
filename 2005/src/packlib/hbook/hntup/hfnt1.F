*
* $Id: hfnt1.F,v 1.1.1.1 1996/01/16 17:07:57 mclareni Exp $
*
* $Log: hfnt1.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:57  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :          05/09/95  16.22.10  by  Julian Bunn
*-- Author :    Fons Rademakers   06/01/92
      SUBROUTINE HFNT1(IDD, BLKNA1)
*.==========>
*.
*.           Filling of a new (variable row length) n-tuple.
*.           For the data-structure description see routine HBNT.
*.
*.  This routine loops over all blocks when BLKNA1 = '*', otherwise
*.  it only fills block BLKNA1.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
#include "hbook/hntcur.inc"
#include "hbook/hcsave.inc"
C
      COMMON /QUEST/ IQUEST(100)
*
      CHARACTER*(*) BLKNA1
      CHARACTER*8   BLKNAM, BLKSAV
      INTEGER       HNBPTR, HNBMIN
      LOGICAL       ALLBLK
*
      SAVE          BLKSAV
      DATA          BLKSAV /' '/
*
      IQUEST(1) = 0
      IERR   = 0
      NTCUR  = 0
      NTSAVE = .FALSE.
*
      IF (IDD .NE. IDLAST) THEN
         ID    = IDD
         IDPOS = LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
         IF (IDPOS .LE. 0) THEN
            CALL HBUG('N-tuple does not exist, create it with HBNT',
     +                'HFNT',IDD)
            RETURN
         ENDIF
         LCID = LQ(LTAB-IDPOS)
         I4 = JBIT(IQ(LCID+KBITS),4)
         IF (I4 .EQ. 0) RETURN
         IF (IQ(LCID-2) .NE. ZLINK) THEN
            CALL HBUG('Old N-tuple, fill it with HFN',
     +                'HFNT',IDD)
            RETURN
         ENDIF
         IDLAST = ID
         BLKSAV = ' '
      ENDIF
*
*-- create or update buffer manager structure
*
      CALL HNBUF(IDD)
      IF (IERR .NE. 0) GOTO 99
*
      BLKNAM = BLKNA1
      ALLBLK = .FALSE.
*
*-- fill all blocks or only block BLKNAM
*--
*-- to optimize the filling of one block store a pointer to
*-- this block in reference link -8
*
      IF (BLKNAM(1:1) .EQ. '*') THEN
         ALLBLK = .TRUE.
         LBLOK  = LQ(LCID-1)
***         IF (IQ(LCID+ZNOENT) .NE. HNBMIN()) THEN
***            CALL HBUG('Not all blocks contain the'//
***     +                'same number of events, use HFNTB','HFNT',IDD)
***            RETURN
***         ENDIF
      ELSEIF (BLKSAV .NE. BLKNAM) THEN
         LBLOK = HNBPTR(BLKNAM)
         IF (LBLOK .EQ. 0) THEN
            CALL HBUG('Block does not exist','HFNTB',IDD)
            RETURN
         ENDIF
         BLKSAV = BLKNAM
         LQ(LCID-8) = LBLOK
      ELSE
         LBLOK = LQ(LCID-8)
      ENDIF
*
      LCHAR  = LQ(LCID-2)
      LINT   = LQ(LCID-3)
      LREAL  = LQ(LCID-4)
*
      IF (ALLBLK) THEN
   40    CALL HFNT2
c
c The following needs un-commenting when it is understood how
c the rzcdir in HNTWRT causes iquest(1) non-zero in ostensibly
c valid cases ...
c         IF(IQUEST(1).NE.0) THEN
c            CALL HBUG('An error has occured whilst filling nT Blocks',
c     &                'HFNT1',IDD)
c            GOTO 99
c         ENDIF
         LBLOK = LQ(LBLOK)
         IF (LBLOK .NE. 0) GOTO 40
         IQ(LCID+ZNOENT) = IQ(LCID+ZNOENT) + 1
      ELSE
         CALL HFNT2
         IF(IQUEST(1).NE.0) THEN
            CALL HBUG('An error has occured whilst filling an nT Block',
     &                'HFNT1',IDD)
            GOTO 99
         ENDIF
*
*-- set global event counter equal to the lowest block event counter
*--
*-- check for lowest block event counter only when current block event
*-- counter is one larger than the global event counter
*
         IF (IQ(LBLOK+ZNOENT) .LE. IQ(LCID+ZNOENT)+1) THEN
            IQ(LCID+ZNOENT) = HNBMIN()
         ENDIF
* Instead of above we could make this test
*
*-- set global event counter equal to the lowest block event counter
*-- but never decrease the global event counter
*
***         IM = HNBMIN()
***         IF (IM .GT. IQ(LCID+ZNOENT)) IQ(LCID+ZNOENT) = IM
      ENDIF
*
*-- flush RZ file
*
      IF (NTSAVE) CALL HNTSAV
*
*-- N-tuple structure has been changed
*
      LBLOK = LQ(LCID-1)
      CALL SBIT1(IQ(LBLOK),1)
*
99    RETURN
      END
