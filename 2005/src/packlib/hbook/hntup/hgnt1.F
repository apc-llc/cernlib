*
* $Id: hgnt1.F,v 1.1.1.1 1996/01/16 17:07:57 mclareni Exp $
*
* $Log: hgnt1.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:57  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/09 21/07/94  16.18.13  by  Fons Rademakers
*-- Author :    Fons Rademakers   29/01/92
      SUBROUTINE HGNT1(IDD,BLKNA1,VAR,IOFFST,NVAR,IDNEVT,IERROR)
*.==========>
*.
*.           Return in the preset addresses (set by HBNAME)
*.           the variables of event (row) IDNEVT.
*.
*.  This routine loops over all blocks when BLKNA1 = '*', otherwise
*.  it only retrieves data from block BLKNA1.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
#include "hbook/hntcur.inc"
*
      CHARACTER*(*) BLKNA1, VAR(*)
      CHARACTER*8   BLKNAM, BLKSAV
      INTEGER       HNBPTR, IOFFST(*)
      LOGICAL       ALLBLK
*
      SAVE     BLKSAV
      DATA     BLKSAV /' '/
*
      IERR   = 0
      IERR1  = 0
      IERROR = 0
      IF (IDD.NE.IDLAST .OR. NTCUR.EQ.0) THEN
*
*-- find in memory or read from disk N-tuple IDD
*
         CALL HPARNT(IDD,'HGNT')
         IF (IDD .EQ. 0) GOTO 20
         IDLAST = IDD
         BLKSAV = ' '
      ENDIF
      IF (LCID .LE. 0) GOTO 20
*
*-- create or find buffer manager structure for IDD
*
      CALL HNBUFR(IDD)
      IF (IERR .NE. 0) GOTO 20
*
      NTCUR = IDD
*
      IF (IDNEVT .LE. 0) GOTO 20
*
      BLKNAM = BLKNA1
      ALLBLK = .FALSE.
*
*-- read all blocks or only block BLKNAM
*--
*-- to optimize the reading of one block store a pointer to
*-- this block in reference link -8
*
      IF (BLKNAM(1:1) .EQ. '*') THEN
         ALLBLK = .TRUE.
         LBLOK  = LQ(LCID-1)
         IF (IDNEVT .GT. IQ(LCID+ZNOENT)) GOTO 20
      ELSEIF (BLKSAV .NE. BLKNAM) THEN
         LBLOK = HNBPTR(BLKNAM)
         IF (LBLOK .EQ. 0) THEN
            CALL HBUG('Block does not exist','HGNTB',IDD)
            GOTO 20
         ENDIF
         BLKSAV = BLKNAM
         LQ(LCID-8) = LBLOK
         IF (IDNEVT .GT. IQ(LBLOK+ZNOENT)) GOTO 20
      ELSE
         LBLOK = LQ(LCID-8)
         IF (IDNEVT .GT. IQ(LBLOK+ZNOENT)) GOTO 20
      ENDIF
*
      LCHAR = LQ(LCID-2)
      LINT  = LQ(LCID-3)
      LREAL = LQ(LCID-4)
*
      IQ(LTMP1+1) = 0
*
      IF (ALLBLK) THEN
   10    CALL HGNT2(VAR, IOFFST, NVAR, IDNEVT, IERROR)
         IF (IERROR .NE. 0) IERR1 = 1
         LBLOK = LQ(LBLOK)
         IF (LBLOK .NE. 0) GOTO 10
      ELSE
         CALL HGNT2(VAR, IOFFST, NVAR, IDNEVT, IERROR)
         IF (IERROR .NE. 0) IERR1 = 1
      ENDIF
*
      IF (IERR1 .EQ. 0) THEN
         IQ(LTMP+1) = IDNEVT
      ELSE
         IQ(LTMP+1) = 0
         IERROR = 2
      ENDIF
*
      RETURN
*
*-- error
*
   20 IERROR = 1
*
      END
