*
* $Id: hntrd.F,v 1.1.1.1 1996/01/16 17:07:58 mclareni Exp $
*
* $Log: hntrd.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:58  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/08 11/02/94  16.52.42  by  Fons Rademakers
*-- Author :    Fons Rademakers   30/01/92
      SUBROUTINE HNTRD(INDX, IOFF, IBANK, IERROR)
*.==========>
*.
*.           Read the IBANK contents buffer from RZ file or from memory.
*.           For the data-structure description see routine HBNT.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcrecv.inc"
*
      COMMON/QUEST/IQUEST(100)
      CHARACTER*128 CHWOLD, CHDIR, CWDRZ
      INTEGER       KEYS(2)
*
      IF (IQ(LNAME+IOFF+ZIBANK) .EQ. IBANK) THEN
         LR2 = LQ(LNAME-INDX)
         RETURN
      ENDIF
*
      IERROR = 0
      IDD    = IQ(LBUF-5)
      LCIND  = IQ(LNAME+IOFF+ZLCONT)
*
*           Load bank containing this event
*
      IF (IQ(LCID+ZNPRIM) .LT. 0) THEN
         LR2 = LQ(LBUF-LCIND)
         DO 10 I = 2, IBANK
            IF (LQ(LR2) .NE. 0) LR2 = LQ(LR2)
  10     CONTINUE
         IF (LR2 .EQ. 0) THEN
            CALL HBUG('Bank does not exist', 'HGNT', IDD)
            GOTO 90
         ENDIF
      ELSE
         IF (.NOT.NRECOV .AND. IBANK.GT.IQ(LNAME+IOFF+ZNRZB)) THEN
            CALL HBUG('Bank does not exist', 'HGNT', IDD)
            GOTO 90
         ENDIF
*
*-- goto the RZ directory where this N-tuple is stored
*
         NCHRZ = IQ(LCID+ZNCHRZ)
         IF(NCHRZ.NE.0)THEN
            CALL RZCDIR(CWDRZ,'R')
            CALL HCDIR(CHWOLD,'R')
            CHDIR = ' '
            CALL UHTOC(IQ(LCID+ZNCHRZ+1),4,CHDIR,NCHRZ)
            IF (CHDIR.NE.CWDRZ) THEN
               CALL HCDIR(CHDIR,' ')
            ENDIF
         ENDIF
*
         KEYS(1) = IQ(LCID+ZID)
         KEYS(2) = IBANK*10000 + IQ(LNAME+IOFF+ZLCONT)
*
         IF (NRECOV) THEN
            CALL RZINK(KEYS,99999,'R')
            IF (IQUEST(1) .NE. 0) GOTO 90
            IQ(LNAME+IOFF+ZNRZB) = IBANK
*
*-- if index variable we have to read the buffer
*
            IF (JBIT(IQ(LNAME+IOFF+ZDESC),28) .EQ. 1) THEN
               CALL HRZIN(IHDIV,LBUF,-LCIND,KEYS,99999,'R')
               IF (IQUEST(1) .NE. 0) GOTO 90
            ENDIF
         ELSE
*
            CALL HRZIN(IHDIV,LBUF,-LCIND,KEYS,99999,'R')
*-- Let us make another tentative (if written by a bad version of HBOOK)
            IF (IQUEST(1) .NE. 0) THEN
               KEYS(1)   = 0
               IQUEST(1) = 0
               CALL HRZIN(IHDIV,LBUF,-LCIND,KEYS,99999,'R')
            ENDIF
            IF (IQUEST(1) .NE. 0) GOTO 90
*-- Status word should be 0 when buffer comes from disk. However, in one
*-- strange case (when coming from Piaf) it was not 0. Just to make sure
*-- it is 0 add following statement.
            IQ(LQ(LBUF-LCIND)) = 0
         ENDIF
*
*-- go back to current directory
*
         IF (NCHRZ.NE.0.AND.CHDIR .NE. CWDRZ) THEN
            CALL HCDIR(CHWOLD,' ')
            IF (CHWOLD .NE. CWDRZ) THEN
               CALL RZCDIR(CWDRZ,' ')
            ENDIF
         ENDIF
*
         LR2 = LQ(LBUF-LCIND)
*
      ENDIF
*
      IQ(LNAME+IOFF+ZIBANK) = IBANK
      LQ(LNAME-INDX) = LR2
*
      RETURN
*
*-- error
*
90    IERROR = 1
*
99    END
