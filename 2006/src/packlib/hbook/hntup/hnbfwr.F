*
* $Id: hnbfwr.F,v 1.1.1.1 1996/01/16 17:07:58 mclareni Exp $
*
* $Log: hnbfwr.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:58  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/07 17/12/93  17.49.48  by  Rene Brun
*-- Author :    Fons Rademakers   01/05/92
      SUBROUTINE HNBFWR(IDD)
*.==========>
*.
*.           Write all buffers from the buffer structure for N-tuple IDD.
*.           For the data-structure description see routine HBNT.
*.
*.   This routine assumes that LCID is pointing to the right N-tuple.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
*
      CHARACTER*128 CHWOLD, CHDIR, CWDRZ
      INTEGER       KEYS(2)
*
      IERR = 0
*
*-- Make sure the LBUF pointer points to the right buffer structure.
*-- If buffer structure does not exist then there is also nothing to write.
*
      CALL HNBUFF(IDD, .FALSE.)
      IF (IERR .NE. 0) GOTO 99
*
*-- goto the correct RZ directory
*
      NCHRZ = IQ(LCID+ZNCHRZ)
      IF(NCHRZ.NE.0)THEN
         CALL RZCDIR(CWDRZ,'R')
         CALL HCDIR(CHWOLD,'R')
         CHDIR = ' '
         CALL UHTOC(IQ(LCID+ZNCHRZ+1),4,CHDIR,NCHRZ)
         IF (CHDIR .NE. CWDRZ) THEN
            CALL HCDIR(CHDIR,' ')
         ENDIF
      ENDIF
*
      KEYS(1) = IDD
      KEYS(2) = 0
*
*-- loop over all blocks
*
      LBLOK = LQ(LCID-1)
      LCHAR = LQ(LCID-2)
      LINT  = LQ(LCID-3)
      LREAL = LQ(LCID-4)
*
10    LNAME  = LQ(LBLOK-1)
*
      IOFF = 0
      NDIM = IQ(LBLOK+ZNDIM)
*
*-- loop over all variables in every block
*
      DO 20 I = 1, NDIM
         LCIND = IQ(LNAME+IOFF+ZLCONT)
         LB = LQ(LBUF-LCIND)
         IF (LB .EQ. 0) GOTO 15
         IF (JBIT(IQ(LB),1) .EQ. 0) GOTO 15
         CALL SBIT0(IQ(LB),1)
         KEYS(2) = IQ(LNAME+IOFF+ZNRZB)*10000 + IQ(LNAME+IOFF+ZLCONT)
*
         IF (IQ(LCID+ZNPRIM) .GT. 0) THEN
            CALL HRZOUT(IHDIV,LB,KEYS,ICYCLE,'A')
         ELSE
            CALL HRZOUT(IHDIV,LB,KEYS,ICYCLE,'LA')
         ENDIF
*
15       IOFF = IOFF + ZNADDR
20    CONTINUE
*
      LBLOK = LQ(LBLOK)
      IF (LBLOK .NE. 0) GOTO 10
*
*-- set header save bit if at least one buffer has been written
*
      IF (KEYS(2) .NE. 0) CALL SBIT1(IQ(LQ(LCID-1)),1)
*
      IF (NCHRZ.NE.0.AND.CHDIR .NE. CWDRZ) THEN
         CALL HCDIR(CHWOLD,' ')
         IF (CHWOLD .NE. CWDRZ) THEN
            CALL RZCDIR(CWDRZ,' ')
         ENDIF
      ENDIF
*
99    RETURN
      END
