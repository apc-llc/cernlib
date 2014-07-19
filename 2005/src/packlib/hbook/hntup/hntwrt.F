*
* $Id: hntwrt.F,v 1.1.1.1 1996/01/16 17:07:58 mclareni Exp $
*
* $Log: hntwrt.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:58  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :          05/09/95  16.22.10  by  Julian Bunn
*-- Author :    Fons Rademakers   08/01/92
      SUBROUTINE HNTWRT(INDX, IOFF, IER)
*.==========>
*.
*.           Write the contents bank to RZ file or lift a new
*.           bank when Ntuple is stored in memory.
*.           For the data-structure description see routine HBNT.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcsave.inc"
*
      COMMON/QUEST/IQUEST(100)
      CHARACTER*128 CHWOLD,CHDIR,CWDRZ
      INTEGER       IDRZ(2)
*
*-- Memory bank full ?
*-- Allocate a new block if memory resident Ntuple
*-- Write current block when disk resident Ntuple and clear same block
*
      IDD = IDLAST
      IER = 0
      NWP = IQ(LR2-1)
*
5     IF (IQ(LCID+ZNPRIM) .LT. 0) THEN
*
*-- Memory resident
*
         CALL HSPACE(NWP+10,'HFNT',IDD)
         IF (IERR .NE. 0) GOTO 99
         CALL MZBOOK(IHDIV,LR2,LR2,0,'HCON',0,0,NWP,1,0)
         IQ(LNAME+IOFF+ZNRZB)  = IQ(LNAME+IOFF+ZNRZB) + 1
         IQ(LNAME+IOFF+ZIBANK) = IQ(LNAME+IOFF+ZNRZB)
         LQ(LNAME-INDX) = LR2
      ELSE
*
*-- Disk resident
*
         LCIND = IQ(LNAME+IOFF+ZLCONT)
         LB = LQ(LBUF-LCIND)
         IF (JBIT(IQ(LB),1) .EQ. 0) GOTO 10
*
*-- Before writing buffer check if header needs to be written.
*-- Only write header if bit 2 is set in status word of first block.
*-- Set number of entries to -1 in header. This header is only
*-- supposed to be used by the error recovery routine HRECOV.
*
         IF (JBIT(IQ(LQ(LCID-1)),2) .NE. 0) THEN
            N = IQ(LCID+ZNOENT)
            IQ(LCID+ZNOENT) = -1
            CALL HNHDWR(IDD)
            IQ(LCID+ZNOENT) = N
         ENDIF
*
         LB = LQ(LBUF-LCIND)
         CALL SBIT0(IQ(LB),1)
         NTSAVE  = .TRUE.
         NCHRZ   = IQ(LCID+ZNCHRZ)
         IDRZ(1) = IDD
         IDRZ(2) = IQ(LNAME+IOFF+ZNRZB)*10000 + IQ(LNAME+IOFF+ZLCONT)
         CALL RZCDIR(CWDRZ,'R')
         CALL HCDIR(CHWOLD,'R')
         CHDIR = ' '
         CALL UHTOC(IQ(LCID+ZNCHRZ+1),4,CHDIR,NCHRZ)
         IF (CHDIR .EQ. CWDRZ) THEN
            CALL HRZOUT(IHDIV,LR2,IDRZ,ICYCLE,'A')
C
C CHECK FOR AN ERROR, INDICATED BY IQUEST(1) NON-ZERO
C
            IF(IQUEST(1).NE.0) THEN
               CALL HBUG('An error has occured for '//
     &                   CHDIR(:LENOCC(CHDIR)),'HNTWRT',IQUEST(1))
               GOTO 99
            ENDIF
         ELSE
            CALL HCDIR(CHDIR,' ')
            IF (IQUEST(1) .NE. 0) THEN
               CALL HBUG('+Switch to memory mode','HNTWRT',IDD)
               IQ(LCID+ZNPRIM) = -IQ(LCID+ZNPRIM)
               GOTO 5
            ENDIF
            CALL HRZOUT(IHDIV,LR2,IDRZ,ICYCLE,'A')
C
C CHECK FOR AN ERROR, INDICATED BY IQUEST(1) NON-ZERO
C
            IF(IQUEST(1).NE.0) THEN
               CALL HBUG('An error has occured for '//
     &                   CHDIR(:LENOCC(CHDIR)),'HNTWRT',IQUEST(1))
               GOTO 99
            ENDIF
            CALL HCDIR(CHWOLD,' ')
            IF (CHWOLD .NE. CWDRZ) THEN
               CALL RZCDIR(CWDRZ,' ')
            ENDIF
         ENDIF
*
10       CALL VZERO(IQ(LR2+1), NWP)
         IQ(LNAME+IOFF+ZNRZB)  = IQ(LNAME+IOFF+ZNRZB) + 1
         IQ(LNAME+IOFF+ZIBANK) = IQ(LNAME+IOFF+ZNRZB)
      ENDIF
*
      IDLAST = IDD
      RETURN
*
  99  IER = 1
      RETURN
      END
