*
* $Id: pchncd.F,v 1.1.1.1 1996/03/01 11:38:44 mclareni Exp $
*
* $Log: pchncd.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:44  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.03/16 06/10/93  12.14.16  by  Fons Rademakers
*-- Author :    Fons Rademakers   24/03/93
      SUBROUTINE PCHNCD(TCHAIN, IER)
******************************************************************
*                                                                *
*  Change directory to CHAIN. This routine checks if CHAIN       *
*  exists. If yes, it positions the chain traversal routine      *
*  to the first element and opens the first element file and     *
*  changes RZ directory to this file.                            *
*  In case CHAIN does not exist IER=1.                           *
*  In case of error in opening chain IER=2.                      *
*                                                                *
******************************************************************
*
#include "paw/pawchn.inc"
#include "paw/pcntwk.inc"
#include "hbook/hcpiaf.inc"
*
      CHARACTER*(*)  TCHAIN
      CHARACTER*80   PCHAIN, CCHAIN, CHNDUM
      CHARACTER*128  MEMBER, CHWARN
      INTEGER        PISCHN, PCHCNT, PISCPF
*
      IER = 0
      IF (TCHAIN .EQ. ' ') THEN
         CALL HCDIR(PCHAIN,'R')
      ELSE
         PCHAIN = TCHAIN
      ENDIF
*
      CHAIN = .FALSE.
      CFILE = ' '
*
      IF (PCHAIN(1:2) .EQ. '//') THEN
         I = INDEX(PCHAIN(3:), '/')
         IF (I .NE. 0) THEN
            CCHAIN = PCHAIN(3:I+1)
         ELSE
            CCHAIN = PCHAIN(3:)
         ENDIF
      ELSE
         I = INDEX(PCHAIN, '/')
         IF (I .NE. 0) THEN
            CCHAIN = PCHAIN(1:I-1)
         ELSE
            CCHAIN = PCHAIN
         ENDIF
      ENDIF
*
*-- does the chain exist?
*
      LC = LENOCC(CCHAIN)
      IF (PISCHN(CCHAIN, LC) .EQ. 0) GOTO 901
*
*-- get the total number of files and luns in this chain
*
      NCHAIN = PCHCNT(CCHAIN, LC)
*
      K = PISCPF(CCHAIN, LC)
      IF (K .EQ. 1) THEN
#if defined(CERNLIB_CZ)
*
*-- remote (piaf) chain: send chain definition to piaf (if not already done)
*
         IF (CCHAIN .NE. CURCHN) CHNXMT = .TRUE.
*
         CALL PCHSET(CCHAIN, LC, MEMBER, LM)
         IF (LM .LT. 0) THEN
            MEMBER = ' '
            LM     = 1
            GOTO 5
         ENDIF
*
         IF (.NOT.CHNXMT) GOTO 5
*
*-- strip off //piaf/ from member names
*
         CHLMPF = 'nt/chain -;nt/chain '//CCHAIN(1:LC)//' '//
     +            MEMBER(8:LM)
*
   6     CALL PCHNXT(CHNDUM, LD, MEMBER, LM)
         IF (LM .GT. 0) THEN
            LL = LENOCC(CHLMPF)
            IF (LL+LM-6 .GT. LEN(CHLMPF)) THEN
               CALL PFKUIP(CHLMPF, ISTAT)
               IF (ISTAT.NE.0) THEN
                  CALL HBUG('Cannot send chain to Piaf server',
     +                      'PCHNCD',0)
                  GOTO 999
               ENDIF
               CHLMPF = 'nt/chain '//CCHAIN(1:LC)
               LL = LENOCC(CHLMPF)
            ENDIF
            CHLMPF(LL+2:) = MEMBER(8:LM)
            GOTO 6
         ENDIF
*
         CALL PFKUIP(CHLMPF, ISTAT)
         IF (ISTAT.NE.0) THEN
            CALL HBUG('Cannot send chain to Piaf server','PCHNCD',0)
            GOTO 999
         ENDIF
*
         CALL PCHSET(CCHAIN, LC, MEMBER, LM)
*
   5     CALL PCHROP(CCHAIN, MEMBER(1:LM), IER)
         IF (IER .NE. 0) GOTO 902
         CHNXMT = .FALSE.
#endif
#if !defined(CERNLIB_CZ)
         PRINT *,' PAW not compiled with communication option'
#endif
*
      ELSEIF (K .EQ. 0) THEN
*
*-- local chain: get first element from chain and prepare for chain traversal
*
         CALL PCHSET(CCHAIN, LC, MEMBER, LM)
         IF (LM .LT. 0) THEN
            MEMBER = ' '
            LM     = 1
         ENDIF
*
*-- open first element of chain to be able to see its contents
*
  10     CALL PCHROP(CCHAIN, MEMBER(1:LM), IER)
         IF (IER .NE. 0) THEN
            WRITE(CHWARN,1000) MEMBER(1:LM)
            CALL HBUG(CHWARN,'PCHNCD',0)
            CALL PCHNXT(CHNDUM, LD, MEMBER, LM)
            IF (LM .EQ. 0) THEN
               CALL PCHSET(CCHAIN, LC, MEMBER, LM)
               WRITE(CHWARN,1010) CCHAIN(1:LC)
               CALL HBUG(CHWARN,'PCHNCD',0)
               GOTO 902
            ENDIF
            GOTO 10
         ENDIF
*
      ELSE
*
*-- illegal: mixed remote and local chain
*
         WRITE(CHWARN,1020) CCHAIN(1:LC)
         CALL HBUG(CHWARN,'PCHNCD',0)
         GOTO 903
      ENDIF
*
      CHAIN  = .TRUE.
      CFILE  = MEMBER(1:LM)
      CURCHN = CCHAIN
*
      CALL HCDIR(PCHAIN, ' ')
*
      RETURN
*
901   IER = 1
      RETURN
*
902   IER = 2
      RETURN
*
903   IER = 3
      RETURN
*
1000  FORMAT('+Cannot open file ',A)
1010  FORMAT('Cannot Cdir to chain ',A)
1020  FORMAT('Chain ',A,
     +       ': mixed local and remote (piaf) chain not allowed')
*
 999  END
