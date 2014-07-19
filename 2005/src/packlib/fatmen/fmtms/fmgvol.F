*
* $Id: fmgvol.F,v 1.1.1.1 1996/03/07 15:18:15 mclareni Exp $
*
* $Log: fmgvol.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:15  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMGVOL(GENAM,LFAT,KEYS,CHLIB,CHPOOL,CHFREE,CHOPT,IRC)
      CHARACTER*(*) GENAM
      CHARACTER*(*) CHLIB, CHPOOL, CHFREE
      CHARACTER*6   CHVSN, CHVID
#include "fatmen/fmnkeys.inc"
      DIMENSION     KEYS(LKEYFA)
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatopts.inc"
*
*     Get a tape volume that has at least NFREE megabytes from pool
*     CHPOOL. If no volume exists in this pool with enough space, a
*     volume is allocated from the pool CHFREE.
*
      IRC = 0
 
      LGN = LENOCC(GENAM)
 
      IF(LFAT.EQ.0) THEN
         CALL FMGETK(GENAM(1:LGN),LBANK,KEYS,IRC)
         IF(IRC.NE.0) THEN
            WRITE (LPRTFA,*) 'FMGVOL. Return code ',IRC,' from FMGETK'
            RETURN
         ENDIF
         LBANK = LBANK + KOFUFA
      ELSEIF(LFAT.EQ.-1) THEN
         CALL FMUPKY(GENAM(1:LGN),LADDBK,KEYS,IRC)
         LBANK = LADDBK + KOFUFA
      ELSE
         IF(IDEBFA.GE.0) WRITE(LPRTFA,* )
     +        'FMGVOL. Enter for user-supplied bank at address ',LFAT
         LBANK = LFAT + KOFUFA
      ENDIF
 
      NFREE  = IQ(LBANK+MFSZFA)
 
      IMEDIA = IQ(LBANK+MMTPFA)
 
      IF(NFREE.LE.0) THEN
         IRC = 4
         IF(IDEBFA.GE.-3) PRINT *,'FMGVOL. error - file size must '//
     +      'be set to a positive integer before calling this routine'
         GOTO 99
      ENDIF
*
*     Now try to get volume
*
      CALL FMGVID(NFREE,IMEDIA,CHLIB,CHPOOL,CHFREE,CHVSN,CHVID,IFILE,
     +            CHOPT,IRC)
*
*     Update bank with volume information if successful
*
      CALL VBLANK(IQ(LBANK+MVSNFA),2)
      CALL VBLANK(IQ(LBANK+MVIDFA),2)
 
      IF(IRC.EQ.0) THEN
         IQ(LBANK+MFSQFA) = IFILE
         CALL UCTOH(CHVSN,IQ(LBANK+MVSNFA),4,NVSNFA)
         CALL UCTOH(CHVID,IQ(LBANK+MVIDFA),4,NVIDFA)
      ENDIF
 
99    CONTINUE
 
      END
