*
* $Id: faextr.F,v 1.1.1.1 1996/03/07 15:18:06 mclareni Exp $
*
* $Log: faextr.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:06  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FAEXTR(GENAM,KEYS,IRC)
      CHARACTER*(*) GENAM
*CMZ :          03/09/91  17.30.54  by  Jamie Shiers
*-- Author :    Jamie Shiers   03/09/91
#include "fatmen/fmnkeys.inc"
      DIMENSION    KEYS(LKEYFA)
      DIMENSION    IOCH(80),IUHEAD(80)
      CHARACTER*20 FNAME
      PARAMETER    (NW=80)
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
      SAVE         NENTRY,IOCH
#include "fatmen/faexcm.inc"
 
      DATA         NENTRY/0/
 
      IF(NENTRY.EQ.0) THEN
*
*     Set up descriptor of header vector
*
         CALL MZIOCH(IOCH,NW,'70H 10I')
         NENTRY = 1
      ENDIF
 
      IRC = 0
      LGN = LENOCC(GENAM)
 
      IF(IDEBFA.GE.2) PRINT *,'FAEXTR. processing ',
     +   GENAM(1:LGN)
      NFILES = NFILES + 1
 
      CALL FMGETK(GENAM(1:LGN),LBANK,KEYS,IRC)
      IF(IRC.EQ.0) THEN
*
*     Fill with blanks for safety
*
         CALL VBLANK(IUHEAD,70)
*
*     Fill header vector
*
         CALL UCTOH('MOD ',IUHEAD,4,4)
 
         CALL UCTOH(GENAM,IUHEAD(2),4,LGN)
*
*     Keys
*
         LEND = INDEXB(GENAM,'/') + 1
         FNAME = GENAM(LEND:LGN)
         IUHEAD(71) = KEYS(1)
         IUHEAD(77) = IQ(LBANK+MCPLFA+KOFUFA)
         IUHEAD(78) = IQ(LBANK+MMTPFA+KOFUFA)
         IUHEAD(79) = IQ(LBANK+MLOCFA+KOFUFA)
         IUHEAD(80) = LKEYFA
         LENFN = LGN-LEND+1
*
*     IUHEAD 71-80 contains the keys, which includes the filename
*
         DO 2 I=LENFN+1,20
    2    FNAME(I:I) = ' '
         CALL UCTOH(FNAME,IUHEAD(72),4,20)
         CALL FZOUT(LUFZFA,IDIVFA,LBANK,1,'S',IOCH,NW,IUHEAD)
         CALL MZDROP(IDIVFA,LBANK,' ')
         LBANK = 0
      ELSE
         IRC  = 0
         NBAD = NBAD + 1
      ENDIF
 
      END
