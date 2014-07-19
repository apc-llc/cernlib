*
* $Id: fmpeek.F,v 1.1.1.1 1996/03/07 15:18:04 mclareni Exp $
*
* $Log: fmpeek.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:04  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMPEEK(GENAME,IVECT,CHOPT,IRC)
*CMZ :          11/12/90  15.28.48  by  Jamie Shiers
*-- Author :    Jamie Shiers   11/12/90
      CHARACTER*(*) GENAME,CHOPT
      CHARACTER*255 GENOLD
#include "fatmen/fat.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatusr.inc"
      DIMENSION     IVECT(NWDSFA)
#include "fatmen/fmnkeys.inc"
#include "fatmen/fmaxcop.inc"
      DIMENSION     KEYSI(LKEYFA)
      DIMENSION     KEYSO(LKEYFA,MAXCOP)
      SAVE          NKEYS,JKEY,LGNOLD,GENOLD
#include "fatmen/fatoptd.inc"
      DATA          LGNOLD/1/,GENOLD/'.'/
#include "fatmen/fatoptc.inc"
*     CHOPT:
*            Default: return in IVECT the FATMEN bank at the
*                     current LADDBK address (created by FMADDD/T, for example)
      IRC    = 0
      LGN    = LENOCC(GENAME)
      IF(GENOLD(1:LGNOLD).NE.GENAME(1:LGN)) JKEY = 0
      LGNOLD = LGN
      GENOLD = GENAME(1:LGN)
 
      IF((IOPTG.EQ.0).AND.(IOPTA.EQ.0).AND.(IOPTN.EQ.0)) THEN
         IF(IDEBFA.GE.3) THEN
            PRINT *,'FMPEEK. Copying ',NWDSFA,' words from FATMEN bank',
     +              ' at address ',LADDBK,' into user vector'
         ENDIF
         IF(IDEBFA.GE.3)
     +      CALL FMSHOW(GENAME(1:LGN),LADDBK,KEYSO,'A',IRC)
         CALL UCOPY(IQ(LADDBK+1),IVECT,NWDSFA)
         NKEYS = 0
         JKEY  = 0
*
*            G      : Get information from FATMEN catalogue
*
      ELSEIF(IOPTG.NE.0) THEN
         CALL FMGET(GENAME,LADDBK,KEYSO,IRET)
         IF(IRET.NE.0) THEN
            PRINT *,'FMPEEK. Error from FMGET for ',GENAME(1:LGN)
            IRC = IRET
            GOTO 99
         ENDIF
         IF(IDEBFA.GE.3)
     +      CALL FMSHOW(GENAME(1:LGN),LADDBK,KEYSO,'A',IRC)
         CALL UCOPY(IQ(LADDBK+1),IVECT,NWDSFA)
*
*            A      : Get any matching entry from catalogue
*
      ELSEIF(IOPTA.NE.0) THEN
         KEYSI(MKMTFA) = -1
         KEYSI(MKLCFA) = -1
         KEYSI(MKCLFA) = -1
         CALL FMSELK(GENAME,KEYSI,KEYSO,NKEYS,MAXCOP,IRET)
         IF(IRET.NE.0) THEN
            PRINT *,'FMPEEK. Error from FMSELK for ',GENAME(1:LGN)
            IRC = IRET
            GOTO 99
         ENDIF
         CALL FMGETK(GENAME,LADDBK,KEYSO(1,1),IRET)
         IF(IRET.NE.0) THEN
            PRINT *,'FMPEEK. Error from FMGETK for ',GENAME(1:LGN)
            IRC = IRET
            GOTO 99
         ENDIF
         IF(IDEBFA.GE.3)
     +      CALL FMSHOW(GENAME(1:LGN),LADDBK,KEYSO,'A',IRC)
         CALL UCOPY(IQ(LADDBK+1),IVECT,NWDSFA)
*
*            N      : Get next matching entry from catalogue
*
      ELSEIF(IOPTN.NE.0) THEN
         KEYSI(MKMTFA) = -1
         KEYSI(MKLCFA) = -1
         KEYSI(MKCLFA) = -1
         CALL FMSELK(GENAME,KEYSI,KEYSO,NKEYS,MAXCOP,IRET)
         IF(IRET.NE.0) THEN
            PRINT *,'FMPEEK. Error from FMSELK for ',GENAME(1:LGN)
            IRC = IRET
            GOTO 99
         ENDIF
         JKEY = JKEY + 1
         IF(JKEY.GT.NKEYS) THEN
            PRINT *,'FMPEEK. No more entries match ',GENAME(1:LGN)
            PRINT *,'        # entries found = ',NKEYS
            IRC = -1
            GOTO 99
         ENDIF
         CALL FMGETK(GENAME,LADDBK,KEYSO(1,JKEY),IRET)
         IF(IRET.NE.0) THEN
            PRINT *,'FMPEEK. Error from FMGETK for ',GENAME(1:LGN)
            IRC = IRET
            GOTO 99
         ENDIF
         IF(IDEBFA.GE.3)
     +      CALL FMSHOW(GENAME(1:LGN),LADDBK,KEYSO(1,JKEY),'A',IRC)
         CALL UCOPY(IQ(LADDBK+1),IVECT,NWDSFA)
      ENDIF
 
      IF(IOPTD.NE.0) THEN
         CALL MZDROP(IXSTOR,LADDBK,' ')
      ENDIF
   99 END
