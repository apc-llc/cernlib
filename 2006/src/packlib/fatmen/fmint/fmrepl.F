*
* $Id: fmrepl.F,v 1.1.1.1 1996/03/07 15:18:13 mclareni Exp $
*
* $Log: fmrepl.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:13  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMREPL(GENAME,LENTRY,CHOPT,IRC)
      CHARACTER*(*) GENAME
      CHARACTER*4 COMM
      CHARACTER*1 CHOPT
      PARAMETER (LKEYFA=10)
      DIMENSION KEYS(LKEYFA)
      DIMENSION LENTRY(1)
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
*
*     Replace catalogue entry
*
      IRC = 0
      COMM='MOD '
      LGN = LENOCC(GENAME)
      IF(IDEBFA.GE.1) WRITE(LPRTFA,9001) COMM,GENAME(1:LGN)
9001  FORMAT(' FMREPL. ',A4,1X,A)
      COMM(4:4) = CHOPT
*
*     Fill KEYS vector for call to FMVERI
*
      LPATH        = INDEXB(GENAME(1:LGN),'/')
      CALL VBLANK(KEYS(2),5)
      CALL UCTOH(GENAME(LPATH+1:LGN),KEYS(2),4,LGN-LPATH)
      KEYS(MKSRFA) = 0
      KEYS(MKCLFA) = IQ(LENTRY(1)+KOFUFA+MCPLFA)
      KEYS(MKLCFA) = IQ(LENTRY(1)+KOFUFA+MLOCFA)
      KEYS(MKMTFA) = IQ(LENTRY(1)+KOFUFA+MMTPFA)
      KEYS(MKNBFA) = MKNBFA
 
      CALL FMVERI(GENAME,LENTRY(1),KEYS,'A',IRC)
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GT.-3) PRINT *,'FMREPL. errors detected by FMVERI - '
     +                           ,'entry will not be added to catalogue'
         RETURN
      ENDIF
 
      CALL FMFZO(COMM,GENAME,LENTRY(1),KEYS,IRC)
 
      END
 
