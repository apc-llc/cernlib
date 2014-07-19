*
* $Id: fmput.F,v 1.1.1.1 1996/03/07 15:18:10 mclareni Exp $
*
* $Log: fmput.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:10  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMPUT(GENAME,LENTRY,IRC)
      CHARACTER*(*) GENAME
      CHARACTER*4 COMM
      PARAMETER (LKEYFA=10)
      DIMENSION KEYS(LKEYFA)
      DIMENSION LENTRY(1)
#include "fatmen/fatpara.inc"
#include "fatmen/fatbank.inc"
      IRC = 0
      COMM='PUT '
      LGN = LENOCC(GENAME)
      IF(IDEBFA.GE.1) WRITE(LPRTFA,9001) COMM,GENAME(1:LGN)
9001  FORMAT(' FMPUT. ',A4,1X,A)
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
         IF(IDEBFA.GT.-3) PRINT *,'FMPUT. errors detected by FMVERI - ',
     +                           'entry will not be added to catalogue'
         RETURN
      ENDIF
 
      CALL FMFZO(COMM,GENAME,LENTRY(1),KEYS,IRC)
      END
 
