*
* $Id: fmget.F,v 1.1.1.1 1996/03/07 15:18:24 mclareni Exp $
*
* $Log: fmget.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:24  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMGET(GENAME,L,KEYS,IRC)
      CHARACTER*(*) GENAME
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
      PARAMETER       (LKEYFA=10)
      DIMENSION       KEYS(LKEYFA)
*     CHARACTER PATHN*256,PATH*256,PATHX*256,FNAME*20
      DIMENSION LSUP(9)
      LOGICAL GETK
      GETK = .FALSE.
      NCH=LENOCC(GENAME)
      CALL CLTOU(GENAME)
      IF(IDEBFA.GE.1) WRITE(LPRTFA,9001) GENAME(1:NCH)
9001  FORMAT(' FMGET. ',A)
      GOTO 10
      ENTRY FMGETK(GENAME,L,KEYS,IRC)
      GETK = .TRUE.
      NCH=LENOCC(GENAME)
      CALL CLTOU(GENAME)
      IF(IDEBFA.GE.1) WRITE(LPRTFA,9002) GENAME(1:NCH)
9002  FORMAT(' FMGETK. ',A)
*
*     If KEYS(1) not set, then we do FMGET...
*
      IF(KEYS(1).EQ.0) THEN
        GETK = .FALSE.
        ENDIF
 
      IF(IDEBFA.GE.1) CALL FMPKEY(KEYS,10)
10    CONTINUE
      L=0
      IRC = 0
      IF(NCH.LT.3.OR.GENAME(1:2).NE.'//'.OR.GENAME(NCH:NCH).EQ.'/')THEN
          IQUEST(1)=61
          GO TO 999
          ENDIF
      ICH=INDEXB(GENAME(1:NCH-1),'/')
      IF(ICH.LE.3.OR.NCH-ICH.GT.20) THEN
          IQUEST(1)=62
          GO TO 999
          ENDIF
*
*     Modify for AIX Fortran
*
*     PATHN=GENAME(1:ICH-1)
*     FNAME=GENAME(ICH+1:NCH)
      IF(LTDSFA.NE.0) CALL MZDROP(IDIVFA,LTDSFA,'L')
      IF (.NOT. GETK) CALL VZERO(KEYS,NKDSFA)
      NWORDS = NKDSFA
      IFLAG  = 0
      IF (GETK) IFLAG = 1
      JBIAS = 1
#if !defined(CERNLIB_CZ)
*
*     Modify for AIX
*
*     CALL FTRIN(PATHN,FNAME,IDIVFA,LTDSFA,JBIAS,NWORDS,KEYS,IFLAG)
      CALL FMRZIN(GENAME(1:NCH),IDIVFA,LTDSFA,JBIAS,NWORDS,KEYS,IFLAG)
#endif
#if defined(CERNLIB_CZ)
*     This will have to be fixed too...
*     CALL FMRIN(PATHN,FNAME,IDIVFA,LTDSFA,JBIAS,NWORDS,KEYS,IFLAG)
#endif
      L=LTDSFA
 999  IRC=IQUEST(1)
*
*     Return a zero bank address if not found
*
      IF(IRC.NE.0) L=0
      RETURN
      END
 
