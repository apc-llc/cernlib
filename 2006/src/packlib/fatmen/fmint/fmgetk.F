*
* $Id: fmgetk.F,v 1.1.1.1 1996/03/07 15:18:12 mclareni Exp $
*
* $Log: fmgetk.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:12  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMGETK(GENAME,L,KEYS,IRC)
#include "fatmen/faust.inc"
      CHARACTER*(*) GENAME
      CHARACTER*20  FNAME1,FNAME2
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
      PARAMETER       (LKEYFA=10)
      DIMENSION       KEYS(LKEYFA)
 
      NCH=LENOCC(GENAME)
      CALL CLTOU(GENAME)
 
      NFGETK = NFGETK + 1
 
      L   = 0
      IRC = 0
 
      IF(IDEBFA.GE.2) THEN
         PRINT *,'FMGETK. enter for ',GENAME(1:NCH)
         CALL FMPKEY(KEYS,LKEYFA)
      ENDIF
 
      IF(NCH.LT.3.OR.GENAME(1:2).NE.'//'.OR.GENAME(NCH:NCH).EQ.'/')THEN
          IQUEST(1)=61
          GO TO 999
       ENDIF
 
      ICH=INDEXB(GENAME(1:NCH-1),'/')
      IF(ICH.LE.3.OR.NCH-ICH.GT.20) THEN
          IQUEST(1)=62
          GO TO 999
       ENDIF
 
      IF(LTDSFA.NE.0) THEN
         CALL MZDROP(IDIVFA,LTDSFA,'L')
         LTDSFA = 0
      ENDIF
 
      NWORDS = NKDSFA
      IFLAG  = 1
      JBIAS  = 1
 
      IF(KEYS(1).EQ.0) THEN
         IFLAG = 0
         CALL VZERO(KEYS,LKEYFA)
      ELSE
*
*     Check if file name in keys vector matches that in generic name
*
         FNAME1 = GENAME(ICH+1:NCH)
         LFN1   = NCH - ICH
         CALL UHTOC(KEYS(MKFNFA),4,FNAME2,(MKCLFA-MKFNFA)*4)
         LFN2   = LENOCC(FNAME2)
         IF(FNAME1(1:LFN1).NE.FNAME2(1:LFN2)) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMGETK. file name in ',
     +         'keys vector (',FNAME2(1:LFN2),') does not ',
     +         'match that in generic name (',FNAME1(1:LFN1),')'
            IQUEST(1) = -1
            GOTO 999
         ENDIF
      ENDIF
 
      CALL FMRZIN(GENAME(1:NCH),IDIVFA,LTDSFA,JBIAS,NWORDS,KEYS,IFLAG)
      L      = LTDSFA
 999  IRC    = IQUEST(1)
*
*     Return a zero bank address if not found
*
      IF(IRC.NE.0) L=0
      RETURN
      END
 
