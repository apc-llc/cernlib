*
* $Id: fmget.F,v 1.2 1996/09/11 16:08:53 jamie Exp $
*
* $Log: fmget.F,v $
* Revision 1.2  1996/09/11 16:08:53  jamie
* remote duplicate parameter lkeyfa
*
* Revision 1.1.1.1  1996/03/07 15:18:10  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMGET(GENAME,L,KEYS,IRC)
      CHARACTER*(*) GENAME
#include "fatmen/faust.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatkey.inc"
      DIMENSION       KEYS(LKEYFA)
 
      NCH=LENOCC(GENAME)
      CALL CLTOU(GENAME)
 
      NFGET = NFGET + 1
 
      L   = 0
      IRC = 0
 
      IF(IDEBFA.GE.2) PRINT *,'FMGET. enter for ',GENAME(1:NCH)
 
      IF(NCH.LT.3.OR.GENAME(1:2).NE.'//'.OR.GENAME(NCH:NCH).EQ.'/')THEN
          IQUEST(1) = 61
          IRC       = 61
          RETURN
       ENDIF
 
      ICH=INDEXB(GENAME(1:NCH-1),'/')
      IF(ICH.LE.3.OR.NCH-ICH.GT.20) THEN
          IQUEST(1) = 62
          IRC       = 62
          RETURN
       ENDIF
 
      IF(LTDSFA.NE.0) THEN
         CALL MZDROP(IDIVFA,LTDSFA,'L')
         LTDSFA = 0
      ENDIF
 
      CALL VZERO(KEYS,LKEYFA)
*
*     Has a keys matrix been supplied?
*
      IF(NUMKEY.EQ.0) THEN
 
         NWORDS = NKDSFA
 
         IFLAG  = 0
         JBIAS  = 1
         CALL FMRZIN(GENAME(1:NCH),IDIVFA,LTDSFA,JBIAS,
     +               NWORDS,KEYS,IFLAG)
         L      = LTDSFA
         IRC    = IQUEST(1)
*
*     Return a zero bank address if not found
*
         IF(IRC.NE.0) L=0
         RETURN
      ELSE
         CALL FMSELM(GENAME(1:NCH),L,KEYS,MFMKEY,NUMKEY,CHKEY,IRC)
      ENDIF
 
      END
 
