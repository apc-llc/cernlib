*
* $Id: fmkeys.F,v 1.1.1.1 1996/03/07 15:18:05 mclareni Exp $
*
* $Log: fmkeys.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:05  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMKEYS(MAXDIM,MAXKEY,IFIRST,ILAST,KEYS,NKEYS,IRC)
*
************************************************************************
*
*        Returns the list of keys currently in the CWD
* Input:
*   MAXDIM   The actual first dimension of output array KEYS.   It should
*            in principle be at least equal to the number of key elements
*            NWKEY as declared to RZMDIR.
*   MAXKEY   The actual second dimension of output array KEYS
*   IFIRST   The first key vector that should be returned in KEYS
*   ILAST    The last key vector that should be returned in KEYS
* Output:
*   KEYS*    A 2-dimensional array  dimensioned KEYS(MAXDIM,MAXKEY).   It
*            will contain the key vectors associated with the CWD.
*            Its first index runs over the  key elements for a given key,
*            while its second index runs over the different keys.
*   NKEYS*   Actual number of keys present for the CWD.
*
* Called by <USER>
*
*  Author  : R.Brun DD/US/PD
*  Written : 05.04.86
*  Last mod: 29.10.90 Allow user to specify first and last key number
*                     to avoid the use of massive matrices.
*
************************************************************************
#include "zebra/rzcl.inc"
#include "zebra/rzk.inc"
      DIMENSION KEYS(MAXDIM,MAXKEY)
*
*-----------------------------------------------------------------------
*
      IRC       = 0
      IQUEST(1) = 0
      NKEYS     = 0
      IF(LQRS.EQ.0)GO TO 30
      IF(LCDIR.EQ.0)GO TO 30
      LS = IQ(KQSP+LCDIR+KLS)
      LK = IQ(KQSP+LCDIR+KLK)
      NK = IQ(KQSP+LCDIR+KNKEYS)
      NWK= IQ(KQSP+LCDIR+KNWKEY)
      NKEYS=NK
      IQUEST(11)=NK
      IQUEST(12)=NWK
      NWKEY=NWK
      IF(NWK.GT.MAXDIM)THEN
         IQUEST(1)=1
         IQUEST(11)=NKEYS
         IQUEST(12)=NWK
         NWKEY=MAXDIM
      ENDIF
*
      NDONE = 0
      DO 20 I=IFIRST,MIN(ILAST,NKEYS)
 
         NDONE = NDONE + 1
         K=LK+(NWK+1)*(I-1)
         DO 10 J=1,NWK
            IKDES=(J-1)/10
            IKBIT1=3*J-30*IKDES-2
            IF(JBYT(IQ(KQSP+LCDIR+KKDES+IKDES),IKBIT1,3).LT.3)THEN
               KEYS(J,NDONE)=IQ(KQSP+LCDIR+K+J)
            ELSE
               CALL ZITOH(IQ(KQSP+LCDIR+K+J),KEYS(J,NDONE),1)
            ENDIF
   10    CONTINUE
 
         IF(NDONE.EQ.MAXKEY) THEN
            IRC = 1
            GOTO 30
         ENDIF
 
   20 CONTINUE
*
   30 IQUEST(13) = NDONE
      END
 
