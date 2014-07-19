*
* $Id: csimpl.F,v 1.1.1.1 1996/02/26 17:16:29 mclareni Exp $
*
* $Log: csimpl.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:29  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/00 16/02/94  15.25.03  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSIMPL(IPLI,MTYPE,MLENEL)
#include "comis/cstab.inc"
#include "comis/cstabps.inc"
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/csichv.inc"
      COMMON /CSGSCM/IGSST,JGSST,NGSST,NGSPAR,JGSSB,GSNLAB
      INTEGER CSKNCH,CSILET,CSKIDN,CSLIMP
      PARAMETER (  KTINT=1,   KLINT=1, KTREAL=2,  KLREAL=1,
     +            KTCHAR=3,   KTLOG=4, KLLOG=1,   KTDOU=5,
     +            KTHOLL=6,   KTCMLX=7,KLCMLX=2 )
      K=ICHCOMM
 1018 IF(K.EQ.ICHCOMM)THEN
        K=CSKNCH(JGSST,NGSST)
        L1=CSILET(K)
        IF(L1.EQ.0)GO TO 1
        L2=L1
        K=CSKNCH(JGSST,NGSST)
        IF(K.EQ.ICHMINU)THEN
          K=CSKNCH(JGSST,NGSST)
          L2=CSILET(K)
          IF(L2.EQ.0)GO TO 1
        ELSE
          JGSST=JGSST-1
          NGSST=NGSST+1
        ENDIF
        IF(CSLIMP(L1,L2,MTYPE,MLENEL).EQ.0)GO TO 1
        K=CSKNCH(JGSST,NGSST)
        GO TO 1018
      ENDIF
      IGSST=1
      JGSST=JGSST-1
      NGSST=NGSST+1
* CORRECT TYPE AND LENGTH OF ELEMENT
      I=IPLI
*----      J=MJCHAR(IDGI(1))
 1019 IF(I.EQ.0)GO TO 1
      CALL CSLDLI(I)
      IF(ITYPGI.LT.0)THEN
        ITYPGI=CSKIDN(MJCHAR(IQ(I+KSIDL)),LENEGI)
        IF(IABS(ITYPGI).EQ.KTCHAR)THEN
         IF(NUMGI.EQ.-2 .OR. MODEGI.EQ.-2)GO TO 711
*ERR   NO CHAR FUNCTION
        ENDIF
        CALL CSRTLI(I)
      ENDIF
      I=IQ(I)
      GO TO 1019
  711 IGSST=-11
  1   END
