*
* $Id: cspdls.F,v 1.1.1.1 1996/02/26 17:16:33 mclareni Exp $
*
* $Log: cspdls.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:33  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 13/01/95  11.22.56  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
       SUBROUTINE CSPDLS(IADDR)
***--------------------------------------------
*   routine for create the parameters desc list
***--------------------------------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstab.inc"
#include "comis/cstabps.inc"
#include "comis/cspnts.inc"
      PARAMETER (KSDIM=3)
      LIST=0
      LE=0
      I=IPLI
  1   IF(I.EQ.0)GO TO 2
      CALL CSLDLI(I)
      IF(NUMGI.EQ.-1)THEN
        NDIM=0
        IF(MODEGI.GT.1)NDIM=IQ(MODEGI+KSDIM)
        IND=MHLOC(NDIM+4)
        IQ(IND)=0
        IQ(IND+1)=IABS(ITYPGI)
        IQ(IND+2)=LENEGI*4
        IF(IABS(ITYPGI).EQ.3)IQ(IND+2)=LENEGI
        IQ(IND+3)=NDIM
        ISP=1
        DO K=1,NDIM
          ISI=IQ(MODEGI+KSDIM+K)/ISP
          IF(ISI.EQ.100000)ISI=0
          IQ(IND+K+3)=ISI
          IF(ISI.NE.0)ISP=ISI
        ENDDO
        IF(LIST.EQ.0)THEN
          LIST=IND
        ELSE
          IQ(LE)=IND
        ENDIF
        LE=IND
      ELSEIF(NUMGI.GE.0)THEN
        GO TO 2
      ENDIF
      I=IQ(I)
      GO TO 1
  2   IF(LIST.EQ.0)LIST=-1
      IF(IQ(IADDR+KSDESP).GT.0)CALL CSLFRE(IQ(IADDR+KSDESP))
      IQ(IADDR+KSDESP)=LIST
      END
