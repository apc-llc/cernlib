*
* $Id: cstadv.F,v 1.1.1.1 1996/02/26 17:16:31 mclareni Exp $
*
* $Log: cstadv.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:31  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 09/11/94  11.53.32  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSTADV(IPADDR)
***------------------------------------
*    Try add var to list of used
***------------------------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstab.inc"
#include "comis/cscbwl.inc"
*
*- before call was   CALL CSLDLI(I)
*- and   ICBWL.GT.0  NUMGI.GT.0  LXXGLI.NE.0 IQ(NUMGI+1).LE.2
*
      I=ICBWL
*
*- try find corresp. common blocks list
*-  element=( next, proglist, baseaddr, nchs, cbname )
*
   10 IF(I.EQ.0)RETURN
      IF(IQ(I+2).NE.NUMGI)THEN
         I=IQ(I)
         GO TO 10
      ENDIF
*
*- try find corr. prog list
*-  element=( next, varlist, progaddr )
*
      IPLP=I+1
   15 I=IQ(IPLP)
      IF(I.GT.0)THEN
        IF(IQ(I+2).EQ.IPADDR)GO TO 16
        IPLP=I
        GO TO 15
      ENDIF
*
*- not found, add to prog list
*
      IND=MHLOC(3)
      IQ(IPLP) =IND
      IQ(IND)  =0
      IQ(IND+1)=0
      IQ(IND+2)=IPADDR
      I=IND
*
*-  element=( next, offset, nel, lenel )
*-  first try find element
*
   16 IVLP=I+1
   20 I=IQ(IVLP)
      IF(I.GT.0)THEN
         IF(IQ(I+1).LT.ISHGI)THEN
            IVLP=I
            GO TO 20
         ELSEIF(IQ(I+1).EQ.ISHGI)THEN
           IF(MODEGI.GT.1)THEN
             NDIM=IQ(MODEGI+3)
             IQ(I+2)=IQ(MODEGI+NDIM+3)
           ENDIF
           RETURN
         ENDIF
      ENDIF
* add element
      IND=MHLOC(4)
      IQ(IVLP) =IND
      IQ(IND)  =I
      IQ(IND+1)=ISHGI
      IQ(IND+2)=1
      IQ(IND+3)=LENEGI
      IF(MODEGI.GT.1)THEN
         NDIM=IQ(MODEGI+3)
         IQ(IND+2)=IQ(MODEGI+NDIM+3)
      ENDIF
**      print *,' elem added: addr,offs=',ipaddr,ishgi
      END
