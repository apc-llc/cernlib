*
* $Id: csexgb.F,v 1.1.1.1 1996/02/26 17:16:16 mclareni Exp $
*
* $Log: csexgb.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:16  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 09/11/94  11.57.45  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSEXGB
***--------------------
*      allocates new memory block for global blocks
***--------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstabps.inc"
#include "comis/cstab.inc"
#include "comis/cspnts.inc"
#include "comis/cssysd.inc"
      IT=IPGB
      IEXTGB=0
  1   IF(IT.EQ.0)RETURN
      LENGB=-IQ(IT+KSLENB)
      IF(LENGB.GT.0)THEN
         IADGB=IQ(IT+KSADGB)
         I=MHLOC(LENGB+2)
         IQ(I)=LENGB
         IQ(IT+KSADGB)=LOCF(IQ(I+2))
         IQ(IT+KSLENB)=LENGB
         IF(IADGB.EQ.0)THEN
           IQ(I+1)=0
         ELSE
           IP=IADGB-LOCF(IQ(2))
           LENP=IQ(IP)
           ID=IQ(IP+1)
           CALL CCOPYA(IQ(IP+2),IQ(I+2),LENP)
           CALL MHFREE(IP)
           IQ(I+1)=ID
           IS=I-IP
           IF(IQ(IT+KSICGB).EQ.5)IS=IS*NBYTPW
   2       IF(ID.GT.0)THEN
             IQ(ID+2)=IQ(ID+2)+IS
             ID=IQ(ID)
             GO TO 2
           ENDIF
        ENDIF
      ENDIF
      IT=IQ(IT)
      GO TO 1
      END
