*
* $Id: csdclr.F,v 1.1.1.1 1996/02/26 17:16:16 mclareni Exp $
*
* $Log: csdclr.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:16  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZU:  1.16/16 01/10/93  12.00.02  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSDCLR
***-----------------------------------------------
*     csdclr clears the list of currently known to comis
*     global blocks and cs_routine
***-----------------------------------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstab.inc"
#include "comis/cspnts.inc"
*   clears global blocks
       I=IPGB
       IPVS=IPGB
  1    IF(I.EQ.0)GO TO 11
       NEXT=IQ(I)
       CALL CCOPYA(IQ(I+2),IADGB,3)
       IF(ICGB.GT.2)THEN
         IP=IADGB-LOCF(IQ(2))
         ID=IQ(IP+1)
  2      IF(ID.GT.0)THEN
           INXT=IQ(ID)
           CALL MHFREE(ID)
           ID=INXT
           GO TO 2
         ENDIF
         CALL MHFREE(IP)
         IF(IPVS.EQ.IPGB)THEN
           IPVS=NEXT
           IPGB=NEXT
         ELSE
           IQ(IPVS)=NEXT
         ENDIF
         CALL MHFREE(I)
       ELSE
         IPVS=I
       ENDIF
       I=NEXT
       GO TO 1
 11    CONTINUE
*   clears IPGI list
       I=IPGI
 22    IF(I.EQ.0)GO TO 33
       NEXT=IQ(I)
       CALL MHFREE(I)
       I=NEXT
       GO TO 22
 33    IPGI=0
*   clears IPCAD list
       I=IPMCAD
 44    IF(I.EQ.0)GO TO 55
       NEXT=IQ(I)
       CALL MHFREE(I)
       I=NEXT
       GO TO 44
  55   IPMCAD=0
*    clears comis routines
       I=IPGP
       IPVS=I
  66   IF(I.EQ.0)GO TO 77
       NEXT=IQ(I)
       CALL CSDPRO(I)
       CALL CCOPYA(IQ(I+2),IADGP,3)
       IF(IFCS.EQ.0)THEN
         IF(IPVS.EQ.IPGP)THEN
           IPVS=NEXT
           IPGP=NEXT
         ELSE
           IQ(IPVS)=NEXT
         ENDIF
         CALL MHFREE(I)
       ELSE
         IPVS=I
       ENDIF
       I=NEXT
       GO TO 66
 77    END
