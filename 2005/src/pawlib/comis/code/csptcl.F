*
* $Id: csptcl.F,v 1.1.1.1 1996/02/26 17:16:16 mclareni Exp $
*
* $Log: csptcl.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:16  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZU:  1.16/16 01/10/93  11.32.28  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSPTCL
***-----------------------------------------------
*     csptcl outputs to terminal list of currently
*     known to comis blocks
***-----------------------------------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstabps.inc"
#include "comis/cstab.inc"
#include "comis/cspnts.inc"
       CHARACTER NAME*(KLENID) ,CG*4, BUFF*72
       I=IPGB
       PRINT *,' List of currently defined blocks:'
       IF(I.EQ.0) THEN
         PRINT *,' List is empty'
         RETURN
       ENDIF
       NO=0
       IPOS=3
       BUFF=' '
  1    IF(I.EQ.0)GO TO 999
       CALL CCOPYA(IQ(I+1),NCIDGB,KSLENB)
       CALL CSGTIDB(I,NAME,NC)
       CG='COM/'
       IF(ICGB.GT.2)CG='GLO/'
       NO=NO+1
       BUFF(IPOS:IPOS+3)=CG
       BUFF(IPOS+4:IPOS+11)=NAME(1:NCIDGB)
       BUFF(IPOS+12:IPOS+12)='/'
       IPOS=IPOS+16
       IF(MOD(NO,4).EQ.0)THEN
         PRINT 10,BUFF
         BUFF=' '
         IPOS=3
       ENDIF
 10    FORMAT(A)
       I=IQ(I)
       GO TO 1
 999   IF(IPOS.GT.3)PRINT 10,BUFF
       END
