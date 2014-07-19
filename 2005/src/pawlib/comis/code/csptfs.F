*
* $Id: csptfs.F,v 1.1.1.1 1996/02/26 17:16:16 mclareni Exp $
*
* $Log: csptfs.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:16  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZU:  1.16/16 01/10/93  11.25.53  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
       SUBROUTINE CSPTFS
***-----------------------------------------------
*     csptfs outputs to terminal list of currently
*     known to comis routines
***-----------------------------------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstabps.inc"
#include "comis/cstab.inc"
#include "comis/cspnts.inc"
       CHARACTER NAME*(KLENID), TYPE(3)*4, BUFF*72
       DATA TYPE/'f77','*m*','cs'/
       I=IPGP
       PRINT *,' List of currently defined routines:'
       NO=0
       IPOS=3
       BUFF=' '
  1    IF(I.EQ.0)GO TO 999
       CALL CCOPYA(IQ(I+1),NCIDGP,KSIFCS)
       CALL CSGTIDP(I,NAME,NC)
       IF(IFCS.LT.0)THEN
         KI=1
       ELSEIF(IFCS.EQ.0)THEN
         KI=2
       ELSE
         KI=3
       ENDIF
       NO=NO+1
       BUFF(IPOS:IPOS+7)=NAME(1:NCIDGP)
       BUFF(IPOS+10:IPOS+13)=TYPE(KI)
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
