*
* $Id: csrnext.F,v 1.1.1.1 1996/02/26 17:16:33 mclareni Exp $
*
* $Log: csrnext.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:33  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 16/01/95  17.14.00  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
       SUBROUTINE CSRNEXT(NEXT,NAME,TR,KIND,IADDR)
***-----------------------------------------------
*     csrnext gets info about next routine from internal list
*     of currently  known to comis routines
*
*     next=0  for asking info about first routine
*     next=-1 if no more routine on the list
*     name    routine's name
*     tr      routine's type
*                  ='I' for integer function
*                  ='R' for real    function
*                  ='C' for character function
*                  ='L' for logical function
*                  ='D' for double pr. function
*                  ='S' for subroutine
*                  ='X' for complex function
*                  ='?' for unknown or error
*    kind     routine's kind
*                  ='f77' for application's routine
*                  ='cs'  for comis routine
*                  ='*m*' for missing routines
*    iaddr    comis address of routines
***-----------------------------------------------
      CHARACTER*(*) NAME,TR,KIND
      INTEGER NEXT,IADDR,CSADDR
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstabps.inc"
#include "comis/cstab.inc"
#include "comis/cspnts.inc"
       CHARACTER  TYPE(3)*4
       DATA TYPE/'f77','*m*','cs'/
       IF(NEXT.EQ.0)NEXT=IPGP
       I=NEXT
       IADDR=0
       IF(I.LE.0)GO TO 999
       CALL CCOPYA(IQ(I+1),NCIDGP,KSIFCS)
       CALL CSGTIDP(I,NAME,NC)
       IADDR=CSADDR(NAME)
       IF(IFCS.LT.0)THEN
         KI=1
       ELSEIF(IFCS.EQ.0)THEN
         KI=2
       ELSE
         KI=3
       ENDIF
       CALL CSTYPE(I,TR)
       KIND=TYPE(KI)
       NEXT=IQ(I)
 999   IF(NEXT.EQ.0)NEXT=-1
       END
