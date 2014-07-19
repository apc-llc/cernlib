*
* $Id: csundf.F,v 1.1.1.1 1996/02/26 17:16:28 mclareni Exp $
*
* $Log: csundf.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:28  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZU:  1.16/16 30/09/93  18.10.11  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
       SUBROUTINE CSUNDF(IMNONE,NHNAME,IGSST)
***--------------------------------------------
*  translater's routine for test undeclared vars
***--------------------------------------------
      INTEGER NHNAME(*)
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstab.inc"
#include "comis/cspnts.inc"
      PARAMETER ( LBUFF=72)
      CHARACTER  NAME*(KLENID), BUFF*(LBUFF)
      IF(IMNONE.NE.1)RETURN
      NOUNDF=0
      I=IPLI
  1   IF(I.EQ.0)GO TO 2
      CALL CSLDLI(I)
      IF(ITYPGI.LT.0)NOUNDF=NOUNDF+1
      I=IQ(I)
      GO TO 1
  2   IF(NOUNDF.EQ.0)RETURN
      CALL UHTOC(NHNAME,4,NAME,KLENID)
      PRINT *,' Implicit none warning: in ', NAME,
     + ' undeclared variables are:'
*      IGSST=-1
      IPOS=3
      BUFF=' '
      I=IPLI
  3   IF(I.EQ.0)GO TO 999
      CALL CSLDLI(I)
      IF(ITYPGI.LT.0)THEN
        IF(IPOS+NCIDGI+3.GE.LBUFF)THEN
          PRINT 10,BUFF
          BUFF=' '
        ENDIF
        CALL CSGTIDL(I,NAME,NC)
        BUFF(IPOS:IPOS+NCIDGI-1)=NAME(1:NCIDGI)
        IPOS=IPOS+NCIDGI+3
 10    FORMAT(A)
      ENDIF
      I=IQ(I)
      GO TO 3
 999  IF(IPOS.GT.3)PRINT 10,BUFF
      END
