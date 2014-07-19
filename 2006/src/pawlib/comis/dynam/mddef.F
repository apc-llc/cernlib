*
* $Id: mddef.F,v 1.1.1.1 1996/02/26 17:16:56 mclareni Exp $
*
* $Log: mddef.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:56  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.13/00 27/03/92  14.14.24  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MDDEF(IND,N,NC,MALL,MFREE,MEXP)
      LOGICAL MDEXP
#include "comis/cssysd.inc"
      COMMON /MDPOOL/ IQ(6)
      EXTERNAL MALL,MFREE,MEXP
      I=IND
      M=N
      IF(I.GT.4) GO TO 1
      IF(M.LT.6) M=3000
      IF(MOD(M,2).EQ.1) M=M+1
      J=IQ(3)
   10 CONTINUE
      IF(MDEXP(M,I)) GO TO 2
      CALL MDPAUS('?MDDEF: NO SUFFICIENT CORE')
      GO TO 10
    1 IF(M.LT.6) GO TO 2
      CALL MDPAUS('?MDDEF: AREA"S LENGTH IS TOO LITTLE')
      STOP
    2 IQ(I)=0
      IQ(I+1)=JUMPAD(MALL)
      IQ(I+2)=JUMPAD(MFREE)
      IQ(I+3)=M
      IQ(I+4)=JUMPAD(MEXP)
      M=NC
      IF(M.EQ.0) M=NWPR
      IQ(I+5)=M
      MDDEF=I
      RETURN
      END
