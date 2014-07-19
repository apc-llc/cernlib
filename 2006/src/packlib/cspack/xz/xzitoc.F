*
* $Id: xzitoc.F,v 1.1.1.1 1996/03/08 15:44:32 mclareni Exp $
*
* $Log: xzitoc.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:32  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZITOC(NUM,STR,JS)
*
*     Convert integer NUM into character string STR with JS non-blank
*     characters. JS = 1 ... 12
*     Examples:  CALL PNVI( -1,STR,JS)   -> STR='-1' ; JS = 2
*                CALL PNVI(317,STR,JS)   -> STR='317' ; JS = 3
*
      CHARACTER*(*) STR
      CHARACTER*12 DIG(0:9)*1
      INTEGER ND(12)
      DATA DIG/'0','1','2','3','4','5','6','7','8','9'/
      JS=0
*
      K=IABS(NUM)
      DO 10 I=1,12
      ND(I)=MOD(K,10)
      K    =K/10
      IF(K.EQ.0) GOTO 12
   10 CONTINUE
      I=12
   12 STR=' '
      IF(NUM.LT.0) THEN
         JS=JS+1
         STR(JS:JS)='-'
      END IF
      DO 14 J=I,1,-1
      JS=JS+1
   14 STR(JS:JS)=DIG(ND(J))
      RETURN
      END
