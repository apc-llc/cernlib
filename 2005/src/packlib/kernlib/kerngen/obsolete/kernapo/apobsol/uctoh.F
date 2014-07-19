*
* $Id: uctoh.F,v 1.1.1.1 1996/02/15 17:51:10 mclareni Exp $
*
* $Log: uctoh.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:10  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      SUBROUTINE UCTOH (KCHAR,KHOLL,NPW,NCH)
C
C CERN PROGLIB# M409    UCTOH           .VERSION KERNAPO  1.07  860212
C ORIG. 11/02/86 A. PETRILLI, CERN
C
      PARAMETER (MAXCPW=4)

      CHARACTER*(9) KCHAR,KHOLL

      IF (NCH.LE.0)          RETURN
      GO TO (11, 21, 31), NPW

C----                    NPW = 4

      KHOLL(1:NCH) = KCHAR(1:NCH)
      I = MOD(NCH,MAXCPW)
      IF (I.NE.0) KHOLL(NCH+1:NCH+(MAXCPW-I)) = ' '
      RETURN

C----                    NPW = 1

C--   EQUIVALENT TO 'CALL UCTOH1(KCHAR,KHOLL,NCH)'

   11 J = 1
      KHOLL(1:4*NCH) = ' '
      DO 14 L=1,NCH
         KHOLL(J:J) = KCHAR(L:L)
         J = J+MAXCPW
   14 CONTINUE
      RETURN

C----                    NPW = 2

   21 J = 1
      I = MOD(NCH,2)
      K = NCH-I
      DO 24 L=1,K,2
         KHOLL(J:J+MAXCPW-1) = KCHAR(L:L+1)
         J = J+MAXCPW
   24 CONTINUE
      IF (I.NE.0) KHOLL(J:J+MAXCPW-1) = KCHAR(NCH:NCH)
      RETURN

C----                    NPW = 3

   31 J = 1
      I = MOD(NCH,3)
      K = NCH-I
      DO 34 L=1,K,3
         KHOLL(J:J+MAXCPW-1) = KCHAR(L:L+2)
         J = J+MAXCPW
   34 CONTINUE
      IF (I.NE.0) KHOLL(J:J+MAXCPW-1) = KCHAR(NCH-I+1:NCH)
      RETURN
      END
#ifdef CERNLIB_TCGEN_UCTOH
#undef CERNLIB_TCGEN_UCTOH
#endif
