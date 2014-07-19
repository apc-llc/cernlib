*
* $Id: store.F,v 1.1.1.1 1996/02/15 17:53:27 mclareni Exp $
*
* $Log: store.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:27  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE STOREB (IWORD,J,N0,IB)
C
C
      DIMENSION NPOW2(31)
C     DIMENSION NT(35)                                                  A6
C     DIMENSION NT(47)                                                  A8
      DIMENSION NT(59)                                                  A10
      EQUIVALENCE (NPOW2,NT)
C
      DIMENSION N2(4)
      EQUIVALENCE (N2,NT(32))                                           -A4
C
      DIMENSION N3(12)                                                  A8,A10
      EQUIVALENCE (N3,NT(36))                                           A8,A10
C
      DIMENSION N4(12)                                                  A10
      EQUIVALENCE (N4,NT(48))                                           A10
C
      DATA NPOW2 /
     + 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192,
     + 16384, 32768, 65536, 131072, 262144, 524288, 1048576, 2097152,
     + 4194304, 8388608, 16777216, 33554432, 67108864, 134217728,
     + 268435456, 536870912, 1073741824/
C
      DATA N2 /                                                         -A4
     + 2147483648, 4294967296, 8589934592, 17179869184/                 -A4
C
      DATA N3 /                                                         A8,A10
     + 34359738368, 68719476736, 137438953472, 274877906944,            A8,A10
     + 549755813888, 1099511627776, 2199023255552, 4398046511104,       A8,A10
     + 8796093022208, 17592186044416, 35184372088832, 70368744177664/   A8,A10
C
      DATA N4 /                                                         A10
     + 140737488355328, 281474976710656, 562949953421312,               A10
     + 1125899906842624, 2251799813685248, 4503599627370496,            A10
     + 9007199254740992, 18014398509481984, 36028797018963968,          A10
     + 72057594037927936, 144115188075855872, 288230376151711744/       A10
C
C-         ENTRY STOREB
      IEXP2=IB
      N=NPOW2(IB+1)
      GO TO 12
C
         ENTRY STORE                                                     CDC
C        ENTRY STORE   (IWORD,J,N0)                                     -CDC
C
         ENTRY STORE6                                                    CDC
C        ENTRY STORE6  (IWORD,J,N0)                                     -CDC
C
      IEXP2=6
      N= 64
      GO TO 12
C
         ENTRY STORE1                                                    CDC
C        ENTRY STORE1  (IWORD,J,N0)                                     -CDC
C
      IEXP2=1
      N= 2
C
   12 IF (IWORD.LT.0) IWORD=-IWORD
      IP=IEXP2*(J-1)+1
      IPOWER=NPOW2(IP)
      IPW=IPOWER*N
      IWORD=(IWORD/IPW)*IPW
     1      +MOD (N0,N)*IPOWER
     2      +MOD (IWORD,IPOWER)
      RETURN
      END
