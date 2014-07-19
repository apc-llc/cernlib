*
* $Id: tpkch.F,v 1.1.1.1 1996/02/15 17:54:55 mclareni Exp $
*
* $Log: tpkch.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:55  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE TPKCH

#include "kerngent/mkcde.inc"

      COMMON /SLATE/NWCHAR,DUMMY(39)

      DIMENSION IPACK(96)
      DIMENSION JGOTIM(2)
      DIMENSION IPARAM(5)
      EQUIVALENCE (NFILL,IPARAM(5))
      INTEGER TINF(4), TINF2(8)

      DATA  NPACK/96/
      DATA  IPACK/
     +  6,5,0,0,       6,4,0,3,       6,0,0,0,       6,0,0,3
     +, 6,10,60,0,     6,6,60,21,     6,20,120,0,    9,12,120,6
     +, 6,0,120,0,     6,30,120,21,   8,0,120,0,     8,0,120,4
     +, 6,0,54,0,      6,0,54,3,      6,0,63,0,      6,0,63,3
     +, 6,8,48,0,      6,6,48,9,      6,6,36,0,      6,4,36,9
     +, 8,4,32,0,      8,3,32,4,      8,8,64,0,      8,6,64,12    /

      DATA  JGOTIM /9, 33/
      DATA  TINF   /1000, 4HPKCH, 80, 4HCHAR/
      DATA  TINF2  /4HPK 1, 4HUPK , 4HPK 2, 4HUPK , 4HPK 3, 4HUPK ,
     +              4HPK 4, 4HUPK  /


      CALL NEWGUY ('PKCHAR-UPKCH.','TPKCH   ')

C-     JGO =  1  +  4*( (TN-1)/2 )
C-     TN  =  1  +  2*( (JGO-1)/4 )    FOR NFILL ZERO
C-     TN  =  2  +  ...                FOR NFILL NON ZERO

      JTEST= 1

      DO 29 JPK=1,NPACK,4
      CALL UCOPY (IPACK(JPK),IPARAM,4)
      IPARAM(5)= 0
   22 CALL VZERO (A,20)
      CALL PKCHAR (INTG,A(2),40,IPARAM)
      IA(1)= NWCHAR
C     CALL TCDUMP (4HPACK,A,15,0)
      CALL VZERO (B(2),40)
      IB(42)= 41
      CALL UPKCH (A(2),B(2),40,IPARAM)
      IB(1)= NWCHAR
      CALL UCOPY (INTG,IA(2),41)
      CALL MVERII (JTEST,B,A,42)
      JTEST= JTEST + 1
      IF (IPARAM(5).NE.0)  GO TO 29
      IPARAM(5)= IBCD(1)
      GO TO 22
   29 CONTINUE

C--      TIMING
      IF (ITIMES.EQ.0)  RETURN
      NTIMES = ITIMES*TINF(1)
      TINF(1) = NTIMES
      JEX= 0

      DO 94 JLOOP=1,2
      JGO = JGOTIM(JLOOP)
      CALL UCOPY (IPACK(JGO),IPARAM,4)
      IPARAM(5)= 0
      GO TO 82

   81 IPARAM(4)= 3
      IPARAM(5)= IBCD(1)
   82 JOPER= 0
      GO TO 86
   83 JOPER= 1

   86 JEX= JEX + 1
      TINF(2)= TINF2(JEX)
      CALL TIMED (TIMERD)
      IF (JOPER.NE.0)  GO TO 88

      DO 87 J=1,NTIMES
   87 CALL PKCHAR (INTG,A,80,IPARAM)

      GO TO 91

   88 DO 89 J=1,NTIMES
   89 CALL UPKCH (A,B,80,IPARAM)

   91 CALL TIMING (TINF)
      IF (JOPER.EQ.0)  GO TO 83
      IF (NFILL.EQ.0)  GO TO 81
   94 CONTINUE
      RETURN

      END
