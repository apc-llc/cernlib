*
* $Id: v202m.F,v 1.1.1.1 1996/04/01 15:01:31 mclareni Exp $
*
* $Log: v202m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:31  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE V202M
C     This program tests the operation of MATHLIB subprograms
C     PERMU, PERMUT and COMBI

#include "gen/imp64.inc"
C     Set maximum error allowed for test to be considered successful
      PARAMETER ( ITSTERR=0 )

      DIMENSION IPT(12),IP(12),IAT(9),IA(9),IBT(12),IB(12),IB1(6)
      DIMENSION IFCT(0:12)
#include "iorc.inc"
      DATA (IFCT(I),I=0,12)
     1 /1,1,2,6,24,120,720,5040,40320,362880,3628800,39916800,
     2  479001600/

      DATA IAT /2,4,6,8,5*0/
      DATA IPT/ 1,12,11,10,9,8,7,6,5,4,3,2/
      DATA IBT/ 1,2,3,5,8,8,7,8,4,6,4,7/
      DATA IB / 1,2,3,4,4,5,6,7,7,8,8,8/
      DATA IB1/ 1,2,3,4,4,5/

      CALL HEADER('V202',0)
      IERRMAX=0

      WRITE(LOUT,*)'COMBI(IA,8,4)'

      IA(1)=0
      J=4
      N=8
      DO 1 I = 1,IFCT(8)/(IFCT(4)*IFCT(4))
      CALL COMBI(IA,N,J)
      IF(I .EQ. 50) THEN
       IS=0
       DO 11 K = 1,J
   11  IS=IS+IA(K)-IAT(K)
      IERRMAX=IABS(MAX(IERRMAX,IS))
      WRITE(LOUT,'(/3X,''IA(1)'',1X,''IA(2)'',1X,''IA(3)'',1X,
     +''IA(4)'',5X,''Error'')')
       WRITE(LOUT,'(1X,4I6,6X,I3)') (IA(K),K=1,J),IS
      ENDIF
    1 CONTINUE
      CALL COMBI(IA,N,J)
      WRITE(LOUT,'(/3X,''IA(1)'')')
      WRITE(LOUT,'(1X,I6)') IA(1)

      WRITE(LOUT,'(//)')
      WRITE(LOUT,*)'PERMU(IB,12)'
      WRITE(LOUT,'(/''IB(1)'',1X,''IB(2)'',1X,''IB(3)'',1X,
     +''IB(4)'',1X,''IB(5)'',1X,''IB(6)'',1X,
     +''IB(7)'',1X,''IB(8)'',1X,''IB(9)'',1X,
     +''IB(10)'',1X,''IB(11)'',1X,
     +''IB(12)'')')
      N=12
      WRITE(LOUT,'(1X,I3,8I6,3I7)') (IB(K),K=1,N)
      DO 2 I = 1,5000
    2 CALL PERMU(IB,N)
      IS=0
      DO 12 I = 1,N
   12 IS=IS+IB(I)-IBT(I)
      IERRMAX=IABS(MAX(IERRMAX,IS))
      WRITE(LOUT,'(1X,I3,8I6,3I7)') (IB(K),K=1,N)
      WRITE(LOUT,'(1X,''Error'')')
      WRITE(LOUT,'(1X,I3)') IS
      WRITE(LOUT,'(//)')
      WRITE(LOUT,*) 'PERMU(IB1,6)'
      N=6
      WRITE(LOUT,'(/3X,''IB1(1)'',1X,''IB1(2)'',1X,''IB1(3)'',1X,
     +''IB1(4)'',1X,''IB1(5)'',1X,''IB1(6)'')')
      WRITE(LOUT,'(1X,6I7)') (IB1(K),K=1,N)
      I1=IFCT(6)/IFCT(2)
      DO 22 I = 2,I1
      CALL PERMU(IB1,N)
      IF(I .EQ. I1) WRITE(LOUT,'(1X,6I7)') (IB1(K),K=1,N)
   22 CONTINUE
      WRITE(LOUT,'(/3X,''IB1(1)'')')
      CALL PERMU(IB1,N)
      WRITE(LOUT,'(1X,I6)') IB1(1)

      WRITE(LOUT,'(//)')
      WRITE(LOUT,*) 'PERMUT(39916800,12,IP)'
      N=12
      CALL PERMUT(IFCT(11),N,IP)
      IS=0
      DO 3 I = 1,N
    3 IS=IS+IP(I)-IPT(I)
      IERRMAX=IABS(MAX(IERRMAX,IS))
      WRITE(LOUT,'(/''IP(1)'',1X,''IP(2)'',1X,''IP(3)'',1X,
     +''IP(4)'',1X,''IP(5)'',1X,''IP(6)'',1X,
     +''IP(7)'',1X,''IP(8)'',1X,''IP(9)'',1X,
     +''IP(10)'',1X,''IP(11)'',1X,
     +''IP(12)'')')
      WRITE(LOUT,'(1X,I3,8I6,3I7)') (IP(I),I=1,N)
      WRITE(LOUT,'(1X,''Error'')')
      WRITE(LOUT,'(1X,I3)') IS
      CALL PERMUT(1,0,IP)
      WRITE(LOUT,'(1X)')
      WRITE(LOUT,*) 'PERMUT(L,4,IP) with L=1,24'
      WRITE(LOUT,'(/3X,''IP(1)'',1X,''IP(2)'',1X,''IP(3)'',1X,
     +''IP(4)'')')
      DO 4 L = 1,24
      CALL PERMUT(L,4,IP)
    4 WRITE(LOUT,'(1X,4I6)') (IP(I),I=1,4)
      WRITE(LOUT,'(/''TESTING ERROR MESSAGES:''/)')
      CALL PERMUT(10,3,IP)
      CALL PERMUT(5,13,IP)
      CALL COMBI(IA,3,6)

      WRITE(LOUT,'(/'' Largest Absolute Error was'',I3)') IERRMAX
C     Check if the test was successful
      IRC=ITEST('V202',IERRMAX .LE. ITSTERR)
      CALL PAGEND('V202')
      RETURN
      END
