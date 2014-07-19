*
* $Id: c313m.F,v 1.1.1.1 1996/04/01 15:01:14 mclareni Exp $
*
* $Log: c313m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:14  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C313M
C     This program tests the operation of KERNLIB subprograms
C     BESI0, BESI1, BESK0, BESK1 and
C     DBESI0, DBESI1, DBESK0, DBESK1  (C313)

#include "imp64r.inc"
      REAL BESI0, BESI1, BESK0, BESK1
      REAL EBESI0, EBESI1, EBESK0, EBESK1
C     Set maximum error allowed for test to be considered successful
      DIMENSION TOL(2)


      LOGICAL LTEST
C     Set the total number of tests for BESI0 and BESI1
      PARAMETER ( NT=20)
#if defined(CERNLIB_DOUBLE)
      DIMENSION X(NT),EX(NT,4),ER(NT,4)
      REAL RX(NT),REX(NT,4),RER(NT,4)
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL X(NT),EX(NT,4),ER(NT,4)
#endif
#include "iorc.inc"
      DATA TOL/1D-6, 1D-13/
C     Input parameters for individual tests
       DATA X( 1 )/ -10.00D0/
       DATA X( 2 )/  -6.00D0/
       DATA X( 3 )/  -4.00D0/
       DATA X( 4 )/  -1.00D0/
       DATA X( 5 )/  -0.50D0/
       DATA X( 6 )/  0.00D0/
       DATA X( 7 )/  0.70D0/
       DATA X( 8 )/  2.50D0/
       DATA X( 9 )/  7.00D0/
       DATA X(10 )/  9.00D0/
C     Analytical values expected to be obtained

       DATA (EX(1,J),J=1,4)
     + /0.127833337163428D0,-0.121262681384455D0,
     +        0.000000000000000D0,0.000000000000000D0/
       DATA (EX(2,J),J=1,4)
     + /0.166657432639816D0,-0.152051459308506D0,
     +    0.000000000000000D0,0.000000000000000D0/
       DATA (EX(3,J),J=1,4)
     + /0.207001921223987D0,-0.178750839502435D0,
     + 0.000000000000000D0, 0.000000000000000D0/
       DATA (EX(4,J),J=1,4)
     + / 0.465759607593640D0,-0.207910415349708D0,
     +  0.000000000000000D0, 0.000000000000000D0/
       DATA (EX(5,J),J=1,4)
     + / 0.645035270449149D0,-0.156420803184872D0,
     +  0.000000000000000D0, 0.000000000000000D0/
       DATA (EX(6,J),J=1,4)
     + / 1.000000000000000D0, 0.000000000000000D0,
     +  0.000000000000000D0, 0.000000000000000D0/
       DATA (EX(7,J),J=1,4)
     + / 0.559305526507068D0, 0.184669982762747D0,
     +  1.330123656242055D0, 2.115011312848052D0/
       DATA (EX(8,J),J=1,4)
     + / 0.270046441612203D0, 0.206584649531266D0,
     +  0.759548690328100D0, 0.900174423907878D0/
       DATA (EX(9,J),J=1,4)
     + / 0.153737744672881D0, 0.142289234709599D0,
     +  0.465845096093016D0, 0.498071575095476D0/
       DATA (EX(10,J),J=1,4)
     + / 0.134959524581723D0, 0.127224983935891D0,
     +  0.412295549231418D0, 0.434625245347434D0/
       DATA (EX(11,J),J=1,4)
     + / 0.127833337163429D0,-0.121262681384455D0,
     +  0.000000000000000D0, 0.000000000000000D0/
       DATA (EX(12,J),J=1,4)
     + / 0.166657432639816D0,-0.152051459308506D0,
     +  0.000000000000000D0, 0.000000000000000D0/
       DATA (EX(13,J),J=1,4)
     + / 0.207001921223987D0, -0.178750839502435D0,
     +  0.000000000000000D0,  0.000000000000000D0/
       DATA (EX(14,J),J=1,4)
     + / 0.465759607593640D0, -0.207910415349708D0,
     +  0.000000000000000D0,  0.000000000000000D0/
       DATA (EX(15,J),J=1,4)
     + / 0.645035270449149D0, -0.156420803184872D0,
     +  0.000000000000000D0,  0.000000000000000D0/
       DATA (EX(16,J),J=1,4)
     + / 1.000000000000000D0,  0.000000000000000D0,
     +  0.000000000000000D0,  0.000000000000000D0/
       DATA (EX(17,J),J=1,4)
     + / 0.559305526507068D0,  0.184669982762747D0,
     +  1.330123656242055D0,  2.115011312848052D0/
       DATA (EX(18,J),J=1,4)
     + / 0.270046441612203D0,  0.206584649531266D0,
     +  0.759548690328100D0,  0.900174423907878D0/
       DATA (EX(19,J),J=1,4)
     + / 0.153737744672881D0,  0.142289234709599D0,
     +  0.465845096093016D0,  0.498071575095476D0/
       DATA (EX(20,J),J=1,4)
     + / 0.134959524581723D0,  0.127224983935891D0,
     +  0.412295549231418D0,  0.434625245347434D0/
      DATA LTEST/.TRUE./
      CALL HEADER('C313',0)
#if defined(CERNLIB_DOUBLE)
      NF=2
#endif
#if !defined(CERNLIB_DOUBLE)
      NF=1
#endif
      DO 1000 JF=1,NF
         ERRMAX= 0.0D0
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/10X,''TEST FOR BESI0, BESI1,'',
     +'' BESK0, BESK1'')')
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)WRITE(LOUT,'(/10X,''TEST FOR BESI0, BESI1,'',
     +'' BESK0, BESK1'')')
      IF(JF.EQ.2)WRITE(LOUT,'(/10X,''TEST FOR DBESI0,'',
     +'' DBESI1, DBESK0, DBESK1'')')

#endif
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/9X,''X'',9X,
     +''(e**(-|X|))* BESI0'',7X,''(e**(-|X|))* BESI1'',7X,
     +''EBESI0'',7X,''EBESI1''/)')
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)
     +WRITE(LOUT,'(/9X,''X'',9X,
     +''(e**(-|X|))* BESI0'',7X,''(e**(-|X|))* BESI1'',7X,
     +''EBESI0'',7X,''EBESI1''/)')
      IF(JF.EQ.2)
     +WRITE(LOUT,'(/9X,''X'',9X,
     +''(e**(-|X|))*DBESI0'',7X,''(e**(-|X|))*DBESI1'',7X,
     +''DEBSI0'',7X,''DEBSI1''/)')
#endif
c     DO 55 I = 1,10
      DO 55 I = 1,4
#if !defined(CERNLIB_DOUBLE)
      BI0=EXP(-ABS(X(I)))* BESI0(X(I))
      BI1=EXP(-ABS(X(I)))* BESI1(X(I))
      EBI0= EBESI0(X(I))
      EBI1= EBESI1(X(I))
      ER(I,1)= ABS(EX(I,1)-BI0 )
      ER(I,2)= ABS(EX(I,2)-BI1 )
      ER(I+10,1)= ABS(EX(I+10,1)-EBI0 )
      ER(I+10,2)= ABS(EX(I+10,2)-EBI1 )
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)THEN
                 RX(I)=X(I)
                 REX(I,1)=EX(I,1)
                 REX(I,2)=EX(I,2)
                 REX(I+10,1)=EX(I+10,1)
                 REX(I+10,2)=EX(I+10,2)
      RBI0=EXP(-ABS(RX(I)))*BESI0(RX(I))
      RBI1=EXP(-ABS(RX(I)))*BESI1(RX(I))
      REBI0= EBESI0(RX(I))
      REBI1= EBESI1(RX(I))
      ER(I,1)= ABS(REX(I,1)-RBI0 )
      ER(I,2)= ABS(REX(I,2)-RBI1 )
      ER(I+10,1)= ABS(REX(I+10,1)-RBI0 )
      ER(I+10,2)= ABS(REX(I+10,2)-RBI1 )
                  BI0=RBI0
                  BI1=RBI1
                  EBI0=REBI0
                  EBI1=REBI1
      ENDIF
      IF(JF.EQ.2)THEN
      BI0=EXP(-ABS(X(I)))*DBESI0(X(I))
      BI1=EXP(-ABS(X(I)))*DBESI1(X(I))
      EBI0=DEBSI0(X(I))
      EBI1=DEBSI1(X(I))
      ER(I,1)= ABS(EX(I,1)-BI0 )
      ER(I,2)= ABS(EX(I,2)-BI1 )
      ER(I+10,1)= ABS(EX(I+10,1)-EBI0 )
      ER(I+10,2)= ABS(EX(I+10,2)-EBI1 )
      ENDIF
#endif
      WRITE(LOUT,'(1X,F10.1,4F25.15)') X(I),BI0,BI1,EBI0,EBI1
      DO 88 J = 1,2
         ERRMAX = MAX( ERRMAX ,ER(I,J) )
         ERRMAX = MAX( ERRMAX ,ER(I+10,J) )
   88 CONTINUE
   55 CONTINUE

#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/9X,''X'',9X,
     +''(e**( |X|))* BESK0'',7X,''(e**( |X|))* BESK1'',7X,
     +'' EBESK0'',7X,'' EBESK1''/)')
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)
     +WRITE(LOUT,'(/9X,''X'',9X,
     +''(e**( |X|))* BESK0'',7X,''(e**( |X|))* BESK1'',7X,
     +'' EBESK0'',7X,'' EBESK1''/)')
      IF(JF.EQ.2)
     +WRITE(LOUT,'(/9X,''X'',9X,
     +''(e**( |X|))*DBESK0'',7X,''(e**( |X|))*DBESK1'',7X,
     +''DEBSK0'',7X,''DEBSK1''/)')
#endif
      DO 66 I = 7,10
      BK0=0
      BK1=0
#if !defined(CERNLIB_DOUBLE)
       BK0=EXP(X(I))* BESK0(X(I))
       BK1=EXP(X(I))* BESK1(X(I))
       EBK0=           EBESK0(X(I))
       EBK1=           EBESK1(X(I))
      ER(I,3)= ABS(EX(I,3)-BK0 )
      ER(I,4)= ABS(EX(I,4)-BK1 )
      ER(I+10,3)= ABS(EX(I+10,3)-EBK0 )
      ER(I+10,4)= ABS(EX(I+10,4)-EBK1 )
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)THEN
                 RX(I)=X(I)
                 REX(I,3)=EX(I,3)
                 REX(I,4)=EX(I,4)
                 REX(I+10,3)=EX(I+10,3)
                 REX(I+10,4)=EX(I+10,4)
       RBK0=EXP(RX(I))*BESK0(RX(I))
       RBK1=EXP(RX(I))*BESK1(RX(I))
       REBK0=            EBESK0(RX(I))
       REBK1=            EBESK1(RX(I))
      ER(I,3)= ABS(REX(I,3)-RBK0 )
      ER(I,4)= ABS(REX(I,4)-RBK1 )
      ER(I+10,3)= ABS(REX(I+10,3)-REBK0 )
      ER(I+10,4)= ABS(REX(I+10,4)-REBK1 )
                  BK0=RBK0
                  BK1=RBK1
                  EBK0=REBK0
                  EBK1=REBK1
      ENDIF
      IF(JF.EQ.2)THEN
       BK0=EXP(X(I))*DBESK0(X(I))
       BK1=EXP(X(I))*DBESK1(X(I))
      EBK0=DEBSK0(X(I))
      EBK1=DEBSK1(X(I))
      ER(I,3)= ABS(EX(I,3)-BK0 )
      ER(I,4)= ABS(EX(I,4)-BK1 )
      ER(I+10,3)= ABS(EX(I+10,3)-EBK0 )
      ER(I+10,4)= ABS(EX(I+10,4)-EBK1 )
      ENDIF
#endif
      WRITE(LOUT,'(1X,F10.1,4F25.15)') X(I),BK0,BK1,EBK0,EBK1
      DO 8 J = 3,4
         ERRMAX = MAX( ERRMAX ,ER(I,J) )
         ERRMAX = MAX( ERRMAX ,ER(I+10,J) )
    8 CONTINUE
   66 CONTINUE

      WRITE(LOUT,'(/''LARGEST RELATIVE ERROR WAS'',1P,D10.1)')
     +ERRMAX
#if !defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF+1)
#endif
#if defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF)
#endif
      LTEST=LTEST.AND.(ERRMAX.LE.ETOL)
         ERRMAX= 0D0

      WRITE(LOUT,'(/''TESTING ERROR MESSAGES:'')')

      DO 77 I = 5,6
      BK0=0
      BK1=0
#if !defined(CERNLIB_DOUBLE)
       BK0=EXP(X(I))* BESK0(X(I))
       BK1=EXP(X(I))* BESK1(X(I))
       EBK0=          EBESK0(X(I))
       EBK1=          EBESK1(X(I))
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)THEN
                 RX(I)=X(I)
       RBK0=EXP(RX(I))*BESK0(RX(I))
       RBK1=EXP(RX(I))*BESK1(RX(I))
       REBK0=EBESK0(RX(I))
       REBK1=EBESK1(RX(I))
       ENDIF
            IF(JF.EQ.2)THEN
       BK0=EXP(X(I))*DBESK0(X(I))
       BK1=EXP(X(I))*DBESK1(X(I))
       EBK0=          DEBSK0(X(I))
       EBK1=          DEBSK1(X(I))
      ENDIF
#endif
   77 CONTINUE
1000  CONTINUE

      IRC=ITEST('C313',LTEST)
      CALL PAGEND('C313')
      END
