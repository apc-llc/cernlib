*
* $Id: check3.F,v 1.2 1996/03/21 17:16:07 mclareni Exp $
*
* $Log: check3.F,v $
* Revision 1.2  1996/03/21 17:16:07  mclareni
* Kernnumt corrections for unaligned access on OSF1 by John Marafino, Fermilab
*
* Revision 1.1.1.1  1996/02/15 17:48:43  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
          SUBROUTINE CHECK3(LWORK,W,NPK,OK,PKN,PKT)
          REAL      W(LWORK), PKT(*)
          LOGICAL   OK, OKPK
          CHARACTER*4 PKN(*)
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          CHARACTER*4 LISTPK(10), NULL
          INTEGER   NREPPK(10)
          INTEGER   NT(34),   MT(34),   KT(34),   MN(3),    MX(3)
          DATA      LISTPK
     +              / 'E106', 'F002', 'F003', 'F004', 'F011',
     +                'F012', 'G900', 'G901', 'N001', 'NULL'  /
          DATA      NULL      /  'NULL'  /
          DATA      NREPPK    /  50, 1, 1, 1, 1, 1, 5, 3, 1, 0  /
          DATA      NT        /  -5,-2,-1,0,1,2,3,4,5,6,7,8,9,10,
     +                           11,12,13,14,15,16,17,18,19,20,
     +                           21,22,23,24,25,26,27,28,29,30  /
          DATA      MT        /  -3,1,0,3,4,6,2,10,7,12,8,9,27,
     +                           -2,-1,20,19,18,17,16,30,29,28,
     +                           15,14,13,11,26,25,24,23,22,21,5  /
          DATA      KT        /  0,3,6,9,12,15,18,21,24,-1,-5,
     +                           27,-6,1,4,7,10,13,16,19,22,25,
     +                           28,2,5,8,11,14,17,20,23,26,29,30  /
#if defined(CERNLIB_NUMAP)
          DATA      MN        /  -32, -32, -32  /
          DATA      MX        /  +31, +31, +31  /
          DATA      NC        /  1  /
#endif
#if defined(CERNLIB_NUMCD)
          DATA      MN        /  -242, -242, -242  /
          DATA      MX        /  +266, +266, +266  /
          DATA      NC        /  6  /
#endif
#if defined(CERNLIB_NUMCR)
          DATA      MN        /  -2048, -2048, -2048  /
          DATA      MX        /  +2047, +2047, +2047  /
          DATA      NC        /  6  /
#endif
#if defined(CERNLIB_NUMCV)
          DATA      MN        /  -2048, -2048, -2048  /
          DATA      MX        /  +2047, +2047, +2047  /
          DATA      NC        /  6  /
#endif
#if defined(CERNLIB_NUMDE)
          DATA      MN        /  -32, -32, -32  /
          DATA      MX        /  +31, +31, +31  /
          DATA      NC        /  3  /
#endif
#if defined(CERNLIB_NUMIB)
          DATA      MN        /  -64, -64, -64  /
          DATA      MX        /  +63, +63, +63  /
          DATA      NC        /  6  /
#endif
#if defined(CERNLIB_NUMND)
          DATA      MN        /  -64, -64, -64  /
          DATA      MX        /  +63, +63, +63  /
          DATA      NC        /  3  /
#endif
#if defined(CERNLIB_NUMUC)
          DATA      MN        /  -32, -32, -32  /
          DATA      MX        /  +31, +31, +31  /
          DATA      NC        /  6  /
#endif
#if defined(CERNLIB_NUMMS)
          DATA      MN        /  -32, -32, -32  /
          DATA      MX        /  +31, +31, +31  /
          DATA      NC        /  1 /
#endif
#if defined(CERNLIB_NUMLN)
          DATA      MN        /  -32, -32, -32  /
          DATA      MX        /  +31, +31, +31  /
          DATA      NC        /  1 /
#endif
          DO 100    NPK  =  1, 100
             IF(LISTPK(NPK) .EQ. NULL)  GOTO 101
 100         CONTINUE
          NPK  =  0
          RETURN
 101      NPK  =  NPK - 1
          OK   =  .TRUE.
          VOID(1)   =  022.83 28 36
          VOID(2)   =  022.83 28 36
          DO 102    JTAB  =  1, 34
             NTAB(JTAB)  =  NT(JTAB)
             MTAB(JTAB)  =  MT(JTAB)
             KTAB(JTAB)  =  KT(JTAB)
  102        CONTINUE
          DO 103    JHXT  =  1, 3
             MINHXT(JHXT)  =  MN(JHXT)
             MAXHXT(JHXT)  =  MX(JHXT)
 103         CONTINUE
          DO 200    IPK  =  1, NPK
             NAMEPK  =  LISTPK(IPK)
             PKN(IPK)=  LISTPK(IPK)
             NREP    =  NREPPK(IPK)
             OKPK    =  .FALSE.
             WRITE(*,1000) NAMEPK
             GOTO(01,02,03,04,05,06,07,08,09), IPK
  01         K   =  500
             LR  =  1
             LI  =  LR + K
             LIM =  LI + K
             IF(LIM .GT. LWORK)  GOTO 900
             CALL E106CH(NREP,K,W(LR),W(LI),OKPK)
             GOTO 90
  02         LTABT(1)  =  34
             LTABT(2)  =  24
             LTABT(3)  =  24
             NCNFG     =  NC
             CALL F002CH(NREP,LWORK,W(1),OKPK)
             GOTO 90
  03         LTABT(1)  =  34
             LTABT(2)  =  24
             LTABT(3)  =  24
             NCNFG     =  NC
             CALL F003CH(NREP,LWORK,W(1),OKPK)
             GOTO 90
  04         LTABT(1)  =  34
             LTABT(2)  =  24
             LTABT(3)  =  24
             NCNFG     =  NC
             CALL F004CH(NREP,LWORK,W(1),OKPK)
             GOTO 90
  05         LTABT(1)  =  24
             LTABT(2)  =  14
             LTABT(3)  =  14
             NCNFG     =  6
             CALL F011CH(NREP,LWORK,W(1),OKPK)
             GOTO 90
  06         LTABT(1)  =  24
             LTABT(2)  =  14
             LTABT(3)  =  14
             NCNFG     =  6
             CALL F012CH(NREP,LWORK,W(1),OKPK)
             GOTO 90
  07         CALL G900CH(NREP,OKPK)
             GOTO 90
  08         K   =  LWORK/4
             LX  =  1
             LY  =  LX + K
             LZ  =  LY + K
             LT  =  LZ + K
             CALL G901CH(NREP,K,W(LX),W(LY),W(LZ),W(LT),OKPK)
             GOTO 90
  09         CALL N001CH(OKPK)
             GOTO 91
  90         IF(      OKPK)  WRITE(*,1013) NAMEPK
  91         IF(.NOT. OKPK)  WRITE(*,1012) NAMEPK
             OK  =  OK .AND. OKPK
             CALL TIMEX( PKT(IPK) )
 200         CONTINUE
          IF(NPK .LT. 1)  OK  =  .FALSE.
          RETURN
 900      OK  =  .FALSE.
          WRITE(*,1020)
          RETURN
1000      FORMAT(// 10H CHECK OF , A4,1H.)
1012      FORMAT(/ 5X, 24H ????? CHECK OF PACKAGE ,A4,
     +           18H HAS FAILED. ????? )
1013      FORMAT( 15X, 18H CHECK OF PACKAGE ,A4,12H SUCCESSFUL. )
1020      FORMAT(// 42H ????? CHECK 3 HAS INSUFFICIENT WORK SPACE )
          END
          SUBROUTINE CNFGMX(M,N,IRDIM,LA11,LA12,LA21)
#include "ch3dat.inc"
          IRANF(I,K)  =  INT( RANF()*FLOAT(K-I+1) ) + I
          LA11  =  1
          LA12  =  1
          LA21  =  1
          IF(M .LE. 0  .OR.  N .LE. 0)  RETURN
          ITDIM  =  IRDIM / LENGTH
          IF(ITDIM/(M*N) .GE. 5)  THEN
             DO 20     K  =  1, 5
                IX  =  IRANF(1,ITDIM/M)
                JX  =  1
                IF(N .LE. 1)  GOTO 40
                JX  =  IRANF(1,(ITDIM-1-IX*(M-1))/(N-1))
                IF((M-1)*(N-1) .EQ. 0)  GOTO 40
                DO 12  I  =  2, M
                   LCOL  =  (I-1)*IX
                   DO 11  J  =  2, N
                      LROW  =  (J-1)*JX
                      IF(LROW .GT. LCOL)  GOTO 12
                      IF(LROW .EQ. LCOL)  GOTO 20
  11                  CONTINUE
  12               CONTINUE
                GOTO 40
  20            CONTINUE
          ENDIF
          IF(RANF() .LE. 0.5)  THEN
             JX  =  IRANF(M,ITDIM/N)
             IX  =  IRANF(1,JX/M)
          ELSE
             IX  =  IRANF(N,ITDIM/M)
             JX  =  IRANF(1,IX/N)
          ENDIF
  40      MDIM  =  1 + (M-1)*IX + (N-1)*JX
          IF(RANF() .GT. 0.5)  IX  =  -IX
          IF(RANF() .GT. 0.5)  JX  =  -JX
          LC  =  (ITDIM - MDIM) / 2
          LA11  =  LC
          IF(IX .LT. 0)  LA11  =  LA11 - IX*(M-1)
          IF(JX .LT. 0)  LA11  =  LA11 - JX*(N-1)
          LA12  =  LA11 + JX
          LA21  =  LA11 + IX
          LA11  =  LA11*LENGTH + 1
          LA12  =  LA12*LENGTH + 1
          LA21  =  LA21*LENGTH + 1
          RETURN
          END
          SUBROUTINE CHECKL(M,N,F,IRDIM,G,LG,LG12,LG21,OK,VDIST,VSET)
          REAL      F(*),     G(IRDIM)
          LOGICAL   OK
*JMM
          EXTERNAL  VDIST,    VSET
          DOUBLE PRECISION    DVOID
          REAL                RVOID(2)
          COMMON/CH3ALIGN/    DVOID
          EQUIVALENCE         (DVOID,RVOID(1))
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          NDX2F(I,J)  =  ((J-1)*IDIM + I-1)*LENGTH + 1
          NDX2G(I,J)  =  (I-1)*IG + (J-1)*JG + LG
          IG  =  0
          JG  =  0
          IF(M .GT. 1)  IG  =  LG21 - LG
          IF(N .GT. 1)  JG  =  LG12 - LG
          OK  =  .TRUE.
          DO 10     I  =  1, M
             I1F  =  NDX2F(I,1)
             I2F  =  NDX2F(I,2)
             I1G  =  NDX2G(I,1)
             I2G  =  NDX2G(I,2)
             E  =  VDIST(N,F(I1F),F(I2F),G(I1G),G(I2G))
             IF(E .NE. 0.)  GOTO 91
*            CALL VSET(N,VOID(1),G(I1G),G(I2G))
*JMM
             IF(HTYPE.EQ.'D')  THEN
               RVOID(1) = VOID(1)
               RVOID(2) = VOID(2)
               CALL VSET(N,DVOID,  G(I1G),G(I2G))
             ELSE
               CALL VSET(N,VOID(1),G(I1G),G(I2G))
             ENDIF
  10         CONTINUE
          DO 20     J  =  1, IRDIM
             IF(G(J) .NE. VOID(1))  GOTO 92
  20         CONTINUE
          RETURN
  91      OK  =  .FALSE.
          WRITE(IOUNIT,1001)  I, E
          RETURN
  92      OK  =  .FALSE.
          WRITE(IOUNIT,1002)  J
          RETURN
1001      FORMAT(/ 30H ??? LOGIC ERROR OF FIRST KIND,I5, 1P, E12.3)
1002      FORMAT(/ 31H ??? LOGIC ERROR OF SECOND KIND,I7)
          END
#if defined(CERNLIB_NUMCD)
          IDENT     KFLUSH
          ENTRY     KFLUSH
 KFLUSH   JP        *+1S17              ENTRY/EXIT LINE
 +        SA1       N1TON5
          MX0       48
          BX2       -X0*X1              X2=N5
          LX1       12                  X1=N2 N3 N4 N5 N1
          SB5       X2                                      B5=N5
          BX2       -X0*X1              X2=N1
          LX1       12                  X1=N3 N4 N5 N1 N2
          SB1       X2                                      B1=N1
          BX2       -X0*X1              X2=N2
          LX1       12                  X1=N4 N5 N1 N2 N3
          SB2       X2                                      B2=N2
          BX2       -X0*X1              X2=N3
          LX1       12                  X1=N5 N1 N2 N3 N4
          SB3       X2                                      B3=N3
          BX2       -X0*X1              X2=N4
          BX6       X1                  X6=N5 N1 N2 N3 N4
          SB4       X2                                      B4=N4
          SA6       A1                  N1TON5 = N5 N1 N2 N3 N4
          SA1       A6+B1
          SA2       A6+B2
          SA3       A6+B3
          SA4       A6+B4
          SA5       A6+B5
          SB6       X1
          SB7       X2
          LX1       23
          LX2       23
          LX3       23
          LX4       23
          LX5       23
          SB1       X1
          SB2       X2
          SB3       X3
          SB4       X4
          SB5       X5
          BX6       X1
          BX7       X2
          SA6       A2
          SA7       A3
          BX6       X3
          BX7       X4
          SA6       A4
          SA7       A5
          BX6       X5
          LX0       X1,B1               KFLUSHX0
          SA6       A1
          JP        KFLUSH
 N1TON5   DATA      B00010004000200050003
          DATA      B67517144676213571322
          DATA      B25561154663002140654
          DATA      B53347352345101361316
          DATA      B73106476445465300106
          DATA      B26523571121415474312
          END
#endif
#if !defined(CERNLIB_NUMCD)
          SUBROUTINE KFLUSH
          RETURN
          END
#endif
          SUBROUTINE          RVMAXA(N,X,X2,I,E)
          REAL                X(*), X2(*), A
          REAL                E, T, ZERO, ABSF
          DATA      ZERO      / 0. /
          ABSF(A)  =  ABS(A)
          E  =  ZERO
          I  =  0
          IF(N .LE. 0)  RETURN
          E  =  ABSF(X(1))
          I  =  1
          IF(N .EQ. 1)  RETURN
          LXJ  =  1
#if (!defined(CERNLIB_NUMUC))&&(!defined(CERNLIB_NUMCR))&&(!defined(CERNLIB_NUMDE))
          JX  =  LOCF(X2) - LOCF(X)
#endif
#if defined(CERNLIB_NUMUC)||defined(CERNLIB_NUMCR)
          JX  =  LOC(X2) - LOC(X)
#endif
#if defined(CERNLIB_NUMDE)
          JX  =  (%LOC(X2) - %LOC(X)) / 4
#endif
          DO 10     J  =  2, N
             LXJ  =  LXJ + JX
             T    =  ABSF(X(LXJ))
             IF(T .LE. E)  GOTO 10
             E  =  T
             I  =  J
  10      CONTINUE
          RETURN
          END
          SUBROUTINE          DVMAXA(N,X,X2,I,E)
          DOUBLE PRECISION    X(*), X2(*), A
          REAL                E, T, ZERO, ABSF
          DATA      ZERO      / 0. /
          ABSF(A)  =  ABS(SNGL(A))
          E  =  ZERO
          I  =  0
          IF(N .LE. 0)  RETURN
          E  =  ABSF(X(1))
          I  =  1
          IF(N .EQ. 1)  RETURN
          LXJ  =  1
#if (!defined(CERNLIB_NUMUC))&&(!defined(CERNLIB_NUMCR))&&(!defined(CERNLIB_NUMDE))
          JX  =  (LOCF(X2) - LOCF(X)) / 2
#endif
#if defined(CERNLIB_NUMUC)||defined(CERNLIB_NUMCR)
          JX  =  (LOC(X2) - LOC(X)) / 2
#endif
#if defined(CERNLIB_NUMDE)
          JX  =  (%LOC(X2) - %LOC(X)) / 8
#endif
          DO 10     J  =  2, N
             LXJ  =  LXJ + JX
             T    =  ABSF(X(LXJ))
             IF(T .LE. E)  GOTO 10
             E  =  T
             I  =  J
  10      CONTINUE
          RETURN
          END
          SUBROUTINE          CVMAXA(N,X,X2,I,E)
          COMPLEX             X(*), X2(*), A
          REAL                E, T, ZERO, ABSF
          DATA      ZERO      / 0. /
          ABSF(A)  =  AMAX1(ABS(REAL(A)),ABS(AIMAG(A)))
          E  =  ZERO
          I  =  0
          IF(N .LE. 0)  RETURN
          E  =  ABSF(X(1))
          I  =  1
          IF(N .EQ. 1)  RETURN
          LXJ  =  1
#if (!defined(CERNLIB_NUMUC))&&(!defined(CERNLIB_NUMCR))&&(!defined(CERNLIB_NUMDE))
          JX  =  (LOCF(X2) - LOCF(X)) / 2
#endif
#if defined(CERNLIB_NUMUC)||defined(CERNLIB_NUMCR)
          JX  =  (LOC(X2) - LOC(X)) / 2
#endif
#if defined(CERNLIB_NUMDE)
          JX  =  (%LOC(X2) - %LOC(X)) / 8
#endif
          DO 10     J  =  2, N
             LXJ  =  LXJ + JX
             T    =  ABSF(X(LXJ))
             IF(T .LE. E)  GOTO 10
             E  =  T
             I  =  J
  10      CONTINUE
          RETURN
          END
          REAL FUNCTION       RABSR(X)
          REAL                X
          RABSR  =  ABS(X)
          RETURN
          END
          REAL FUNCTION       RABSD(X)
          DOUBLE PRECISION    X
          RABSD  =  ABS(SNGL(X))
          RETURN
          END
          REAL FUNCTION       RABSC(X)
          COMPLEX             X
          RABSC  =  AMAX1(ABS(REAL(X)),ABS(AIMAG(X)))
          RETURN
          END
          REAL FUNCTION       RVSUMA(N,X,X2)
                    REAL      X(*), X2(*)
          RVSUMA  =  0.
          IF(N .LE. 0)  RETURN
          LXJ  =  1
#if (!defined(CERNLIB_NUMUC))&&(!defined(CERNLIB_NUMCR))&&(!defined(CERNLIB_NUMDE))
          JX  =  LOCF(X2) - LOCF(X)
#endif
#if defined(CERNLIB_NUMUC)||defined(CERNLIB_NUMCR)
          JX  =  LOC(X2) - LOC(X)
#endif
#if defined(CERNLIB_NUMDE)
          JX  =  (%LOC(X2) - %LOC(X)) / 4
#endif
          DO 10     J  =  1, N
             RVSUMA  =  RVSUMA + ABS(X(LXJ))
             LXJ  =  LXJ + JX
 10          CONTINUE
          RETURN
          END
          REAL FUNCTION DVSUMA(N,X,X2)
          DOUBLE PRECISION    X(*), X2(*)
          DVSUMA  =  0.
          IF(N .LE. 0)  RETURN
          LXJ  =  1
#if (!defined(CERNLIB_NUMUC))&&(!defined(CERNLIB_NUMCR))&&(!defined(CERNLIB_NUMDE))
          JX  =  (LOCF(X2) - LOCF(X)) / 2
#endif
#if defined(CERNLIB_NUMUC)||defined(CERNLIB_NUMCR)
          JX  =  (LOC(X2) - LOC(X)) / 2
#endif
#if defined(CERNLIB_NUMDE)
          JX  =  (%LOC(X2) - %LOC(X)) / 8
#endif
          DO 10     J  =  1, N
             DVSUMA  =  DVSUMA + ABS(SNGL(X(LXJ)))
             LXJ  =  LXJ + JX
 10          CONTINUE
          RETURN
          END
          REAL FUNCTION       CVSUMA(N,X,X2)
          COMPLEX             X(*), X2(*)
          CVSUMA  =  0.
          IF(N .LE. 0)  RETURN
          LXJ  =  1
#if (!defined(CERNLIB_NUMUC))&&(!defined(CERNLIB_NUMCR))&&(!defined(CERNLIB_NUMDE))
          JX  =  (LOCF(X2) - LOCF(X)) / 2
#endif
#if defined(CERNLIB_NUMUC)||defined(CERNLIB_NUMCR)
          JX  =  (LOC(X2) - LOC(X)) / 2
#endif
#if defined(CERNLIB_NUMDE)
          JX  =  (%LOC(X2) - %LOC(X)) / 8
#endif
          DO 10     J  =  1, N
          CVSUMA = CVSUMA + AMAX1(ABS(REAL(X(LXJ))),ABS(AIMAG(X(LXJ))))
          LXJ  =  LXJ + JX
 10          CONTINUE
          RETURN
          END
          SUBROUTINE RSCALE(N,A,IDIM,KHEX)
          REAL      A(IDIM,N)
          COEFF  =  16.**KHEX
          DO 20     I  =  1, N
             DO 10  J  =  1, N
                A(I,J)  =  COEFF * A(I,J)
  10            CONTINUE
  20         CONTINUE
          RETURN
          END
          SUBROUTINE DSCALE(N,A,IDIM,KHEX)
          DOUBLE PRECISION      A(IDIM,N)
          COEFF  =  16.**KHEX
          DO 20     I  =  1, N
             DO 10  J  =  1, N
                A(I,J)  =  COEFF * A(I,J)
  10            CONTINUE
  20         CONTINUE
          RETURN
          END
          SUBROUTINE CSCALE(N,A,IDIM,KHEX)
          COMPLEX      A(IDIM,N)
          COEFF  =  16.**KHEX
          DO 20     I  =  1, N
             DO 10  J  =  1, N
                A(I,J)  =  COEFF * A(I,J)
  10            CONTINUE
  20         CONTINUE
          RETURN
          END
          SUBROUTINE          RSETEX(N,A,IDIM,RELPR,KFAIL,MODE,K1,K2)
          REAL                A(IDIM,N), ZERO, ONE, UNIT, PERT, S
          DATA      ZERO      /  0.  /
          DATA      ONE       /  1.  /
          DATA      UNIT      /  1.  /
          DATA      IHEX      /  65 536  /
          DO 30     J  =  1, N
             S  =  ZERO
             JMINUS  =  J-1
             DO 10  I  =  1, JMINUS
                S  =  S + AINT(FLOAT(IHEX*16/N)*RANF()) / FLOAT(IHEX)
                A(I,J)  =  UNIT*S
  10            CONTINUE
             S  =  S + ONE
             DO 20  I  =  J, N
                A(I,J)  =  UNIT*S
  20            CONTINUE
  30         CONTINUE
          IF(N .EQ. 1)  GOTO 40
          IF(MODE .EQ. 1)  CALL RVCPY(N,A(1,K1),A(2,K1),A(1,K2),A(2,K2))
          IF(MODE .EQ. 2)  CALL RVCPY(N,A(K1,1),A(K1,2),A(K2,1),A(K2,2))
          IF(KFAIL .EQ. -1)  GOTO 40
          IF(KFAIL .EQ. 0)  ETA  =  4.*RELPR
          IF(MODE .EQ. 1)  KOL  =  K2
          IF(MODE .EQ. 2)  KOL  =  MIN0(K2+1,N)
          CALL RVMAXA(N,A(K2,1),A(K2,2),IDUMMY,ROW)
          PERT  =  ETA * SQRT(FLOAT(N)) * ROW * UNIT
          A(K2,KOL)  =  A(K2,KOL) + PERT
  40      IF(N .EQ. 1)  A(1,1)  =  ZERO
          RETURN
          END
          SUBROUTINE          DSETEX(N,A,IDIM,RELPR,KFAIL,MODE,K1,K2)
          DOUBLE PRECISION    A(IDIM,N), ZERO, ONE, UNIT, PERT, S
          DATA      ZERO      /  0.D0  /
          DATA      ONE       /  1.D0  /
          DATA      UNIT      /  1.D0  /
          DATA      IHEX      /  65 536  /
          DO 30     J  =  1, N
             S  =  ZERO
             JMINUS  =  J-1
             DO 10  I  =  1, JMINUS
                S  =  S +
     +             DBLE( AINT(FLOAT(IHEX*16/N)*RANF()) / FLOAT(IHEX) )
                A(I,J)  =  UNIT*S
  10            CONTINUE
             S  =  S + ONE
             DO 20  I  =  J, N
                A(I,J)  =  UNIT*S
  20            CONTINUE
  30         CONTINUE
          IF(N .EQ. 1)  GOTO 40
          IF(MODE .EQ. 1)  CALL DVCPY(N,A(1,K1),A(2,K1),A(1,K2),A(2,K2))
          IF(MODE .EQ. 2)  CALL DVCPY(N,A(K1,1),A(K1,2),A(K2,1),A(K2,2))
          IF(KFAIL .EQ. -1)  GOTO 40
          IF(KFAIL .EQ. 0)  ETA  =  4.*RELPR
          IF(MODE .EQ. 1)  KOL  =  K2
          IF(MODE .EQ. 2)  KOL  =  MIN0(K2+1,N)
          CALL DVMAXA(N,A(K2,1),A(K2,2),IDUMMY,ROW)
          PERT  =  ETA * SQRT(FLOAT(N)) * ROW * UNIT
          A(K2,KOL)  =  A(K2,KOL) + PERT
  40      IF(N .EQ. 1)  A(1,1)  =  ZERO
          RETURN
          END
          SUBROUTINE          CSETEX(N,A,IDIM,RELPR,KFAIL,MODE,K1,K2)
          COMPLEX             A(IDIM,N), ZERO, ONE, UNIT, PERT, S
          DATA      ZERO      /  (0.,0.)  /
          DATA      ONE       /  (1.,0.)  /
          DATA      UNIT      /  (1.,1.)  /
          DATA      IHEX      /  65 536  /
          DO 30     J  =  1, N
             S  =  ZERO
             JMINUS  =  J-1
             DO 10  I  =  1, JMINUS
                S  =  S + CMPLX(
     +                 AINT(FLOAT(IHEX*16/N)*RANF()) / FLOAT(IHEX),
     +                 AINT(FLOAT(IHEX*16/N)*RANF()) / FLOAT(IHEX) )
                A(I,J)  =  UNIT*S
  10            CONTINUE
             S  =  S + ONE
             DO 20  I  =  J, N
                A(I,J)  =  UNIT*S
  20            CONTINUE
  30         CONTINUE
          IF(N .EQ. 1)  GOTO 40
          IF(MODE .EQ. 1)  CALL CVCPY(N,A(1,K1),A(2,K1),A(1,K2),A(2,K2))
          IF(MODE.EQ.2)CALL CVCPY(N,A(K1,1),A(K1,2),A(K2,1),A(K2,2))
          IF(KFAIL .EQ. -1)  GOTO 40
          IF(KFAIL .EQ. 0)  ETA  =  4.*RELPR
          IF(MODE .EQ. 1)  KOL  =  K2
          IF(MODE .EQ. 2)  KOL  =  MIN0(K2+1,N)
          CALL CVMAXA(N,A(K2,1),A(K2,2),IDUMMY,ROW)
          PERT  =  ETA * SQRT(FLOAT(N)) * ROW * UNIT
          A(K2,KOL)  =  A(K2,KOL) + PERT
  40      IF(N .EQ. 1)  A(1,1)  =  ZERO
          RETURN
          END
          SUBROUTINE          RSSETX(N,A,IDIM,MODE,K1,K2)
          REAL                A(IDIM,N), ZERO, ONE, UNIT, S
          IRANF(I,K)  =  INT( RANF()*FLOAT(K-I+1) ) + I
          DATA      ZERO      /  0.  /
          DATA      ONE       /  1.  /
          DATA      UNIT      /  1.  /
          DATA      IEXP      /  50  /
          IF(N .EQ. 1) THEN
             A(1,1)  =  ZERO
          ELSE
             M  =  IEXP/N
             S  =  ZERO
             DO 30  J  =  1, N
                K  =  IRANF(1,M)
                S  =  S + 2**K
                DO 10  I  =  J, N
                   A(I,J)  =  UNIT*S
                   A(J,I)  =  A(I,J)
  10               CONTINUE
  30            CONTINUE
             CALL RVCPY(N,A(1,K1),A(2,K1),A(1,K2),A(2,K2))
             CALL RVCPY(N,A(K1,1),A(K1,2),A(K2,1),A(K2,2))
          ENDIF
          IF(MODE .EQ. 2) THEN
             DO 40  J  =  1, N
                A(J,J)  =  - UNIT
  40            CONTINUE
          ENDIF
          END
          SUBROUTINE          DSSETX(N,A,IDIM,MODE,K1,K2)
          DOUBLE PRECISION    A(IDIM,N), ZERO, ONE, UNIT, S
          IRANF(I,K)  =  INT( RANF()*FLOAT(K-I+1) ) + I
          DATA      ZERO      /  0.D0  /
          DATA      ONE       /  1.D0  /
          DATA      UNIT      /  1.D0  /
          DATA      IEXP      /  50  /
          IF(N .EQ. 1) THEN
             A(1,1)  =  ZERO
          ELSE
             M  =  IEXP/N
             S  =  ZERO
             DO 30  J  =  1, N
                K  =  IRANF(1,M)
                S  =  S + 2**K
                DO 10  I  =  J, N
                   A(I,J)  =  UNIT*S
                   A(J,I)  =  A(I,J)
  10               CONTINUE
  30            CONTINUE
             CALL DVCPY(N,A(1,K1),A(2,K1),A(1,K2),A(2,K2))
             CALL DVCPY(N,A(K1,1),A(K1,2),A(K2,1),A(K2,2))
          ENDIF
          IF(MODE .EQ. 2) THEN
             DO 40  J  =  1, N
                A(J,J)  =  - UNIT
  40            CONTINUE
          ENDIF
          END
          SUBROUTINE          CSETCR(N,A,IDIM,DET)
          COMPLEX             A(IDIM,N),DET,      RANGE,    CVSUM
          COMPLEX             C
          REAL                PIBY4N,   PHI
          DATA      RANGE     /  (+1.,+1.)  /
          CALL CVRAN(N,-RANGE,+RANGE,A,A(1,2))
          DET  =  CVSUM(N,A,A(1,2))
          IF(N .EQ. 1)  RETURN
          PIBY4N  =  ATAN(1.) / FLOAT(N)
          DO 11     J  =  1, N
             PHI  =  FLOAT(8*(J-1)) * PIBY4N
             A(2,J)  =  CMPLX(COS(PHI),SIN(PHI))
  11         CONTINUE
          DO 22     I  =  2, N
             C  =  (0.,0.)
             L  =  1
             DO 21  J  =  1, N
                C  =  C + A(1,J)*A(2,L)
                L  =  L + I-1
                IF(L .GT. N)  L  =  L-N
  21            CONTINUE
             DET  =  DET*C
  22         CONTINUE
          DO 31     I  =  2, N
             A(I,1)  =  A(I-1,N)
             CALL CVCPY(N-1,A(I-1,1),A(I-1,2),A(I,2),A(I,3))
  31         CONTINUE
          RETURN
          END
          SUBROUTINE          RSETCR(N,A,IDIM,DET)
          REAL                A(IDIM,N),DET,    RANGE,    RVSUM
          COMPLEX             C,        D
#include "kernnumt/sysdat.inc"
          REAL                PIBY4N,   PHI
          DATA      RANGE    /  1.  /
          CALL RVRAN(N,-RANGE,+RANGE,A,A(1,2))
          DET  =  RVSUM(N,A,A(1,2))
          IF(N .EQ. 1)  GOTO 90
          IF(N .GT. 2)  GOTO 10
             DET     =  DET*(A(1,1) - A(1,2))
             A(2,1)  =  A(1,2)
             A(2,2)  =  A(1,1)
             GOTO 90
  10      PIBY4N  =  ATAN(1.) / FLOAT(N)
          DO 11     J  =  1, N
             PHI  =  FLOAT(8*(J-1)) * PIBY4N
             A(2,J)  =  COS(PHI)
             A(3,J)  =  SIN(PHI)
  11      CONTINUE
          D  =  CMPLX(DET,0.)
          DO 22     I  =  2, N
             C  =  (0.,0.)
             L  =  1
             DO 21  J  =  1, N
                C  =  C + A(1,J) * CMPLX(A(2,L),A(3,L))
                L  =  L + I-1
                IF(L .GT. N)  L  =  L-N
  21            CONTINUE
             D  =  D * C
  22         CONTINUE
          DET  =  REAL(D)
          DO 31     I  =2, N
             A(I,1)  =  A(I-1,N)
             CALL RVCPY(N-1,A(I-1,1),A(I-1,2),A(I,2),A(I,3))
  31         CONTINUE
  90      RETURN
          END
          SUBROUTINE          DSETCR(N,A,IDIM,DET)
          DOUBLE PRECISION    A(IDIM,N),DET,      RANGE,    DVSUM
          DOUBLE PRECISION    CR,       CI,       DR,       DI
          DOUBLE PRECISION    PIBY4N,   PHI
          DATA      RANGE     /  1.D0  /
          CALL DVRAN(N,-RANGE,+RANGE,A,A(1,2))
          DET  =  DVSUM(N,A,A(1,2))
          IF(N .EQ. 1)  RETURN
          IF(N .GT. 2)  GOTO  10
             DET  =  DET * (A(1,1) - A(1,2))
             A(2,1)  =  A(1,2)
             A(2,2)  =  A(1,1)
             RETURN
  10      PIBY4N  =  DATAN(1.D0) / FLOAT(N)
          DO 11     J  =  1, N
             PHI  =  FLOAT(8*(J-1)) * PIBY4N
             A(2,J)  =  DCOS(PHI)
             A(3,J)  =  DSIN(PHI)
  11         CONTINUE
          DR  =  DET
          DI  =  0.D0
          DO 22     I  =  2, N
             CR  =  0.D0
             CI  =  0.D0
             L   =  1
             DO 21  J  =  1, N
                CR  =  CR + A(1,J) * A(2,L)
                CI  =  CI + A(1,J) * A(3,L)
                L   =  L + I-1
                IF(L .GT. N)  L  =  L - N
  21            CONTINUE
             DET  =  DR*CR - DI*CI
             DI   =  DR*CI + DI*CR
             DR   =  DET
  22         CONTINUE
          DO 31     I  =  2, N
             A(I,1)  =  A(I-1,N)
             CALL DVCPY(N-1,A(I-1,1),A(I-1,2),A(I,2),A(I,3))
  31         CONTINUE
          RETURN
          END
