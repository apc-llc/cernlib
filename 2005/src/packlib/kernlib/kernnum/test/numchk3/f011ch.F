*
* $Id: f011ch.F,v 1.2 1996/03/21 17:16:10 mclareni Exp $
*
* $Log: f011ch.F,v $
* Revision 1.2  1996/03/21 17:16:10  mclareni
* Kernnumt corrections for unaligned access on OSF1 by John Marafino, Fermilab
*
* Revision 1.1.1.1  1996/02/15 17:48:45  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
          SUBROUTINE F011CH(NREP,LWORK,W,OK)
          REAL      W(LWORK)
          LOGICAL   OK, OKT
#include "kernnumt/sysdat.inc"
          OK  =  .TRUE.
          DO 100    JREP  =  1, NREP
             CALL TF011(LWORK,W,OKT)
             OK  =  OK .AND. OKT
 100         CONTINUE
          RETURN
          END
          SUBROUTINE TF011(LWORK,W,OK)
          DIMENSION           W(LWORK), HT(3)
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          LOGICAL             OK,       ROK,      DOK,      COK
          REAL                RZERO(1), RONE(1),  RALPHA(1),RBETA(1)
          REAL                RA1(1),   RR1(1),   RB1(1),   RD1(1)
          DOUBLE PRECISION    DZERO(1), DONE(1),  DALPHA(1),DBETA(1)
          DOUBLE PRECISION    DA1(1),   DR1(1),   DB1(1),   DD1(1)
          COMPLEX             CZERO(1), CONE(1),  CALPHA(1),CBETA(1)
          COMPLEX             CA1(1),   CR1(1),   CB1(1),   CD1(1)
          EXTERNAL  RFACT,    RFEQN,    RFINV
          EXTERNAL  RMCPY,    RMMNA,    RMRAN
          EXTERNAL  RVCPY,    RVDIST
          EXTERNAL  RVMAXA,   RVSET,    RVSUMA,   RVXCH
          EXTERNAL  RABSR,    RSCALE,   RSETCR,   RSETEX
          EXTERNAL  DFACT,    DFEQN,    DFINV
          EXTERNAL  DMCPY,    DMMNA,    DMRAN
          EXTERNAL  DVCPY,    DVDIST
          EXTERNAL  DVMAXA,   DVSET,    DVSUMA,   DVXCH
          EXTERNAL  RABSD,    DSCALE,   DSETCR,   DSETEX
          EXTERNAL  CFACT,    CFEQN,    CFINV
          EXTERNAL  CMCPY,    CMMNA,    CMRAN
          EXTERNAL  CVCPY,    CVDIST
          EXTERNAL  CVMAXA,   CVSET,    CVSUMA,   CVXCH
          EXTERNAL  RABSC,    CSCALE,   CSETCR,   CSETEX
          CHARACTER*4         HT
          DATA      HT        /  'R   ', 'D   ', 'C  '  /
          DATA      RZERO,    DZERO,    CZERO  / 0., 0.D0, (0.,0.) /
          DATA      RONE,     DONE,     CONE   / 1., 1.D0, (1.,0.) /
          DATA      RALPHA,   DALPHA,   CALPHA / -1., -1.D0, (-1.,-1.) /
          DATA      RBETA,    DBETA,    CBETA  / +1., +1.D0, (+1.,+1.) /
          DATA      RA1, DA1, CA1 / 3.333E20, 3.333D20, (3.333E20,3.33)/
          DATA      RR1, DR1, CR1 / 4.444E20, 4.444D20, (4.444E20,4.44)/
          DATA      RB1, DB1, CB1 / 5.555E20, 5.555D20, (5.555E20,5.55)/
          DATA      RD1, DD1, CD1 / 6.666E20, 6.666D20, (6.666E20,6.66)/
          WRITE(IOUNIT,1001)
          HTYPE   =  HT(1)
          LENGTH  =  1
          TRELPR  =  RELPRT(1)
          LTAB    =  LTABT(1)
          MINHEX  =  MINHXT(1)
          MAXHEX  =  MAXHXT(1)
          CALL XF011(LWORK,W,RZERO,RONE,RALPHA,RBETA,RA1,RR1,RB1,RD1,
     +      ROK,RFACT,RFEQN,RFINV,RMCPY,RMMNA,RMRAN,RSCALE,RSETCR,
     +      RSETEX,RABSR,RVCPY,RVDIST,RVMAXA,RVSET,RVSUMA,RVXCH)
          WRITE(IOUNIT,1002)
          HTYPE   =  HT(2)
          LENGTH  =  2
          TRELPR  =  RELPRT(2)
          LTAB    =  LTABT(2)
          MINHEX  =  MINHXT(2)
          MAXHEX  =  MAXHXT(2)
          CALL XF011(LWORK,W,DZERO,DONE,DALPHA,DBETA,DA1,DR1,DB1,DD1,
     +      DOK,DFACT,DFEQN,DFINV,DMCPY,DMMNA,DMRAN,DSCALE,DSETCR,
     +      DSETEX,RABSD,DVCPY,DVDIST,DVMAXA,DVSET,DVSUMA,DVXCH)
          WRITE(IOUNIT,1003)
          HTYPE   =  HT(3)
          LENGTH  =  2
          TRELPR  =  RELPRT(1)
          LTAB    =  LTABT(3)
          MINHEX  =  MINHXT(3)
          MAXHEX  =  MAXHXT(3)
          CALL XF011(LWORK,W,CZERO,CONE,CALPHA,CBETA,CA1,CR1,CB1,CD1,
     +      COK,CFACT,CFEQN,CFINV,CMCPY,CMMNA,CMRAN,CSCALE,CSETCR,
     +      CSETEX,RABSC,CVCPY,CVDIST,CVMAXA,CVSET,CVSUMA,CVXCH)
          OK  =  ROK .AND. DOK .AND. COK
  90      IF(.NOT. OK)  WRITE(IOUNIT,1004)
          IF(      OK)  WRITE(IOUNIT,1005)
          RETURN
1001      FORMAT(17H1F011.  TYPE = R. )
1002      FORMAT(17H1F011.  TYPE = D. )
1003      FORMAT(17H1F011.  TYPE = C. )
1004      FORMAT(/ 5X, 37H ????? TEST OF F011 HAS FAILED. ????? )
1005      FORMAT(/ 9X, 44HACCEPTANCE TEST OF F011 HAS BEEN SUCCESSFUL ,
     +             38HIF ERROR MESSAGE TESTS ARE SUCCESSFUL. )
          END
          SUBROUTINE XF011(LWORK,W,ZERO,ONE,ALPHA,BETA,A1,R1,B1,D1,
     +       OK,FACT,FEQN,FINV,MCPY,MMNA,MRAN,SCALE,SETCR,
     +       SETEX,RABS,VCPY,VDIST,VMAXA,VSET,VSUMA,VXCH)
          REAL      W(LWORK), ZERO(2), ONE(2),  ALPHA(2),BETA(2)
          REAL      A1(2),    R1(2),   B1(2),   D1(2)
          LOGICAL   OK, OKN, OKT
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          NDX1F(J)  =  (J -1)*LENGTH + 1
          NDX2F(I,J)  =  ((J-1)*IDIM + I-1)*LENGTH + 1
          OK  =  .TRUE.
          LF  =  LWORK / 6
          LA  =  1
          LB  =  LA + LF
          LC  =  LB + LF
          LX  =  LC + LF
          LR  =  LX + LF
          LT  =  LR + LF
#if defined(CERNLIB_NUMDE)
          DO 100    JTEST  =  1, 4
#endif
#if !defined(CERNLIB_NUMDE)
          DO 100    JTEST  =  1, 5
#endif
             OKT  =  .TRUE.
             KNTSKP  =  LTAB
             GOTO(12,13,14,15,16), JTEST
  12         WRITE(IOUNIT,1002)
             GOTO 20
  13         WRITE(IOUNIT,1003)
             GOTO 20
  14         WRITE(IOUNIT,1004)
             GOTO 20
  15         WRITE(IOUNIT,1005) TRELPR
             GOTO 20
  16         WRITE(IOUNIT,1006)
             IDIM  =  2
             OKT   =  .FALSE.
             IF(LF .GE. 8) CALL TMSG(W(LA),W(LR),W(LB),A1,R1,B1,D1,
     +                               OKT,FACT,FEQN,FINV,VCPY,VDIST)
             IF(LF .LT. 8) WRITE(*,1015) LF
             GOTO 40
  20         DO 30  JTAB  =  1, LTAB
                N  =  NTAB(JTAB)
                IDIM  =  N
                IF(N .GE. 3  .AND.  N .LE. 7)  IDIM  =  N + 3
                IF(LF .LT. LENGTH*IDIM**2)  GOTO 30
                KNTSKP  =  KNTSKP - 1
                L2   =  NDX1F(2)
                L12  =  NDX2F(1,2)
                L21  =  NDX2F(2,1)
                GOTO(22, 23, 24, 25), JTEST
  22            CALL TPIVOT(N,W(LA),W(LC),W(LR),W(LT),ALPHA,BETA,OKN,
     +                    FACT,MCPY,MRAN,VCPY,VDIST,VMAXA,VXCH)
                GOTO 29
  23            CALL TSING(N,W(LA),W(LR),OKN,FACT,SETEX,RABS,VXCH)
                GOTO 29
   24            CALL TDET(N,W(LA),W(LC),W(LR),ALPHA,BETA,OKN,
     +                    FACT,MCPY,MRAN,SCALE,RABS)
                GOTO 29
  25            CALL TRESID(N,W(LA),W(LB),W(LC),W(LX),W(LR),W(LT),
     +              ZERO,ONE,ALPHA,BETA,OKN,FACT,FEQN,FINV,
     +            MCPY,MMNA,MRAN,RABS,SETCR,VCPY,VDIST,VMAXA,VSET,VSUMA)
  29            OKT  =  OKT .AND. OKN
  30            CONTINUE
             IF(KNTSKP .NE. 0)  WRITE(*,1014) KNTSKP
             IF(      OKT)  WRITE(IOUNIT,1011)
  40         IF(.NOT. OKT)  WRITE(IOUNIT,1010)
             OK  =  OK .AND. OKT
 100         CONTINUE
          IF(.NOT. OK)  WRITE(IOUNIT,1012)  HTYPE
          IF(      OK)  WRITE(IOUNIT,1013)  HTYPE
          RETURN
1002      FORMAT(/// 15H PIVOTING TEST. )
1003      FORMAT(/// 18H SINGULARITY TEST. )
1004      FORMAT(/// 24H DETERMINANT RANGE TEST. )
1005      FORMAT(/// 15H RESIDUAL TEST.,
     +           // 8H RELPR =, 1PE8.1,1H,, 5X,
     +       // 52H TABLE OF NORMALIZED RESIDUALS (AND OF RESID/RELPR).,
     +       // 20X, 9HEQUATIONS,26X, 9HINVERSION,25X, 11HDETERMINANT,
     +           /  4X, 1HN,3(9X,4HMEAN,15X,3HMAX,4X)/)
1006      FORMAT(/// 20H ERROR MESSAGE TEST. )
1010      FORMAT(/ 5X, 37H ????? FEATURE TEST HAS FAILED. ????? )
1011      FORMAT(/ 5X, 25H FEATURE TEST SUCCESSFUL. )
1012      FORMAT(/ 5X, 21H ????? TEST FOR TYPE ,A1,
     +            18H HAS FAILED. ?????  )
1013      FORMAT(/ 5X, 15H TEST FOR TYPE ,A1,15H SUCCESSFUL IF ,
     +             28HMESSAGE TEST WAS SUCCESSFUL. )
1014      FORMAT( 20X, 11HWARNING ..., I3,
     +         52H CONFIGURATIONS HAVE BEEN SKIPPED FOR WANT OF SPACE.)
1015      FORMAT(/ 11H ***** LF =, I10,
     +             38H INSUFFICIENT TO PERFORM MESSAGE TEST.)
          END
          SUBROUTINE TPIVOT(N,A,C,R,T,ALPHA,BETA,OK,
     +                      FACT,MCPY,MRAN,VCPY,VDIST,VMAXA,VXCH)
          REAL      A(*), C(*), R(*), T(*), ALPHA(2), BETA(2)
          LOGICAL   OK
          INTEGER   MIX(50)
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          REAL      DET(2),  E
*JMM
          DOUBLE PRECISION   DDET
          EQUIVALENCE        (DET(1),DDET)
          IRANF(I,K)  =  INT( RANF()*FLOAT(K-I+1) ) + I
          NDX2F(I,J)  =  ((J-1)*IDIM + I-1)*LENGTH + 1
          IF(N .GT. 50)  THEN
             OK  =  .FALSE.
             WRITE(IOUNIT,1000) N
             RETURN
          ENDIF
          OK  =  .TRUE.
          IF(N .LE. 1)  RETURN
  10      CALL  MRAN(N,N,ALPHA,BETA,A,A(L12),A(L21))
          L  =  0
          DO 11     I  =  1, N
             LI1  =  NDX2F(I,1)
             LI2  =  NDX2F(I,2)
             CALL VMAXA(N,A(LI1),A(LI2),J,E)
             IF(J .EQ. 1)  L  =  L + 1
  11         CONTINUE
          IF(L .GT. 1)  GOTO 10
          CALL MCPY(N,N,A,A(L12),A(L21),C,C(L12),C(L21))
          CALL FACT(N,C,IDIM,R,IFAIL,DET,JFAIL)
          CALL VCPY(N,C,C(L21),T,T(L2))
          CALL MCPY(N,N,A,A(L12),A(L21),C,C(L12),C(L21))
          DO 20     J  =  1, N
             MIX(J)  =  J
  20         CONTINUE
          DO 21     L  =  1, N-1
             K  =  IRANF(L,N)
             I  =  MIX(K)
             IF(K .EQ. L)  GOTO 21
                MIX(K)  =  MIX(L)
                MIX(L)  =  I
                LK1  =  NDX2F(K,1)
                LK2  =  NDX2F(K,2)
                LL1  =  NDX2F(L,1)
                LL2  =  NDX2F(L,2)
                CALL VXCH(N,C(LK1),C(LK2),C(LL1),C(LL2))
  21         CONTINUE
          CALL FACT(N,C,IDIM,R,IFAIL,DET,JFAIL)
          E  =  VDIST(N,C,C(L21),T,T(L2))
          IF(E .EQ. 0) GOTO 30
             OK  =  .FALSE.
             WRITE(IOUNIT,1001)  N, IDIM, E
  30      RETURN
1000      FORMAT(/ ' ??? ERROR DETECTED BY TPIVOT.  N =',I5,
     +           ' EXCEEDS INTERNAL LIMIT OF 50')
1001      FORMAT(/ 35H ??? ERROR DETECTED BY TPIVOT.  N =,I5,
     +           3X,7H IDIM =,I5, 3X, 4H E =,1PE12.3)
          END
          SUBROUTINE TSING(N,A,R,OK,FACT,SETEX,RABS,VXCH)
          REAL      A(*),     R(*),     DET(2)
          LOGICAL   OK,       OKDET
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          IRANF(I,K)  =  INT( RANF()*FLOAT(K-I+1) ) + I
          NDX2F(I,J)  =  ((J-1)*IDIM + I-1)*LENGTH + 1
          OK  =  .TRUE.
          IF(N .LE. 0)  RETURN
          IF(N .EQ. 1)  KINDS  =  1
          IF(N .NE. 1)  KINDS  =  2
          DO 90     KIND  =  1, KINDS
             KFAIL  =  KIND - 2
  20         DO 80  MODE  =  1, 2
                DO 70  JREP  =  1, NCNFG
                   IF(N .EQ. 1)  GOTO 30
                   IF(JREP .NE. 1)  GOTO 21
                      K1  =  1
                      K2  =  N
                      GOTO 30
  21               K1  =  IRANF(1,N-1)
                   K2  =  IRANF(K1+1,N)
  30               CALL SETEX(N,A,IDIM,TRELPR,KFAIL,MODE,K1,K2)
  50               IF(N .EQ. 2) GOTO 60
                   DO 51  I  =  2, N
                      K  =  IRANF(1,N)
                      L  =  IRANF(1,N)
                      IF(K .EQ. L) GOTO 51
                      LK1  =  NDX2F(K,1)
                      LK2  =  NDX2F(K,2)
                      LL1  =  NDX2F(L,1)
                      LL2  =  NDX2F(L,2)
                      CALL VXCH(N,A(LK1),A(LK2),A(LL1),A(LL2))
  51                  CONTINUE
  60               IFAIL  =  -99
                   JFAIL  =  -99
                   CALL FACT(N,A,IDIM,R,IFAIL,DET,JFAIL)
                   E  =  RABS(DET)
                   OKDET  =  (KFAIL .NE. -1) .OR.
     +                       (E .EQ. 0.  .AND. JFAIL .EQ. 0)
                   IF(OKDET .AND. (IFAIL .EQ. KFAIL))  GOTO 70
                      OK  =  .FALSE.
                      WRITE(IOUNIT,1001) N, IDIM, KIND, MODE, K1, K2,
     +                                KFAIL, IFAIL, JFAIL, E
  70               CONTINUE
  80            CONTINUE
  90         CONTINUE
          RETURN
1001      FORMAT(/ 32H ??? ERROR DETECTED BY TSING ...,9I5,1P,E12.3)
          END
          SUBROUTINE TDET(N,A,C,R,ALPHA,BETA,OK,
     +                    FACT,MCPY,MRAN,SCALE,RABS)
          REAL                A(*), C(*), R(*), DET(2)
*JMM
          DOUBLE PRECISION    DDET
          EQUIVALENCE         (DET(1),DDET)
          REAL                ALPHA(2),   BETA(2)
          LOGICAL             OK,   OKR
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          OK  =  .TRUE.
          IF(N .LE. 0)  RETURN
          DO 30     JREP  =  1, NCNFG
             CALL MRAN(N,N,ALPHA,BETA,A,A(L12),A(L21))
             CALL MCPY(N,N,A,A(L12),A(L21),C,C(L12),C(L21))
             CALL FACT(N,A,IDIM,R,IFAIL,DET,JFAIL)
             KFAIL  =  0
             ABSDET  =  RABS(DET)
             OKR  =  JFAIL .EQ. KFAIL
             OK   =  OK .AND. OKR
             IF(.NOT. OKR) WRITE(IOUNIT,1000) N,IDIM,JFAIL,KFAIL,ABSDET
             IF(N .LE. 2)  GOTO 30
             DO 20  MODE  =  1, 2
                KFAIL  =  2*MODE - 3
                IF(MODE .EQ. 1)  MHEX  =  INT(1.1*FLOAT(MINHEX))
                IF(MODE. EQ. 2)  MHEX  =  INT(1.1*FLOAT(MAXHEX))
                P1  =  ALOG(16**(FLOAT(MHEX)/FLOAT(N)))
                P2  =  ALOG(ABSDET) / FLOAT(N)
                HEX =  (P1 - P2) / ALOG(16.)
                IF(MODE .EQ. 1)  KHEX  =  INT(HEX) - 1
                IF(MODE .EQ. 2)  KHEX  =  INT(HEX) + 1
                CALL MCPY(N,N,A,A(L12),A(L21),C,C(L12),C(L21))
                CALL SCALE(N,C,IDIM,KHEX)
                CALL FACT(N,C,IDIM,R,IFAIL,DET,JFAIL)
                RD  =  RABS(DET)
                OKR  =  (JFAIL .EQ. KFAIL)  .AND.
     +                  ((MODE .EQ. 2)  .OR.  (RD .EQ. 0.))
                OK   =  OK  .AND.  OKR
                IF(.NOT. OKR) WRITE(IOUNIT,1000)N,IDIM,JFAIL,KFAIL,RD
  20            CONTINUE
  30         CONTINUE
          RETURN
1000      FORMAT(/31H ??? ERROR DETECTED BY TDET ..., 4I10,1P,E12.3)
          END
          SUBROUTINE TRESID(N,A,B,C,X,R,T,ZERO,ONE,
     +           ALPHA,BETA,OK,FACT,FEQN,FINV,MCPY,MMNA,MRAN,RABS,SETCR,
     +               VCPY,VDIST,VMAXA,VSET,VSUMA)
          REAL      A(*), B(*), C(*), X(*), R(*), T(*)
          REAL      ZERO(2), ONE(2), ALPHA(2), BETA(2)
          LOGICAL   OK
          EXTERNAL  VMAXA
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          REAL      DET(2),   DETREF(2)
*JMM
          DOUBLE PRECISION    DDET, DDETREF
          EQUIVALENCE         (DET(1),   DDET)
          EQUIVALENCE         (DETREF(1),DDETREF)
          DATA      KLIM    / 3 /
          IRESF(RES)  =  NINT(RES/TRELPR)
          OK  =  .TRUE.
          IF(N .LE. 0)  RETURN
          RQ0  =  0.
          RQ1  =  0.
          RI0  =  0.
          RI1  =  0.
          RD0  =  0.
          RD1  =  0.
          DO 20     JREP  =  1, NCNFG
             CALL MRAN(N,N,ALPHA,BETA,A,A(L12),A(L21))
             CALL MCPY(N,N,A,A(L12),A(L21),C,C(L12),C(L21))
             CALL KFLUSH
             CALL FACT(N,C,IDIM,R,IFAIL,DET,JFAIL)
             IF(IFAIL .NE. 0)  WRITE(IOUNIT,1000) N, JREP, IDIM, IFAIL
             DO 10  K  =  1, KLIM
                CALL MRAN(N,K,ALPHA,BETA,B,B(L12),B(L21))
                CALL MCPY(N,K,B,B(L12),B(L21),X,X(L12),X(L21))
                CALL KFLUSH
                CALL FEQN(N,C,IDIM,R,K,X)
                CALL RESIDU(N,K,A,B,X,T,R0,R1,ZERO,ONE,
     +                      MMNA,VCPY,VMAXA,VSET,VSUMA)
                RQ0  =  AMAX1(RQ0,R0)
                RQ1  =  RQ1 + R1
  10            CONTINUE
             CALL KFLUSH
             CALL FINV(N,C,IDIM,R)
             CALL RESIDU(N,0,A,B,C,T,R0,R1,ZERO,ONE,
     +                   MMNA,VCPY,VMAXA,VSET,VSUMA)
             RI0  =  AMAX1(RI0,R0)
             RI1  =  RI1 + R1
             CALL SETCR(N,C,IDIM,DETREF)
             CALL KFLUSH
             CALL FACT(N,C,IDIM,R,IFAIL,DET,JFAIL)
             RELERR  =  VDIST(1,DET,DUMMY,DETREF,DUMMY) / RABS(DETREF)
             RD0  =  AMAX1(RD0,RELERR)
             RD1  =  RD1 + RELERR
  20         CONTINUE
          RQ1  =  RQ1 / FLOAT(KLIM*NCNFG)
          RI1  =  RI1 / FLOAT(NCNFG)
          RD1  =  RD1 / FLOAT(NCNFG)
          NQ0  =  IRESF(RQ0)
          NQ1  =  IRESF(RQ1)
          NI0  =  IRESF(RI0)
          NI1  =  IRESF(RI1)
          ND0  =  IRESF(RD0)
          ND1  =  IRESF(RD1)
          MX0  =  200*N
          MX1  =  40*N
          OK  =  (MAX0(NQ0,NI0,ND0) .LE. MX0) .AND.
     +           (MAX0(NQ1,NI1,ND1) .LE. MX1)
          IF(.NOT. OK)  WRITE(IOUNIT,1001) N, IDIM, NQ1, NI1, ND1, MX1,
     +                                           NQ0, NI0, ND0, MX0
          WRITE(IOUNIT,1002) N, RQ1, NQ1, RQ0, NQ0, RI1, NI1, RI0, NI0,
     +                                           RD1, ND1, RD0, ND0
          RETURN
1000      FORMAT(/33H ??? ERROR DETECTED BY TRESID ..., 4I5)
1001      FORMAT(/33H ??? ERROR DETECTED BY TRESID ...,/10(1X,I10))
1002      FORMAT(I5,1P,3(E12.1,2H (,I4,1H),E9.1,2H (,I4,1H) ))
          END
          SUBROUTINE RESIDU (N,K,A,B,X,T,R0,R1,ZERO,ONE,
     +               MMNA,VCPY,VMAXA,VSET,VSUMA)
          REAL                A(*), B(*), X(*), T(*)
          REAL                ZERO(2), ONE(2), R0, R1, SJ
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          NDX1F(J)    =   (J - 1)*LENGTH + 1
          NDX2F(I,J)  =  ((J - 1)*IDIM + I-1)*LENGTH + 1
 
          R0  =  0.
          R1  =  0.
          L  =  K
          IF(K .EQ. 0)  L  =  N
          DO 20     J  =  1, L
             LJ   =  NDX1F(J)
             L1J  =  NDX2F(1,J)
             L2J  =  NDX2F(2,J)
             IF(K .EQ. 0)  GOTO 11
             CALL VCPY(N,B(L1J),B(L2J),T,T(L2))
             GO TO 12
  11         CALL VSET(N,ZERO,T,T(L2))
             CALL VSET(1,ONE,T(LJ),T(LJ))
  12         CALL MMNA(N,N,A,A(L12),A(L21),X(L1J),X(L2J),T,T(L2))
             CALL VMAXA(N,X(L1J),X(L2J),IDUMMY,SJ)
             CALL VMAXA(N,T,T(L2),IDUMMY,ABSRES)
             R0  =  AMAX1(R0, ABSRES/SJ)
             SUMRES  =  VSUMA(N,T,T(L2))
             R1  =  R1 + SUMRES / SJ
  20         CONTINUE
          R1  =  R1 / FLOAT(N*L)
          RETURN
          END
          SUBROUTINE          TMSG(A,R,B,A1,R1,B1,D1,
     +                             OK,FACT,FEQN,FINV,VCPY,VDIST)
          REAL                A(*),R(*),B(*),A1(2),R1(2),B1(2),D1(2)
          REAL                D(2), E(4)
*JMM
          DOUBLE PRECISION    DD
          EQUIVALENCE         (D(1),DD)
          LOGICAL             OK, OKT
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          CHARACTER*4         HSUB(3), HCNT(3)
          INTEGER             NERR(5), KERR(5)
          DATA                HSUB  /  'FACT', 'FEQN', 'FINV'  /
          DATA                HCNT  /  'THRE', ' FIV', 'THRE'  /
          DATA                NERR  /   0, -1, +2, +1, +1  /
          DATA                KERR  /  +1, +1, +1,  0, -1  /
          IDIM  =  1
          OK    =  .TRUE.
          DO 30     JSUB  =  1, 3
             IF(ERPRNT .AND. ERSTOP) WRITE(*,1001) HCNT(JSUB),
     +                                                HTYPE, HSUB(JSUB)
             IF(ERPRNT .AND. .NOT. ERSTOP) WRITE(*,1002) HCNT(JSUB),
     +                                                HTYPE, HSUB(JSUB)
             IF(.NOT. ERPRNT .AND. ERSTOP) WRITE(*,1003) HCNT(JSUB)
             L  =  3
             IF(JSUB .EQ. 2)  L  =  5
             DO 20  JERR  =  1, L
                N  =  NERR(JERR)
                K  =  KERR(JERR)
                CALL VCPY(1,A1,DUMMY,A,DUMMY)
                CALL VCPY(1,R1,DUMMY,R,DUMMY)
                CALL VCPY(1,B1,DUMMY,B,DUMMY)
                CALL VCPY(1,D1,DUMMY,D,DUMMY)
                GOTO(11, 12, 13), JSUB
  11               CALL FACT(N,A,IDIM,R,IFAIL,D,JFAIL)
                   GOTO 19
  12               CALL FEQN(N,A,IDIM,R,K,B)
                   GOTO 19
  13               CALL FINV(N,A,IDIM,R)
  19            E(1)  =  VDIST(1,A,DUMMY,A1,DUMMY)
                E(2)  =  VDIST(1,R,DUMMY,R1,DUMMY)
                E(3)  =  VDIST(1,B,DUMMY,B1,DUMMY)
                E(4)  =  VDIST(1,D,DUMMY,D1,DUMMY)
                OKT  =  (E(1) .EQ. 0.) .AND. (E(2). EQ. 0.) .AND.
     +                  (E(3) .EQ. 0.) .AND. (E(4) .EQ. 0.)
                IF(.NOT. OKT) WRITE(*,1011) JSUB, JERR, E
                OK  =  OK .AND. OKT
  20            CONTINUE
  30         CONTINUE
          IF( ERPRNT .OR. ERSTOP ) WRITE(*,1012)
          RETURN
1001      FORMAT( / 1X, A4, 32HE ERROR AND ABEND MESSAGES FROM , A1, A4,
     +           19H SHOULD NOW FOLLOW. )
1002      FORMAT( / 1X, A4, 22HE ERROR MESSAGES FROM , A1, A4,
     +           19H SHOULD NOW FOLLOW. )
1003      FORMAT( / 1X, A4, 35HE ABEND MESSAGES SHOULD NOW FOLLOW. )
1011      FORMAT( / 31H ??? ERROR DETECTED BY TMSG ..., 2I6, 1P, 4E12.3)
1012      FORMAT( // 47H RESULT ... ERROR MESSAGE TEST IS SUCCESSFUL IF,
     1    //12X,52H(1) THERE ARE NO MESSAGES CONTAINING QUESTION MARKS.,
     2      /12X, 49H(2) THE NUMBER OF MESSAGES IS AS SPECIFIED ABOVE.,
     3      /12X, 46H(3) NO TWO ERROR MESSAGES ARE IDENTICAL IN ALL,
     4            32H CHARACTERS, INCLUDING NUMERALS. )
          END
