*
* $Id: f012ch.F,v 1.2 1996/03/21 17:16:10 mclareni Exp $
*
* $Log: f012ch.F,v $
* Revision 1.2  1996/03/21 17:16:10  mclareni
* Kernnumt corrections for unaligned access on OSF1 by John Marafino, Fermilab
*
* Revision 1.1.1.1  1996/02/15 17:48:45  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
          SUBROUTINE F012CH(NREP,LWORK,W,OK)
          REAL       W(LWORK)
          LOGICAL   OK, OKT
#include "kernnumt/sysdat.inc"
          OK  =  .TRUE.
          DO 100    JREP  =  1, NREP
             CALL TF012(LWORK,W,OKT)
             OK  =  OK .AND. OKT
 100         CONTINUE
          RETURN
          END
          SUBROUTINE TF012(LWORK,W,OK)
          DIMENSION           W(LWORK), HT(3)
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
C         LOGICAL             OK,       ROK,      DOK,      COK
          LOGICAL             OK,       ROK,      DOK
          REAL                RZERO(1), RONE(1),  RALPHA(1),RBETA(1)
          REAL                RA1(1),   RR1(1),   RB1(1),   RD1(1)
          DOUBLE PRECISION    DZERO(1), DONE(1),  DALPHA(1),DBETA(1)
          DOUBLE PRECISION    DA1(1),   DR1(1),   DB1(1),   DD1(1)
          EXTERNAL  RSFACT,   RSFEQN,   RSFINV,   RSEQN,    RSINV
          EXTERNAL  RFACT,    RMMLT,    RMCPY,    RMMNA,    RMRAN
          EXTERNAL  RVCPY,    RVDIST,   RABSR,    RSCALE,   RSSETX
          EXTERNAL  RVMAXA,   RVSET,    RVSUMA,   RVXCH
          EXTERNAL  DSFACT,   DSFEQN,   DSFINV,   DSEQN,    DSINV
          EXTERNAL  DFACT,    DMMLT,    DMCPY,    DMMNA,    DMRAN
          EXTERNAL  DVCPY,    DVDIST,   RABSD,    DSCALE,   DSSETX
          EXTERNAL  DVMAXA,   DVSET,    DVSUMA,   DVXCH
          CHARACTER*4         HT
          DATA      HT        /  'R   ', 'D   ', 'C   '  /
          DATA      RZERO,    DZERO     / 0., 0.D0 /
          DATA      RONE,     DONE      / 1., 1.D0 /
          DATA      RALPHA,   DALPHA    / -1., -1.D0 /
          DATA      RBETA,    DBETA     / +1., +1.D0 /
          DATA      RA1,      DA1       / 3.333E20, 3.333D20 /
          DATA      RR1,      DR1       / 4.444E20, 4.444D20 /
          DATA      RB1,      DB1       / 5.555E20, 5.555D20 /
          DATA      RD1,      DD1       / 6.666E20, 6.666D20 /
          WRITE(IOUNIT,1001)
          HTYPE     =  HT(1)
          LENGTH    =  1
          TRELPR    =  RELPRT(1)
          MINHEX    =  MINHXT(1)
          MAXHEX    =  MAXHXT(1)
          LTAB      =  LTABT(1)
          CALL XF012(LWORK,W,RZERO,RONE,RALPHA,RBETA,RA1,RR1,RB1,RD1,
     +     ROK,RSFACT,RSFEQN,RSFINV,RSEQN,RSINV,RFACT,RMMLT,RMCPY,RMMNA,
     +     RMRAN,RSCALE,RSSETX,RABSR,RVCPY,RVDIST,RVMAXA,RVSET,
     +     RVSUMA,RVXCH)
          WRITE(IOUNIT,1002)
          HTYPE     =  HT(2)
          LENGTH    =  2
          TRELPR    =  RELPRT(2)
          MINHEX    =  MINHXT(2)
          MAXHEX    =  MAXHXT(2)
          LTAB      =  LTABT(2)
          CALL XF012(LWORK,W,DZERO,DONE,DALPHA,DBETA,DA1,DR1,DB1,DD1,
     +     DOK,DSFACT,DSFEQN,DSFINV,DSEQN,DSINV,DFACT,DMMLT,DMCPY,DMMNA,
     +     DMRAN,DSCALE,DSSETX,RABSD,DVCPY,DVDIST,DVMAXA,DVSET,
     +     DVSUMA,DVXCH)
          OK  =  ROK .AND. DOK
          IF(.NOT. OK)  WRITE(IOUNIT,1004)
          IF(      OK)  WRITE(IOUNIT,1005)
          RETURN
1001      FORMAT(17H1F012.  TYPE = R. )
1002      FORMAT(17H1F012.  TYPE = D. )
1003      FORMAT(17H1F012.  TYPE = C. )
1004      FORMAT(/ 5X, 37H ????? TEST OF F012 HAS FAILED. ????? )
1005      FORMAT(/ 9X, 44HACCEPTANCE TEST OF F012 HAS BEEN SUCCESSFUL ,
     +             38HIF ERROR MESSAGE TESTS ARE SUCCESSFUL. )
          END
          SUBROUTINE XF012(LWORK,W,ZERO,ONE,ALPHA,BETA,A1,R1,B1,D1,
     +      OK,SFACT,SFEQN,SFINV,SEQN,SINV,FACT,MMLT,MCPY,MMNA,
     +      MRAN,SCALE,SSETX,RABS,VCPY,VDIST,VMAXA,VSET,
     +      VSUMA,VXCH)
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
          DO 100    JTEST  =  1, 3
#endif
#if !defined(CERNLIB_NUMDE)
          DO 100    JTEST  =  1, 4
#endif
             OKT  =  .TRUE.
             KNTSKP  =  LTAB
             GOTO(13,14,15,16), JTEST
  13         WRITE(IOUNIT,1003)
             GOTO 20
  14         WRITE(IOUNIT,1004)
             GOTO 20
  15         WRITE(IOUNIT,1005) TRELPR
             GOTO 20
  16         WRITE(IOUNIT,1006)
             IDIM  =  2
             OKT   =  .FALSE.
             IF(LF .GE. 8) CALL TSMSG(W(LA),W(LR),W(LB),A1,R1,B1,D1,OKT,
     +                           SFACT,SFEQN,SFINV,SEQN,SINV,VCPY,VDIST)
             IF(LF .LT. 8)  WRITE(*,1015) LF
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
                GOTO(23, 24, 25), JTEST
  23            CALL TSSING(N,W(LA),OKN,SFACT,SSETX,RABS,VXCH)
                GOTO 29
  24            CALL TSDET(N,W(LA),W(LC),W(LR),ALPHA,BETA,OKN,
     +                    FACT,MCPY,MRAN,SCALE,VMAXA)
                GOTO 29
  25            CALL TSRESD(N,W(LA),W(LB),W(LC),W(LX),W(LR),W(LT),
     +             ZERO,ONE,ALPHA,BETA,OKN,SFACT,SFEQN,SFINV,SEQN,SINV,
     +             FACT,MMLT,MCPY,MMNA,MRAN,VCPY,VDIST,VMAXA,VSET,VSUMA)
  29            OKT  =  OKT .AND. OKN
  30            CONTINUE
             IF(KNTSKP .NE. 0) WRITE(*,1014) KNTSKP
             IF(      OKT)  WRITE(IOUNIT,1011)
  40         IF(.NOT. OKT)  WRITE(IOUNIT,1010)
             OK  =  OK .AND. OKT
 100         CONTINUE
          RETURN
1003      FORMAT(/// 18H SINGULARITY TEST. )
1004      FORMAT(/// 24H DETERMINANT RANGE TEST. )
1005      FORMAT(/// 15H RESIDUAL TEST.,
     +           // 8H RELPR =,1PE8.1,1H,, 5X,
     +       // 52H TABLE OF NORMALIZED RESIDUALS (AND OF RESID/RELPR).,
     +       // 20X, 9HEQUATIONS,26X, 9HINVERSION,25X, 11HDETERMINANT,
     +           /  4X, 1HN, 3(9X,4HMEAN,15X,3HMAX,4X)/)
1006      FORMAT(/// 20H ERROR MESSAGE TEST. )
1010      FORMAT(/ 5X, 37H ????? FEATURE TEST HAS FAILED. ????? )
1011      FORMAT(/ 5X, 25H FEATURE TEST SUCCESSFUL. )
1014      FORMAT( 20X, 11HWARNING ..., I3,
     +         52H CONFIGURATIONS HAVE BEEN SKIPPED FOR WANT OF SPACE.)
1015      FORMAT( / 11H ***** LF =, I10,
     +              38H INSUFFICIENT TO PERFORM MESSAGE TEST.)
          END
          SUBROUTINE TSSING(N,A,OK,SFACT,SSETX,RABS,VXCH)
          REAL      A(*),     DET(2)
*JMM
          DOUBLE PRECISION    DDET
          EQUIVALENCE         (DET(1),DDET)
          LOGICAL   OK,  OKT
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          IRANF(I,K)  =  INT( RANF()*FLOAT(K-I+1) ) + I
          NDX2F(I,J)  =  ((J-1)*IDIM + I-1)*LENGTH + 1
          OK  =  .TRUE.
          IF(N .LE. 0)  RETURN
          K1  =  -1
          K2  =  -1
          DO 80  MODE  =  1, 2
             DO 70  JREP  =  1, NCNFG
                IF(N .NE. 1) THEN
                   IF(JREP .EQ. 1) THEN
                      K1  =  1
                      K2  =  N
                   ELSE
                      K1  =  IRANF(1,N-1)
                      K2  =  IRANF(K1+1,N)
                   ENDIF
                ENDIF
                CALL SSETX(N,A,IDIM,MODE,K1,K2)
                IFAIL  =  -99
                JFAIL  =  -99
                CALL SFACT(N,A,IDIM,IFAIL,DET,JFAIL)
                E  =  RABS(DET)
                OKT  =  JFAIL .EQ. -2  .AND.  IFAIL .EQ. -1
                IF(.NOT. OKT) WRITE(IOUNIT,1001) N, IDIM, MODE, K1, K2,
     +                                          IFAIL, JFAIL, E
                OK  =  OK .AND. OKT
  70            CONTINUE
  80         CONTINUE
          RETURN
1001      FORMAT(/ 33H ??? ERROR DETECTED BY TSSING ...,7I5,1P,E12.3)
          END
          SUBROUTINE          TSDET(N,A,C,R,ALPHA,BETA,OK,
     +                             FACT,MCPY,MRAN,SCALE,VMAXA)
          REAL                A(*), C(*), R(*), DET(2)
*JMM
          DOUBLE PRECISION    DDET
          EQUIVALENCE         (DET(1),DDET)
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
             CALL VMAXA(1,DET,DUMMY,IDUMMY,RD)
             OKR  =  JFAIL .EQ. KFAIL
             OK   =  OK .AND. OKR
             IF(.NOT. OKR) WRITE(IOUNIT,1000) N, IDIM, JFAIL, KFAIL, RD
             IF(N .LE. 2)  GOTO 30
             CALL VMAXA(1,DET,DUMMY,IDUMMY,RABSD)
             DO 20  MODE  =  1, 2
                KFAIL  =  2*MODE - 3
                IF(MODE .EQ. 1)  MHEX  =  INT(1.1*FLOAT(MINHEX))
                IF(MODE. EQ. 2)  MHEX  =  INT(1.1*FLOAT(MAXHEX))
                P1  =  ALOG(16**(FLOAT(MHEX)/FLOAT(N)))
                P2  =  ALOG(RABSD) / FLOAT(N)
                HEX =  (P1 - P2) / ALOG(16.)
                IF(MODE .EQ. 1)  KHEX  =  INT(HEX) - 1
                IF(MODE .EQ. 2)  KHEX  =  INT(HEX) + 1
                CALL MCPY(N,N,A,A(L12),A(L21),C,C(L12),C(L21))
                CALL SCALE(N,C,IDIM,KHEX)
                CALL FACT(N,C,IDIM,R,IFAIL,DET,JFAIL)
                CALL VMAXA(1,DET,DUMMY,IDUMMY,RD)
                OKR  =  (JFAIL .EQ. KFAIL)  .AND.
     +                  ((MODE .EQ. 2)  .OR.  (RD .EQ. 0.))
                OK   =  OK  .AND.  OKR
                IF(.NOT. OKR) WRITE(IOUNIT,1000) N,IDIM,JFAIL,KFAIL,RD
  20            CONTINUE
  30         CONTINUE
          RETURN
1000      FORMAT(/32H ??? ERROR DETECTED BY TSDET ...,4I10,1P,E12.3)
          END
          SUBROUTINE TSRESD(N,A,B,C,X,R,T,ZERO,ONE,ALPHA,BETA,OK,
     +         SFACT,SFEQN,SFINV,SEQN,SINV,FACT,MMLT,MCPY,MMNA,MRAN,
     +         VCPY,VDIST,VMAXA,VSET,VSUMA)
          REAL      A(*), B(*), C(*), X(*), R(*), T(*)
          REAL      ZERO(2), ONE(2), ALPHA(2), BETA(2)
          LOGICAL   OK, OKF, OKM
          EXTERNAL  VCPY, VMAXA
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          REAL      DET(2),   DETREF(2)
*JMM
          DOUBLE PRECISION    DDET,   DDETREF
          EQUIVALENCE         (DET(1)   ,DDET)
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
             CALL MRAN(N,2*N,ALPHA,BETA,A,A(L12),A(L21))
             CALL MMLT(N,2*N,N,A,A(L12),A(L21),A,A(L21),A(L12),
     +                         A,A(L12),A(L21),C)
             I1  =  1
             DO 2   I  =  1, N
                I2  =  I1 + L12 - 1
                CALL VCPY(I,A(I1),A(I2),C(I1),C(I2))
                I1  =  I1 + L21 - 1
   2            CONTINUE
             CALL KFLUSH
             CALL SFACT(N,C,IDIM,IFAIL,DET,JFAIL)
             OKF  =  IFAIL .EQ. 0
             OK   =  OK .AND. OKF
             IF(.NOT. OKF)  WRITE(IOUNIT,1000) N, IDIM, JREP, IFAIL
             DO 10  K  =  1, KLIM
                CALL MRAN(N,K,ALPHA,BETA,B,B(L12),B(L21))
                CALL MCPY(N,K,B,B(L12),B(L21),X,X(L12),X(L21))
                CALL KFLUSH
                CALL SFEQN(N,C,IDIM,K,X)
                CALL RESIDU(N,K,A,B,X,T,R0,R1,ZERO,ONE,
     +                      MMNA,VCPY,VMAXA,VSET,VSUMA)
                RQ0  =  AMAX1(RQ0,R0)
                RQ1  =  RQ1 + R1
                I1  =  1
                DO 3   I  =  1, N
                   I2  =  I1 + L12 - 1
                   CALL VCPY(I,A(I1),A(I2),C(I1),C(I2))
                   I1  =  I1 + L21 - 1
   3               CONTINUE
                CALL MCPY(N,K,B,B(L12),B(L21),X,X(L12),X(L21))
                CALL KFLUSH
                CALL SEQN(N,C,IDIM,IFAIL,K,X)
                OKF  =  IFAIL .EQ. 0
                OK   =  OK .AND. OKF
                IF(.NOT. OKF)  WRITE(IOUNIT,1001) N,IDIM,JREP,K,IFAIL
                CALL RESIDU(N,K,A,B,X,T,R0,R1,ZERO,ONE,
     +                      MMNA,VCPY,VMAXA,VSET,VSUMA)
                RQ0  =  AMAX1(RQ0,R0)
                RQ1  =  RQ1 + R1
  10            CONTINUE
             CALL KFLUSH
             CALL SFINV(N,C,IDIM)
             CALL RESIDU(N,0,A,B,C,T,R0,R1,ZERO,ONE,
     +                   MMNA,VCPY,VMAXA,VSET,VSUMA)
             RI0  =  AMAX1(RI0,R0)
             RI1  =  RI1 + R1
             I1  =  1
             DO 11  I  =  1, N
                I2  =  I1 + L12 - 1
                CALL VCPY(I,A(I1),A(I2),C(I1),C(I2))
                I1  =  I1 + L21 - 1
  11            CONTINUE
             CALL KFLUSH
             CALL SINV(N,C,IDIM,IFAIL)
             OKF  =  IFAIL .EQ. 0
             OK   =  OK .AND. OKF
             IF(.NOT. OKF)  WRITE(IOUNIT,1002) N,IDIM,JREP,IFAIL
             CALL RESIDU(N,0,A,B,C,T,R0,R1,ZERO,ONE,
     +                   MMNA,VCPY,VMAXA,VSET,VSUMA)
             RI0  =  AMAX1(RI0,R0)
             RI1  =  RI1 + R1
             I1  =  1
             DO 12  I  =  1, N
                I2  =  I1 + L12 - 1
                CALL VCPY(I,A(I1),A(I2),C(I1),C(I2))
                I1  =  I1 + L21 - 1
  12            CONTINUE
             CALL FACT(N,A,IDIM,R,IFAIL1,DETREF,JFAIL1)
             CALL KFLUSH
             CALL SFACT(N,C,IDIM,IFAIL2,DET,JFAIL2)
             OKF  =  IFAIL1 .EQ. IFAIL2  .AND.  JFAIL1 .EQ. JFAIL2
             OK   =  OK .AND. OKF
             IF(.NOT. OKF) WRITE(IOUNIT,1003)
     +          N,IDIM,JREP,IFAIL1,IFAIL2,JFAIL1,JFAIL2
             IF(JFAIL1 .EQ. 0)  THEN
                CALL VMAXA(1,DETREF,DUMMY,IDUMMY,ABSRES)
                RELERR  =  VDIST(1,DET,DUMMY,DETREF,DUMMY) / ABSRES
                RD0  =  AMAX1(RD0,RELERR)
                RD1  =  RD1 + RELERR
             ENDIF
  20         CONTINUE
          RQ1  =  RQ1 / (2.* FLOAT(KLIM*NCNFG))
          RI1  =  RI1 / (2.* FLOAT(NCNFG))
          RD1  =  RD1 / FLOAT(NCNFG)
          NQ0  =  IRESF(RQ0)
          NQ1  =  IRESF(RQ1)
          NI0  =  IRESF(RI0)
          NI1  =  IRESF(RI1)
          ND0  =  IRESF(RD0)
          ND1  =  IRESF(RD1)
          MX0  =  200*N
          MX1  =  40*N
          OKM  =  (MAX0(NQ0,NI0,ND0) .LE. MX0) .AND.
     +            (MAX0(NQ1,NI1,ND1) .LE. MX1)
          OK   =  OK .AND. OKM
          IF(.NOT. OKM) WRITE(IOUNIT,1011) N, IDIM, NQ1, NI1, ND1, MX1,
     +                                           NQ0, NI0, ND0, MX0
          WRITE(IOUNIT,1012) N, RQ1, NQ1, RQ0, NQ0, RI1, NI1, RI0, NI0,
     +                                           RD1, ND1, RD0, ND0
          RETURN
1000      FORMAT(/35H ??? ERROR 0 DETECTED BY TSRESD ...,4I5)
1001      FORMAT(/35H ??? ERROR 1 DETECTED BY TSRESD ...,5I5)
1002      FORMAT(/35H ??? ERROR 2 DETECTED BY TSRESD ...,4I5)
1003      FORMAT(/35H ??? ERROR 3 DETECTED BY TSRESD ...,7I5)
1011      FORMAT(/36H ??? ERROR 11 DETECTED BY TSRESD ...,10(1X,I8))
1012      FORMAT(I5,1P,3(E12.1,2H (, I4,1H), E9.1,2H (, I4,1H) ))
          END
          SUBROUTINE TSMSG(A,R,B,A1,R1,B1,D1,OK,
     +                     SFACT,SFEQN,SFINV,SEQN,SINV,VCPY,VDIST)
          REAL                A(*),R(*),B(*),A1(2),R1(2),B1(2),D1(2)
          REAL                D(2), E(4)
*JMM
          DOUBLE PRECISION    DD
          EQUIVALENCE         (D(1),DD)
          LOGICAL             OK, OKT
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          DIMENSION           HSUB(5), HCNT(5)
          DIMENSION           NERR(5), KERR(5), LCNT(5)
          CHARACTER*4         HSUB,  HCNT
          DATA      HSUB  /  'FACT', 'EQN ', 'FEQN', 'INV ', 'FINV'  /
          DATA      HCNT  /  'THRE', ' FIV', ' FIV', 'THRE', 'THRE'  /
          DATA      LCNT  /  3,      5,      5,      3,      3  /
          DATA      NERR  /   0, -1, +2, +1, +1  /
          DATA      KERR  /  +1, +1, +1,  0, -1  /
          IDIM  =  1
          OK    =  .TRUE.
          DO 30     JSUB  =  1, 5
             IF(ERPRNT .AND. ERSTOP) WRITE(*,1001) HCNT(JSUB),
     +                                                HTYPE, HSUB(JSUB)
             IF(ERPRNT .AND. .NOT. ERSTOP) WRITE(*,1002) HCNT(JSUB),
     +                                                HTYPE, HSUB(JSUB)
             IF(.NOT. ERPRNT .AND. ERSTOP) WRITE(*,1003) HCNT(JSUB)
             L  =  LCNT(JSUB)
             DO 20  JERR  =  1, L
                N  =  NERR(JERR)
                K  =  KERR(JERR)
                IF(JSUB .EQ. 2  .AND.  K .EQ. 0)  K  =  -2
                CALL VCPY(1,A1,DUMMY,A,DUMMY)
                CALL VCPY(1,R1,DUMMY,R,DUMMY)
                CALL VCPY(1,B1,DUMMY,B,DUMMY)
                CALL VCPY(1,D1,DUMMY,D,DUMMY)
                GOTO(11, 12, 13, 14, 15), JSUB
  11               CALL SFACT(N,A,IDIM,IFAIL,D,JFAIL)
                   GOTO 19
  12               CALL SEQN(N,A,IDIM,IFAIL,K,B)
                   GOTO 19
  13               CALL SFEQN(N,A,IDIM,K,B)
                   GOTO 19
  14               CALL SINV(N,A,IDIM,IFAIL)
                   GOTO 19
  15               CALL SFINV(N,A,IDIM)
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
1001      FORMAT( / 1X,A4,32HE ERROR AND ABEND MESSAGES FROM ,A1,1HS,A4,
     +           19H SHOULD NOW FOLLOW. )
1002      FORMAT( / 1X, A4, 22HE ERROR MESSAGES FROM , A1, 1HS, A4,
     +           19H SHOULD NOW FOLLOW. )
1003      FORMAT( / 1X, A4, 35HE ABEND MESSAGES SHOULD NOW FOLLOW. )
1011      FORMAT( / 32H ??? ERROR DETECTED BY TSMSG ...,2I6, 1P, 4E12.3)
1012      FORMAT( // 47H RESULT ... ERROR MESSAGE TEST IS SUCCESSFUL IF,
     +    //12X,52H(1) THERE ARE NO MESSAGES CONTAINING QUESTION MARKS.,
     +      /12X, 49H(2) THE NUMBER OF MESSAGES IS AS SPECIFIED ABOVE.,
     +      /12X, 46H(3) NO TWO ERROR MESSAGES ARE IDENTICAL IN ALL,
     +            32H CHARACTERS, INCLUDING NUMERALS. )
          END
