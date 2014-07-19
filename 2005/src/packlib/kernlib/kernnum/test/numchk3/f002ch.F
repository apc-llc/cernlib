*
* $Id: f002ch.F,v 1.2 1996/03/21 17:16:08 mclareni Exp $
*
* $Log: f002ch.F,v $
* Revision 1.2  1996/03/21 17:16:08  mclareni
* Kernnumt corrections for unaligned access on OSF1 by John Marafino, Fermilab
*
* Revision 1.1.1.1  1996/02/15 17:48:43  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
          SUBROUTINE F002CH(NREP,LWORK,W,OK)
          REAL      W(LWORK)
          LOGICAL   OK, OKT
#include "kernnumt/sysdat.inc"
          OK  =  .TRUE.
          DO 100    JREP  =  1, NREP
             CALL TF002(LWORK,W,OKT)
             OK  =  OK .AND. OKT
 100         CONTINUE
          RETURN
          END
          SUBROUTINE TF002(LWORK,W,OK)
          DIMENSION           W(LWORK), HT(3)
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
#include "f002dat1.inc"
          LOGICAL             OK,       ROK,      DOK,      COK
*         REAL                RALPHA(1),RBETA(1)
*         DOUBLE PRECISION    DALPHA(1),DBETA(1)
*         COMPLEX             CALPHA(1),CBETA(1)
          EXTERNAL  RVADD,    RVCPY
          EXTERNAL  RVMUL,    RVMULA,   RVMUNA,   RVRAN
          EXTERNAL  RVSCA,    RVSCL,    RVSCS,    RVSET,    RVSUB
          EXTERNAL  RVXCH
          EXTERNAL  DVADD,    DVCPY
          EXTERNAL  DVMUL,    DVMULA,   DVMUNA,   DVRAN
          EXTERNAL  DVSCA,    DVSCL,    DVSCS,    DVSET,    DVSUB
          EXTERNAL  DVXCH
          EXTERNAL  CVADD,    CVCPY
          EXTERNAL  CVMUL,    CVMULA,   CVMUNA,   CVRAN
          EXTERNAL  CVSCA,    CVSCL,    CVSCS,    CVSET,    CVSUB
          EXTERNAL  CVXCH
          EXTERNAL  RAADD,    RADIV,    RAMUL,    RASUB,    RVMAXA
          EXTERNAL  RAMULA,   RAMUNA,   RVRANT,   RVDIST,   RVDIVT
          EXTERNAL  RASCA,    RASCL,    RASCS,    RVSUMT,   RASUMT
          EXTERNAL  RVMPAT,   RAMPAT,   RVMPYT,   RAMPYT
          EXTERNAL  DAADD,    DADIV,    DAMUL,    DASUB,    DVMAXA
          EXTERNAL  DAMULA,   DAMUNA,   DVRANT,   DVDIST,   DVDIVT
          EXTERNAL  DASCA,    DASCL,    DASCS,    DVSUMT,   DASUMT
          EXTERNAL  DVMPAT,   DAMPAT,   DVMPYT,   DAMPYT
          EXTERNAL  CAADD,    CADIV,    CAMUL,    CASUB,    CVMAXA
          EXTERNAL  CAMULA,   CAMUNA,   CVRANT,   CVDIST,   CVDIVT
          EXTERNAL  CASCA,    CASCL,    CASCS,    CVSUMT,   CASUMT
          EXTERNAL  CVMPAT,   CAMPAT,   CVMPYT,   CAMPYT
          CHARACTER*4         HT
          DATA      HT        / 'R   ', 'D   ', 'C   '  /
#include "f002dat2.inc"
*         DATA      RALPHA,   RBETA     /  -100.,   +100.       /
*         DATA      DALPHA,   DBETA     /  -100.D0, +100.D0     /
*         DATA      CALPHA,   CBETA     / (-100.,-100.),(+100.,+100.)/
          WRITE(IOUNIT,1001)
          HTYPE     =  HT(1)
          LENGTH    =  1
          TRELPR    =  RELPRT(1)
          LTAB      =  LTABT(1)
          CALL XF002(LWORK,W,RALPHA,RBETA,ROK,RVADD,RVCPY,RVDIVT,
     +               RVMUL,RVMULA,RVMUNA,RVRAN,RVSCA,RVSCL,RVSCS,
     +               RVSET,RVSUB,RVXCH,RVMPYT,RAMPYT,RVRANT,RVMAXA,
     +               RASCA,RASCL,RASCS,RVMPAT,RAMPAT,RVSUMT,RASUMT,
     +               RAADD,RADIV,RAMUL,RAMULA,RAMUNA,RASUB,RVDIST)
          WRITE(IOUNIT,1002)
          HTYPE     =  HT(2)
          LENGTH    =  2
          TRELPR    =  RELPRT(2)
          LTAB      =  LTABT(2)
          CALL XF002(LWORK,W,DALPHA,DBETA,DOK,DVADD,DVCPY,DVDIVT,
     +               DVMUL,DVMULA,DVMUNA,DVRAN,DVSCA,DVSCL,DVSCS,
     +               DVSET,DVSUB,DVXCH,DVMPYT,DAMPYT,DVRANT,DVMAXA,
     +               DASCA,DASCL,DASCS,DVMPAT,DAMPAT,DVSUMT,DASUMT,
     +               DAADD,DADIV,DAMUL,DAMULA,DAMUNA,DASUB,DVDIST)
          WRITE(IOUNIT,1003)
          HTYPE     =  HT(3)
          LENGTH    =  2
          TRELPR    =  RELPRT(1)
          LTAB      =  LTABT(3)
          CALL XF002(LWORK,W,CALPHA,CBETA,COK,CVADD,CVCPY,CVDIVT,
     +               CVMUL,CVMULA,CVMUNA,CVRAN,CVSCA,CVSCL,CVSCS,
     +               CVSET,CVSUB,CVXCH,CVMPYT,CAMPYT,CVRANT,CVMAXA,
     +               CASCA,CASCL,CASCS,CVMPAT,CAMPAT,CVSUMT,CASUMT,
     +               CAADD,CADIV,CAMUL,CAMULA,CAMUNA,CASUB,CVDIST)
          OK  =  ROK .AND. DOK .AND. COK
  90      IF(.NOT. OK)  WRITE(IOUNIT,1004)
          IF(      OK)  WRITE(IOUNIT,1005)
          RETURN
1001      FORMAT(17H1F002.  TYPE = R. )
1002      FORMAT(17H1F002.  TYPE = D. )
1003      FORMAT(17H1F002.  TYPE = C. )
1004      FORMAT(/ 5X, 37H ????? TEST OF F002 HAS FAILED. ????? )
1005      FORMAT(/35X, 44HACCEPTANCE TEST OF F002 HAS BEEN SUCCESSFUL. )
          END
          SUBROUTINE XF002(LWORK,W,ALPHA,BETA,OK,VADD,VCPY,VDIVT,
     +                 VMUL,VMULA,VMUNA,VRAN,VSCA,VSCL,VSCS,
     +                 VSET,VSUB,VXCH,VMPYT,AMPYT,VRANT,VMAXA,
     +                 ASCA,ASCL,ASCS,VMPAT,AMPAT,VSUMT,ASUMT,
     +                 AADD,ADIV,AMUL,AMULA,AMUNA,ASUB,VDIST)
          REAL      W(LWORK), ALPHA(2), BETA(2)
          EXTERNAL  CVMCAT,   CAMCAT,   CVMPCT,   CAMPCT
          LOGICAL   OK, OKN, OKT
          DIMENSION HN(2,18), HNAME(2)
          CHARACTER*4    HN, HNAME
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
#include "f002dat1.inc"
          DATA      HN     /  'V   ', 'ADD ',
     +                        'V   ', 'CPY ',
     +                        'V   ', 'DIV ',
     +                        'V   ', 'MPA ',
     +                        'V   ', 'MPY ',
     +                        'V   ', 'MUL ',
     +                        'V   ', 'MULA',
     +                        'V   ', 'MUNA',
     +                        'V   ', 'RAN ',
     +                        'V   ', 'SCA ',
     +                        'V   ', 'SCL ',
     +                        'V   ', 'SCS ',
     +                        'V   ', 'SET ',
     +                        'V   ', 'SUB ',
     +                        'V   ', 'SUM ',
     +                        'V   ', 'XCH ',
     +                        'V   ', 'MPAC',
     +                        'V   ', 'MPYC'   /
#include "f002dat2.inc"
          NDX1F(J)  =  (J-1)*LENGTH + 1
          OK  =  .TRUE.
          LF   =  LWORK / 11
          LG   =  (LWORK - 5*LF) / 3
          LA   =  1
          LB   =  LA + LF
          LC   =  LB + LF
          LR   =  LC + LF
          LT   =  LR + LF
          LGA  =  LT + LF
          LGB  =  LGA + LG
          LGC  =  LGB + LG
          IRDIM  =  LG
          CALL RVSET(IRDIM,VOID(1),W(LGC),W(LGC+1))
          IF(HTYPE .EQ. 'C   ') NTEST  =  18
          IF(HTYPE .NE. 'C   ') NTEST  =  16
          CCFLAG  =  .FALSE.
          DO 300    JTEST  =  1, NTEST
             OKT  =  .TRUE.
             HNAME(1)  =  HN(1,JTEST)
             HNAME(2)  =  HN(2,JTEST)
             WRITE(IOUNIT,1001)  HTYPE, HNAME
             KNTSKP  =  LTAB
  20         DO 200  JTAB  =  1, LTAB
                N  =  NTAB(JTAB)
                IDIM  =  MAX0(N,1)
                IF(IDIM .GE. 3  .AND.  IDIM .LE. 7)  IDIM  =  IDIM + 3
                IF(LF .LT. LENGTH*IDIM**2)  GOTO 200
                KNTSKP  =  KNTSKP - 1
                L2  =  NDX1F(2)
                GOTO(01, 02, 04, 08, 09, 10, 11, 12, 13,
     +               14, 15, 16, 17, 18, 19, 21, 22, 23), JTEST
  01            CALL TVADD(N,W(LA),W(LB),W(LC),W(LR),W(LT),IRDIM,W(LGA),
     +                     W(LGB),W(LGC),ALPHA,BETA,OKN,
     +                VADD,VCPY,VDIST,VMAXA,VSET,VRAN,AADD)
                GOTO 99
  02            CALL TVCPY(N,W(LC),IRDIM,W(LGA),
     +                      W(LGC),ALPHA,BETA,OKN,
     +                VCPY,VDIST,VSET,VRAN)
                GOTO 99
  04            CALL TVADD(N,W(LA),W(LB),W(LC),W(LR),W(LT),IRDIM,W(LGA),
     +                     W(LGB),W(LGC),ALPHA,BETA,OKN,
     +                VDIVT,VCPY,VDIST,VMAXA,VSET,VRAN,ADIV)
                GOTO 99
  08            CALL TVMPA(N,W(LA),W(LB),W(LC),W(LT),ALPHA,BETA,OKN,
     +                VMPAT,AMPAT,VDIST,VMAXA,VRAN)
                GOTO 99
  09            CALL TVMPY(N,W(LA),W(LB),W(LC),W(LT),ALPHA,BETA,OKN,
     +                VMPYT,AMPYT,VDIST,VMAXA,VRAN)
                GOTO 99
  10            CALL TVADD(N,W(LA),W(LB),W(LC),W(LR),W(LT),IRDIM,W(LGA),
     +                     W(LGB),W(LGC),ALPHA,BETA,OKN,
     +                VMUL,VCPY,VDIST,VMAXA,VSET,VRAN,AMUL)
                GOTO 99
  11            CALL TVADD(N,W(LA),W(LB),W(LC),W(LR),W(LT),IRDIM,W(LGA),
     +                     W(LGB),W(LGC),ALPHA,BETA,OKN,
     +                VMULA,VCPY,VDIST,VMAXA,VSET,VRAN,AMULA)
                GOTO 99
  12            CALL TVADD(N,W(LA),W(LB),W(LC),W(LR),W(LT),IRDIM,W(LGA),
     +                     W(LGB),W(LGC),ALPHA,BETA,OKN,
     +                VMUNA,VCPY,VDIST,VMAXA,VSET,VRAN,AMUNA)
                GOTO 99
  13            CALL TVRAN(N,W(LC),IRDIM,W(LGC),ALPHA,BETA,OKN,
     +                  VRAN,VRANT,VCPY,VDIST,VSET)
                GOTO 99
  14            CALL TVSCA(N,W(LA),W(LB),W(LC),W(LT),IRDIM,W(LGA),
     +                     W(LGB),W(LGC),ALPHA,BETA,OKN,
     +                VSCA,VCPY,VDIST,VMAXA,VSET,VRAN,ASCA)
                GOTO 99
  15            CALL TVSCL(N,W(LA),W(LC),W(LT),IRDIM,W(LGA),
     +                     W(LGC),ALPHA,BETA,OKN,
     +                VSCL,VCPY,VDIST,VMAXA,VSET,VRAN,ASCL)
                GOTO 99
  16            CALL TVSCA(N,W(LA),W(LB),W(LC),W(LT),IRDIM,W(LGA),
     +                     W(LGB),W(LGC),ALPHA,BETA,OKN,
     +                VSCS,VCPY,VDIST,VMAXA,VSET,VRAN,ASCS)
                GOTO 99
  17            CALL TVSET(N,W(LA),W(LC),IRDIM,W(LGC),ALPHA,BETA,OKN,
     +                     VSET,VDIST,VRAN)
                GOTO 99
  18            CALL TVADD(N,W(LA),W(LB),W(LC),W(LR),W(LT),IRDIM,W(LGA),
     +                     W(LGB),W(LGC),ALPHA,BETA,OKN,
     +                VSUB,VCPY,VDIST,VMAXA,VSET,VRAN,ASUB)
                GOTO 99
  19            CALL TVSUM(N,W(LA),W(LC),W(LT),ALPHA,BETA,OKN,
     +                     VSUMT,ASUMT,VDIST,VMAXA,VRAN)
                GOTO 99
  21            CALL TVXCH(N,W(LA),W(LB),IRDIM,W(LGA),
     +                      W(LGB),ALPHA,BETA,OKN,
     +                VXCH,VCPY,VDIST,VSET,VRAN)
                GOTO 99
  22            CCFLAG  =  .TRUE.
                CALL TVMPA(N,W(LA),W(LB),W(LC),W(LT),ALPHA,BETA,OKN,
     +                CVMCAT,CAMCAT,VDIST,VMAXA,VRAN)
                GOTO 99
  23            CALL TVMPY(N,W(LA),W(LB),W(LC),W(LT),ALPHA,BETA,OKN,
     +                CVMPCT,CAMPCT,VDIST,VMAXA,VRAN)
                GOTO 99
  99            OKT  =  OKT .AND. OKN
 200            CONTINUE
             IF(.NOT. OKT)  WRITE(IOUNIT,1010)
             IF(      OKT)  WRITE(IOUNIT,1011)
             OK  =  OK .AND. OKT
 300         CONTINUE
          CCFLAG  =  .FALSE.
          IF(KNTSKP .NE. 0)  WRITE(*,1014)  KNTSKP
          IF(.NOT. OK)  WRITE(IOUNIT,1012)  HTYPE
          IF(      OK)  WRITE(IOUNIT,1013)  HTYPE
          RETURN
1001      FORMAT(/ 9H TEST OF , 2A1,A4 )
1010      FORMAT(/ 5X, 37H ????? FEATURE TEST HAS FAILED. ????? )
1011      FORMAT( 15X, 25H FEATURE TEST SUCCESSFUL. )
1012      FORMAT(/ 5X, 21H ????? TEST FOR TYPE , A1,
     +           18H HAS FAILED. ????? )
1013      FORMAT(/25X, 15H TEST FOR TYPE , A1,12H SUCCESSFUL. )
1014      FORMAT( 20X, 11HWARNING ..., I3,
     +         52H CONFIGURATIONS HAVE BEEN SKIPPED FOR WANT OF SPACE.)
          END
          SUBROUTINE TVADD(N,A,B,C,R,T,IRDIM,GA,GB,GC,ALPHA,BETA,OK,
     +                     VFCN,VCPY,VDIST,VMAXA,VSET,VRAN,AFCN)
          REAL      A(*), B(*), C(*), R(*), T(*), GA(*), GB(*), GC(*)
          REAL      ALPHA(2),   BETA(2)
          LOGICAL   OK,   OKL
          REAL      R0,   RES
          EXTERNAL  VDIST,    VSET
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
#include "f002dat1.inc"
#include "f002dat2.inc"
          NDX1F(J)  =  (J-1)*LENGTH + 1
          IRESF(RES)  =  NINT(RES/TRELPR)
          OKFLAG  =  .TRUE.
*
*         CALL VRAN(N,ALPHA,BETA,A,A(L2))
*         CALL VRAN(N,ALPHA,BETA,B,B(L2))
*         CALL VRAN(N,ALPHA,BETA,R,R(L2))
*JMM
          IF(HTYPE.EQ.'R') THEN
            CALL VRAN(N,RALPHA,RBETA,A,A(L2))
            CALL VRAN(N,RALPHA,RBETA,B,B(L2))
            CALL VRAN(N,RALPHA,RBETA,R,R(L2))
          ELSEIF(HTYPE.EQ.'D')  THEN
            CALL VRAN(N,DALPHA,DBETA,A,A(L2))
            CALL VRAN(N,DALPHA,DBETA,B,B(L2))
            CALL VRAN(N,DALPHA,DBETA,R,R(L2))
          ELSEIF(HTYPE.EQ.'C')  THEN
            CALL VRAN(N,CALPHA,CBETA,A,A(L2))
            CALL VRAN(N,CALPHA,CBETA,B,B(L2))
            CALL VRAN(N,CALPHA,CBETA,R,R(L2))
          ENDIF
*
          CALL VCPY(N,R,R(L2),C,C(L2))
          CALL KFLUSH
          CALL VFCN(N,A,A(L2),B,B(L2),C,C(L2))
          CALL VCPY(N,R,R(L2),T,T(L2))
          CALL AFCN(N,A,B,T)
          R0  =  VDIST(N,C,C(L2),T,T(L2))
          IF(R0 .EQ. 0.)  GOTO 11
          CALL VMAXA(N,T,T(L2),IDUMMY,ABSREF)
          R0  =  R0 / ABSREF
  11      N0  =  IRESF(R0)
          M0  =  20
          OK  =  N0 .LE. M0
          IF(.NOT. OK)  WRITE(IOUNIT,1000)  N, IDIM, N0, R0
          DO 13     JCNFG  =  1, NCNFG
             CALL CNFGMX(N,1,IRDIM,LA,LDMY,LA2)
             CALL CNFGMX(N,1,IRDIM,LB,LDMY,LB2)
             CALL CNFGMX(N,1,IRDIM,LC,LDMY,LC2)
             CALL VCPY(N,A,A(L2),GA(LA),GA(LA2))
             CALL VCPY(N,B,B(L2),GB(LB),GB(LB2))
             CALL VCPY(N,R,R(L2),GC(LC),GC(LC2))
             CALL KFLUSH
             CALL VFCN(N,GA(LA),GA(LA2),GB(LB),GB(LB2),GC(LC),GC(LC2))
             CALL CHECKL(N,1,C,IRDIM,GC,LC,LDMY,LC2,OKL,VDIST,VSET)
             IF(.NOT. OKL)  WRITE(IOUNIT,1001)  N, JCNFG, LC, LC2
             OK  =  OK .AND. OKL
  13         CONTINUE
          OK  =  OK .AND. OKFLAG
          RETURN
1000      FORMAT(/ 25H ??? ARITHMETIC ERROR ???, 3I8, 1P, E12.3 )
1001      FORMAT(/ 20H ??? LOGIC ERROR ???, 4I8 )
          END
          SUBROUTINE          RVDIVT(N,X,X2,Y,Y2,Z,Z2)
          REAL                X(*), X2(*), Y(*), Y2(*), Z(*), Z2(*)
          REAL                S, ZERO
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          LOGICAL   OK
          IRANF(I,K)  =  INT( RANF()*FLOAT(K-I+1) ) + I
          DATA      ZERO      / 0. /
          CALL KFLUSH
          CALL RVDIV(N,X,X2,Y,Y2,Z,Z2,IFAIL)
          OK      =  IFAIL .EQ. 0
          OKFLAG  =  OKFLAG .AND. OK
          IF(.NOT. OK)  WRITE(IOUNIT,1000) N, IFAIL
          IF(N .LE. 0) RETURN
          KFAIL  =  IRANF(1,N)
#if (!defined(CERNLIB_NUMUC))&&(!defined(CERNLIB_NUMCR))&&(!defined(CERNLIB_NUMDE))
          JY  =  LOCF(Y2) - LOCF(Y)
#endif
#if defined(CERNLIB_NUMUC)||defined(CERNLIB_NUMCR)
          JY  =  LOC(Y2) - LOC(Y)
#endif
#if defined(CERNLIB_NUMDE)
          JY  =  (%LOC(Y2) - %LOC(Y)) / 4
#endif
          LY     =  (KFAIL - 1)*JY + 1
          S      =  Y(LY)
          Y(LY)  =  ZERO
          CALL KFLUSH
          CALL RVDIV(N,X,X2,Y,Y2,Z,Z2,IFAIL)
          OK      =  IFAIL .EQ. KFAIL
          OKFLAG  =  OKFLAG .AND. OK
          IF(.NOT. OK)  WRITE(IOUNIT,1001) N, KFAIL, IFAIL
          Y(LY)  =  S
          RETURN
1000      FORMAT(/ 31H ??? RVDIV DOES NOT CLEAR IFAIL, 2I8)
1001      FORMAT(/ 34H ??? RVDIV SETS IFAIL INCORRECTLY ,3I8)
          END
          SUBROUTINE          DVDIVT(N,X,X2,Y,Y2,Z,Z2)
          DOUBLE PRECISION    X(*), X2(*), Y(*), Y2(*), Z(*), Z2(*)
          DOUBLE PRECISION    S, ZERO
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          LOGICAL   OK
          IRANF(I,K)  =  INT( RANF()*FLOAT(K-I+1) ) + I
          DATA      ZERO      / 0.D0 /
          CALL KFLUSH
          CALL DVDIV(N,X,X2,Y,Y2,Z,Z2,IFAIL)
          OK      =  IFAIL .EQ. 0
          OKFLAG  =  OKFLAG .AND. OK
          IF(.NOT. OK)  WRITE(IOUNIT,1000) N, IFAIL
          IF(N .LE. 0) RETURN
          KFAIL  =  IRANF(1,N)
#if (!defined(CERNLIB_NUMUC))&&(!defined(CERNLIB_NUMCR))&&(!defined(CERNLIB_NUMDE))
          JY  =  (LOCF(Y2) - LOCF(Y)) / 2
#endif
#if defined(CERNLIB_NUMUC)||defined(CERNLIB_NUMCR)
          JY  =  (LOC(Y2) - LOC(Y)) / 2
#endif
#if defined(CERNLIB_NUMDE)
          JY  =  (%LOC(Y2) - %LOC(Y)) / 8
#endif
          LY     =  (KFAIL - 1)*JY + 1
          S      =  Y(LY)
          Y(LY)  =  ZERO
          CALL KFLUSH
          CALL DVDIV(N,X,X2,Y,Y2,Z,Z2,IFAIL)
          OK      =  IFAIL .EQ. KFAIL
          OKFLAG  =  OKFLAG .AND. OK
          IF(.NOT. OK)  WRITE(IOUNIT,1001) N, KFAIL, IFAIL
          Y(LY)  =  S
          RETURN
1000      FORMAT(/ 31H ??? DVDIV DOES NOT CLEAR IFAIL, 2I8)
1001      FORMAT(/ 34H ??? DVDIV SETS IFAIL INCORRECTLY ,3I8)
          END
          SUBROUTINE          CVDIVT(N,X,X2,Y,Y2,Z,Z2)
          COMPLEX             X(*), X2(*), Y(*), Y2(*), Z(*), Z2(*)
          COMPLEX             S, ZERO
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          LOGICAL   OK
          IRANF(I,K)  =  INT( RANF()*FLOAT(K-I+1) ) + I
          DATA      ZERO      / (0.,0.) /
          CALL KFLUSH
          CALL CVDIV(N,X,X2,Y,Y2,Z,Z2,IFAIL)
          OK      =  IFAIL .EQ. 0
          OKFLAG  =  OKFLAG .AND. OK
          IF(.NOT. OK)  WRITE(IOUNIT,1000) N, IFAIL
          IF(N .LE. 0) RETURN
          KFAIL  =  IRANF(1,N)
#if (!defined(CERNLIB_NUMUC))&&(!defined(CERNLIB_NUMCR))&&(!defined(CERNLIB_NUMDE))
          JY  =  (LOCF(Y2) - LOCF(Y)) / 2
#endif
#if defined(CERNLIB_NUMUC)||defined(CERNLIB_NUMCR)
          JY  =  (LOC(Y2) - LOC(Y)) / 2
#endif
#if defined(CERNLIB_NUMDE)
          JY  =  (%LOC(Y2) - %LOC(Y)) / 8
#endif
          LY     =  (KFAIL - 1)*JY + 1
          S      =  Y(LY)
          Y(LY)  =  ZERO
          CALL KFLUSH
          CALL CVDIV(N,X,X2,Y,Y2,Z,Z2,IFAIL)
          OK      =  IFAIL .EQ. KFAIL
          OKFLAG  =  OKFLAG .AND. OK
          IF(.NOT. OK)  WRITE(IOUNIT,1001) N, KFAIL, IFAIL
          Y(LY)  =  S
          RETURN
1000      FORMAT(/ 31H ??? CVDIV DOES NOT CLEAR IFAIL, 2I8)
1001      FORMAT(/ 34H ??? CVDIV SETS IFAIL INCORRECTLY ,3I8)
          END
          SUBROUTINE TVCPY(N,C,IRDIM,GA,GC,ALPHA,BETA,OK,
     +                     VCPY,VDIST,VSET,VRAN)
          REAL      C(*), GA(*), GC(*)
          REAL      ALPHA(2),   BETA(2)
          LOGICAL   OK,   OKL
          EXTERNAL  VDIST,    VSET
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
#include "f002dat1.inc"
#include "f002dat2.inc"
          OK  =  .TRUE.
*JMM
*         CALL VRAN(N,ALPHA,BETA,C,C(L2))
          IF(HTYPE.EQ.'R')  CALL VRAN(N,RALPHA,RBETA,C,C(L2))
          IF(HTYPE.EQ.'D')  CALL VRAN(N,DALPHA,DBETA,C,C(L2))
          IF(HTYPE.EQ.'C')  CALL VRAN(N,CALPHA,CBETA,C,C(L2))
*
          DO 13     JCNFG  =  1, NCNFG
             CALL CNFGMX(N,1,IRDIM,LA,LDMY,LA2)
             CALL CNFGMX(N,1,IRDIM,LC,LDMY,LC2)
             CALL VCPY(N,C,C(L2),GA(LA),GA(LA2))
             CALL KFLUSH
             CALL VCPY(N,GA(LA),GA(LA2),GC(LC),GC(LC2))
             CALL CHECKL(N,1,C,IRDIM,GC,LC,LDMY,LC2,OKL,VDIST,VSET)
             IF(.NOT. OKL) WRITE(IOUNIT,1001) N,JCNFG,LC,LC2
             OK  =  OK .AND. OKL
  13         CONTINUE
          RETURN
1001      FORMAT(/ 20H ??? LOGIC ERROR ???, 4I8 )
          END
          SUBROUTINE TVSCA(N,A,B,C,T,IRDIM,GA,GB,GC,ALPHA,BETA,OK,
     +                     VFCN,VCPY,VDIST,VMAXA,VSET,VRAN,AFCN)
          REAL      A(*), B(*), C(*), T(*), GA(*), GB(*), GC(*)
          REAL      ALPHA(2),   BETA(2)
          LOGICAL   OK,   OKL
          REAL      R0,   RES
          EXTERNAL  VDIST,    VSET
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
#include "f002dat1.inc"
#include "f002dat2.inc"
          NDX1F(J)  =  (J-1)*LENGTH + 1
          IRESF(RES)  =  NINT(RES/TRELPR)
*
*         CALL VRAN(N,ALPHA,BETA,A,A(L2))
*         CALL VRAN(N,ALPHA,BETA,B,B(L2))
*JMM
          IF(HTYPE.EQ.'R')  THEN
            CALL VRAN(N,RALPHA,RBETA,A,A(L2))
            CALL VRAN(N,RALPHA,RBETA,B,B(L2))
          ELSEIF(HTYPE.EQ.'D')  THEN
            CALL VRAN(N,DALPHA,DBETA,A,A(L2))
            CALL VRAN(N,DALPHA,DBETA,B,B(L2))
          ELSE
            CALL VRAN(N,CALPHA,CBETA,A,A(L2))
            CALL VRAN(N,CALPHA,CBETA,B,B(L2))
          ENDIF
*
          CALL KFLUSH
          CALL VFCN(N,A,A,A(L2),B,B(L2),C,C(L2))
          CALL AFCN(N,A,A,B,T)
          R0  =  VDIST(N,C,C(L2),T,T(L2))
          IF(R0 .EQ. 0.)  GOTO 11
          CALL VMAXA(N,T,T(L2),IDUMMY,ABSREF)
          R0  =  R0 / ABSREF
  11      N0  =  IRESF(R0)
          M0  =  20
          OK  =  N0 .LE. M0
          IF(.NOT. OK)  WRITE(IOUNIT,1000)  N, IDIM, N0, R0
          DO 13     JCNFG  =  1, NCNFG
             CALL CNFGMX(N,1,IRDIM,LA,LDMY,LA2)
             CALL CNFGMX(N,1,IRDIM,LB,LDMY,LB2)
             CALL CNFGMX(N,1,IRDIM,LC,LDMY,LC2)
             CALL VCPY(N,A,A(L2),GA(LA),GA(LA2))
             CALL VCPY(N,B,B(L2),GB(LB),GB(LB2))
             CALL KFLUSH
             CALL VFCN(N,GA(LA),GA(LA),GA(LA2),GB(LB),GB(LB2),
     +                                         GC(LC),GC(LC2))
             CALL CHECKL(N,1,C,IRDIM,GC,LC,LDMY,LC2,OKL,VDIST,VSET)
             IF(.NOT. OKL)  WRITE(IOUNIT,1001)  N, JCNFG, LC, LC2
             OK  =  OK .AND. OKL
  13         CONTINUE
          RETURN
1000      FORMAT(/ 25H ??? ARITHMETIC ERROR ???, 3I8, 1P, E12.3 )
1001      FORMAT(/ 20H ??? LOGIC ERROR ???, 4I8 )
          END
          SUBROUTINE TVSCL(N,A,C,T,IRDIM,GA,GC,ALPHA,BETA,OK,
     +                     VSCL,VCPY,VDIST,VMAXA,VSET,VRAN,ASCL)
          REAL      A(*), C(*), T(*), GA(*), GC(*)
          REAL      ALPHA(2),   BETA(2)
          LOGICAL   OK,   OKL
          REAL      R0,   RES
          EXTERNAL  VDIST,    VSET
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
#include "f002dat1.inc"
#include "f002dat2.inc"
          NDX1F(J)  =  (J-1)*LENGTH + 1
          IRESF(RES)  =  NINT(RES/TRELPR)
*
*         CALL VRAN(N,ALPHA,BETA,A,A(L2))
*JMM
          IF(HTYPE.EQ.'R')  CALL VRAN(N,RALPHA,RBETA,A,A(L2))
          IF(HTYPE.EQ.'D')  CALL VRAN(N,DALPHA,DBETA,A,A(L2))
          IF(HTYPE.EQ.'C')  CALL VRAN(N,CALPHA,CBETA,A,A(L2))
*
          CALL KFLUSH
          CALL VSCL(N,A,A,A(L2),C,C(L2))
          CALL ASCL(N,A,A,T)
          R0  =  VDIST(N,C,C(L2),T,T(L2))
          IF(R0 .EQ. 0.)  GOTO 11
          CALL VMAXA(N,T,T(L2),IDUMMY,ABSREF)
          R0  =  R0 / ABSREF
  11      N0  =  IRESF(R0)
          M0  =  20
          OK  =  N0 .LE. M0
          IF(.NOT. OK)  WRITE(IOUNIT,1000)  N, IDIM, N0, R0
          DO 13     JCNFG  =  1, NCNFG
             CALL CNFGMX(N,1,IRDIM,LA,LDMY,LA2)
             CALL CNFGMX(N,1,IRDIM,LC,LDMY,LC2)
             CALL VCPY(N,A,A(L2),GA(LA),GA(LA2))
             CALL KFLUSH
             CALL VSCL(N,GA(LA),GA(LA),GA(LA2),GC(LC),GC(LC2))
             CALL CHECKL(N,1,C,IRDIM,GC,LC,LDMY,LC2,OKL,VDIST,VSET)
             IF(.NOT. OKL)  WRITE(IOUNIT,1001)  N, JCNFG, LC, LC2
             OK  =  OK .AND. OKL
  13         CONTINUE
          RETURN
1000      FORMAT(/ 25H ??? ARITHMETIC ERROR ???, 3I8, 1P, E12.3 )
1001      FORMAT(/ 20H ??? LOGIC ERROR ???, 4I8 )
          END
          SUBROUTINE TVRAN(N,C,IRDIM,GC,ALPHA,BETA,OK,
     +                    VRAN,VRANT,VCPY,VDIST,VSET)
          REAL      C(*), GC(*)
          REAL      ALPHA(2), BETA(2)
          LOGICAL   OK,   OKL,   OKC
          REAL      R0,   R1,   R2,   P,    Q
          EXTERNAL  VDIST,    VSET
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
#include "f002dat1.inc"
#include "f002dat2.inc"
*
*         R0  =  VDIST(1,ALPHA,DMY,BETA,DMY)
*         CALL VSET(N,ALPHA,C,C(L2))
*         CALL KFLUSH
*         CALL VRAN(N,ALPHA,BETA,C,C(L2))
*         R1  =  VDIST(N,C,C(L2),ALPHA,ALPHA)
*         R2  =  VDIST(N,C,C(L2),BETA,BETA)
*JMM
          IF(HTYPE.EQ.'R')  THEN
            R0  =  VDIST(1,RALPHA,DMY,RBETA,DMY)
            CALL VSET(N,RALPHA,C,C(L2))
            CALL KFLUSH
            CALL VRAN(N,RALPHA,RBETA,C,C(L2))
            R1  =  VDIST(N,C,C(L2),RALPHA,RALPHA)
            R2  =  VDIST(N,C,C(L2),RBETA,RBETA)
          ELSEIF(HTYPE.EQ.'D')  THEN
            R0  =  VDIST(1,DALPHA,DMY,DBETA,DMY)
            CALL VSET(N,DALPHA,C,C(L2))
            CALL KFLUSH
            CALL VRAN(N,DALPHA,DBETA,C,C(L2))
            R1  =  VDIST(N,C,C(L2),DALPHA,DALPHA)
            R2  =  VDIST(N,C,C(L2),DBETA,DBETA)
          ELSE
            R0  =  VDIST(1,CALPHA,DMY,CBETA,DMY)
            CALL VSET(N,CALPHA,C,C(L2))
            CALL KFLUSH
            CALL VRAN(N,CALPHA,CBETA,C,C(L2))
            R1  =  VDIST(N,C,C(L2),CALPHA,CALPHA)
            R2  =  VDIST(N,C,C(L2),CBETA,CBETA)
          ENDIF
*
          OK  =  AMAX1(R1,R2) .LT. R0
          IF(.NOT. OK)  WRITE(IOUNIT,1000)  N, IDIM, R0, R1, R2
          DO 13     JCNFG  =  1, NCNFG
             CALL CNFGMX(N,1,IRDIM,LC,LDMY,LC2)
             CALL KFLUSH
*
*            CALL VRAN(N,ALPHA,BETA,GC(LC),GC(LC2))
*JMM
             IF(HTYPE.EQ.'R')  CALL VRAN(N,RALPHA,RBETA,GC(LC),GC(LC2))
             IF(HTYPE.EQ.'D')  CALL VRAN(N,DALPHA,DBETA,GC(LC),GC(LC2))
             IF(HTYPE.EQ.'C')  CALL VRAN(N,CALPHA,CBETA,GC(LC),GC(LC2))
*
             CALL VCPY(N,GC(LC),GC(LC2),C,C(L2))
             CALL CHECKL(N,1,C,IRDIM,GC,LC,LDMY,LC2,OKL,VDIST,VSET)
             IF(.NOT. OKL)  WRITE(IOUNIT,1001)  N, JCNFG, LC, LC2
             OK  =  OK .AND. OKL
  13         CONTINUE
          P  =  -1.
          Q  =  +3.
          AMEAN   =  (P+Q) / 2.
          SIGMA2  =  (Q-P)**2 / 12.
          K  =  1000
          CALL VRANT(P,Q,C(1),C(1),K)
          CALL RVVAR(K,AMEAN,C(1),C(2),CM,CM2,DUMMY)
          DM   =  ABS(CM - AMEAN)
          DM2  =  ABS(CM2-SIGMA2)
          CL   =  2.*ABS(Q - P) / SQRT(3.*FLOAT(K))
          CL2  =  8.*SIGMA2  / SQRT(5.*FLOAT(K))
          OKC  =  DM .LE. CL  .AND.  DM2 .LE. CL2
          IF(.NOT. OKC)  WRITE(IOUNIT,1002) N,K,P,Q, AMEAN,CM,DM,CL,
     +                                          SIGMA2,CM2,DM2,CL2
          OK  =  OK .AND .OKC
          RETURN
1000      FORMAT(/ 25H ??? ARITHMETIC ERROR ???,  2I8, 1P, 3E12.3)
1001      FORMAT(/ 20H ??? LOGIC ERROR ???,  4I8)
1002      FORMAT(/ 27H ??? DISTRIBUTION ERROR ???,  2I8,
     +           / 1X, 2F8.2, 5X, 4F12.3,
     +           / 22X,           4F12.3 )
          END
          SUBROUTINE          RVRANT(P,Q,T,R,K)
          REAL                T(K), R(K)
          CALL RVRAN(K,P,Q,R(1),R(2))
          RETURN
          END
          SUBROUTINE          DVRANT(P,Q,T,R,K)
          DOUBLE PRECISION    T(K), PHI, PSI
          REAL                R(K)
          PHI  =  DBLE(P)
          PSI  =  DBLE(Q)
          CALL DVRAN(K,PHI,PSI,T(1),T(2))
          DO 10     J  =  1, K
             R(J)  =  SNGL(T(J))
  10         CONTINUE
          RETURN
          END
          SUBROUTINE          CVRANT(P,Q,T,R,K)
          COMPLEX             T(K), PHI, PSI
          REAL                R(K)
          PHI  =  CMPLX(P,P)
          PSI  =  CMPLX(Q,Q)
          CALL CVRAN(K,PHI,PSI,T(1),T(2))
          DO 10     J  =  1, K
             R(2*J-1)  =  REAL(T(J))
             R(2*J)    =  AIMAG(T(J))
  10         CONTINUE
          K  =  2 * K
          RETURN
          END
          SUBROUTINE TVMPY(N,A,B,C,T,ALPHA,BETA,OK,
     +                     VMPYT,AMPYT,VDIST,VMAXA,VRAN)
          REAL      A(*), B(*), C(*), T(*)
          REAL      ALPHA(2),   BETA(2)
          LOGICAL   OK
          REAL      R0,   RES
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
#include "f002dat1.inc"
#include "f002dat2.inc"
          IRESF(RES)  =  NINT(RES/TRELPR)
          OKFLAG  =  .TRUE.
*
*         CALL VRAN(N,ALPHA,BETA,A,A(L2))
*         CALL VRAN(N,ALPHA,BETA,B,B(L2))
*JMM
          IF(HTYPE.EQ.'R')  THEN
            CALL VRAN(N,RALPHA,RBETA,A,A(L2))
            CALL VRAN(N,RALPHA,RBETA,B,B(L2))
          ELSEIF(HTYPE.EQ.'D')  THEN
            CALL VRAN(N,DALPHA,DBETA,A,A(L2))
            CALL VRAN(N,DALPHA,DBETA,B,B(L2))
          ELSE
            CALL VRAN(N,CALPHA,CBETA,A,A(L2))
            CALL VRAN(N,CALPHA,CBETA,B,B(L2))
          ENDIF
*
          CALL VMPYT(N,A,A(L2),B,B(L2),C)
          CALL AMPYT(N,A,B,T)
          R0  =  VDIST(1,C,C(L2),T,T(L2))
          IF(R0 .EQ. 0.)  GOTO 11
          CALL VMAXA(1,T,DMY,IDUMMY,ABSREF)
          R0  =  R0 / ABSREF
  11      N0  =  IRESF(R0)
          M0  =  400
          OK  =  N0 .LE. M0
          IF(.NOT. OK)  WRITE(IOUNIT,1000)  N, IDIM, M0, N0, R0, ABSREF
          OK  =  OK .AND. OKFLAG
          RETURN
1000      FORMAT(/ 25H ??? ARITHMETIC ERROR ???, 4I8, 1P, 2E12.3 )
          END
          SUBROUTINE          RVMPYT(N,X,X2,Y,Y2,Z)
          REAL                X(*), X2(*), Y(*), Y2(*), Z, RVMPY
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          CALL KFLUSH
          Z  =  RVMPY(N,X,X2,Y,Y2)
          IF(N .GT. 0  .OR.  Z .EQ. 0.)  RETURN
          OKFLAG  =  .FALSE.
          WRITE(IOUNIT,1000)  N
          RETURN
1000      FORMAT(/ 37H ??? VECTOR FUNCTION NOT ZERO FOR N =, I3)
          END
          SUBROUTINE          DVMPYT(N,X,X2,Y,Y2,Z)
          DOUBLE PRECISION    X(*), X2(*), Y(*), Y2(*), Z, DVMPY
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          CALL KFLUSH
          Z  =  DVMPY(N,X,X2,Y,Y2)
          IF(N .GT. 0  .OR.  Z .EQ. 0.D0)  RETURN
          OKFLAG  =  .FALSE.
          WRITE(IOUNIT,1000)  N
          RETURN
1000      FORMAT(/ 37H ??? VECTOR FUNCTION NOT ZERO FOR N =, I3)
          END
          SUBROUTINE          CVMPYT(N,X,X2,Y,Y2,Z)
          COMPLEX             X(*), X2(*), Y(*), Y2(*), Z, CVMPY
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          CALL KFLUSH
          Z  =  CVMPY(N,X,X2,Y,Y2)
          IF(N .GT. 0  .OR.  AMAX1(REAL(Z),AIMAG(Z)) .EQ. 0.)  RETURN
          OKFLAG  =  .FALSE.
          WRITE(IOUNIT,1000)  N
          RETURN
1000      FORMAT(/ 37H ??? VECTOR FUNCTION NOT ZERO FOR N =, I3)
          END
          SUBROUTINE          CVMPCT(N,X,X2,Y,Y2,Z)
          COMPLEX             X(*), X2(*), Y(*), Y2(*), Z, CVMPYC
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          CALL KFLUSH
          Z  =  CVMPYC(N,X,X2,Y,Y2)
          IF(N .GT. 0  .OR.  AMAX1(REAL(Z),AIMAG(Z)) .EQ. 0.)  RETURN
          OKFLAG  =  .FALSE.
          WRITE(IOUNIT,1000)  N
          RETURN
1000      FORMAT(/ 37H ??? VECTOR FUNCTION NOT ZERO FOR N =, I3)
          END
          SUBROUTINE          RAMPYT(N,X,Y,Z)
          REAL                X(*), Y(*), Z, RAMPY
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          Z  =  RAMPY(N,X,Y)
          IF(N .GT. 0  .OR.  Z .EQ. 0.)  RETURN
          OKFLAG  =  .FALSE.
          WRITE(IOUNIT,1000)  N
          RETURN
1000      FORMAT(/ 36H ??? ARRAY FUNCTION NOT ZERO FOR N =, I3)
          END
          SUBROUTINE          DAMPYT(N,X,Y,Z)
          DOUBLE PRECISION    X(*), Y(*), Z, DAMPY
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          Z  =  DAMPY(N,X,Y)
          IF(N .GT. 0  .OR.  Z .EQ. 0.D0)  RETURN
          OKFLAG  =  .FALSE.
          WRITE(IOUNIT,1000)  N
          RETURN
1000      FORMAT(/ 36H ??? ARRAY FUNCTION NOT ZERO FOR N =, I3)
          END
          SUBROUTINE          CAMPYT(N,X,Y,Z)
          COMPLEX             X(*), Y(*), Z, CAMPY
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          Z  =  CAMPY(N,X,Y)
          IF(N .GT. 0  .OR.  AMAX1(REAL(Z),AIMAG(Z)) .EQ. 0.)  RETURN
          OKFLAG  =  .FALSE.
          WRITE(IOUNIT,1000)  N
          RETURN
1000      FORMAT(/ 36H ??? ARRAY FUNCTION NOT ZERO FOR N =, I3)
          END
          SUBROUTINE          CAMPCT(N,X,Y,Z)
          COMPLEX             X(*), Y(*), Z, CAMPYC
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          Z  =  CAMPYC(N,X,Y)
          IF(N .GT. 0  .OR.  AMAX1(REAL(Z),AIMAG(Z)) .EQ. 0.)  RETURN
          OKFLAG  =  .FALSE.
          WRITE(IOUNIT,1000)  N
          RETURN
1000      FORMAT(/ 36H ??? ARRAY FUNCTION NOT ZERO FOR N =, I3)
          END
          SUBROUTINE TVMPA(N,A,B,C,T,ALPHA,BETA,OK,
     +                     VMPAT,AMPAT,VDIST,VMAXA,VRAN)
          REAL      A(*), B(*), C(*), T(*)
          REAL      ALPHA(2),   BETA(2)
          LOGICAL   OK
          REAL      R0,   RES
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
#include "f002dat1.inc"
#include "f002dat2.inc"
          IRESF(RES)  =  NINT(RES/TRELPR)
          OKFLAG  =  .TRUE.
*
*         IF(N .LE. 0)  CALL VRAN(1,ALPHA,BETA,A,DMY)
*         CALL VRAN(N,ALPHA,BETA,A,A(L2))
*         CALL VRAN(N,ALPHA,BETA,B,B(L2))
*JMM
          IF(HTYPE.EQ.'R')  THEN
            IF(N .LE. 0)  CALL VRAN(1,RALPHA,RBETA,A,DMY)
            CALL VRAN(N,RALPHA,RBETA,A,A(L2))
            CALL VRAN(N,RALPHA,RBETA,B,B(L2))
          ELSEIF(HTYPE.EQ.'D')  THEN
            IF(N .LE. 0)  CALL VRAN(1,DALPHA,DBETA,A,DMY)
            CALL VRAN(N,DALPHA,DBETA,A,A(L2))
            CALL VRAN(N,DALPHA,DBETA,B,B(L2))
          ELSE
            IF(N .LE. 0)  CALL VRAN(1,CALPHA,CBETA,A,DMY)
            CALL VRAN(N,CALPHA,CBETA,A,A(L2))
            CALL VRAN(N,CALPHA,CBETA,B,B(L2))
          ENDIF
*
          CALL VMPAT(N,A,A(L2),B,B(L2),A,C)
          CALL AMPAT(N,A,B,A,T)
          R0  =  VDIST(1,C,DMY,T,DMY)
          IF(R0 .EQ. 0.)  GOTO 11
          CALL VMAXA(1,T,DMY,IDUMMY,ABSREF)
          R0  =  R0 / ABSREF
  11      N0  =  IRESF(R0)
          M0  =  400
          OK  =  N0 .LE. M0
          IF(.NOT. OK)  WRITE(IOUNIT,1000)  N, IDIM, M0, N0, R0, ABSREF
          OK  =  OK .AND. OKFLAG
          RETURN
1000      FORMAT(/ 25H ??? ARITHMETIC ERROR ???, 4I8, 1P, 2E12.3 )
          END
          SUBROUTINE          RVMPAT(N,X,X2,Y,Y2,S,Z)
          REAL                X(*), X2(*), Y(*), Y2(*), S, Z, RVMPA
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          CALL KFLUSH
          Z  =  RVMPA(N,X,X2,Y,Y2,S)
          IF(N .GT. 0  .OR.  Z .EQ. S)  RETURN
          OKFLAG  =  .FALSE.
          WRITE(IOUNIT,1000)  N
          RETURN
1000      FORMAT(/ 34H ??? VECTOR FUNCTION FAILS FOR N =, I3)
          END
          SUBROUTINE          DVMPAT(N,X,X2,Y,Y2,S,Z)
          DOUBLE PRECISION    X(*), X2(*), Y(*), Y2(*), S, Z, DVMPA
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          CALL KFLUSH
          Z  =  DVMPA(N,X,X2,Y,Y2,S)
          IF(N .GT. 0  .OR.  Z .EQ. S)  RETURN
          OKFLAG  =  .FALSE.
          WRITE(IOUNIT,1000)  N
          RETURN
1000      FORMAT(/ 34H ??? VECTOR FUNCTION FAILS FOR N =, I3)
          END
          SUBROUTINE          CVMPAT(N,X,X2,Y,Y2,S,Z)
          COMPLEX             X(*), X2(*), Y(*), Y2(*), S, Z, CVMPA
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          CALL KFLUSH
          Z  =  CVMPA(N,X,X2,Y,Y2,S)
          IF(N .GT. 0  .OR.
     +       (REAL(Z) .EQ. REAL(S)  .AND. AIMAG(Z) .EQ. AIMAG(S)))
     +              RETURN
          OKFLAG  =  .FALSE.
          WRITE(IOUNIT,1000)  N
          RETURN
1000      FORMAT(/ 34H ??? VECTOR FUNCTION FAILS FOR N =, I3)
          END
          SUBROUTINE          CVMCAT(N,X,X2,Y,Y2,S,Z)
          COMPLEX             X(*), X2(*), Y(*), Y2(*), S, Z, CVMPAC
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          CALL KFLUSH
          Z  =  CVMPAC(N,X,X2,Y,Y2,S)
          IF(N .GT. 0  .OR.
     +       (REAL(Z) .EQ. REAL(S)  .AND. AIMAG(Z) .EQ. AIMAG(S)))
     +              RETURN
          OKFLAG  =  .FALSE.
          WRITE(IOUNIT,1000)  N
          RETURN
1000      FORMAT(/ 34H ??? VECTOR FUNCTION FAILS FOR N =, I3)
          END
          SUBROUTINE          RAMPAT(N,X,Y,S,Z)
          REAL                X(*), Y(*), S, Z, RAMPA
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          Z  =  RAMPA(N,X,Y,S)
          IF(N .GT. 0  .OR.  Z .EQ. S)  RETURN
          OKFLAG  =  .FALSE.
          WRITE(IOUNIT,1000)  N
          RETURN
1000      FORMAT(/ 33H ??? ARRAY FUNCTION FAILS FOR N =, I3)
          END
          SUBROUTINE          DAMPAT(N,X,Y,S,Z)
          DOUBLE PRECISION    X(*), Y(*), S, Z, DAMPA
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          Z  =  DAMPA(N,X,Y,S)
          IF(N .GT. 0  .OR.  Z .EQ. S)  RETURN
          OKFLAG  =  .FALSE.
          WRITE(IOUNIT,1000)  N
          RETURN
1000      FORMAT(/ 33H ??? ARRAY FUNCTION FAILS FOR N =, I3)
          END
          SUBROUTINE          CAMPAT(N,X,Y,S,Z)
          COMPLEX             X(*), Y(*), S, Z, CAMPA
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          Z  =  CAMPA(N,X,Y,S)
          IF(N .GT. 0  .OR.
     +       (REAL(Z) .EQ. REAL(S)  .AND. AIMAG(Z) .EQ. AIMAG(S)))
     +              RETURN
          OKFLAG  =  .FALSE.
          WRITE(IOUNIT,1000)  N
          RETURN
1000      FORMAT(/ 33H ??? ARRAY FUNCTION FAILS FOR N =, I3)
          END
          SUBROUTINE          CAMCAT(N,X,Y,S,Z)
          COMPLEX             X(*), Y(*), S, Z, CAMPAC
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          Z  =  CAMPAC(N,X,Y,S)
          IF(N .GT. 0  .OR.
     +       (REAL(Z) .EQ. REAL(S)  .AND. AIMAG(Z) .EQ. AIMAG(S)))
     +              RETURN
          OKFLAG  =  .FALSE.
          WRITE(IOUNIT,1000)  N
          RETURN
1000      FORMAT(/ 33H ??? ARRAY FUNCTION FAILS FOR N =, I3)
          END
          SUBROUTINE TVSET(N,A,C,IRDIM,GC,ALPHA,BETA,OK,
     +                     VSET,VDIST,VRAN)
          REAL      A(*), C(*), GC(*)
          REAL      ALPHA(2),   BETA(2)
          LOGICAL   OK,   OKL
          REAL      R0,   RES
          EXTERNAL  VDIST,    VSET
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
#include "f002dat1.inc"
#include "f002dat2.inc"
          IRESF(RES)  =  NINT(RES/TRELPR)
          OK  =  .TRUE.
          IF(N .LE. 0)  GOTO 12
*
*         CALL VRAN(1,ALPHA,BETA,A,DMY)
*JMM
          IF(HTYPE.EQ.'R')  CALL VRAN(1,RALPHA,RBETA,A,DMY)
          IF(HTYPE.EQ.'D')  CALL VRAN(1,DALPHA,DBETA,A,DMY)
          IF(HTYPE.EQ.'C')  CALL VRAN(1,CALPHA,CBETA,A,DMY)
*
          CALL KFLUSH
          CALL VSET(N,A,C,C(L2))
          R0  = VDIST(N,C,C(L2),A,A)
          N0  =  IRESF(R0)
          M0  =  0
          OK  =  N0 .LE. M0
          IF(.NOT. OK)  WRITE(IOUNIT,1000)  N, IDIM, M0, N0, R0
  12      DO 13     JCNFG  =  1, NCNFG
             CALL CNFGMX(N,1,IRDIM,LC,LDMY,LC2)
             CALL KFLUSH
             CALL VSET(N,A,GC(LC),GC(LC2))
             CALL CHECKL(N,1,C,IRDIM,GC,LC,LDMY,LC2,OKL,VDIST,VSET)
             IF(.NOT. OKL)  WRITE(IOUNIT,1001)  N, JCNFG, LC, LC2
             OK  =  OK .AND. OKL
  13         CONTINUE
          RETURN
1000      FORMAT(/ 25H ??? ARITHMETIC ERROR ???, 4I8,1P,E12.3 )
1001      FORMAT(/ 20H ??? LOGIC ERROR ???, 4I8 )
          END
          SUBROUTINE TVSUM(N,A,C,T,ALPHA,BETA,OK,
     +                     VSUMT,ASUMT,VDIST,VMAXA,VRAN)
          REAL      A(*), C(*), T(*)
          REAL      ALPHA(2),   BETA(2)
          LOGICAL   OK
          REAL      R0,   RES
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
#include "f002dat1.inc"
#include "f002dat2.inc"
          IRESF(RES)  =  NINT(RES/TRELPR)
          OKFLAG  =  .TRUE.
*
*         CALL VRAN(N,ALPHA,BETA,A,A(L2))
*JMM
          IF(HTYPE.EQ.'R')  CALL VRAN(N,RALPHA,RBETA,A,A(L2))
          IF(HTYPE.EQ.'D')  CALL VRAN(N,DALPHA,DBETA,A,A(L2))
          IF(HTYPE.EQ.'C')  CALL VRAN(N,CALPHA,CBETA,A,A(L2))
*
          CALL VSUMT(N,A,A(L2),C)
          CALL ASUMT(N,A,T)
          R0  =  VDIST(1,C,DMY,T,DMY)
          IF(R0 .EQ. 0.)  GOTO 11
          CALL VMAXA(1,T,DMY,IDUMMY,ABSREF)
          R0  =  R0 / ABSREF
  11      N0  =  IRESF(R0)
          M0  =  200
          OK  =  N0 .LE. M0
          IF(.NOT. OK)  WRITE(IOUNIT,1000)  N, IDIM, M0, N0, R0
          OK  =  OK .AND. OKFLAG
          RETURN
1000      FORMAT(/ 25H ??? ARITHMETIC ERROR ???, 4I8, 1P, E12.3 )
          END
          SUBROUTINE          RVSUMT(N,X,X2,Z)
          REAL                X(*), X2(*), Z, RVSUM
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          CALL KFLUSH
          Z  =  RVSUM(N,X,X2)
          IF(N .GT. 0  .OR.  Z .EQ. 0.)  RETURN
          OKFLAG  =  .FALSE.
          WRITE(IOUNIT,1000)  N
          RETURN
1000      FORMAT(/ 37H ??? VECTOR FUNCTION NOT ZERO FOR N =,  I3)
          END
          SUBROUTINE          DVSUMT(N,X,X2,Z)
          DOUBLE PRECISION    X(*), X2(*), Z, DVSUM
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          CALL KFLUSH
          Z  =  DVSUM(N,X,X2)
          IF(N .GT. 0  .OR.  Z .EQ. 0.D0)  RETURN
          OKFLAG  =  .FALSE.
          WRITE(IOUNIT,1000)  N
          RETURN
1000      FORMAT(/ 37H ??? VECTOR FUNCTION NOT ZERO FOR N =, I3)
          END
          SUBROUTINE          CVSUMT(N,X,X2,Z)
          COMPLEX             X(*), X2(*), Z, CVSUM
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          CALL KFLUSH
          Z  =  CVSUM(N,X,X2)
          IF(N .GT. 0  .OR.  AMAX1(REAL(Z),AIMAG(Z)) .EQ. 0.)  RETURN
          OKFLAG  =  .FALSE.
          WRITE(IOUNIT,1000)  N
          RETURN
1000      FORMAT(/ 37H ??? VECTOR FUNCTION NOT ZERO FOR N =, I3)
          END
          SUBROUTINE          RASUMT(N,X,Z)
          REAL                X(*), Z, RASUM
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          Z  =  RASUM(N,X)
          IF(N .GT. 0  .OR.  Z .EQ. 0.)  RETURN
          OKFLAG  =  .FALSE.
          WRITE(IOUNIT,1000)  N
          RETURN
1000      FORMAT(/ 36H ??? ARRAY FUNCTION NOT ZERO FOR N =, I3)
          END
          SUBROUTINE          DASUMT(N,X,Z)
          DOUBLE PRECISION    X(*), Z, DASUM
          EXTERNAL            DASUM
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          Z  =  DASUM(N,X)
          IF(N .GT. 0  .OR.  Z .EQ. 0.D0)  RETURN
          OKFLAG  =  .FALSE.
          WRITE(IOUNIT,1000)  N
          RETURN
1000      FORMAT(/ 36H ??? ARRAY FUNCTION NOT ZERO FOR N =, I3)
          END
          SUBROUTINE          CASUMT(N,X,Z)
          COMPLEX             X(*), Z, CASUM
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          Z  =  CASUM(N,X)
          IF(N .GT. 0  .OR.  AMAX1(REAL(Z),AIMAG(Z)) .EQ. 0.)  RETURN
          OKFLAG  =  .FALSE.
          WRITE(IOUNIT,1000)  N
          RETURN
1000      FORMAT(/ 36H ??? ARRAY FUNCTION NOT ZERO FOR N =, I3)
          END
          SUBROUTINE TVXCH(N,A,B,IRDIM,GA,GB,ALPHA,BETA,OK,
     +                     VXCH,VCPY,VDIST,VSET,VRAN)
          REAL      A(*), B(*), GA(*), GB(*)
          REAL      ALPHA(2),   BETA(2)
          LOGICAL   OK,   OK1,   OK2
          EXTERNAL  VDIST,    VSET
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
#include "f002dat1.inc"
#include "f002dat2.inc"
          OK  =  .TRUE.
*
*         CALL VRAN(N,ALPHA,BETA,A,A(L2))
*         CALL VRAN(N,ALPHA,BETA,B,B(L2))
*JMM
          IF(HTYPE.EQ.'R')  THEN
            CALL VRAN(N,RALPHA,RBETA,A,A(L2))
            CALL VRAN(N,RALPHA,RBETA,B,B(L2))
          ELSEIF(HTYPE.EQ.'D')  THEN
            CALL VRAN(N,DALPHA,DBETA,A,A(L2))
            CALL VRAN(N,DALPHA,DBETA,B,B(L2))
          ELSE
            CALL VRAN(N,CALPHA,CBETA,A,A(L2))
            CALL VRAN(N,CALPHA,CBETA,B,B(L2))
          ENDIF
*
          CALL RVSET(IRDIM,VOID(1),GA(1),GA(2))
          CALL RVSET(IRDIM,VOID(1),GB(1),GB(2))
          DO 13     JCNFG  =  1, NCNFG
             CALL CNFGMX(N,1,IRDIM,LA,LDMY,LA2)
             CALL CNFGMX(N,1,IRDIM,LB,LDMY,LB2)
             CALL VCPY(N,A,A(L2),GB(LB),GB(LB2))
             CALL VCPY(N,B,B(L2),GA(LA),GA(LA2))
             CALL KFLUSH
             CALL VXCH(N,GA(LA),GA(LA2),GB(LB),GB(LB2))
             CALL CHECKL(N,1,A,IRDIM,GA,LA,LDMY,LA2,OK1,VDIST,VSET)
             CALL CHECKL(N,1,B,IRDIM,GB,LB,LDMY,LB2,OK2,VDIST,VSET)
             IF(.NOT.(OK1 .AND. OK2))
     +          WRITE(IOUNIT,1001) N,JCNFG,LA,LA2,LB,LB2
             OK  =  OK .AND. OK1 .AND. OK2
  13         CONTINUE
          RETURN
1001      FORMAT(/ 20H ??? LOGIC ERROR ???, 6I8 )
          END
          SUBROUTINE          RVVAR(N,EX,X,X2,XM,XS2,XM2)
          REAL                EX, X(*), X2(*), XM, XS2, XM2
          REAL                S1, S2,   ZERO
          DATA      ZERO      /  0.  /
          XM   =  ZERO
          XS2  =  ZERO
          XM2  =  ZERO
          IF(N .LE. 0)  RETURN
#if (!defined(CERNLIB_NUMUC))&&(!defined(CERNLIB_NUMCR))&&(!defined(CERNLIB_NUMDE))
          JX  =  LOCF(X2) - LOCF(X)
#endif
#if defined(CERNLIB_NUMUC)||defined(CERNLIB_NUMCR)
          JX  =  LOC(X2) - LOC(X)
#endif
#if defined(CERNLIB_NUMDE)
          JX  =  (%LOC(X2) - %LOC(X)) / 4
#endif
          LX  =  1
          S1  =  ZERO
          S2  =  ZERO
          IF(EX .EQ. ZERO)  GOTO 11
          DO 10     J  =  1, N
             S1  =  S1 + (X(LX) - EX)
             S2  =  S2 + (X(LX) - EX)**2
             LX  =  LX + JX
  10         CONTINUE
          XM   =  S1 / FLOAT(N)  +  EX
          XS2  =  S2 / FLOAT(N)
          XM2  =  XS2 - (XM-EX)**2
          RETURN
  11      DO 12     J  =  1, N
             S1  =  S1 + X(LX)
             S2  =  S2 + X(LX)**2
             LX  =  LX + JX
  12         CONTINUE
          XM   =  S1 / FLOAT(N)
          XS2  =  S2 / FLOAT(N)
          XM2  =  XS2 - XM**2
          RETURN
          END
