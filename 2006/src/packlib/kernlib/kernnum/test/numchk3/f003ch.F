*
* $Id: f003ch.F,v 1.2 1997/02/04 17:36:57 mclareni Exp $
*
* $Log: f003ch.F,v $
* Revision 1.2  1997/02/04 17:36:57  mclareni
* Merge Winnt and 97a versions
*
* Revision 1.1.1.1.2.1  1997/01/21 11:32:21  mclareni
* All mods for Winnt 96a on winnt branch
*
* Revision 1.1.1.1  1996/02/15 17:48:44  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
          SUBROUTINE F003CH(NREP,LWORK,W,OK)
          REAL      W(LWORK)
          LOGICAL   OK, OKT
#include "kernnumt/sysdat.inc"
          OK  =  .TRUE.
          DO 100    JREP  =  1, NREP
             CALL TF003(LWORK,W,OKT)
             OK  =  OK .AND. OKT
 100         CONTINUE
          RETURN
          END
          SUBROUTINE TF003(LWORK,W,OK)
          DIMENSION           W(LWORK), HT(3)
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          LOGICAL             OK,       ROK,      DOK,      COK
          REAL                RZERO(1), RALPHA(1),RBETA(1)
          DOUBLE PRECISION    DZERO(1), DALPHA(1),DBETA(1)
          COMPLEX             CZERO(1), CALPHA(1),CBETA(1)
	  DOUBLE PRECISION    DMBIL
	  COMPLEX	      CMBIL
          EXTERNAL  RMADD,    RMBIL,    RMCPY,    RMUTL,    RRSCL
          EXTERNAL  RMMNA,    RMMNS,    RMMPA,    RMMPS,    RMMPY
          EXTERNAL  RMRAN,    RMRANT,   RMSCL,    RMSET,    RMSUB
          EXTERNAL  RUMNA,    RUMNS,    RUMPA,    RUMPS,    RUMPY
          EXTERNAL  DMADD,    DMBIL,    DMCPY,    DMUTL,    DRSCL
          EXTERNAL  DMMNA,    DMMNS,    DMMPA,    DMMPS,    DMMPY
          EXTERNAL  DMRAN,    DMRANT,   DMSCL,    DMSET,    DMSUB
          EXTERNAL  DUMNA,    DUMNS,    DUMPA,    DUMPS,    DUMPY
          EXTERNAL  CMADD,    CMBIL,    CMCPY,    CMUTL,    CRSCL
          EXTERNAL  CMMNA,    CMMNS,    CMMPA,    CMMPS,    CMMPY
          EXTERNAL  CMRAN,    CMRANT,   CMSCL,    CMSET,    CMSUB
          EXTERNAL  CUMNA,    CUMNS,    CUMPA,    CUMPS,    CUMPY
          EXTERNAL  RMBILT,   RVADD,    RVDIST,   RVSET,    RVSUB
          EXTERNAL  RVCPY,    RVSCA,    RVSCL,    RVSCS,    RVMAXA
          EXTERNAL  DMBILT,   DVADD,    DVDIST,   DVSET,    DVSUB
          EXTERNAL  DVCPY,    DVSCA,    DVSCL,    DVSCS,    DVMAXA
          EXTERNAL  CMBILT,   CVADD,    CVDIST,   CVSET,    CVSUB
          EXTERNAL  CVCPY,    CVSCA,    CVSCL,    CVSCS,    CVMAXA
          CHARACTER*4         HT
          DATA      HT        /  'R   ', 'D   ', 'C  '  /
          DATA      RZERO     /  0.     /
          DATA      DZERO     /  0.D0   /
          DATA      CZERO     / (0.,0.) /
          DATA      RALPHA,   RBETA     /  -100.,   +100.       /
          DATA      DALPHA,   DBETA     /  -100.D0, +100.D0     /
          DATA      CALPHA,   CBETA     / (-100.,-100.),(+100.,+100.)/
          WRITE(IOUNIT,1001)
          HTYPE   =  HT(1)
          LENGTH  =  1
          TRELPR  =  RELPRT(1)
          LTAB    =  LTABT(1)
          CALL XF003(LWORK,W,RZERO,RALPHA,RBETA,ROK,
     +              RMADD,RMBILT,RMCPY,RMMNA,RMMNS,RMMPA,RMMPS,RMMPY,
     +              RMRAN,RMRANT,RMSCL,RMSET,RMSUB,RMUTL,RRSCL,
     +              RUMNA,RUMNS,RUMPA,RUMPS,RUMPY,RVMAXA,
     +              RVADD,RVCPY,RVDIST,RVSCA,RVSCL,RVSCS,RVSET,RVSUB)
          WRITE(IOUNIT,1002)
          HTYPE   =  HT(2)
          LENGTH  =  2
          TRELPR  =  RELPRT(2)
          LTAB    =  LTABT(2)
          CALL XF003(LWORK,W,DZERO,DALPHA,DBETA,DOK,
     +              DMADD,DMBILT,DMCPY,DMMNA,DMMNS,DMMPA,DMMPS,DMMPY,
     +              DMRAN,DMRANT,DMSCL,DMSET,DMSUB,DMUTL,DRSCL,
     +              DUMNA,DUMNS,DUMPA,DUMPS,DUMPY,DVMAXA,
     +              DVADD,DVCPY,DVDIST,DVSCA,DVSCL,DVSCS,DVSET,DVSUB)
          WRITE(IOUNIT,1003)
          HTYPE   =  HT(3)
          LENGTH  =  2
          TRELPR  =  RELPRT(1)
          LTAB    =  LTABT(3)
          CALL XF003(LWORK,W,CZERO,CALPHA,CBETA,COK,
     +              CMADD,CMBILT,CMCPY,CMMNA,CMMNS,CMMPA,CMMPS,CMMPY,
     +              CMRAN,CMRANT,CMSCL,CMSET,CMSUB,CMUTL,CRSCL,
     +              CUMNA,CUMNS,CUMPA,CUMPS,CUMPY,CVMAXA,
     +              CVADD,CVCPY,CVDIST,CVSCA,CVSCL,CVSCS,CVSET,CVSUB)
          OK  =  ROK .AND. DOK .AND. COK
  90      IF(.NOT. OK)  WRITE(IOUNIT,1004)
          IF(      OK)  WRITE(IOUNIT,1005)
          RETURN
1001      FORMAT(17H1F003.  TYPE = R. )
1002      FORMAT(17H1F003.  TYPE = D. )
1003      FORMAT(17H1F003.  TYPE = C. )
1004      FORMAT(/ 5X, 37H ????? TEST OF F003 HAS FAILED. ????? )
1005      FORMAT(/35X, 44HACCEPTANCE TEST OF F003 HAS BEEN SUCCESSFUL. )
          END
          SUBROUTINE XF003(LWORK,W,ZERO,ALPHA,BETA,OK,
     +              MADD,MBILT,MCPY,MMNA,MMNS,MMPA,MMPS,MMPY,
     +              MRAN,MRANT,MSCL,MSET,MSUB,MUTL,RSCL,
     +              UMNA,UMNS,UMPA,UMPS,UMPY,VMAXA,
     +              VADD,VCPY,VDIST,VSCA,VSCL,VSCS,VSET,VSUB)
          REAL      W(LWORK), ZERO(2), ALPHA(2), BETA(2)
          EXTERNAL  CMMPYC,   CCMMPY
          EXTERNAL  CUMPYC,   CCUMPY
          LOGICAL   OK, OKN, OKT
          DIMENSION HN(2,15), HNAME(2)
          CHARACTER*4    HN, HNAME
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          DATA      HN     /  'M   ', 'ADD ',
     +                        'M   ', 'BIL ',
     +                        'M   ', 'CPY ',
     +                        'M   ', 'MPY ',
     +                        'M   ', 'RAN ',
     +                        'M   ', 'SCL ',
     +                        'M   ', 'SET ',
     +                        'M   ', 'SUB ',
     +                        'M   ', 'UTL ',
     +                        'R   ', 'SCL ',
     +                        'U   ', 'MPY ',
     +                        'C   ', 'MMPY',
     +                        'M   ', 'MPYC',
     +                        'C   ', 'UMPY',
     +                        'U   ', 'MPYC'  /
          NDX2F(I,J)  =  ((J-1)*IDIM + I-1)*LENGTH + 1
          OK  =  .TRUE.
          LF   =  LWORK / 15
          LG   =  (LWORK - 6*LF) / 3
          LA   =  1
          LB   =  LA + LF
          LC   =  LB + LF
          LX   =  LC + LF
          LR   =  LX + LF
          LT   =  LR + LF
          LGA  =  LT + LF
          LGB  =  LGA + LG
          LGC  =  LGB + LG
          IRDIM  =  LG
          CALL RVSET(IRDIM,VOID(1),W(LGC),W(LGC+1))
          IF(HTYPE .EQ. 'C   ')  NTEST  =  15
          IF(HTYPE .NE. 'C   ')  NTEST  =  11
          CCFLAG  =  .FALSE.
          DO 300    JTEST  =  1, NTEST
             OKT  =  .TRUE.
             IF(JTEST .EQ. 3) CALL RVSET(IRDIM,VOID(1),W(LGC),W(LGC+1))
             HNAME(1)  =  HN(1,JTEST)
             HNAME(2)  =  HN(2,JTEST)
             WRITE(IOUNIT,1001)  HTYPE, HNAME
             KNTSKP  =  LTAB
  20         DO 200  JTAB  =  1, LTAB
                M  =  MTAB(JTAB)
                N  =  NTAB(JTAB)
                IDIM  =  MAX0(M,N,1)
                IF(IDIM .GE. 3  .AND.  IDIM .LE. 7)  IDIM  =  IDIM + 3
                IF(LF .LT. LENGTH*IDIM**2)  GOTO 200
                KNTSKP  =  KNTSKP - 1
                L12  =  NDX2F(1,2)
                L21  =  NDX2F(2,1)
                L22  =  NDX2F(2,2)
                L2   =  L21
                GOTO(01, 02, 03, 04, 05, 06, 07, 08, 09, 10,
     +               11, 12, 13, 14, 15),  JTEST
  01            CALL TMADD(M,N,W(LA),W(LB),W(LC),W(LT),IRDIM,W(LGA),
     +                      W(LGB),W(LGC),ALPHA,BETA,OKN,
     +                      MCPY,MADD,MRAN,VDIST,VADD,VSET)
                GOTO 99
  02            CALL TMBIL(N,W(LA),W(LB),W(LC),W(LR),W(LT),IRDIM,
     +                     W(LGA),W(LGB),W(LGC),ALPHA,BETA,OKN,
     +                     MBILT,MCPY,MMPY,MRAN,VCPY,VDIST,VMAXA)
                GOTO 99
  03            CALL TMCPY(M,N,W(LA),W(LC),IRDIM,W(LGA),W(LGC),
     +                     ALPHA,BETA,OKN,
     +                     MCPY,MRAN,VDIST,VSET)
                GOTO 99
  04            CALL TMMPY(M,N,W(LA),W(LB),W(LC),W(LR),W(LT),IRDIM,
     +              W(LGA),W(LGB),W(LGC),ZERO,ALPHA,BETA,OKN,
     +              MMNA,MMNS,MMPA,MMPS,MMPY,MCPY,MRAN,
     +              VADD,VDIST,VMAXA,VSCA,VSCS,VSET,VSUB)
                GOTO 99
  05            CALL TMRAN(M,N,W(LC),IRDIM,W(LGC),ALPHA,BETA,OKN,
     +                     MRAN,MRANT,MCPY,MSET,VDIST,VSET)
                GOTO 99
  06            CALL TMSCL(M,N,W(LA),W(LC),W(LT),IRDIM,W(LGA),
     +                     W(LGC),ALPHA,BETA,OKN,
     +                     MCPY,MSCL,MRAN,VDIST,VMAXA,VSCL,VSET)
                GOTO 99
  07            CALL TMSET(M,N,W(LA),W(LC),IRDIM,
     +                      W(LGC),ALPHA,BETA,OKN,
     +                      MSET,MRAN,VDIST,VSET)
                GOTO 99
  08            CALL TMADD(M,N,W(LA),W(LB),W(LC),W(LT),IRDIM,W(LGA),
     +                      W(LGB),W(LGC),ALPHA,BETA,OKN,
     +                      MCPY,MSUB,MRAN,VDIST,VSUB,VSET)
                GOTO 99
  09            CALL TMUTL(N,W(LA),W(LC),IRDIM,W(LGC),ALPHA,BETA,OKN,
     +                     MUTL,MCPY,MRAN,VCPY,VDIST,VSET)
                GOTO 99
  10            CALL TRSCL(M,N,W(LA),W(LB),W(LC),W(LT),IRDIM,W(LGA),
     +                     W(LGB),W(LGC),ALPHA,BETA,OKN,
     +                     MCPY,RSCL,MRAN,VDIST,VMAXA,VSCL,VSET)
                GOTO 99
  11            CALL TUMPY(N,W(LA),W(LB),W(LC),W(LR),W(LT),IRDIM,
     +              W(LGA),W(LGB),W(LGC),ZERO,ALPHA,BETA,OKN,
     +              UMNA,UMNS,UMPA,UMPS,UMPY,MCPY,MRAN,
     +              VADD,VDIST,VMAXA,VSCA,VSCS,VSET,VSUB)
                GOTO 99
  12            CCFLAG  =  .TRUE.
                CALL TCMMPY(M,N,W(LA),W(LB),W(LC),W(LR),W(LT),IRDIM,
     +              W(LGA),W(LGB),W(LGC),ZERO,ALPHA,BETA,OKN,
     +              CCMMPY,MCPY,MRAN,VDIST,VMAXA,VSET)
                GOTO 99
  13            CALL TMMPY(M,N,W(LA),W(LB),W(LC),W(LR),W(LT),IRDIM,
     +              W(LGA),W(LGB),W(LGC),ZERO,ALPHA,BETA,OKN,
     +              DUMMY,DUMMY,DUMMY,DUMMY,CMMPYC,MCPY,MRAN,
     +              VADD,VDIST,VMAXA,VSCA,VSCS,VSET,VSUB)
                GOTO 99
  14            CALL TCUMPY(N,W(LA),W(LB),W(LC),W(LR),W(LT),IRDIM,
     +              W(LGA),W(LGB),W(LGC),ZERO,ALPHA,BETA,OKN,
     +              CCUMPY,MCPY,MRAN,VDIST,VMAXA,VSET)
                GOTO 99
  15            CALL TUMPY(N,W(LA),W(LB),W(LC),W(LR),W(LT),IRDIM,
     +              W(LGA),W(LGB),W(LGC),ZERO,ALPHA,BETA,OKN,
     +              DUMMY,DUMMY,DUMMY,DUMMY,CUMPYC,MCPY,MRAN,
     +              VADD,VDIST,VMAXA,VSCA,VSCS,VSET,VSUB)
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
          SUBROUTINE TMADD(M,N,A,B,C,T,IRDIM,GA,GB,GC,ALPHA,BETA,OK,
     +                     MCPY,MFCN,MRAN,VDIST,VFCN,VSET)
          REAL      A(*), B(*), C(*), T(*), GA(*), GB(*), GC(*)
          REAL      ALPHA(2),   BETA(2)
          LOGICAL   OK,   OKL
          REAL      E,   R0,   RES
          EXTERNAL  VDIST,    VSET
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          NDX2F(I,J)  =  ((J-1)*IDIM + I-1)*LENGTH + 1
          IRESF(RES)  =  NINT(RES/TRELPR)
          OK  =  .TRUE.
          IF(M .LE. 0  .OR.  N .LE. 0)  GOTO 12
          CALL MRAN(M,N,ALPHA,BETA,A,A(L12),A(L21))
          CALL MRAN(M,N,ALPHA,BETA,B,B(L12),B(L21))
          CALL KFLUSH
          CALL MFCN(M,N,A,A(L12),A(L21),B,B(L12),B(L21),C,C(L12),C(L21))
          R0  =  0.
          DO 10     I  =  1, M
             I1  =  NDX2F(I,1)
             I2  =  NDX2F(I,2)
             CALL VFCN(N,A(I1),A(I2),B(I1),B(I2),T,T(L2))
             E   =  VDIST(N,C(I1),C(I2),T,T(L2))
             R0  =  AMAX1(R0,E)
  10         CONTINUE
          N0  =  IRESF(R0)
          M0  =  5
          OK  =  N0 .LE. M0
          IF(.NOT. OK)  WRITE(IOUNIT,1000)  M, N, IDIM, M0, N0, R0
  12      DO 13     JCNFG  =  1, NCNFG
             CALL CNFGMX(M,N,IRDIM,LA,LA12,LA21)
             CALL CNFGMX(M,N,IRDIM,LB,LB12,LB21)
             CALL CNFGMX(M,N,IRDIM,LC,LC12,LC21)
             CALL MCPY(M,N,A,A(L12),A(L21),GA(LA),GA(LA12),GA(LA21))
             CALL MCPY(M,N,B,B(L12),B(L21),GB(LB),GB(LB12),GB(LB21))
             CALL KFLUSH
             CALL MFCN(M,N,GA(LA),GA(LA12),GA(LA21),
     +                     GB(LB),GB(LB12),GB(LB21),
     +                     GC(LC),GC(LC12),GC(LC21))
             CALL CHECKL(M,N,C,IRDIM,GC,LC,LC12,LC21,OKL,VDIST,VSET)
             IF(.NOT. OKL)  WRITE(IOUNIT,1001)  M,N,JCNFG,LC,LC12,LC21
             OK  =  OK .AND. OKL
  13         CONTINUE
          RETURN
1000      FORMAT(/ 25H ??? ARITHMETIC ERROR ???, 5I8,1P,E12.3 )
1001      FORMAT(/ 20H ??? LOGIC ERROR ???, 6I8 )
          END
          SUBROUTINE TMBIL(N,A,B,C,R,T,IRDIM,GA,GB,GC,
     +                        ALPHA,BETA,OK,
     +                        MBILT,MCPY,MMPY,MRAN,VCPY,VDIST,VMAXA)
          REAL      A(*),     B(*),     C(*),     R(*),     T(*)
          REAL      GA(*),    GB(*),    GC(*)
          REAL      ALPHA(2), BETA(2)
          LOGICAL   OK,       OKL,      OKZERO
          EXTERNAL  VDIST
          COMMON    /DATBIL/  OKZERO
          REAL      R0,       RES
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          IRESF(RES)  =  NINT(RES/TRELPR)
          OK  =  .TRUE.
          OKZERO  =  .TRUE.
          CALL MRAN(1,N,ALPHA,BETA,A,A(L2),DMY)
          CALL MRAN(1,N,ALPHA,BETA,C,C(L2),DMY)
          CALL MRAN(N,N,ALPHA,BETA,B,B(L12),B(L21))
          CALL KFLUSH
          CALL MBILT(N,A,A(L2),B,B(L12),B(L21),C,C(L2),R)
          IF(N .LE. 0)  GOTO 12
          CALL MMPY(N,N,B,B(L12),B(L21),C,C(L2),T,T(L2))
          CALL MMPY(1,N,A,A(L2),DMY,T,T(L2),T,DMY)
          R0  =  VDIST(1,R,DMY,T,DMY)
          IF(R0 .EQ. 0.)  GOTO 11
          CALL VMAXA(1,T,DUMMY,IDUMMY,ABSREF)
          R0  =  R0 / ABSREF
  11      N0  =  IRESF(R0)
          M0  =  300
          OK  =  N0 .LE. M0
          IF(.NOT. OK) WRITE(IOUNIT,1000) N, M0, N0, R0
  12      OK  =  OK .AND. OKZERO
          IF(.NOT. OK) RETURN
          DO 13     JCNFG  =  1, NCNFG
             CALL CNFGMX(N,1,IRDIM,LA,LDMY,LA2)
             CALL CNFGMX(N,1,IRDIM,LC,LDMY,LC2)
             CALL CNFGMX(N,N,IRDIM,LB,LB12,LB21)
             CALL VCPY(N,A,A(L2),GA(LA),GA(LA2))
             CALL VCPY(N,C,C(L2),GC(LC),GC(LC2))
             CALL MCPY(N,N,B,B(L12),B(L21),GB(LB),GB(LB12),GB(LB21))
             CALL KFLUSH
             CALL MBILT(N,GA(LA),GA(LA2),GB(LB),GB(LB12),GB(LB21),
     +                     GC(LC),GC(LC2),T)
             R0  =  VDIST(1,R,DMY,T,DMY)
             N0  =  IRESF(R0)
             OKL =  N0 .EQ. 0
             IF(.NOT. OKL) WRITE(IOUNIT,1001) N,JCNFG,N0,R0
             OK  =  OK .AND. OKL
  13         CONTINUE
          RETURN
1000      FORMAT(/ 25H ??? ARITHMETIC ERROR ???, 3I8,1P,E12.3)
1001      FORMAT(/ 20H ??? LOGIC ERROR ???,3I8,1P,E12.3)
          END
          SUBROUTINE          RMBILT(N,X,X2,Y,Y12,Y21,Z,Z2,R)
          REAL      X(*), X2(*), Y(*), Y12(*), Y21(*), Z(*), Z2(*)
          REAL      R, RMBIL
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          COMMON    /DATBIL/  OKZERO
          LOGICAL             OKZERO
          R  =  RMBIL(N,X,X2,Y,Y12,Y21,Z,Z2)
          OKZERO  =  N .GT. 0  .OR.  R .EQ. 0.
          IF(.NOT. OKZERO)  WRITE(IOUNIT,1000)  N, R
          RETURN
1000      FORMAT(/ 25H ??? RMBIL(N=0,... NOT 0 ,I5,E12.3)
          END
          SUBROUTINE          DMBILT(N,X,X2,Y,Y12,Y21,Z,Z2,R)
          DOUBLE PRECISION X(*),X2(*),Y(*),Y12(*),Y21(*),Z(*),Z2(*)
          DOUBLE PRECISION R, DMBIL
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          COMMON    /DATBIL/  OKZERO
          LOGICAL             OKZERO
          R  =  DMBIL(N,X,X2,Y,Y12,Y21,Z,Z2)
          OKZERO  =  N .GT. 0  .OR.  SNGL(R) .EQ. 0.
          IF(.NOT. OKZERO)  WRITE(IOUNIT,1000)  N
          RETURN
1000      FORMAT(/ 25H ??? DMBIL(N=0,... NOT 0 ,I5)
          END
          SUBROUTINE          CMBILT(N,X,X2,Y,Y12,Y21,Z,Z2,R)
          COMPLEX  X(*), X2(*), Y(*), Y12(*), Y21(*), Z(*), Z2(*)
          COMPLEX  R, CMBIL
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          COMMON    /DATBIL/  OKZERO
          LOGICAL             OKZERO
          R  =  CMBIL(N,X,X2,Y,Y12,Y21,Z,Z2)
          OKZERO  =  N .GT. 0  .OR.  CABS(R) .EQ. 0.
          IF(.NOT. OKZERO)  WRITE(IOUNIT,1000)  N, R
          RETURN
1000      FORMAT(/ 25H ??? CMBIL(N=0,... NOT 0 ,I5,2E12.3)
          END
          SUBROUTINE TMCPY(M,N,A,C,IRDIM,GA,GC,ALPHA,BETA,OK,
     +                     MFCN,MRAN,VDIST,VSET)
          REAL                A(*),     C(*),     GA(*),    GC(*)
          REAL                ALPHA(2), BETA(2)
          LOGICAL             OK,       OKL,      OKM
          EXTERNAL            VDIST
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          NDX2F(I,J)  =  ((J-1)*IDIM + I-1)*LENGTH + 1
          OK  =  .TRUE.
          CALL MRAN(M,N,ALPHA,BETA,A,A(L12),A(L21))
          CALL MFCN(M,N,A,A(L12),A(L21),C,C(L12),C(L21))
          IF(M .LE. 0)  GOTO 12
          DO 10     I  =  1, M
             I1  =  NDX2F(I,1)
             I2  =  NDX2F(I,2)
             E   =  VDIST(N,A(I1),A(I2),C(I1),C(I2))
             OKM =  E .EQ. 0
             OK  =  OK .AND. OKM
             IF(.NOT. OKM) WRITE(IOUNIT,1000)  M,N,E
  10         CONTINUE
          IF(.NOT. OK)  RETURN
  12      DO 13     JCNFG  =  1, NCNFG
             CALL CNFGMX(M,N,IRDIM,LA,LA12,LA21)
             CALL CNFGMX(M,N,IRDIM,LC,LC12,LC21)
             CALL MFCN(M,N,A,A(L12),A(L21),GA(LA),GA(LA12),GA(LA21))
             CALL KFLUSH
             CALL MFCN(M,N,GA(LA),GA(LA12),GA(LA21),
     +                 GC(LC),GC(LC12),GC(LC21))
             CALL CHECKL(M,N,C,IRDIM,GC,LC,LC12,LC21,OKL,VDIST,VSET)
             IF(.NOT. OKL) WRITE(IOUNIT,1001)  M,N
             OK  =  OK .AND. OKL
  13         CONTINUE
          RETURN
1000      FORMAT(/ 31H ??? ARITHMETIC ERROR ???   M =,I3,2X,3HN =,
     +            I3,1P,E12.3)
1001      FORMAT(/ 26H ??? LOGIC ERROR ???   M =,I3,2X,3HN =,I3)
          END
          SUBROUTINE TMMPY(M,N,A,B,C,R,T,IRDIM,GA,GB,GC,ZERO,ALPHA,BETA,
     +                     OK,MMNA,MMNS,MMPA,MMPS,MMPY,MCPY,MRAN,
     +                     VADD,VDIST,VMAXA,VSCA,VSCS,VSET,VSUB)
          REAL      A(*), B(*), C(*), R(*), T(*), GA(*), GB(*), GC(*)
          REAL      ZERO(2),    ALPHA(2),   BETA(2),     R0,    RES
          COMPLEX   BCONJG
          LOGICAL   OK,       OKA,      OKL
          EXTERNAL  VDIST,    VSET
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          NDX1F(J)    =  (J-1)*LENGTH + 1
          NDX2F(I,J)  =  ((J-1)*IDIM + I-1)*LENGTH + 1
          IRESF(RES)  =  NINT(RES/TRELPR)
          OK  =  .TRUE.
          IF(CCFLAG)  THEN
             MFUNCT  =  5
          ELSE
             MFUNCT  =  1
          ENDIF
          DO 40     JFUNCT  =  MFUNCT, 5
             IF(M .LE. 0  .OR.  N .LE. 0)  GOTO 30
             CALL MRAN(M,N,ALPHA,BETA,A,A(L12),A(L21))
             CALL MRAN(N,1,ALPHA,BETA,B,DUMMY,B(L2))
             CALL MRAN(M,1,ALPHA,BETA,C,DUMMY,C(L2))
             CALL MCPY(M,1,C,DUMMY,C(L2),R,DUMMY,R(L2))
             CALL VSET(M,ZERO,T(1),T(L2))
             DO 10  J  =  1, N
                LJ   =  NDX1F(J)
                L1J  =  NDX2F(1,J)
                L2J  =  NDX2F(2,J)
                IF(CCFLAG) THEN
                   BCONJG  =  CMPLX(B(LJ),-B(LJ+1))
                   CALL VSCA(M,BCONJG,A(L1J),A(L2J),T,T(L2),T,T(L2))
                ELSE
                   CALL VSCA(M,B(LJ),A(L1J),A(L2J),T,T(L2),T,T(L2))
                ENDIF
  10            CONTINUE
             GOTO(11,12,13,14,15), JFUNCT
  11         CALL VSUB(M,C(1),C(L2),T(1),T(L2),T(1),T(L2))
             CALL KFLUSH
             CALL MMNA(M,N,A,A(L12),A(L21),B,B(L2),C,C(L2))
             GOTO 20
  12         CALL VADD(M,T(1),T(L2),C(1),C(L2),T(1),T(L2))
             CALL VSCS(M,ZERO,T(1),T(L2),T(1),T(L2),T(1),T(L2))
             CALL KFLUSH
             CALL MMNS(M,N,A,A(L12),A(L21),B,B(L2),C,C(L2))
             GOTO 20
  13         CALL VADD(M,T(1),T(L2),C(1),C(L2),T(1),T(L2))
             CALL KFLUSH
             CALL MMPA(M,N,A,A(L12),A(L21),B,B(L2),C,C(L2))
             GOTO 20
  14         CALL VSUB(M,T(1),T(L2),C(1),C(L2),T(1),T(L2))
             CALL KFLUSH
             CALL MMPS(M,N,A,A(L12),A(L21),B,B(L2),C,C(L2))
             GOTO 20
  15         CALL KFLUSH
             CALL MMPY(M,N,A,A(L12),A(L21),B,B(L2),C,C(L2))
  20         R0  =  VDIST(M,C,C(L2),T,T(L2))
             IF(R0 .NE. 0.)  THEN
                CALL VMAXA(M,T,T(L2),IDUMMY,ABSREF)
                R0  =  R0 / ABSREF
             ENDIF
             N0  =  IRESF(R0)
             M0  =  40
             OKA =  N0 .LE. M0
             IF(.NOT. OKA) WRITE(IOUNIT,1000) JFUNCT,M,N,IDIM,M0,N0,R0
             OK  =  OK .AND. OKA
             IF(.NOT. OK)  RETURN
  30         DO 39  JCNFG  =  1, NCNFG
                CALL CNFGMX(M,N,IRDIM,LA,LA12,LA21)
                CALL CNFGMX(N,1,IRDIM,LB,LB12,LB21)
                CALL CNFGMX(M,1,IRDIM,LC,LC12,LC21)
                CALL MCPY(M,N,A,A(L12),A(L21),GA(LA),GA(LA12),GA(LA21))
                CALL MCPY(N,1,B,DMY,B(L2),GB(LB),DMY,GB(LB21))
                IF(N .GT. 0)
     +             CALL MCPY(M,1,R,DMY,R(L2),GC(LC),DMY,GC(LC21))
                GOTO(31,32,33,34,35),  JFUNCT
  31            CALL KFLUSH
                CALL MMNA(M,N,GA(LA),GA(LA12),GA(LA21),GB(LB),GB(LB21),
     +                                                 GC(LC),GC(LC21))
                GOTO 38
  32            CALL KFLUSH
                CALL MMNS(M,N,GA(LA),GA(LA12),GA(LA21),GB(LB),GB(LB21),
     +                                                 GC(LC),GC(LC21))
                GOTO 38
  33            CALL KFLUSH
                CALL MMPA(M,N,GA(LA),GA(LA12),GA(LA21),GB(LB),GB(LB21),
     +                                                 GC(LC),GC(LC21))
                GOTO 38
  34            CALL KFLUSH
                CALL MMPS(M,N,GA(LA),GA(LA12),GA(LA21),GB(LB),GB(LB21),
     +                                                 GC(LC),GC(LC21))
                GOTO 38
  35            CALL KFLUSH
                CALL MMPY(M,N,GA(LA),GA(LA12),GA(LA21),GB(LB),GB(LB21),
     +                                                 GC(LC),GC(LC21))
                GOTO 38
  38            IF(N .GT. 0)
     +           CALL CHECKL(M,1,C,IRDIM,GC,LC,LC12,LC21,OKL,VDIST,VSET)
                IF(N .LE. 0)
     +           CALL CHECKL(0,0,C,IRDIM,GC,LC,LC12,LC21,OKL,VDIST,VSET)
                IF(.NOT.OKL) WRITE(IOUNIT,1001) JFUNCT,M,N,JCNFG,LC,
     +                                            LC12,LC21
                OK  =  OK .AND. OKL
  39            CONTINUE
  40         CONTINUE
          RETURN
1000      FORMAT(/ 26H ??? ARITHMETIC ERROR ??? ,6I8,1P,E12.3)
1001      FORMAT(/ 21H ??? LOGIC ERROR ??? ,7I8)
          END
          SUBROUTINE TCMMPY(M,N,A,B,C,R,T,IRDIM,GA,GB,GC,ZERO,
     +                ALPHA,BETA,OK,MMPY,MCPY,MRAN,VDIST,VMAXA,VSET)
          REAL      A(*), B(*), C(*), R(*), T(*), GA(*), GB(*), GC(*)
          REAL      ZERO(2),    ALPHA(2),   BETA(2),     R0,    RES
          LOGICAL   OK,       OKA,      OKL
          EXTERNAL  VDIST,    VSET
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          NDX1F(J)    =  (J-1)*LENGTH + 1
          NDX2F(I,J)  =  ((J-1)*IDIM + I-1)*LENGTH + 1
          IRESF(RES)  =  NINT(RES/TRELPR)
          OK  =  .TRUE.
          IF(M .LE. 0  .OR.  N .LE. 0)  GOTO 30
          CALL MRAN(M,N,ALPHA,BETA,A,A(L12),A(L21))
          CALL MRAN(N,1,ALPHA,BETA,B,DUMMY,B(L2))
          CALL MRAN(M,1,ALPHA,BETA,C,DUMMY,C(L2))
          CALL MCPY(M,1,C,DUMMY,C(L2),R,DUMMY,R(L2))
          DO 10     I  =  1, M
             LI   =  NDX1F(I)
             LI1  =  NDX2F(I,1)
             LI2  =  NDX2F(I,2)
             CALL CCVDOT(N,A(LI1),A(LI2),B,B(L2),T(LI))
  10         CONTINUE
          CALL KFLUSH
          CALL MMPY(M,N,A,A(L12),A(L21),B,B(L2),C,C(L2))
          R0  =  VDIST(M,C,C(L2),T,T(L2))
          IF(R0 .NE. 0.)  THEN
             CALL VMAXA(M,T,T(L2),IDUMMY,ABSREF)
             R0  =  R0 / ABSREF
          ENDIF
          N0  =  IRESF(R0)
          M0  =  40
          OKA =  N0 .LE. M0
          IF(.NOT. OKA) WRITE(IOUNIT,1000) M,N,IDIM,M0,N0,R0
          OK  =  OK .AND. OKA
          IF(.NOT. OK)  RETURN
  30      DO 39  JCNFG  =  1, NCNFG
             CALL CNFGMX(M,N,IRDIM,LA,LA12,LA21)
             CALL CNFGMX(N,1,IRDIM,LB,LB12,LB21)
             CALL CNFGMX(M,1,IRDIM,LC,LC12,LC21)
             CALL MCPY(M,N,A,A(L12),A(L21),GA(LA),GA(LA12),GA(LA21))
             CALL MCPY(N,1,B,DMY,B(L2),GB(LB),DMY,GB(LB21))
             IF(N .GT. 0)
     +          CALL MCPY(M,1,R,DMY,R(L2),GC(LC),DMY,GC(LC21))
             CALL KFLUSH
             CALL MMPY(M,N,GA(LA),GA(LA12),GA(LA21),GB(LB),GB(LB21),
     +                                              GC(LC),GC(LC21))
             IF(N .GT. 0)
     +        CALL CHECKL(M,1,C,IRDIM,GC,LC,LC12,LC21,OKL,VDIST,VSET)
             IF(N .LE. 0)
     +        CALL CHECKL(0,0,C,IRDIM,GC,LC,LC12,LC21,OKL,VDIST,VSET)
             IF(.NOT.OKL) WRITE(IOUNIT,1001) M,N,JCNFG,LC,LC12,LC21
             OK  =  OK .AND. OKL
  39         CONTINUE
          RETURN
1000      FORMAT(/ 26H ??? ARITHMETIC ERROR ??? ,5I8,1P,E12.3)
1001      FORMAT(/ 21H ??? LOGIC ERROR ??? ,6I8)
          END
          SUBROUTINE CCVDOT(N,X,X2,Y,Y2,DOT)
          COMPLEX   X(*), X2(*), Y(*), Y2(*), DOT, CVMPYC
          DOT  =  CVMPYC(N,Y,Y2,X,X2)
          RETURN
          END
          SUBROUTINE TMRAN(M,N,C,IRDIM,GC,ALPHA,BETA,OK,
     +                    MRAN,MRANT,MCPY,MSET,VDIST,VSET)
          REAL      C(*), GC(*), ALPHA(2), BETA(2)
          LOGICAL   OK,   OKL,   OKC
          REAL      E1,   E2,   R0,   R1,   R2,   P,    Q
          EXTERNAL  VDIST,    VSET
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          NDX2F(I,J)  =  ((J-1)*IDIM + I-1)*LENGTH + 1
          OK  =  .TRUE.
          R0  =  VDIST(1,ALPHA,DMY,BETA,DMY)
          R1  =  0.
          R2  =  0.
          CALL MSET(M,N,ALPHA,C,C(L12),C(L21))
          CALL KFLUSH
          CALL MRAN(M,N,ALPHA,BETA,C,C(L12),C(L21))
          IF(M .LE. 0)  GOTO 12
          DO 10     I  =  1, M
             I1  =  NDX2F(I,1)
             I2  =  NDX2F(I,2)
             E1  =  VDIST(N,C(I1),C(I2),ALPHA,ALPHA)
             E2  =  VDIST(N,C(I1),C(I2),BETA,BETA)
             R1  =  AMAX1(R1,E1)
             R2  =  AMAX1(R2,E2)
  10         CONTINUE
          OK  =  AMAX1(R1,R2) .LT. R0
          IF(.NOT. OK)  WRITE(IOUNIT,1000)  M, N, IDIM, R0, R1, R2
  12      DO 13     JCNFG  =  1, NCNFG
             CALL CNFGMX(M,N,IRDIM,LC,LC12,LC21)
             CALL KFLUSH
             CALL MRAN(M,N,ALPHA,BETA,GC(LC),GC(LC12),GC(LC21))
             CALL MCPY(M,N,GC(LC),GC(LC12),GC(LC21),C,C(L12),C(L21))
             CALL CHECKL(M,N,C,IRDIM,GC,LC,LC12,LC21,OKL,VDIST,VSET)
             IF(.NOT. OKL)  WRITE(IOUNIT,1001)  M,N,JCNFG,LC,LC12,LC21
             OK  =  OK .AND. OKL
  13         CONTINUE
          P  =  -1.
          Q  =  +3.
          AMEAN   =  (P+Q) / 2.
          SIGMA2  =  (Q-P)**2 / 12.
          K  =  1000
          CALL MRANT(P,Q,C(1),C(1),K)
          CALL RVVAR(K,AMEAN,C(1),C(2),CM,CM2,DUMMY)
          DM   =  ABS(CM - AMEAN)
          DM2  =  ABS(CM2-SIGMA2)
          CL   =  2.*ABS(Q - P) / SQRT(3.*FLOAT(K))
          CL2  =  8.*SIGMA2  / SQRT(5.*FLOAT(K))
          OKC  =  DM .LE. CL  .AND.  DM2 .LE. CL2
          IF(.NOT. OKC)  WRITE(IOUNIT,1002) M,N,K,P,Q, AMEAN,CM,DM,CL,
     +                                          SIGMA2,CM2,DM2,CL2
          OK  =  OK .AND .OKC
          RETURN
1000      FORMAT(/ 25H ??? ARITHMETIC ERROR ???,  3I8, 1P, 3E12.3)
1001      FORMAT(/ 20H ??? LOGIC ERROR ???,  6I8)
1002      FORMAT(/ 27H ??? DISTRIBUTION ERROR ???,  3I8,
     +           / 1X, 2F8.2, 5X, 4F12.3,
     +           / 22X,           4F12.3 )
          END
          SUBROUTINE          RMRANT(P,Q,T,R,K)
          REAL                T(K), R(K)
          CALL RMRAN(K,1,P,Q,R(1),DMY,R(2))
          RETURN
          END
          SUBROUTINE          DMRANT(P,Q,T,R,K)
          DOUBLE PRECISION    T(K), PHI, PSI
          REAL                R(K)
          PHI  =  DBLE(P)
          PSI  =  DBLE(Q)
          CALL DMRAN(K,1,PHI,PSI,T(1),DMY,T(2))
          DO 10     J  =  1, K
             R(J)  =  SNGL(T(J))
  10         CONTINUE
          RETURN
          END
          SUBROUTINE          CMRANT(P,Q,T,R,K)
          COMPLEX             T(K), PHI, PSI
          REAL                R(K)
          PHI  =  CMPLX(P,P)
          PSI  =  CMPLX(Q,Q)
          CALL CMRAN(K,1,PHI,PSI,T(1),DMY,T(2))
          DO 10     J  =  1, K
             R(2*J-1)  =  REAL(T(J))
             R(2*J)    =  AIMAG(T(J))
  10         CONTINUE
          K  =  2 * K
          RETURN
          END
          SUBROUTINE TMSCL(M,N,A,C,T,IRDIM,GA,GC,ALPHA,BETA,OK,
     +                     MCPY,MSCL,MRAN,VDIST,VMAXA,VSCL,VSET)
          REAL      A(*), C(*), T(*), GA(*), GC(*)
          REAL      ALPHA(2),   BETA(2)
          LOGICAL   OK,   OKL
          REAL      E,   R0,   RES
          EXTERNAL  VDIST,    VSET
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          NDX2F(I,J)  =  ((J-1)*IDIM + I-1)*LENGTH + 1
          IRESF(RES)  =  NINT(RES/TRELPR)
          OK  =  .TRUE.
          IF(M .LE. 0  .OR.  N .LE. 0)  GOTO 12
          CALL MRAN(M,N,ALPHA,BETA,A,A(L12),A(L21))
          CALL KFLUSH
          CALL MSCL(M,N,A,A,A(L12),A(L21),C,C(L12),C(L21))
          R0  =  0.
          DO 10     I  =  1, M
             I1  =  NDX2F(I,1)
             I2  =  NDX2F(I,2)
             CALL VSCL(N,A,A(I1),A(I2),T,T(L2))
             E   =  VDIST(N,C(I1),C(I2),T,T(L2))
             CALL VMAXA(N,T,T(L2),IDUMMY,ABSREF)
             R0  =  AMAX1(R0,E/ABSREF)
  10         CONTINUE
          N0  =  IRESF(R0)
          M0  =  20
          OK  =  N0 .LE. M0
          IF(.NOT. OK)  WRITE(IOUNIT,1000)  M, N, IDIM, M0, N0, R0
  12      DO 13     JCNFG  =  1, NCNFG
             CALL CNFGMX(M,N,IRDIM,LA,LA12,LA21)
             CALL CNFGMX(M,N,IRDIM,LC,LC12,LC21)
             CALL MCPY(M,N,A,A(L12),A(L21),GA(LA),GA(LA12),GA(LA21))
             CALL KFLUSH
             CALL MSCL(M,N,A,GA(LA),GA(LA12),GA(LA21),
     +                       GC(LC),GC(LC12),GC(LC21))
             CALL CHECKL(M,N,C,IRDIM,GC,LC,LC12,LC21,OKL,VDIST,VSET)
             IF(.NOT. OKL)  WRITE(IOUNIT,1001)  M,N,JCNFG,LC,LC12,LC21
             OK  =  OK .AND. OKL
  13         CONTINUE
          RETURN
1000      FORMAT(/ 25H ??? ARITHMETIC ERROR ???, 5I8,1P,E12.3 )
1001      FORMAT(/ 20H ??? LOGIC ERROR ???, 6I8 )
          END
          SUBROUTINE TMSET(M,N,A,C,IRDIM,GC,ALPHA,BETA,OK,
     +                     MSET,MRAN,VDIST,VSET)
          REAL      A(*), C(*), GC(*)
          REAL      ALPHA(2),   BETA(2)
          LOGICAL   OK,   OKL
          REAL      E,   R0,   RES
          EXTERNAL  VDIST,    VSET
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          NDX2F(I,J)  =  ((J-1)*IDIM + I-1)*LENGTH + 1
          IRESF(RES)  =  NINT(RES/TRELPR)
          OK  =  .TRUE.
          IF(M .LE. 0  .OR.  N .LE. 0)  GOTO 12
          CALL MRAN(1,1,ALPHA,BETA,A,DMY,DMY)
          CALL KFLUSH
          CALL MSET(M,N,A,C,C(L12),C(L21))
          R0  =  0.
          DO 10     I  =  1, M
             I1  =  NDX2F(I,1)
             I2  =  NDX2F(I,2)
             E   =  VDIST(N,C(I1),C(I2),A,A)
             R0  =  AMAX1(R0,E)
  10         CONTINUE
          N0  =  IRESF(R0)
          M0  =  0
          OK  =  N0 .LE. M0
          IF(.NOT. OK)  WRITE(IOUNIT,1000)  M, N, IDIM, M0, N0, R0
  12      DO 13     JCNFG  =  1, NCNFG
             CALL CNFGMX(M,N,IRDIM,LC,LC12,LC21)
             CALL KFLUSH
             CALL MSET(M,N,A,GC(LC),GC(LC12),GC(LC21))
             CALL CHECKL(M,N,C,IRDIM,GC,LC,LC12,LC21,OKL,VDIST,VSET)
             IF(.NOT. OKL)  WRITE(IOUNIT,1001)  M,N,JCNFG,LC,LC12,LC21
             OK  =  OK .AND. OKL
  13         CONTINUE
          RETURN
1000      FORMAT(/ 25H ??? ARITHMETIC ERROR ???, 5I8,1P,E12.3 )
1001      FORMAT(/ 20H ??? LOGIC ERROR ???, 6I8 )
          END
          SUBROUTINE TMUTL(N,A,C,IRDIM,GC,ALPHA,BETA,OK,
     +                     MUTL,MCPY,MRAN,VCPY,VDIST,VSET)
          REAL                A(*),     C(*),     GC(*)
          REAL                ALPHA(2), BETA(2)
          LOGICAL             OK,       OKL
          EXTERNAL  VDIST
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          NDX2F(I,J)  =  ((J-1)*IDIM + I-1)*LENGTH + 1
          OK  =  .TRUE.
          CALL MRAN(N,N,ALPHA,BETA,A,A(L12),A(L21))
          IF(N .LE. 0)  GOTO 12
          DO 11     I  =  1, N
             II  =  NDX2F(I,I)
             II1 =  NDX2F(I,I+1)
             I1I =  NDX2F(I+1,I)
             CALL VCPY(N-I+1,A(II),A(II1),C(II),C(II1))
             CALL VCPY(N-I+1,A(II),A(II1),C(II),C(I1I))
  11         CONTINUE
  12      DO 13     JCNFG  =  1, NCNFG
             CALL CNFGMX(N,N,IRDIM,LC,LC12,LC21)
             CALL MCPY(N,N,A,A(L12),A(L21),GC(LC),GC(LC12),GC(LC21))
             CALL KFLUSH
             CALL MUTL(N,GC(LC),GC(LC12),GC(LC21))
             CALL CHECKL(N,N,C,IRDIM,GC,LC,LC12,LC21,OKL,VDIST,VSET)
             IF(.NOT. OKL) WRITE(IOUNIT,1001)  N
             OK  =  OK .AND. OKL
  13         CONTINUE
          RETURN
1001      FORMAT(/ 26H ??? LOGIC ERROR ???   N =, I3)
          END
          SUBROUTINE TRSCL(M,N,A,B,C,T,IRDIM,GA,GB,GC,ALPHA,BETA,OK,
     +                     MCPY,RSCL,MRAN,VDIST,VMAXA,VSCL,VSET)
          REAL      A(*), B(*), C(*), T(*), GA(*), GB(*), GC(*)
          REAL      ALPHA(2),   BETA(2)
          LOGICAL   OK,   OKL
          REAL      E,   R0,   RES
          EXTERNAL  VDIST,    VSET
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          NDX1F(J)    =  (J-1)*LENGTH + 1
          NDX2F(I,J)  =  ((J-1)*IDIM + I-1)*LENGTH + 1
          IRESF(RES)  =  NINT(RES/TRELPR)
          OK  =  .TRUE.
          IF(M .LE. 0  .OR.  N .LE. 0)  GOTO 12
          CALL MRAN(1,M,ALPHA,BETA,A,A(L2),DMY)
          CALL MRAN(M,N,ALPHA,BETA,B,B(L12),B(L21))
          CALL KFLUSH
          CALL RSCL(M,N,A,A(L2),B,B(L12),B(L21),C,C(L12),C(L21))
          R0  =  0.
          DO 10     I  =  1, M
             ID  =  NDX1F(I)
             I1  =  NDX2F(I,1)
             I2  =  NDX2F(I,2)
             CALL VSCL(N,A(ID),B(I1),B(I2),T,T(L2))
             E   =  VDIST(N,C(I1),C(I2),T,T(L2))
             CALL VMAXA(N,T,T(L2),IDUMMY,ABSREF)
             R0  =  AMAX1(R0,E/ABSREF)
  10         CONTINUE
          N0  =  IRESF(R0)
          M0  =  20
          OK  =  N0 .LE. M0
          IF(.NOT. OK)  WRITE(IOUNIT,1000)  M, N, IDIM, M0, N0, R0
  12      DO 13     JCNFG  =  1, NCNFG
             CALL CNFGMX(M,1,IRDIM,LA,LDMY,LA2)
             CALL CNFGMX(M,N,IRDIM,LB,LB12,LB21)
             CALL CNFGMX(M,N,IRDIM,LC,LC12,LC21)
             CALL MCPY(1,M,A,A(L2),DMY,GA(LA),GA(LA2),DMY)
             CALL MCPY(M,N,B,B(L12),B(L21),GB(LB),GB(LB12),GB(LB21))
             CALL KFLUSH
             CALL RSCL(M,N,GA(LA),GA(LA2),GB(LB),GB(LB12),GB(LB21),
     +                                    GC(LC),GC(LC12),GC(LC21))
             CALL CHECKL(M,N,C,IRDIM,GC,LC,LC12,LC21,OKL,VDIST,VSET)
             IF(.NOT. OKL)  WRITE(IOUNIT,1001)  M,N,JCNFG,LC,LC12,LC21
             OK  =  OK .AND. OKL
  13         CONTINUE
          RETURN
1000      FORMAT(/ 25H ??? ARITHMETIC ERROR ???, 5I8,1P,E12.3 )
1001      FORMAT(/ 20H ??? LOGIC ERROR ???, 6I8 )
          END
          SUBROUTINE TUMPY(N,A,B,C,R,T,IRDIM,GA,GB,GC,ZERO,ALPHA,BETA,
     +                     OK,UMNA,UMNS,UMPA,UMPS,UMPY,MCPY,MRAN,
     +                     VADD,VDIST,VMAXA,VSCA,VSCS,VSET,VSUB)
          REAL      A(*), B(*), C(*), R(*), T(*), GA(*), GB(*), GC(*)
          REAL      ZERO(2),    ALPHA(2),   BETA(2)
          COMPLEX   BCONJG,   RCONJG
          LOGICAL   OK,       OKA,      OKL
          REAL      R0, RES
          EXTERNAL  VDIST,    VSET
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          NDX1F(J)    =  (J-1)*LENGTH + 1
          NDX2F(I,J)  =  ((J-1)*IDIM + I-1)*LENGTH + 1
          IRESF(RES)  =  NINT(RES/TRELPR)
          OK  =  .TRUE.
          IF(CCFLAG)  THEN
             MFUNCT  =  5
          ELSE
             MFUNCT  =  1
          ENDIF
          DO 40     JFUNCT  =  MFUNCT, 5
             IF(N .LE. 0)  GOTO 30
             CALL MRAN(N,N,ALPHA,BETA,A,A(L12),A(L22))
             CALL MRAN(N,1,ALPHA,BETA,B,DUMMY,B(L2))
             CALL MRAN(N,1,ALPHA,BETA,C,DUMMY,C(L2))
             CALL MCPY(N,1,C,DUMMY,C(L2),R,DUMMY,R(L2))
             CALL VSET(N,ZERO,T(1),T(L2))
             DO 10  J  =  1, N
                LJ   =  NDX1F(J)
                L1J  =  NDX2F(1,J)
                L2J  =  NDX2F(2,J)
                IF(CCFLAG) THEN
                   RCONJG  =  CMPLX(R(LJ),-R(LJ+1))
                   CALL VSCA(J,RCONJG,A(L1J),A(L2J),T,T(L2),T,T(L2))
                ELSE
                   CALL VSCA(J,R(LJ),A(L1J),A(L2J),T,T(L2),T,T(L2))
                ENDIF
  10            CONTINUE
             GOTO(11,12,13,14,15), JFUNCT
  11         CALL VSUB(N,C(1),C(L2),T(1),T(L2),T(1),T(L2))
             CALL KFLUSH
             CALL UMNA(N,A,A(L12),A(L22),C,C(L2),C,C(L2))
             GOTO 18
  12         CALL VADD(N,T(1),T(L2),C(1),C(L2),T(1),T(L2))
             CALL VSCS(N,ZERO,T(1),T(L2),T(1),T(L2),T(1),T(L2))
             CALL KFLUSH
             CALL UMNS(N,A,A(L12),A(L22),C,C(L2),C,C(L2))
             GOTO 18
  13         CALL VADD(N,T(1),T(L2),C(1),C(L2),T(1),T(L2))
             CALL KFLUSH
             CALL UMPA(N,A,A(L12),A(L22),C,C(L2),C,C(L2))
             GOTO 18
  14         CALL VSUB(N,T(1),T(L2),C(1),C(L2),T(1),T(L2))
             CALL KFLUSH
             CALL UMPS(N,A,A(L12),A(L22),C,C(L2),C,C(L2))
             GOTO 18
  15         CALL KFLUSH
             CALL UMPY(N,A,A(L12),A(L22),C,C(L2),C,C(L2))
  18         R0  =  VDIST(N,C,C(L2),T,T(L2))
             IF(R0 .EQ. 0.)  GOTO 19
             CALL VMAXA(N,T,T(L2),IDUMMY,ABSREF)
             R0  =  R0 / ABSREF
  19         N0  =  IRESF(R0)
             M0  =  40
             OKA =  N0 .LE. M0
             IF(.NOT. OKA) WRITE(IOUNIT,1002)  JFUNCT,N,IDIM,M0,N0,R0
             OK  =  OK .AND. OKA
             CALL MCPY(N,1,R,DUMMY,R(L2),C,DUMMY,C(L2))
             CALL VSET(N,ZERO,T(1),T(L2))
             DO 20  J  =  1, N
                LJ   =  NDX1F(J)
                L1J  =  NDX2F(1,J)
                L2J  =  NDX2F(2,J)
                IF(CCFLAG) THEN
                   BCONJG  =  CMPLX(B(LJ),-B(LJ+1))
                   CALL VSCA(J,BCONJG,A(L1J),A(L2J),T,T(L2),T,T(L2))
                ELSE
                   CALL VSCA(J,B(LJ),A(L1J),A(L2J),T,T(L2),T,T(L2))
                ENDIF
  20            CONTINUE
             GOTO(21,22,23,24,25), JFUNCT
  21         CALL VSUB(N,C(1),C(L2),T(1),T(L2),T(1),T(L2))
             CALL KFLUSH
             CALL UMNA(N,A,A(L12),A(L22),B,B(L2),C,C(L2))
             GOTO 28
  22         CALL VADD(N,T(1),T(L2),C(1),C(L2),T(1),T(L2))
             CALL VSCS(N,ZERO,T(1),T(L2),T(1),T(L2),T(1),T(L2))
             CALL KFLUSH
             CALL UMNS(N,A,A(L12),A(L22),B,B(L2),C,C(L2))
             GOTO 28
  23         CALL VADD(N,T(1),T(L2),C(1),C(L2),T(1),T(L2))
             CALL KFLUSH
             CALL UMPA(N,A,A(L12),A(L22),B,B(L2),C,C(L2))
             GOTO 28
  24         CALL VSUB(N,T(1),T(L2),C(1),C(L2),T(1),T(L2))
             CALL KFLUSH
             CALL UMPS(N,A,A(L12),A(L22),B,B(L2),C,C(L2))
             GOTO 28
  25         CALL KFLUSH
             CALL UMPY(N,A,A(L12),A(L22),B,B(L2),C,C(L2))
  28         R0  =  VDIST(N,C,C(L2),T,T(L2))
             IF(R0 .EQ. 0.)  GOTO 29
             CALL VMAXA(N,T,T(L2),IDUMMY,ABSREF)
             R0  =  R0 / ABSREF
  29         N0  =  IRESF(R0)
             M0  =  10
             OKA =  N0 .LE. M0
             IF(.NOT. OKA) WRITE(IOUNIT,1000)  JFUNCT,N,IDIM,M0,N0,R0
             OK  =  OK .AND. OKA
             IF(.NOT. OK)  RETURN
  30         DO 39  JCNFG  =  1, NCNFG
                CALL CNFGMX(N,N,IRDIM,LA,LA12,LA21)
                LA22  =  LA12 + LA21 - LA
                CALL CNFGMX(N,1,IRDIM,LB,LB12,LB21)
                CALL CNFGMX(N,1,IRDIM,LC,LC12,LC21)
                CALL MCPY(N,N,A,A(L12),A(L21),GA(LA),GA(LA12),GA(LA21))
                CALL MCPY(N,1,B,DMY,B(L2),GB(LB),DMY,GB(LB21))
                CALL MCPY(N,1,R,DMY,R(L2),GC(LC),DMY,GC(LC21))
                GOTO(31,32,33,34,35),  JFUNCT
  31            CALL KFLUSH
                CALL UMNA(N,GA(LA),GA(LA12),GA(LA22),GB(LB),GB(LB21),
     +                                                 GC(LC),GC(LC21))
                GOTO 38
  32            CALL KFLUSH
                CALL UMNS(N,GA(LA),GA(LA12),GA(LA22),GB(LB),GB(LB21),
     +                                                 GC(LC),GC(LC21))
                GOTO 38
  33            CALL KFLUSH
                CALL UMPA(N,GA(LA),GA(LA12),GA(LA22),GB(LB),GB(LB21),
     +                                                 GC(LC),GC(LC21))
                GOTO 38
  34            CALL KFLUSH
                CALL UMPS(N,GA(LA),GA(LA12),GA(LA22),GB(LB),GB(LB21),
     +                                                 GC(LC),GC(LC21))
                GOTO 38
  35            CALL KFLUSH
                CALL UMPY(N,GA(LA),GA(LA12),GA(LA22),GB(LB),GB(LB21),
     +                                                 GC(LC),GC(LC21))
                GOTO 38
  38            CALL CHECKL(N,1,C,IRDIM,GC,LC,LC12,LC21,OKL,VDIST,VSET)
                IF(.NOT. OKL) WRITE(IOUNIT,1001) JFUNCT,N,JCNFG,
     +                                            LC,LC12,LC21
                OK  =  OK .AND. OKL
  39            CONTINUE
  40         CONTINUE
          RETURN
1000      FORMAT(/ 26H ??? ARITHMETIC ERROR ??? , 5I8,1P,E12.3)
1001      FORMAT(/ 20H ??? LOGIC ERROR ???,6I8)
1002      FORMAT(/ 26H ??? ERROR FOR Z = Y  ??? , 5I8,1P,E12.3)
          END
          SUBROUTINE TCUMPY(N,A,B,C,R,T,IRDIM,GA,GB,GC,ZERO,ALPHA,BETA,
     +                     OK,UMPY,MCPY,MRAN,VDIST,VMAXA,VSET)
          REAL      A(*), B(*), C(*), R(*), T(*), GA(*), GB(*), GC(*)
          REAL      ZERO(2),    ALPHA(2),   BETA(2),     R0,    RES
          LOGICAL   OK,       OKA,      OKL
          EXTERNAL  VDIST,    VSET
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          NDX1F(J)    =  (J-1)*LENGTH + 1
          NDX2F(I,J)  =  ((J-1)*IDIM + I-1)*LENGTH + 1
          IRESF(RES)  =  NINT(RES/TRELPR)
          OK  =  .TRUE.
          IF(N .LE. 0)  GOTO 30
          CALL MRAN(N,N,ALPHA,BETA,A,A(L12),A(L22))
          CALL MRAN(N,1,ALPHA,BETA,B,DUMMY,B(L2))
          CALL MRAN(N,1,ALPHA,BETA,C,DUMMY,C(L2))
          CALL MCPY(N,1,C,DUMMY,C(L2),R,DUMMY,R(L2))
          CALL VSET(N,ZERO,T(1),T(L2))
          DO 10  I  =  1, N
             LI   =  NDX1F(I)
             LIP  =  NDX1F(I+1)
             LII  =  NDX2F(I,I)
             LIIP =  NDX2F(I,I+1)
             CALL CCVDOT(N-I+1,A(LII),A(LIIP),C(LI),C(LIP),T(LI))
  10         CONTINUE
          CALL KFLUSH
          CALL UMPY(N,A,A(L12),A(L22),C,C(L2),C,C(L2))
          R0  =  VDIST(N,C,C(L2),T,T(L2))
          IF(R0 .EQ. 0.)  GOTO 19
          CALL VMAXA(N,T,T(L2),IDUMMY,ABSREF)
          R0  =  R0 / ABSREF
  19      N0  =  IRESF(R0)
          M0  =  40
          OKA =  N0 .LE. M0
          IF(.NOT. OKA) WRITE(IOUNIT,1002)  N,IDIM,M0,N0,R0
          OK  =  OK .AND. OKA
          CALL MCPY(N,1,R,DUMMY,R(L2),C,DUMMY,C(L2))
          CALL VSET(N,ZERO,T(1),T(L2))
          DO 20  I  =  1, N
             LI   =  NDX1F(I)
             LIP  =  NDX1F(I+1)
             LII  =  NDX2F(I,I)
             LIIP =  NDX2F(I,I+1)
             CALL CCVDOT(N-I+1,A(LII),A(LIIP),B(LI),B(LIP),T(LI))
  20         CONTINUE
          CALL KFLUSH
          CALL UMPY(N,A,A(L12),A(L22),B,B(L2),C,C(L2))
          R0  =  VDIST(N,C,C(L2),T,T(L2))
          IF(R0 .EQ. 0.)  GOTO 29
          CALL VMAXA(N,T,T(L2),IDUMMY,ABSREF)
          R0  =  R0 / ABSREF
  29      N0  =  IRESF(R0)
          M0  =  10
          OKA =  N0 .LE. M0
          IF(.NOT. OKA) WRITE(IOUNIT,1000)  N,IDIM,M0,N0,R0
          OK  =  OK .AND. OKA
          IF(.NOT. OK)  RETURN
  30      DO 39  JCNFG  =  1, NCNFG
             CALL CNFGMX(N,N,IRDIM,LA,LA12,LA21)
             LA22  =  LA12 + LA21 - LA
             CALL CNFGMX(N,1,IRDIM,LB,LB12,LB21)
             CALL CNFGMX(N,1,IRDIM,LC,LC12,LC21)
             CALL MCPY(N,N,A,A(L12),A(L21),GA(LA),GA(LA12),GA(LA21))
             CALL MCPY(N,1,B,DMY,B(L2),GB(LB),DMY,GB(LB21))
             CALL MCPY(N,1,R,DMY,R(L2),GC(LC),DMY,GC(LC21))
             CALL KFLUSH
             CALL UMPY(N,GA(LA),GA(LA12),GA(LA22),GB(LB),GB(LB21),
     +                                              GC(LC),GC(LC21))
             CALL CHECKL(N,1,C,IRDIM,GC,LC,LC12,LC21,OKL,VDIST,VSET)
             IF(.NOT. OKL) WRITE(IOUNIT,1001) N,JCNFG,LC,LC12,LC21
             OK  =  OK .AND. OKL
  39         CONTINUE
          RETURN
1000      FORMAT(/ 26H ??? ARITHMETIC ERROR ??? , 4I8,1P,E12.3)
1001      FORMAT(/ 20H ??? LOGIC ERROR ???,5I8)
1002      FORMAT(/ 26H ??? ERROR FOR Z = Y  ??? , 4I8,1P,E12.3)
          END
