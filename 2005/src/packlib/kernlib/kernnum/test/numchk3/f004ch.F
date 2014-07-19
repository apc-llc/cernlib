*
* $Id: f004ch.F,v 1.1.1.1 1996/02/15 17:48:45 mclareni Exp $
*
* $Log: f004ch.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:45  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
          SUBROUTINE F004CH(NREP,LWORK,W,OK)
          REAL      W(LWORK)
          LOGICAL   OK, OKT
#include "kernnumt/sysdat.inc"
          OK  =  .TRUE.
          DO 100    JREP  =  1, NREP
             CALL TF004(LWORK,W,OKT)
             OK  =  OK .AND. OKT
 100         CONTINUE
          RETURN
          END
          SUBROUTINE TF004(LWORK,W,OK)
          DIMENSION           W(LWORK), HT(3)
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          LOGICAL             OK,       ROK,      DOK,      COK
          REAL                RALPHA(1),RBETA(1), RFMONE(1)
          DOUBLE PRECISION    DALPHA(1),DBETA(1), DFMONE(1)
          COMPLEX             CALPHA(1),CBETA(1), CFMONE(1)
          EXTERNAL            RMMLT,    DMMLT,    CMMLT
          EXTERNAL            RMMLA,    DMMLA,    CMMLA
          EXTERNAL            RMMLS,    DMMLS,    CMMLS
          EXTERNAL            RMNMA,    DMNMA,    CMNMA
          EXTERNAL            RMNMS,    DMNMS,    CMNMS
          EXTERNAL            RMADD,    DMADD,    CMADD
          EXTERNAL            RMCPY,    DMCPY,    CMCPY
          EXTERNAL            RMMPY,    DMMPY,    CMMPY
          EXTERNAL            RMRAN,    DMRAN,    CMRAN
          EXTERNAL            RMSCL,    DMSCL,    CMSCL
          EXTERNAL            RMSUB,    DMSUB,    CMSUB
          EXTERNAL            RVDIST,   DVDIST,   CVDIST
          EXTERNAL            RVMAXA,   DVMAXA,   CVMAXA
          EXTERNAL            RVSET,    DVSET,    CVSET
          CHARACTER*4         HT
          DATA      HT        /  'R   ', 'D   ', 'C  '  /
          DATA      RFMONE              /  -1.     /
          DATA      DFMONE              /  -1.D0   /
          DATA      CFMONE              / (-1.,0.) /
          DATA      RALPHA,   RBETA     /  -100.,   +100.       /
          DATA      DALPHA,   DBETA     /  -100.D0, +100.D0     /
          DATA      CALPHA,   CBETA     / (-100.,-100.),(+100.,+100.)/
          WRITE(IOUNIT,1001)
          HTYPE   =  HT(1)
          LENGTH  =  1
          TRELPR  =  RELPRT(1)
          LTAB    =  LTABT(1)
          CALL XF004(LWORK,W,RFMONE(1),RALPHA(1),RBETA(1),ROK,
     +               RMMLT,RMMLA,RMMLS,RMNMA,RMNMS,RMADD,RMCPY,
     +               RMMPY,RMRAN,RMSCL,RMSUB,RVDIST,RVMAXA,RVSET)
          WRITE(IOUNIT,1002)
          HTYPE   =  HT(2)
          LENGTH  =  2
          TRELPR  =  RELPRT(2)
          LTAB    =  LTABT(2)
          CALL XF004(LWORK,W,DFMONE(1),DALPHA(1),DBETA(1),DOK,
     +               DMMLT,DMMLA,DMMLS,DMNMA,DMNMS,DMADD,DMCPY,
     +               DMMPY,DMRAN,DMSCL,DMSUB,DVDIST,DVMAXA,DVSET)
          WRITE(IOUNIT,1003)
          HTYPE   =  HT(3)
          LENGTH  =  2
          TRELPR  =  RELPRT(1)
          LTAB    =  LTABT(3)
          CALL XF004(LWORK,W,CFMONE(1),CALPHA(1),CBETA(1),COK,
     +               CMMLT,CMMLA,CMMLS,CMNMA,CMNMS,CMADD,CMCPY,
     +               CMMPY,CMRAN,CMSCL,CMSUB,CVDIST,CVMAXA,CVSET)
          OK  =  ROK .AND. DOK .AND. COK
  90      IF(.NOT. OK)  WRITE(IOUNIT,1004)
          IF(      OK)  WRITE(IOUNIT,1005)
          RETURN
1001      FORMAT(17H1F004.  TYPE = R. )
1002      FORMAT( // 17H0F004.  TYPE = D. )
1003      FORMAT( // 17H0F004.  TYPE = C. )
1004      FORMAT(/35X, 37H ????? TEST OF F004 HAS FAILED. ????? )
1005      FORMAT(/35X, 44HACCEPTANCE TEST OF F004 HAS BEEN SUCCESSFUL.)
          END
          SUBROUTINE XF004(LWORK,W,FMONE,ALPHA,BETA,OK,
     +                     MMLT,MMLA,MMLS,MNMA,MNMS,MADD,MCPY,
     +                     MMPY,MRAN,MSCL,MSUB,VDIST,VMAXA,VSET)
          REAL      W(LWORK)
          REAL      FMONE(2), ALPHA(2), BETA(2)
          LOGICAL   OK,       OKN,      OKT
          DIMENSION HN(2,3),  HNAME(2)
          CHARACTER*4         HN, HNAME
          EXTERNAL  CMMLTC,   CMMPYC
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          DATA      HN        /  'M   ', 'MLT ',
     +                           'M   ', 'MXX ',
     +                           'M   ', 'MLTC'  /
          NDX2F(I,J)  =  ((J-1)*IDIM + I-1)*LENGTH + 1
          OK  =  .TRUE.
          LF   =  LWORK / 12
          LG   =  (LWORK - 4*LF) / 3
          LFA  =  1
          LFB  =  LFA + LF
          LFC  =  LFB + LF
          LFT  =  LFC + LF
          LGA  =  LFT + LF
          LGB  =  LGA + LG
          LGC  =  LGB + LG
          CALL RVSET(LG,VOID(1),W(LGC),W(LGC+1))
          IF(HTYPE .EQ. 'C   ')  NTEST = 3
          IF(HTYPE .NE. 'C   ')  NTEST = 2
          CCFLAG  =  .FALSE.
          DO 300    JTEST  =  1, NTEST
             OKT  =  .TRUE.
             HNAME(1)  =  HN(1,JTEST)
             HNAME(2)  =  HN(2,JTEST)
             WRITE(IOUNIT,1001)  HTYPE, HNAME
             KNTSKP  =  LTAB
  20         DO 200  JTAB  =  1, LTAB
                M  =  MTAB(JTAB)
                N  =  NTAB(JTAB)
                K  =  KTAB(JTAB)
                IDIM  =  MAX0(M,N,K,1)
                IF(IDIM .GE. 3  .AND.  IDIM .LE. 7)  IDIM  =  IDIM + 3
                IF(LF .LT. LENGTH*IDIM**2)  GOTO 200
                KNTSKP  =  KNTSKP - 1
                L12  =  NDX2F(1,2)
                L21  =  NDX2F(2,1)
                L2   =  L21
                GOTO(01, 02, 03),  JTEST
  01            CALL TMMLT(M,N,K,W(LFA),W(LFB),W(LFC),W(LFT),
     +                  LG,W(LGA),W(LGB),W(LGC),ALPHA(1),BETA(1),OKN,
     +                      MCPY,MMLT,MMPY,MRAN,VDIST,VMAXA,VSET)
                GOTO 99
  02            CALL TMXXX(M,N,K,W(LFA),W(LFB),W(LFC),W(LFT),
     +            LG,W(LGA),W(LGB),W(LGC),FMONE(1),ALPHA(1),BETA(1),OKN,
     +            MMLA,MMLS,MNMA,MNMS,MADD,MCPY,
     +            MMPY,MRAN,MSCL,MSUB,VDIST,VMAXA,VSET)
                GOTO 99
  03            CCFLAG  =  .TRUE.
                CALL TMMLT(M,N,K,W(LFA),W(LFB),W(LFC),W(LFT),
     +                  LG,W(LGA),W(LGB),W(LGC),ALPHA(1),BETA(1),OKN,
     +                      MCPY,CMMLTC,CMMPYC,MRAN,VDIST,VMAXA,VSET)
  99            OKT  =  OKT .AND. OKN
 200            CONTINUE
             CCFLAG  =  .FALSE.
             IF(.NOT. OKT)  WRITE(IOUNIT,1010)
             IF(      OKT)  WRITE(IOUNIT,1011)
             OK  =  OK .AND. OKT
 300         CONTINUE
          IF(KNTSKP .NE. 0)  WRITE(*,1014)  KNTSKP
          IF(.NOT. OK)  WRITE(IOUNIT,1012)  HTYPE
          IF(      OK)  WRITE(IOUNIT,1013)  HTYPE
          RETURN
1001      FORMAT(/// 9H TEST OF ,2A1,A4,1H. )
1010      FORMAT(/ 5X, 37H ????? FEATURE TEST HAS FAILED. ????? )
1011      FORMAT( 15X, 25H FEATURE TEST SUCCESSFUL. )
1012      FORMAT(/25X, 21H ????? TEST FOR TYPE ,A1,
     +            18H HAS FAILED. ?????  )
1013      FORMAT(/25X, 15H TEST FOR TYPE ,A1,13H SUCCESSFUL.  )
1014      FORMAT( 20X, 11HWARNING ..., I3,
     +         52H CONFIGURATIONS HAVE BEEN SKIPPED FOR WANT OF SPACE.)
          END
          SUBROUTINE TMMLT(M,N,K,A,B,C,T,LG,GA,GB,GC,
     +          ALPHA,BETA,OK,MCPY,MMLT,MMPY,MRAN,VDIST,VMAXA,VSET)
          REAL      A(*), B(*), C(*), T(*), GA(*), GB(*), GC(*)
          REAL      ALPHA(2), BETA(2)
          LOGICAL   OK, OKA, OKL, OKT
          REAL      E, R0, RES
          EXTERNAL  VDIST,    VSET
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          NDX2F(I,J)  =  ((J-1)*IDIM + I-1)*LENGTH + 1
          IRESF(RES)  =  NINT(RES/TRELPR)
          OK  =  .TRUE.
          OKA  =  .TRUE.
          IF(MIN0(M,N,K) .LE. 0)  GOTO 12
          CALL MRAN(M,N,ALPHA,BETA,A,A(L12),A(L21))
          CALL MRAN(N,K,ALPHA,BETA,B,B(L12),B(L21))
          CALL KFLUSH
          CALL MMLT(M,N,K,A,A(L12),A(L21),B,B(L12),B(L21),
     +                                    C,C(L12),C(L21),T)
          R0  =  0.
          DO 11     I  =  1, M
             I1  =  NDX2F(I,1)
             I2  =  NDX2F(I,2)
             IF(CCFLAG) THEN
                CALL CCMMPY(K,N,B,B(L21),B(L12),A(I1),A(I2),T,T(L2))
             ELSE
                CALL MMPY(K,N,B,B(L21),B(L12),A(I1),A(I2),T,T(L2))
             ENDIF
             E   =  VDIST(K,C(I1),C(I2),T,T(L2))
             IF(E .EQ. 0.)  GOTO 11
             CALL VMAXA(K,T,T(L2),IDUMMY,ABSREF)
             R0  =  AMAX1(R0,E/ABSREF)
  11         CONTINUE
          N0  =  IRESF(R0)
          M0  =  20
          OKA =  N0 .LE. M0
          IF(.NOT. OKA)
     +          WRITE(IOUNIT,1001) M,N,K,IDIM,M0,N0,R0
  12      OKL  =  .TRUE.
          DO 13     JCNFG  =  1, NCNFG
             CALL CNFGMX(M,N,LG,LA,LA12,LA21)
             CALL CNFGMX(N,K,LG,LB,LB12,LB21)
             CALL CNFGMX(M,K,LG,LC,LC12,LC21)
             CALL MCPY(M,N,A,A(L12),A(L21),GA(LA),GA(LA12),GA(LA21))
             CALL MCPY(N,K,B,B(L12),B(L21),GB(LB),GB(LB12),GB(LB21))
             CALL MMLT(M,N,K,GA(LA),GA(LA12),GA(LA21),
     +                       GB(LB),GB(LB12),GB(LB21),
     +                       GC(LC),GC(LC12),GC(LC21), T)
             IF(N .GT. 0)
     +          CALL CHECKL(M,K,C,LG,GC,LC,LC12,LC21,OKT,VDIST,VSET)
             IF(N .LE. 0)
     +          CALL CHECKL(0,0,C,LG,GC,LC,LC12,LC21,OKT,VDIST,VSET)
             IF(.NOT. OKT)
     +             WRITE(IOUNIT,1011)M,N,K,JCNFG,LC,LC12,LC21
             OKL  =  OKL .AND. OKT
  13         CONTINUE
          OK  =  OK .AND. OKA .AND. OKL
          CALL MRAN(M,N,ALPHA,BETA,A,A(L12),A(L21))
          CALL MRAN(N,K,ALPHA,BETA,B,B(L12),B(L21))
          CALL MCPY(N,K,B,B(L12),B(L21),C,C(L12),C(L21))
          CALL KFLUSH
          CALL MMLT(M,N,K,A,A(L12),A(L21),C,C(L12),C(L21),
     +                                    C,C(L12),C(L21),T)
          CALL MMLT(M,N,K,A,A(L12),A(L21),B,B(L12),B(L21),
     +                                    T,T(L12),T(L21),T)
          R0  =  0.
          IF(M .LE. 0  .OR.  N .LE. 0)  GOTO 22
          DO 21     I  =  1, M
             I1  =  NDX2F(I,1)
             I2  =  NDX2F(I,2)
             E   =  VDIST(K,C(I1),C(I2),T(I1),T(I2))
             IF(E .EQ. 0.)  GOTO 21
             CALL VMAXA(K,T(I1),T(I2),IDUMMY,ABSREF)
             R0  =  AMAX1(R0,E/ABSREF)
  21         CONTINUE
  22      N0  =  IRESF(R0)
          M0  =  20
          OKA =  N0 .LE. M0
          IF(.NOT. OKA)
     +          WRITE(IOUNIT,1002) M,N,K,IDIM,M0,N0,R0
          OKL  =  .TRUE.
          IF(N .GT. M)  GOTO 24
          DO 23     JCNFG  =  1, NCNFG
             CALL CNFGMX(M,N,LG,LA,LA12,LA21)
             CALL CNFGMX(M,K,LG,LC,LC12,LC21)
             CALL MCPY(M,N,A,A(L12),A(L21),GA(LA),GA(LA12),GA(LA21))
             CALL MCPY(N,K,B,B(L12),B(L21),GC(LC),GC(LC12),GC(LC21))
             CALL MMLT(M,N,K,GA(LA),GA(LA12),GA(LA21),
     +                       GC(LC),GC(LC12),GC(LC21),
     +                       GC(LC),GC(LC12),GC(LC21), T)
             IF(N .GT. 0)
     +          CALL CHECKL(M,K,C,LG,GC,LC,LC12,LC21,OKT,VDIST,VSET)
             IF(N .LE. 0)
     +          CALL CHECKL(0,0,C,LG,GC,LC,LC12,LC21,OKT,VDIST,VSET)
             IF(.NOT. OKT)
     +             WRITE(IOUNIT,1012)M,N,K,JCNFG,LC,LC12,LC21
             OKL  =  OKL .AND. OKT
  23         CONTINUE
  24      OK  =  OK .AND. OKA .AND. OKL
          CALL MRAN(M,N,ALPHA,BETA,A,A(L12),A(L21))
          CALL MRAN(N,K,ALPHA,BETA,B,B(L12),B(L21))
          CALL MCPY(M,N,A,A(L12),A(L21),C,C(L12),C(L21))
          CALL KFLUSH
          CALL MMLT(M,N,K,C,C(L12),C(L21),B,B(L12),B(L21),
     +                                    C,C(L12),C(L21),T)
          CALL MMLT(M,N,K,A,A(L12),A(L21),B,B(L12),B(L21),
     +                                    T,T(L12),T(L21),T)
          R0  =  0.
          IF(M .LE. 0  .OR.  N .LE. 0)  GOTO 32
          DO 31     I  =  1, M
             I1  =  NDX2F(I,1)
             I2  =  NDX2F(I,2)
             E   =  VDIST(K,C(I1),C(I2),T(I1),T(I2))
             IF(E .EQ. 0.)  GOTO 31
             CALL VMAXA(K,T(I1),T(I2),IDUMMY,ABSREF)
             R0  =  AMAX1(R0,E/ABSREF)
  31         CONTINUE
  32      N0  =  IRESF(R0)
          M0  =  20
          OKA =  N0 .LE. M0
          IF(.NOT. OKA)
     +          WRITE(IOUNIT,1003) M,N,K,IDIM,M0,N0,R0
          OKL  =  .TRUE.
          IF(N .GT. K)  GOTO 34
          DO 33     JCNFG  =  1, NCNFG
             CALL CNFGMX(N,K,LG,LB,LB12,LB21)
             CALL CNFGMX(M,K,LG,LC,LC12,LC21)
             CALL MCPY(M,N,A,A(L12),A(L21),GC(LC),GC(LC12),GC(LC21))
             CALL MCPY(N,K,B,B(L12),B(L21),GB(LB),GB(LB12),GB(LB21))
             CALL MMLT(M,N,K,GC(LC),GC(LC12),GC(LC21),
     +                       GB(LB),GB(LB12),GB(LB21),
     +                       GC(LC),GC(LC12),GC(LC21), T)
             IF(N .GT. 0)
     +          CALL CHECKL(M,K,C,LG,GC,LC,LC12,LC21,OKT,VDIST,VSET)
             IF(N .LE. 0)
     +          CALL CHECKL(0,0,C,LG,GC,LC,LC12,LC21,OKT,VDIST,VSET)
             IF(.NOT. OKT) WRITE(IOUNIT,1013) M,N,K,JCNFG,LC,LC12,LC21
             OKL  =  OKL .AND. OKT
  33         CONTINUE
  34      OK  =  OK .AND. OKA .AND. OKL
          OKA  =  .TRUE.
          IF(MIN0(M,N) .LE. 0)  GOTO 42
          CALL MRAN(M,N,ALPHA,BETA,A,A(L12),A(L21))
          CALL KFLUSH
          CALL MMLT(M,N,M,A,A(L12),A(L21),A,A(L21),A(L12),
     +                                    C,C(L12),C(L21),T)
          R0  =  0.
          DO 41     I  =  1, M
             I1  =  NDX2F(I,1)
             I2  =  NDX2F(I,2)
             J1  =  NDX2F(1,I)
             J2  =  NDX2F(2,I)
             CALL MMPY(M,N,A,A(L12),A(L21),A(I1),A(I2),T,T(L2))
             E   =  VDIST(M,C(J1),C(J2),T,T(L2))
             IF(E .EQ. 0.)  GOTO 41
             CALL VMAXA(M,T,T(L2),IDUMMY,ABSREF)
             R0  =  AMAX1(R0,E/ABSREF)
  41         CONTINUE
          N0  =  IRESF(R0)
          M0  =  20
          OKA =  N0 .LE. M0
          IF(.NOT. OKA) WRITE(IOUNIT,1004) M,N,IDIM,M0,N0,R0
  42      OKL  =  .TRUE.
          DO 43     JCNFG  =  1, NCNFG
             CALL CNFGMX(M,N,LG,LA,LA12,LA21)
             CALL CNFGMX(M,M,LG,LC,LC12,LC21)
             CALL MCPY(M,N,A,A(L12),A(L21),GA(LA),GA(LA12),GA(LA21))
             CALL MMLT(M,N,M,GA(LA),GA(LA12),GA(LA21),
     +                       GA(LA),GA(LA21),GA(LA12),
     +                       GC(LC),GC(LC12),GC(LC21),T)
             IF(N .GT. 0)
     +          CALL CHECKL(M,M,C,LG,GC,LC,LC12,LC21,OKT,VDIST,VSET)
             IF(N .LE. 0)
     +          CALL CHECKL(0,0,C,LG,GC,LC,LC12,LC21,OKT,VDIST,VSET)
             IF(.NOT. OKT) WRITE(IOUNIT,1014) M,N,JCNFG,LC,LC12,LC21
             OKL  =  OKL .AND. OKT
  43         CONTINUE
          OK  =  OK .AND. OKA .AND. OKL
          OKA  =  .TRUE.
          IF(MIN0(M,N) .LE. 0)  GOTO 52
          CALL MRAN(M,N,ALPHA,BETA,A,A(L12),A(L21))
          CALL MCPY(M,N,A,A(L12),A(L21),C,C(L12),C(L21))
          CALL KFLUSH
          CALL MMLT(M,N,M,C,C(L12),C(L21),C,C(L21),C(L12),
     +                                    C,C(L12),C(L21),T)
          CALL MMLT(M,N,M,A,A(L12),A(L21),A,A(L21),A(L12),
     +                                  T,T(L12),T(L21),T)
          R0  =  0.
          DO 51     I  =  1, M
             I1  =  NDX2F(I,1)
             I2  =  NDX2F(I,2)
             E   =  VDIST(M,C(I1),C(I2),T(I1),T(I2))
             IF(E .EQ. 0.)  GOTO 51
             CALL VMAXA(M,T(I1),T(I2),IDUMMY,ABSREF)
             R0  =  AMAX1(R0,E/ABSREF)
  51         CONTINUE
          N0  =  IRESF(R0)
          M0  =  20
          OKA =  N0 .LE. M0
          IF(.NOT. OKA)  WRITE(IOUNIT,1005)M,N,IDIM,M0,N0,R0
  52      OKL  =  .TRUE.
          IF(N .GT. M)  GOTO 54
          DO 53     JCNFG  =  1, NCNFG
             CALL CNFGMX(M,M,LG,LC,LC12,LC21)
             CALL MCPY(M,N,A,A(L12),A(L21),GC(LC),GC(LC12),GC(LC21))
             CALL MMLT(M,N,M,GC(LC),GC(LC12),GC(LC21),
     +                       GC(LC),GC(LC21),GC(LC12),
     +                       GC(LC),GC(LC12),GC(LC21), T)
             IF(N .GT. 0)
     +          CALL CHECKL(M,M,C,LG,GC,LC,LC12,LC21,OKT,VDIST,VSET)
             IF(N .LE. 0)
     +          CALL CHECKL(0,0,C,LG,GC,LC,LC12,LC21,OKT,VDIST,VSET)
             IF(.NOT. OKT)
     +             WRITE(IOUNIT,1015)M,N,JCNFG,LC,LC12,LC21
             OKL  =  OKL .AND. OKT
  53         CONTINUE
  54      OK  =  OK .AND. OKA .AND. OKL
          IF(.NOT. CCFLAG)  RETURN
          OKA  =  .TRUE.
          IF(MIN0(M,N) .LE. 0)  GOTO 62
          CALL MRAN(M,N,ALPHA,BETA,A,A(L12),A(L21))
          CALL MCPY(M,N,A,A(L12),A(L21),C,C(L12),C(L21))
          CALL KFLUSH
          CALL MMLT(M,N,M,C,C(L12),C(L21),C,C(L21),C(L12),
     +                                    C,C(L21),C(L12),T)
          CALL MMLT(M,N,M,A,A(L12),A(L21),A,A(L21),A(L12),
     +                                  T,T(L21),T(L12),T)
          R0  =  0.
          DO 61     I  =  1, M
             I1  =  NDX2F(I,1)
             I2  =  NDX2F(I,2)
             E   =  VDIST(M,C(I1),C(I2),T(I1),T(I2))
             IF(E .EQ. 0.)  GOTO 61
             CALL VMAXA(M,T(I1),T(I2),IDUMMY,ABSREF)
             R0  =  AMAX1(R0,E/ABSREF)
  61         CONTINUE
          N0  =  IRESF(R0)
          M0  =  20
          OKA =  N0 .LE. M0
          IF(.NOT. OKA)  WRITE(IOUNIT,1006)M,N,IDIM,M0,N0,R0
  62      OKL  =  .TRUE.
          IF(N .GT. M)  GOTO 64
          DO 63     JCNFG  =  1, NCNFG
             CALL CNFGMX(M,M,LG,LC,LC12,LC21)
             CALL MCPY(M,N,A,A(L12),A(L21),GC(LC),GC(LC12),GC(LC21))
             CALL MMLT(M,N,M,GC(LC),GC(LC12),GC(LC21),
     +                       GC(LC),GC(LC21),GC(LC12),
     +                       GC(LC),GC(LC21),GC(LC12), T)
             IF(N .GT. 0)
     +          CALL CHECKL(M,M,C,LG,GC,LC,LC12,LC21,OKT,VDIST,VSET)
             IF(N .LE. 0)
     +          CALL CHECKL(0,0,C,LG,GC,LC,LC12,LC21,OKT,VDIST,VSET)
             IF(.NOT. OKT)
     +             WRITE(IOUNIT,1016)M,N,JCNFG,LC,LC12,LC21
             OKL  =  OKL .AND. OKT
  63         CONTINUE
  64      OK  =  OK .AND. OKA .AND. OKL
          RETURN
1001      FORMAT(/ 25H ??? ARITHMETIC ERROR ???,6I8,1P,E12.3)
1002      FORMAT(/ 32H ??? ARITHMETIC ERROR (Y=XY) ???,6I8,1P,E12.3)
1003      FORMAT(/ 32H ??? ARITHMETIC ERROR (X=XY) ???,6I8,1P,E12.3)
1004      FORMAT(/ 33H ??? ARITHMETIC ERROR (Z=XX') ???,5I8,1P,E12.3)
1005      FORMAT(/ 33H ??? ARITHMETIC ERROR (X=XX') ???,5I8,1P,E12.3)
1006      FORMAT(/ 33H ??? ARITHMETIC ERROR (X'=XX')???,5I8,1P,E12.3)
1011      FORMAT(/ 20H ??? LOGIC ERROR ???,7I8)
1012      FORMAT(/ 27H ??? LOGIC ERROR (Y=XY) ???,7I8)
1013      FORMAT(/ 27H ??? LOGIC ERROR (X=XY) ???,7I8)
1014      FORMAT(/ 28H ??? LOGIC ERROR (Z=XX') ???,6I8)
1015      FORMAT(/ 28H ??? LOGIC ERROR (X=XX') ???,6I8)
1016      FORMAT(/ 28H ??? LOGIC ERROR (X'=XX')???,6I8)
          END
         SUBROUTINE TMXXX(M,N,K,A,B,C,T,LG,GA,GB,GC,FMONE,ALPHA,BETA,OK,
     +               MMLA,MMLS,MNMA,MNMS,MADD,MCPY,
     +               MMPY,MRAN,MSCL,MSUB,VDIST,VMAXA,VSET)
          REAL      A(*), B(*), C(*), T(*), GA(*), GB(*), GC(*)
          REAL      FMONE(2), ALPHA(2), BETA(2)
          LOGICAL   OK, OKA, OKL, OKT
          REAL      E, R0, RES
          EXTERNAL  VDIST,    VSET
#include "kernnumt/sysdat.inc"
#include "ch3dat.inc"
          NDX2F(I,J)  =  ((J-1)*IDIM + I-1)*LENGTH + 1
          IRESF(RES)  =  NINT(RES/TRELPR)
          OK  =  .TRUE.
          DO 20     ISUB  =  1, 4
          OKA  =  .TRUE.
          IF(MIN0(M,N,K) .LE. 0)  GOTO 10
          CALL MRAN(M,N,ALPHA,BETA,A,A(L12),A(L21))
          CALL MRAN(N,K,ALPHA,BETA,B,B(L12),B(L21))
          CALL MRAN(M,K,ALPHA,BETA,C,C(L12),C(L21))
          CALL MCPY(M,K,C,C(L12),C(L21),T,T(L12),T(L21))
          GOTO(1,2,3,4), ISUB
   1      CALL KFLUSH
          CALL MMLA(M,N,K,A,A(L12),A(L21),B,B(L12),B(L21),
     +                                    C,C(L12),C(L21))
          CALL MSUB(M,K,C,C(L12),C(L21),T,T(L12),T(L21),
     +                                  GB,GB(L12),GB(L21))
          GOTO 6
   2      CALL KFLUSH
          CALL MMLS(M,N,K,A,A(L12),A(L21),B,B(L12),B(L21),
     +                                    C,C(L12),C(L21))
          CALL MADD(M,K,C,C(L12),C(L21),T,T(L12),T(L21),
     +                                  GB,GB(L12),GB(L21))
          GOTO 6
   3      CALL KFLUSH
          CALL MNMA(M,N,K,A,A(L12),A(L21),B,B(L12),B(L21),
     +                                    C,C(L12),C(L21))
          CALL MSUB(M,K,T,T(L12),T(L21),C,C(L12),C(L21),
     +                                  GB,GB(L12),GB(L21))
          GOTO 6
   4      CALL KFLUSH
          CALL MNMS(M,N,K,A,A(L12),A(L21),B,B(L12),B(L21),
     +                                    C,C(L12),C(L21))
          CALL MSCL(M,K,FMONE,C,C(L12),C(L21),GB,GB(L12),GB(L21))
          CALL MSUB(M,K,GB,GB(L12),GB(L21),T,T(L12),T(L21),
     +                                  GB,GB(L12),GB(L21))
   6      R0  =  0.
          DO 7      I  =  1, M
             I1  =  NDX2F(I,1)
             I2  =  NDX2F(I,2)
             CALL MMPY(K,N,B,B(L21),B(L12),A(I1),A(I2),GA,GA(L2))
             E   =  VDIST(K,GB(I1),GB(I2),GA,GA(L2))
             IF(E .EQ. 0.)  GOTO 7
             CALL VMAXA(K,GA,GA(L2),IDUMMY,ABSREF)
             R0  =  AMAX1(R0,E/ABSREF)
   7         CONTINUE
          N0  =  IRESF(R0)
          M0  =  40
          OKA =  N0 .LE. M0
          IF(.NOT. OKA) WRITE(IOUNIT,1001) ISUB,M,N,K,IDIM,M0,N0,R0
  10      OKL  =  .TRUE.
          DO 19     JCNFG  =  1, NCNFG
             CALL CNFGMX(M,N,LG,LA,LA12,LA21)
             CALL CNFGMX(N,K,LG,LB,LB12,LB21)
             CALL CNFGMX(M,K,LG,LC,LC12,LC21)
             CALL MCPY(M,N,A,A(L12),A(L21),GA(LA),GA(LA12),GA(LA21))
             CALL MCPY(N,K,B,B(L12),B(L21),GB(LB),GB(LB12),GB(LB21))
             IF(MIN0(M,N,K) .GT. 0)
     +       CALL MCPY(M,K,T,T(L12),T(L21),GC(LC),GC(LC12),GC(LC21))
             GOTO(11, 12, 13, 14), ISUB
  11         CALL MMLA(M,N,K,GA(LA),GA(LA12),GA(LA21),
     +                       GB(LB),GB(LB12),GB(LB21),
     +                       GC(LC),GC(LC12),GC(LC21))
             GOTO 16
  12         CALL MMLS(M,N,K,GA(LA),GA(LA12),GA(LA21),
     +                       GB(LB),GB(LB12),GB(LB21),
     +                       GC(LC),GC(LC12),GC(LC21))
             GOTO 16
  13         CALL MNMA(M,N,K,GA(LA),GA(LA12),GA(LA21),
     +                       GB(LB),GB(LB12),GB(LB21),
     +                       GC(LC),GC(LC12),GC(LC21))
             GOTO 16
  14         CALL MNMS(M,N,K,GA(LA),GA(LA12),GA(LA21),
     +                       GB(LB),GB(LB12),GB(LB21),
     +                       GC(LC),GC(LC12),GC(LC21))
  16         IF(N .GT. 0)
     +          CALL CHECKL(M,K,C,LG,GC,LC,LC12,LC21,OKT,VDIST,VSET)
             IF(N .LE. 0)
     +          CALL CHECKL(0,0,C,LG,GC,LC,LC12,LC21,OKT,VDIST,VSET)
             IF(.NOT. OKT)
     +             WRITE(IOUNIT,1011) ISUB,M,N,K,JCNFG,LC,LC12,LC21
             OKL  =  OKL .AND. OKT
  19         CONTINUE
          OK  =  OK .AND. OKA .AND. OKL
  20      CONTINUE
  60      OK  =  OK .AND. OKA .AND. OKL
          RETURN
1001      FORMAT(/ 25H ??? ARITHMETIC ERROR ???,7I8,1P,E12.3)
1011      FORMAT(/ 20H ??? LOGIC ERROR ???,8I8)
          END
