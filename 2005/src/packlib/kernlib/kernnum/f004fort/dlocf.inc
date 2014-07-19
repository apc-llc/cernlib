*
* $Id: dlocf.inc,v 1.1.1.1 1996/02/15 17:49:00 mclareni Exp $
*
* $Log: dlocf.inc,v $
* Revision 1.1.1.1  1996/02/15 17:49:00  mclareni
* Kernlib
*
*
*
* dlocf.inc
*
#if defined(CERNLIB_NUMAP)||defined(CERNLIB_NUMCD)||defined(CERNLIB_NUMCV)||defined(CERNLIB_NUMIB2)||defined(CERNLIB_NUMND)
          IF(MIN0(M,N,K) .LE. 0)  RETURN
          LOCX  =  LOCF(X(1))
          LOCY  =  LOCF(Y(1))
          LOCZ  =  LOCF(Z(1))
          IX  =  (LOCF(X21(1)) - LOCX) / 2
          JX  =  (LOCF(X12(1)) - LOCX) / 2
          JY  =  (LOCF(Y21(1)) - LOCY) / 2
          LY  =  (LOCF(Y12(1)) - LOCY) / 2
          IZ  =  (LOCF(Z21(1)) - LOCZ) / 2
          LZ  =  (LOCF(Z12(1)) - LOCZ) / 2
#endif
#if defined(CERNLIB_NUMCR)||defined(CERNLIB_NUMUC)
          IF(MIN0(M,N,K) .LE. 0)  RETURN
          LOCX  =  LOC(X(1))
          LOCY  =  LOC(Y(1))
          LOCZ  =  LOC(Z(1))
          IX  =  (LOC(X21(1)) - LOCX) / 2
          JX  =  (LOC(X12(1)) - LOCX) / 2
          JY  =  (LOC(Y21(1)) - LOCY) / 2
          LY  =  (LOC(Y12(1)) - LOCY) / 2
          IZ  =  (LOC(Z21(1)) - LOCZ) / 2
          LZ  =  (LOC(Z12(1)) - LOCZ) / 2
#endif
#if defined(CERNLIB_NUMDE)
          IF(MIN0(M,N,K) .LE. 0)  RETURN
          LOCX  =  %LOC(X(1))
          LOCY  =  %LOC(Y(1))
          LOCZ  =  %LOC(Z(1))
          IX  =  (%LOC(X21(1)) - LOCX) / 8
          JX  =  (%LOC(X12(1)) - LOCX) / 8
          JY  =  (%LOC(Y21(1)) - LOCY) / 8
          LY  =  (%LOC(Y12(1)) - LOCY) / 8
          IZ  =  (%LOC(Z21(1)) - LOCZ) / 8
          LZ  =  (%LOC(Z12(1)) - LOCZ) / 8
#endif
#if defined(CERNLIB_NUMIB1)
          IF(MIN0(M,N,K) .LE. 0)  RETURN
          LOCX  =  IADDR(X(1))
          LOCY  =  IADDR(Y(1))
          LOCZ  =  IADDR(Z(1))
          IX  =  (IADDR(X21(1)) - LOCX) / 8
          JX  =  (IADDR(X12(1)) - LOCX) / 8
          JY  =  (IADDR(Y21(1)) - LOCY) / 8
          LY  =  (IADDR(Y12(1)) - LOCY) / 8
          IZ  =  (IADDR(Z21(1)) - LOCZ) / 8
          LZ  =  (IADDR(Z12(1)) - LOCZ) / 8
#endif
