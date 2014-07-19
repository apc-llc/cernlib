*
* $Id: c316m.F,v 1.1.1.1 1996/04/01 15:01:15 mclareni Exp $
*
* $Log: c316m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:15  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C316M
C     This program tests the operation of MATHLIB subprograms
C     RPSIPG and DPSIPG  (C316)

#include "imp64r.inc"
      REAL RPSIPG
C
      CHARACTER*6 TFUNC(2)
C
C     Set maximum error allowed for test to be considered successful
      DIMENSION TOL(2)

      LOGICAL LTEST
      PARAMETER (Z0 = 0, Z1 = 1)
      DIMENSION X(9),TEST(0:6,8)
#include "iorc.inc"
      DATA TFUNC/'RPSIPG','DPSIPG'/
      DATA TOL/1D-6, 8D-9 /
      DATA LTEST/.TRUE./

C     Input parameters for individual tests
       DATA X( 1 )/   0.4D0/
       DATA X( 2 )/   2.5D0/
       DATA X( 3 )/   3.0D0/
       DATA X( 4 )/   8.0D0/
       DATA X( 5 )/  12.0D0/
       DATA X( 6 )/  15.0D0/
       DATA X( 7 )/  -2.5D0/
       DATA X( 8 )/  -0.4D0/
C      Analytical values expected to be obtained

             DATA (TEST(0,J),J=1,8)
     +  /-2.56138454458511600     D0,  0.703156640645243275     D0,
     +   0.922784335098467037     D0,   2.01564147795560977     D0,
     +    2.44266167997581185     D0,   2.67434666166079360     D0,
     +    1.10315664064524266     D0,  0.959380786106809769     D0/
             DATA (TEST(1,J),J=1,8)
     +  / 7.27535659052959738     D0,  0.490357756100234854     D0,
     +   0.394934066848226420     D0,  0.133137014694031428     D0,
     +   0.869018728717683708D-01   ,  0.689382278476837890D-01   ,
     +    9.53924664498912400     D0,   9.88620967090235880     D0/
             DATA (TEST(2,J),J=1,8)
     +  /-32.2391286235783596     D0, -0.236204051641727378     D0,
     +  -0.154113806319188543     D0, -0.176995691957677753D-01   ,
     +  -0.754720536899890996D-02   , -0.475060271655155294D-02   ,
     +  -0.108204051641735938     D0,   21.2871684628565419     D0/
             DATA (TEST(3,J),J=1,8)
     +  / 236.195259033947110     D0,  0.223905848817252034     D0,
     +   0.118939402266829114     D0,  0.469923979594508157D-02   ,
     +   0.131009323107082591D-02   ,  0.654479778282737376D-03   ,
     +    194.747876219187631     D0,   281.782041045345863     D0/
             DATA (TEST(4,J),J=1,8)
     +  /-2348.59127348799035     D0, -0.313755999506731345     D0,
     +  -0.136266123440878212     D0, -0.186879515063761992D-02   ,
     +  -0.340910050701089473D-03   , -0.135196191875192876D-03   ,
     +  -0.679959995074066875D-01   ,   2032.55726807073870     D0/
             DATA (TEST(5,J),J=1,8)
     +  / 29313.5424692587758     D0,  0.578569178567183456     D0,
     +   0.206167438133896735     D0,  0.989510004771321228D-03   ,
     +   0.118208290510815061D-03   ,  0.372221509496196595D-04   ,
     +    15382.1400480263037     D0,   31876.5069077684757     D0/
             DATA (TEST(6,J),J=1,8)
     +  /-439523.164998806766     D0,  -1.31800610755003489     D0,
     +  -0.386479714984435299     D0, -0.654007738835768460D-03   ,
     +  -0.512034865641779365D-04   , -0.128049887546262730D-04   ,
     +  -0.138358107663345109     D0,   413705.127071637107     D0/

      CALL HEADER('C316',0)
         ERRMAX= 0.0D0
      WRITE(LOUT,'(1X)')
#if defined(CERNLIB_DOUBLE)
      NF=2
#endif
#if !defined(CERNLIB_DOUBLE)
      NF=1
#endif
      DO 1000 JF=1,NF
         ERRMAX= 0.0D0
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/10X,''TEST FOR RPSIPG'')')
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)WRITE(LOUT,'(/10X,''TEST FOR RPSIPG'')')
      IF(JF.EQ.2)WRITE(LOUT,'(/10X,''TEST FOR DPSIPG'')')
#endif
        WRITE(LOUT,'(/5X,''K'',8X,''X'',
     +   14X,'' '',A,''(X)'',18X,''Exact'',11X,''Rel Error'')')
     +      TFUNC(JF)
      DO 9 N = 0,6
      DO 1 I = 1,8
#if !defined(CERNLIB_DOUBLE)
      IF ( .NOT. (N .EQ. 6 .AND. I .EQ. 7))THEN
      DAT=RPSIPG(X(I),N)
      IF(DAT .NE. 0) EDAT=ABS((DAT-TEST(N,I))/DAT)
         ERRMAX= MAX( ERRMAX,EDAT)
      WRITE(LOUT,'(1X,I5,F10.1,1P,2D27.18,D10.1)') N,X(I),DAT,
     + TEST(N,I),EDAT
      ENDIF
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1) THEN
      RAT=RPSIPG(SNGL(X(I)),N)
      IF(RAT .NE. 0) RDAT=ABS((RAT-SNGL(TEST(N,I)))/RAT)
         ERRMAX= MAX( SNGL(ERRMAX),RDAT)
      WRITE(LOUT,'(1X,I5,F10.1,1P,2E27.9,E10.1)') N,X(I),RAT,
     + SNGL(TEST(N,I)),RDAT
       ENDIF
      IF(JF.EQ.2) THEN
      DAT=DPSIPG(X(I),N)
      IF(DAT .NE. 0) EDAT=ABS((DAT-TEST(N,I))/DAT)
         ERRMAX= MAX( ERRMAX,EDAT)
      WRITE(LOUT,'(1X,I5,F10.1,1P,2D27.18,D10.1)') N,X(I),DAT,
     + TEST(N,I),EDAT
       ENDIF
#endif
    1 CONTINUE
    9  CONTINUE
#if !defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF+1)
#endif
#if defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF)
#endif
      WRITE(LOUT,'(/''Largest Relative Error was''
     +,1P,D10.1)') ERRMAX
      LTEST=LTEST.AND.(ERRMAX.LE.ETOL)
         ERRMAX= 0D0
      WRITE(LOUT,'(/''TESTING ERROR MESSAGES:'')')
      WRITE(LOUT,'(1X)')
#if !defined(CERNLIB_DOUBLE)
      DR=RPSIPG(Z1,8)
      WRITE(LOUT,'(1X)')
      DR=RPSIPG(-Z1,3)
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)THEN
                 RZ1=Z1
      DR=RPSIPG(RZ1,8)
      WRITE(LOUT,'(1X)')
      DR=RPSIPG(-RZ1,3)
      ENDIF
      IF(JF.EQ.2)THEN
      DR=DPSIPG(Z1,8)
      WRITE(LOUT,'(1X)')
      DR=DPSIPG(-Z1,3)
      ENDIF
#endif
 1000 CONTINUE
C     Check if the test was successful
      IRC=ITEST('C316',LTEST)
      CALL PAGEND('C316')
      END
