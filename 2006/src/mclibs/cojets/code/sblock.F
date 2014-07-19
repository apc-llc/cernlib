*
* $Id: sblock.F,v 1.1.1.1 1996/01/11 14:14:42 mclareni Exp $
*
* $Log: sblock.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:42  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE SBLOCK
C     *****************
C-- COMPLETES BLOCK DATA SETTINGS
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/alqgen.inc"
#include "cojets/bopar.inc"
#include "cojets/cfun1.inc"
#include "cojets/cutoff.inc"
#include "cojets/data1.inc"
#include "cojets/data2.inc"
#include "cojets/data3.inc"
#include "cojets/decpar.inc"
#include "cojets/edpar.inc"
#include "cojets/eicht.inc"
#include "cojets/event.inc"
#include "cojets/evtype.inc"
#include "cojets/forgen.inc"
#include "cojets/fratab.inc"
#include "cojets/fstate.inc"
#include "cojets/inifl.inc"
#include "cojets/inmat.inc"
#include "cojets/intype.inc"
#include "cojets/itapes.inc"
#include "cojets/jetset.inc"
#include "cojets/kdump.inc"
#include "cojets/keybre.inc"
#include "cojets/keyjet.inc"
#include "cojets/m2qua.inc"
#include "cojets/maxn.inc"
#include "cojets/nflav.inc"
#include "cojets/par.inc"
#include "cojets/parq.inc"
#include "cojets/parqua.inc"
#include "cojets/qcds.inc"
#include "cojets/stable.inc"
#include "cojets/tabpsq.inc"
#include "cojets/tabqrk.inc"
#include "cojets/tleave.inc"
#include "cojets/top.inc"
#include "cojets/weakbo.inc"
#include "cojets/weakon.inc"
#include "cojets/zpar2.inc"
#include "cojets/zwpar.inc"
C
      EXTERNAL FUN2
      LOGICAL INPMW,INPDR
      DIMENSION KDP2(18,2)
      DATA KDP2/ 7,11,15, 9,13,17, 1000, 2000, 3000, 4000, 5000, 6000
     1                           , 9,13,17, 1000, 4000, 6000
     2          ,8,12,16,10,14,18,-1000,-2000,-3000,-4000,-5000,-6000
     3                           , 8,12,16,-2000,-3000,-5000/
      DATA INPMW,INPDR/2*.FALSE./
      DATA ICALL/0/
C
      IF(ICALL.GT.0) RETURN
      ICALL=1
      IFL=0
C-- READ PARTICLE TABLE
      CALL READTB
C-- /IDRUN/
      CALL IDGEN
C-- SET MXSPE
      MXSPE=MXINQ-1
C-- SET TOP MASS IF ALTERED BY OLD-TYPE INPUT
      IF(TOPMAS.NE.0.) QMAS(6)=TOPMAS
        IF(QMAS(6).LT.16.) THEN
          WRITE(ITLIS,155)
155       FORMAT(/' ***BAD INPUT (MINIMUM ACCEPTED'
     *    ,' VALUE FOR TOPMAS IS 16.)'
     *    ,' -- JOB WILL BE ABORTED')
          STOP
        ENDIF
C-- CALCULATE AGLU FOR NA3 STRUCTURE FUNCTIONS
      IFUN=6
      IF(IEICHT.EQ.0) THEN
        A1=0.
        B1=1.
        EPSI=.001
        AGLU=1.-ASIMP(A1,B1,EPSI,M,2,FUN2)
      ENDIF
C
C-- READ INPUT COMMANDS IF CALLED FROM SUB. COJETS
      IF(INTYPE.EQ.1) CALL READIN(IFL)
C
C-- Z, W MASSES AND DELTAR (RADIATIVE CORRECTION FROM SIRLIN, INCLUDING
C-- VARIATION WITH TOP MASS FROM HOLLIK)
      PI=4.*ATAN(1.)
      PI2=2.*PI
      ALFQED=1./137.03604
      IF(PMAS(3).GT.0.) INPMW=.TRUE.
      IF(DELTAR.GT.0.)  INPDR=.TRUE.
      DRT=0.
      IF(.NOT.INPDR) THEN
        DELTAR=.0709
        FDRT=-(ALFQED/(4.*PI))*3./4.*(1.-S2THW)/S2THW**2
        DMT2=QMAS(6)**2-35.**2
        IF(DMT2.LT.0.) DMT2=0.
      ENDIF
      IF(.NOT.INPMW) THEN
        AMW=SQRT(PI*ALFQED/(S2THW*GF*SQRT(2.)))
        PMAS(3)=AMW/SQRT(ABS(1.-(DELTAR+DRT)))
        IF(.NOT.INPDR) THEN
          DRT=FDRT*DMT2/PMAS(3)**2
          PMAS(3)=AMW/SQRT(ABS(1.-(DELTAR+DRT)))
        ENDIF
      ELSE IF(.NOT.INPDR) THEN
        DRT=FDRT*DMT2/PMAS(3)**2
      ENDIF
      DELTAR=DELTAR+DRT
      IF(DELTAR.GE.1.) THEN
        IF(INPDR) THEN
          WRITE(ITLIS,7) DELTAR
7         FORMAT(/' ***DELTAR =',E12.5,' IN INPUT IS TOO HIGH'
     *    ,' -- MUST BE DELTAR < 1. -- JOB WILL BE ABORTED')
          STOP
        ELSE
          WRITE(ITLIS,8) DELTAR,QMAS(6),PMAS(3)
8         FORMAT(/' ***DELTAR =',E12.5,' IS CALCULATED TOO HIGH'
     *    ,' -- MUST BE DELTAR < 1.'
     *    /' TOP MASS =',E12.5
     *    /' W MASS   =',E12.5
     *    /' -- JOB WILL BE ABORTED')
          STOP
        ENDIF
      ENDIF
      PMAS(4)=PMAS(3)
      IF(PMAS(2).LE.0.) PMAS(2)=PMAS(3)/SQRT(ABS(1.-S2THW))
C
      WEAKON=WEAKBO
      IF(IEVTYP.EQ.2.AND.IDECBO.GT.0) NFLAV=MAX(NFLAV,IDECBO-6)
      IF(IEVTYP.EQ.3.AND.IDECBO.GT.0) NFLAV=MAX(NFLAV,(IDECBO-3)*2)
C
C-- SET IDB FOR Z,W
      IDB(2)=1
      IDB(3)=13
      IDB(4)=IDB(3)
C
C-- Z0 BRANCHING FRACTIONS
      RHOZ=(QMAS(6)/PMAS(2))**2
      XW=S2THW
      AQ(1)=-2./3.*XW+.25
      BQ(1)=.25
      AQ(2)=1./3.*XW-.25
      BQ(2)=-.25
      AL(1)=XW-.25
      BL(1)=-.25
      AL(2)=.25
      BL(2)=.25
      FL=.25-XW+2.*XW**2
      FN=.25
      FUP=3.*(.25-2./3.*XW+8./9.*XW**2)
      FDW=3.*(.25-1./3.*XW+2./9.*XW**2)
      CBR( 1)=FL
      CBR( 2)=FL
      CBR( 3)=FL
      CBR( 4)=FN
      CBR( 5)=FN
      CBR( 6)=FN
      CBR( 7)=FUP
      CBR( 8)=FDW
      CBR( 9)=FDW
      CBR(10)=FUP
      CBR(11)=FDW
      CBR(12)=0.
      IF(NFLAV.GE.6)
     *CBR(12)=(FUP-3.*RHOZ*(.25+4./3.*XW-16./9.*XW**2))
     *        *SQRT(ABS(1.-4.*RHOZ))
      IF(CBR(12).LT.0.) CBR(12)=0.
      IF((2.*QMAS(6)+2.).GT.PMAS(2)) CBR(12)=0.
      DO 1 L=2,12
1     CBR(L)=CBR(L)+CBR(L-1)
      ZCHDEC=CBR(12)/(CBR(11)+FUP)
      DO 2 L=1,12
2     CBR(L)=CBR(L)/CBR(12)
      CBR(12)=1.
C
C-- W BRANCHING FRACTIONS
      RHOT=(QMAS(6)/PMAS(3))**2
      RHOB=(QMAS(5)/PMAS(3))**2
      CBR(13)=1.
      CBR(14)=1.
      CBR(15)=1.
      CBR(16)=3.
      CBR(17)=3.
      CBR(18)=0.
      IF(NFLAV.GE.6)
     *CBR(18)=3.*(1.-.5*(RHOT+RHOB)-.5*(RHOT-RHOB)**2)
     *        *SQRT(ABS(1.-2.*(RHOT+RHOB)+(RHOT-RHOB)**2))
      IF(CBR(18).LT.0.) CBR(18)=0.
      IF((QMAS(6)+QMAS(5)+2.).GT.PMAS(3)) CBR(18)=0.
      DO 3 L=14,18
3     CBR(L)=CBR(L)+CBR(L-1)
      WCHDEC=CBR(18)/(CBR(17)+3.)
      DO 4 L=13,18
4     CBR(L)=CBR(L)/CBR(18)
      CBR(18)=1.
C
C-- SET DECAY CHANNELS FOR Z, W
      DO 21 L2=1,2
      DO 21 L1=1,18
21    KDP(L1,L2)=KDP2(L1,L2)
      DO 22 L2=3,5
      DO 22 L1=1,18
22    KDP(L1,L2)=0
C
C-- Z, W WIDTHS
      ZGAM=GF*SQRT(2.)/PI*(1.-2.*XW+8./3.*XW**2)*PMAS(2)**3*ZCHDEC
      WGAM=GF*SQRT(2.)/PI*PMAS(3)**3*WCHDEC
C
C-- SET UNDEFINED MASSES FOR HADRONS
      DO 30 NP=1,352
      IDA=ABS(IDENTF(NP))
      IF(IDA.GT.100.AND.PMAS(NP).EQ.0.) THEN
        IP=IDA/1000
        JP=MOD(IDA/100,10)
        KP=MOD(IDA/10,10)
        JSPIN=MOD(IDA,10)
        PMAS(NP)=QMAS(JP)+QMAS(KP)-.03+.04*JSPIN
        IF(IP.NE.0) PMAS(NP)=PMAS(NP)+QMAS(IP)
      ENDIF
30    CONTINUE
C-- MODIFY IDB( ) ACCORDING TO RDECAY( ) REQUESTS
      DO 6 IP=1,352
      IF(RDECAY(IP).NE.0.) GO TO 6
      IDB(IP)=0
      IDPM=-IDEXT(IP)
      IPA=INTID(IDPM)
      IF(IDEXT(IPA).NE.IDPM) GO TO 6
      IDB(IPA)=0
6     CONTINUE
C
C-- SET MASS**2 OF FINAL QUANTA, QUANTA CUTOFFS
      DO 18 L=1,3
      QZMASS(L)=0.
18    Q2QUA(L)=0.
      DO 17 L=3,6
      QZMASS(L)=QMAS(L)
17    Q2QUA(L)=(QMAS(L))**2
C
C-- HANDLE FORCING COMMANDS
      CALL FORPRE(IFL)
C
C-- HANDLE STOP DECAY COMMANDS (FOR CONVENTIONAL INPUT)
      IF(INTYPE.NE.0) GO TO 302
      DO 303 I=1,352
      IF(RDECAY(I).NE.0.) GO TO 303
      NSTOPD=NSTOPD+1
      KSTOPD(NSTOPD)=IDEXT(I)
303   CONTINUE
302   CONTINUE
C
C-- ABNORMAL EXIT
300   IF(IFL.EQ.1) THEN
        WRITE(ITLIS,301)
301     FORMAT(/' ***JOB ABORTED')
        STOP
      ENDIF
      RETURN
      END
