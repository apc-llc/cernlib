*
* $Id: eudcin.F,v 1.1.1.1 1996/03/08 16:58:50 mclareni Exp $
*
* $Log: eudcin.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:50  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      SUBROUTINE EUDCIN
C.----------------------------------------------------------------------
C.
C.    THE SUBROUTINE INITIALIZES THE COMMON BLOCKS WHICH CONTAIN
C.    DETAILED INFORMATION FOR THE FRAGMENTATION AND DECAY PACKAGE.
C.    MOST OF THE DEFAULT VALUES SET HERE CAN BE CHANGED BY THE USER.
C.    LAST UPDATE: 12/09/89
C.
C.----------------------------------------------------------------------
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION YGLU,YLQU,YHVY
#endif
#include "eurodec/eudver.inc"
#include "eurodec/hvyini.inc"
#include "eurodec/eudopt.inc"
#include "eurodec/inpout.inc"
#include "eurodec/wekpar.inc"
#include "eurodec/ptable.inc"
#include "eurodec/epsfrg.inc"
#include "eurodec/glufix.inc"
#include "eurodec/quafix.inc"
#include "eurodec/frgfix.inc"
#include "eurodec/ratmix.inc"
#include "eurodec/pwidth.inc"
#include "eurodec/dforce.inc"
#include "eurodec/convrt.inc"
#include "eurodec/inivtx.inc"
#include "eurodec/picons.inc"
#include "eurodec/runinf.inc"
#include "eurodec/rnseed.inc"
#include "eurodec/momgen.inc"
#include "eurodec/filnam.inc"
#include "eurodec/onipot.inc"
#include "eurodec/boscop.inc"
      SAVE INIT
      DATA INIT/ 0/
C--
C--   RETURN IF ALREADY INITIALIZED...
      IF (INIT.NE.0) RETURN
      INIT=1
C--
C--   EURODEC VERSION NUMBER
      IVDEC1=204
      IVDEC2=890912
C--
C--   DEFAULT MASSES FOR INIBRN (INTERFACE WITH GENERATOR)...
      AMINIE(1)=60.
      AMINIE(2)=100.
      AMINIE(3)=160.
      AMINIE(4)=40.
      AMINIE(5)=9.6
      AMINIE(6)=23.
      AMINIE(7)=82.
      AMINIE(8)=93.
      AMINIE(9)=10.
      AMINIE(10)=20.
      AMINIE(11)=80.
      AMINIE(12)=200.
C--
C--   ...AND CHARGES
      EFERM(1)=2./3.
      EFERM(2)=-1./3.
      EFERM(3)=EFERM(2)
      EFERM(4)=EFERM(1)
      EFERM(5)=EFERM(2)
      EFERM(6)=EFERM(1)
      EFERM(7)=EFERM(2)
      EFERM(8)=EFERM(1)
      EFERM(9)=-1.
      EFERM(10)=0.
      EFERM(11)=-1.
      EFERM(12)=0.
      EFERM(13)=-1.
      EFERM(14)=0.
      EFERM(15)=-1.
      EFERM(16)=0.
C--
C--   EURODEC FRAGMENTATION AND DECAY OPTIONS
      ICASC=1
      IDEFDC=0
      IBRDMP=0
      IHVYFR=1
      MSMEAR=0
      PLIFET=1.E+12
C--
C--   BOUND STATE POTENTIAL: RICHARDSON (DEFAULT) OR QCD
      IONIPT=0
C--
C--   INITIALIZE PARTICLE CONVERSION TABLE
      DO 10 I=1,NCMAX
   10 ICONV(I)=0
C--
C--   LOGICAL INPUT/OUTPUT UNITS FOR TERMINAL, DATAFILES
      LUN0=10
      LUN1=11
      LUN2=6
C--
C--   FILENAMES FOR DATA AND TITLE FILES
      EUDATD='EUDDAT'
      EUTITD='EUDTIT'
C--
C--   E.M. AND WEAK INTERACTION PARAMETERS
      ALPHA=.0078247261
      XW=.22
      GF=1.16632E-5
C--
C--   GLUON FRAGMENTATION PARAMETERS
      EPSGLU=0.
      SIGG=.4
      EGLMIN=.8
      OFFVAC=.45
C--
C--   QUARK FRAGMENTATION PARAMETERS
      EPSLQU=.89
      EPSHVY(1)=.05
      EPSHVY(2)=.015
      EPSHVY(3)=.00025
      EPSHVY(4)=.0001
      EPSHVY(5)=.0001
      SIGQ=.4
      EQUMIN(1)=.2
      EQUMIN(2)=.5
      QDEPOL=.5
C--
C--   FRAGMENTATION PARAMETERS:
C--   PROMES: PROBABILITY MESON OVER BARYON
C--   PROPSE: PROBABILITY PSEUDOSCALAR OVER VECTOR MESON (LIGHT, HEAVY)
C--   PROS  : PROBABILITY VACUUM --> S SBAR OVER U UBAR ETC.
C--   PRUS  : PROBABILITY VACUUM --> US UBARSBAR OVER UU UBARUBAR ETC.
C--   PRSS  : PROBABILITY VACUUM --> SS SBAR SBAR OVER UU UBARUBAR
C--   PRCC  : PROBABILITY GLUON  --> C CBAR (NON-PERTURBATIVE)
      PROMES=.9
      PROPSE(1)=.5
      PROPSE(2)=.4
      PROS=.18
      PRUS=.15
      PRSS=.10
      PRCC=0.
C--
C--   MIXING PARAMETERS IN THE BD-BDBAR AND BS-BSBAR SYSTEMS
      RD0MIX=0.
      RBDMIX=0.
      RBSMIX=0.
C--
C--   INITIALIZATION FOR PARTICLE WIDTHS AND DECAY FORCING
      DO 20 I=1,NPMAX
        VMLO(I)=-1.
        VMUP(I)=-1.
        GAM(I)=0.
        IFRCP(I)=0
   20 IFRCM(I)=0
C--
C--   INITIALIZE PRIMARY VERTEX, NO GENERATION OF SECONDARY VERTEX!
      VTXSIG(1)=0.
      VTXSIG(2)=0.
      VTXSIG(3)=0.
      IPVTX=0
      DO 30 I=1,NGMAX
        DO 30 J=1,3
   30 PRIVTX(J,I)=0.
      ISVTX=0
C--
C--   PI CONSTANTS
      PI=3.1415926537
      TWOPI=6.2831853073
C--
C--   SEEDS FOR RANDOM NUMBER GENERATOR
      ISEED(1)=1802
      ISEED(2)=9373
C--
C--   INITIALIZE RANDOM NUMBER GENERATOR
      CALL EURRIN
C--
C--   RUN INFORMATION
      IRUN=1
      NEVENT=0
      WEIGHT=0
      RETURN
      END
