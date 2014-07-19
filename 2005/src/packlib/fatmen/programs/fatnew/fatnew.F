*
* $Id: fatnew.F,v 1.1.1.1 1996/03/07 15:17:39 mclareni Exp $
*
* $Log: fatnew.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:39  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      PROGRAM FATNEW
*=======================================================================
*
*     Main program to create a new CERN FATRZ file
*
*     VM   systems: REXX variables FATSYS & FATGRP & FATMODE
*     VMS  systems: Global symbols FATSYS & FATGRP & FATMODE
*     Unix systems: Environmental variables FATSYS & FATGRP & FATMODe
*     MVS  systems: GOPARM string 'fatsys,fatgrp,path'
*
*     e.g. FATSYS:==CERN
*          FATGRP:==L3    to make a new RZ file CERN.FATRZ for CERN
*                         experiment L3
*     e.g. FATSYS:==DESY
*          FATGRP:==H1    to make a new RZ file DESY.FATRZ for DESY
*                         experiment H1
*
*     On MVS systems, if PATH=R00PAV.H1, then the previous file will
*                     be created as R00PAV.H1.DESY.FATRZ
*
*=======================================================================
#include "fatmen/fatsys.inc"
#include "fatmen/slate.inc"
#if defined(CERNLIB_IBMMVS)
      CHARACTER*100 CHPARM
#endif
      CHARACTER*4   CHMODE
      CHARACTER*12  CHOPT
      CHARACTER*240 GENAM
      CHARACTER*12  CHUSER,CHSYS
      CHARACTER*80  PATH
      PARAMETER (LURCOR=200000)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)
      DIMENSION    LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
      COMMON /FATUSE/ IDEBFA, IDIVFA, IKDRFA, KOFSFA, KOFUFA, LBFXFA
     +              , LSAVFA, LTOPFA, LBBKFA, LBGNFA, LTDSFA, LBDSFA
     +              , LPRTFA, NTOPFA, LUFZFA, IOUPFA, IOBKFA, IODSFA
*
      COMMON /USRLNK/LUSRK1,LUSRBK,LUSRLS
*
      PARAMETER    (LKEYFA=10)
      CHARACTER*8  CHTAG(LKEYFA)
      CHARACTER*10 CHFOR
      DATA CHTAG/'Num.Id.',5*'Fname'
     +          ,'cp.level','loc.code','medium','nm.banks'/
      DATA CHFOR/'IHHHHHIIII'/
*
*     Initialise ZEBRA
*
      CALL MZEBRA(-3)
      CALL MZSTOR(IXSTOR,'/CRZT/','Q',IFENCE,LEV,BLVECT(1),BLVECT(1),
     +            BLVECT(5000),BLVECT(LURCOR))
 
      CALL MZLOGL(IXSTOR,-3)
*
* *** Define user division and link area like:
*
      CALL MZDIV  (IXSTOR, IXDIV, 'USERS', 50000, LURCOR, 'L')
      CALL MZLINK (IXSTOR, '/USRLNK/', LUSRK1, LUSRLS, LUSRK1)
#if defined(CERNLIB_IBMMVS)
      CALL GOPARM(LPARM,CHPARM)
      IF(LPARM.EQ.0) THEN
         NPARMS = 0
         PRINT *,'FATNEW. no GOPARM string.',
     +           ' FATSYS will default to CERN'
         CHSYS = 'CERN'
      ELSE
         CALL FMNWRD(',',CHPARM(1:LPARM),NPARMS)
      ENDIF
 
      IF(NPARMS.GT.0) THEN
         CALL FMWORD(CHSYS,0,',',CHPARM(1:LPARM),IRC)
      ENDIF
 
      IF(NPARMS.GE.2) THEN
         CALL FMWORD(CHUSER,1,',',CHPARM(1:LPARM),IRC)
      ELSE
         IRC = 1
      ENDIF
 
      IF(NPARMS.GE.3) THEN
         CALL FMWORD(DEFAULT,2,',',CHPARM(1:LPARM),IRC)
         LDEF = LENOCC(DEFAULT)
      ELSE
         CALL KPREFI(DEFAULT,LDEF)
      ENDIF
 
      IF(NPARMS.GE.4) THEN
         CALL FMWORD(CHMODE,3,',',CHPARM(1:LPARM),IRC)
         LMODE = LENOCC(MODE)
      ELSE
         LMODE = 0
      ENDIF
 
#endif
#if defined(CERNLIB_IBMVM)
      CALL VMREXX('F','FATSYS',CHSYS,IRC)
#endif
#if defined(CERNLIB_UNIX)||defined(CERNLIB_VAXVMS)
      CALL GETENVF('FATSYS',CHSYS)
      LDEF = IS(1)
      IRC = 0
      IF(LDEF.EQ.0) IRC = -1
#endif
#if !defined(CERNLIB_IBMMVS)
      IF(IRC.NE.0) THEN
         CHSYS = 'CERN'
#endif
#if defined(CERNLIB_IBMVM)
         PRINT *,'FATNEW. REXX variable FATSYS not defined. '
#endif
#if defined(CERNLIB_VAXVMS)
         PRINT *,'FATNEW. symbol FATSYS not defined.'
#endif
#if defined(CERNLIB_UNIX)
         PRINT *,'FATNEW. environmental variable FATSYS not defined. '
#endif
#if !defined(CERNLIB_IBMMVS)
         PRINT *,'defaulted to CERN'
      ENDIF
#endif
      CALL CLTOU(CHSYS)
      TOPDIR = '//'//CHSYS
#if defined(CERNLIB_IBMVM)
*
*     Take username from REXX variable 'FATGRP'
*     If not defined, use current username
*     This username determines the FATMEN group for whom we are working...
*
      CALL VMREXX('F','FATGRP',CHUSER,IRC)
#endif
#if defined(CERNLIB_UNIX)||defined(CERNLIB_VAXVMS)
      CALL GETENVF('FATGRP',CHUSER)
      LDEF = IS(1)
      IRC = 0
      IF(LDEF.EQ.0) IRC = -1
#endif
      IF(IRC.NE.0) THEN
         IC = FMUSER(CHUSER)
#if defined(CERNLIB_IBMMVS)
         PRINT *,'FATNEW. FATGRP not defined. '
#endif
#if defined(CERNLIB_IBMVM)
         PRINT *,'FATNEW. REXX variable FATGRP not defined. '
#endif
#if defined(CERNLIB_VAXVMS)
         PRINT *,'FATNEW. symbol FATGRP not defined.'
#endif
#if defined(CERNLIB_UNIX)
         PRINT *,'FATNEW. environmental variable FATGRP not defined. '
#endif
         PRINT *,'Using current username'
      ENDIF
      CALL CLTOU(CHUSER)
      SERNAM = CHUSER
 
      IF(CHUSER(1:2).NE.'FM') THEN
        SERNAM = 'FM' // CHUSER(1:LENOCC(CHUSER))
        ELSE
        SERNAM = CHUSER(1:LENOCC(CHUSER))
        ENDIF
      LSN    = LENOCC(SERNAM)
 
      CALL CLTOU(SERNAM)
*
*     Get directory where RZ file is kept...
*
#if defined(CERNLIB_UNIX)||defined(CERNLIB_VAXVMS)
      CALL CFILL(' ',DEFAULT,1,132)
      CALL GETENVF(SERNAM(1:LENOCC(SERNAM)),DEFAULT)
      LDEF = IS(1)
#endif
#if defined(CERNLIB_UNIX)||defined(CERNLIB_VAXVMS)
      CALL GETENVF('FATMODE',CHMODE)
      LMODE = IS(1)
#endif
#if defined(CERNLIB_IBMVM)
      CALL VMREXX('F','FATMODE',CHMODE,IRC)
      IF(IRC.EQ.0) THEN
         LMODE = LENOCC(CHMODE)
      ELSE
         LMODE = 0
      ENDIF
#endif
#if defined(CERNLIB_UNIX)
      IF(LMODE.EQ.0) THEN
         PRINT *,'FATNEW. RZ file will be created in EXCHANGE format'
         LMODE  = 1
         CHMODE = 'X'
      ENDIF
#endif
      IF(LMODE.EQ.0) THEN
         PRINT *,'FATNEW. native mode FORTRAN RZ file will be created'
         CHOPT = '1DLZ'
      ELSE
         PRINT *,'FATNEW. RZ file will be created with options ',CHMODE
         CHOPT = '1DLZ'//CHMODE
      ENDIF
*
*
*     Initialise FATMEN...
*
      LUNRZ  = 1
      LUFZFA = 2
      IFLAG  = 1
*
      FATNOD = ' '
 
      CALL FMLOGL(3)
*
*     Single user mode
*
      PATH = '//' // CHSYS(1:LENOCC(CHSYS)) // '/' //
     +               CHUSER(1:LENOCC(CHUSER))
      LP   = LENOCC(PATH)
      IF(IDEBFA.GE.3) PRINT *,'FATNEW. call FATINI for ',
     +   PATH(1:LP)
      CALL FATINI(IXSTOR,LUNRZ,LUFZFA,
     +            PATH(1:LP),CHOPT)
      CALL FATMDI(PATH(1:LP),LKEYFA,CHFOR,CHTAG)
      CALL FMEND(IRC)
      END
 
