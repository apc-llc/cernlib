*
* $Id: fato2f.F,v 1.1.1.1 1996/03/07 15:17:39 mclareni Exp $
*
* $Log: fato2f.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:39  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      PROGRAM FATO2F
*
*     Main program for ORACLE->FZ conversion (recovery)
*     This program expects to read a group name followed by a list
*     of generic names on LUN=5 (i.e. terminal or stack)
*     Each generic name is processed in turn and the matching entry
*     sent to the server as an FZ file. The generic name(s) may
*     be terminated with a wild-card.
*
#include "fatmen/fatsys.inc"
#include "fatmen/slate.inc"
      CHARACTER*240 GENAM
      CHARACTER*20  CHUSER,CHSYS
      CHARACTER*80  CARD
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
#if defined(CERNLIB_IBMVM)
      CALL VMREXX('F','FATSYS',CHSYS,IRC)
#endif
#if defined(CERNLIB_UNIX)||defined(CERNLIB_VAXVMS)
      CALL GETENVF('FATSYS',CHSYS)
      LDEF = IS(1)
      IRC = 0
      IF(LDEF.EQ.0) IRC = -1
#endif
      IF(IRC.NE.0) THEN
         CHSYS = 'CERN'
#if defined(CERNLIB_IBMVM)
         PRINT *,'FATSRV. REXX variable FATSYS not defined. '
#endif
#if defined(CERNLIB_VAXVMS)
         PRINT *,'FATSRV. symbol FATSYS not defined.'
#endif
#if defined(CERNLIB_UNIX)
         PRINT *,'FATSRV. environmental variable FATSYS not defined. '
#endif
         PRINT *,'defaulted to CERN'
      ENDIF
      CALL CLTOU(CHSYS)
      TOPDIR = '//'//CHSYS
#if defined(CERNLIB_IBMVM)
*
*     Take username from REXX variable 'FATMAN'
*     If not defined, use current username
*     This username determines the FATMEN group for whom we are working...
*
      CALL VMREXX('F','FATMAN',CHUSER,IRC)
#endif
#if defined(CERNLIB_UNIX)||defined(CERNLIB_VAXVMS)
      CALL GETENVF('FATGRP',CHUSER)
      LDEF = IS(1)
      IRC = 0
      IF(LDEF.EQ.0) IRC = -1
#endif
      IF(IRC.NE.0) THEN
         IC = FMUSER(CHUSER)
#if defined(CERNLIB_IBMVM)
         PRINT *,'FATSRV. REXX variable FATMAN not defined. '
#endif
#if defined(CERNLIB_VAXVMS)
         PRINT *,'FATSRV. symbol FATGRP not defined.'
#endif
#if defined(CERNLIB_UNIX)
         PRINT *,'FATSRV. environmental variable FATGRP not defined. '
#endif
         PRINT *,'Using current username'
      ENDIF
      CALL CLTOU(CHUSER)
*
*
*     Initialise FATMEN...
*
      LUNRZ = 1
      LUNFZ = 2
      IFLAG = 0
 
      LTOP  = LENOCC(TOPDIR)
      CALL FMINIT(IXSTOR,LUNRZ,LUNFZ,TOPDIR(1:LTOP)//'/'//
     +            CHUSER(1:LENOCC(CHUSER)),IRC)
      CALL FMLOGI(IRC)
10    CONTINUE
      PRINT *,'Enter generic name'
      READ(5,'(A)',END=99) CARD
      CALL FMOTOZ(CARD(1:LENOCC(CARD)),IFLAG)
      GOTO 10
99    CONTINUE
      CALL FMEND(IRC)
      END
 
