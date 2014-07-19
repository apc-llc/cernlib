*
* $Id: fato2z.F,v 1.1.1.1 1996/03/07 15:17:39 mclareni Exp $
*
* $Log: fato2z.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:39  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      PROGRAM FATO2Z
*
*     Main program for ORACLE->RZ conversion (recovery)
*
      CHARACTER*240 GENAM
      CHARACTER*12  CUSER
      PARAMETER (LURCOR=200000)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)
      DIMENSION    LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
      COMMON /FATUSE/ IDEBFA, IDIVFA, IKDRFA, KOFSFA, KOFUFA, LBFXFA
     +              , LSAVFA, LTOPFA, LBBKFA, LBGNFA, LTDSFA, LBDSFA
     +              , LPRTFA, NTOPFA, LUFZFA, IOUPFA, IOBKFA, IODSFA
#include "fatmen/fatsys.inc"
      CHARACTER*20    CHSYS,CHUSER
*
      COMMON /USRLNK/LUSRK1,LUSRBK,LADDR,LUSRLS
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
      LUNRZ  = 1
      LUFZFA = 2
      IFLAG  = 1
*
*     Single user mode
*
      LTOP   = LENOCC(TOPDIR)
      CALL FATINI(IXSTOR,LUNRZ,LUFZFA,TOPDIR(1:LTOP),'1DLZ')
      CUSER = ' '
      IC = FMUSER(CUSER)
      GENAM = TOPDIR(1:LTOP)//'/'//CHUSER(1:LENOCC(CHUSER)) //'/*'
      LGNAME = LENOCC(GENAM)
      CALL FMLOGI(IRC)
      WRITE(LPRTFA,*) 'Recovering from ',GENAM(1:LGNAME)
      CALL FMOTOZ(GENAM(1:LGNAME),IFLAG)
      CALL RZSTAT(TOPDIR(1:LTOP),20,' ')
      CALL FMEND(IRC)
      END
