*
* $Id: fmvers.F,v 1.5 1996/04/12 07:16:31 cernlib Exp $
*
* $Log: fmvers.F,v $
* Revision 1.5  1996/04/12 07:16:31  cernlib
* new handling of qftitle
*
* Revision 1.4  1996/03/29 11:29:57  jamie
* qftitlch
*
* Revision 1.3  1996/03/29 10:56:06  jamie
* print 'title' more like good-old patchy
*
* Revision 1.2  1996/03/28 10:28:59  jamie
* update idatqq/itimqq and remove check on old version in fminit
*
* Revision 1.1.1.1  1996/03/07 15:18:11  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMVERS
#include "fatmen/fatapol3.inc"
#include "fatmen/fatclio.inc"
#include "fatmen/fatmss.inc"
#include "fatmen/fatpara.inc"
#include "zebra/quest.inc"
#if !defined(CERNLIB_CRAY)
      PARAMETER (IQCHAW=4)
#endif
#if defined(CERNLIB_CRAY)
      PARAMETER (IQCHAW=8)
#endif
#include "fatmen/qftitlch.inc"
      CHARACTER    VIDQQ*(*)
      PARAMETER   (VIDQQ = '@(#)' //
     + FatmenTitleFortranString
     + // '>')
      CALL CLEFT (VIDQQ,1,0)
 
      IDATQQ = 960329
      ITIMQQ = 1200
         PRINT *,
     + FatmenTitleFortranString
         PRINT *,'         This version created on ',IDATQQ,
     +           ' at ',ITIMQQ
         PRINT *
         PRINT *,'Main flags selected:'
#if defined(CERNLIB_CERN)
         PRINT *,'   CERN'
         PRINT *,'      to activate CERN specific features'
         PRINT *,'      (e.g. list of accessible media types)'
#endif
#if defined(CERNLIB_CSPACK)
         PRINT *,'   CSPACK'
         PRINT *,'      for CSPACK access to remote catalogue and data'
#endif
#if defined(CERNLIB_IBMVM)
         PRINT *,'   IBMVM'
#endif
#if defined(CERNLIB_IBMMVS)
         PRINT *,'   IBMMVS'
#endif
#if defined(CERNLIB_VAXVMS)
         PRINT *,'   VAXVMS'
#endif
#if defined(CERNLIB_UNIX)
         PRINT *,'   UNIX'
#endif
#if defined(CERNLIB_SHIFT)
         PRINT *,'   SHIFT'
         PRINT *,'      interface to SHIFT software'
#endif
#if defined(CERNLIB_VAXTAP)
         PRINT *,'   VAXTAP'
         PRINT *,'      use VAXTAP package for tape handling'
#endif
#if defined(CERNLIB_HEPVM)
         PRINT *,'   HEPVM'
         PRINT *,'      use HEPVM STAGE and SETUP for tape handling'
#endif
#if defined(CERNLIB_TMS)
         PRINT *,'   TMS'
         PRINT *,'      interface to HEPVM Tape Management System'
#endif
#if defined(CERNLIB_NOSMCF)
         PRINT *,'   NOSMCF'
         PRINT *,'      no access to SMCF robot'
#endif
#if defined(CERNLIB_PREFIX)
         PRINT *,'   PREFIX'
         PRINT *,'      provide VID prefix support'
#endif
#if defined(CERNLIB_VMBATCH)
         PRINT *,'   VMBATCH'
         PRINT *,'      interface to VMBATCH system'
#endif
#if defined(CERNLIB_VMTAPE)
         PRINT *,'   VMTAPE'
         PRINT *,'      interface to VMTAPE system'
#endif
#if defined(CERNLIB_NEEDFILE)
         PRINT *,'   NEEDFILE'
         PRINT *,'      interface to NEEDFILE command'
#endif
#if defined(CERNLIB_APOLLO)
         IF(IAPOL3.NE.0)  THEN
            PRINT *,'   L3STAGE (run-time option)'
            PRINT *,'      interface to L3STAGE command'
         ENDIF
#endif
#if defined(CERNLIB_UNIX)
         IF(ICLIO.NE.0)  THEN
            PRINT *,'   VMSTAGE (run-time option)'
            PRINT *,'      interface to VMSTAGE command via CLIO'
         ENDIF
         IF(IFMMSS.NE.0)  THEN
            PRINT *,'   MSS (run-time option)'
            PRINT *,'      interface to "mssget" (Lachman OSM etc.)'
         ENDIF
#endif
      END
 
