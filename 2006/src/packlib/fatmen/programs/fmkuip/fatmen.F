*
* $Id: fatmen.F,v 1.3 1996/04/12 07:56:01 cernlib Exp $
*
* $Log: fatmen.F,v $
* Revision 1.3  1996/04/12 07:56:01  cernlib
* new handling of title string
*
* Revision 1.2  1996/03/29 11:30:03  jamie
* qftitlch
*
* Revision 1.1.1.1  1996/03/07 15:17:42  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
*
      PROGRAM FATMEN
*
#include "fatmen/fatsys.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/fatron.inc"
      COMMON /PAWC/ PAW(1000000)
      EXTERNAL FMEXIT,FMQUIT
#include "zebra/quest.inc"
      CHARACTER    VIDQQ*(*)
#include "fatmen/qftitlch.inc"
      PARAMETER   (VIDQQ = '@(#)' //
     + FatmenTitleFortranString
     + // '>')
      CALL CLEFT (VIDQQ,1,0)
*
*     Initialise C
*
      CALL INITC
*
*     Initialise Zebra...
*
      CALL MZEBRA(-3)
      CALL MZPAW(1000000,' ')
*
*     Kuip...
*
      NWORDS = 50000
      CALL KUINIT(NWORDS)
*
*     FATMEN...
*
      LPRTFA = 6
      IDEBFA = 0
      SERNAM = 'INIT'
      FATNOD = ' '
#if (!defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_IBMMVS))
*
*     XZ...
*
      CALL XZINIT(LPRTFA,IDEBFA,3,4,IC)
#endif
*
*     CDF...
*
      CALL FMCDF
#if defined(CERNLIB_PREV93A)
*
*     Was required to allow 'rm' and 'rmdir' etc.
*
      CALL KUEXEC('STYLE -U')
#endif
*
*     Set prompt...
*
      CALL KIPRMT('Type INIT to initialise FATMEN>')
      THRONG = 'NONE'
 
      CALL FMLOGN(IRC)
*
*     If FATMEN.KUMAC not found, use KULOGN
*
      IF(IRC.NE.0) THEN
         PRINT *,'Warning - future versions of FATMEN will not support',
     +      ' FATMEN.KUMAC files'
         PRINT *,'The new scheme executes the following macros: '
         PRINT *,'FATSYS, FATGRP, FATUSR and FATLOCAL'
         PRINT *,'See the FATMEN manual for more information'
      ELSE
         CALL KULOGN('FAT','SGULV')
      ENDIF
*
*     Set up the EXIT and QUIT routines
*
      CALL KUEXIT(FMEXIT)
      CALL KUQUIT(FMQUIT)
*
*     Pass control to KUIP...
*
#if !defined(CERNLIB_MOTIF)
      CALL KUWHAT
#endif
#if defined(CERNLIB_MOTIF)
      CALL KUWHAM('Fatmen')
#endif
 
#if defined(CERNLIB_VAXVMS)
      CALL SYS$EXIT(%VAL(1))
#endif
      END
#if defined(CERNLIB_UNIX)
      SUBROUTINE XYOPEN
*
*     Dummies - not needed for interactive version
*
      ENTRY      XYCLOS
      ENTRY      XYREAD
      ENTRY      XYWRIT
      END
*+
#endif
