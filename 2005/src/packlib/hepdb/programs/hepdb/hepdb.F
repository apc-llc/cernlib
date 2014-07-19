*
* $Id: hepdb.F,v 1.4 1996/04/12 09:42:30 cernlib Exp $
*
* $Log: hepdb.F,v $
* Revision 1.4  1996/04/12 09:42:30  cernlib
* New handling of title
*
* Revision 1.3  1996/03/29 11:18:23  jamie
* qftitlch
*
* Revision 1.2  1996/03/12 13:09:16  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:27  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      PROGRAM HEPDB
#include "hepdb/cdilun.inc"
#include "hepdb/cduscm.inc"
#include "hepdb/quest.inc"
*
*     HEPDB interactive interface
*
#include "hepdb/nwpaw.inc"
      COMMON /PAWC/  NWPAWC,IXPAWC,IHBOOK,IXHIGZ,IXKU,IFENCE(5)
     +,              LMAIN, IPAW(NWPAW)
      EXTERNAL CDEXIT,CDQUIT
#include "hepdb/qftitlch.inc"
      CHARACTER    VIDQQ*(*)
      PARAMETER   (VIDQQ = '@(#)' //
     + HepdbTitleFortranString
     + // '>')
      CALL CLEFT (VIDQQ,1,0)
*
*     Initialise C environment
*
      CALL INITC
*
*     Initialise Zebra...
*
      CALL MZEBRA(-3)
      CALL MZPAW(NWPAW,' ')
*
*     Link area...
*
      CALL MZLINK(IXPAWC,'/CDULNK/',LINK01,LINK01,LINK01)
*
*     Kuip...
*
      NWORDS = 50000
      CALL KUINIT(NWORDS)
*
*     HEPDB...
*
      CALL CDINIC
*
*     CDF...
*
      CALL CDCDF
*
*     Set prompt...
*
      CALL KIPRMT('HEPDB>')
*
*     Set up the EXIT and QUIT routines
*
      CALL KUEXIT(CDEXIT)
      CALL KUQUIT(CDQUIT)
*
*     Define some FORTRAN units
*
      CALL CDSETU(LUNARR,NLUN,IRC)
*
*     Default loglevel, output unit
*
      LPRTCD = 6
      IDEBCD = 0
*
      LTOPCD = 0
*
*     Automatic execution of LOGON kumacs
*
      CALL KULOGN('HDB','SGULV')
*
*     Pass control to KUIP...
*
#if !defined(CERNLIB_MOTIF)
      CALL KUWHAT
#endif
#if defined(CERNLIB_MOTIF)
      CALL KUWHAM('HEPDB')
#endif
 
#if defined(CERNLIB_VAXVMS)
      CALL SYS$EXIT(%VAL(1))
#endif
*
      END
