*
* $Id: fatkeops.F,v 1.1.1.1 1996/03/07 15:17:38 mclareni Exp $
*
* $Log: fatkeops.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:38  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
#if defined(CERNLIB_IBMVM)
      PROGRAM FATKEOPS
*CMZ :          21/02/91  16.24.17  by  Jamie Shiers
*-- Author :    Jamie Shiers   21/02/91
*     Program to move updates between CERNVM and the CHEOPS
*     directory on fatcat
*
      PARAMETER     (NMAX=100)
      CHARACTER*64  FILES(NMAX)
      CHARACTER*8   FATUSR,FATNOD,REMUSR,REMNOD
      CHARACTER*64  REMOTE,REQUST
      CHARACTER*12  CHTIME
      CHARACTER*8   CHUSER,CHPASS
      CHARACTER*8   CHNODE,CHTYPE,CHSYS,CHRAND
      CHARACTER*80  CHMAIL,LINE
      CHARACTER*255 ERRMSG
      COMMON/PAWC/PAW(50000)
      PARAMETER     (IPRINT=6)
      PARAMETER     (IDEBUG=3)
      PARAMETER     (LUNI=1)
      PARAMETER     (LUNO=2)
      INTEGER       FMHOST
#include "zebra/quest.inc"
      COMMON/SLATE/IS(6),IDUMM(34)
      DATA          NENTRY/0/
*
*     Initialise ZEBRA
*
      CALL HLIMIT(50000)
*
*     Initialise XZ
*
      CALL XZINIT(IPRINT,IDEBUG,LUNI,LUNO)
*
      IC = FMHOST(CHNODE,CHTYPE,CHSYS)
      LNODE = LENOCC(CHNODE)
*
*     Open connection to FATCAT...
*
#if defined(CERNLIB_TCPSOCK)
      IDUMMY = CINIT(IDUMMY)
#endif
#if !defined(CERNLIB_TCPSOCK)
      CALL VMREXX('F','USER',CHUSER,IC)
      CALL VMREXX('F','PWD' ,CHPASS,IC)
      CALL CUTOL(CHUSER)
      CALL CUTOL(CHPASS)
      CALL VMSTAK(CHPASS,'L',IC)
      CALL VMSTAK(CHUSER,'L',IC)
#endif
 
      CALL CZOPEN('zserv','FATCAT',IRC)
 
      CALL XZCD('/fatmen/fmcheops',IRC)
 
    1 CALL VMCMS('EXEC FATSERV',IRC)
      IF(IRC.EQ.99) GOTO 1
      IF(IRC.NE.0) THEN
         PRINT *,'FATKEOPS. error ',IRC,' from FATSERV. Stopping...'
         GOTO 99
      ENDIF
 
      NENTRY = NENTRY + 1
*
*     Get the user and node name for this file...
*
      CALL VMCMS('GLOBALV SELECT *EXEC STACK FATADDR',IC)
      CALL VMRTRM(LINE,IEND)
      ISTART = ICFNBL(LINE,1,IEND)
      CALL FMWORD(FATUSR,0,' ',LINE(ISTART:IEND),IC)
      LFAT   = LENOCC(FATUSR)
      CALL FMWORD(FATNOD,1,' ',LINE(ISTART:IEND),IC)
      LNOD   = LENOCC(FATNOD)
 
      PRINT *,'FATKEOPS. Update received from ',FATUSR(1:LFAT), ' at ',
     +         FATNOD(1:LNOD)
 
      CALL DATIME(ID,IT)
      WRITE(CHTIME,'(I6.6,I4.4,I2.2)') ID,IT,IS(6)
      CALL FMRAND(CHRAND,IRC)
*     WRITE(CHRAND,'(I8.8)') MOD(IRNDM(IDUMMY),100000000)
*
*    Now put this file...
*
      REMOTE = ' '
      REMOTE = FATUSR(1:LFAT)//'_'//
     +         FATNOD(1:LNOD)//'.'//CHTIME//CHRAND
      LREM   = LENOCC(REMOTE)
 
      CALL XZPUTA('FATMEN.RDRFILE.A',REMOTE(1:LREM),' ',IC)
      IF(IC.NE.0) THEN
         WRITE(ERRMSG,9001) IC,FATUSR,FATNOD
9001     FORMAT(' FATKEOPS. error ',I6,' sending update from ',
     +            A,' at ',A,' to FATKEOPS')
         LMSG = LENOCC(ERRMSG)
         PRINT *,ERRMSG(1:LMSG)
         CALL VMCMS('EXEC TELL JAMIE '//ERRMSG(1:LMSG),IC)
         CALL VMCMS('EXEC TELL JAMIE Logging off...',IC)
         CALL VMSTAK('LOGOFF','L',IC)
         STOP
      ELSE
*
*     Now rename update
*
         CALL CUTOL(REMOTE(1:LREM))
         REQUST = REMOTE(1:LREM)
         LREQ   = LREM + 4
         REQUST(LREQ-3:LREQ) = '.req'
         CALL XZMV(REMOTE(1:LREM),REQUST(1:LREQ),' ',IRC)
 
      ENDIF
*
*     Delete this update...
*
      CALL VMCMS('ERASE FATMEN RDRFILE A',IC)
*
*     Wait for some action...
*
      GOTO 1
 
   99 CALL CZCLOS(ISTAT)
      END
#endif
