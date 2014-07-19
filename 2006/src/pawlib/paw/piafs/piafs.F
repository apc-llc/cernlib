*
* $Id: piafs.F,v 1.6 1996/12/10 11:23:12 dinofm Exp $
*
* $Log: piafs.F,v $
* Revision 1.6  1996/12/10 11:23:12  dinofm
* Call a function to check the disk quota (if used).
*
* Revision 1.5  1996/09/25 12:39:32  dinofm
* Old client warning message has been changed
*
* Revision 1.4  1996/09/17 08:37:27  dinofm
* A constant Fortran PARAMETER was passed to PSPAR which in turn relayed it
* to PSLAVE; unfortunately the latter used to manage that parameter as an
* output one, thus assigning a new value to a constant. This behavior
* caused a crash on the CS2 Piaf. The fix is just to assign the PARAMETER
* to an integer variable, then pass the variable instead than the constant.
*
* Revision 1.3  1996/06/12 09:40:47  dinofm
* Allow to change the ntuple cache size according to the configuration file
*
* Revision 1.2  1996/04/24 08:45:32  dinofm
* The PIAF server tag is now correctly set at startup (ISERVERTAG=1).
* A warning message is issued if an old PAW client is detected.
*
* Revision 1.1.1.1  1996/03/01 11:38:50  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :          24/01/96  13.27.09  by  Timo Hakulinen
*-- Author :
#if !defined(CERNLIB_VAX)
      PROGRAM PIAFS
#endif
#if defined(CERNLIB_VAX)
      SUBROUTINE PIAFS(ARGS)
      include '($syidef)'
#endif
********************************************************************
*                                                                  *
*  Remote server for the Parallel Interactive Analysis Facility.   *
*                                                                  *
*  The server is started when the PAW command CONNECT is invoked.  *
*  HISTO/FILE commands for files starting with "//piaf" will open  *
*  the Hbook file on the server and subsequent Ntuple operation    *
*  are performed by the server.                                    *
*                                                                  *
********************************************************************
*
#include "hbook/czsock.inc"
#include "hbook/czbuff.inc"
#include "hbook/hcdire.inc"
#include "hbook/hcpiaf.inc"
#include "paw/pchost.inc"
#include "paw/rzcxio.inc"
#include "paw/pcargs.inc"
#include "paw/pcwk.inc"
#include "paw/quest.inc"
*
      PARAMETER(NWPAW=2000000)
      PARAMETER(ISERVERTAG=1)
      COMMON /PAWC/ PAWCOM(NWPAW)
      COMMON /CDBUF/DBUF(10)

      CHARACTER*80 ARGS
      CHARACTER*80 VSIZE
      INTEGER IRET,MAXSLAVES

      EXTERNAL CZTCP,CZGETA,CZPUTA,FZIN,FZOUT

#if defined(CERNLIB_VAX)
      INTEGER STATUS
      structure /itemlist/
        integer*2 buflen
        integer*2 itemcode
        integer*4 buffadd
        integer*4 retladd
      end structure
      record /itemlist/ item_list(2)
      integer iosb(2)
#endif
#if defined(CERNLIB_HPUX)
      ON REAL UNDERFLOW IGNORE
#endif
*
* Get command line arguments, 1st argument is either "piafserv" or "piafslave"
* 2nd argument is the directory where the config file etc/piaf.conf resides
*
#if !defined(CERNLIB_VAX)
      CALL KGETAR(CHARGS)
      ARGS=CHARGS
      I = INDEX(ARGS,' ')
      CONDIR = ARGS(I+1:LENOCC(ARGS))
      ARGS(I:) = ' '
#endif
*
* ISETUP was done by front-end server;  initialize /CZSOCK/
*
      LUNCZ  = 0
      LBUFCZ = 270
      ISKIN  = 0
      ISKOUT = 0
      IADTCP = JUMPAD(CZTCP)
      IPROT  = 0
      ISLVID = 0
*
      CALL PFINIT
*
#if !defined(CERNLIB_VAX)
      CALL PSHOST(CHHOST)
#endif
#if defined(CERNLIB_VAX)
      item_list(1).buflen   = 15
      item_list(1).itemcode = syi$_nodename
      item_list(1).buffadd  = %loc(chhost)
      item_list(1).retladd  = %loc(lchhost)
      item_list(2).buflen   = 0
      item_list(2).itemcode = 0
      status = sys$getsyi(,,,item_list,iosb,,)
      if(.not.iosb(1)) chhost = '?????? '
      chhost(lchhost+1:) = ' '
      if(index(args,'piafserv').ne.0) then
         args = 'piafserv'
      else
         args = 'piafslave'
      endif
      condir = 'PIAF_DIR '
      CALL VMSSOCK(ISKIN,ISKOUT)
#endif
*
* Tell client that the startup went fine
*
      IF (ARGS .EQ. 'piafserv' .or. ARGS.eq.'PIAFSERV') THEN
         CHSMPF = 'OK: **** Welcome to the Piaf/QP server @ '//
     +            CHHOST(1:LENOCC(CHHOST))//' ****'
         MASTER = 1
         MASTPF = .TRUE.
         CALL GETPIDF(MASPID)
*1        if (mastpf) then
*            i = 1
*            goto 1
*         endif
      ELSE
         CHSMPF = 'OK: **** Piaf/QP slave @ '//
     +            CHHOST(1:LENOCC(CHHOST))//' started ****'
         MASTER = 0
         SLAVPF = .TRUE.
*2        if (slavpf) then
*            i = 1
*            goto 2
*         endif
      ENDIF
      CALL CZPUTA(CHSMPF,ISTAT)
*
* Get the user's name on the client side which we use as name for
* the working directory, get the version which is used when
* starting the slaves, get the workstation type and the protocol level
*
      CALL CZGETA(CHSMPF,ISTAT)
      IF (MASTPF) THEN
         READ(CHSMPF,'(2A16,2I12)') CHUSER, CHVERS, IWK, ISPROTO
         PIAFRC = ' '
         IF (ISPROTO .GE. 1) THEN
*
* Read the userpass information necessary to start the slave servers
*
            CALL CZGETA(CHSMPF,ISTAT)
            READ(CHSMPF,'(A64)') PIAFRC
         ENDIF
      ELSE
         READ(CHSMPF,'(2A16,4I12)') CHUSER, CHVERS, IWK, ISPROTO,
     +                              MASPID, ISLVID
      ENDIF
*
* Divert output socket from stdout
*
      CALL PSDVRT(ISKOUT, MASTER, CHBUF, CHUSER, CHVERS, CONDIR,
     +            ISPROTO, IWK, ISLVID)
*
* Initialize packages
*
      CALL PAWINT2(NWPAW,'P',0,IWTYP)
      CALL PAWINT3
      CALL KIPIAF(CZGETA,CZPUTA,FZIN,FZOUT)
      CALL PZCXIO(RZXIN,RZXOUT)
      CALL PSADDR(NSLAVE, NSLVPF, SLINPF(1), SLHOPF(1))
*
* Enter the socket connection into CHTOP
*
      NCHTOP=NCHTOP+1
      ICHTOP(NCHTOP)=CLINPF+CLUTPF*10000
      ICHLUN(NCHTOP)=0
*
* Open FZ buffers; set LBUF and IADTCP in /CZSOCK/
*
      CALL FZFILE(998,LBUFCZ,'SOC')
      CALL FZFILE(999,LBUFCZ,'SIOC')
      CALL FZHOOK(998,CZTCP,DBUF)
      CALL FZHOOK(999,CZTCP,DBUF)
*
* If no logins on PIAF are allowed ISLVID = -1
*
      IF (MASTPF .AND. ISLVID.EQ.-1) THEN
         IQUEST(1) = 1
         CALL PSPLOG(IQUEST(1), ISTAT)
         CALL PSEXIT(CLINPF)
         GOTO 999
      ENDIF

*
* Init PIAF version
*
      CALL SETPIAFVER(ISERVERTAG)

*
* Adjust PAW's VMEM size if required
*
      CALL VMSIZE (VSIZE,IRET)
      IF (IRET.NE.0) THEN 
         CALL KUEXEC('VMEM '// VSIZE)
      ENDIF

*
* Start slave servers
*
      MAXSLAVES = MXSLPF
      CALL PSPAR(MAXSLAVES, 0)

*
* Send motd message to client
*
      CALL PSPLOG(0, ISTAT)

      IF (MASTPF) THEN
         CALL CQUOTA
         IF (ISPROTO .LT. 2) THEN
*
* Send a warning
*
            PRINT *,' '
            PRINT *, 'PIAF warning: old PAW client detected:'
            PRINT *, 'some graphics side effect may occur.' 
            PRINT *,' '
         ENDIF
      ENDIF

*
* Main loop on client messages
*
      CALL PSMAIN(CLINPF,CLUTPF)
*
      CALL PSEXIT(CLINPF)
*
* Force loading of dummy definitions
*
      CALL PSDUMM
      CALL PAEXIT
*
 999  CONTINUE
      END











