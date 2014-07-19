*
* $Id: izscli.F,v 1.2 2005/10/28 12:38:18 couet Exp $
*
* $Log: izscli.F,v $
* Revision 1.2  2005/10/28 12:38:18  couet
* - SVG driver, workstation type -779
*
* Revision 1.1.1.1  1996/02/14 13:11:13  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_PSCRIPT)||defined(CERNLIB_MAIL)
*CMZ :  1.22/05 30/01/95  10.57.22  by  O.Couet
*-- Author :
      SUBROUTINE IZSCLI(ICLSW)
*.===========>
*.
*.   This routine stores in the current NT bank the state of the clipping
*. indicator .
*.
*. _Input parameters:
*.
*.  INTEGER ICLSW : Clipping indicator .
*.
*..==========> (O.Couet)
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
#if defined(CERNLIB_PSCRIPT)
#include "higz/hipost.inc"
#endif
#if defined(CERNLIB_PSCRIPT)
#include "higz/hiatt.inc"
#endif
#if defined(CERNLIB_ZEBRA)
#include "higz/hipaw.inc"
#endif
#if defined(CERNLIB_MAIL)
#include "higz/himail.inc"
#endif
*.______________________________________
*
#if defined(CERNLIB_PSCRIPT)
#include "higz/hiwcps.inc"
#endif
#if defined(CERNLIB_PSCRIPT)
      IF(LOSCLI.OR..NOT.LOZONE)GOTO 20
      IF(PFLAG.AND..NOT.TFLAG.AND..NOT.VFLAG)THEN
         IF(IPSWTY.EQ.-113.AND..NOT.LOBB)GOTO 20
         IF(ICLSW.NE.0)THEN
            IDXNC = IXWCPS(X2W)-IXWCPS(X1W)
            IDYNC = IYWCPS(Y2W)-IYWCPS(Y1W)
            IXNC  = IXWCPS(X1W)
            IYNC  = IYWCPS(Y1W)
            IF(IDXNC .EQ.IPDXC.AND.IDYNC.EQ.IPDYC.AND.
     +         IXNC  .EQ.IPXC .AND.IYNC .EQ.IPYC .AND.
     +         IPCLIP.EQ.ICLSW)GOTO 10
            CALL IPIOUT(IDXNC)
            CALL IPIOUT(IDYNC)
            CALL IPIOUT(IXNC)
            CALL IPIOUT(IYNC)
            CALL IPPSTF(2,' C')
            IPDXC = IDXNC
            IPDYC = IDYNC
            IPXC  = IXNC
            IPYC  = IYNC
         ELSE
            IF(IPCLIP.NE.ICLSW)CALL IPPSTF(3,' NC')
         ENDIF
   10    IPCLIP = ICLSW
      ENDIF
   20 CONTINUE
#endif
#if defined(CERNLIB_MAIL)
*
      IF(MFLAG)THEN
         WRITE (CHMAIL,'(I3,I2)') ICLICO,ICLSW
         CALL IMWRIT(4)
      ENDIF
#endif
#if defined(CERNLIB_ZEBRA)
*
      IF(ZFLAG)THEN
         IF(LPICT.LT.0)RETURN
         CALL IZSCAN(LHNT,ICLICO,ITBPTR-1,-1,IPOS)
         IF(IPOS.NE.0)THEN
            IF(IZGADR(LHNT,IPOS).EQ.ICLSW)RETURN
            IF(IPOS.EQ.ITBPTR-1)THEN
               IQ(LHNT+IPOS)=1000000*ICLICO+ICLSW
               RETURN
            ENDIF
         ENDIF
         IF(IZPUSH(0,0,0,'IZSCLI').NE.0)RETURN
         CALL IZSTCC(ICLICO,ICLSW)
      ENDIF
#endif
*
      END
#endif
