*
* $Id: cdx_mess.F,v 1.1.1.1 1996/02/28 16:23:39 mclareni Exp $
*
* $Log: cdx_mess.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:39  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDX_MESS(MESSAGE)
*     ----------------------------
*
      CHARACTER*(*) MESSAGE
*
* (LUNINP,LUNOUT,LUNPRO,LUNCMD,...)
#include "cdxluns.inc"
*
      CHARACTER*20  PRONAME
      DATA IFPROTO/0/,IFPROPN/0/
*
      IFTTOUT=1
      GOTO 10
*
      ENTRY CDX_PROT(MESSAGE)
*      ------------------------
      IFTTOUT=0
*
   10 CONTINUE
#if defined(CERNLIB_UNIX)
      IF(IFTTOUT.GT.0) WRITE(LUNOUT,1110) MESSAGE
 1110 FORMAT (A)
#endif
#if !defined(CERNLIB_UNIX)
      IF(IFTTOUT.GT.0) WRITE(LUNOUT,1111) MESSAGE
#endif
      IF(IFPROTO.GT.0) WRITE(LUNPRO,1111) MESSAGE
 1111 FORMAT (1X,A)
      RETURN
*
      ENTRY CDX_SETPR(IFPRO)
*      -----------------------
*
      IF(IFPRO.GT.0) THEN
        IF(IFPROPN.LE.0) THEN
#if !defined(CERNLIB_IBMVM)
          PRONAME='CDEX_PRO'
#endif
#if defined(CERNLIB_IBMVM)
          PRONAME='/CDX_PRO DATA *'
#endif
          OPEN(LUNPRO,FILE=PRONAME,STATUS='UNKNOWN')
          IFPROPN=1
          IFPROTO=1
          WRITE(LUNOUT,*) '=== Protocol OPENED ==='
          WRITE(LUNPRO,*) '=== Protocol OPENED ==='
        ELSE IF(IFPROTO.LE.0) THEN
          IFPROTO=1
          WRITE(LUNOUT,*) '=== Protocol ON ==='
          WRITE(LUNPRO,*) '=== Protocol ON ==='
        ENDIF
      ELSE IF(IFPRO.LT.0) THEN
        IF(IFPROPN.GT.0) THEN
          WRITE(LUNOUT,*) '=== Protocol CLOSED ==='
          IF(IFPROTO.GT.0) WRITE(LUNPRO,*) '=== Protocol CLOSED ==='
          CLOSE(LUNPRO)
        ENDIF
        IFPROPN=0
        IFPROTO=0
      ELSE
        IF(IFPROTO.GT.0) THEN
          WRITE(LUNOUT,*) '=== Protocol OFF ==='
          WRITE(LUNPRO,*) '=== Protocol OFF ==='
        ENDIF
        IFPROTO=0
      ENDIF
      END
*
*
      SUBROUTINE CDX_ANSW(PROM,ANSW)
*     -------------------------====-
*     -- arguments --
      CHARACTER*(*) PROM,ANSW
*
      CALL CDX_MESS(PROM(:LNBLNK(PROM)))
      READ (*,'(A)') ANSW
      CALL CLTOU(ANSW)
      END
