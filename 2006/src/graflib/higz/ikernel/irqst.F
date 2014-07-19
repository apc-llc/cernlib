*
* $Id: irqst.F,v 1.2 1996/09/25 14:58:51 couet Exp $
*
* $Log: irqst.F,v $
* Revision 1.2  1996/09/25 14:58:51  couet
* - GPR driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:56  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.21/09 29/08/94  09.14.25  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IRQST(IWKID,ISTD,IISTAT,L,STR)
      ENTRY      GRQST(IWKID,ISTD,IISTAT,L,STR)
#if defined(CERNLIB_KERNEL)
#include "higz/hikern.inc"
#endif
#include "higz/hiatt.inc"
#include "higz/hiques.inc"
#if defined(CERNLIB_MAIL)
#include "higz/himail.inc"
#endif
#if defined(CERNLIB_MAIL)
#include "higz/hiflag.inc"
#endif
      CHARACTER*(*) STR
*
#if defined(CERNLIB_MSDOS)
      INTEGER BACKSP, RETKEY, LARROW, DELCHR, CTRL_C, ESC
      DATA BACKSP, RETKEY, LARROW, DELCHR, ESC /8, 13, -75, -83, 27/
      DATA CTRL_C /3/
#endif
      IF(IWKID.LT.0)THEN
         IIWKID = ABS(IWKID)
         RXPOS  = RQUEST(81)
         RYPOS  = RQUEST(91)
      ELSE
         IIWKID = IWKID
         RXPOS  = 0.
         RYPOS  = 0.
      ENDIF
*
      IWKIND    = IGIWIN(IIWKID)
      IQUEST(1) = 0
      IF(IWKIND.EQ.0)THEN
         CALL IGERR('Workstation is not open','IRQST')
         RETURN
      ENDIF
      IF(IWTYL(IWKIND).LE.0)THEN
* No locator for Metafile.
         CALL IGERR('Request string is not available','IRQST')
         RETURN
      ENDIF
*
      IISTAT=0
#if defined(CERNLIB_MSDOS)
      ISAVE = IWKNB
      IWKNB = IWKIND
#endif
*
#if defined(CERNLIB_MAIL)
      IF(MFLAG)THEN
         WRITE (CHMAIL,'(3I3)') 556,IIWKID,ISTD
         CALL IMWRIT(1)
         READ (5,'(2I5,A)') IISTAT,L,STR
         CALL IMWRIT(5)
         RETURN
      ENDIF
*
#endif
#if defined(CERNLIB_MACMPW)
      CALL IMACPLO(0,0,-999)
      WRITE (6,'(X,A,$)',ERR=100,END=100) '>'
      READ (5,'(A)',ERR=100,END=100) STR
      L=LENOCC(STR)
      IISTAT=1
100   RETURN
#endif
#if defined(CERNLIB_FALCO)
      IF(IWTYL(IWKIND).EQ.7878.OR.IWTYL(IWKIND).EQ.7879)THEN
         CALL IFMOVE(0.,0.)
         CALL IFPUT(0)
         READ (*,'(A)') STR
         CALL IFPUT(31)
         CALL IFPUT(24)
         CALL IFPUT(0)
         L = LENOCC(STR)
         RETURN
      ENDIF
*
#endif
#if (defined(CERNLIB_GL))&&(!defined(CERNLIB_MSDOS))
      CALL WINSET(IWINID(IWKIND))
      CALL QRESET
      CALL QDEVIC(KEYBD)
      CALL QDEVIC(RETKEY)
      CALL QDEVIC(BACKSP)
      CALL CLKON
      IXPOS=INT(RXPOS)
      L=1
      STR=' '
   20 CALL COLOR(IOFCOL+1)
      ICURCI(IWKIND)=1
      CALL CMOV2I(IXPOS,INT(RYPOS))
      CALL CHARST('_',1)
      IDEV=QREAD(IVAL)
      CALL COLOR(IOFCOL)
      ICURCI(IWKIND)=0
      CALL CMOV2I(IXPOS,INT(RYPOS))
      CALL CHARST('_',1)
      IF(IDEV.EQ.RETKEY)THEN
         CALL UNQDEV(KEYBD)
         CALL UNQDEV(RETKEY)
         CALL UNQDEV(BACKSP)
         CALL CLKOFF
         IISTAT=1
         RETURN
      ENDIF
      IF(IDEV.EQ.BACKSP)THEN
         IF(L.EQ.1)THEN
            CALL RINGBE
            GOTO 20
         ENDIF
         L=L-1
         IXPOS=IXPOS-STRWID(STR(L:L),1)
         CALL CMOV2I(IXPOS,INT(RYPOS))
         CALL CHARST(STR(L:L),1)
         STR(L:L)=' '
         GOTO 20
      ENDIF
      IF(IVAL.LT.32)GOTO 20
      IF(L.GT.LEN(STR))THEN
         CALL RINGBE
         GOTO 20
      ENDIF
      CALL COLOR(IOFCOL+1)
      ICURCI(IWKIND)=1
      CALL CMOV2I(IXPOS,INT(RYPOS))
      CALL CHARST(CHAR(IVAL),1)
      STR(L:L)=CHAR(IVAL)
      IXPOS=IXPOS+STRWID(CHAR(IVAL),1)
      L=L+1
      GOTO 20
*
#endif
#if defined(CERNLIB_MSDOS)
      CALL WINSET(IWINID(IWKIND))
      IHHH  = STRWID('_',1)
      RYPOS = YWKSIZ(IWKIND)-RYPOS
      IXPOS = NINT(RXPOS)
      IYPOS = NINT(RYPOS) - IHHH + 1
      L     = 1
      STR   = ' '
   30 CALL COLOR(IOFCOL+1)
      ICURCI(IWKIND)=1
      IER  = GRAPHIC_TEXT('_',IXPOS,IYPOS,IOFCOL+1)
      IDEV = PAUSE()
      IF (IDEV .EQ. CTRL_C) THEN
         CALL IGEND
         CALL ABEND
      END IF
      IF (IDEV .EQ. LARROW .OR. IDEV .EQ. DELCHR) IDEV = BACKSP
      IF (IDEV .LE. 0)GOTO 30
      CALL COLOR(IOFCOL)
      ICURCI(IWKIND)=0
      IER = GRAPHIC_TEXT('_',IXPOS,IYPOS,IOFCOL)
      IF(IDEV.EQ.RETKEY .OR. IDEV .EQ. ESC)THEN
         IISTAT=1
         IWKNB = ISAVE
         RETURN
      ENDIF
      IF(IDEV.EQ.BACKSP)THEN
         IF(L.EQ.1)THEN
            IER = BEEP()
            IER = BEEP()
            GOTO 30
         ENDIF
         L=L-1
         IXPOS=IXPOS-STRWID(STR(L:L),1)
         IER = GRAPHIC_TEXT(STR(L:L),IXPOS,IYPOS,IOFCOL)
         STR(L:L)=' '
         GOTO 30
      ENDIF
      IF(IDEV.LT.32)GOTO 30
      IF(L.GT.LEN(STR))THEN
         IER = BEEP()
         IER = BEEP()
         GOTO 30
      ENDIF
      CALL COLOR(IOFCOL+1)
      ICURCI(IWKIND)=1
      IER = GRAPHIC_TEXT(CHAR(IDEV),IXPOS,IYPOS,IOFCOL+1)
      STR(L:L)=CHAR(IDEV)
      IXPOS=IXPOS+STRWID(CHAR(IDEV),1)
      L=L+1
      GOTO 30
*
#endif
#if defined(CERNLIB_X11)
      RYPOS  = YWKSIZ(IWKIND)-RYPOS
      IXPOS  = NINT(RXPOS)
      IYPOS  = NINT(RYPOS)
      L      = LEN(STR)
      IISTAT = IXREQST(IXPOS,IYPOS,L,STR)
      L      = LENOCC(STR)
*
#endif
      END
