*
* $Id: igqwk.F,v 1.3 1998/01/28 14:34:22 couet Exp $
*
* $Log: igqwk.F,v $
* Revision 1.3  1998/01/28 14:34:22  couet
* - GGDM driver removed
*
* Revision 1.2  1996/09/25 14:57:20  couet
* - GPR driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:37  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.22/08 24/02/95  17.03.49  by  O.Couet
*-- Author :
      SUBROUTINE IGQWK(IWKID,PNAME,RVAL)
*.===========>
*.
*.   This routine returns in VAL the values of the parameters
*. asked in PNAME.
*.
*. _Input parameters:
*.
*.  INTEGER IWKID   : Workstation identifier.
*.  CHARACTER PNAME : Parameter name .
*.
*. _Output parameters:
*.
*.  REAL RVAL : Parameter value .
*.
*. +-------+---------------------------------------+-------------+
*. | PNAME |            RVAL Description           | RVAL Lenght |
*. +-------+---------------------------------------+-------------+
*. |'MXDS' |  Maximal display surface (XMAX YMAX)  |      2      |
*. +-------+---------------------------------------+-------------+
*. |'NTNB' |  Current NT number (NT)               |      1      |
*. +-------+---------------------------------------+-------------+
*. |'NTWN' |  Current window parameter             |      4      |
*. +-------+---------------------------------------+-------------+
*. |'NTVP' |  Current viewport parameter           |      4      |
*. +-------+---------------------------------------+-------------+
*. |'DVOL' |  Display volume in 3D  (obsolete)     |      3      |
*. +-------+---------------------------------------+-------------+
*. |'ACTI' |  1. if IWKID is active 0. if not      |      1      |
*. +-------+---------------------------------------+-------------+
*. |'OPEN' |  1. if IWKID is open 0. if not        |      1      |
*. +-------+---------------------------------------+-------------+
*. |'NBWK' |  Number and list of open workstations |     11      |
*. +-------+---------------------------------------+-------------+
*. |'2BUF' |  Double buffer value (X11 only)       |      1      |
*. +-------+---------------------------------------+-------------+
*. |'HWCO' |  Number of COlours suported by the    |      1      |
*. |       |  HardWare. If 2 it is a B&W screen.   |             |
*. +-------+---------------------------------------+-------------+
*. |'WIID' |  Return the window identifier associa-|      1      |
*. |       |  -ted to the workstation ID IWKID.    |             |
*. +-------+---------------------------------------+-------------+
*.
*..==========> (O.Couet)
#if defined(CERNLIB_KERNEL)
#include "higz/hikern.inc"
#endif
#if defined(CERNLIB_KERNEL)||defined(CERNLIB_FALCO)
#include "higz/hiques.inc"
#endif
#if defined(CERNLIB_PSCRIPT)
#include "higz/hipost.inc"
#endif
#include "higz/hiatt.inc"
#if defined(CERNLIB_MSDOS)
#include "higz/himeta.inc"
#endif
      CHARACTER*(*) PNAME
      DIMENSION RVAL(*)
#if defined(CERNLIB_DECGKS)
      REAL*4 RDUMMY(4),VIEW(4)
#endif
*.______________________________________
*
      IF(PNAME(1:4).EQ.'MXDS')THEN
#if defined(CERNLIB_KERNEL)||defined(CERNLIB_BATCH)
         RMDSX=1.
         RMDSY=1.
#endif
#if defined(CERNLIB_KERNEL)||defined(CERNLIB_FALCO)
         IF(INOPWK.EQ.0)RETURN
         I=IGIWIN(IWKID)
         IF(I.EQ.0)THEN
            IQUEST(1)=1
            GOTO 20
         ENDIF
         IDTY=IGIWTY(IWKID)
#endif
#if defined(CERNLIB_PSCRIPT)
         IF(IDTY.LT.0)THEN
            IF(LOPRNG)THEN
               RVAL(1)=XPSR
               RVAL(2)=YPSR
            ELSE
               RVAL(1)=XWKSIZ(IPSI)
               RVAL(2)=YWKSIZ(IPSI)
            ENDIF
            RETURN
         ENDIF
#endif
#if (defined(CERNLIB_GL))&&(!defined(CERNLIB_MSDOS))
         CALL IGSG(I)
         CALL GETORI(IQUEST(10),IQUEST(11))
         CALL GETSIZ(IXX,IYY)
         CALL RESHAP
         CALL ORTHO2(0.,FLOAT(IXX-1),0.,FLOAT(IYY-1))
         XWKSIZ(I)=FLOAT(IXX)
         YWKSIZ(I)=FLOAT(IYY)
#endif
#if defined(CERNLIB_MSDOS)
         CALL IGSG(I)
         IF(ILOPWK(I).NE.IDMETA)THEN
         ELSE
            XWKSIZ(I)=10000.0
            YWKSIZ(I)=10000.0
         ENDIF
#endif
#if defined(CERNLIB_FALCO)
         ITY = IGIWTY(IWKID)
         IF(ITY.EQ.7878.OR.ITY.EQ.7879)THEN
            IQUEST(10) = 128000
            XWKSIZ(I)  = 128000.
            IF(ITY.EQ.7878)THEN
               IQUEST(11) = 100000
               YWKSIZ(I)  = 100000.
            ELSE
               IQUEST(11) = 101120
               YWKSIZ(I)  = 101120.
            ENDIF
            GOTO 10
         ENDIF
#endif
#if defined(CERNLIB_X11)
         CALL IXGETGE(IWINID(I),WINSIZ(1),WINSIZ(2),WINSIZ(3),WINSIZ(4))
         IQUEST(10)=WINSIZ(1)
         IQUEST(11)=WINSIZ(2)
         XWKSIZ(I)=FLOAT(WINSIZ(3))
         YWKSIZ(I)=FLOAT(WINSIZ(4))
#endif
#if defined(CERNLIB_MACMPW)
         CALL IMGETGE(IWINID(I),WINSIZ(1),WINSIZ(2),WINSIZ(3),WINSIZ(4))
         IQUEST(10)=WINSIZ(1)
         IQUEST(11)=WINSIZ(2)
         XWKSIZ(I)=FLOAT(WINSIZ(3))
         YWKSIZ(I)=FLOAT(WINSIZ(4))
#endif
#if defined(CERNLIB_PHIGS)
         CALL PQDSP(IGIWTY(IWKID),IERR,IDC,RMDSX,RMDSY,ILX,ILY)
#endif
#if defined(CERNLIB_KERNEL)||defined(CERNLIB_FALCO)
   10    CONTINUE
         RMDSX=XWKSIZ(I)
         RMDSY=YWKSIZ(I)
         WKMAX(I)=MAX(RMDSX,RMDSY)
         CALL IGSRAP(REDIT)
   20    CONTINUE
#endif
#if defined(CERNLIB_MGKS)||defined(CERNLIB_PLOT10GKS)||defined(CERNLIB_UNIGKS)
         IERR=0
         CALL GQMDS(IGIWTY(IWKID),IERR,IDC,RMDSX,RMDSY,ILX,ILY)
#endif
#if defined(CERNLIB_GKSGRAL)||defined(CERNLIB_ATCGKS)||defined(CERNLIB_GKSBX)
         CALL GQDSP(IGIWTY(IWKID),IERR,IDC,RMDSX,RMDSY,ILX,ILY)
#endif
#if defined(CERNLIB_DECGKS)
         IF(MOD(IGIWTY(IWKID),256).LT.200)THEN
            CALL GQDSP(IGIWTY(IWKID),IERR,IDC,RMDSX,RMDSY,ILX,ILY)
         ELSE
            CALL GQWKT(1,IERR,ISTATE,RDUMMY,RDUMMY,VIEW,RDUMMY)
            RMDSX=VIEW(2)-VIEW(1)
            RMDSY=VIEW(4)-VIEW(3)
         ENDIF
#endif
#if defined(CERNLIB_UNIGKS)
         IF (IERR.EQ.31) THEN
            RMDSX=1.
            RMDSY=1.
         ENDIF
#endif
         RVAL(1)=RMDSX
         RVAL(2)=RMDSY
         RETURN
      ENDIF
*
      IF(PNAME(1:4).EQ.'NTNB')THEN
         RVAL(1)=FLOAT(INTR)
         RETURN
      ENDIF
*
      IF(PNAME(1:4).EQ.'NTWN')THEN
         RVAL(1)=RWXMIN
         RVAL(2)=RWXMAX
         RVAL(3)=RWYMIN
         RVAL(4)=RWYMAX
         RETURN
      ENDIF
*
      IF(PNAME(1:4).EQ.'NTVP')THEN
         RVAL(1)=RVXMIN
         RVAL(2)=RVXMAX
         RVAL(3)=RVYMIN
         RVAL(4)=RVYMAX
         RETURN
      ENDIF
*
      IF(PNAME(1:4).EQ.'DVOL')THEN
         CALL IGERR('Obsolete option','IGQWK')
         RETURN
      ENDIF
*
      IF(PNAME(1:4).EQ.'ACTI')THEN
         RVAL(1)=0.
         DO 30 I=1,INOPWK
            IF(IWKID.EQ.ILOPWK(I))THEN
               IF(ACWKFL(I))RVAL(1)=1.
               RETURN
            ENDIF
   30    CONTINUE
         RETURN
      ENDIF
*
      IF(PNAME(1:4).EQ.'OPEN')THEN
         RVAL(1)=0.
         DO 40 I=1,INOPWK
            IF(IWKID.EQ.ILOPWK(I))THEN
               RVAL(1)=1.
               RETURN
            ENDIF
   40    CONTINUE
         RETURN
      ENDIF
*
      IF(PNAME(1:4).EQ.'NBWK')THEN
         RVAL(1)=FLOAT(INOPWK)
         DO 50 I=1,INOPWK
            RVAL(I+1)=FLOAT(ILOPWK(I))
   50    CONTINUE
         RETURN
      ENDIF
*
      IF(PNAME(1:4).EQ.'2BUF')THEN
#if defined(CERNLIB_X11)
         IWIN=IWINID(IGIWIN(IWKID))
         IVAL=IXQ2BUF(IWIN)
         RVAL(1)=FLOAT(IVAL)
#endif
#if !defined(CERNLIB_X11)
         RVAL(1)=0.
#endif
         RETURN
      ENDIF
*
      IF(PNAME(1:4).EQ.'HWCO')THEN
         RVAL(1) = 8.
#if defined(CERNLIB_FALCO)
         ITY     = IGIWTY(IWKID)
         IF(ITY.EQ.7878.OR.ITY.EQ.7879)THEN
            RVAL(1) = 2.
            RETURN
         ENDIF
#endif
#if defined(CERNLIB_X11)
         CALL IXGETPL(NPLAN)
         RVAL(1) = 2.**FLOAT(NPLAN)
#endif
         RETURN
      ENDIF
*
      IF(PNAME(1:4).EQ.'WIID')THEN
         RVAL(1) = FLOAT(IWINID(IGIWIN(IWKID)))
         RETURN
      ENDIF
*
      CALL IGERR('Unavailable option','IGQWK')
*
      END
