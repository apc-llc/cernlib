*
* $Id: iopwk.F,v 1.4 1998/12/01 15:48:36 couet Exp $
*
* $Log: iopwk.F,v $
* Revision 1.4  1998/12/01 15:48:36  couet
* - Clean up: commented lines of code removed
*
* Revision 1.3  1998/01/28 14:34:45  couet
* - GGDM driver removed
*
* Revision 1.2  1996/09/25 14:58:48  couet
* - GPR driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:56  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.21/00 10/03/94  17.48.13  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IOPWK(IWKID,ICONID,IWTYPI)
      ENTRY      GOPWK(IWKID,ICONID,IWTYPI)
#if defined(CERNLIB_KERNEL)
#include "higz/hikern.inc"
#endif
#if defined(CERNLIB_MSDOS)
#include "higz/himeta.inc"
#endif
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
#if defined(CERNLIB_KERNEL)
      CHARACTER*32 CHWIN
#endif
      DIMENSION RVAL(2)
#if !defined(CERNLIB_X11)
      SAVE IFIRST
#endif
#if defined(CERNLIB_X11)
      INTEGER WINID
#endif
#if (defined(CERNLIB_X11))&&(defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
      CHARACTER*3 CHLUN
#endif
#if (defined(CERNLIB_GL))&&(!defined(CERNLIB_MSDOS))
      INTEGER*2 IHATCH(16,8)
      DATA (IHATCH(I,1),I=1,16)
     +   / X'8888' , X'2222' , X'8888' , X'2222'
     +   , X'8888' , X'2222' , X'8888' , X'2222'
     +   , X'8888' , X'2222' , X'8888' , X'2222'
     +   , X'8888' , X'2222' , X'8888' , X'2222' /
      DATA (IHATCH(I,2),I=1,16)
     +   / X'9999' , X'0000' , X'6666' , X'0000'
     +   , X'9999' , X'0000' , X'6666' , X'0000'
     +   , X'9999' , X'0000' , X'6666' , X'0000'
     +   , X'9999' , X'0000' , X'6666' , X'0000' /
      DATA (IHATCH(I,3),I=1,16)
     +   / X'8181' , X'0000' , X'0000' , X'0000'
     +   , X'1818' , X'0000' , X'0000' , X'0000'
     +   , X'8181' , X'0000' , X'0000' , X'0000'
     +   , X'1818' , X'0000' , X'0000' , X'0000' /
      DATA (IHATCH(I,4),I=1,16)
     +   / X'8001' , X'0000' , X'0000' , X'0000'
     +   , X'0000' , X'0000' , X'0000' , X'0000'
     +   , X'0180' , X'0000' , X'0000' , X'0000'
     +   , X'0000' , X'0000' , X'0000' , X'0000' /
      DATA (IHATCH(I,5),I=1,16)
     +   / X'FFFF' , X'8181' , X'8181' , X'8181'
     +   , X'FFFF' , X'FFFF' , X'1818' , X'1818'
     +   , X'1818' , X'1818' , X'FFFF' , X'FFFF'
     +   , X'8181' , X'8181' , X'8181' , X'FFFF' /
      DATA (IHATCH(I,6),I=1,16)
     +   / X'CCCC' , X'CCCC' , X'3333' , X'3333'
     +   , X'CCCC' , X'CCCC' , X'3333' , X'3333'
     +   , X'CCCC' , X'CCCC' , X'3333' , X'3333'
     +   , X'CCCC' , X'CCCC' , X'3333' , X'3333' /
      DATA (IHATCH(I,7),I=1,16)
     +   / X'F0F0' , X'F0F0' , X'F0F0' , X'F0F0'
     +   , X'0F0F' , X'0F0F' , X'0F0F' , X'0F0F'
     +   , X'F0F0' , X'F0F0' , X'F0F0' , X'F0F0'
     +   , X'0F0F' , X'0F0F' , X'0F0F' , X'0F0F' /
      DATA (IHATCH(I,8),I=1,16)
     +   / X'FF00' , X'FF00' , X'FF00' , X'FF00'
     +   , X'FF00' , X'FF00' , X'FF00' , X'FF00'
     +   , X'00FF' , X'00FF' , X'00FF' , X'00FF'
     +   , X'00FF' , X'00FF' , X'00FF' , X'00FF' /
#endif
#if !defined(CERNLIB_X11)
      DATA IFIRST/1/
#endif
*
      IF(IWKID.EQ.0)THEN
         CALL IGERR('Workstation identifier equal 0','IOPWK')
         RETURN
      ENDIF
      IF(INOPWK.EQ.10)THEN
         CALL IGERR('Maximal number of open workstation is 10','IOPWK')
         RETURN
      ENDIF
      IF(IGIWIN(IWKID).NE.0)THEN
         CALL IGERR('Workstation already open','IOPWK')
         RETURN
      ENDIF
*
      IWTYP=IWTYPI
#if defined(CERNLIB_X11)
      IF(IWTYP.EQ.999)IWTYP=1
#endif
      INOPWK=INOPWK+1
      ILOPWK(INOPWK)=IWKID
      IWTYL(INOPWK)=IWTYP
      ACWKFL(INOPWK)=.FALSE.
      NODRFL(INOPWK)=.TRUE.
#if defined(CERNLIB_MSDOS)
      ISAVE  = IWKNB
      IWKNB  = INOPWK
#endif
#if defined(CERNLIB_PSCRIPT)
*
      IF(IWTYP.LT.0)THEN
         CALL IPINIT(IWKID,ABS(ICONID),IWTYP)
         RETURN
      ENDIF
#endif
#if !defined(CERNLIB_BATCH)
*
      IF(GFLAG)THEN
#endif
#if defined(CERNLIB_FALCO)
         IF(IWTYP.EQ.7878.OR.IWTYP.EQ.7879)THEN
            IWTYPE = IWTYP
            IFONT  = 0
            IPREC  = 2
            IFTPR  = 20
            CALL IFINIT
            CALL IFVWIN(0.,1.,0.,.8)
            IF(IWTYP.EQ.7878)THEN
               CALL IFSWIN(0,128000,0,100000)
            ELSE
               CALL IFSWIN(0,128000,0,101120)
            ENDIF
            RETURN
         ENDIF
#endif
#if (defined(CERNLIB_KERNEL))&&(!defined(CERNLIB_PHIGS))&&(!defined(CERNLIB_MSDOS))
         IF(IWTYP.LE.0.OR.IWTYP.GT.20)THEN
            CALL IGERR('Invalid workstation type','IOPWK')
            INOPWK=INOPWK-1
            RETURN
         ENDIF
#endif
#if defined(CERNLIB_MSDOS)
         IF(IWTYP.LE.0.)THEN
            CALL IGERR('Invalid workstation type','IOPWK')
            INOPWK=INOPWK-1
            RETURN
         ENDIF
         IF(IWTYP.GT.10)IWTYP=IWTYP-10
*
#endif
#if (defined(CERNLIB_KERNEL))&&(!defined(CERNLIB_PHIGS))&&(!defined(CERNLIB_MSDOS))
         CALL IKFILE(IWTYP)
#endif
#if (defined(CERNLIB_KERNEL))&&(!defined(CERNLIB_PHIGS))
*
         WRITE(CHWIN,'(''HIGZ_'',I2.2)') IWKID
#endif
#if (defined(CERNLIB_GL))&&(defined(CERNLIB_SGI))&&(!defined(CERNLIB_MSDOS))
         CALL FOREGR
#endif
#if (defined(CERNLIB_GL))&&(!defined(CERNLIB_MSDOS))
         CALL PREFPO(WINSIZ(1),WINSIZ(1)+WINSIZ(3)-1
     +,              WINSIZ(2),WINSIZ(2)+WINSIZ(4)-1)
         WINID=WINOPE(CHWIN,11)
         IWINID(INOPWK)=WINID
         CALL ICLRWK(IWKID,0)
         CALL WINCON
         CALL CONCAV(.TRUE.)
         IF(IFIRST.NE.0)THEN
            IFIRST=0
            CALL DEFLIN(1,X'F0F0')
            CALL DEFLIN(2,X'8888')
            CALL DEFLIN(3,X'E4E4')
            CALL DEFLIN(4,X'FAFA')
            DO 20 I=1,8
               CALL DEFPAT(I,16,IHATCH(1,I))
   20       CONTINUE
            CALL SETPAT(0)
         ENDIF
#endif
#if defined(CERNLIB_MSDOS)
         CALL IGSG(INOPWK)
         IER = GET_DEVICE_LIMITS(IXX
     +,                          IYY
     +,                          MAX_COLOR(INOPWK))
         XWKSIZ(INOPWK) = IXX
         YWKSIZ(INOPWK) = IYY
         CALL ICLRWK(IWKID,0)
         IF (IFIRST .NE. 0) THEN
            IFIRST = 0
            DASH_PATTERN( 1) =
     +             CHAR(Z'FF')//CHAR(Z'FF')//CHAR(Z'FF')//CHAR(Z'FF')
            DASH_PATTERN( 2) =
     +            CHAR(Z'0F')//CHAR(Z'0F')//CHAR(Z'0F')//CHAR(Z'0F')
            DASH_PATTERN(3) =
     +            CHAR(Z'11')//CHAR(Z'11')//CHAR(Z'11')//CHAR(Z'11')
            DASH_PATTERN(4) =
     +            CHAR(Z'F6')//CHAR(Z'F6')//CHAR(Z'F6')//CHAR(Z'F6')
            DASH_PATTERN( 5) =
     +             CHAR(Z'F0')//CHAR(Z'F0')//CHAR(Z'F0')//CHAR(Z'F0')
            DASH_PATTERN( 6) =
     +             CHAR(Z'00')//CHAR(Z'FF')//CHAR(Z'FF')//CHAR(Z'FF')
            DASH_PATTERN( 7) =
     +             CHAR(Z'FF')//CHAR(Z'00')//CHAR(Z'FF')//CHAR(Z'00')
            DASH_PATTERN( 8) =
     +            CHAR(Z'00')//CHAR(Z'FF')//CHAR(Z'00')//CHAR(Z'FF')
            DASH_PATTERN( 9) =
     +            CHAR(Z'F0')//CHAR(Z'0F')//CHAR(Z'F0')//CHAR(Z'0F')
            DASH_PATTERN(10) =
     +             CHAR(Z'00')//CHAR(Z'00')//CHAR(Z'00')//CHAR(Z'FF')
            DASH_PATTERN(11) =
     +             CHAR(Z'00')//CHAR(Z'00')//CHAR(Z'FF')//CHAR(Z'FF')
            DASH_PATTERN(12) = DASH_PATTERN(2)
            DASH_PATTERN(13) = DASH_PATTERN(4)
            DASH_PATTERN(14) = DASH_PATTERN(3)
            DASH_PATTERN(15) =
     +            CHAR(Z'55')//CHAR(Z'55')//CHAR(Z'55')//CHAR(Z'55')
            DASH_PATTERN(16) =
     +            CHAR(Z'AA')//CHAR(Z'AA')//CHAR(Z'AA')//CHAR(Z'AA')
            MAG_X = 1
            MAG_Y = 1
         ENDIF
         IF(IWKID.EQ.IDMETA)THEN
            LUNMET(INOPWK) = ABS(ICONID)
         ENDIF
         IWKNB = ISAVE
#endif
#if defined(CERNLIB_X11)
         IWINN=INDEX(CHHOST,' ')
         I=IWINN-1
         IF(I.LE.0)I=LENOCC(CHHOST)
         IF(I.NE.0)THEN
#endif
#if (defined(CERNLIB_X11))&&(defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
            CALL VMCMS('GLOBALV SELECT CENV SETL DISPLAY '
     +      //CHHOST(1:I)//':0',IRET)
#endif
#if defined(CERNLIB_X11)
            FX11 = CHHOST
            IF(INDEX(CHHOST,':').EQ.0)THEN
               FX11(I+1:) = ':0.0'
               I          = I+4
            ENDIF
         ELSE
            FX11 = ' '
         ENDIF
         IF(IXOPNDS(I,FX11).LT.0)THEN
            CALL IGERR('Can''t open DISPLAY','IOPWK')
            INOPWK=INOPWK-1
            IWTYPE=0
            RETURN
         ENDIF
         IF(LENOCC(CHHOST).GT.I)THEN
            CHWIN(1:1)='-'
            CHWIN(2:)=CHHOST(IWINN:)
         ENDIF
         IFLAG=0
         WINID=IXOPNWI(WINSIZ(1),WINSIZ(2),WINSIZ(3),WINSIZ(4)
     +,                 LENOCC(CHWIN),CHWIN,IFLAG)
         IF(WINID.LT.0)THEN
            CALL IGERR('Can''t open WINDOW','IOPWK')
            INOPWK=INOPWK-1
            IWTYPE=0
            RETURN
         ENDIF
         CALL IXMINIT(WINID)
         IWINID(INOPWK)=WINID
         CALL IXSETCO(1,0.,0.,0.)
         CALL IXSETFC(1)
         CALL IXSETLC(1)
         CALL IXSETMC(1)
         CALL IXSETTC(1)
         CALL ICLRWK(IWKID,0)
*
         CALL IGQWK(IWKID,'MXDS',RVAL)
         XWKWN=MIN(1.,RVAL(1)/RVAL(2))
         YWKWN=MIN(1.,RVAL(2)/RVAL(1))
         CALL ISWKWN(IWKID,0.,XWKWN,0.,YWKWN)
         CALL ISWKVP(IWKID,0.,RVAL(1),0.,RVAL(2))
         CALL IKUWK(IWKID)
*
#endif
#if defined(CERNLIB_PHIGS)
         CALL POPWK(IWKID,1,IWTYP)
         CALL PXOPEN_PIPE(IWKID)
#endif
#if defined(CERNLIB_MACMPW)
         WINID=IMOPNWI(WINSIZ(1),WINSIZ(2),WINSIZ(3),WINSIZ(4)
     +,                20,'HIGZ Graphics Window')
#endif
#if !defined(CERNLIB_BATCH)
      ENDIF
#endif
*
      END
