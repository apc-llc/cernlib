*
* $Id: itx.F,v 1.3 1998/01/28 14:34:59 couet Exp $
*
* $Log: itx.F,v $
* Revision 1.3  1998/01/28 14:34:59  couet
* - GGDM driver removed
*
* Revision 1.2  1996/09/25 14:59:03  couet
* - GPR driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:59  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.20/03 19/11/93  13.52.59  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE ITX(X,Y,CHARS)
      ENTRY      GTX(X,Y,CHARS)
#if defined(CERNLIB_KERNEL)
#include "higz/hikern.inc"
#endif
#include "higz/hiflag.inc"
#include "higz/hiatt.inc"
      CHARACTER*(*) CHARS
      CHARACTER*1 CHOPT
      LOGICAL ZFSAV,PFSAV
*
#if defined(CERNLIB_KERNEL)
#include "higz/hiwcdc.inc"
#endif
      ILEN = LENOCC(CHARS)
      IF(ILEN.EQ.0)RETURN
      IF((IFONT.EQ.0.AND.IPREC.EQ.2).OR.
     +         (IFONT.LT.0.AND.IFONT.GT.-25.AND.IPREC.EQ.1))THEN
         IF((ITXALH.EQ.0).OR.(ITXALH.EQ.1))CHOPT='L'
         IF(ITXALH.EQ.2)THEN
            CHOPT = 'C'
         ELSEIF(ITXALH.EQ.3)THEN
            CHOPT = 'R'
         ELSE
            CHOPT = 'L'
         ENDIF
         IF((ITXALV.EQ.2).OR.(ITXALV.EQ.1))THEN
            YY = Y-RCHH*COS((3.14159/180.)*RANGLE)
            XX = X+RCHH*SIN((3.14159/180.)*RANGLE)
         ELSEIF(ITXALV.EQ.3)THEN
            YY = Y-(RCHH/2.)*COS((3.14159/180.)*RANGLE)
            XX = X+(RCHH/2.)*SIN((3.14159/180.)*RANGLE)
         ELSE
            YY = Y
            XX = X
         ENDIF
         ZFSAV  = ZFLAG
         ZFLAG  = .FALSE.
         PFSAV  = PFLAG
         PFLAG  = .FALSE.
         GLFLAG = (ZFLAG.OR.PFLAG.OR.MFLAG)
         CALL IGTEXT(XX,YY,CHARS,RCHH,RANGLE,CHOPT)
         ZFLAG  = ZFSAV
         PFLAG  = PFSAV
         GLFLAG = (ZFLAG.OR.PFLAG.OR.MFLAG)
      ELSE
         IF(GFLAG)THEN
#if defined(CERNLIB_PHIGS)
            CALL PTX(X,Y,CHARS)
#endif
#if defined(CERNLIB_KERNEL)||defined(CERNLIB_FALCO)
            IF(ITXALH.EQ.0)ITXALH=1
            DO 10 IWKNB=1,INOPWK
               IF(NODRFL(IWKNB))GOTO 10
               CALL IGSG(IWKNB)
#endif
#if defined(CERNLIB_FALCO)
               IF(IWTYL(IWKNB).EQ.7878.OR.IWTYL(IWKNB).EQ.7879)GOTO 10
#endif
#if defined(CERNLIB_GL)||defined(CERNLIB_MSDOS)
               CALL COLOR(ITXCI+IOFCOL)
               ICURCI(IWKNB)=ITXCI
               ITXW2=STRWID(CHARS,ILEN)/2
               IXX=IXWCDC(X)-(ITXALH-1)*ITXW2
               IYY=IYWCDC(Y)
#endif
#if (defined(CERNLIB_GL))&&(!defined(CERNLIB_MSDOS))
               CALL CMOV2I(IXX,IYY)
               CALL CHARST(CHARS,ILEN)
#endif
#if defined(CERNLIB_MSDOS)
               CALL OUT_TEXT(CHARS(1:ILEN),IXX,IYY,ITXCI+IOFCOL)
#endif
#if defined(CERNLIB_X11)
               CALL IXSETTA(ITXALH,ITXALV)
               IXX=IXWCDC(X)
               IYY=IYWCDC(Y)
               IF(IKFNTX(IWKNB).EQ.0)THEN
                  CALL IXTEXT(0,IXX,IYY,RANGLE,RMAGN,ILEN,CHARS(1:ILEN))
               ENDIF
#endif
#if defined(CERNLIB_KERNEL)||defined(CERNLIB_FALCO)
   10       CONTINUE
#endif
         ENDIF
      ENDIF
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_PSCRIPT)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZTX(X,Y,CHARS(1:ILEN))
#endif
      END
