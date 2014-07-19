*
* $Id: itx.F,v 1.2 1996/06/05 10:21:04 cernlib Exp $
*
* $Log: itx.F,v $
* Revision 1.2  1996/06/05 10:21:04  cernlib
* Move pilot.h before the ifdef for GKS
*
* Revision 1.1.1.1  1996/02/14 13:10:46  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_GKS)
*CMZ :  1.19/14 05/10/93  16.00.03  by  O.Couet
*-- Author :
      SUBROUTINE ITX(X,Y,CHARS)
*.===========>
*.
*.   This routine draws a text at the position X,Y .
*.
*. _Input parameters:
*.
*.  REAL X,Y : Position of the text .
*.  CHARACTER CHARS : Text .
*.
*..==========> (O.Couet)
#include "higz/hiflag.inc"
#include "higz/hiatt.inc"
      CHARACTER*(*) CHARS
      CHARACTER*1 CHOPT
      LOGICAL ZFSAV,PFSAV
*.______________________________________
*
      ILEN=LENOCC(CHARS)
      IF(ILEN.EQ.0)RETURN
      IF(GFLAG)THEN
         CALL IGSG(0)
         IF((IFONT.EQ.0.AND.IPREC.EQ.2).OR.
     +      (IFONT.LT.0.AND.IFONT.GT.-25.AND.IPREC.EQ.1))THEN
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
            CALL GTX(X,Y,CHARS(1:ILEN))
         ENDIF
      ENDIF
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_PSCRIPT)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZTX(X,Y,CHARS(1:ILEN))
#endif
*
      END
#endif
