*
* $Id: iztx.F,v 1.2 2004/06/10 07:49:03 couet Exp $
*
* $Log: iztx.F,v $
* Revision 1.2  2004/06/10 07:49:03  couet
* - The text vertical alignment (in PS files) was not correct for rotated
*   text in case of software font.
*
* Revision 1.1.1.1  1996/02/14 13:11:14  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_PSCRIPT)||defined(CERNLIB_MAIL)
*CMZ :  1.22/10 05/04/95  15.39.08  by  O.Couet
*-- Author :
      SUBROUTINE IZTX(XX,YY,CHARS)
*.===========>
*.
*.   This routine stores the text data in the current picture .
*.
*. _Input parameters:
*.
*.  REAL XX,YY : Text position in world coordinates .
*.  CHARACTER CHARS : Text string .
*.
*..==========> (O.Couet)
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
#include "higz/hicode.inc"
#endif
#if defined(CERNLIB_PSCRIPT)
#include "higz/hiatt.inc"
#endif
#include "higz/hiflag.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hipaw.inc"
#endif
#if defined(CERNLIB_MAIL)
#include "higz/himail.inc"
#endif
      CHARACTER*(*) CHARS
#if defined(CERNLIB_PSCRIPT)
      LOGICAL ZSAV,GSAV
      CHARACTER*1 CHOPT
#endif
*.______________________________________
*
      X=XX
      Y=YY
      NCHP=LENOCC(CHARS)
      IF(NCHP.LE.0)RETURN
*
#if defined(CERNLIB_PSCRIPT)
      IF(PFLAG)THEN
         GSAV   = GFLAG
         GFLAG  = .FALSE.
         ZSAV   = ZFLAG
         ZFLAG  = .FALSE.
         GLFLAG = (ZFLAG.OR.PFLAG.OR.MFLAG)
         CALL IPZONE
         CALL IPSCOL(ITXCI)
         IF((IFONT.EQ.0).AND.(IPREC.EQ.2))THEN
            IF(ITXALH.EQ.2)THEN
               CHOPT='C'
            ELSEIF(ITXALH.EQ.3)THEN
               CHOPT='R'
            ELSE
               CHOPT='L'
            ENDIF
            IF((ITXALV.EQ.2).OR.(ITXALV.EQ.1))THEN
               Y = Y-RCHH*COS((3.14159/180.)*RANGLE)
               X = X+RCHH*SIN((3.14159/180.)*RANGLE)
            ELSEIF(ITXALV.EQ.3)THEN
               Y = Y-(RCHH/2.)*COS((3.14159/180.)*RANGLE)
               X = X+(RCHH/2.)*SIN((3.14159/180.)*RANGLE)
            ENDIF
            CALL IGTEXT(X,Y,CHARS,RCHH,RANGLE,CHOPT)
         ELSEIF(TFLAG) THEN
            CALL IPTTEX(XX,YY,CHARS)
         ELSE
            CALL IPTEXT(XX,YY,CHARS)
         ENDIF
         GFLAG  = GSAV
         ZFLAG  = ZSAV
         GLFLAG = (ZFLAG.OR.PFLAG.OR.MFLAG)
      ENDIF
#endif
#if defined(CERNLIB_ZEBRA)
*
      IF(ZFLAG)THEN
         IF(LPICT.LT.0)RETURN
         IF(IZPUSH(3,2,(NCHP+3)/4,'ITX').NE.0)RETURN
         CALL IZCFA(ITXCCO,1)
         CALL IZCFA(ITXACO,1)
         CALL IZCFA(ICHHCO,1)
         CALL IZCFA(IANGCO,1)
         CALL IZCFA(ITXFCO,1)
         CALL IZCFA(IGTSCO,1)
         CALL IZCFA(IGTWCO,1)
*
         CALL IZSTCC(ITXCO,INTPTR)
         IQ(LHI+INTPTR)=IFLPTR
         IQ(LHI+INTPTR+1)=ISTPTR
         IQ(LHI+INTPTR+2)=NCHP
         CALL IZINCI(3)
         Q(LHF+IFLPTR)=X
         Q(LHF+IFLPTR+1)=Y
         CALL IZINCF(2)
         CALL UCTOH(CHARS,IQ(LHC+ISTPTR),4,NCHP)
         CALL IZINCS((NCHP+3)/4)
      ENDIF
#endif
#if defined(CERNLIB_MAIL)
*
      IF(MFLAG)THEN
         WRITE (CHMAIL,'(I3,2E16.7)') ITXCO,X,Y
         CALL IMWRIT(1)
         CHMAIL=' '
         CHMAIL=CHARS
         CALL IMWRIT(3)
      ENDIF
#endif
*
      END
#endif
