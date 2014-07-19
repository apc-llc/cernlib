*
* $Id: itx.F,v 1.1.1.1 1996/02/14 13:10:30 mclareni Exp $
*
* $Log: itx.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:30  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.19/06 27/08/93  13.43.36  by  O.Couet
*-- Author :
      SUBROUTINE ITX(X,Y,CHARS)
*.===========>
*.
*.   Purpose:
*.   --------
*.     Output a graphics arts quality text string as an output primnitive
*.
*.   DI-3000 access:
*.   ---------------
*.     CALL JHSTRG (STRING)
*.
*.   Parameters:
*.   -----------
*.     STRING                     [ CHARACTER; Input ]
*.
*.      - The literal string to be output as a graphic arts quality text
*.         primitive.
*.
*.      *NOTE* :  The current position is unchanged when JHSTRG returns
*.                to the calling program.
*.
*..==========> (O.Couet, H.Johnstad, L.Roberts)
#include "higz/hiflag.inc"
#include "higz/hiatt.inc"
      CHARACTER*(*) CHARS
      CHARACTER*1 CHOPT
      LOGICAL ZFSAV,PFSAV
#include "higz/di3seg.inc"
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
            IF (.NOT. SEGOPN) THEN
               CALL JOPEN
               SEGOPN=.TRUE.
            ENDIF
            CALL JMOVE (X, Y)
            IF (IPREC.EQ.0) THEN
               CALL J1STRG (CHARS (1:ILEN))
            ELSEIF (IPREC.EQ.1) THEN
               CALL J2STRG (CHARS (1:ILEN))
            ELSE
               CALL JHSTRG (CHARS (1:ILEN))
            ENDIF
         ENDIF
      ENDIF
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_PSCRIPT)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZTX(X,Y,CHARS(1:ILEN))
#endif
*
      END
#endif
