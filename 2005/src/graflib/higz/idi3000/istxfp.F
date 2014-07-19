*
* $Id: istxfp.F,v 1.1.1.1 1996/02/14 13:10:30 mclareni Exp $
*
* $Log: istxfp.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:30  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.07/01 14/09/89  11.13.34  by  O.Couet
*-- Author :
      SUBROUTINE ISTXFP(JJFONT,JPREC)
*.===========>
*.
*.   Purpose:
*.   --------
*.     Set character font text attribute.
*.
*.   DI-3000 access:
*.   ---------------
*.     CALL JFONT (CVALUE)
*.
*.   Parameters:
*.   -----------
*.     CVALUE                     [ INTEGER; Input ]
*.
*.      - The new value for the character font.
*.
*.        *NOTE* : CVALUE must be in the range 1...32767.
*.            GKS: CFONT  must be in the range 1...30
*.            GKS: CPREC  must be in the range 1...4
*.
*..==========> (O.Couet, H.Johnstad, L.Roberts)
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
#include "higz/di3seg.inc"
      INTEGER CFONT
*.______________________________________
*
      IFONT=JJFONT
      IPREC=JPREC
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IFTPR=10*JJFONT+ISIGN(1,JJFONT)*JPREC
      IF(GLFLAG)CALL IZATT(ITXFCO)
#endif
      IF (JJFONT.LT.0) THEN
         IF (JJFONT.EQ.-1) THEN
            CFONT=1
         ELSEIF (JJFONT.EQ.-2) THEN
            CFONT=3
         ELSEIF (JJFONT.EQ.-3) THEN
            CFONT=5
         ELSEIF (JJFONT.EQ.-4) THEN
            CFONT=7
         ELSEIF (JJFONT.EQ.-5) THEN
            CFONT=6
         ELSEIF (JJFONT.EQ.-6) THEN
            CFONT=8
         ELSEIF (JJFONT.EQ.-101) THEN
            CFONT=2
         ELSEIF (JJFONT.EQ.-102) THEN
            CFONT=4
         ELSEIF (JJFONT.EQ.-103) THEN
            CFONT=6
         ELSEIF (JJFONT.EQ.-104) THEN
            CFONT=8
         ELSEIF (JJFONT.EQ.-105) THEN
            CFONT=6
         ELSEIF (JJFONT.EQ.-106) THEN
            CFONT=8
         ELSEIF (JJFONT.EQ.-107) THEN
            CFONT=11
         ELSEIF (JJFONT.EQ.-108) THEN
            CFONT=12
         ELSEIF (JJFONT.EQ.-109) THEN
            CFONT=15
         ELSEIF (JJFONT.EQ.-110) THEN
            CFONT=17
         ELSEIF (JJFONT.EQ.-201) THEN
            CFONT=1
         ELSEIF (JJFONT.EQ.-202) THEN
            CFONT=3
         ELSEIF (JJFONT.EQ.-203) THEN
            CFONT=5
         ELSEIF (JJFONT.EQ.-204) THEN
            CFONT=7
         ELSE
            CFONT=1
         ENDIF
      ELSEIF ((JJFONT.EQ.0).AND.(JPREC.EQ.2)) THEN
         RETURN
      ELSE
         CFONT=MAX(MOD(JJFONT,32768),1)
      ENDIF
      IF (GFLAG) THEN
         IF (.NOT. SEGOPN) THEN
            CALL JOPEN
            SEGOPN=.TRUE.
         ENDIF
         CALL JFONT (CFONT)
      ENDIF
*
      END
#endif
