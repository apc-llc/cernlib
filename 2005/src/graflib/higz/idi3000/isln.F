*
* $Id: isln.F,v 1.1.1.1 1996/02/14 13:10:29 mclareni Exp $
*
* $Log: isln.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:29  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.07/01 14/09/89  11.10.48  by  O.Couet
*-- Author :
      SUBROUTINE ISLN(LTYPE)
*.===========>
*.
*.   Purpose:
*.   --------
*.     Set the linestyle primitive attribute.
*.
*.   DI-3000 access:
*.   ---------------
*.     CALL JLSTYL (CVALUE)
*.
*.   Parameters:
*.   -----------
*.     CVALUE                     [ INTEGER; Input ]
*.
*.      - The new value for the linestyle.
*.
*.        NOTE : CVALUE must be in the range 0...32767.
*.
*..==========> (O.Couet, H.Johnstad, L.Roberts)
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
#include "higz/di3seg.inc"
      INTEGER VALUE
*.______________________________________
*
      ILN=LTYPE
      IF (LTYPE.EQ.1) THEN
         VALUE = 0
      ELSE IF (LTYPE.EQ.2) THEN
         VALUE = 2
      ELSE IF (LTYPE.EQ.3) THEN
         VALUE = 1
      ELSE IF (LTYPE.EQ.4) THEN
         VALUE = 3
      ELSE IF ((LTYPE.GE.12).AND.(LTYPE.LE.15)) THEN
         VALUE = 0
      ELSE
         VALUE = ABS(MOD(LTYPE,32768))
      ENDIF
      IF (GFLAG) THEN
         IF (.NOT. SEGOPN) THEN
            CALL JOPEN
            SEGOPN=.TRUE.
         ENDIF
         CALL JLSTYL (VALUE)
      ENDIF
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZATT(ILNCO)
#endif
*
      END
#endif
