*
* $Id: iscr.F,v 1.1.1.1 1996/02/14 13:10:29 mclareni Exp $
*
* $Log: iscr.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:29  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.16/08 10/12/92  09.29.34  by  O.Couet
*-- Author :
      SUBROUTINE ISCR(IWKID,ICI,CR,CG,CB)
*.===========>
*.
*.   Purpose:
*.   --------
*.     Set color primitive attribute.
*.
*.   DI-3000 access:
*.   ---------------
*.     CALL JCOLOR (CVALUE)
*.
*.   Parameters:
*.   -----------
*.     CVALUE                     [ INTEGER; Input ]
*.
*.      - The new value for the color index.
*.
*.        NOTE : CVALUE must be in the range 0...32767.
*.
*.          0 - "Normal" for the DEVICE
*.          1 -  Red
*.          2 -  Green
*.          3 -  Yellow
*.          4 -  Blue
*.          5 -  Magenta
*.          6 -  Cyan
*.          7 -  White
*.          8 -  Black
*.          9 -  Complement of the "normal" DEVICE color
*.
*..==========> (O.Couet, H.Johnstad, L.Roberts)
#include "higz/hiflag.inc"
#include "higz/hilut.inc"
      INTEGER CVALUE(1),HUE(1),SAT(1),LIGHT(1)
      INTEGER MAP(0:7)
      DATA MAP / 9, 0, 1, 2, 4, 3, 5, 6 /
*.______________________________________
*
      I=MIN(NBCLUT-1,ABS(ICI))+1
      REDLUT(I)=CR
      GRNLUT(I)=CG
      BLULUT(I)=CB
      CALL IGSG(0)
      IF(GFLAG)THEN
C
C        Perform GKS-->DI3000 mapping for color indices 0-7.
C
         IF((ICI.GE.0).AND.(ICI.LE.7))THEN
            CVALUE(1)=MAP(ICI)
         ELSE
            CVALUE(1)=ICI
         ENDIF
C
C        Transform RGB --> HSL.  Use routine provided by C. O'Reilly.
C
         CALL RGTOHS( CR, CG, CB, HUE, SAT, LIGHT)
         CALL JCOTBL(IWKID,1,CVALUE,HUE,SAT,LIGHT)
      ENDIF
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZSCOR(IWKID,ICI,CR,CG,CB)
#endif
*
      END
#endif
