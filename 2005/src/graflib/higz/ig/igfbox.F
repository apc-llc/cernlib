*
* $Id: igfbox.F,v 1.1.1.1 1996/02/14 13:10:34 mclareni Exp $
*
* $Log: igfbox.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:34  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.15/00 03/07/92  13.16.33  by  O.Couet
*-- Author :
      SUBROUTINE IGFBOX(X1,X2,Y1,Y2,X3,X4,Y3,Y4)
*.===========>
*.
*.   This routine draws a frame boxe . The frame is drawn according to the
*. current fill area attributes .
*.
*. _Input parameters:
*.
*.  INTEGER X1,Y1 : Left down corner coordinates of the inside boxe
*.                  of the frame .
*.  INTEGER X2,Y2 : Right up corner coordinates of the inside boxe
*.                  of the frame .
*.  INTEGER X3,Y3 : Left down corner coordinates of the outside boxe
*.                  of the frame .
*.  INTEGER X4,Y4 : Right up corner coordinates of the outside boxe
*.                  of the frame .
*.
*..==========> (O.Couet)
#include "higz/hiflag.inc"
#include "higz/hiatt.inc"
      DIMENSION X(12),Y(12)
      INTEGER ILNSAV
      LOGICAL ZFSAV
*.______________________________________
*
      IF(X1 .LE. X2)THEN
         XIN1 = X1
         XIN2 = X2
      ELSE
         XIN1 = X2
         XIN2 = X1
      ENDIF
      IF(X3.LE.X4)THEN
         XIN3 = X3
         XIN4 = X4
      ELSE
         XIN3 = X4
         XIN4 = X3
      ENDIF
      X(1)=XIN1
      X(2)=XIN1
      X(3)=XIN2
      X(4)=XIN2
      X(5)=XIN1
      X(6)=XIN1
      X(7)=XIN3
      X(8)=XIN3
      X(9)=XIN4
      X(10)=XIN4
      X(11)=XIN3
      X(12)=XIN3
*
      Y(1)=(Y1+Y2)/2
      Y(2)=Y2
      Y(3)=Y2
      Y(4)=Y1
      Y(5)=Y1
      Y(6)=Y(1)
      Y(7)=Y(1)
      Y(8)=Y3
      Y(9)=Y3
      Y(10)=Y4
      Y(11)=Y4
      Y(12)=Y(1)
      IF(GFLAG.OR.PFLAG)THEN
         ZFSAV=ZFLAG
         ZFLAG=.FALSE.
         GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
         IF(IFAIS.NE.0)CALL IFA(12,X,Y)
         IF((IFAIS.EQ.0).OR.(IBORD.NE.0))THEN
            ILNSAV=ILN
            CALL ISLN(1)
            CALL IPL(6,X,Y)
            CALL IPL(6,X(7),Y(7))
            CALL ISLN(ILNSAV)
         ENDIF
         ZFLAG=ZFSAV
         GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      ENDIF
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZFBOX(X1,X2,Y1,Y2,X3,X4,Y3,Y4)
#endif
*
      END
