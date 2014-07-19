*
* $Id: marker.F,v 1.1.1.1 1996/02/14 13:11:03 mclareni Exp $
*
* $Log: marker.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:03  mclareni
* Higz
*
*
#if defined(CERNLIB_MSDOS)
#include "higz/pilot.h"
*CMZ :  1.14/11 29/06/92  13.49.21  by  Unknown
*-- Author : V.Fine
      SUBROUTINE MARKER(N,X,Y)
#include "higz/hiatt.inc"
#include "higz/hikern.inc"
#include "higz/himeta.inc"
      DIMENSION X(N),Y(N)
C--
C--     Corresponing between marker type and it code into centered.sym
C
      DIMENSION IPLMTP(5)
      DATA IPLMTP / 59, 39, 41, 33, 40/
*
#include "higz/hiwcdc.inc"
C
      IF (IMK .EQ. 1) THEN
         DO 10 I=1,N
            IXX = IXWCDC(X(I))
            IYY = IYWCDC(Y(I))
            CALL SETPIX(IXX,IYY)
   10    CONTINUE
      ELSE
         IS = IWKNB
         NTSAV = INTR
         CALL ISELNT(0)
         IWKNB = IS
         ISF = IXWCDC(RMKSC*0.009)
         CALL ISELNT(NTSAV)
         IWKNB = IS
         IF (ILOPWK(IWKNB) .EQ. IDMETA) THEN
            IF (IWTYL(IWKNB) .EQ. ISURF) THEN
               CALL GOLWRI(SS,0,'CENTERED.SYM')
               DO 20 I=1,N
                  COOR(1) = FLOAT(IXWCDC(X(I)))
                  COOR(2) = YWKSIZ(IWKNB)-IYWCDC(Y(I))
                  COOR(3) = FLOAT(ISF)
                  COOR(4) = 0.0
                  IF (IMK .LE. 5) THEN
                     CALL GOLWRI(PS,COOR,CHAR(IPLMTP(IMK)))
                  ELSE
                     CALL GOLWRI(PS,COOR,CHAR(IMK+31))
                  END IF
   20          CONTINUE
               CALL GOLWRI(SS,0,SYMFIL)
            END IF
         ELSE
            ISF = ISF/2
            DO 80 I=1,N
               IXX = IXWCDC(X(I))
               IYY = IYWCDC(Y(I))
               GO TO (30,40,50,60),MOD(IMK-1,4)+1
   30          CALL SBOXI(IXX-ISF,IYY-ISF,IXX+ISF,IYY+ISF)
               GOTO 70
   40          CALL SBOXFI(IXX-ISF,IYY-ISF,IXX+ISF,IYY+ISF)
               GOTO 70
   50          CALL CIRCI(IXX,IYY,ISF)
               GOTO 70
   60          CALL CIRCF(IXX,IYY,ISF)
   70          CONTINUE
   80       CONTINUE
            IXSAVE(IWKNB) = IXX
            IYSAVE(IWKNB) = IYY
         END IF
         IWKNB = IS
      END IF
      END
#endif
