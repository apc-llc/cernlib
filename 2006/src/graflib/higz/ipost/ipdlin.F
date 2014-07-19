*
* $Id: ipdlin.F,v 1.1.1.1 1996/02/14 13:11:06 mclareni Exp $
*
* $Log: ipdlin.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:06  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.21/01 10/05/94  17.42.25  by  O.Couet
*-- Author :
      SUBROUTINE IPDLIN(NN,IX,IY)
      DIMENSION IX(*),IY(*)
*.===========>
*.
*.   Write the drawing command to the LaTeX metafile
*.   after removing intermediate points lying on a straight line.
*.
*..==========> (A.Nathaniel)
#include "higz/hiflag.inc"

      IF(NN.EQ.0) RETURN

   10 CONTINUE
      DO 30 I=2,NN
         IDX1=IX(I-1)
         IDY1=IY(I-1)
         IDX2=IX(I)
         IDY2=IY(I)
         IF(SIGN(1,IDX1).EQ.SIGN(1,IDX2).AND.SIGN(1,IDY1).EQ.
     +      SIGN(1,IDY2).AND.IDX1*IDY2.EQ.IDX2*IDY1)THEN
            IX(I-1)=IDX1+IDX2
            IY(I-1)=IDY1+IDY2
            NN=NN-1
            DO 20 J=I,NN
               IX(J)=IX(J+1)
               IY(J)=IY(J+1)
   20       CONTINUE
            GOTO 10
         ENDIF
   30 CONTINUE
      DO 40 I=1,NN
         IF(IX(I).EQ.0) THEN
            IF(IY(I).GT.0) THEN
               CALL IPTLIN('U',IY(I),0)
            ELSE
               CALL IPTLIN('D',-IY(I),0)
            ENDIF
         ELSEIF(IY(I).EQ.0) THEN
            IF(IX(I).GT.0) THEN
               CALL IPTLIN('h',IX(I),0)
            ELSE
               CALL IPTLIN('r',-IX(I),0)
            ENDIF
         ELSE
            CALL IPTLIN('s',IX(I),IY(I))
         ENDIF
   40 CONTINUE
      NN=0
      END
