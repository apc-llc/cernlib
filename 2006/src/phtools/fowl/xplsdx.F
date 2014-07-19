*
* $Id: xplsdx.F,v 1.1.1.1 1996/03/22 16:42:47 mclareni Exp $
*
* $Log: xplsdx.F,v $
* Revision 1.1.1.1  1996/03/22 16:42:47  mclareni
* Phtools
*
*
      FUNCTION XPLSDX(X,DX)
      DATA CON/ 0.915 /
      R=0.0
      DO  100  I= 1, 10
C     R = R + RANF(-1)
      R = R + RNDM(DUMMY)
  100 CONTINUE
      R2 = (R-5.0) / CON
      XPLSDX = X + DX*R2
      RETURN
      END
