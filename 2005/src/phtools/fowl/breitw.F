*
* $Id: breitw.F,v 1.1.1.1 1996/03/22 16:42:46 mclareni Exp $
*
* $Log: breitw.F,v $
* Revision 1.1.1.1  1996/03/22 16:42:46  mclareni
* Phtools
*
*
      FUNCTION BREITW(X,Y,Z)
      WID = Z/2.0
      BREITW = 1.0/((X-Y)**2 + WID**2)
      RETURN
      END
