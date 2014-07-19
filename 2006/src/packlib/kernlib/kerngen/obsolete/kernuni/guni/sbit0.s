*
* $Id: sbit0.s,v 1.1.1.1 1996/02/15 17:53:57 mclareni Exp $
*
* $Log: sbit0.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:57  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.
 .                                      ADAPTED FROM ORSAY, 1/10/76
.     SUBROUTINE SBIT0(X,LX)                     *** M 421 ***
          AXR$
$(1)
SBIT0*
          LNA       A0,*1,X11 .   -LX
          LA        A5,*0,X11 .   X
          LSSC      A5,37,A0 .    THE BIT LX IS RIGHT ADJUSTED
          SSL       A5,1 .        BIT = ZERO INTRODUCED AT LEFT
          SSC       A5,36,A0 .
          SA        A5,*0,X11 .   NEW X
          J         3,X11
          END
#ifdef CERNLIB_TCGEN_SBIT0
#undef CERNLIB_TCGEN_SBIT0
#endif
