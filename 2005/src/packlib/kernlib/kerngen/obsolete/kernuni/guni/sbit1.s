*
* $Id: sbit1.s,v 1.1.1.1 1996/02/15 17:53:58 mclareni Exp $
*
* $Log: sbit1.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:58  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.
 .                                      ADAPTED FROM ORSAY, 1/10/76
.      SUBROUTINE SBIT1(X,LX)                    *** M 421 ***
          AXR$
$(1)
SBIT1*
          LNA       A0,*1,X11 .   -LX
          LA        A5,*0,X11 .   X
          LA,U      A4,1 .
          LSSC      A5,37,A0 .    THE BIT LX IS RIGHT ADJUSTED
          DSL       A4,1 .        BIT = 1 INTRODUCED AT LEFT
          SSC       A5,36,A0
          SA        A5,*0,X11 .   NEW X
          J         3,X11
 END
#ifdef CERNLIB_TCGEN_SBIT1
#undef CERNLIB_TCGEN_SBIT1
#endif
