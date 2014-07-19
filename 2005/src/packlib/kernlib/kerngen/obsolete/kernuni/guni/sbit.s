*
* $Id: sbit.s,v 1.1.1.1 1996/02/15 17:53:57 mclareni Exp $
*
* $Log: sbit.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:57  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.
 .                                      ADAPTED FROM ORSAY, 1/10/76
.     SUBROUTINE SBIT(A,X,LX)                    *** M 421 ***
          AXR$
$(1)
SBIT*
          LNA       A0,*2,X11 .   -LX
          LA        A5,*1,X11 .   X
          LA        A4,*0,X11 .   A
          LSSC      A5,37,A0 .    THE BIT LX IS RIGHT ADJUSTED
          DSL       A4,1 .
          SSC       A5,36,A0 .
         SA        A5,*1,X11 .
          J         4,X11
          END
#ifdef CERNLIB_TCGEN_SBIT
#undef CERNLIB_TCGEN_SBIT
#endif
