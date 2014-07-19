*
* $Id: cbyt.s,v 1.1.1.1 1996/02/15 17:53:56 mclareni Exp $
*
* $Log: cbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:56  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.
 .                                      ADAPTED FROM ORSAY, 1/10/76
.     SUBROUTINE CBYT(A,LA,X,LX,NBITS)           *** M 421 ***
          AXR$
$(1)
CBYT*
          LNA       A0,*3,X11 .   -LX
          LNA       A3,*1,X11 .   -LA
          LA        A5,*2,X11 .   X
          LA        A4,*0,X11 .   A
          LA        A2,*4,X11 .   NBITS
          LSSC      A5,37,A0 .    THE BYTE AT LX OF X IS RIGHT ADJUSTED
          LSSC      A4,37,A3 .    THE BYTE AT LA OF A IS RIGHT ADJUSTED
          AN        A0,A2 .       -LX-NBITS
          DSL       A4,0,A2 .     INTRODUCE NEW BYTE AT LEFT
          SSC       A5,37,A0 .
          SA        A5,*2,X11 .   NEW X
          J         6,X11
          END
#ifdef CERNLIB_TCGEN_CBYT
#undef CERNLIB_TCGEN_CBYT
#endif
