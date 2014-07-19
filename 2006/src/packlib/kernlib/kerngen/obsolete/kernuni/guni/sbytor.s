*
* $Id: sbytor.s,v 1.1.1.1 1996/02/15 17:53:58 mclareni Exp $
*
* $Log: sbytor.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:58  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.
 .                                      ADAPTED FROM ORSAY, 1/10/76
.     SUBROUTINE SBYTOR(A,X,LX,NBITS)            *** M 421 ***
          AXR$
$(1)
SBYTOR*
          LNA       A0,*2,X11 .   -LX
          LA        A2,*1,X11 .   X
          LA        A1,*3,X11 .   NBITS
          LSSC      A2,37,A0 .    THE BYTE AT LX IS RIGHT ADJUSTED
          AN        A0,A1 .       -LX-NBITS
          OR        A2,*0,X11
          DSC       A2,0,A1            . INTRODUCE NEW BYTE AT LEFT
          SSC       A2,37,A0
          S         A2,*1,X11
          J         5,X11
          END
#ifdef CERNLIB_TCGEN_SBYTOR
#undef CERNLIB_TCGEN_SBYTOR
#endif
