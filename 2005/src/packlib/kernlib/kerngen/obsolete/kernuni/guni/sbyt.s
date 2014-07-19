*
* $Id: sbyt.s,v 1.1.1.1 1996/02/15 17:53:58 mclareni Exp $
*
* $Log: sbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:58  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.
 .                                      ADAPTED FROM ORSAY, 1/10/76
.     SUBROUTINE SBYT(A,X,LX,NBITS)              *** M 421 ***
          AXR$
$(1)
SBYT*
          LNA       A0,*2,X11 .   -LX
          LA        A5,*1,X11 .   X
          LA        A4,*0,X11 .   A
          LA        A2,*3,X11 .   NBITS
          LSSC      A5,37,A0 .    THE BYTE AT LX OF X IS RUGHT ADJUSTED
          AN        A0,A2 .
          DSL       A4,0,A2 .     NEW BYTE IS INTRODUCED AT LEFT
          SSC       A5,37,A0 .
          SA        A5,*1,X11 .   NEW X
          J         5,X11
 END
#ifdef CERNLIB_TCGEN_SBYT
#undef CERNLIB_TCGEN_SBYT
#endif
