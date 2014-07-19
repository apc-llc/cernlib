*
* $Id: jbytet.s,v 1.1.1.1 1996/02/15 17:53:57 mclareni Exp $
*
* $Log: jbytet.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:57  mclareni
* Kernlib
*
*
.                                       LAST MODIF.  15/01/77
 .                                      ADAPTED FROM ORSAY, 1/10/76
.     FUNCTION JBYTET(A,X,LX,NBITS)              *** M 421 ***
          AXR$
$(1)
JBYTET*
          LNMA      A0,*3,X11 .   -NBITS
          ANU       A0,*2,X11          . -LX-NBITS
          LA        A4,*1,X11 .   X
          LSSL      A4,37,A1           . PUSH LEFT AND RIGHT PART OUT OF A4
          SSL       A4,36,A0 .
          AND       A4,*0,X11 .   A.AND.BYTE(X)
          LA        A0,A5 .
          J         5,X11
 END
#ifdef CERNLIB_TCGEN_JBYTET
#undef CERNLIB_TCGEN_JBYTET
#endif
