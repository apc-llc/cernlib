*
* $Id: jbyt.s,v 1.1.1.1 1996/02/15 17:53:57 mclareni Exp $
*
* $Log: jbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:57  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.
 .                                      ADAPTED FROM ORSAY, 1/10/76
.     FUNCTION JBYT(A,LA,NBITS)                  *** M 421 ***
          AXR$
$(1)
JBYT*
          LNMA      A2,*2,X11 .   -NBITS
          ANU       A2,*1,X11          . -LA-NBITS
          LA        A0,*0,X11 .   A
          LSSL      A0,37,A3 .    37-LA-NBITS
          SSL       A0,36,A2 .    36-NBITS
          J         4,X11
 END
#ifdef CERNLIB_TCGEN_JBYT
#undef CERNLIB_TCGEN_JBYT
#endif
