*
* $Id: jbit.s,v 1.1.1.1 1996/02/15 17:53:57 mclareni Exp $
*
* $Log: jbit.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:57  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.
 .                                      ADAPTED FROM ORSAY, 1/10/76
.     FUNCTION JBIT(A,LA)                        *** M 421 ***
          AXR$
$(1)
JBIT*
          LNMA      A2,*1,X11 .   -LA
          LA        A0,*0,X11 .
          LSSL      A0,36,A2 .    36-LA
          SSL       A0,35 .
          J         3,X11
          END
#ifdef CERNLIB_TCGEN_JBIT
#undef CERNLIB_TCGEN_JBIT
#endif
