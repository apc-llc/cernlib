*
* $Id: timex.s,v 1.1.1.1 1996/02/15 17:53:58 mclareni Exp $
*
* $Log: timex.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:58  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.  01/09/76
          AXR$
 .    CALL TIMEX (T)         T = EXECUTION TIME PAST
 .                           STRASSBURG, AUG 1976
$(1)
TIMEX*    L,U       A1,2
          ER        TWAIT$              . WAIT 2 MILLISECONDS
          L         A0,(014,PCT)        . LENGTH AND BUFFER ADDRESS
          ER        PCT$
          LA,XU     A2,155
          LCF       A2,PCT+013
          FM        A3,(2.0*-4)         . TIME = TIME / 5000.
          SA        A3,*0,X11
          J         2,X11               . RETURN
 .
 .    COMMON /SLATE/ PCT(14),DUMM(26)
SLATE*    INFO      2 4
$(4)
PCT       RES       14
DUM       RES       26
          END
#ifdef CERNLIB_TCGEN_TIMEX
#undef CERNLIB_TCGEN_TIMEX
#endif
