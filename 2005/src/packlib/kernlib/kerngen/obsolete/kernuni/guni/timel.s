*
* $Id: timel.s,v 1.1.1.1 1996/02/15 17:53:58 mclareni Exp $
*
* $Log: timel.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:58  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.  01/09/76
          AXR$
 .    CALL TIMEL (T)         EXECUTION TIME LEFT
 .                           GENEVA UNIVERSITY, SEPTEMBER 1976
$(1)
TIMEL*    L,U       A1,2
          ER        TWAIT$              . WAIT 2 MILLISECONDS
          L         A0,(014,PCT)        . LENGTH AND BUFFER ADDRESS
          ER        PCT$
          LA        A0,PCT+3            . NT       TIME LIMIT IN SUP'S
          ANA       A0,PCT+013          .    - NU  TIME USED  IN SUP'S
          LA,XU     A2,155
          LCF       A2,A0               . TIME = FLOAT(NT-NU)  SUP'S
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
#ifdef CERNLIB_TCGEN_TIMEL
#undef CERNLIB_TCGEN_TIMEL
#endif
