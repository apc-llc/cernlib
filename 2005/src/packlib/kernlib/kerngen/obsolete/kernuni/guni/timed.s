*
* $Id: timed.s,v 1.1.1.1 1996/02/15 17:53:58 mclareni Exp $
*
* $Log: timed.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:58  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.  01/09/76
          AXR$
 .    CALL TIMED (TD)        TD = TIME - LAST, EXECUTION TIME INTERVAL
 .                           STRASSBURG, AUG 1976
$(1)
TIMED*    L,U       A1,2
          ER        TWAIT$              . WAIT 2 MILLISECONDS
          L         A0,(014,PCT)        . LENGTH AND BUFFER ADDRESS
          ER        PCT$
          LA,XU     A2,155
          LCF       A2,PCT+013
          FM        A3,(2.0*-4)         . TIME = TIME / 5000.
          L         A0,LAST             . TIME-VALUE OF LAST ENTRY
          S         A3,LAST             . SET TIME-VALE OF THIS ENTRY
          FAN       A3,A0               . TD = TIME - LAST
          SA        A3,*0,X11
          J         2,X11               . RETURN
$(0)
LAST      +         0
 .
 .    COMMON /SLATE/ PCT(14),DUMM(26)
SLATE*    INFO      2 4
$(4)
PCT       RES       14
DUM       RES       26
          END
#ifdef CERNLIB_TCGEN_TIMED
#undef CERNLIB_TCGEN_TIMED
#endif
