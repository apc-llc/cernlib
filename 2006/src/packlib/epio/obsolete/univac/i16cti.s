*
* $Id: i16cti.s,v 1.1.1.1 1996/03/08 15:22:00 mclareni Exp $
*
* $Log: i16cti.s,v $
* Revision 1.1.1.1  1996/03/08 15:22:00  mclareni
* Epio
*
*
#if defined(CERNLIB_UNIVAC)
 .        EP UTILITY PROGRAM
 . SUBROUTINE I16CTI(A,NW) CONVERTS 16 BIT UNIVAC INTEGERS (INCLUDING
 .   SIGN) TO IBM INTEGERS.
 . UNIVAC  F T N  VERSION BY B.S.NILSSON, NBI, MAY 1981
 .
          AXR$                        .
$(1),I16CTI* LA     A1,1,A0           . ADDR(NW)
          LR        R1,0,A1           . NW
          JGD       R1,$+2            . NW > 0 ?
          J         0,X11             . NO, RETURN
          LA        A2,0,A0           . ADDR(A)
          LXI,U     A2,1              . INCREMENT
          LA,U      A5,1              .
          LSSL      A5,20             . 1 BIT FOR CONVERSION OF NEGATIVE NUMBERS
LOOP      LA        A0,0,A2           . GET A WORD IN A
          LSSL      A0,20             . SHIFT TO LEFT OF A0
          JP        A0,$+2            . POSITIVE ?
          AA        A0,A5             . ADD 1 TO LOWEST BIT WHEN NEGATIVE
          SSA       A0,20             . SHIFT INTO POSITION
          SA        A0,0,*A2          . STORE BACK
          JGD       R1,LOOP           . REPEAT
          J         0,X11             . RETURN
          END                         .
#endif
