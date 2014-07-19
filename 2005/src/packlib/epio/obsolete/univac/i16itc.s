*
* $Id: i16itc.s,v 1.1.1.1 1996/03/08 15:22:00 mclareni Exp $
*
* $Log: i16itc.s,v $
* Revision 1.1.1.1  1996/03/08 15:22:00  mclareni
* Epio
*
*
#if defined(CERNLIB_UNIVAC)
 .        EP UTILITY PROGRAM
 . SUBROUTINE I16ITC(A,NW) CONVERTS NW 16 BIT IBM INTEGERS RIGHT ADJUSTED
 .   IN A TO UNIVAC INTEGERS (INCLUDING SIGN).
 . UNIVAC  F T N  VERSION BY B.S.NILSSON, NBI, MAY 1981
 .
          AXR$                        .
$(1),I16ITC* LA     A1,1,A0           . ADDR(NW)
          LR        R1,0,A1           . NW
          JGD       R1,$+2            . NW > 0 ?
          J         0,X11             . NO, RETURN
          LA        A2,0,A0           . ADDR(A)
          LXI,U     A2,1              . INCREMENT
LOOP      LA        A0,0,A2           . GET A WORD IN A
          LSSL      A0,20             . SHIFT IN POSITION WITH SIGN
          SSA       A0,20             .
          JP        A0,$+2            . POSITIVE ?
          ANA,U     A0,1              .
          SA        A0,0,*A2          . STORE BACK
          JGD       R1,LOOP           . REPEAT
          J         0,X11             . RETURN
          END                         .
#endif
