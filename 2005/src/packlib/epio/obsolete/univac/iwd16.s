*
* $Id: iwd16.s,v 1.1.1.1 1996/03/08 15:22:00 mclareni Exp $
*
* $Log: iwd16.s,v $
* Revision 1.1.1.1  1996/03/08 15:22:00  mclareni
* Epio
*
*
#if defined(CERNLIB_UNIVAC)
 .        EP UTILITY PROGRAM
 . FUNCTION IWD16(M,N) RETURNS THE N-TH 16 BIT WORD OF STRING M AS A
 .   POSITIVE INTEGER.
 . UNIVAC  F T N  VERSION BY B.S.NILSSON, NBI, MAY 1981
 .
          AXR$                        .
$(1),IWD16* LA      A2,1,A0           . ADDR(N)
          LA        A3,0,A2           . N
          ANA,U     A3,1              . N-1
          LSSL      A3,4              . (N-1)*16
          SZ        A2                .
          DI,U      A2,36             .
          AA        A2,0,A0           . WORD ADDRESS
          DL        A0,0,A2           . LOAD 2 WORDS
          LDSL      A0,0,A3           . SHIFT LEFT
          SSL       A0,20             . SHIFT BACK TO RIGHT BOUNDARY
          J         0,X11             . RETURN
          END                         .
#endif
