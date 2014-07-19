*
* $Id: blobyt.s,v 1.1.1.1 1996/03/08 15:21:59 mclareni Exp $
*
* $Log: blobyt.s,v $
* Revision 1.1.1.1  1996/03/08 15:21:59  mclareni
* Epio
*
*
#if defined(CERNLIB_UNIVAC)
 .        EP UTILITY PROGRAM
 . SUBROUTINE BLOBYT(SOURCE,TARGET,NBYTES,NBITS,NSTART) CONVERTS THE
 .   NBYTES NBITS/BYTE STRING STARTING IN THE NSTART:TH BIT OF
 .   SOURCE INTO RIGHT-ADJUSTED, ZERO-FILLED FULL WORDS OF TARGET
 .   WITH ONE BYTE PER WORD.
 . UNIVAC  F T N  VERSION BY B.S.NILSSON, NBI, MAY 1981
 .
          AXR$                        .
$(1),BLOBYT*                          .
          DL        A1,2,A0           .
          LA        A4,0,A1           . NBYTES
          JZ        A4,0,X11          . = 0 ?
          JN        A4,0,X11          . < 0 ?
          LA        A5,0,A2           . NBITS
          TG,U      A5,37             . > 36 ?
          J         ERRBLO            . YES
          TLE,U     A5,1              . < 1 ?
          J         ERRBLO            . YES
          ANA,U     A4,1              .
          LR        R1,A4             . REPEAT COUNT
          LR        R2,A5             . NBITS
          LA        A1,4,A0           .
          LA        A5,0,A1           . NSTART
          JGD       A5,$+2            . < 1 ?
          J         ERRBLO            . YES
          SZ        A4                .
          DI,U      A4,36             .
          DL        A0,0,A0           .
          AA        A0,A4             . START SOURCE WORD
          LXI,U     A1,1              . TARGET ADDRESS
          LA        A2,A5             . LEFT SHIFT
          LA,U      A3,36             .
          ANA       A3,R2             . 36-NBITS, I.E. RIGHT SHIFT
BLOW      DL        A4,0,A0           . LOAD A DOUBLE WORD
          LDSL      A4,0,A2           . SHIFT IN POSITION
          SSL       A4,0,A3           .
          SA        A4,0,*A1          . STORE ONE WORD = ONE BYTE
          AA        A2,R2             . CALCULATE SHIFTS FOR NEXT WORD
          TLE,U     A2,37             . IS START IN A NEW WORD ?
          JGD       R1,BLOW           . NO, REPEAT
          AA,U      A0,1              . YES, UPDATE A0 AND A2
          ANA,U     A2,36             .
          JGD       R1,BLOW           . END OF LOOP
          J         0,X11             . RETURN
          END                         .
#endif
