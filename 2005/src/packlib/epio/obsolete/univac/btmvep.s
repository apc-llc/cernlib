*
* $Id: btmvep.s,v 1.1.1.1 1996/03/08 15:21:59 mclareni Exp $
*
* $Log: btmvep.s,v $
* Revision 1.1.1.1  1996/03/08 15:21:59  mclareni
* Epio
*
*
#if defined(CERNLIB_UNIVAC)
 .        EP UTILITY PROGRAM
 . SUBROUTINE BTMVEP(IS,ISBIT,IT,ITBIT,NBITS) MOVES A STRING OF NBITS
 .   BITS FROM STRING IS (STARTING ON BIT ISBIT) INTO STRING IT (STARTING
 .   ON BIT ITBIT).
 . UNIVAC  F T N  VERSION BY B.S.NILSSON, NBI, MAY 1981
 .
          AXR$                        .
$(0),SAVE RES       4                 . A-REG SAVE AREA
$(1),BTMVEP*                          .
          LA,U      A2,SAVE           . SAVE A6-A9
          DS        A6,0,A2           .
          DS        A8,2,A2           .
          LA        A3,4,A0           .
          LA        A6,0,A3           . NBITS
          TLE,U     A6,1              . < 1 ?
          J         RET               .
          DL        A2,2,A0           .
          LA        A4,0,A3           . ITBIT
          JGD       A4,$+2            . > 0 ?
          J         RET               .
          DSL       A4,36             .
          DI,U      A4,36             . START WORD # OF IT
          AU        A2,A4             . TARGET ADDR IN A3, REM BIT COUNT IN A5
          DL        A0,0,A0           .
          LA        A2,0,A1           . ISBIT
          JGD       A2,$+2            . > 0 ?
          J         RET               .
          SZ        A1                .
          DI,U      A1,36             . START WORD # OF IS
          AA        A0,A1             . SOURCE ADDR IN A0, REM BIT COUNT IN A2
          LXI,U     A3,1              .
          LA        A7,0,A3           . FIRST WORD OF IT
          LA        A4,A2             .
          DL        A8,0,A0           . FIRST TWO WORDS OF IS
          LA,U      A2,36             .
          ANA       A2,A5             . 36 - # BITS FROM FIRST WORD OF IT
          LA        A1,A4             .
          SSC       A7,0,A2           . SHIFT FIRST WORDS INTO POSITION
          LDSL      A8,0,A1           .
          LA        A1,A2             .
          TG        A1,A6             . MIN(A2,A6)
          LA        A1,A6             .
          LDSL      A7,0,A1           . SHIFT IN BITS
          ANA       A2,A1             .
          JZ        A2,$+2            .
          LSSC      A7,0,A2           . SHIFT IF NOT ON RIGHT BOUNDARY
          SA        A7,0,*A3          . STORE FIRST WORD
          ANA       A6,A1             . # OF BITS LEFT
          JZ        A6,RET            . RETURN IF DONE
          AA        A4,A1             . NEXT PART FROM IS
          TLE,U     A4,37             . NEW WORD ?
          J         $+3               .
          ANA,U     A4,36             .
          AA,U      A0,1              .
          LA        A1,A4             . LEFT SHIFT FOR IS IN LOOP
          LXI,U     A0,1              .
          SZ        A5                .
          DI,U      A5,36             . FIND LOOP COUNT
          JGD       A5,$+2            . ANYTHING TO BE DONE IN LOOP ?
          J         FINAL             .
LOOP      DL        A7,0,*A0          . LOOP FOR TRANSFER OF FULL WORDS
          LDSL      A7,0,A1           .
          SA        A7,0,*A3          .
          JGD       A5,LOOP           .
FINAL     JZ        A6,RET            .
          DL        A8,0,A0           . FIX UP ON LAST WORD
          LA        A7,0,A3           .
          LA        A2,A6             .
          LDSL      A8,0,A1           .
          LDSL      A7,0,A2           .
          SSC       A7,0,A2           .
          SA        A7,0,A3           .
RET       LA,U      A2,SAVE           . RESTORE REGISTERS
          DL        A6,0,A2           .
          DL        A8,2,A2           .
          J         0,X11             . RETURN
          END                         .
#endif
