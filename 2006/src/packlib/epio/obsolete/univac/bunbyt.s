*
* $Id: bunbyt.s,v 1.1.1.1 1996/03/08 15:21:59 mclareni Exp $
*
* $Log: bunbyt.s,v $
* Revision 1.1.1.1  1996/03/08 15:21:59  mclareni
* Epio
*
*
#if defined(CERNLIB_UNIVAC)
 .        EP UTILITY PROGRAM
 . SUBROUTINE BUNBYT(SOURCE,TARGET,NBYTES,NBITS,NSTART) MERGES THE
 .   RIGHTMOST NBITS BITS OF EACH OF THE NBYTES WORDS OF SOURCE INTO
 .   A CONTINUOUS BITSTRING STARTING AT BIT NSTART IN TARGET.
 . UNIVAC  F T N  VERSION BY B.S.NILSSON, NBI, MAY 1981
 .
          AXR$                        .
$(0),SAVE +         0                 . A6 SAVE AREA
$(1),BUNBYT*                          .
          DL        A1,2,A0           .
          LA        A4,0,A1           . NBYTES
          TLE,U     A4,1              . > 0 ?
          J         0,X11             .
          LA        A5,0,A2           . NBITS
          TG,U      A5,37             . > 36 ?
          J         ERRBUN            .
          TLE,U     A5,1              . < 1 ?
          J         ERRBUN            .
          ANA,U     A4,1              .
          LR        R1,A4             . REPEAT COUNT
          LR        R2,A5             . NBITS
          LA        A1,4,A0           .
          LA        A5,0,A1           . NSTART
          JGD       A5,$+2            . < 1 ?
          J         ERRBUN            .
          SZ        A4                .
          DI,U      A4,36             .
          DL        A0,0,A0           .
          AA        A1,A4             . START WORD OF TARGET
          LXI,U     A0,1              .
          LA,U      A2,SAVE           .
          SA        A6,0,A2           . SAVE A6
          LA        A2,A5             . LEFT/RIGHT SHIFT
          LA        A3,R2             . NBITS
LOOP      DL        A5,0,A1           . LOOP, ONE PASS PER BYTE
          LA        A4,0,*A0          .
          LDSC      A5,0,A2           . SHIFT THE PIECES TOGETHER
          LSSL      A5,0,A3           .
          DSL       A4,0,A3           .
          DSC       A5,0,A2           .
          DS        A5,0,A1           . STORE BACK
          AA        A2,R2             . CALCULATE SHIFTS FOR NEXT BYTE
          TLE,U     A2,37             . NEW WORD ?
          JGD       R1,LOOP           . NO, REPEAT LOOP
          AA,U      A1,1              . YES, UPDATE A1 AND A2
          ANA,U     A2,36             .
          JGD       R1,LOOP           . END OF LOOP
RET       LA,U      A2,SAVE           . RESTORE A6
          LA        A6,0,A2           .
          J         0,X11             . RETURN
          END                         .
#endif
