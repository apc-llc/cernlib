*
* $Id: tapeio.s,v 1.1.1.1 1996/03/08 15:22:00 mclareni Exp $
*
* $Log: tapeio.s,v $
* Revision 1.1.1.1  1996/03/08 15:22:00  mclareni
* Epio
*
*
#if defined(CERNLIB_UNIVAC)
 . SUBROUTINE TAPEIO MAKES VARIOUS I/O OPERATIONS ON TAPE.
 . THIS IS A  F T N  VERSION REQUIRING FILENAME IN FIELDATA CODE.
 . CODED BY B.S.NILSSON, NBI, COPENHAGEN, 1979-03-16, REVISED 1981-06-30.
 .
          AXR$                        .
$(0),IOPKT  I$OT     '  ',0  0,0      . I/O PACKET
 .
 . SUBROUTINE TIOWEF('FILENAME',STATUS)         WRITE EOF
$(1),TIOWEF*  LA,U  A3,WEF$           . WRITE EOF CODE
          J         ARG2              .
 .
 . SUBROUTINE TIOREW('FILENAME',STATUS)         REWIND
TIOREW*   LA,U      A3,REW$           . REWIND CODE
ARG2      LA,U      A2,IOPKT          . ADDR(IOPKT)
          SA,S2     A3,3,A2           . STORE I/O CODE ON PACKET
          LMJ       A1,FILENAME       . FILENAME HANDLER
          LA        A0,A2             . SET UP I/O
          ER        IOW$              .
          LNA,S1    A4,3,A2           . STATUS
          LA        A0,1,A3           .
          JNZ       A4,$+2            . EVERYTHING OK ?
          SZ        A4                . YES, SET STATUS TO ZERO
          SA        A4,0,A0           . STORE STATUS
          J         0,X11             . RETURN FROM 2-ARG ENTRIES
 .
 . SUBROUTINE TIOMVB('FILENAME',NBLOCKS,STATUS)  MOVE BLOCK
TIOMVB*   LA,U      A3,MF$            . MOVE BLOCK FORWARD CODE
          LR,U      R1,01             . EOF CODE IN R1
          J         ARG3              .
 .
 . SUBROUTINE TIOMVF('FILENAME',NFILES,STATUS)  MOVE FILE
TIOMVF*   LA,U      A3,FSF$           . MOVE FILE FORWARD CODE
          LR,U      R1,1943           . NOTHING FUNNY IN R1
ARG3      LA,U      A2,IOPKT          . ADDR(IOPKT)
          LA        A1,1,A0           .
          LA        A5,0,A1           . NBLOCKS/NFILES
          JZ        A5,0,X11          . RETURN IF ZERO
          JP        A5,$+2            .
          AA,U      A3,1              . BACKWARDS MOVE
          SA,S2     A3,3,A2           . STORE I/O CODE
          LMA       A5,A5             .
          ANA,U     A5,1              .
          LR        R2,A5             . LOOP COUNT
          LMJ       A1,FILENAME       . FILENAME HANDLER
          LA        A1,R1             .
          LA        A0,A2             . SET UP I/O
LOOP3     ER        IOW$              . MAKE I/O
          TE,S1     A1,3,A2           . EOF FOR MOVE BLOCK ?
          JGD       R2,LOOP3          . END OF LOOP
          LNA,S1    A4,3,A2           . GET STATUS
          LA        A0,2,A3           . ADDR(STATUS)
          JNZ       A4,$+2            .
          SZ        A4                .
          SA        A4,0,A0           . STORE STATUS
          J         0,X11             . RETURN FROM 3-ARG ENTRIES
 .
 . SUBROUTINE TIOGW('FILENAME',ACCESSWDS,NWORDS,STATUS)   GATHER WRITE TAPE
TIOGW*    LA,U      A3,GW$            . GATHER WRITE CODE
          SZ        R1                .
          J         ARG4              .
 .
 . SUBROUTINE TIOW('FILENAME',BUFFERADR,NWORDS,STATUS)   WRITE TAPE
TIOW*     LA,U      A3,W$             . WRITE CODE
          SZ        R1                .
          J         ARG4              .
 .
 . SUBROUTINE TIOSCR('FILENAME',ACCESSWDS,NWORDS,STATUS,AFC)  SCATTER READ
TIOSCR*   LA,U      A3,SCR$           . SCATTER READ CODE
          LR,U      R1,1              .
          J         ARG4              .
 .
 . SUBROUTINE TIOR('FILENAME',BUFFERADR,NWORDS,STATUS,AFC)   READ TAPE
TIOR*     LA,U      A3,R$             . READ CODE
          LR,U      R1,1              .
ARG4      LA,U      A2,IOPKT          . A2=ADDR(IOPKT)
          SA,S2     A3,3,A2           . STORE I/O CODE ON PACKET
          LMJ       A1,FILENAME       . FILENAME HANDLER
          LA        A1,1,A3           . BUFFER ADDRESS
          SA,H2     A1,4,A2           .
          LA        A1,2,A3           .
          LA        A0,0,A1           . # OF WORDS TO TRANSFER
          SA,H1     A0,4,A2           .
          LA        A0,A2             . SET UP I/O
          ER        IOW$              .
          LA        A5,R1             .
          LNA,S1    A4,3,A2           . GET STATUS
          JNZ       A5,READ           . READ ?
          JNZ       A4,$+2            . OK ?
          LA,H2     A4,3,A2           . YES, STATUS=(# OF WORDS)
          LA        A1,3,A3           .
          SA        A4,0,A1           . STORE STATUS
          J         0,X11             . RETURN FROM WRITE
READ      AU,U      A4,04             .
          JZ        A5,AFC            . ABNORMAL FRAME COUNT ?
          LA        A1,4,A3           .
          SZ        0,A1              . SET AFC=0
          JNZ       A4,$+2            . OK ?
          LA,H2     A4,3,A2           . YES, STATUS=(# OF WORDS)
          LA        A1,3,A3           .
          SA        A4,0,A1           . STORE STATUS
          SZ        3,A2              .
          J         0,X11             . RETURN FROM READ, AFC=0
AFC       LA,H2     A4,3,A2           . AFC.NE.0
          LA        A1,3,A3           .
          SA        A4,0,A1           . STORE STATUS
          LA,S3     A4,3,A2           . GET AFC
          LA        A1,4,A3           .
          SA        A4,0,A1           . STORE AFC
          SZ        3,A2              .
          J         0,X11             . RETURN FROM READ, AFC.NE.0
 .
 . HANDLER FOR FILENAME.
 . ASSUMES A2=ADDR(IOPKT), SAVES A0 IN A3 AND STORES FILENAME ON PACKET
 . INTERFACES BY LMJ A1,FILENAME
 .
FILENAME  LA        A3,A0             . SAVE A0 IN A3
          LA        A0,0,A0           .
          SFS       1,A2              . SET BLANKS IN 2ND HALF OF FILENAME
          DL        A4,0,A0           .
          SA        A4,0,A2           . FILENAME, FIRST WORD
          LA,S6     A4,0,A2           . CHECK FOR BLANK IN 6TH CHARACTER
          TNE,U     A4,05             .
          J         CONT              .
          LA        A4,A5             .
          SSL       A4,30             . CHECK IF 7TH CHARACTER BLANK
          TE,U      A4,05             .
          SA        A5,1,A2           . NO, STORE FILENAME SECOND WORD
CONT      J         0,A1              . RETURN TO CALLER
          END                         .
#endif
