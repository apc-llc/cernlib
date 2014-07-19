*
* $Id: iodev.s,v 1.1.1.1 1996/03/08 15:22:00 mclareni Exp $
*
* $Log: iodev.s,v $
* Revision 1.1.1.1  1996/03/08 15:22:00  mclareni
* Epio
*
*
* ROUTINE CHANGED FOR FTN COMPILER LEVEL 10 EP-PACKAGE
#if defined(CERNLIB_UNIVAC)
 . INTEGER FUNCTION IODEV('FILENAME') RETURNS DEVICE EQUIPMENT CODE.
 . SEE PRM (UP-4144) 2.7-12
 . CODED BY B.S.NILSSON, NBI, COPENHAGEN, AUG 1980.
 . THIS IS A  F T N  VERSION REQUIRING FILENAME OF TYPE CHARACTER
 .   IN ASCII CODE.
 .
          AXR$      .
$(0),PKT  '            '              . FITEM$-PACKET.
          RES       11                .
RSAVE     RES       2                 . SAVE AREA FOR X11 AND # OF CHAR:S
FITA0     +         13,PKT            .
$(1),IODEV* LA,U    A2,PKT            . (PKT) IN A2
          SX        X11,14,A2         . SAVE X11
          LA        A1,A0             . CHECK IF ARGUMENT IS TYPE CHARACTER
          SSL       A1,30             .
          ANA,U     A1,1              .
          JZ        A1,$+3            .
          SN1       A0                .
          J         0,X11             . RETURN
          LA,Q2     A3,1,A0           . OK, # OF CHARACTERS
          SA        A3,13,A2          .
          AA,U      A3,3              .
          DSL       A3,38             .
          LA,H2     A1,0,A0           . ADDRESS OF FILENAME
          LA,Q1     A3,1,A0           . FIND OFFSET
          LA        A0,A4             .
          JZ        A3,OFF            .
          MSI,U     A3,9              .
          DL        A4,0,A1           .
          LDSL      A4,0,A3           .
          SA        A4,2,A2           .
          DL        A4,1,A1           .
          LDSL      A4,0,A3           .
          SA        A4,3,A2           .
          DL        A4,2,A1           .
          LDSL      A4,0,A3           .
          SA        A4,4,A2           .
          LA,U      A1,PKT+2          .
OFF       LXI,U     X11,PIRCB$        .
          LIJ       X11,BASCFD$       . TRANSLATE FILENAME TO FIELDATA
          LA,U      A2,PKT            .
          LA        A1,13,A2          . # OF CHAR:S
          TG,U      A1,7              .
          J         TWO               .
ONE       SFS       1,A2              . ONE WORD
          SFS       A3                .
          LA        A4,0,A2           .
          MSI,U     A1,6              .
          LDSL      A3,0,A1           . SHIFT IN FD BLANKS
          SSC       A3,0,A1           .
          SA        A3,0,A2           .
          J         CONT              .
TWO       SFS       A3                . TWO WORDS
          ANA,U     A1,6              .
          LA        A4,1,A2           .
          MSI,U     A1,6              .
          LDSL      A3,0,A1           . SHIFT IN FD BLANKS
          SSC       A3,0,A1           .
          SA        A3,1,A2           .
CONT      LA        A0,15,A2          . PREPARE A0
          ER        FITEM$            .
          LA,S1     A0,6,A2           . RETURN EQUIPMENT CODE
          LX        X11,14,A2         . RESTORE X11
          J         0,X11             . RETURN
          END                         .
#endif
