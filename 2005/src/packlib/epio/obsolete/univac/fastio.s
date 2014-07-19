*
* $Id: fastio.s,v 1.1.1.1 1996/03/08 15:22:00 mclareni Exp $
*
* $Log: fastio.s,v $
* Revision 1.1.1.1  1996/03/08 15:22:00  mclareni
* Epio
*
*
#if defined(CERNLIB_UNIVAC)
 . SUBROUTINE FIOR/W/SCR/GW('FILENAME',BUFFERADR,NWORDS,STARTADR,STATUS)
 . CODED BY B.S.NILSSON, NBI, COPENHAGEN 1977-05-13
 .   THOROUGH REVISION MADE ON 1980-03-12
 . THIS IS A  F T N  VERSION REQUIRING FILENAME IN FIELDATA CODE
 .
          AXR$      .
$(0),IOPKT I$OD     ' ',0 0,0 0       . I/O PACKET
$(1),FIOSCR* LA,U   A3,SCR$           . SCATTER READ ENTRY
          J         ALL               .
FIOGW*    LA,U      A3,GW$            . GATHER WRITE ENTRY
          J         ALL               .
FIOR*     LA,U      A3,R$             . READ ENTRY
          J         ALL               .
FIOW*     LA,U      A3,W$             . WRITE ENTRY
ALL       LA,U      A2,IOPKT          . (IOPKT) IN A2
          SA,S2     A3,3,A2           .
          LA        A3,A0             . SAVE A0 IN A3
          LA        A1,0,A0           .
          SFS       1,A2              . SET BLANKS IN 2ND HALF OF FILENAME
          DL        A4,0,A1           .
          SA        A4,0,A2           . FILENAME FIRST WORD
          LA,S6     A4,0,A2           . CHECK FOR BLANK IN 6TH CHAR
          TNE,U     A4,05             .
          J         CONT              .
          LA        A4,A5             .
          SSL       A4,30             . CHECK FOR 7TH CHARACTER BLANK
          TNE,U     A4,05             .
          J         CONT              .
          TE,U      A4,077            . CHECK 7TH CHARACTER FOR STOPCODE
          SA        A5,1,A2           . NO, STORE FILENAME SECOND WORD
CONT      LA        A1,1,A0           .
          SA,H2     A1,4,A2           . BUFFER ADDRESS
          LA        A1,2,A0           .
          LA        A4,0,A1           .
          SA,H1     A4,4,A2           . NUMBER OF WORDS
          LA        A1,3,A0           .
          LA        A4,0,A1           .
          SA        A4,5,A2           . START ADDRESS
          LA        A0,A2             . MAKE I/O ER
          ER        IOW$              .
          LNA,S1    A4,3,A2           . GET STATUS
          JNZ       A4,$+2            . EVERYTHING OK ?
          LA,H2     A4,3,A2           . YES, STATUS = # OF WORDS
          LA        A0,4,A3           .
          SA        A4,0,A0           .
          J         0,X11             . RETURN
          END                         .
#endif
