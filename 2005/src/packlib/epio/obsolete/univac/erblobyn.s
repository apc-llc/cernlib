*
* $Id: erblobyn.s,v 1.1.1.1 1996/03/08 15:22:00 mclareni Exp $
*
* $Log: erblobyn.s,v $
* Revision 1.1.1.1  1996/03/08 15:22:00  mclareni
* Epio
*
*
#if defined(CERNLIB_UNIVAC)
 .        EP UTILITY PROGRAM
 . ERROR HANDLER FOR BLOBYT AND BUNBYT
 .
 . ERROR PRINTOUT WHEN NBITS > 36, NBITS < 1 OR NSTART < 1.
 .
 . UNIVAC  F T N  VERSION BY B.S.NILSSON, NBI, MAY 1981
 .
          AXR$                        .
$(0),A0SAVE RES     6                 . SCRATCH AREA
ERRTEX1   '  *** ERROR IN BLOBYT *** ' .
ERRTEX2   '  *** ERROR IN BUNBYT *** ' .
ERRTEX3   '  A0  +  X11  +' .
ERRTEX4   '  NBYTES=+  NBITS=+  NSTART=+' .
ERRTEX5   '  *** NO ACTION TAKEN ***' .
PKT       E$PKT     8,LINE            . PRINT PACKET FOR ERROR MESSAGE
LINE      RES       8                 .
$(1),ERRBLO* LA,U   A1,A0SAVE         . ENTRY FROM BLOBYT
          SA        A0,0,A1           . SAVE A0
          LXM,U     A0,ERRTEX1        . FIRST LINE OF ERROR TEXT
          J         ERRPR             .
ERRBUN*   LA,U      A1,A0SAVE         . ENTRY FROM BUNBYT
          SA        A0,0,A1           . SAVE A0
          LXM,U     A0,ERRTEX2        . FIRST LINE OF ERROR TEXT
ERRPR     LXI,U     A0,0205           .
          ER        PRINT$            . PRINT FIRST ERROR LINE
          SX        X5,5,A1           . STORE X5
          LX        X5,A1             . ADDR(A0SAVE) IN X5
          SX        X11,1,A1          . STORE X11
          LA        A0,0,A1           . COPY ARGUMENT VALUES
          DL        A2,2,A0           .
          LA        A4,0,A2           .
          LA        A5,0,A3           .
          DS        A4,2,A1           .
          LA        A2,4,A0           .
          LA        A4,0,A2           .
          SA        A4,4,A1           .
          LA,U      A0,PKT            . START EDITED PRINT
          LMJ       X11,EDIT$         .
          LA,U      A0,ERRTEX3        .
          LMJ       X11,EMSG$         .
          LA,U      A1,12             .
          LA        A0,0,X5           .
          LMJ       X11,EOCTF$        .
          LMJ       X11,EMSGR$        .
          LA,U      A1,12             .
          LA        A0,1,X5           .
          LMJ       X11,EOCTF$        .
          LMJ       X11,EDITX$        .
          LXI,U     A0,0107           .
          LXM,U     A0,LINE           .
          ER        PRINT$            . 2ND LINE
          LA,U      A0,PKT            . START NEXT LINE
          LMJ       X11,EDIT$         .
          LA,U      A0,ERRTEX4        .
          LMJ       X11,EMSG$         .
          LA        A0,2,X5           .
          LMJ       X11,EDECV$        .
          LMJ       X11,EMSGR$        .
          LA        A0,3,X5           .
          LMJ       X11,EDECV$        .
          LMJ       X11,EMSGR$        .
          LA        A0,4,X5           .
          LMJ       X11,EDECV$        .
          LMJ       X11,EDITX$        .
          LXI,U     A0,0110           .
          LXM,U     A0,LINE           .
          ER        PRINT$            . 3RD LINE
          LXI,U     A0,0105           .
          LXM,U     A0,ERRTEX5        .
          ER        PRINT$            . 4TH LINE
          LX        X11,1,X5          . RESTORE X5 AND X11
          LX        X5,5,X5           .
          J         0,X11             . RETURN
          END                         .
#endif
