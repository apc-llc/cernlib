*
* $Id: iucomp.s,v 1.1.1.1 1996/02/15 17:51:21 mclareni Exp $
*
* $Log: iucomp.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:21  mclareni
* Kernlib
*
*
          IDENT IUCOMP
*
* CERN PROGLIB# V304    IUCOMP          .VERSION KERNCDC  0.1   750820
*
          ENTRY IUCOMP
*     FUNCTION IUCOMP (ITEXT,IVECT,N)
*        (WRITTEN FOR SCOPE 3.3 - FTN)
          VFD   36/6HIUCOMP,24/IUCOMP
 IUCOMP   BSS   1
          SA3   A1+2
          SA2   A1+1
          SA4   X3             X4= N
          MX6   0
          SB6   1              B6= 1
          SA5   X1             X5= ITEXT
          SA3   X2                 FIRST ITEM OF IVECT IN X3
          ZR    X4,IUCOMP
          IX1   X3-X5              PRE-COMPARE
          SX6   B6             X6= 1
          SB4   X4             B4= N
 LOOP     SA3   A3+B6              NEXT ITEM IN X3
          SB4   B4-B6
          ZR    X1,IUCOMP          ITEM FOUND
          IX1   X3-X5
          SX6   X6+B6          X6= INDEX OF JUST CHECKED
          LT    B0,B4,LOOP
          MX6   0                  NOT FOUND
          EQ    IUCOMP
          END
#ifdef CERNLIB_TCGEN_IUCOMP
#undef CERNLIB_TCGEN_IUCOMP
#endif
