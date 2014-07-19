*
* $Id: datimh.s,v 1.1.1.1 1996/02/15 17:51:33 mclareni Exp $
*
* $Log: datimh.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:33  mclareni
* Kernlib
*
*
          IDENT DATIMH
*
* CERN PROGLIB# Z007    DATIMH          .VERSION KERNCDC  2.06  830901
*
          ENTRY DATIMH
          EXT   DATE,TIME
*
*     CALL DATIMH (ND,NT)   RETURNS HOLL. DATE    ND = 8HDD/MM/YY
*                                   HOLL. TIME    NT = 8HHH.MM.SS
*                             AUG-76, JZ.
*     MODIFIED JULY 83 TO ACCEPT CHARACTER TYPE ARGUMENTS WHICH ARE
*     EITHER SCALAR OR THE FIRST ELEMENT OF AN ARRAY.  H.RENSHALL/DD
*
 TRACE    VFD   36/6HDATIMH,24/DATIMH
 DATIMH   DATA  0
          SB7   1
          SA2   A1+B7
          MX0   42B
          BX1   -X0*X1
          BX6   X1
          BX7   -X0*X2
          SA6   SAVE               ADR(ND) SAVED
          SA7   A6+B7              ADR(NT) SAVED
+RJDATEBCDDATE,CDCSYSTEMROUTINE
-         VFD   12/0,18/TRACE
          SA1   SAVE
          LX6   6
          SA6   X1
*
          SA1   SAVE+1
+RJTIMEBCDTIME,CDCSYSTEMROUTINE
-         VFD   12/0,18/TRACE
          SA1   SAVE+1
          LX6   6
          SA6   X1
          EQ    DATIMH
 SAVE     DATA  0
          DATA  0
          END
#ifdef CERNLIB_TCGEN_DATIMH
#undef CERNLIB_TCGEN_DATIMH
#endif
