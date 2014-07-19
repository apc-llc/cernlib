*
* $Id: timed.s,v 1.1.1.1 1996/02/15 17:51:36 mclareni Exp $
*
* $Log: timed.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:36  mclareni
* Kernlib
*
*
          IDENT TIMED
*
* CERN PROGLIB# Z007    TIMED           .VERSION KERNCDC  0.1   760901
*
          ENTRY TIMED
          EXT   SECOND
*
*     CALL TIMED (T)   RETURNS RUNNING TIME SINCE LAST CALL
*                             AUG-76, JZ.
*
 TRACE    VFD   30/5HTIMED,30/TIMED
 TIMED    DATA  0
          BX6   X1
          SA6   SVX1               ADR(N) SAVED
+RJSECONDFLOATINGTIME,CDCSYSTEMROUTINE
-         VFD   12/0,18/TRACE
          SA5   LAST
          SA2   SVX1
          FX1   X6-X5
          SA6   A5                 STORE LAST
          NX6   B0,X1
          SA6   X2                 STORE RESULT
          EQ    TIMED
 SVX1     DATA  0
 LAST     DATA  0
          END
#ifdef CERNLIB_TCGEN_TIMED
#undef CERNLIB_TCGEN_TIMED
#endif
