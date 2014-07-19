*
* $Id: timed.s,v 1.1.1.1 1996/02/15 17:51:30 mclareni Exp $
*
* $Log: timed.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:30  mclareni
* Kernlib
*
*
          IDENT TIMED
*
* CERN PROGLIB# Z007    TIMED           .VERSION KERNCDC  1.14  820114
*
          ENTRY TIMED
*
*     CALL TIMED (T)   RETURNS RUNNING TIME SINCE LAST CALL
*                      ACCURATE TO 1 CLOCK CYCLE     JAN-82, JZ
*
 TRACE    VFD   30/5HTIMED,30/TIMED
 TIMED    DATA  0
          BX6   X1
          SA6   SVX1               ADR(N) SAVED
          TIME  NEW,7CLK           TIME REQUEST MACRO
          SB7   1              B7= 1
          SA1   LAST           X1= LAST
          SA2   A1+B7          X2= NEW
          SA3   A2+B7          X3= FACTOR
          IX5   X2-X1          X5= NEW-LAST  INTEGER
          BX7   X2             X7= NEW   FOR LAST
          PX0   B0,X5
          NX5   B0,X0          X5= NEW/LAST   FLOATING
          SA2   A3+B7          X2= ADR(T)
          FX6   X3*X5          X6= NEW-LAST   SECONDS
          SA7   A1                 STORE LAST
          SA6   X2                 STORE RESULT
          EQ    TIMED
 LAST     DATA  0
 NEW      DATA  0
 FACT     DATA  27.5E-9        SECONDS PER CLOCK-CYCLE
 SVX1     DATA  0
          END
#ifdef CERNLIB_CDCSYS_TIMED
#undef CERNLIB_CDCSYS_TIMED
#endif
#ifdef CERNLIB_TCGEN_TIMED
#undef CERNLIB_TCGEN_TIMED
#endif
