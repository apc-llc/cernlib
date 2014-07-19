*
* $Id: timel.s,v 1.1.1.1 1996/02/15 17:51:36 mclareni Exp $
*
* $Log: timel.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:36  mclareni
* Kernlib
*
*
          IDENT TIMEL
*
* CERN PROGLIB# Z007    TIMEL           .VERSION KERNCDC  0.1   760901
*
          ENTRY TIMEL
*
*     CALL TIMEL (T)   RETURNS REMAINING TIME IN FLOATING SECONDS
*                             AUG-76, JZ.
*
 TRACE    VFD   30/5HTIMEL,30/TIMEL
 TIMEL    DATA  0
          BX6   X1
          SA6   SVX1               ADR(N) SAVED
          TIME  REC                TIME MACRO RQUEST
          SA1   REC            A1= 24/JL, 24/JS, 12/JM
          SB7   1              B7= 1
          MX0   24
          BX5   X0*X1          X5= 24/JL, 36/0
          LX0   24
          LX1   48
          BX4   X0*X1          X4= JS   SEC DONE
          AX0   12
          LX1   12
          BX3   X0*X1          X3= JM   MILI-SEC DONE
          SA1   A1+B7          X1= .001
          PX0   B0,X3              JM
          PX6   B0,X4              JS
          NX3   B0,X0          X3= AJM
          FX0   X1*X3              .001 * AJM
          NX4   B0,X6          X4= AJS
          LX5   24             X5= JL   SEC ALLOWED
          SA2   A1+B7          X2= ADR(T)
          PX7   B0,X5
          NX5   B0,X7          X5= AJL
          NX3   B0,X0          X3= .001 * AJM
          FX6   X3+X4          X6= AJS + .001*AJM   TIME DONE
          FX0   X5-X6
          NX7   B0,X0          X7= AJL - AJS+M
          SA7   X2                 STORE RESULT
          EQ    TIMEL
 REC      DATA  0
          DATA  .001
 SVX1     DATA  0
          END
#ifdef CERNLIB_TCGEN_TIMEL
#undef CERNLIB_TCGEN_TIMEL
#endif
