*
* $Id: datime.s,v 1.1.1.1 1996/02/15 17:51:33 mclareni Exp $
*
* $Log: datime.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:33  mclareni
* Kernlib
*
*
          IDENT DATIME
*
* CERN PROGLIB# Z007    DATIME          .VERSION KERNCDC  1.13  820111
*
          ENTRY DATIME
          EXT   DATE,TIME
*
*     CALL DATIME (ND,NT)   RETURNS INTEGER DATE   ND = YYMMDD
*                                   INTEGER TIME   NT =   HHMM
*                             AUG-76, JZ.
*
 TRACE    VFD   36/6HDATIME,24/DATIME
 DATIME   DATA  0
          SB7   1
          SA2   A1+B7
          BX6   X1
          BX7   X2
          SA6   SAVE               ADR(ND) SAVED
          SA7   A6+B7              ADR(NT) SAVED
+RJDATEBCDDATE,CDCSYSTEMROUTINE
-         VFD   12/0,18/TRACE
#if !defined(CERNLIB_USADATE)
*                              X6= BCD-DATE   .DD/MM/YY.
*                                              9  6  3
          MX0   12             X0= 12-BIT MASK
          LX0   18                 PICK YY FROM 3/2
          BX1   X0*X6
          LX1   42                 YY INTO 10/9
          LX0   18                 PICK MM FROM 6/5
          BX5   X0*X6
          LX5   12                 MM INTO 8/7
          BX1   X1+X5
          LX0   18                 PICK DD FROM 9/8
          BX5   X0*X6
          LX5   42                 DD INTO 6/5
          BX7   X1+X5          X7= 6HYYMMDD0000
*                                     9 7 5
#endif
#if defined(CERNLIB_USADATE)
*                              X6= BCD-DATE   .MM/DD/YY.
*                                              9  6  3
          MX0   12             X0= 12-BIT MASK
          LX0   18                 PICK YY FROM 3/2
          BX1   X0*X6
          LX1   42                 YY INTO 10/9
          LX0   36                 PICK MM FROM 9/8
          BX5   X0*X6
          LX5   54                 MM INTO 8/7
          BX1   X1+X5
          LX0   42                 PICK DD FROM 6/5
          BX5   X0*X6
          BX7   X1+X5          X7= 6HYYMMDD0000
*                                     9 8 5
#endif
          SA2   SAVE
          SB2   6              B2= 6, LOOP COUNT
          SB3   B2             B3  .NE.0 FLAG FOR DATE
*
 CONV     SX4   10             X4= 10
          SX5   33B            X5= DISPLAY-CODE VALUE OF ZERO
          MX0   6
          LX0   6              X0= ..00077
          SB7   1              B7= 1
          MX6   0
*
 LOOP     DX6   X4*X6          X6= 10*(ACCU)
          LX7   6                  READY NEXT DIGIT
          BX1   X0*X7
          IX3   X1-X5          X3= INTEGER-VALE OF DIGIT
          SB2   B2-B7
          IX6   X6+X3              ACCU = 10*ACCU + DIGIT
          NE    B0,B2,LOOP
          SA6   X2                 STORE RESULT
          EQ    B0,B3,DATIME
*
*     PROCESS TIME
*
          SA1   SAVE+1
+RJTIMEBCDTIME,CDCSYSTEMROUTINE
-         VFD   12/0,18/TRACE
*
          LX6   6              X6= 8HHH.MM.SS.
          MX0   12
          BX7   X0*X6
          LX6   6
          BX6   -X0*X6
          BX7   X6+X7          X1= 7HHHMM.SS
*
          SA2   SAVE+1
          SB2   4              B2= 4,  LOOP COUNT
          SB3   B0             B3= 0, FLAG FOR TIME
          EQ    CONV
*
 SAVE     DATA  0
          DATA  0
          END
#ifdef CERNLIB_TCGEN_DATIME
#undef CERNLIB_TCGEN_DATIME
#endif
