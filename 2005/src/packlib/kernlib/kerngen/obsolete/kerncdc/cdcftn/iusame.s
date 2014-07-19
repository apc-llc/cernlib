*
* $Id: iusame.s,v 1.1.1.1 1996/02/15 17:51:21 mclareni Exp $
*
* $Log: iusame.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:21  mclareni
* Kernlib
*
*
          IDENT IUSAME
*
* CERN PROGLIB# M501    IUSAME          .VERSION KERNCDC  0.1   760426
*
          ENTRY IUSAME
*
*               X0 MASK FULL / EXPONENT
*               X1 REFERENCE WORD                 B1 ADR (VECT(1))
*               X2 CURRENT WORD                   B2 J-1 / J OF CURRENT
*               X3 WORKING                        B3 JR
*               X4 MIN
*               X5 ADR(JASME)                     B5 JSAME
*               X6 = ZERO                         B6 JMIN = JSAME+MIN
*                                                 B7 = 1
*         CURRENT: VECT(JFAIL) BEING COMPARED AGAINST VECT(JFAIL-1)
*
*     NSAME = IUSAME (VECT,JL,JR,MIN,JSAME)
*
          VFD   36/6HIUSAME,24/IUSAME
 IUSAME   DATA  0
          SB7   1             *B7= 1
          SA2   A1+B7
          SA3   A2+B7
          SA4   A3+B7
          SA5   A4+B7         *X5= ADR(JSAME)
          SA2   X2             X2= JL  LEFT
          SA3   X3             X3= JR  RIGHT
          SA4   X4            *X4= MIN = MIN STRING LENGTH
          SB1   X1            *B1= ADR (VECT(1))
          SB2   X2             B2= JL = J-1 OF CURRENT, VECT(JL+1) INIT.
          SB3   X3             B3= JR
          SB6   B2+X4          B6= JL+MIN
          MX6   0             *X6= ZERO
          SB6   B6-B7
          SB6   B6-B7          B6= JL+MIN-2
          GE    B6,B3,SHORT        EXIT IF  JL+MIN-2 .GE. JR
          SA2   B1+B2          X2= VECT(JL+1)
          SA1   A2-B7          X1= VECT(JL)
*
 NEW      SB5   B2             B5= JSAME = J OF FIRST WORD OF STRING
          SB6   B2+X4          B6= JMIN = JSAME+MIN
          OR    X1,SPEC            X1 OUT OF RANGE
          ID    X1,SPEC            X1 INDEFINITE
          MX0   60             X0= MASK FULL WORD
 LOOP     BX3   X2-X1          X3= COMPARE
          SB2   B2+B7          B2= JFAIL, J OF CURRENT WORD
          BX1   X2                 CURRENT WORD TO REFERENCE
          SA2   B1+B2          X2= VECT(J+1)   NEXT WORD READY
          BX3   X0*X3          X3= COMPARE, SIGNIFICANT
          GE    B2,B3,EXHAUST      EXIT IF  JFAIL .GE. JR
          ZR    X3,LOOP
          LT    B2,B6,NEW          NO STRING IF  JFAIL .LT. JSAME+MIN
          EQ    FOUND
 SPEC     MX0   12             X0= MASK EXPONENT ONLY
          EQ    LOOP
*
*               VECT (JL/JR)  EXHAUSTED
*
 EXHAUST  BSS   0                  JFAIL = JR
          NZ    X3,NOTLAST
          SB2   B2+B7              JFAIL = JR+1
NOTLAST   GE    B2,B6,FOUND        STRING IF  JFAIL .GE. JSAME+MIN
 SHORT    SX7   B3+B7              JSAME = JR+1
          SA7   X5
          EQ    IUSAME
*
 FOUND    SX7   B5             X7= JSAME
          SX6   B2-B5          X6= NSAME = JFAIL - JSAME
          SA7   X5                 STORE JSAME
          EQ    IUSAME
          END
#ifdef CERNLIB_TCGEN_IUSAME
#undef CERNLIB_TCGEN_IUSAME
#endif
