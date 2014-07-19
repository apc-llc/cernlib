*
* $Id: uset.s,v 1.1.1.1 1996/02/15 17:51:26 mclareni Exp $
*
* $Log: uset.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:26  mclareni
* Kernlib
*
*
          IDENT USET
*
* CERN PROGLIB# M410    USET            .VERSION KERNCDC  1.06  780614
*
          ENTRY USET
*
*     CALL USET (IVAL,CH,JL,JR)
*                              COMPASS VERSION  MAY 1978, JZ
*
*         REGISTER ASSIGNMENTS
*
*         X0  = 10 INTEGER
*         X1  IVAL             B1  ADR(CH(1))
*         X2  IVAL/10          B2  JL
*                              B3  JR
*         X4  = 10. NORMAL     B4  J, INDEX FOR STORE
*         X5  = 1H0 ROTATED    B7  = -1
*
          USE   /SLATE/
 NDIGIT   BSS   1
 JNEXT    BSS   39
          USE
*
          VFD   24/4HUSET,36/USET
 USET     DATA  0
          SB7   -1
          SB5   X1             B5= ADR(IVAL)
          SA1   A1-B7          X1= ADR(CH(1))
          SA2   A1-B7          X2= ADR(JL)
          SA3   A2-B7          X3= ADR(JR)
          SX0   10             X0= 10
          PX7   X0
          NX4   X7             X4= 10.
          SA2   X2             X2= JL
          SA3   X3             X3= JR
          SA5   ZERO           X5= 1H0
          SB1   X1             B1= ADR(CH)
          SB2   X2             B2= JL
          SA2   B5             X2= IVAL
          SB3   X3             B3= JR
          SB4   X3             B4= J = JR
*
 LOOP     LT    B4,B2,END      B4= J
          BX1   X2             X1= IVAL
          PX7   X2             X7= IVAL FLOATING
          FX3   X7/X4          X3= IVAL/10  FLOATING
          UX7   B5,X3
          LX2   B5,X7          X2= IVAL/10  INTEGER
          DX7   X0*X2          X7= 10 * (IVAL/10)
          IX3   X1-X7          X3= N = MOD(IVAL,10)
          IX7   X5+X3
          LX7   54             X7= DISPAY CODE REPR. OF N
          SB4   B4+B7          B4= JNEXT = J-1
          SA7   B1+B4              STORE NEW CHARACTER
          NZ    X2,LOOP
*
 END      SX6   B3-B4              NDIG = JR - JN
          SX7   B4                 JN
          SA6   NDIGIT
          SA7   A6-B7
          EQ    USET
*
 ZERO     DATA  55555555555555555533B
          END
#ifdef CERNLIB_TCGEN_USET
#undef CERNLIB_TCGEN_USET
#endif
