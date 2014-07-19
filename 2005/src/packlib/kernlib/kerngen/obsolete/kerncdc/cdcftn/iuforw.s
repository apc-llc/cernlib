*
* $Id: iuforw.s,v 1.1.1.1 1996/02/15 17:51:21 mclareni Exp $
*
* $Log: iuforw.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:21  mclareni
* Kernlib
*
*
          IDENT IUFORW
*
* CERN PROGLIB# M410    IUFORW          .VERSION KERNCDC  1.06  780614
*
          ENTRY IUFORW
*
*     NVAL = IUFORW (CH,JL,JR)
*                              COMPASS VERSION  MAY 1978, JZ
*
*         REGISTER ASSIGNMENTS
*
*         X0  MASK  7700-00
*         X1  CH(J)            B1  J
*         X2  CH(J+1) AHEAD    B2  JR
*                              B3  = -27 = DC FOR ZERO (DISPLAY CODE)
*         X4  = 10             B4          DC FOR CH(J)  - 27
*         X5  NVAL CUMULATION  B5  = 18  = DC FOR BLANK  - 27
*         X6  DIGITS READ      B6  = 10  = DC FOR PLUS   - 27
*                              B7  = 1
*
          USE   /SLATE/
 NDIGIT   BSS   1
 JEND     BSS   39
          USE
*
          VFD   36/6HIUFORW,24/IUFORW
 IUFORW   BSSZ  1              X1= ADR(CH)
          SB7   1
          SA2   A1+B7          X2= ADR(JL)
          SA3   A2+B7          X3= ADR(JR)
          MX0   6
          SB3   -27
          MX5   0
          SA2   X2             X2= JL
          SA3   X3             X3= JR
          SB5   18
          SB6   10
          MX6   0
          SX4   B6             X4= 10
          SB1   X2             B1= JL
          SB2   X3             B2= JR
          SB1   B1-B7          B1= J = JL-1
          SA2   X1+B1          X2= CH(J+1) = CH(JL)
          GE    B1,B2,BLANK
*
 LOOP     BX1   X0*X2          X1= CH(J)
          SA2   A2+B7          X2= CH(J+1)  READ AHEAD
          SB1   B1+B7          B1= J
          LX1   6
          SB4   X1+B3          B4= DC - 27
          LT    B4,B0,END          -->  DC .LT. 27
          EQ    B4,B5,BLANK        -->  DC .EQ. 45
          GE    B4,B6,END          -->  DC .GE. 37
          DX5   X4*X5
          SX1   B4
          SX6   X6+B7
          IX5   X1+X5
 BLANK    LT    B1,B2,LOOP
          SB1   B1+B7              J = J+1
 END      SA6   NDIGIT             STORE NUMBER OF DIGITS READ
          SX7   B1
          SA7   A6+B7              STORE JEND
          BX6   X5
          EQ    IUFORW
          END
#ifdef CERNLIB_TCGEN_IUFORW
#undef CERNLIB_TCGEN_IUFORW
#endif
