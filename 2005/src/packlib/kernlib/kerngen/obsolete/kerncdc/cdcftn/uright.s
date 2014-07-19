*
* $Id: uright.s,v 1.1.1.1 1996/02/15 17:51:26 mclareni Exp $
*
* $Log: uright.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:26  mclareni
* Kernlib
*
*
          IDENT URIGHT
*
* CERN PROGLIB# M410    URIGHT          .VERSION KERNCDC  1.06  780614
*
          ENTRY URIGHT
*
*     CALL URIGHT (CH,JL,JR)
*                              COMPASS VERSION  MAY 1978, JZ
*
*         REGISTER ASSIGNMENTS
*
*         X1  CH(JTK)          B1  ADR(CH(0))
*         X2  CH(JTK-1)        B2  JL
*                              B3  JR
*         X4  BLANK WORD       B4  JTK
*
          USE   /SLATE/
 NDIGIT   BSS   1
 JEND     BSS   39
          USE
*
          VFD   36/6HURIGHT,24/URIGHT
 URIGHT   DATA  0              X1= ADR(CH)
          SB7   1
          SA2   A1+B7          X2= ADR(JL)
          SA3   A2+B7          X3= ADR(JR)
          SA4   BLANK          X4= ALL BLANKS
          SA2   X2             X2= JL
          SA3   X3             X3= JR
          SB1   X1             B1= ADR(CH(1))
          SB1   B1-B7          B1= ADR(CH(0))
          SB2   X2             B2= JL
          SB3   X3             B3= JR
          SB5   B3                 JEND = JR  IN CASE
          LT    B3,B2,HOME
          SB4   B3+B7          B4= JTK = JR + 1
          SA2   B1+B3          X2= CH(JTK-1) = CH(JR)
*
*         LOOP TO FIND FIRST BLANK
*
          SB5   B2-B7              JEND = JL-1  IN CASE NO BLANK
 LOOP1    EQ    B4,B2,HOME
          BX1   X2             X1= CH(JTK)
          SA2   A2-B7          X2= CH(JTK-1)  READ AHEAD
          IX3   X1-X4
          SB4   B4-B7          B4= JTK
          NZ    X3,LOOP1
*
*         LOOP TO RUN OVER FIRST STRING OF BLANKS
*
          SB5   B4             B5= JEND = JTK OF FIRST BLANK
 LOOP2    EQ    B4,B2,HOME
          BX1   X2             X1= CH(JTK)
          SA2   A2-B7          X2= CH(JTK-1)  READ AHEAD
          IX3   X1-X4
          SB4   B4-B7          B4= JTK
          ZR    X3,LOOP2
*
*         LOOP TO COPY
*
          BX6   X1                 STORE FIRST NON-BLANK CHAR
          SA6   B1+B5
          SB6   B4             B6= JTK OF LAST NON-BLANK CHAR
 LOOP3    EQ    B4,B2,END3
          BX6   X2             X6= CH(JTK)
          SA2   A2-B7          X2= CH(JTK-1)  READ AHEAD
          IX3   X6-X4
          SB4   B4-B7          B4= JTK
          ZR    X3,LOOP3
          SA6   A6-B7
          SB6   B4             B6= JTK OF LAST NON-BLANK CHAR
          EQ    LOOP3
*
*         LOOP TO BLANK TRAILING WORDS
*
 END3     SB5   A6-B1          B5= JPUT OF LAST STORED
          SB4   B5-B6          B4= N CHAR. TO BE CLEARED
          BX6   X4
          SB5   B5-B7              JEND = JPUT-1
 LOOP4    SA6   A6-B7
          SB4   B4-B7
          NE    B4,B0,LOOP4
*
 HOME     SX7   B5                 JEND
          SX6   B3-B5              NDIG = JR - JEND
          SA6   NDIGIT
          SA7   A6+B7
          EQ    URIGHT
*
 BLANK    DATA  55555555555555555555B
          END
#ifdef CERNLIB_TCGEN_URIGHT
#undef CERNLIB_TCGEN_URIGHT
#endif
