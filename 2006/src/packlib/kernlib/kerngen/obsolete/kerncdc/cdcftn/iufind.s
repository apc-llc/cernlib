*
* $Id: iufind.s,v 1.1.1.1 1996/02/15 17:51:21 mclareni Exp $
*
* $Log: iufind.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:21  mclareni
* Kernlib
*
*
          IDENT IUFIND
*
* CERN PROGLIB# V304    IUFIND          .VERSION KERNCDC  0.1   730801
*
*         FUNCTION IUFIND(IT,IVEC,JL,JR)
*
*         ROUTINE WRITTEN BY P. CROZIER FOR FTN
*
          ENTRY IUFIND
          VFD   36/6HIUFIND,24/IUFIND
 IUFIND   DATA  0
          SB7   -1             B7= -1
          SA2   A1-B7
          SA3   A2-B7
          SA4   A3-B7
          SB2   X2
          SA3   X3             X3= JL
          SA4   X4             X4= JR
          SB3   B2+B7
          SA1   X1             X1= IT
          SB6   X3+B3          B6= LOC OF ELEMENT JL
          SB5   X4+B3          B5= MAX ALLOWED ADDRESS
 LOOP     SA2   B6             X2= IVEC(I)
          NO
          SB6   B6-B7              INCREMENT INDEX I
          BX0   X1-X2          X0= 0 IF IT = IVEC(I)
          ZR    X0,OUTLET          JUMP IF EQUAL
          GE    B5,B6,LOOP         JUMP IF JR.GE.I
          SX6   B6-B3              RETURN JR+1
          EQ    IUFIND
 OUTLET   SX6   B6-B2              RETURN SUBSCRIPT I
          EQ    IUFIND
          END
#ifdef CERNLIB_TCGEN_IUFIND
#undef CERNLIB_TCGEN_IUFIND
#endif
