*
* $Id: uctoh1.s,v 1.1.1.1 1996/02/15 17:51:25 mclareni Exp $
*
* $Log: uctoh1.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:25  mclareni
* Kernlib
*
*
#if defined(CERNLIB_F77)
          IDENT UCTOH1
*
* CERN PROGLIB# M409    UCTOH1          .VERSION KERNCDC  2.07  831121
* ORIG. 14/11/83  JZ, CERN
*
*     SUBROUTINE UCTOH1 (CH,HOLL,NCH)
*
*-----             USAGE OF REGISTERS
*
*         B1    NO. OF CHARS STILL TO BE TAKEN FROM CUR. INPUT CH(J)
*         B2    = 10   CHAR PER INPUT WORD
*         B4    = NCH COUNTED DOWN TO ZERO
*         B7    = -1
*
*      A1,X1    INPUT WORD CH(J)
*         X0    MASK 77B FOR RIGHT-MOST CHARACTER
*         X4    MASK 0055..55 OF 9 BLANKS
*         X5    = 6  FOR INTEGER MULTIPLY
*      A6,X6    OUTPUT WORD HOLL(L)
*
          ENTRY UCTOH1
          VFD   36/6HUCTOH1,24/UCTOH1
 UCTOH1   BSS   1
          SB7   -1            *B7= -1
*                              X1= ADR(CH)
          SA2   A1-B7          X2= ADR(HOLL)
          SA4   A2-B7          X4= ADR(NCH)
          MX0   6             *X0= 77B  MASK OF 1 CHAR.
*
          SA2   X2+B7          A2= ADR(HOLL) - 1
          SA4   X4             X4= NCH
*
          SB2   10            *B2= 10
          ZR    X4,UCTOH1          EXIT IF NCH=0
*
          BX6   X2
          SA6   A2            *A6= ADR(HOLL) - 1
          SB4   X4            *B4= NCH
          SX5   6             *X5= 6
          SA4   BLANK         *X4= ZERO/9 BLANKS
*
*--             READY FIRST INPUT WORD
*
#if !defined(CERNLIB_F77)
          SA1   X1            *A1= ADR(CH)
          SB1   B2            *B1= 10      TAKE 10 CHARS
#endif
#if defined(CERNLIB_F77)
          SB6   X1             B6= ADR(CH)
          AX1   24
          BX2   X1*X0          X2= NSK = SKIP NSK FIRST CHARS
          SA1   B6            *A1= ADR(CH)
          SB1   X2             B1= NSK
          DX2   X2*X5          X2= 6*NSK
          SB6   X2             B6= 6*NSK
          LX1   B6,X1         *X1= CH(1)  ROTATED READY
          SB1   B2-B1         *B1= 10-NSK CHARS STILL TO BE TAKEN
#endif
*
*--             START LOOPING
*
          SA2   A1-B7         *X2= LOOK-AHEAD INPUT WORD
*
*-----          COPY NEXT CHARACTER
*
 LOOPCH   BX6   X0*X1          X6= 1 INPUT CHAR ISOLATED
          SB4   B4+B7          B4  CHARS TO BE DONE IN ALL
          LX1   6
          BX6   X4+X6          X6  OUTPUT WORD WITH BLANK-FILL
          SB1   B1+B7          B1  CHARS TO BE TAKEN FROM CURRENT
          SA6   A6-B7              STORED
          EQ    B4,B0,UCTOH1
          NE    B1,B0,LOOPCH
*
*--             LOAD NEXT INPUT WORD
*
 NEXTIN   BX1   X2
          SA2   A2-B7          X2  NEXT LOOK-AHEAD INPUT WORD
          SB1   B2             B1= 10
          JP    LOOPCH
 BLANK    DATA  00555555555555555555B
          END
#ifdef CERNLIB_TCGEN_UCTOH1
#undef CERNLIB_TCGEN_UCTOH1
#endif
#endif
