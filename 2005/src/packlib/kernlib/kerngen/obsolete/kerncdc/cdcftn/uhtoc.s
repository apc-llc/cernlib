*
* $Id: uhtoc.s,v 1.1.1.1 1996/02/15 17:51:25 mclareni Exp $
*
* $Log: uhtoc.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:25  mclareni
* Kernlib
*
*
          IDENT UHTOC
*
* CERN PROGLIB# M409    UHTOC           .VERSION KERNCDC  2.07  831121
* ORIG. 14/11/83  JZ, CERN
*
*     SUBROUTINE UHTOC (HOLL,K,CH,NCH)
*
*-----             USAGE OF REGISTERS
*
*         B1    NO. OF CHARS STILL TO BE TAKEN FROM CUR. INPUT HOLL(J)
*         B2    = K = CHAR CAPACITY OF 1 HOLL WORD
*         B4    = NCH COUNTED DOWN TO ZERO
*         B5    = 10   CHAR PER OUTPUT WORD
*         B6    NO. OF CHARS STILL TO BE PLACED INTO CUR. OUTPUT CH
*         B7    = -1
*
*      A1,X1    INPUT WORD HOLL(J)
*         X0    MASK 77B FOR RIGHT-MOST CHARACTER
*         X5    = 6  FOR INTEGER MULTIPLY
*      A6,X6    OUTPUT WORD CH(L)
*
          ENTRY UHTOC
          VFD   30/5HUHTOC,30/UHTOC
 UHTOC    BSS   1
          SB7   -1            *B7= -1
*                              X1= ADR(HOLL)
          SA2   A1-B7          X2= ADR(K)
          SA3   A2-B7          X3= ADR(CH)
          SA4   A3-B7          X4= ADR(NCH)
          MX0   6
          LX0   6             *X0= 77B  MASK OF 1 CHAR.
*
          SA2   X2             X2= K
          SB6   X3             B6= ADR(CH)
          SA4   X4             X4= NCH
*
          SA5   B6+B7          A5= ADR(CH) - 1
          SB5   10            *B5= 10
          ZR    X4,UHTOC           EXIT IF NCH=0
*
          BX6   X5
          SA6   A5            *A6= ADR(CH) - 1
          SB4   X4            *B4= NCH
          SX5   6             *X5= 6
          SA1   X1            *A1= ADR(HOLL)
*
*--             CALC. B2 = K = MIN0 (K,10)
*
          SX6   B5             X6= 10
          IX7   X6-X2          X7= 10 - K
          AX7   59             X7= MASK ALL ZERO OR ALL ONE
          BX6    X7*X6         X6= ZERO OR 10
          BX2   -X7*X2         X2=    K OR ZERO
          BX2   X2+X6          X2= MIN0 (K,10)
          SB2   X2            *B2= K
*
*--             READY FIRST OUTPUT WORD
*
          MX6   0             *X6=  CLEAR
          SB6   B5            *B6= 10      PUT 10 CHARS
#if defined(CERNLIB_F77)
          AX3   24
          BX3   X3*X0          X3= NSK = KEEP NSK FIRST CHARS
          ZR    X3,READY
          SA2   A6-B7          X2= CH(1)
          SB6   X3             B6= NSK
          DX3   X3*X5          X3= 6*NSK
          MX4   1
          SB3   X3+B7          B3= 6*NSK - 1
          SB1   X3             B1= 6*NSK
          AX4   B3,X4          X4= MASK OF 6*NSK BITS LEFT
          SB6   B5-B6         *B6= 10-NSK CHARS STILL TO BE PUT
          BX6   X4*X2          X6= NSK CHARS / ZERO
          LX6   B1,X6         *X6= CH(1)  ROTATED
 READY    BSS   0
#endif
*
*--             START LOOPING
*
          SA2   A1-B7         *X2= LOOK-AHEAD INPUT WORD
          SB1   B2            *B1= K CHARS PER INPUT WORD
*
*-----          COPY NEXT CHARACTER
*
 LOOPCH   LX1   6
          SB1   B1+B7          B1  CHARS TO BE TAKEN FROM CURRENT
          LX6   6
          SB4   B4+B7          B4  CHARS TO BE DONE IN ALL
          BX7   X0*X1          X7=   ONE INPUT CHAR ISOLATED
          SB6   B6+B7          B6  CHARS TO BE PUT INTO CURRENT
          BX6   X6+X7                OUTPUT CHARS ACCUMULATED
          EQ    B4,B0,DONE
          EQ    B6,B0,NEXTOUT
          NE    B1,B0,LOOPCH
*
*--             LOAD NEXT INPUT WORD
*
 NEXTIN   BX1   X2
          SA2   A2-B7          X2  NEXT LOOK-AHEAD INPUT WORD
          SB1   B2             B1= K
          JP    LOOPCH
*
*--             STORE CURRENT OUTPUT WORD, READY FOR NEXT
*
 NEXTOUT  SA6   A6-B7              STORED
          SB6   B5             B6= 10
          MX6   0
          NE    B1,B0,LOOPCH       CURRENT INPUT NOT EXHAUSTED
          JP    NEXTIN
*
*-----          ALL DONE, STORE LAST OUTPUT WORD
*
*                           NKEEP = 10-NDONE  TRAILING CHARS
*                         - LEFT  SHIFT RESULT BY 6*NKEEP
*                         - MERGE NKEEP ORIGINAL CHARS
*
 DONE     EQ    B6,B0,STORE    B6= NKEEP
          SX2   B6             X2= NKEEP
          MX3   1
          DX2   X2*X5          X2= 6*NKEEP
          SA1   A6-B7          X1= ORIGINAL CONTENT OF CH(N)
          SB1   X2             B1= 6*NKEEP
          SB2   X2+B7          B2= 6*NKEEP - 1
          LX6   B1,X6          X6= NDONE CHARS / ZERO
          AX3   B2,X3          X3= MASK OF 6*NKEEP BITS LEFT
          LX3   B1,X3          X3= MASK OF 6*NKEEP BITS RIGHT
          BX1   X3*X1          X1= NKEEP ORIGINAL CHARS ISOLATED
          BX6   X1+X6
STORE     SA6   A6-B7              STORE
          JP    UHTOC
          END
#ifdef CERNLIB_TCGEN_UHTOC
#undef CERNLIB_TCGEN_UHTOC
#endif
