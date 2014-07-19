*
* $Id: uh1toc.s,v 1.1.1.1 1996/02/15 17:51:26 mclareni Exp $
*
* $Log: uh1toc.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:26  mclareni
* Kernlib
*
*
#if defined(CERNLIB_F77)
          IDENT UH1TOC
*
* CERN PROGLIB# M409    UH1TOC          .VERSION KERNCDC  2.14  850320
* ORIG. 14/11/83  JZ, CERN
*
*     SUBROUTINE UH1TOC (HOLL,CH,NCH)
*
*-----             USAGE OF REGISTERS
*
*         B4    = NCH COUNTED DOWN TO ZERO
*         B5    = 10   CHAR PER OUTPUT WORD
*         B6    NO. OF CHARS STILL TO BE PLACED INTO CUR. OUTPUT CH
*         B7    = -1
*
*      A1,X1    INPUT WORD HOLL(J)
*         X0    MASK  FOR LEFT-MOST CHARACTER
*         X5    = 6  FOR INTEGER MULTIPLY
*      A6,X6    OUTPUT WORD CH(L)
*
          ENTRY UH1TOC
          VFD   36/6HUH1TOC,24/UH1TOC
 UH1TOC   BSS   1
          SB7   -1            *B7= -1
*                              X1= ADR(HOLL)
          SA3   A1-B7          X3= ADR(CH)
          SA4   A3-B7          X4= ADR(NCH)
          MX0   6             *X0= 7700...00  MASK OF 1 CHAR.
*
          SB6   X3             B6= ADR(CH)
          SA4   X4             X4= NCH
*
          SA5   B6+B7          A5= ADR(CH) - 1
          SB5   10            *B5= 10
          ZR    X4,UH1TOC          EXIT IF NCH=0
*
          BX6   X5
          SA6   A5            *A6= ADR(CH) - 1
          SB4   X4            *B4= NCH
          SX5   6             *X5= 6
          SA1   X1            *A1= ADR(HOLL)
*
*--             READY FIRST OUTPUT WORD
*
          MX6   0             *X6=  CLEAR
          SB6   B5            *B6= 10      PUT 10 CHARS
#if defined(CERNLIB_F77)
          LX3   30
          BX3   X3*X0          X3= NSK = KEEP NSK FIRST CHARS
          LX3   6
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
*-----          COPY NEXT CHARACTER
*
 LOOPCH   BX7   X0*X1          X7=   ONE INPUT CHAR ISOLATED
          SA1   A1-B7              READY NEXT LOOK-AHEAD
          SB4   B4+B7          B4  CHARS TO BE DONE IN ALL
          BX6   X6+X7                OUTPUT CHARS ACCUMULATED
          SB6   B6+B7          B6  CHARS TO BE PUT INTO CURRENT
          LX6   6
          EQ    B4,B0,DONE
          NE    B6,B0,LOOPCH
*
*--             STORE CURRENT OUTPUT WORD, READY FOR NEXT
*
          SA6   A6-B7              STORED
          SB6   B5             B6= 10
          MX6   0
          JP    LOOPCH
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
          JP    UH1TOC
          END
#ifdef CERNLIB_TCGEN_UH1TOC
#undef CERNLIB_TCGEN_UH1TOC
#endif
#endif
