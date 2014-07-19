*
* $Id: ccrack.s,v 1.1.1.1 1996/04/01 15:03:02 mclareni Exp $
*
* $Log: ccrack.s,v $
* Revision 1.1.1.1  1996/04/01 15:03:02  mclareni
* Mathlib gen
*
*
#if defined(CERNLIB_CDC)
          IDENT  CCRACK
          ENTRY  CCRACK
          SPACE  1
************************************************************
*
*         CCRACK - FORTRAN-CALLABLE COMPASS SUBROUTINE TO MOVE YHE
*                  CRACKED CONTROL CARD FROM RAS+2 INTO COMMON/CCARD/
*         INPUT - RAS+2, RAS+3, ...
*
*         OUTPUT - CRACKED CONTROL CARD IN COMMON /CCARD/
*                  IF NO PARAMETERS, WORD 1 OF /CCARD/ WILL BE ZERO
*                             (NOT SPACE-FILLED)
*
*                  THE COMMON BLOCK HAS A LENGTH OF 100B,AND IT HAS TO
*                  BE DECLARED IN THE FORTRAN PROGRAM CALLING THIS
*                  ROUTINE
*
*         SOME EXTRA CHECKS HAVE BEEN ADDED (JULY 1979) TO PREVENT
*         INFINITE LOOPS FROM OCCURRING WHEN NUMBER OF FILES IN THE
*         FTN PROGRAM CARD IS GREATER THAN THE NUMBER OF PARAMETERS
*         IN THE CALLING SEQUENCE.
***************************************************************
          SPACE  2
TRACE     DATA   10H CCRACK
          SPACE  3
          USE    /CCARD/           COMMON BLOCK FOR RESULT OF CRACKING
CCARD1    BSSZ   100B
          USE    *
          SPACE  3
CCRACK    DATA   0                 MAIN ENTRY
          SB7    1
          SA5 64B
          MX6  0
          MX0  42
          SA6  CCARD1                  STORE ZERO WORD IN CCARD
          BX3  -X0*X5
          ZR   X3,CCRACK               RETURN IF NO PARAMETERS
          SB3   X3-1                SAVE NUMBER OF PARAMETERS
          SA5  2                       INITIALIZE SCANNING
 NEXT     BSS  0
          BX1  X0*X5
          RJ   BLANKF
          BX6  -X0*X5                  SEPARATOR CODE
          BX1  X0*X1
          SX3  X6-17B
          BX6  X6+X1                   SYMBOL + SEPARATOR CODE
          SA6  A6
          SA5  A5+B7                   PICK NEXT
          MX6  0
          SA6  A6+B7                   NEW ZERO WORD AT END
          NZ    B3,CCRACK1          STILL MORE PARAMETERS
          ZR    X5,CCRACK           STOP ON ZERO WORD
          ZR   X3,CCRACK               RETURN WHEN SEP. CODE = 17B
          EQ   NEXT
CCRACK1   SB3   B3-1
          ZR    X3,CCRACK
          EQ    NEXT
          SPACE  3
***************************************
*
*         BLANKF - BLANK FILLS SYMBOL IN X1
*         INPUT  - X1
*         OUTPUT - X1
*
          SPACE  1
BLANKF    DATA   0
          MX3    6
          SX4    1R
BLANKF1   BSS    0
          LX3    6
          BX2    X1*X3
          NZ     X2,BLANKF         NONZERO CHAR DETECTED,RETURN
          BX1    X1+X4             OR IN BLANK
          LX4    6
          EQ     BLANKF1
          SPACE  2
          END
#endif
