*
* $Id: expair.s,v 1.1 1996/05/06 15:33:28 mclareni Exp $
*
* $Log: expair.s,v $
* Revision 1.1  1996/05/06 15:33:28  mclareni
* Move CDC assembler routines expair and pairpak from isatape to utils/cdc
*
* Revision 1.2  1996/04/26 09:52:19  mclareni
* To prepare for new versions of isajet, change the isajet directory to code
* and the isajet714 directory to isajet, as for other packages.
* Modify all #includes and the main Imakefile to cater for this.
*
* Revision 1.1.1.1  1996/03/08 17:27:54  mclareni
* Isajet714
*
*
#include "isajet/pilot.h"
#if defined(CERNLIB_CDCPACK)
          IDENT  EXPAIR
* 7/7/75  COMMENT LINE THAT FOLLOWS EXPAND HAS BEEN CORRECTED IN LIGHT
* OF INSIGHT GAINED IN FIXING A BUG IN PAIRPAK.  HOWEVER, THE ARITHMETIC
* PERFORMED IS UNCHANGED, SO THIS VERSION OF EXPAIR IS COMPATIBLE WITH
* BOTH PREVIOUS AND NEW VERSIONS OF PAIRPAK.
* VERSION OF 25 MARCH 75, PRESERVES DISTINCTION BETWEEN +ZERO AND -ZERO
*
* CALL EXPAIR(W1,W2,OUTW,IALARM) UNPACKS W1, W2 FROM LEFT, RIGHT HALVES
* OF OUTW, RESPECTIVELY.
* RETURNS IALARM=0 IF W1 AND W2 ARE INTEGER OR FINITE, DEFINITE REAL.
* RETURNS IALARM=1 IF W1 IS INFINITE OR INDEFINITE, 2 IF W2 IS SO,
* 3 IF BOTH W1 AND W2 ARE SO.
* CAUTION * REQUIRES DATA IN FORMAT PRODUCED BY PAIRPAK
* WORKS WITH RUN OR FTN CALLING SEQ. IF ASSEMBLED UNDER APPROPRIATE
* COMPILER.
* J. KOPP, BROOKHAVEN NATIONAL LABORATORY, 22 MARCH 1975
          ENTRY  EXPAIR
+VFD36/6HEXPAIR,24/4
EXPAIR    DATA   0
          IFLE   *F,1
          SA5    B3
          ELSE
          SA3    A1+2
          SA5    X3
          ENDIF
          SB6    1            ALARM BIT FOR ARG1 = INFINITE
          MX2    0            TOTAL OF ALARMS TO DATE
          RJ     EXPAND       UNPACK LEFT HALF OUTW
          BX6    X5           W1
          SA5    A5+B0
          SB6    B6+B6        ALARM BIT FOR ARG2 = INFINITE
          LX5    30
          RJ     EXPAND       UNPACK RIGHT HALF
          BX7    X5           W2
          IFLE   *F,1
          SA6   B1
          SA7   B2
          BX6    X2           ALARM
          SA6    B4
          ELSE
          SA4    A1+1
          SA5    A3+1         FETCH LOC ALARM
          SA6    X1
          SA7    X4
          BX6    X2
          SA6    X5
          ENDIF
          JP     EXPAIR
* ANALYZE NUMBER IN LEFT 30 BITS OF X5, RETURN IN X5 AS PROPERLY
* POSITIONED INTEGER, REAL OR INFINITY.
* USES X0,X3,X4
EXP4      AX3    30                SHIFT INTEGER TO LOW END
EXP1      PL     X5,EXP2           IF ORIG. SIGN BIT ON, COMPLEMENT ANS.
          BX3    -X3
EXP2      BX5    X3
EXPAND    DATA   0
          MX0    29                DROP LOW 30 BITS + OUR SIGN BIT
          LX0    59
          BX3    X5*X0
          MX0    6
          LX0    59                MAKE EXPONENT-BIT MASK FOR LATER
          BX4    X3*X0             TEST FOR INTEGER
          ZR     X4,EXP4           IF EXPONENT BITS 0, RETURN INTEGER
          BX4    X4-X0             REAL, TEST FOR DUMMY EXPONENT 63
          ZR     X4,EXP3           YES, OUT-OF-RANGE REAL PACKED BELOW
          AX3    5                 NO, SPECIALLY BIASSED IN-RANGE EXP.
          UX4    B4,X3
* NOW B4=OUR EXPONENT-1023 = TRUE EXPONENT+77-1023 = TRUE -946
* (1023, NOT 1024, BECAUSE EXP. ARITHMETIC IS 1-S COMPLEMENT, 11 BITS)
          SB4    B4+946            RESTORE TRUE EXPONENT
          EQ     B0,B0,EXP5        DO STANDARD PROCESSING FOR BOTH TYPES
* OUT-OF-RANGE REAL PACKED BELOW DUMMY EXPONENT
EXP3      BX3    -X0*X3            DROP DUMMY EXPONENT
          LX3    6
          UX4    B4,X3
* REAL IN B4 AND X4. NOW RESTORE NORMALIZE BIT IN CHARACTERISTIC
EXP5      AX4    1
          MX0    1
          LX0    48
          BX4    X4+X0
          PX3    B4,X4
          OR     X3,EXP6           CHECK FOR INFINITE OR INDEFINITE
          DF     X3,EXP1           FINITE + DEFINITE, CHECK SIGN
EXP6      SX2    B6+X2             SET ALARM BIT FOR THIS ARG
          JP     EXP1
          END
#endif
