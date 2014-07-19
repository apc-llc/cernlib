*
* $Id: chmoveml.s,v 1.1.1.1 1996/03/22 16:42:45 mclareni Exp $
*
* $Log: chmoveml.s,v $
* Revision 1.1.1.1  1996/03/22 16:42:45  mclareni
* Phtools
*
*
#if defined(CERNLIB_CDC)
          IDENT  CHMOVE
MOVE      TITLE    CHARACTER MOVE SUBROUTINE
***       CALL CHMOVE(X,I,Y,J,N)
*
*         MOVE N CHARACTERS
*           FROM PACKED BUFFER X STARTING IN COLUMN I
*           TO   PACKED BUFFER Y STARTING IN COLUMN J
*           (WHERE THE FIRST CHARACTER OF A BUFFER IS NUMBERED 1.)
*
*         UNDEFINED RESULTS PRODUCED IF THE CHARACTER FIELDS OVERLAP.
*
*         SCM DIRECT RANGE ERROR IF N<0 OR N>20000
*
M1        VFD       42/0LCHMOVE,18/5
          ENTRY     CHMOVE
* SUBROUTINE CHMOVE(IBUF,IPOS,KBUF,KPOS,NCHAR)
CHMOVE    DATA      0
          SB7      1                   CONVERT FROM RUN TO FTN CALL. SEQ
          SA2      A1
          SB1      X2
          SA2      A2+B7
          SB2      X2
          SA2      A2+B7
          SB3      X2
          SA2      A2+B7
          SB4      X2
          SA2      A2+B7
          SB5      X2                  END OF CALL SEQ. CONVERSION
          SA1      =20000000000000000006B  GET 6
          SA2      B2              GET COUNT PARAMETERS
          SA4      B4
          PX2      X2
          SB7      -6
          DX2      X1*X2
          SA5      B5
          PX4      X4
          ZR       X5,CHMOVE       IF 0 CHARACTER MOVE
          DX4      X1*X4
          PX5      X5
          SB2      X2+B7
          DX5      X1*X5
          SB4      X4+B7
          SB5      X5
          SB6      60
          SB7      1
          SX5      X5-120001
          NG       B5,MOVE6        IF N<0
          PL       X5,MOVE6        IF N>20000
+SB4B4-B6ADJUSTTOFIRSTSIGNIFICANTWORD
          SB3      B3+B7
          PL       B4,*
+SB2B2-B6
          SB1      B1+B7
          PL       B2,*
          SB2      B2+B6
          SA1      B1-B7           GET FIRST WORD OF X
          SB4      B4+B6
          SA3      B3-B7           GET FIRST WORD OF Y
          MX0      1               FORM INITIAL MASKS
          SB1      B2-B7
          AX2      B1,X0
          SB3      B4-B7
          AX4      B3,X0
          MX7      59
          BX2      X7*X2
          BX4      X7*X4
          SB3      B2-B4           GET OFFSET SHIFT
          BX1      -X2*X1          GET INITIAL X
          SA5      A3-B7           PRESET STORE ADDRESS
          BX6      X5
          SA6      A5
          BX6      X4*X3           GET INITIAL Y
          SB5      B5-B6           COUNT INITIAL WORD FILL
          SB5      B5+B4
          NZ       B3,MOVE2        IF NONZERO OFFSET
          BX6      X1+X6           FILL FIRST WORD
          GE       B0,B5,MOVE5     IF MOVE COMPLETE
MOVE1     SA6      A6+B7           STORE OLD WORD
          SA1      A1+B7           GET NEW WORD
          SB5      B5-B6
          BX6      X1
          LT       B0,B5,MOVE1     IF MORE TO GO
          EQ       MOVE5
MOVE2     NG       B3,MOVE3        IF INITIAL X AND Y HAVE > 10 CHARS
          SB1      B6-B3
          LX1      B3,X1           POSITION INITIAL WORD
          SB1      B1-B7           FORM OFFSET MASK
          BX6      X1+X6
          SA1      A1+B7
          AX7      B1,X0
          LX1      B3,X1
          BX2      -X7*X1
          BX6      X2+X6
          GE       B0,B5,MOVE5     IF FIRST WORD IS LAST ONE
          EQ       MOVE4           GO DO BODY
MOVE3     SB1      B0-B3
          SB3      B3+B6           ADJUST OFFSET
          LX1      B3,X1           POSITION INITIAL WORD
          SB1      B1-B7           FORM OFFSET MASK
          BX2      -X4*X1
          AX7      B1,X0
          BX6      X2+X6
          GE       B0,B5,MOVE5     IF FIRST WORD IS THE LAST
MOVE4     SA6      A6+B7           STORE OLD WORD
          BX6      X7*X1           GET FIRST HALF OF NEW
          SA1      A1+B7
          SB5      B5-B6
          LX1      B3,X1
          BX2      -X7*X1
          BX6      X2+X6           COMPLETE NEW WORD
          LT       B0,B5,MOVE4     IF MORE TO GO
MOVE5     SB2      B5+B6           GET NUMBER OF REMAINING BITS
          SA3      A6+B7           GERT LAST WORD OF Y
          SB1      B2-B7
          AX2      B1,X0
          BX3      -X2*X3          COMBINE TAIL WORD
          BX6      X2*X6
          BX6      X3+X6
          SA6      A3
          EQ       CHMOVE
MOVE6     SX1      69              FATAL ERROR 69
          SX2      =C* CHARACTER COUNT <0 OR >20000.*
          SA1      377777B         MODE1 FOR USER ERROR
          END
#endif
