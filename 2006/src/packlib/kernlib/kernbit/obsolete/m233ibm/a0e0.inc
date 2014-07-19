*
* $Id: a0e0.inc,v 1.1.1.1 1996/02/15 17:47:40 mclareni Exp $
*
* $Log: a0e0.inc,v $
* Revision 1.1.1.1  1996/02/15 17:47:40  mclareni
* Kernlib
*
*
*
* a0e0.inc
*
          MACRO
&L        A0E0
          SPACE
&L        DS        0C                  BYTE ALIGNMENT ONLY
          SPACE
*         THIS TRANSLATE TABLE CONVERTS CERN ASCII TO CERN EBCDIC
*
*         IT IS BASED ON THE RUTHERFORD TABLE WITH THE FOLLOWING
*         MODIFICATIONS:-
*         ASCII 5E IS CONVERTED TO EBCDIC 6A (CIRCUMFLEX)
*         ILLEGAL CHARACTERS ARE CONVERTED TO 7B (HASH) RATHER THAN
*         6C (PERCENT)
*
*         Changed for new CERN character set/conversions
*         See below for details of change.
*         Tony Cass  User Support  CERN/DD  5th December 1989
*
*STANDARD CERN ASCII TO EBCDIC CONVERSION TABLE OBTAINED FROM
*ROB WATTS ON APRIL 28TH 1981
*
*         CODE  00,&OPT,00,N8      . NULL
*         CODE  01,&OPT,01,N8      . SOH
*         CODE  02,&OPT,02,N8      . STX
*         CODE  03,&OPT,03,N8      . ETX
*         CODE  04,&OPT,37,N8      . EOT
*         CODE  05,&OPT,2D,N8      . ENQ
*         CODE  06,&OPT,2E,N8      . ACK
*         CODE  07,&OPT,2F,N8      . BEL
*         CODE  08,&OPT,16,N8      . BACKSPACE
*         CODE  09,&OPT,05,N8      . HT
*         CODE  0A,&OPT,25,N8      . LF
*         CODE  0B,&OPT,0B,N8      . VT
*         CODE  0C,&OPT,0C,N8      . FF
*         CODE  0D,&OPT,0D,N8      . CR
*         CODE  0E,&OPT,0E,N8      . SO
*         CODE  0F,&OPT,0F,N8      . SI
*         CODE  10,&OPT,10,N8      . DLE
*         CODE  11,&OPT,11,N8      . DC1
*         CODE  12,&OPT,12,N8      . DC2
*         CODE  13,&OPT,13,N8      . DC3
*         CODE  14,&OPT,3C,N8      . DC4
*         CODE  15,&OPT,3D,N8      . NAK
*         CODE  16,&OPT,32,N8      . SYN
*         CODE  17,&OPT,26,N8      . ETB
*         CODE  18,&OPT,18,N8      . CAN
*         CODE  19,&OPT,19,N8      . EM
*         CODE  1A,&OPT,3F,N8      . SUB
*         CODE  1B,&OPT,27,N8      . ESC
*         CODE  1C,&OPT,1C,N8      . FS
*         CODE  1D,&OPT,1D,N8      . GS
*         CODE  1E,&OPT,1E,N8      . RS
*         CODE  1F,&OPT,1F,N8      . US
*         CODE  20,&OPT,40,N8      . SPACE
*         CODE  21,&OPT,5A,N8      . EXCLAMATION MARK
*         CODE  22,&OPT,7F,N8      . DOUBLE QUOTE
*         CODE  23,&OPT,7B,N8      . HASH SIGN
*         CODE  24,&OPT,5B,N8      . DOLLAR SIGN
*         CODE  25,&OPT,6C,N8      . PERCENT
*         CODE  26,&OPT,50,N8      . AMPERSAND
*         CODE  27,&OPT,7D,N8      . SINGLE QUOTE
*         CODE  28,&OPT,4D,N8      . LEFT BRACKET
*         CODE  29,&OPT,5D,N8      . RIGHT BRACKET
*         CODE  2A,&OPT,5C,N8      . ASTERISK
*         CODE  2B,&OPT,4E,N8      . PLUS
*         CODE  2C,&OPT,6B,N8      . COMMA
*         CODE  2D,&OPT,60,N8      . MINUS
*         CODE  2E,&OPT,4B,N8      . PERIOD (FULL STOP)
*         CODE  2F,&OPT,61,N8      . SLASH
*         CODE  30,&OPT,F0,N8      . ZERO
*         CODE  31,&OPT,F1,N8      . ONE
*         CODE  32,&OPT,F2,N8      . TWO
*         CODE  33,&OPT,F3,N8      . THREE
*         CODE  34,&OPT,F4,N8      . FOUR
*         CODE  35,&OPT,F5,N8      . FIVE
*         CODE  36,&OPT,F6,N8      . SIX
*         CODE  37,&OPT,F7,N8      . SEVEN
*         CODE  38,&OPT,F8,N8      . EIGHT
*         CODE  39,&OPT,F9,N8      . NINE
*         CODE  3A,&OPT,7A,N8      . COLON
*         CODE  3B,&OPT,5E,N8      . SEMI-COLON
*         CODE  3C,&OPT,4C,N8      . LESS THAN
*         CODE  3D,&OPT,7E,N8      . EQUALS
*         CODE  3E,&OPT,6E,N8      . GREATER THAN
*         CODE  3F,&OPT,6F,N8      . QUESTION MARK
*         CODE  40,&OPT,7C,N8      . AT SIGN
*         CODE  41,&OPT,C1,N8      . UPPER A
*         CODE  42,&OPT,C2,N8      . UPPER B
*         CODE  43,&OPT,C3,N8      . UPPER C
*         CODE  44,&OPT,C4,N8      . UPPER D
*         CODE  45,&OPT,C5,N8      . UPPER E
*         CODE  46,&OPT,C6,N8      . UPPER F
*         CODE  47,&OPT,C7,N8      . UPPER G
*         CODE  48,&OPT,C8,N8      . UPPER H
*         CODE  49,&OPT,C9,N8      . UPPER I
*         CODE  4A,&OPT,D1,N8      . UPPER J
*         CODE  4B,&OPT,D2,N8      . UPPER K
*         CODE  4C,&OPT,D3,N8      . UPPER L
*         CODE  4D,&OPT,D4,N8      . UPPER M
*         CODE  4E,&OPT,D5,N8      . UPPER N
*         CODE  4F,&OPT,D6,N8      . UPPER O
*         CODE  50,&OPT,D7,N8      . UPPER P
*         CODE  51,&OPT,D8,N8      . UPPER Q
*         CODE  52,&OPT,D9,N8      . UPPER R
*         CODE  53,&OPT,E2,N8      . UPPER S
*         CODE  54,&OPT,E3,N8      . UPPER T
*         CODE  55,&OPT,E4,N8      . UPPER U
*         CODE  56,&OPT,E5,N8      . UPPER V
*         CODE  57,&OPT,E6,N8      . UPPER W
*         CODE  58,&OPT,E7,N8      . UPPER X
*         CODE  59,&OPT,E8,N8      . UPPER Y
*         CODE  5A,&OPT,E9,N8      . UPPER Z
*         CODE  5B,&OPT,AD,N8      . LEFT SQUARE BRACKET
*         CODE  5C,&OPT,E0,N8      . BACK SLASH
*         CODE  5D,&OPT,BD,N8      . RIGHT SQUARE BRACKET
*         CODE  5E,&OPT,6A,N8      . CIRCUMFLEX
*         CODE  5F,&OPT,6D,N8      . UNDERSCORE
*         CODE  60,&OPT,79,N8      . GRAVE ACCENT
*         CODE  61,&OPT,81,N8      . LOWER A
*         CODE  62,&OPT,82,N8      . LOWER B
*         CODE  63,&OPT,83,N8      . LOWER C
*         CODE  64,&OPT,84,N8      . LOWER D
*         CODE  65,&OPT,85,N8      . LOWER E
*         CODE  66,&OPT,86,N8      . LOWER F
*         CODE  67,&OPT,87,N8      . LOWER G
*         CODE  68,&OPT,88,N8      . LOWER H
*         CODE  69,&OPT,89,N8      . LOWER I
*         CODE  6A,&OPT,91,N8      . LOWER J
*         CODE  6B,&OPT,92,N8      . LOWER K
*         CODE  6C,&OPT,93,N8      . LOWER L
*         CODE  6D,&OPT,94,N8      . LOWER M
*         CODE  6E,&OPT,95,N8      . LOWER N
*         CODE  6F,&OPT,96,N8      . LOWER O
*         CODE  70,&OPT,97,N8      . LOWER P
*         CODE  71,&OPT,98,N8      . LOWER Q
*         CODE  72,&OPT,99,N8      . LOWER R
*         CODE  73,&OPT,A2,N8      . LOWER S
*         CODE  74,&OPT,A3,N8      . LOWER T
*         CODE  75,&OPT,A4,N8      . LOWER U
*         CODE  76,&OPT,A5,N8      . LOWER V
*         CODE  77,&OPT,A6,N8      . LOWER W
*         CODE  78,&OPT,A7,N8      . LOWER X
*         CODE  79,&OPT,A8,N8      . LOWER Y
*         CODE  7A,&OPT,A9,N8      . LOWER Z
*         CODE  7B,&OPT,8B,N8      . LEFT BRACE
*         CODE  7C,&OPT,4F,N8      . VERTICAL BAR
*         CODE  7D,&OPT,9B,N8      . RIGHT BRACE
*         CODE  7E,&OPT,5F,N8      . LOGICAL NOT TO TILDE
*         CODE  7F,&OPT,07,N8      . DEL (RUBOUT)
*
          SPACE
TNT$A0E0  DC        X'00010203372D2E2F1605250B0C0D0E0F'
          DC        X'101112133C3D322618193F271C1D1E1F'
          DC        X'405A7F7B5B6C507D4D5D5C4E6B604B61'
          DC        X'F0F1F2F3F4F5F6F7F8F97A5E4C7E6E6F'
          DC        X'7CC1C2C3C4C5C6C7C8C9D1D2D3D4D5D6'
          DC        X'D7D8D9E2E3E4E5E6E7E8E9ADE0BD6A6D'
          DC        X'79818283848586878889919293949596'
          DC        X'979899A2A3A4A5A6A7A8A98B4F9B5F07'
          DC        X'7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B'
          DC        X'7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B'
          DC        X'7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B'
          DC        X'7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B'
          DC        X'7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B'
          DC        X'7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B'
          DC        X'7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B'
          DC        X'7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B'
          ORG TNT$A0E0+X'5E'   TNT:  Map ASCII caret/circumflex
          DC        X'5F'              to EBCDIC logical NOT
          ORG TNT$A0E0+X'7B'   TNT:  Map ASCII left curly bracket
          DC        X'C0'              to new CERN code
          ORG TNT$A0E0+X'7D'   TNT:  Map ASCII rihjt curly bracket
          DC        X'D0'              to new CERN code
          ORG TNT$A0E0+X'7E'   TNT:  Map ASCII tilde
          DC        X'A1'              to EBCDIC tilde
          ORG ,                TNT:  Reset Location counter
          SPACE
          MEND
