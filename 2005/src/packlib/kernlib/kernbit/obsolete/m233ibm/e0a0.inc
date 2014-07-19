*
* $Id: e0a0.inc,v 1.1.1.1 1996/02/15 17:47:40 mclareni Exp $
*
* $Log: e0a0.inc,v $
* Revision 1.1.1.1  1996/02/15 17:47:40  mclareni
* Kernlib
*
*
*
* e0a0.inc
*
          MACRO
&L        E0A0
          SPACE
&L        DS        0C                  BYTE ALIGNMENT ONLY
          SPACE
*         THIS TRANSLATE TABLE CONVERTS CERN EBCDIC TO CERN ASCII
*
*         IT IS BASED ON THE RUTHERFORD TABLE WITH THE FOLLOWING
*         MODIFICATIONS:-
*         EBCDIC 6A IS CONVERTED TO ASCII 5E (CIRCUMFLEX)
*         EBCDIC 71 IS CONSIDERED ILLEGAL
*         ILLEGAL CHARACTERS ARE CONVERTED TO 23 (HASH) RATHER THAN
*         25 (PERCENT)
*
*         Changed for new CERN character set/conversions
*         See below for details of change.
*         Tony Cass  User Support  CERN/DD  5th December 1989
*
          SPACE
TNT$E0A0  DC        X'000102032309237F2323230B0C0D0E0F'
          DC        X'1011121323230823181923231C1D1E1F'
          DC        X'23232323230A171B2323232323050607'
          DC        X'2323162323232304232323231415231A'
          DC        X'20232323232323232323232E3C282B7C'
          DC        X'2623232323232323232321242A293B7E'
          DC        X'2D2F23232323232323235E2C255F3E3F'
          DC        X'232323232323232323603A2340273D22'
          DC        X'23616263646566676869237B23232323'
          DC        X'236A6B6C6D6E6F707172237D23232323'
          DC        X'2323737475767778797A2323235B2323'
          DC        X'232323232323232323232323235D2323'
          DC        X'23414243444546474849232323232323'
          DC        X'234A4B4C4D4E4F505152232323232323'
          DC        X'5C23535455565758595A232323232323'
          DC        X'30313233343536373839232323232323'
          ORG TNT$E0A0+X'5F'   TNT:  Map EBCDIC Logical NOT
          DC        X'5E'              to ASCII caret/circumflex
          ORG TNT$E0A0+X'6A'   TNT:  Map EBCDIC Split bar
          DC        X'23'              to ASCII default
          ORG TNT$E0A0+X'8B'   TNT:  Old CERN curly bracket (left)
          DC        X'23'              to ASCII default
          ORG TNT$E0A0+X'9B'   TNT:  Old CERN curly bracket (right)
          DC        X'23'              to ASCII default
          ORG TNT$E0A0+X'A1'   TNT:  Map EBCDIC tilde
          DC        X'7E'              to ASCII tilde
          ORG TNT$E0A0+X'C0'   TNT:  New CERN curly bracket (left)
          DC        X'7B'              to ASCII
          ORG TNT$E0A0+X'D0'   TNT:  New CERN curly bracket (right)
          DC        X'7D'              to ASCII
          ORG ,                TNT:  Reset Location counter
          SPACE
          MEND
