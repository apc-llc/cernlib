*
* $Id: xzetoa.s,v 1.1.1.1 1996/03/08 15:44:30 mclareni Exp $
*
* $Log: xzetoa.s,v $
* Revision 1.1.1.1  1996/03/08 15:44:30  mclareni
* Cspack
*
*
#if defined(CERNLIB_IBM)
#if defined(CERNLIB_IBM)
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
XZETOA    CSECT
#endif
#if defined(CERNLIB_QMIBMXA)
XZETOA   AMODE ANY
XZETOA   RMODE ANY
#endif
#if defined(CERNLIB_IBM)
*
*               CALL XZETOA(AREA,N)
*                  TRANSLATES N BYTES IN AREA
*
          B     12(15)            BRANCH PAST NAME
          DC    X'07',CL7'XZETOA '
*
          STM   14,12,12(13)
          BALR  12,0
          USING *,12
         B     START
TAB       E0A0
START     L     2,0(1)   ADDRESS OF AREA
          L     3,4(1)   ADDRESS OF N
          L     3,0(3)   VALUE OF N
          LA    4,256(0)  SET UP 256
LOOP      SR    3,4       SEE IF MORE THAN 256 LEFT
          BC    13,FINAL
          TR    0(256,2),TAB     TRANSLATE 256 BYTE AT A TIME
          LA    2,256(2)
          B    LOOP
FINAL     AR   3,4
          BC   8,RETURN    IF ZERO LEFT RETURN
          BCTR 3,0
          EX   3,TRA       TRANSLATE THE REST
RETURN    LM    2,12,28(13)       RESTORE REGISTERS
**        MVI   12(13),X'FF'
          BR    14
TRA       TR    0(,2),TAB
*
          END
#endif
#endif
