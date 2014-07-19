*
* $Id: getusr.s,v 1.1.1.1 1996/03/08 15:44:22 mclareni Exp $
*
* $Log: getusr.s,v $
* Revision 1.1.1.1  1996/03/08 15:44:22  mclareni
* Cspack
*
*
#if defined(CERNLIB_IBMVM)
         PRINT GEN
GETUSR   EDCPROL
         BALR 12,0
         USING *,12                      R12 <-- REGISTER BASE
*********************************************************************
         LA    2,EXTRACT                                            2
         LA    3,2
         DC    X'83',X'23',XL2'0180'     DIAGNOSE X'180'            2
*********************************************************************
         SR    1,1                       INDEX
         L     4,=F'1'                   INCREMENT
         L     5,=F'7'                   LIMIT
         LA    15,RESULT                 STRING ADDR
         LA    6,RESULT-1                POINTER (I NEED IT FOR CLI)
BUCLE    EQU   *
         A     6,=F'1'                   UPDATE POINTER
         CLI   0(6),C' '                 FIRST BLANK ?
         BE    PUTNUL
         TR    0(,6),TAB                 Translate to lowercase
         BXLE  1,4,BUCLE
         A     6,=F'1'                   UPDATE POINTER
PUTNUL   SR    4,4
         STC   4,0(6)
FIN      EDCEPIL
         LTORG *
EXTRACT  DC    CL8'USER    '
RESULT   DS    CL8
         DS    C                         NULL HOLDER
TAB       EQU  *
          DC        X'000102030405060708090A0B0C0D0E0F'
          DC        X'101112131415161718191A1B1C1D1E1F'
          DC        X'202122232425262728292A2B2C2D2E2F'
          DC        X'303132333435363738393A3B3C3D3E3F'
          DC        X'404142434445464748494A4B4C4D4E4F'
          DC        X'505152535455565758595A5B5C5D5E5F'
          DC        X'606162636465666768696A6B6C6D6E6F'
          DC        X'707172737475767778797A7B7C7D7E7F'
          DC        X'808182838485868788898A8B8C8D8E8F'
          DC        X'909192939495969798999A9B9C9D9E9F'
          DC        X'A0A1A2A3A4A5A6A7A8A9AAABACADAEAF'
          DC        X'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF'
          DC        X'C0818283848586878889CACBCCCDCECF'
          DC        X'D0919293949596979899DADBDCDDDEDF'
          DC        X'E0E1A2A3A4A5A6A7A8A9EAEBECEDEEEF'
          DC        X'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF'
         END   GETUSR
#endif
