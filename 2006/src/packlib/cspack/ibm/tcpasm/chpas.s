*
* $Id: chpas.s,v 1.1.1.1 1996/03/08 15:44:22 mclareni Exp $
*
* $Log: chpas.s,v $
* Revision 1.1.1.1  1996/03/08 15:44:22  mclareni
* Cspack
*
*
#if defined(CERNLIB_IBMVM)
         PRINT GEN
CHPAS    EDCPROL
         BALR 12,0
         USING *,12                      R12 <-- REGISTRO BASE
         L     2,0(,1)                   GET ADDR OF PARM 1
         MVC   USR,0(2)
         L     2,4(,1)                   GET ADDR OF PARM 2
         MVC   PASSW,0(2)
         SR    1,1                       INDEX
         L     4,=F'1'                   INCREMENT
         L     5,=F'7'                   LIMIT
         LA    15,USR                    STRING ADDR
         LA    6,USR-1                   POINTER (I NEED IT FOR CLI)
LOOP1    EQU   *
         A     6,=F'1'                   UPDATE POINTER
         CLI   0(6),0                    FIRST NULL ?
         BE    MORBLAN
         BXLE  1,4,LOOP1
         B     NEXTP
MORBLAN  IC    4,=C' '
         STC   4,0(6)                    PUT A BLANC
NEXTP    SR    1,1                       INDEX
         L     4,=F'1'                   INCREMENT
         L     5,=F'7'                   LIMIT
         LA    15,PASSW                  STRING ADDR
         LA    6,PASSW-1                 POINTER (I NEED IT FOR CLI)
LOOP2    EQU   *
         A     6,=F'1'                   UPDATE POINTER
         CLI   0(6),0                    FIRST NULL ?
         BE    MORNUL
         BXLE  1,4,LOOP2
         B     CONT
MORNUL   IC    4,=C' '
         STC   4,0(6)
*********************************************************************
CONT     LA    2,EXTRACT                                            2
         LA    3,4
         DC    X'83',X'23',XL2'0180'     DIAGNOSE X'180'            2
*********************************************************************
         CLI   0(15),C'Y'
         BNE   PASNOK
         SR    15,15
         B     FIN
PASNOK   L     15,=F'1'
FIN      EDCEPIL
         LTORG *
         DS    0D
EXTRACT  DC    CL8'SETUSER '
USR      DC    CL8'        '
         DC    CL8'VERIFYPW'
PASSW    DC    CL8'        '
         DS    CL8
         END   CHPAS
#endif
