*
* $Id: fatschl.inc,v 1.1.1.1 1996/03/07 15:17:59 mclareni Exp $
*
* $Log: fatschl.inc,v $
* Revision 1.1.1.1  1996/03/07 15:17:59  mclareni
* Fatmen
*
*
#ifndef CERNLIB_FATMEN_FATSCHL_INC
#define CERNLIB_FATMEN_FATSCHL_INC
*
*
* fatschl.inc
*
#if defined(CERNLIB_IBMMVS)
         MACRO
&NAME    SCHL  &RET=,&MODE=
&NAME    L     13,4(13)            RELOAD SA-REG
         AIF   ('&RET' NE '').RETCODE
         LM    14,12,12(13)        RELOAD REGISTERS
         BR    14                  RETURN TO CALLER
         MEXIT
.RETCODE AIF   ('&RET'(1,1) EQ '(').REGIST
         AIF   (T'&RET NE 'N').ADDRESS
         LA    15,&RET             LOAD REG 15 WITH RET CODE
         AGO   .FURTHER
.REGIST  AIF   ('&RET(1)' EQ '15').FURTHER
         LR    15,&RET(1)          LOAD REG 15 WITH SPECIF. REGISTER
         AGO   .FURTHER
.ADDRESS L     15,&RET             LOAD REG 15 FROM STORAGE
.FURTHER LM    0,12,20(13)         RESTORE REG 0 TO 12
         L     14,12(13)           RESTORE REG 14
         BR    14                  RETURN TO CALLER
         MEND
#endif

#endif
