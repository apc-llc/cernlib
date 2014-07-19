*
* $Id: fmhost.s,v 1.1.1.1 1996/03/07 15:17:36 mclareni Exp $
*
* $Log: fmhost.s,v $
* Revision 1.1.1.1  1996/03/07 15:17:36  mclareni
* Fatmen
*
*
#if defined(CERNLIB_IBMMVS)
#include "fatreg.inc"
#include "fatbeg.inc"
#include "fatschl.inc"
#include "fatanf.inc"
*   30/01/91 101301642  MEMBER NAME  SYSCON   (X.S)         ASSEMBLER
***********************************************************************
*                                                                     *
*                      S Y S C O N                                    *
*                                                                     *
*         Written by Dietrich Moenkemeyer DESY Hamburg                *
*                                                                     *
*                      January 1991                                   *
*                                                                     *
***********************************************************************
*                                                                     *
*   This routine returns some system constants.
*                                                                     *
***********************************************************************
*                                                                     *
*   CALL:     CALL SYSCON(node,system,model)                          *
*                                                                     *
*   Output:   node      8 BYTES   node name                           *
*             system    8 BYTES   operating system                    *
*             model     8 BYTES   CPU model number                    *
*                                                                     *
*   LKED:     reentrant, 31bit                                        *
*                                                                     *
***********************************************************************
*   Change activity:                                                  *
***********************************************************************
*
         REGISTER
FMHOST   BEG   *,MODE=RENT,RMODE=ANY
*
         LR    R9,R1              SAVE ADDRESS OF ARGUMENTLIST
*
**********************************************************************
*                                                                    *
*  Branch to amode 31 code to get the system constants               *
*                                                                    *
**********************************************************************
*
         LA    R15,ABOVE           LOAD BRANCH ADDRESS
         L     R1,BIT32
         OR    R15,R1              SET AMODE 31 BIT
         BASSM R14,R15             BRANCH, SET AMODE AND SAVE OLD AMODE
         SCHL  RET=(R15),MODE=RENT
*
**********************************************************************
*                                                                    *
*  Get system constants from the CVT                                 *
*                                                                    *
**********************************************************************
*
ABOVE    L     R2,CVTPTR
         LR    R4,R2
         SH    R4,=AL2(CVTMAP-CVTFIX)
         USING CVTFIX,R4                  MAKE CVT PRFIX ADDRESSABLE
         USING CVTMAP,R2                  MAKE CVT ADDESSABLE
*
*        GET NODE NAME (FIXED)
*
         L     R1,0(R9)
         MVC   0(8,R1),NODE               MOVE NODENAME
*
*        GET OPERATING SYSTEM
*
         L     R1,4(R9)
         MVC   0(8,R1),OPVM
         TM    CVTDCB,CVT1SSS             VM
         BO    SYS010
         MVC   0(8,R1),OPMVSXA
         TM    CVTDCB,CVTMVSE+CVT8AOS2    MVS-XA
         BO    SYS010
         MVC   0(8,R1),OPMVS
         TM    CVTDCB,CVT8AOS2            MVS
         BO    SYS010
         MVC   0(8,R1),OPMVT
         TM    CVTDCB,CVT4MS1             MVT
         BO    SYS010
         MVC   0(8,R1),OPMFT
         TM    CVTDCB,CVT2SPS             MFT
         BO    SYS010
*
*        GET CPU MODEL ID
*
SYS010   L     R1,8(R9)
         MVC   0(8,R1),=CL8' '            INIT WITH BLANKS
         MVC   3(1,R1),CVTMDL+1           GET LAST DIGIT
         OI    3(R1),X'F0'                MAKE IT PRINTABLE
         UNPK  0(3,R1),CVTMDL             GET OTHER DIGITS
         OI    2(R1),X'F0'                MAKE IT PRINTABLE
*
         XR    R15,R15
RET15    BSM   R0,R14                     RETURN AND RESET AMODE
*
**********************************************************************
*                                                                    *
*  FIXED DATA AREA                                                   *
*                                                                    *
**********************************************************************
*
         DS    0F
BIT32    DC    XL4'80000000'
NODE     DC    CL8'DHHDESY3'
OPMVSXA  DC    CL8'MVS/XA'
OPVM     DC    CL8'VM'
OPMFT    DC    CL8'MFT'
OPMVT    DC    CL8'MVT'
OPMVS    DC    CL8'MVS'
         LTORG
*
**********************************************************************
*                                                                    *
*  D S E C T S                                                       *
*                                                                    *
**********************************************************************
*
         CVT   DSECT=YES,PREFIX=YES,LIST=YES
         END
#endif
