*
* $Id: maxdsk.s,v 1.1.1.1 1996/02/15 17:51:56 mclareni Exp $
*
* $Log: maxdsk.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:56  mclareni
* Kernlib
*
*
         PRINT NOGEN
MAXDSK   CSECT
#if defined(CERNLIB_QMIBMXA)
MAXDSK   AMODE ANY
MAXDSK   RMODE ANY
#endif
*
***********************************************************************
*                                                                     *
* CERN Program Library Z306    MAXDSK                                 *
*                                                                     *
* Function:  Returns the mode of the CMS R/W minidisk with the        *
*            most free space. Returns ' ' if none found.              *
*                                                                     *
* Called by:  CHARACTER*1 DISK                                        *
*             CALL MAXDSK(DISK,NFREE,ISIZE)                           *
*             DISK will be returned as the appropriate mode letter    *
*             or blank if none was found. When DISK is non-blank      *
*             NFREE and ISIZE will be returned as the number of       *
*             free blocks and the blocksize respectively.             *
*                                                                     *
*      Also:  CHARACTER*1 FUNCTION CMXDSK,DISK                        *
*             DISK = CMXDSK()                                         *
*                                                                     *
***********************************************************************
*                                                                     *
* Modification log:                                                   *
*                                                                     *
* 1.00 Original version   Tony Cass  CERN/DD   16th January 1989      *
*                                                                     *
***********************************************************************
*                                                                     *
         ENTRY CMXDSK
CMXDSK   DS    0H
         USING MAXDSK,R15
         USING NUCON,0
         USING ADTSECT,R1
         B     SAVEREGS
         DC    AL1(7),CL7'MAXDSK' Name
         DC    CL4'1.00'          Version.modification
SAVEREGS STM   R14,R12,12(R13)
         LR    R4,R13
         CNOP  0,4
         BAL   R13,CARRYON
SAVEAREA DC    18F'0'
         DROP  R15
         USING SAVEAREA,R13
CARRYON  ST    R13,8(,R4)
         ST    R4,R4(,R13)
         XC    8(4,R13),8(R13)
*
         LR    R11,R1             R11 -> parameter list
         L     R2,0(R11)          R2  -> reply area
         MVI   0(R2),C' '         Blank it out
*
         LA    R1,QUERY-24        Need ADTLKW's ?? function
         L     R15,AADTLKW
         BALR  R14,R15            Call the function
         BNZ   RETURN             No disk found
         MVC   0(1,R2),ADTM       Send back the mode
         TM    0(R11),X'80'       Any more arguments?
         BO    RETURN             No
         LA    R11,4(,R11)        Point to address of next arg
         L     R2,0(R11)          Get address
         L     R3,ADTNUM          Total blocks
         S     R3,ADTUSED         Subtract number used
         ST    R3,0(,R2)          Send back number free
         TM    0(R11),X'80'       Any more arguments?
         BO    RETURN             No
         LA    R11,4(,R11)        Point to address of next arg
         L     R2,0(R11)          Get address
         MVC   0(4,R2),ADTDBSIZ   Send back block size
*
RETURN   L     R13,4(,R13)
         LM    R14,R12,12(R13)
         OI    12(R13),X'FF'
         BR    R14
*
QUERY    DC    CL2'??'
*
         NUCON
         ADT
*
         REGEQU
         END
