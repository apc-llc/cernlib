*
* $Id: normfact.inc,v 1.1.1.1 1996/02/15 17:51:56 mclareni Exp $
*
* $Log: normfact.inc,v $
* Revision 1.1.1.1  1996/02/15 17:51:56  mclareni
* Kernlib
*
*
*    CPU factor at CERNVM
*
* normfact.inc
*
#if defined(CERNLIB_CERN)
#if (defined(CERNLIB_CERN))&&(!defined(CERNLIB_CERNVMB))
NORMFACT DC    D'20.0'
#endif
#endif
*    CPU factor at CERNVMB
#if defined(CERNLIB_CERN)
#if (defined(CERNLIB_CERN))&&(defined(CERNLIB_CERNVMB))
NORMFACT DC    D'5.0'
#endif
#endif
*                        CPU factor elsewhere
#if defined(CERNLIB_CERN)
NORMFACT DC    D'1.0'
#endif
         PRINT NOGEN
JOB$VM   CSECT
#if defined(CERNLIB_QMIBMXA)
JOB$VM   AMODE ANY
JOB$VM   RMODE ANY
#endif
*
***********************************************************************
*                                                                     *
* CERN Program Library Z307    JOB$VM                                 *
*                                                                     *
* Function:  Returns various details about the current CMS            *
*            environment for HepVM sites.                             *
*                                                                     *
* Usage Example:                                                      *
*                                                                     *
*      DIMENSION SPACE(32)                                            *
*      LOGICAL BATCH,JOB$VM                                           *
*      CHARACTER*8 INFO(10)                                           *
*      EQUIVALENCE (INFO(1),SPACE(1))                                 *
*      EQUIVALENCE (TIME,SPACE(21))                                   *
*      EQUIVALENCE (TNORM,SPACE(23))                                  *
*      EQUIVALENCE (MACH,SPACE(25))                                   *
*C                                                                    *
*      BATCH = JOB$VM(SPACE)                                          *
*                                                                     *
*                                                                     *
* The information returned is:                                        *
*                                                                     *
*    BATCH = .TRUE.  if the job is running under SLAC Batch           *
*            .FALSE. otherwise                                        *
*                                                                     *
*   INFO(1 ) =  (Batch job submitter's) user ID                       *
*   INFO(2 ) =  Account code                                          *
*   INFO(3 ) =  Distribution code                                     *
*   INFO(4 ) =  Machine ID (= user ID if interactive)                 *
*   INFO(5 ) =  Job ID (INTERACT if interactive)                      *
*   INFO(6 ) =  Virtual Machine Storage size                          *
*   INFO(7 ) =  START/RESTART for a batch job                         *
*   INFO(8 ) =  Node                                                  *
*   INFO(9 ) =  BMON ID                                               *
*   INFO(10) =  Master BMON ID                                        *
*                                                                     *
*   TIME    =  Time limit for job in accounting units                 *
*   TNORM   =  Conversion factor between accounting and machine units *
*              TNORM = (accounting second) / (machine second)         *
*   MACH    =  0  ->  Machine type unknown                            *
*              1  ->  370 Machine                                     *
*              2  ->   XA Machine                                     *
*                                                                     *
***********************************************************************
*                                                                     *
*   Information is gleaned from the results of a $B$INFO$ query and   *
*   a call to the HepVM EXTRACT Diagnose function.                    *
*   NODE comes from CP QUERY USERID for an interactive machine        *
*                                                                     *
*   Virtual Machine type determined by examining NUCMFLAG (previously *
*   a reserved field). $B$INFO$ called by SVC 202 or SVC 204 as       *
*   appropriate.                                                      *
*                                                                     *
*                                                                     *
***********************************************************************
*                                                                     *
* Modification log:                                                   *
*                                                                     *
* 2.00 Drastically rewritten for public release. This code should     *
*      now run correctly on all Hep$VM sites.                         *
*      Simple Fortran code in Patch SYVM for other sites - however    *
*      those using SLAC batch but without the HepVM EXTRACT code      *
*      may prefer just to avoid the DIAGNOSE instruction.             *
*                                                                     *
* 2.01 Clear R15 before OR before SVC 204                             *
*      Allow for 1digit time limits                                   *
*                                                                     *
* 2.02 Fix storage and Node determination for non-XA systems          *
*                                                                     *
* Tony Cass  CERN/DD   8th March 1989                                 *
*      Acknowledgments to H. Renshall/CERN and J. Wood/RAL            *
*                                                                     *
***********************************************************************
*                                                                     *
         USING JOB$VM,R15
         B     SAVEREGS
         DC    AL1(7),CL7'JOB$VM'    Identifier
         DC    C'2.02'               Version.Modification level
SAVEREGS STM   R14,R12,12(R13)
         LR    R4,R13
         CNOP  0,4
         BAL   R13,AROUNDSA
SAVEAREA DC    18F'0'
         DROP  R15
         USING SAVEAREA,R13
AROUNDSA ST    R13,8(0,R4)
         ST    R4,4(0,R13)
         XC    8(4,R13),8(R13)
*
         L     R11,0(0,R1)           R11 -> Argument address
*
         TM    JOBFLAGS,CALLED       Already done the work?
         BO    COPYDATA              Yes, just copy the info
*
         LM    R5,R8,SIZEDIAG        First find virtual storage
         DIAG  R5,R7,X'0008'
         MVC   VSTORE(5),RETSTORE+10 Copy over size
         CLI   RETSTORE+14,C'M'      Pre XA response?
         BE    WHATTYPE              No, got all the details
         MVI   VSTORE+5,C'K'         Yes, give storage units
*
WHATTYPE TM    X'5EA'(0),X'80'       XA/370? (No mnemonics to allow
         BO    MACHXA                 for earlier CMSes
*
MACH370  MVI   MACHTYPE+3,ITS370
         LA    R1,$B$PLIST
         ICM   R1,B'1000',=X'05'     Want nucleus extension
         SVC   202
         DC    AL4(1)
         B     MACHALL
*
MACHXA   MVI   MACHTYPE+3,ITSXA
         LA    R1,$B$PLIST           Expand CMSCALL for non CMS 5.5
         LA    R15,0
         O     R15,=X'0000A000'
         SVC   204
*
MACHALL  LTR   R15,R15
         BM    NOTBATCH              Not in Slac batch
*
         OI    INBATCH+3,X'01'       In Slac Batch
*
         USING BATINFO,R15
*                                    Copy interesting fields over
         MVC   USERID,BATUSER
         MVC   ACCOUNT,BATACNT
         MVC   DISTCODE,BATBIN
         MVC   JOBID,BATJOBID
         MVC   RESTART,BATSTRST
         MVC   NODE,BATNODE
         MVC   BMONID,BATMONID
         MVC   BMASTER,BATMASTR
*
         LA    R9,8                  Find end of time limit
TESTLIM  LA    R8,BATMXTME-1(R9)
         CLI   0(R8),C' '
         BNE   PACKLIM
         BCT   R9,TESTLIM
         B     EXTRACT               Zero length limit is pretty silly
*
IPACKLIM PACK  PKTLIM,BATMXTME(*-*)
IPACKNRM PACK  PKTNORM,BATTNORM(*-*)
*
PACKLIM  BCTR  R9,0
         EX    R9,IPACKLIM           Pack time limit
         CVB   R9,PKTLIM              and convert to binary
*
         LA    R10,7                 Find end of normalisation factor
TESTNORM LA    R8,BATTNORM(R10)
         CLI   0(R8),C' '
         BNE   PACKNORM
         BCT   R10,TESTNORM
         LA    R10,1                 Make norm factor 1.0
         B     FLOATEM
*
PACKNORM EX    R10,IPACKNRM          Pack normalisation factor
         CVB   R10,PKTNORM            and convert to binary
*
FLOATEM  ST    R9,REALLIM+4          Make unnormalised REAL*8 values
         ST    R10,REALNORM+4         for time limit and norm. factor
         LD    F0,REALLIM
         AD    F0,ZERO               Normalise limit
         STD   F0,TIMELIM             and store it
         LD    F0,REALNORM
         AD    F0,ZERO               Normalise normalisation factor(!)
         DD    F0,=D'100.0'           scaled by 100 by $B$INFO$
         STE   F0,NORMFACT             and store it
         B     EXTRACT
*
NOTBATCH LM    R5,R8,NODEDIAG        Set up registers for DIAG 8
         DIAG  R5,R7,X'0008'         Issue command
#if defined(CERNLIB_QMIBMXA)
         BCTR  R8,0                  Point at x'15' added by CP
         LA    R6,0(R8,R6)
         MVI   0(R6),C' '            And replace by a blank
         MVC   NODE,INTNODE          Copy node name across
#endif
#if !defined(CERNLIB_QMIBMXA)
         S     R8,=F'3'              Skip x'15' and node must be
*                                     at least 1 character long
         LA    R6,0(R8,R6)
         LA    R5,0                  Look backwards for
         IC    R7,=C' '               a space
NODECOMP CLM   R7,B'0001',0(R6)
         BE    NODEMOVE
         LA    R5,1(,R5)
         BCT   R6,NODECOMP
*
EXMOVE   MVC   NODE(*-*),1(R6)
*
NODEMOVE EX    R5,EXMOVE             And replace by a blank
#endif
*
EXTRACT  LA    R1,EXTRLIST           Find details from CP
         LA    R2,NKEYS
#if !defined(CERNLIB_QEXTRACT)
         DIAG  R1,R2,X'0180'         HepVM Extract
*
         BC    1,COPYDATA            Extract failed or not available
*
         MVC   USERID,EXTRNAME       Copy over useful details
         MVC   MACHINE,EXTRMACH
         MVC   ACCOUNT,EXTRACNT
         MVC   DISTCODE,EXTRDIST
#endif
COPYDATA MVC   0(128,R11),USERID     Return the information
         L     R0,INBATCH            Return function value
         OI    JOBFLAGS,CALLED       Say we've already been called
RETURN   L     R13,4(0,R13)
         LM    R14,R15,12(R13)
         LM    R1,R12,24(R13)
         MVI   12(R13),X'FF'
         BR    R14
*
         DS    0D
*
* PLIST for call to $B$INFO$ command via SVC 202 or SVC 204
*
$B$PLIST DC    CL8'$B$INFO$'
         DC    8X'FF'
*
* Extract parameter list & work area
*
EXTRLIST DC    CL8'USER'
EXTRMACH DC    CL8' '
         DC    CL8'USERNAME'
EXTRNAME DC    CL8' '
         DC    CL8'ACCOUNT'
EXTRACNT DC    CL8' '
         DC    CL8'DISTCODE'
EXTRDIST DC    CL8' '
NKEYS    EQU   (*-EXTRLIST)/16
*
* Default answers or completed info for future calls
*
USERID   DC    CL8' '
ACCOUNT  DC    CL8' '
DISTCODE DC    CL8' '
MACHINE  DC    CL8' '
JOBID    DC    CL8'INTERACT'
VSTORE   DC    CL8' '
RESTART  DC    CL8' '
NODE     DC    CL8' '
BMONID   DC    CL8' '
BMASTER  DC    CL8' '
TIMELIM  DC    D'9999.0'
*  The following sequence is used to set the CPU normalization factor
*  in JOB$VM. At CERNVM (IBM 9000/900 ) accounted CPU =20 x native CPU
*                       (IBM 3090/600J) accounted CPU = 8 x native CPU
*                       (IBM 3090/600E) accounted CPU = 5 x native CPU
#include "normfact.inc"
MACHTYPE DC    2F'0'
         DC    6F'0'
ITS370   EQU   X'01'
ITSXA    EQU   X'02'
*
PKTLIM   DC    D'0.0'
REALLIM  DC    XL4'4E000000',F'0'
PKTNORM  DC    D'0.0'
REALNORM DC    XL4'4E000000',F'0'
ZERO     DC    D'0.0'
*
INBATCH  DC    F'0'
JOBFLAGS DC    X'00'
CALLED   EQU   X'01'
*
         DS    0D                  DIAG 08 for Storage Query
SIZEDIAG DC    A(QVSTORE)          Rx   -> command
         DC    A(RETSTORE)         Rx+1 -> Return buffer
         DC    XL4'40000015'       Ry   =  x'40' flag + L'command
#if defined(CERNLIB_QMIBMXA)
         DC    F'16'               Ry+1 =  L'return buffer
QVSTORE  DC    CL21'QUERY VIRTUAL STORAGE'
RETSTORE DC    CL16' '
#endif
#if !defined(CERNLIB_QMIBMXA)
         DC    F'20'               Ry+1 =  L'return buffer
QVSTORE  DC    CL21'QUERY VIRTUAL STORAGE'
RETSTORE DC    CL20' '
#endif
*
         DS    0D                  For DIAG 08 if interactive
NODEDIAG DC    A(QUSERID)          Rx   -> command
         DC    A(DIAG8BUF)         Rx+1 -> Return buffer
         DC    XL4'4000000C'       Ry   =  x'40' flag + L'command
         DC    F'20'               Ry+1 =  L'return buffer
QUSERID  DC    CL12'QUERY USERID'
DIAG8BUF DC    CL12' '
INTNODE  DC    CL8' '
*
         LTORG
*
BATINFO  DSECT
BATUSER  DS    CL8                 SENDER'S USERID
BATACNT  DS    CL8                 SENDER'S ACCOUNT
BATBIN   DS    CL8                 SENDER'S DIST CODE
BATJOBID DS    CL8                 JOB ID
BATMONID DS    CL8                 BMON ID
BATMASTR DS    CL8                 MASTER MONITOR ID, OR "*"
BATNODE  DS    CL8                 MASTER NODEID, OR "*"
BATSTRST DS    CL8                 START/RESTART
BATMXTME DS    CL8                 MAX CPU TIME (NORMALISED SECONDS)
BATTNORM DS    CL8                 100*CPU TIME NORMALISATION FACTOR
*
         REGEQU
         END
