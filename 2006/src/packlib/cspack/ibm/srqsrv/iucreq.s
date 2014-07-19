*
* $Id: iucreq.s,v 1.1.1.1 1996/03/08 15:44:22 mclareni Exp $
*
* $Log: iucreq.s,v $
* Revision 1.1.1.1  1996/03/08 15:44:22  mclareni
* Cspack
*
*
#if (defined(CERNLIB_IBMVM))&&(defined(CERNLIB_IUCVREQ))
***************************************************************
*                                                             *
*  Function   : Subroutine to pass data from an inpt array to *
*  ========     a server vm in any cpu, via IUCreq interface. *
*               Return reply into an output array, one record *
*               for each element of the array.                *
*                                                             *
*  Attributes : Fortran callable, re-entrant.                 *
*  ==========   Base version:                                 *
*                  For CMS                                    *
*                  System 370                                 *
*               H-Assembler.                                  *
*                                                             *
*  Format     : CALL SYSREQ(SERNAM,COMTXT,RCODE,REPTXT,ARYSZ) *
*  ======                                                     *
*                                                             *
*  Author     : Tony Cass  CERN CN/SW  May 1991               *
*  ======       This version steals much of its code from the *
*               version using the SYSreq interface - written  *
*               by Jonathan Wood/RAL.  Only the part dealing  *
*               with SYSreq/IUCreq communications is changed. *
*                                                             *
***************************************************************
SYSREQ   CSECT
#if defined(CERNLIB_QMIBMXA)
SYSREQ   AMODE  ANY
SYSREQ   RMODE  ANY
#endif
*
         USING SYSREQ,R15
         B     SAVEREGS
         DC    AL1(7),CL7'SYSREQ'
         DC    C'1.00'
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
         LR    R11,R1        * Save plist pointer
*
GETSTOR  LA    R0,WRKLEN               * Length of free core
         SLL   R0,3                    * Get it in bytes
         GETMAIN R,LV=(R0)             * Get the core
         LR    R8,R1                   * Base Workarea on R8
         USING WRKAREA,R8              * And say so
         ST    R8,BUFFADDR             * For passing to IUCREQ
*
*   Set up Defaults in case not fortran 77.
*
         LA    R3,SYSIDEF    * R3 = default input length
         LA    R5,SYSODEF    * R5 = default output length
         LA    R6,SYSSLEN    * R6 = max len server name
*
*   Check plist to see if it is Fortran 77
*
         LR    R1,R11        * Restore plist pointer
         LA    R2,8          * 8 bytes
         SR    R1,R2         * Back to possible prefix
         CLC   0(4,R1),CHAR77     * Is there a char 77 prefix?
         BNE   NOT77         * :No  - muddle along blindly
*
*   Fortran 77 - get lengths from secondary plist
*
IS77     L     R1,4(R1)      * :Yes - get secndry displacement
         AR    R1,R11        * Point to secondary plist
         L     R2,4(R1)      * Point to input array length
         LA    R3,SYSIMAX    * Max input length
         CL    R3,0(R2)      * Is it greater than max?
         BNH   GETPLEN       * :Yes - then use max
         L     R3,0(R2)      * :No  - then use actual
GETPLEN  DS    0H            * Get   output array length
         L     R5,12(R1)     * Point to output array
         L     R5,0(R5)      * R5 => Length each output item
CHKSLEN  DS    0H            * Check server name length
         L     R6,0(R1)      * Point to server name length
         CL    R1,0(R6)      * Is it greater than max?
         BNH   LENSOK        * :Yes - Then use max
         L     R6,0(R6)      * :No  - Then use true length
LENSOK   DS    0H
*
NOT77    DS    0H
*
         LA    R10,L'PREAMBLE(R3,R6) * Total length of IUCREQ command
         LA    R10,1(,R10)   * add space between server name & command
         BCTR  R6,0          * server length-1 for ex
         L     R2,0(R11)     * Point to server name
         LA    R1,SERVER     * Point at where we want it
         EX    R6,EXMOVE     * Move server name to scan in buffer
         LA    R1,1(R6,R1)   * Next free space in the buffer
         MVI   0(R1),C' '    * Put a blank there
         LA    R1,1(,R1)     * User command goes here...
         BCTR  R3,0          * command length-1 for ex
         L     R2,4(R11)     * Point to user command
         EX    R3,EXMOVE     * Move command to scan in buffer
*
*   Complete register initialisation for decoding reply
*
         LA    R2,REPLY      * R2 => Reply buffer start
         L     R4,12(R11)    * R4 => points to next output item
*                            * R5 => output item length
         XR    R6,R6         * R6 => output item count
         L     R7,16(R11)    * R7 => Max output item count
         L     R7,0(R7)      * Get max output items
*
*   Estimate Max reply data user can cope with
*
         LA    R1,SYSOMAX    * Max output record length
         CLR   R1,R5         * Is his item length bigger?
         BL    ESTOLEN       * :Yes - so use our one
         LR    R1,R5         * :No  - use his length
ESTOLEN  DS    0H
         LR    R12,R7        * Allow one line for possible
         BCTR  R12,0         *  continuation
         MR    R0,R12        * Estimate max space available
         LA    R1,REPLHLEN(R1)    * Add reply header to it
         LA    R0,TOVMMAXR   * Max avaiable reply buffer
         CLR   R0,R1         * Does user have more than us?
         BL    MAXRLEN       * :Yes - So use our maximum
         LR    R0,R1         * :No  - So use estimated maximum
MAXRLEN  STH   R0,BUFFLEN    * Store it in buffer
*
*   Scan the command, call IUCREQ and see what happens!
*
         SCAN  TEXT=(SCANIN,(R10)),BUFFER=(SCANOUT,256)
         MVC   USERINFO(8),=A(BUFFADDR)
         CMSCALL PLIST=(R1),EPLIST=(R0),UFLAGS=X'01'
*
*   Can't use ERROR since that moans about +ve return codes too!
*
         LTR   R15,R15
         BM    ERROR1
*
*  Communications handshake complete.  Now process reply
*
GOTREPLY DS    0H
         LH    R9,REPLYLEN   * Get reply buffer length
         LA    R1,REPLHLEN   * Get header length
         SR    R9,R1         * Account for reply head
         BNP   DORET         * Nothing left - don't print
         LA    R10,REPLY     * Get start of full data
         LA    R10,0(R9,R10) * R10 => pointer to end of reply
         LA    R2,REPLY      * R2  => pointer to reply record
*
*   Loop over reply, placing one record in each output item
*
DEBURST  DS    0H
         XR    R3,R3         * Clear for insert
         IC    R3,0(R2)      * R3 => Length this reply record
         LA    R2,1(R2)      * R2 => Pointer to reply Data
*
         BAL   R14,PUTREC    * Return record to user buffer
*   Registers 2,4,6 are correctly adjusted by PUTREC
         LTR   R15,R15       * What happenend?
         BZ    DEBURST       * All ok, more to go
*
*   Return to caller with SYSREQ return code
*
DORET    DS    0H            * All moved.  Set return
         LH    R15,REPLYRC   * Get rc from server
SETRET   L     R1,8(R11)     * Address of rc field
         ST    R15,0(R1)     * Set the return code
         L     R1,16(R11)    * Point to item count field
         ST    R6,0(R1)      * Set items returned
         B     RETURN        * Finish up and go home
*
*   Communications failure: SYSREQ error of some sort
*
ERROR1   DS    0H
         LA    R2,MSG1       * Special error msg
         LA    R3,L'MSG1     * Length of message
         BAL   R14,PUTREC    * Give it to caller
         LA    R15,1         * Set silly return code
         B     SETRET        * and give up
*
***************************************************************
* RETURN :     Routine to clean up and return to caller       *
***************************************************************
RETURN   DS    0H
         LA    R0,WRKLEN     * Length of work area
         SLL   R0,3          * Get it in bytes
         FREEMAIN R,LV=(R0),A=(R8)     * Free up the work area
         L     R13,4(,R13)
         LM    R0,R12,20(R13)     * Restore callers regist
         L     R14,12(R13)        * Get the return address
         OI    12(R13),X'FF'      * Fortran convention
         BR    R14                * Go home
*
*   MVC instruction EX'ed to put input parms in SCAN buffer
*
         DS    0D
EXMOVE   MVC   0(*-*,R1),0(R2)       ****** EXECUTED ******
*
         EJECT
***************************************************************
* PUTREC: Return a record to array element(s)                 *
*         Registers at entry:     R2 -> reply record data     *
*                                 R3 =  reply record length   *
*                                 R4 -> Next output item      *
*                                 R5 =  Output item length    *
*                                 R6 =  Item counter          *
*                                 R7 =  Max items             *
*                                 R10 -> End of reply buffer  *
***************************************************************
PUTREC   STM   R14,R12,12(R13)    * Save callers registers
*
*   Check if this is the last record
*
         XR    R15,R15       * Set up for rc=0
         LA    R9,0(R3,R2)   * Point end of this record
         CLR   R9,R10        * Is this  the last record?
         BL    NOTLAST       * :No  - Ok, as normal
         LA    R15,1         * Indicate last record
         LNR   R15,R15       * By setting up R15 -ve
NOTLAST  DS    0H
*
         LR    R0,R5         * Item length for restore
         LR    R9,R5         * R9 => Length to move each time
         LR    R1,R3         * R1 => Remaining length
*
PRTLP    DS    0H
         CLR   R6,R7         * End of output buffer?
         BL    MVCAN         * :No  - Ok then
         LA    R15,1         * :Yes - No space, set R15 +ve
         LNR   R6,R15        * Set item counter negative
         B     MVRET         * Return now
MVCAN    LA    R6,1(R6)      * :No  - Bump output item count
         SR    R1,R5         * Adjust remaining length
         BNM   MVDO          * Whole output item  to move
         AR    R9,R1         * Part output item to move
         ICM   R9,B'1000',BLANKS  * Padding byte
MVDO     DS    0H
         LR    R3,R9         * Length reply data this time
         MVCL  R4,R2         * Random storage walk
*
MVDUN    LR    R5,R0         * Restore output item length
         LTR   R1,R1         * Any data left?
         BP    PRTLP         * :Yes - go print it
*
MVRET    LM    R7,R12,48(R13)          * Restore registers
         L     R14,12(0,R13)           * Return address
         BR    R14                     * Return to caller
*
***************************************************************
* Data, Save, Work areas and constants                        *
***************************************************************
         DS    0D
*
SCANIN   DS    CL256
         ORG   SCANIN
PREAMBLE DC    C'IUCREQ SYSREQ '
SERVER   DS    0H
         ORG
SCANOUT  DS    CL256
USERINFO EQU   SCANOUT+12
*
BUFFADDR DC    F'0'                    Buffer address ...
BUFFLEN  DC    AL2(OBLEN)               and length to pass to IUCREQ
*
CHAR77   DC    C'BZ',X'0000' * Fort 77 BZ00 char indi
BLANKS   DC    C'    '       * Some useful blanks
MSG1     DC    C'SYSREQ Communication error.'
*
SYSIDEF  EQU   120
SYSODEF  EQU   80
SYSSLEN  EQU   8
SYSIMAX  EQU   256-L'PREAMBLE-8  * Bufflen - preamble - server len
SYSOMAX  EQU   240
*
         LTORG               * Literal pool
*
         REGEQU              * Register symbolic equates
***************************************************************
* WRKAREA      Free storage data area, containing all         *
*              non-reentrant data items and save areas        *
***************************************************************
WRKAREA  DSECT                    * Start of work area in free
*
OBLEN    EQU   2048               * Total ouput buffer length
*
         DS    0H                 * Reply buffer
REPLYBUF EQU   *
REPLYLEN DS    H                  * Length reply
REPLYRC  DS    H                  * Return code
REPLMGID DS    F                  * Message id
OHLEN    EQU   *-REPLYBUF         * Output header length
REPLY    DS    0CL(OBLEN-OHLEN)   * Reply record buffer
REPLTLEN DS    C                  * Reply record length
REPLYTXT DS    C                  * Reply record text
         DS    CL(OBLEN-OHLEN-2)  * Rest of reply buffer
REPLYEND EQU   *                  * End of the reply buffer
*
REPLHLEN EQU   REPLY-REPLYRC      * Length of TOVM header
TOVMMAXR EQU   REPLYEND-REPLYRC   * Max reply buffer length
*
WRKEND   DS    0D                 * Make whole number of dwords
WRKLEN   EQU   (*-WRKAREA)/8      * Length workarea (dwords)
         SPACE ,
***************************************************************
* End of Free storage work area                               *
***************************************************************
         EJECT ,
*
         END ,
#endif
