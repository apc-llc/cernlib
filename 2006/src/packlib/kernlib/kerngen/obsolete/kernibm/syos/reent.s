*
* $Id: reent.s,v 1.1.1.1 1996/02/15 17:53:22 mclareni Exp $
*
* $Log: reent.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:22  mclareni
* Kernlib
*
*
REENT    CSECT
*
* CERN PROGLIB# Z035    REENT           .VERSION KERNIBM  2.32  910718
*
*     RECOVER FROM FATAL ERROR STOP (0C1 TO 0C6) FOR DIAGNOSTIC DUMP
*     INITIALIZE RECOVERY WITH
*                  EXTERNAL QFATAL  (FOR EXAMPLE)
*                  CALL REENT (QFATAL)
*     WHEN AN ERROR OCCURS, CONTROL IS TRANSFERRED TO QFATAL.
*     THIS ROUTINE MAY THEN PRINT ANY TERMINATION INFORMATION AND IT
*     MUST STOP WITH         CALL ABEND
*
*                  HAMBURG, AUGUST 1973  (H.BUTENSCHOEN)
*
*  CORRECTION FOR MVS/XA OR VM/XA :
*        There was a problem at DESY: REENT specifies AMODE ANY to be
*        able to run "above the line". But it calls the SPIE macro
*        which, with MVS/XA, canNOT run up there.
*        There fore the code was modified by Klaus Tietgen, DESY-R1.
*---------------------------------------------------------------------
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
REENT    AMODE ANY
REENT    RMODE ANY
#endif
         ENTRY ABEND
         USING *,10
         SAVE  (14,12),,*
         LR    10,15
         L     3,0(1)
         LA    3,0(3)
         L     3,0(3)
#if defined(CERNLIB_QMIBMXA)
         BSM   1,0
         LA    0,1
         SLL   0,31
         NR    1,0
         OR    3,1
         ST    3,QF  ADDRESS OF EXTERNAL ROUTINE INTO QF
         LA    3,SPEXIT
         ESPIE SET,(3),((1,6))
         ST    1,PICAAD
         RETURN (14,12)
         USING *,12
ABEND    SAVE  (14,12),,*
         LR    12,15
         ABEND 1,DUMP
SPEXIT   DS    0H
         DROP
         USING *,15
         STM   0,15,SPREG
         MVC   PI(96),0(1)
         L     3,QF
         ST    3,76(1)
         BR    14
PI       DC    96X'0',CL2'PI'
#endif
#if !defined(CERNLIB_QMIBMXA)
         ST    3,QF  ADDRESS OF EXTERNAL ROUTINE INTO QF
         SPIE  SPEXIT,((1,6))
         ST    1,PICAAD
         B     EXIT
EXIT     RETURN (14,12)
SPEXIT   BALR  10,0
         USING *,10
         STM   0,15,SPREG
         MVC   PI(36),0(1)
         L     3,QF
         ST    3,8(1)
         ST    3,16(1)
         BR    14
         USING *,12
ABEND    SAVE  (14,12),,*
         LR    12,15
         ABEND 1
PI       DC    36X'0',CL2'PI'
#endif
SPREG    DC    16F'0'
PICAAD   DC    F'0'
QF       DC    F'0'
         END
#ifdef CERNLIB_TCGEN_ABEND
#undef CERNLIB_TCGEN_ABEND
#endif
