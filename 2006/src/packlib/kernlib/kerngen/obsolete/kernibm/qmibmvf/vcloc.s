*
* $Id: vcloc.s,v 1.1.1.1 1996/02/15 17:53:05 mclareni Exp $
*
* $Log: vcloc.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:05  mclareni
* Kernlib
*
*
VCLOC    CSECT                    CALLING SEQUENCE
* CERN PROGLIB# Z004    VCLOC           .VERSION KERNIBM  2.30  900508
* ORIG. 13/03/90  C.GUERIN, IBM
*
#if defined(CERNLIB_QMIBMXA)
VCLOC    AMODE 31
VCLOC    RMODE ANY
#endif
*****************************************************************
* Read the system clocks an return the result in microseconds   *
*                                                               *
* The format is CALL VCLOC ( TIME)                              *
*                                                               *
*    TIME(1) = elapsed time                                     *
*    TIME(2) = total cpu time                                   *
*    TIME(3) = virtual cpu time                                 *
*    TIME(4) = vector cpu time                                  *
*                                                               *
* where TIME is declared REAL*8 TIME(4)                         *
*****************************************************************
         USING *,15               CALL VCLOC (TIME)
         L     1,0(1)             GET ADDRESS OF ARG
************** ELAPSED TIME IN MICROSECONDS
         STCK  1(1)               STORE TOD SHIFTED 1 BYTE
         MVI   0(1),X'4D'         MAKE RESULT FLOATING POINT
************** VECTOR ACTIVITY IN MICROSECONDS
         VACSV ITIME              STORE VECTOR ACTIVITY
         MVI   ITIME,X'4B'        MAKE RESULT FLOATING POINT
         MVC   24(8,1),ITIME      VECTOR TIME TO RESULT
************** CPU TIME IN MICROSECONDS
         LA    0,ITIME            ADDRESS OF 4 DOUBLE WORDS
         DC    X'8300000C'        GET  TOTAL AND VIRTUAL CPU TIME
         MVI   ITIME+16,X'4E'     MAKE VIRTUAL TIME FLOATING POINT
         MVI   ITIME+24,X'4E'     MAKE TOTAL TIME FLOATING POINT
         MVC   16(8,1),ITIME+16   VIRTUAL CPU TIME TO RESULT
         MVC   8(8,1),ITIME+24    TOTAL CPU TIME TO RESULT
************** RETURN
         BR    14
         DS    0D
ITIME    DS    4D
         END
