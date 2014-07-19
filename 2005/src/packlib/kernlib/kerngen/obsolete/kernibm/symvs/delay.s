*
* $Id: delay.s,v 1.1.1.1 1996/02/15 17:53:19 mclareni Exp $
*
* $Log: delay.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:19  mclareni
* Kernlib
*
*
DELAY    CSECT
*
* DELAY      Wait for some real time and then return to caller
*
* USAGE:     CALL  DELAY (NHSEC)
*
*            NHSEC  Input,  INTEGER, number of time units to wait
*                                    time unit is 26 usec
*
* Restrictions: Usage mutually exclusive with that of TMLOG
*
* Author:    D. Moenkemeyer                           March     1981
*
* ORIGINAL UNEXPANDED FOLLOWS
*DELAY    BEG   *,MODE=RENT,RMODE=ANY
*         L     2,0(1)               GET ADDRESS OF TIMER UNITS
*         STIMER WAIT,TUINTVL=(2)    WAIT
*         SCHL  RET=0,MODE=RENT
*         END
*
DELAY    RMODE ANY
DELAY    AMODE ANY
         ENTRY DELAY
         USING *,12
         B     10(0,15)            BRANCH AROUND ID
         DC    AL1(5)              LENGTH OF ID
         DC    CL5'DELAY'
         STM   14,12,12(13)        SAVE CALLER'S REGISTER
         LR    12,15               LOAD BASE
         GETMAIN R,LV=72
         ST    13,4(1)             BACKWARD CHAIN
         ST    1,8(13)             FORWARD CHAIN
         LR    15,13
         LR    13,1
         LM    15,1,16(15)         RESTORE ALL REGISTER
         L     2,0(1)              GET ADDRESS OF TIMER UNITS
         STIMER WAIT,TUINTVL=(2)   WAIT
         LR    1,13
         L     13,4(13)            RELOAD SA-REG
         FREEMAIN R,LV=72,A=(1)
         LM    14,12,12(13)        RESTORE CALLER'S REGISTER
         XR    15,15
         BR    14                  RETURN TO CALLER
         END
