*
* $Id: rjctb.s,v 1.1.1.1 1996/03/21 17:19:59 mclareni Exp $
*
* $Log: rjctb.s,v $
* Revision 1.1.1.1  1996/03/21 17:19:59  mclareni
* Bvsl
*
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMVF))
**********************************************************************
*                                                                    *
*     SUBROUTINE RJCTB(RAN,XIN,FREJ,XOUT,INDIN,NW,NWOUT,ISWTCH)      *
*                                                                    *
*        PERFORM REJECTION TECHNIQUE IN VECTOR MODE                  *
*        AND WITH MINIMUM RANDOM NUMBERS GENERATION                  *
*                                                                    *
*                                                                    *
**********************************************************************
*     DIMENSION RAN(1000),XIN(1000),XOUT(1000),INDOUT(1000)
*    +,FREJ(1000),INDIN(1000)
*     J=0
*     DO 10 I=1, NW
*       IF(RAN(INDIN(I)).LT.FREJ(INDIN(I)))THEN
*          XOUT(INDIN(I))=XIN(INDIN(I))
*       ELSE
*         J=J+1
*         INDOUT(J)=INDIN(I)
*       ENDIF
*10   CONTINUE
*     NW=J
*     RETURN
*     END
**********************************************************************
RJCTB   CSECT
#if defined(CERNLIB_QMIBMXA)
RJCTB   AMODE 31
RJCTB   RMODE ANY
#endif
         USING *,15
         STM   14,12,12(13)
* R1 RAN, R2 XIN, R3 FREJ, R4 XOUT, R5 INDIN , R6 NW
         LM    1,8,0(1)
         L     9,0(0,6)
* IF FIRST STEP OR GATHER
* TEST SWITCH
         LA    10,1
         S     10,0(0,8)
         BC    6,SWITCH
*  NO INDEX  FIRST CALL OR GATHER
         SR    10,10
*
LOOP0    VLVCU 9
         VLE   3,3
         VLE   2,2
         VCE   2,3,1
         VSTME 2,4
         VCVM
         VCOVM 10
         VSTVM 5
*
         LTR   9,9
         BC    2,LOOP0
         ST    10,0(0,7)
         LA    10,2
         ST    10,0(0,8)
         LM    14,12,12(13)
         BR    14
*
*
* WITH INDEX
SWITCH   SR    10,10
         LR    11,5
         LR    12,5
         LR    14,5
*
LOOP1    VLVCU 9
         VLVM  5
         VLYE  1,1(0)
         VCE   4,1,3
         VNVM  11
         VLME  2,2
         VSTME 2,4
         VXVM  12
         VCOVM 10
         VSTVM 14
*
         LTR   9,9
         BC    2,LOOP1
         ST    10,0(0,7)
         LA    0,2
         ST    0,0(0,8)
         LM    14,12,12(13)
         BR    14
*
         END
#endif
