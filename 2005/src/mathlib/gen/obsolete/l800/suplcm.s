*
* $Id: suplcm.s,v 1.1.1.1 1996/04/01 15:03:19 mclareni Exp $
*
* $Log: suplcm.s,v $
* Revision 1.1.1.1  1996/04/01 15:03:19  mclareni
* Mathlib gen
*
*
#if defined(CERNLIB_NOSBE)
          IDENT    SUPLCM
          ENTRY    SUPLCM
*         SUBROUTINE SUPLCM(N,NTOT,IERR)
************************************************************************
*                                                                      *
**        FTN CALLABLE ROUTINE TO REQUEST USER LCM                     *
*                                                                      *
**        CALLING SEQUENCE                                             *
*         CALL SUPLCM(N,NTOT,IERR)                                     *
*                                                                      *
**        PARAMETERS                                                   *
*         N - IS REQUESTED AMOUNT OF LCM                               *
*          IF N - IS INTEGER ZERO THEN THE CURRENT FLL IS RETURNED     *
*                 IN NTOT BY RETURN TO CALLER                          *
*          IF N - IS POSITIVE INTEGER THE FLL IS INCREASED BY THE      *
*                 REQUESTED AMOUNT UNLESS GT MAXLCM (DYNAMIC),AND      *
*                 NEW FLL IS RETURNED IN NTOT BY RETURN. SHOULD THE    *
*                 REQUESTED AMOUNT EXCEED MAXFLL THEN IERR BY RETURN   *
*                 CONTAINS THE DIFFERENS. I.E. WHAT WAS NOT ALLOCATED. *
*          IF N - IS A NEGATIVE INTEGER THEN FLL IS DECREASED BY N     *
*                 AND THE REMAINING FLL IS RETURNED IN NTOT. SHOULD    *
*                 FLL REQUESTED IS L.T. ZERO, THEN FLL IS DEALLOCATED  *
*                 TO ZERO AND NTOT IS ZERO BY RETURN.                  *
*         NTOT - CONTAINS BY RETURN FROM SUPLCM THE CURRENT FLL.       *
*         IERR - CONTAINS BY RETURN FROM SUPLCM THE DIFFERENS BETWEEN  *
*                REQUESTED ALLOCATION OF LCM AND ACHIEVED FLL.         *
*                IF IERR IS ZERO THEN ALL REQUESTED WAS ALLOCATED      *
*                AND IF POSITIVE THE ACTUAL POSITIVE VALUE WAS NOT     *
*                ALLOCATED. IERR SHOULD ALWAYS BE POSITIVE INTEGER.    *
*                                                                      *
**        RESTRICTIONS                                                 *
*         ALL PARAMETERS MUST RECIDE IN SCM , ELSE SUPLCM WILL         *
*         ABORT JOB WITH DAYFILE MESSAGE .                             *
*                                                                      *
*         MAX FLL IS AN INSTALLATION PARAMETER BUT SUPLCM FINDS        *
*         OUT THE ACTUAL RUNNING VALUE.                                *
*                                                                      *
*         IF N IS FLOATING SUPLCM ABORTS JOB WITH DAYFILE MESSAGE.     *
*                                                                      *
*         ALL THREE PARAMETERS MUST BE PRESENT IN A CALL TO SUPLCM,    *
*         IF NOT SUPLCM ABORTS JOB WITH DAYFILE MESSAGE.               *
*                                                                      *
**        DAYFILE MESSAGES                                             *
*         SUPLCM - PARAMETER ERROR , GIVEN FOR BAD CALL                *
*                                                                      *
************************************************************************
TRACE.    VFD      42/0HSUPLCM,18/SUPLCM
*
REQFLL    BSS      1
LCMFLL    BSS      1
IERR      BSS      1
MESS      DIS      ,*  SUPLCM - PARAMETER ERROR*
MAXLCM    VFD      30/7777777776B,30/0
*
SUPLCM    DATA     0
          ZR       X1,ERROR        NO PARAMETERS AT ALL IN CALL
          NG       X1,ERROR        NO LCM RESIDENT PARAMETERS ALLOWED
          SA2      X1              GET VALUE OF N
          SA3      A1+1            GET ADDRESS OF NTOT
          SA4      A1+2            GET ADDRESS OF IERR
          MX6      0
          MX7      0
          SA6      REQFLL
          SA7      IERR
          SA6      LCMFLL
          ZR       X3,ERROR        NO SECOND PARAMETER
          NG       X3,ERROR        NO LCM RESIDENT PARAMETERS ALLOWED
          ZR       X4,ERROR        NO THIRD PARAMETER
          NG       X4,ERROR        NO LCM RESIDENT PARAMETERS ALLOWED
          SB2      X3              SAVE ADDRESS OF NTOT
          SB3      X4              SAVE ADDRESS OF IERR
          BX0    X2          SAVE X2 MEMORY DESTROYS X1 X2 X6 A6
          SA1    MAXLCM
          PL     X1,SETUP    AREADY DONE
          MEMORY LCM,MAXLCM,RECALL
          SA1    MAXLCM      GET MAX LCM AVAILABLE
          MX6    30D
          BX6    X6*X1
          LX6    30D
          SA6    A1          SET MAXIMUM LCM FIELD LENGTH
 SETUP    BSS    0
          MEMORY   LCM,LCMFLL,RECALL GET CURRENT FLL
          BX2    X0          RESTORE X2 DESTROYED BY MEMORY MACRO
          SA1      LCMFLL
          MX6      30D
          BX1      X1*X6
          LX1      30D
          ZR       X2,FINI
          SA3      MAXLCM
          MX5      12D
          BX4      X5*X2
          PL       X2,POS          SKIP IF N IS POSITIVE
          BX4      X5-X4           ONLY SIGN BITS ALLOWED
POS       BSS      0
          NZ       X4,ERROR        N IS FLOATING
          IX6      X2+X1           X6= NEEDED FLL
          ZR     X6,NONE           DEALLOCATE ALL LCM
          PL     X6,SOME           LCM IS NEEDED
NONE      BSS    0
          MX6    30D
          SA6    REQFLL
          MX1    0                 NTOT RESULT IS ZERO
          EQ     MEM               GO DEALLOCATE ALL LCM
SOME      BSS    0
          IX7      X6-X3           CHECK IF TOO MUCH
          NG       X7,SKIP
          SA7      IERR            SAVE DIFFERENCE
          BX6      X3              REQUEST MAXIMUM
SKIP      BSS      0
          BX1      X6
          LX6      30D
          SA6      REQFLL
MEM       BSS    0
          BX0    X1
          MEMORY   LCM,REQFLL,RECALL
          BX1    X0
FINI      BSS      0
          SA2      IERR
          BX6      X1
          SA6      B2              WRITE NTOT RESULT
          BX7      X2
          SA7      B3              WRITE IERR RESULT
          EQ       SUPLCM
ERROR     BSS      0
          MESSAGE  MESS
          ABORT
          END
#endif
