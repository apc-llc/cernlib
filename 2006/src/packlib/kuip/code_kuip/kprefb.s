*
* $Id: kprefb.s,v 1.1.1.1 1996/03/08 15:32:53 mclareni Exp $
*
* $Log: kprefb.s,v $
* Revision 1.1.1.1  1996/03/08 15:32:53  mclareni
* Kuip
*
*
#if defined(CERNLIB_IBMMVS)
*CMZ :  2.05/02 11/05/94  09.50.52  by  Alfred Nathaniel
*-- Author :
*
*         KPREFB PROGRAM
*              GIVES SOME JOB CARD PARAMETERS IN BATCH MODE
*
*         AUTHOR:
*              G. WEIL      CENTRE DE CALCUL DE STRASBOURG (CNRS)
*
*         CALLING SEQUENCE:
*                   CALL KPREFB ( JOBID )
*
*         JOBID     CHARACTER*8 = JOB NAME                  (OUTPUT)
*
*
KPREFB    START      0
R0        EQU       0
R1        EQU       1
R2        EQU       2
R3        EQU       3
R4        EQU       4
R5        EQU       5
R6        EQU       6
R7        EQU       7
R8        EQU       8
R9        EQU       9
R10       EQU       10
R11       EQU       11
R12       EQU       12
R13       EQU       13
R14       EQU       14
R15       EQU       15
*
          USING     *,R12
          SAVE      (14,12)
          LR        R12,R15
          LR        R11,R13
          LA        R13,OUTSAVE
          ST        R11,4(R13)
          ST        R13,8(R11)
*
          LR        R11,R1                     LISTE ARGUMENTS
          EXTRACT   ADR,'S',FIELDS=TIOT
          L         R7,ADR
          USING     TIOT1,R7
          L         R10,0(R11)                 ARG JOB ID
          MVC       0(8,R10),TIOCNJOB
*         L         R10,4(R11)                 ARG COMPTA
*         MVC       0(4,R10),TIOCNJOB
*         L         R10,8(R11)                 3IEM ARGUMENT
*         MVC       0(8,R10),=CL8'DUMMY   '
*
          L         R13,4(R13)
          RETURN    (14,12),T,RC=0
*
ADR       DS        2F
OUTSAVE   DS        18F
          PRINT     ON,NOGEN
          IKJTCB    DSECT=YES,LIST=NO
          IEZJSCB
TIOTSECT  DSECT
          IEFTIOT1
          END
#endif
