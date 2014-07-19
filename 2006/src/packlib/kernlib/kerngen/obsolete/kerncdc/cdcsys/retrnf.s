*
* $Id: retrnf.s,v 1.1.1.1 1996/02/15 17:51:36 mclareni Exp $
*
* $Log: retrnf.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:36  mclareni
* Kernlib
*
*
          IDENT RETRNF
*
* CERN PROGLIB# K510    RETRNF          .VERSION KERNCDC  1.16  820129
*
          ENTRY RETRNF
          SST
*
*     SUBROUTINE RETRNF (LUN,NERROR)
*        TO RETURN A FILE DURING EXECUTION  (USEFULL FOR P.F.)
*
*             LUN IS THE LOGICAL UNIT NR OF THE FILE INVOLVED
*             NERROR IS THE RETURN CODE (OPTIONAL)
*                  IF NOT SUPPLIED, RETRNF WILL ABORT IF A RM ERROR OCCU
*                  IF SUPPLIED, IT RETURNS THE RM ERROR IF ANY ( 0 OTHER
*
*     NEW VERSION OF PROGRAM  K510  TAKEN FROM CERN PROGLIB 29-APR-78
*
#include "defmodel.inc"
*
 TRACE.   VFD   42/0HRETRNF,18/RETRNF
 TEMPA0   BSS   1
 FICF     BSS   1
 FILT     BSS   1
 ADRC     BSSZ  1                  TO SAVE RC ADDRESS
 RC       BSS   1                  TO SAVE RC
 MSG      DATA  H*RETRNF - RM ERROR =*  *BL
 RMNR     DATA  H*      ON *            *BL
 LFN      BSSZ  1
 #NEVOPN# EQU   0
*
 END      BSS   0
 #MD      IFC    LT, "MODEL" 75
          SX1   A0                 RESTORE X1 DESTROYED BY 6RM
 #MD      ENDIF
          STORE X1,RC=0
          STORE X1,BN=0
 #MD      IFC    GT, "MODEL" 75
          SA4   FILT
          NZ    X4,NOLT            JUMP, FILE WAS NOT LABELLED
          LABEL X1
 NOLT     BSS   0
 #MD      ENDIF
          SA4   FICF
          STORE X1,CF=X4           RESTORE THE ORIGINAL CF IN THE FIT
          STORE X1,OC=NEVOPN       SET FIOC TO NEVER OPENED
          SA5   TEMPA0
          SA2   ADRC
          SA0   X5
          ZR    X2,RETRNF          NO RC, RETURN
          SA3   RC
          BX6   X3
          SA6   X2                 RETURN RC
*
 RETRNF   EQ    *+1BS17
          SX6   A0
          MX7   0
          SA6   TEMPA0
          SA2   A1+1           X2= ADDRESS OF NERROR
          BX6   X2
          SA7   RC                 PRESET RC TO ZERO
          SA6   ADRC               SAVE ADDRESS OF RC
          MX6   1
          BX1   X1+X6
 +        RJ    =XGETFIT.
 -        VFD   30/TRACE.
 #MD      IFC    LT, "MODEL" 75
          SA0   X1                 SAVE X1 WHICH WILL BE DESTROYED BY 6R
 #MD      ENDIF
          STORE X1,EX=RMERROR
          FETCH X1,CF,X6
          SA6   FICF               SAVE THE ORIGINAL CF
          FETCH X1,OC,X2
          SX3   X2-1
          ZR    X3,OPENED
          OPENM X1,I-O,R
 #MD      IFC    LT, "MODEL" 75
          SX1   A0                 RESTORE X1 DESTROYED BY 6RM
 #MD      ENDIF
 OPENED   BSS   0
 #MD      IFC    GT, "MODEL" 75
          FETCH X1,LT,X6
          SA6   FILT               SAVE THE ORIGINAL LT
 #MD      ENDIF
          CLOSEM X1,U
          EQ    END
*
 RMERROR  DATA  0
 #MD      IFC    LT, "MODEL" 75
          SX1   A0                 RESTORE X1 DESTROYED BY 6RM
 #MD      ENDIF
          FETCH X1,IRS,X6
          SA6   RC                 SAVE FIES
          BX3   X6
*
*         TRANSFORM INTEGER IN DISPLAY CODE
          MX6   0
          MX0   60-3
          SB5   3                  ONLY 3 DIGITS IN RM ERROR
          LX3   51
          SB1   1
          SB4   1R0
 COD1     BSS   0
          LX3   3
          BX4   -X0*X3
          SX5   X4+B4
          LX6   6
          IX6   X6+X5
          SB5   B5-B1
          NZ    B5,COD1
          LX6   42
          MX0   18
          SA5   RMNR
          BX4   -X0*X5
          BX6   X0*X6
          IX6   X4+X6
          SA6   A5                 STORE RMNR
          SA2   X1
          BX6   X2
          SA6   LFN                SAVE LFN NAME
          MESSAGE MSG
          SA2   ADRC
          ZR    X2,ABT             NO RC, ABORT
 #MD      IFC    LT, "MODEL" 75
          SX1   A0                 RESTORE X1 DESTROYED BY MESSAGE
 #MD      ENDIF
          STORE X1,ES=0            CLEAR FIES
          EQ    END
 ABT      BSS   0
          MX6   0
          RJ    =XSYSEND.          NO RC, ABORT
          ABORT
          END
