*
* $Id: kfile.s,v 1.1.1.1 1996/02/15 17:51:34 mclareni Exp $
*
* $Log: kfile.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:34  mclareni
* Kernlib
*
*
          IDENT KFILE
*
* CERN PROGLIB# Z261    KFILE           .VERSION KERNCDC  2.07  831121
*
          ENTRY KFILE
*
*     WITH FORTRAN 4.7  (MARCH 1978, REVISION D OF DOCUMENT 60497800)
*     CDC HAVE CHANGED THE HANDLING OF FORTRAN FILES
*     THIS VERSION OF KFILE FOLLOWS THIS CHANGE IF   +USE, FTN47.
*
*     X6/X7 = KFILE (J)
*
*     WITH FORTRAN 5 CDC HAVE CHANGED THE HANDLING AGAIN
*     IT SEEMS NOT POSSIBLE TO GET THE INTERNAL FILE NAME
*     HENCE X7 WILL CONTAIN ANYTHING
*
 KFILE    DATA  0
          SA1   X1             X1= J= 1,2,3,...   J-TH FILE
          MX6   0              X6= 0  PRE-SET
*
#if defined(CERNLIB_F77)
*     FOR         LOF.FTN            SEE FORTRAN MANUAL, SECTION 15
          SA3   =XLOF.FTN      X3= ADR(FILE-TABLE)
          SB4   X1             B4= J = 1,2,3,...
          SB4   B4+X1          B4= 2*J = 2,4,6
          SA2   X3+B4          X2= TABLE-ENTRY FOR FILE J
#endif
#if (defined(CERNLIB_FTN47))&&(!defined(CERNLIB_F77))
*     FOR         LOF.FTN            SEE FORTRAN MANUAL, SECTION 15
          SA3   =XLOF.FTN      FTN 4.7  X3= ADR(FILE-TABLE)
          SB4   X1-1           FTN 4.7  B4= J-1= 0,1,2,...
          SA2   X3+B4          FTN 4.7  X2= TABLE-ENTRY FOR FILE J
***       SA2   X1+1           EARLIER  X2= TABLE-ENTRY FOR FILE J
#endif
#if (!defined(CERNLIB_FTN47))&&(!defined(CERNLIB_F77))
***   FOR         LOF.FTN            SEE FORTRAN MANUAL, SECTION 15
***       SA3   =XLOF.FTN      FTN 4.7  X3= ADR(FILE-TABLE)
***       SB4   X1-1           FTN 4.7  B4= J-1= 0,1,2,...
***       SA2   X3+B4          FTN 4.7  X2= TABLE-ENTRY FOR FILE J
          SA2   X1+1           EARLIER  X2= TABLE-ENTRY FOR FILE J
#endif
*
          MX4   42             X4= MASK FILE-NAME
          BX7   X4*X2          X7= INTERNAL FILE-NAME
          ZR    X7,KFILE           QUIT IF END OF TABLE
          SA3   X2
          BX6   X4*X3          X6= EXTERNAL FILE NAME
          JP    KFILE
          END
