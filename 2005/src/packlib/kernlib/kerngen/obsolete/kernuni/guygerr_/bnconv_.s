*
* $Id: bnconv_.s,v 1.1.1.1 1996/02/15 17:54:08 mclareni Exp $
*
* $Log: bnconv_.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:08  mclareni
* Kernlib
*
*
. ----------------------------------------------------------------------
.     AREA 5 :
.     CODE FOR CONVERSION ROUTINES
.
.     ASSEMBLER ROUTINE BNOCT
.     BINARY TO FIELDATA OCTAL CONVERSION
.     INPUT IN A5
.     OUTPUT IN A4,A5
.     NO OTHER REGISTERS REQUESTED
 
          AXR$
          FORT$
 
GERR$C    INFO  2  4
$(4)
 DO FOR ,FBLANK
BLANK
 DO FOR , +         050505050505
 DO FTN , +         040040040040
DYN$      RES  2
BSAV1     RES  2
 
$(0)
 DO FTN ,FBLANK  +  050505050505
BNOCT$*
          J
          DS        A0,BSAV1
          SZ        A1
BNOCT1    L         A4,(0666666666666)
          L,U       A0,5
BNOCT2
          LSSC      A4,3
          LDSL      A4,3
          JGD       A0,BNOCT2
          JNZ       A1,BNOCT3
          L         A1,A4
          J         BNOCT1
BNOCT3    L         A5,A4
          L         A4,A1
          DL        A0,BSAV1
          J         *BNOCT$
 
 
.     ASSEMBLER ROUTINE BNDC1$
.     CONVERT THE CONTENT OF A3 FROM BINARY FORM TO DECIMAL FIELDATA CODED FORM
.     RESULT IN A3, RIGHT ADJUSTED  (I6 FORMAT)
.     USES REGISTERS  AO, A4, A5, R1
BNDC1$*
          J
          L         A5,FBLANK
          L,U       R1,5
BND1      DSA       A3,36
          DI,U      A3,10
          A,U       A4,060
          DSL       A4,6
          JZ        A3,BND2
          JGD       R1,BND1
          J         BND3
BND2      L,U       A3,6               . RIGHT ADJUST
          MSI       A3,R1
          SSC       A5,0,A3
BND3      L         A3,A5
          J         *BNDC1$
 
 
.     ASSEMBLER ROUTINE  BNDC2$  (EQUIVALENT TO NBTOD$ )
.     CONVERT THE CONTENT OF A3 FROM BINARY FORM TO DECIMAL FIELDATA CODED FORM
.     RESULT IN A3, LEFT ADJUSTED
.     USES REGISTERS  A3, A4, A5, R1
.     OFTEN USED TO GET FILE NAME FROM FORTRAN UNIT NUMBER
 
BNDC2$*
          J
          L         A5,FBLANK
          L,U       R1,5
BND11     DSA       A3,36
          DI,U      A3,10
          A,U       A4,060
          DSL       A4,6
          JZ        A3,BND22
          JGD       R1,BND11
BND22     L         A3,A5
          J         *BNDC2$
 
. ----------------------------------------------------------------------
          END
