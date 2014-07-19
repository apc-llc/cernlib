*
* $Id: frupcas.F,v 1.1.1.1 1996/01/11 14:05:22 mclareni Exp $
*
* $Log: frupcas.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:22  mclareni
* Fritiof
*
*
C********************************* FRUPCAS ***************************
 
      SUBROUTINE FRUPCAS(STR)
 
C.... Convert a string character (length<20) into UPPER CASE one
C.... STR must not contain more than 3 spaces between the characters.
 
      CHARACTER STR*(*),CHALP(2)*26,STR0*23
      DATA CHALP/'abcdefghijklmnopqrstuvwxyz',
     &'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
 
      I=0
      IPH=0
      STR0 = STR
10    I=I+1
      IF(STR0(I:I).EQ.' ') IPH=IPH+1
      IF(IPH.GT.3) GOTO 30
      IF (STR0(I:I) .GE. 'a'  .AND.  STR0(I:I) .LE. 'z')  THEN
CC          STR(I:I) = CHAR(ICHAR(STR(I:I)) - '20'X)
      DO 20 J=1,26
20      IF(STR(I:I).EQ.CHALP(1)(J:J)) STR(I:I)= CHALP(2)(J:J)
      ENDIF
      GOTO 10
 
30    RETURN
      END
