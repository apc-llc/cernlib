*
* $Id: sfactd.inc,v 1.2 1999/09/08 08:05:24 mclareni Exp $
*
* $Log: sfactd.inc,v $
* Revision 1.2  1999/09/08 08:05:24  mclareni
* A problem was reported in DSINV which failed on very small numbers, probably
* due to converting to single before a test. The conversion has been removed here
* and also in DSFACT. This resulted in mods to sfact.inc and sfactd.inc which
* meant that some other routines had to be tidied also.
*
* Revision 1.1.1.1  1996/02/15 17:49:04  mclareni
* Kernlib
*
*
*
* sfactd.inc
*
          IFAIL  =  0
          JFAIL  =  0
          DET    =  ONE
          DO 144    J  =  1, N
             IF((A(J,J)) .LE. ZERO)  GOTO 150
             DET     =  DET * A(J,J)
             A(J,J)  =  ONE / A(J,J)
             T  =  SIZEF(DET)
             IF(T .LT. G1)  THEN
                DET    =  ONE
                IF(JFAIL .EQ. 0)  JFAIL  =  -1
             ELSEIF(T .GT. G2)  THEN
                DET    =  ONE
                IF(JFAIL .EQ. 0)  JFAIL  =  +1
             ENDIF
             IF(J .EQ. N)  GOTO 199
             JP1  =  J+1
             DO 143   L  =  JP1, N
                A(J,L)  =  A(J,J)*A(L,J)
                S1      =  -A(L,J+1)
                DO 141  I  =  1, J
                   S1  =  DOTF(A(L,I),A(I,J+1),S1)
 141               CONTINUE
                A(L,J+1)  =  -S1
 143            CONTINUE
 144         CONTINUE
 150      IFAIL  =  -1
          JFAIL  =  -2
          DET    =  ZERO
          RETURN
 199      IF(JFAIL .NE. 0)   DET  =  ZERO
