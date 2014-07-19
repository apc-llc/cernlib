*
* $Id: sfinv.inc,v 1.1.1.1 1996/02/15 17:49:04 mclareni Exp $
*
* $Log: sfinv.inc,v $
* Revision 1.1.1.1  1996/02/15 17:49:04  mclareni
* Kernlib
*
*
*
* sfinv.inc
*
          IF(N .EQ. 1)  GOTO 399
          A(1,2)  =  -A(1,2)
          A(2,1)  =   A(1,2)*A(2,2)
          IF(N .EQ. 2)  GOTO 320
          DO 314    J  =  3, N
             JM2  =  J - 2
             DO 312 K  =  1, JM2
                S31  =  A(K,J)
                DO 311  I  =  K, JM2
                   S31  =  DOTF(A(K,I+1),A(I+1,J),S31)
 311               CONTINUE
                A(K,J)  =  -S31
                A(J,K)  =  -S31*A(J,J)
 312            CONTINUE
             A(J-1,J)  =  -A(J-1,J)
             A(J,J-1)  =   A(J-1,J)*A(J,J)
 314         CONTINUE
 320      J  =  1
 323         S33  =  A(J,J)
             IF(J .EQ. N)  GOTO 325
             JP1  =  J + 1
             DO 324 I  =  JP1, N
                S33  =  DOTF(A(J,I),A(I,J),S33)
 324            CONTINUE
 325         A(J,J)  =  S33
          JM1  =  J
          J    =  JP1
             DO 328 K  =  1, JM1
                S32  =  ZERO
                DO 327  I  =  J, N
                   S32  =  DOTF(A(K,I),A(I,J),S32)
 327               CONTINUE
                A(K,J)  =  S32
                A(J,K)  =  S32
 328            CONTINUE
          IF(J .LT. N)  GOTO 323
 399      CONTINUE
