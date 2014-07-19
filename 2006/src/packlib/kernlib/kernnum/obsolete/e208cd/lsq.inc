*
* $Id: lsq.inc,v 1.1.1.1 1996/02/15 17:47:59 mclareni Exp $
*
* $Log: lsq.inc,v $
* Revision 1.1.1.1  1996/02/15 17:47:59  mclareni
* Kernlib
*
*
*
* lsq.inc
*
*CALL     BASIC
*CALL     SET
*CALL     LSQ
 MAIN     MACRO     N,M,B,C,I,J,ONE
          LOCAL     [10],[30],[100],[900]
 IDIMPAR  EQU       20
          EXT       LLSQ,RVSUM,RSEQN,E208PR
          HEADER    LSQ,SAVEA0
 LLSQAL   BSS       0                   (N,X,Y,C(1),C(2),IFAIL)
          BSS       1                   (N)
          BSS       1                   (X)
          BSS       1                   (Y)
          BSS       1                   (C(1))
          BSS       1                   (C(2))
          VFD       60/IFAIL            (IFAIL)
          BSSZ      1
 RVSUMAL  BSS       0                   (N,Y(1),Y(2))
          BSS       1                   (N)
          BSS       1                   (Y(1))
          BSS       1                   (Y(2))
          BSSZ      1
 RSEQNAL  BSS       0                   (M,B,IDIM,IFAIL,1,C)
          BSS       1                   (M)
          VFD       60/ARRAYB           (B)
          VFD       60/IDIM             (IDIM)
          VFD       60/IFAIL            (IFAIL)
          VFD       60/INT1             (1)
          BSS       1                   (C)
          BSSZ      1
 E208PRAL BSS       0                   (N,M,IDIM)
          BSS       1                   (N)
          BSS       1                   (M)
          VFD       60/IDIM             (IDIM)
          BSSZ      1
 IFAIL    BSS       1
 IDIM     CON       IDIMPAR
 INT1     CON       1
 FLOATN   BSS       1                   FLOAT(N)
 ARRAYB   BSS       IDIMPAR*IDIMPAR
          EXIT      LSQ                 (N,X,Y,M,C)
*                                       X1=(N)    A1=((N))
          SA4       A1+3                X4=(M)    A4=((M))
          SA2       X1                  X2=N      A2=(N)
          SX7       A0                  X7=OLD A0
          S_ONE     1                                       ONE=1
          SA0       A1                            A0=((N))
          SA5       A4+ONE              X5=(C)    A5=((C))
          SA3       A4-ONE              X3=(Y)    A3=((Y))
          SA4       X4                  X4=M      A4=(M)
          SA7       SAVEA0              SAVE OLD A0
          SA1       A1+ONE              X1=(X)    A1=((X))
          S_B       ARRAYB                                  B=(B)
          S_N       X2                                      N=N
          S_C       X5                                      C=(C)
          S_J       ONE+ONE                                 J=2
          S_M       X4                                      M=M
          GT        M,J,[30]            IF M>2
          LT        M,J,[10]            IF M<2
          SA2       A0                  X2=(N)    A2=((N))
          BX6       X1                  X6=(X)
          BX7       X3                  X7=(Y)
          SA6       LLSQAL+1            STORE (X)
          BX6       X2                  X6=(N)
          SA7       A6+ONE              STORE (Y)
          SA6       A6-ONE              STORE (N)
          SX7       C                   X7=(C(1))
          SX6       C+ONE               X6=(C(2))
          SA7       A7+ONE              STORE (C(1))
          SA6       A7+ONE              XTORE (C(2))
          SA1       LLSQAL              X1=(N)    A1=((N))
 +        RJ        LLSQ                CALL LLSQ(N,X,Y,C(1),C(2),IFAIL)
 -        VFD       12/0,18/TWORD
          JP        [100]
 [10]     BSS       0
          LT        M,ONE,[900]         IF M<1
          LT        N,ONE,[900]         IF N<1
          SA2       A0                  X2=(N)    A2=((N))
          SX6       N                   X6=N
          PX6       X6
          NX6       X6                  X6=FLOAT(N)
          BX7       X2                  X7=(N)
          SA6       FLOATN              STORE FLOAT(N)
          SA7       RVSUMAL             STORE (N)
          SX6       X3                  X6=(Y(1))
          SX7       X3+ONE              X7=(Y(2))
          SA6       A7+ONE              STORE (Y(1))
          SA7       A6+ONE              STORE (Y(2))
          SA1       RVSUMAL
 +        RJ        RVSUM               X6=RVSUM(N,Y(1),Y(2))
 -        VFD       12/0,18/TWORD
          SA1       FLOATN              X1=FLOAT(N)
          SA2       A0+4                X2=(C(1))
          RX6       X6/X1               X6=RVSUM/FLOAT(N)=C(1)
          SA6       X2                  STORE C(1)
          JP        EPILOG
 [30]     BSS       0
          S_J       IDIMPAR                                 J=IDIM
          GT        M,N,[900]           IF M>N
          GT        M,J,[900]           IF M>IDIM
          RLSQP     N,M,B,C,I,J,ONE     X1=(X), X3=(Y)
          SA2       A0+3                X2=(M)
          SA3       A0+4                X3=(C)
          SX6       X2                  X6=(M)
          SX7       X3                  X7=(C)
          SA6       RSEQNAL             STORE (M)
          SA7       RSEQNAL+5           STORE (C)
          SA1       RSEQNAL
 +        RJ        RSEQN               CALL RSEQN(M,B,IDIM,IFAIL,1,C)
 -        VFD       12/0,18/TWORD
 [100]    BSS       0
          SA5       IFAIL               X5=IFAIL
          ZR        X5,EPILOG           IF(IFAIL .EQ. 0) RETURN
 [900]    BSS       0
          SA2       A0                  X2=(N)    A2=((N))
          SA3       A0+3                X3=(M)    A3=((M))
          BX6       X2                  X6=(N)
          BX7       X3                  X7=(M)
          SA6       E208PRAL            STORE (N)
          SA7       E208PRAL+1          STORE (M)
          SA1       E208PRAL
 +        RJ        E208PR              CALL E208PR(N,M,IDIM)
 -        VFD       12/0,18/TWORD
          EPILOG
          ENDM
