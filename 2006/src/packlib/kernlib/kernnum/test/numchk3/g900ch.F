*
* $Id: g900ch.F,v 1.1.1.1 1996/02/15 17:48:46 mclareni Exp $
*
* $Log: g900ch.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:46  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
          SUBROUTINE G900CH(NREP,OK)
          LOGICAL   OK, OKT
#include "kernnumt/sysdat.inc"
          DOUBLE PRECISION    FIRST, SEED, DRANF, DR(20), Y
          DATA      DR     /  0.562 042 328 605 79D0,
     +                        0.747 430 623 326 45D0,
     +                        0.441 347 714 323 67D0,
     +                        0.797 502 695 040 91D0,
     +                        0.269 636 741 168 25D0,
     +                        0.645 535 662 993 14D0,
     +                        0.246 207 703 284 19D0,
     +                        0.183 452 250 568 67D0,
     +                        0.595 366 548 797 63D0,
     +                        0.949 946 004 899 90D0,
     +                        0.278 860 575 609 30D0,
     +                        0.471 266 204 207 20D0,
     +                        0.853 167 626 455 26D0,
     +                        0.970 918 323 001 16D0,
     +                        0.994 956 212 922 71D0,
     +                        0.106 361 109 833 80D0,
     +                        0.706 313 248 108 35D0,
     +                        0.816 562 582 286 04D0,
     +                        0.574 617 317 606 90D0,
     +                        0.665 058 493 879 02D0 /
          OK  =  .TRUE.
          CALL RANGET(FIRST)
          CALL RANSET(SEEDIN)
          DO 300    IREP  =  1, NREP
             CALL RANGET(SEED)
             DO 130 K  =  1, 2
                DO 120    I  =  1, 20
                   DO 110 J  =  1, 100
                      Y  =  DRANF()
 110                  CONTINUE
                   OKT  =  DABS(Y-DR(I)) .LE. 1.D-13
                   IF(OKT)  GOTO 120
                      WRITE(*,1001) I, Y, DR(I)
                      OK =  .FALSE.
 120               CONTINUE
                CALL RANSET(SEED)
 130            CONTINUE
             DO 230 K  =  1, 2
                DO 220    I  =  1, 20
                   DO 210 J  =  1, 100
                      Z  =  RANF()
 210                  CONTINUE
                   OKT  =  ABS(Z-SNGL(DR(I))) .LE. 1.E-5
                   IF(OKT)  GOTO 220
                      WRITE(*,1002) I, Z, DR(I)
                      OK =  .FALSE.
 220               CONTINUE
                CALL RANSET(SEED)
 230            CONTINUE
 300         CONTINUE
  90      CALL RANSET(FIRST)
          RETURN
1001      FORMAT(24H ??? ERROR IN DRANF ??? , I5, 1P, 2D25.14)
1002      FORMAT(24H ??? ERROR IN RANF ???  , I5, 1P, E25.8, D25.8)
          END
