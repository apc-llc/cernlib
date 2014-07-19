      SUBROUTINE ZRECOK

C-    CALL SETRPRV

#include "zebra/qexchcry.inc"

      EXTERNAL     ZRECOV

      CALL SETRPV (ZRECOV,IXP,JREPCD)
      RETURN
      END
