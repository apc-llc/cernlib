CDECK  ID>, MQPAGE.
      SUBROUTINE MQPAGE

C-    Set derived page size parameters

      COMMON /QPAGE/ NQLMAX,NQLTOL,NQLTOK,NQCMAX,NQCPGH,NQPAGE
     +,              NQWYLDO,NQWYL,NQNEWH,NQJOIN,NQDKNO,NQDKPG
C--------------    End CDE              --------------------------------


      NQLTOL = NQLMAX - 7
      NQLTOK = NQLMAX - 4
      RETURN
      END
