CDECK  ID>, QLUMP.
      SUBROUTINE QLUMP

C-    Compact the NAME parameters in /MQCL/ into the name-word
C-    of the bank starting at NQLST

      COMMON /MQCL/  NQLST,NQLNA,NQID,NQTY,NQLI,NQNL,NQNS,NQND
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ)
                     EQUIVALENCE (LQ,IQ,LQGARB)
      COMMON //      IQUEST(100),LQGARB
C--------------    End CDE              --------------------------------

      ISHFTL (IZW,NZB) = LSHIFT (IZW, NZB)


      NACC = NQND
      NACC = ISHFTL(NACC,6) + NQNS
      NACC = ISHFTL(NACC,6) + NQNL
      NACC = ISHFTL(NACC,1) + NQLI
      NACC = ISHFTL(NACC,6) + NQTY

      LQ(NQLNA) = NACC
      IQ(NQLST) = ISHFTL(NQNL,26)
      RETURN
      END
