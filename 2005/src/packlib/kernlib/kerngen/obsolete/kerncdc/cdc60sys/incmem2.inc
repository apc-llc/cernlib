*
* $Id: incmem2.inc,v 1.1.1.1 1996/02/15 17:51:31 mclareni Exp $
*
* $Log: incmem2.inc,v $
* Revision 1.1.1.1  1996/02/15 17:51:31  mclareni
* Kernlib
*
*
*
* incmem2.inc
*
          SA1  A7+B7
          PL   X1,OK
          MEMORY  SCM,MAXFLD,RECALL
          SA1  MAXFLD
          BX6  X1
          AX6  30
          SA6  A1                  STORE MAXFLD (RH)
 OK       BSS  0
*
* incmem2.inc
*
