*
* $Id: help.s,v 1.1.1.1 1996/02/15 17:53:33 mclareni Exp $
*
* $Log: help.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:33  mclareni
* Kernlib
*
*
        UNIVERSAL HELP
        PASS2
        DEFINE HELLO(NAME,NAME1,NAME2)<
        ENTRY   NAME
        IFNB <NAME1>,<ENTRY NAME1>
        IFNB <NAME2>,<ENTRY NAME2>
        SIXBIT/NAME/
IFNB <NAME1>, <NAME1::>
IFNB <NAME2>, <NAME2::>
NAME::
>                               ;END OF HELLO MACRO
        DEFINE  CALL(NAME)<
       PUSHJ      17,NAME
>                               ;END OF CALL MACRO
        DEFINE  ENTER(NAME,ARGBLK)<
        PUSH    17,16
       IFB   <ARGBLK>,< MOVEI     16,.+3>
       IFNB  <ARGBLK>,< MOVEI     16,ARGBLK>
        PUSHJ   17,NAME
        POP     17,16
>                               ; END OF ENTER MACRO
        DEFINE GOODBY<
        POPJ    17,>
        DEFINE SAVACS(NUM)<
        ZZ=2
        REPEAT NUM-1,<
        PUSH    17,ZZ
        ZZ=ZZ+1
>
>
        DEFINE RESTOR(NUM)<
        ZZ=2
        REPEAT NUM-1,<
        POP     17,ZZ
        ZZ=ZZ+1
>
>
        END
