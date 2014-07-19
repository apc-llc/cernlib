/*
 * $Id: lexer.h,v 1.1.1.1 1996/03/01 11:39:11 mclareni Exp $
 *
 * $Log: lexer.h,v $
 * Revision 1.1.1.1  1996/03/01 11:39:11  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/00 08/04/95  23.37.56  by  Gregory Kozlovsky*/
/*-- Author :    Gregory Kozlovsky   08/04/95*/

/*******************************************************************************
 Lexical analyser.
 =====================> Author: G. Kozlovsky, 1989 <===========================
*******************************************************************************/
/****************************************************************
 Function tokname() returns token name given a token number.
****************************************************************/
#define WS_T      0x100
#define NEWLINE_T 0x200
#define EOF_T     0x300
#define SEPAR_T   0x400
#define OPER_T    0x500
#define IDENT_T   0x600
#define INT_T     0x700
#define DOUBLE_T  0x800
#define QSTRING_T 0x900
#define EMPTY_T   0xA00
#define ERR_T     0x000

#define ERR_RET -1

/* separate generic type and specifis type bytes used for SEPAR_T and OPER_T */
#define LX_GENTYPE(type) ((type)&0xFF00)
#define LX_SPECTYPE(type) ((type)&0x00FF)

/***********************************************************************/
typedef struct token {
        int type;       /* type of the token */
        union {
            int i;             /* to keep value of single character token */
            unsigned long ul;  /* for integer constants */
            float  f;
            double d;
            char   s[80];      /* for identifiers and quoted strings */
        } val;
} TOKEN;

/***********************************************************************/
typedef struct tokloc {
        char *linestart; /* points to the line which contains token */
        int lineno;     /* line number */
        int pos;        /* position in line where token starts */
        int inputlen;   /* length of the token in the input buffer */
} TOKLOC;

/***********************************************************************/
#define ERRLEVELNO 5
typedef struct errinfo {
        /* maximum error level =0 no error, =1 warning, =2 error token */
        int maxerrlevel;
        int errcount[ERRLEVELNO];   /* number of errors of every level */
} ERRINFO;

/***********************************************************************/
typedef struct lexstate { /* complete status of the lexer */
    TOKEN token;
    TOKLOC loc;
    ERRINFO errinfo;
    int was_unread;  /* used by unread_token */
    int eofflag;     /* endoffile have been read */
    char fname[30];  /* name of input file for error reports */
    char *ptr;       /* current position of lexer in the input buffer */
} LEXSTATE;

/************************************************************************
 External variables through which lexer and errmessage communicate
 with the caller.
************************************************************************/
extern TOKEN lx_token;
extern TOKLOC lx_loc;
extern ERRINFO lx_errinfo;
/* lx_eofflag is set if end of file have been read */
extern int lx_eofflag;

/************************************************************************
 Lexer functions callable from the outside.
************************************************************************/
void lx_newfile(char *); /* communicate input file name to the lexer */
void lx_load(char *);    /* load an input buffer to be scanned */
void lx_save(LEXSTATE *);/* save complete lexer state */
void lx_restore(LEXSTATE *);/* restore previously saved complete lexer state */
void lx_next(void);         /* read next token from the input buffer */
void lx_skipline(void);     /* skip rest of the line not including newline */
void lx_unread(void);       /* unread token (can be done only once) */
void lx_errmsg(TOKLOC *,int,char *); /* print caller supplied error message */
void lx_prcurline(void);    /* print current line and its number */
char *lx_tokname(int);   /* return token name given token number */
