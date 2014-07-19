/*
 * $Id: lexer.c,v 1.1.1.1 1996/03/01 11:39:07 mclareni Exp $
 *
 * $Log: lexer.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:07  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/00 18/04/95  17.36.41  by  O.Couet*/
/*-- Author :    Gregory Kozlovsky   09/04/95*/

/*******************************************************************************
 Lexical analyser.
 =====================> Author: G. Kozlovsky, 1989 <===========================
********************************************************************************

    Lexer scans the input buffer and convert it into a sequence of tokens.
    It also keeps track of the current line number and position of
    a token in line.

    Additional functions of the lexer are error recovery, printing
    error reports and keeping track of the number and the severity
    of errors encountered so far.

    Error recovery is accomplished by simply skipping
    rest of the line. Error report includes line number and position
    of the erroneous token followed by a caller supplied error
    message.

    Lexer communicates with a calling program through the following
    external variables:
        TOKEN lx_token     - current token,
        TOKLOC lx_loc      - current token location,
        ERRINFO lx_errinfo - current error information,
        int lx_eofflag     - set if end of file have been read.

    A calling program must include file lexer.h which contains
    declarations and definitions necessary to use the lexer.
*******************************************************************************/
#include    <stdio.h>
#include    <stdlib.h>
#include    <string.h>
#include    <ctype.h>
#include    <math.h>
#include "fpanelsh/lexer.h"

static int debug = 0;

#define max(a,b)        (((a) > (b)) ? (a) : (b))

/* internally used functions */
static int myatoi(char *);

static int was_unread;  /* used by unread_token */

/**********************************************************************
 Information needed for printing error reports.
**********************************************************************/
static char fname[30];       /* name of input file for error reports */
static char *ptr;   /* current position of lexer in the input buffer */


/**********************************************************************
 External variables through which lexer communicates with the caller.
**********************************************************************/
TOKEN lx_token;
TOKLOC lx_loc;
ERRINFO lx_errinfo;
int lx_eofflag;               /* if endoffile was read, stop reading */


/*******************************************************************************
DESCRIPTION: Communicate input file name to the lexer.
ARGUMENTS: Name of the input file needed for error reports.
*******************************************************************************/
void lx_newfile(char *inputfilename)
{
    strcpy(fname,inputfilename);
    lx_loc.lineno = 0;   /* input file line number for error messages */
    lx_eofflag = 0;
}

/*******************************************************************************
DESCRIPTION:
 Load an input buffer to be read by subsequent calls to lx_next()
 and initialize the lexer.
 Increment line number (if buffer contains several lines line number is
 incremented by lexer when called after passing newline character).
ARGUMENTS: A pointer to the beginning of the input buffer to be read by lexer.
*******************************************************************************/
void lx_load(char *bufstart)
{
    ptr = bufstart;
    lx_loc.linestart = bufstart;
    lx_loc.lineno++;
    lx_token.type = EMPTY_T;
    was_unread = 0;
}
/*******************************************************************************
DESCRIPTION: Save complete lexer state.
ARGUMENTS: A pointer to LEXSTATE structure.
*******************************************************************************/
void lx_save(LEXSTATE *lstptr)
{
    lstptr->token = lx_token;
    lstptr->loc = lx_loc;
    lstptr->errinfo = lx_errinfo;
    lstptr->was_unread = was_unread;
    lstptr->eofflag = lx_eofflag;
    strcpy(lstptr->fname,fname);
    lstptr->ptr = ptr;
}

/*******************************************************************************
DESCRIPTION: Restore previously saved complete lexer state.
ARGUMENTS: A pointer to LEXSTATE structure.
*******************************************************************************/
void lx_restore(LEXSTATE *lstptr)
{
    lx_token = lstptr->token;
    lx_loc = lstptr->loc;
    lx_errinfo = lstptr->errinfo;
    was_unread = lstptr->was_unread;
    lx_eofflag = lstptr->eofflag;
    strcpy(fname,lstptr->fname);
    ptr = lstptr->ptr;
}

/***********************************************************************
 Lexical analyser
 ****************

DESCRIPTION:
 Reads next token from the input buffer which was loaded by call to
 lx_load() and stores information on it in external TOKEN structure
 named lx_token. In case the token was unread no new token is read from
 the input buffer and the old token is used.

 In case end of file character '\032' is encountered, <newline> token
 is returned and external variable lx_eofflag is set.

 If the current input character can not be interpreted as a token or
 a part of a token, error message is printed, token of type ERR_T is
 returned and the character is skept.

 In some cases then a token has an incorrect form, lexer prints warning
 messages.

 In addition, lexer keeps track of the number of the current line and the
 first character of the current line. Newline at the end of line is
 considered part of the line.

 The definition of TOKEN structure and token types definitions are kept
 in file lexer.h which must be included into a program using lx_next().

RETURN:
 A pointer to the TOKEN structure containing the information on the
 current token. For tokens of all the types character value is returned
 as a string in lx_token.val.s.
 For integers integer value is returned in lx_token.val.ul.
 External structure lx_token contains the current token.
 External variable lx_eofflag is set to 1 if end of file have been
 encountered.
******************************************************************/
void lx_next()
{
    int errinlexer = 0;
    char errmessline[80];
    char *sval = lx_token.val.s;
    unsigned long ulval;
    float dval;

    if (was_unread) {         /****** use old token *****************/
        was_unread = 0;

    }
    else { /********** read next token from the input buffer ************/
          /*************************************************************/
        if (lx_token.type == NEWLINE_T) {  /* previous token was newline */
            lx_loc.lineno++;               /* new line started */
            lx_loc.linestart = ptr;
        }
        if (lx_eofflag) { /***** end of file have already been read */
            lx_errmsg(&lx_loc,2,
                "***lexer: an attempt to read after end of file");
            exit(2);
        }
        /****** white space and comments *********/
        if ((int)*ptr==' ' || (int)*ptr=='\t') {
            while ((int)*ptr==' ' || (int)*ptr=='\t') {
                ptr++;      /* skip a white space */
            }
            /*lx_token.type = WS_T;*/
        }
        /*else if ((int)*ptr == '\n') {*/  /****** newline *************/

        lx_loc.pos = ptr - lx_loc.linestart;  /* token's starting position */

        if ((int)*ptr == '\n'|| (int)*ptr == '\0') {  /***** newline ****/
            lx_token.val.i = '\n';
            ptr++;
            lx_token.type = NEWLINE_T;
        }
        else if ((int)*ptr == '\032') {/****** end of file ****************/
            lx_token.val.i = '\0';   /* treat as newline, lx_set eofflag = 1 */
            ptr++;
            lx_eofflag = 1;
            lx_token.type = NEWLINE_T;
        }
        else if (strchr("+-*/^&|><=",*ptr)) {  /****** operator ***********/
            lx_token.val.i = (int)*ptr;
            lx_token.type = OPER_T + *ptr;
            ptr++;
        }
        else if (strchr("()[]{},;!:\\",*ptr)) {  /**** separator (except '.') */
            lx_token.val.i = (int)*ptr;
            lx_token.type = SEPAR_T + *ptr;
            ptr++;
        }
        else if ((int)*ptr == '.') { /******** number or separator *******/
            *sval++ = *ptr++;
            if (isdigit(*ptr)) { /* floating point number */
                while (isdigit(*ptr)) *sval++ = *ptr++;
                if ((int)*ptr == 'e' || (int)*ptr == 'E') { /* optional exp */
                    *sval++ = *ptr++;
                    if ((int)*ptr == '+' || (int)*ptr == '-') {
                        *sval++ = *ptr++;
                    }
                    while (isdigit(*ptr)) *sval++ = *ptr++;
                }
                *sval = (char)'\0';
                dval = atof(lx_token.val.s); /* convert to float */
                lx_token.val.d = dval;
                lx_token.type = DOUBLE_T;
            }
            else { /* separator */
                lx_token.val.i = '.';
                lx_token.type = SEPAR_T + '.';
            }
        }
        else if (isdigit(*ptr)) {  /******* number *********/
            while (isdigit(*ptr)) {
                *sval++ = *ptr++;
            }
            lx_token.type = INT_T;
            if ((int)*ptr == '.') { /* optional mantissa */
                *sval++ = *ptr++;
                while (isdigit(*ptr)) *sval++ = *ptr++;
                lx_token.type = DOUBLE_T;
            }
            if ((int)*ptr == 'e' || (int)*ptr == 'E') { /* optional exp */
                *sval++ = *ptr++;
                if ((int)*ptr == '+' || (int)*ptr == '-') {
                    *sval++ = *ptr++;
                }
                while (isdigit(*ptr)) *sval++ = *ptr++;
                lx_token.type = DOUBLE_T;
            }
            *sval = (char)'\0';
            if (lx_token.type == DOUBLE_T) {
                dval = atof(lx_token.val.s); /* convert to float */
                lx_token.val.d = dval;
            }
            else {  /* integer is converted to unsigned long */
                ulval = (unsigned long)atol(lx_token.val.s);
                lx_token.val.ul = ulval;
            }
        }
        else if (isalpha(*ptr) || (int)*ptr=='_') { /****** name ***********/
            while (isdigit(*ptr) || isalpha(*ptr) || (int)*ptr=='_') {
                *sval++ = *ptr++;
            }
            *sval++ = (char)'\0';
            lx_token.type = IDENT_T;
        }
        else if ((int)*ptr == '"') {   /****** quoted string **************/
            ptr++;   /* skip opening quotes */
            while ((int)*ptr!='"' && (int)*ptr!='\n' && (int)*ptr!='\032') {
                if ((int)*ptr == '\\') { /* after backslash copy any char */
                    ptr++;
                    /****** except newline or end of file ******/
                    if ((int)*ptr == '\032' || (int)*ptr == '\n') {
                        ptr--;
                        sprintf(errmessline,
                 "lexer: backslash in string precedes newline or end of file");
                        errinlexer = 1;
                    }
                }
                *sval++ = *ptr++;
            }
            if ((int)*ptr == '"') {
                ptr++; /* skip closing quotes */
            }
            else {
                sprintf(errmessline,
                    "lexer: double quotes not closed in the same line");
                errinlexer = 1;
            }
            *sval++ = (char)'\0';
            lx_token.type = QSTRING_T;
        }
        else {                    /****** illegal token ********************/
            ptr++;
            sprintf(errmessline,"lexer: illegal token");
            errinlexer = 2;
            lx_token.type = ERR_T;
        }
        lx_loc.inputlen = ptr - lx_loc.linestart - lx_loc.pos;
        if (errinlexer) lx_errmsg(&lx_loc,errinlexer,errmessline);
    }

    if (debug > 1) { /******* debugging message *******************/
          printf("***lexer*** %s length = %d\n",
              lx_tokname(lx_token.type),lx_loc.inputlen);
          if (lx_token.type == INT_T) {
              printf("int value    = %d\n",lx_token.val.i);
          }
          else if (lx_token.type == DOUBLE_T) {
              printf("float value    = %g\n",lx_token.val.f);
          }
          else  if (lx_token.type == IDENT_T || lx_token.type == QSTRING_T) {
              printf("char value = %s\n",lx_token.val.s);
          }
          printf("----------------------------------------------\n");
    }
}

/*******************************************************************************
DESCRIPTION: Skip rest of the line not including the newline token.
*******************************************************************************/
void lx_skipline()
{
    while (lx_next(),lx_token.type != NEWLINE_T);
    lx_unread();

    if (debug > 1) { /******* debugging message *******************/
        printf("*******line skept****\n");
        printf("----------------------------------------------\n");
    }
}

/*******************************************************************************
DESCRIPTION: Unread token. Token can be unread only once. If the token have
been unread or no token have been read yet an error message is printed.
*******************************************************************************/
void lx_unread()
{
    if (was_unread) {
        printf("lx_unread: internal error: token unread more than once\n");
    }
    else if (lx_token.type == EMPTY_T) {
        printf("lx_unread: internal error: no token to unread\n");
    }
    else  {
        was_unread = 1;
        if (debug > 1) { /******* debugging message *******************/
            printf("*******token unread ****\n");
            printf("----------------------------------------------\n");
        }
    }
}

/***********************************************************************
DESCRIPTION:
 Print caller supplied error message in required format,
 print current line, its number and underline the token pointed
 by tokptr. It is assumed that the token *tokptr belongs to the
 current line.
 External structure ERRINFO lx_errinfo is updated according to
 the error level supplied by the caller.
ARGUMENTS:
 A pointer to token location structure of the token to underline,
 the error level and the message to print.
RETURN:
 External structure lx_errinfo is updated in accordance with error
 level argument.
******************************************************************/
void lx_errmsg(
TOKLOC *tlptr,
int errlevel,
char *message)
{
    int i;
    char *workptr = tlptr->linestart;
    int errposno = tlptr->pos;
    int toklen   = tlptr->inputlen;

    if (errlevel > ERRLEVELNO) {
        printf("error in compiler: illegal error level\n");
        exit(2);
    }
    /* update lx_errinfo */
    lx_errinfo.maxerrlevel = max(lx_errinfo.maxerrlevel,errlevel);
    lx_errinfo.errcount[errlevel]++;

    /******* print caller message ******/
    if (strcmp(fname,"stdin")) { /* if non-interactive */
        lx_prcurline();  /* print the current line */
        printf("%s(%d,%d): %s\n",fname,tlptr->lineno,errposno,message);
    }
    else {  /* interactive message */
        printf("  "); /* skip positions occupied by system prompt */
        for (i=0; i<errposno; i++) printf(" "); /* underline the bad token */
        for (i=0; i<toklen; i++) printf("*");
        printf("\n");
        printf("%s\n",message);
    }
}


/******************************************************************
 Print current line and its number.
******************************************************************/
void lx_prcurline()
{
    char *workptr = lx_loc.linestart;

    printf("line %.4d: ",lx_loc.lineno);
    while (!strchr("\n\0\032",*workptr)) {
        printf("%c",*workptr++);
    }
    printf("\n");
}


/******************************************************************
 Convert string to integer.
******************************************************************/
static int myatoi(char *s)
{
    int n,sign;

    while ((int)*s == ' ' || (int)*s == '\t') s++;    /* skip white space */

    sign = 1;
    if ((int)*s == '+') s++;   /* read the sign */
    else if ((int)*s == '-') {
        sign = -1;
        s++;
    }
    for (n = 0; (int)*s >= '0' && (int)*s <= '9'; s++)
        n = 10 * n + *s - '0';
    return(sign * n);
}

/******************************************************************
 Return token name given token number. Used for diagnostic messages
******************************************************************/
char *lx_tokname(int type)
{
    static char name[20]; /* a string to form token name */

    switch (LX_GENTYPE(type)) {
    case WS_T:      return("white space");   break;
    case NEWLINE_T: return("newline");       break;
    case EOF_T:     return("end of file");   break;
    case SEPAR_T:
        sprintf(name,"separator %c",LX_SPECTYPE(type));
        return(name);
    break;
    case OPER_T:
        sprintf(name,"operator %c",LX_SPECTYPE(type));
        return(name);
    break;
    case IDENT_T:   return("identifier");    break;
    case INT_T:     return("integer");       break;
    case DOUBLE_T:  return("double");        break;
    case QSTRING_T: return("quoted string"); break;
    case EMPTY_T:   return("empty token");   break;
    case ERR_T:    return("invalid token");  break;
    default:;
        printf("*****tokname: internal error: invalid token number\n");
        exit(1);
    }
}
