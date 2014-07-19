/*
 * $Id: kmacro.h,v 1.1.1.1 1996/03/08 15:33:00 mclareni Exp $
 *
 * $Log: kmacro.h,v $
 * Revision 1.1.1.1  1996/03/08 15:33:00  mclareni
 * Kuip
 *
 */
/* kmacro.h: KUIP macros */

typedef enum {
  APPLICATION_Statement,
  ASSIGNMENT_Statement,
  BREAKL_Statement,
  CASE_Label,
  CASE_Statement,
  COMMAND_Statement,
  DO_Statement,
  ELSE_Statement,
  ELSEIF_Statement,
  ENDCASE_Statement,
  ENDDO_Statement,
  ENDFOR_Statement,
  ENDIF_Statement,
  ENDWHILE_Statement,
  EOF_REACHED,
  EXITM_Statement,
  FOR_Statement,
  GOTO_Label,
  GOTO_Statement,
  IF_GOTO_Statement,
  IF_THEN_Statement,
  MACRO_Statement,
  NEXTL_Statement,
  OFF_ERROR_Statement,
  ON_ERROR_Statement,
  ON_ERROR_CONTINUE,
  ON_ERROR_EXITM,
  ON_ERROR_GOTO,
  ON_ERROR_STOPM,
  READ_Statement,
  REPEAT_Statement,
  RETURN_Statement,
  SHIFT_Statement,
  STOPM_Statement,
  UNTIL_Statement,
  WHILE_Statement,
  SYNTAX_ERROR
} KumacStatement;

typedef struct _KumacCode {
  int         (*op)();
  char         *arg[2];
} KumacCode;

typedef struct _KumacMacro {
  char         *name;
  struct _KumacMacro *next;
  int          ncode;
  KumacCode    *code;
} KumacMacro;

typedef struct _KumacFile {
  char        *path;
  int          stamp;
  KumacMacro  *macros;
} KumacFile;

typedef struct _ExecStack {
  KumacFile     *file;
  KumacMacro    *macro;
  char          *argline;
  char          *line_number;
  char          *source_line;
  HashTable     *variables;
  KumacStatement on_error_stmt;
  char          *on_error_arg;
  KumacStatement off_error_stmt;
  char          *off_error_arg;
  int            pc;
  int            status;
} ExecStack;

typedef struct _NestStack {
  KumacStatement stmt;
  char          *var;
  char           tmp1[16];
  char           tmp2[16];
  char           label1[16];
  char           label2[16];
  char           label3[16];
} NestStack;

extern void store_variable( ExecStack*, const char* name, const char* value );

