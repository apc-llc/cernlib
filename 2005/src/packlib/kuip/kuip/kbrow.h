/*
 * $Id: kbrow.h,v 1.2 1996/04/16 13:26:38 gunter Exp $
 *
 * $Log: kbrow.h,v $
 * Revision 1.2  1996/04/16 13:26:38  gunter
 * Mods to compile kuipc. The includes needed only for kuipc where moved
 *  to kuipc, together with m4 files to create/update these header files.
 *
 * Revision 1.1.1.1  1996/03/08 15:32:59  mclareni
 * Kuip
 *
 */
/* kbrow.h: browser definitions */

#define __ /* nul */

#include "kbrow1.h"

#include "kbrow2.h"

#include "kbrow3.h"

#include "kbrow4.h"

#undef __

typedef struct _KmObject {
  struct _KmObject *next;       /* link to next object definition */
  char       *name;             /* unique identifier name */
  char       *stext;            /* short description text */
  char       *ltext;            /* long description text */
  KmClass    *class;            /* pointer to objects's class structure */
} KmObject;

typedef struct _BrVariable {
  struct _BrVariable *next;     /* link to next variable definition */
  char       *name;             /* variable name */
  char       *value;            /* replacement value */
} BrVariable;

typedef struct _BrObject {
  struct _BrObject *next;       /* link to next browsable object */
  char       *name;             /* name of the browsable object */
  BrClass    *class;            /* pointer to browsable's class structure */
  BrVariable *vars;             /* linked list of variable substitutions */
} BrObject;

typedef struct _BrClientdata {
  BrActTag    tag;
  const char *brobj;
  const char *brcls;
  const char *path;
  const char *kmobj;
  const char *kmcls;
  const char *stext;
  const char *ltext;
  const char *mtext;
} BrClientdata;

EXTERN BrClass  *brclasses;
EXTERN KmObject *kmobjects;
EXTERN KmButton *kmbuttons;

extern void  klnkbrcl( BrClass*, int );
extern void  klnkkmcl( KmClass*, int );
extern void  klnkicon( KmIcon*, int );
extern void  klnkbutt( KmButton*, int );

extern void  exec_action( BrAction*, const char*, const char*, int,
                         KmWidget, KmCalldata );
extern KmWidget find_button( const char*, const char* );
extern KmIcon* find_kmicon( const char* );
extern KmClass* find_kmclass( const char* );
extern BrObject* find_brobject( const char* );
extern char* get_variable( const char*, const char* );
extern BrObject* scan_brobjects(void);
extern KmObject* scan_kmobjects( const char*, const char*, int );
extern int   set_action( const char*, int, int, const char*, const char*,
                        int, int );
extern void  set_variable( BrObject*, const char*, const char* );


