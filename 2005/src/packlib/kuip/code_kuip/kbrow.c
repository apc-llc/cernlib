/*
 * $Id: kbrow.c,v 1.1.1.1 1996/03/08 15:32:52 mclareni Exp $
 *
 * $Log: kbrow.c,v $
 * Revision 1.1.1.1  1996/03/08 15:32:52  mclareni
 * Kuip
 *
 */
/*CMZ :  2.07/06 14/06/95  15.23.02  by  Gunter Folger*/
/*-- Author :    Alfred Nathaniel   07/08/92*/
/* kbrow.c */

#include "kuip/kuip.h"
#include "kuip/kfor.h"
#include "kuip/kbrow.h"
#include "kuip/klink.h"
#include "kuip/kmenu.h"
#include "kuip/kflag.h"
#include "kuip/ksig.h"

EXTERN char cdf_def[80], cdf_value[80];


static KmClass  *kmclasses;     /* list of object classes */
static BrObject *brobjects;     /* list of browsable objects */


/* internal functions */
static void  decode_setting( BrObject*, const char* );
static BrClass* find_brclass( const char* );
static KmObject* find_kmobject( const char* );
static BrVariable* find_variable( BrObject*, const char* );
static void  free_variables( BrVariable* );


/*
 * add to the list of browsable classes
 */
void klnkbrcl( BrClass *brcls_list,
              int version )
{
  BrClass *brcls;

  strcpy ( cdf_def, "Browser Class" );
  strcpy ( cdf_value, brcls_list->name );
  check_version( version, KUIP_VERSION );

  /* fill the reference to the class structure in the open action */
  for( brcls = brcls_list; brcls != NULL; brcls = brcls->next ) {
    BrAction *bract;
    for( bract = brcls->open; bract != NULL; bract = bract->next )
      bract->class = brcls;
  }

  if( brclasses == NULL )
    brclasses = brcls_list;
  else {
    for( brcls = brclasses; brcls->next != NULL; )
      brcls = brcls->next;
    brcls->next = brcls_list;
  }
}


/*
 * add to the list of object classes
 */
void klnkkmcl( KmClass *kmcls_list,
              int version )
{
  strcpy ( cdf_def, "Object Class" );
  strcpy ( cdf_value, kmcls_list->name );
  check_version( version, KUIP_VERSION );

  if( kmclasses == NULL )
    kmclasses = kmcls_list;
  else {
    KmClass *kmcls = kmclasses;

    while( kmcls != NULL ) {

      /* redefinition of class name ? */
      if( strcasecmp( kmcls->name, kmcls_list->name ) == 0 ) {

        /* copy new attributes */
        if( kmcls_list->title != NULL )
          kmcls->title = kmcls_list->title;
        if( kmcls_list->big_icon != NULL )
          kmcls->big_icon = kmcls_list->big_icon;
        if( kmcls_list->sm_icon != NULL )
          kmcls->sm_icon = kmcls_list->sm_icon;
        if( kmcls_list->user_icon_F != NULL )
          kmcls->user_icon_F = kmcls_list->user_icon_F;
        if( kmcls_list->user_icon_C != NULL )
          kmcls->user_icon_C = kmcls_list->user_icon_C;

        /* append actions */
        if( kmcls->cont == NULL )
          kmcls->cont = kmcls_list->cont;
        else if( kmcls_list->cont != NULL ) {
          BrAction *bract = kmcls->cont;
          while( bract->next != NULL )
            bract = bract->next;
          bract->next = kmcls_list->cont;
        }

        if( kmcls->graf == NULL )
          kmcls->graf = kmcls_list->graf;
        else if( kmcls_list->graf != NULL ) {
          BrAction *bract = kmcls->graf;
          while( bract->next != NULL )
            bract = bract->next;
          bract->next = kmcls_list->graf;
        }

        if( kmcls_list->next != NULL )
          klnkkmcl( kmcls_list->next, version );
        return;
      }
      else if( kmcls->next == NULL ) {
        /* append new class name at end of list */
        kmcls->next = kmcls_list;
        if( kmcls_list->next != NULL ) {
          KmClass *next = kmcls_list->next;
          kmcls_list->next = NULL;
          klnkkmcl( next, version );
        }
        return;
      }
      else
        kmcls = kmcls->next;
    }
  }
}


/*
 * add to the list of buttons
 */
void klnkbutt( KmButton *kmbut_list,
              int version )
{
  if (kmbut_list->menu != NULL) {
      strcpy ( cdf_def, "Menu" );
      strcpy ( cdf_value, kmbut_list->menu );
  } else {
      strcpy ( cdf_def, "Button" );
      strcpy ( cdf_value, kmbut_list->label );
  }
  check_version( version, KUIP_VERSION );

  if( kmbuttons == NULL )
    kmbuttons = kmbut_list;
  else {
    KmButton *kmbut = kmbuttons;
    while( kmbut->next != NULL )
      kmbut = kmbut->next;
    kmbut->next = kmbut_list;
  }
}


/*
 * return widget ID for a button
 */
KmWidget find_button( const char *label,
                     const char *menu )
{
  KmButton *kmbut;

  if( menu == NULL ) {
    /* search for button */
    for( kmbut = kmbuttons; kmbut != NULL; kmbut = kmbut->next ) {
      if( kmbut->menu == NULL &&
         strcasecmp( kmbut->label, label ) == 0 )
        return kmbut->widget;
    }
  }
  else {
    /* search for menu item */
    for( kmbut = kmbuttons; kmbut != NULL; kmbut = kmbut->next ) {
      if( kmbut->menu != NULL && strcasecmp( kmbut->menu, menu ) == 0 &&
         strcasecmp( kmbut->label, label ) == 0 )
        return kmbut->widget;
    }
  }
  return (KmWidget)0;
}


/*
 * perform the action
 */
void exec_action( BrAction *bract,
                 const char *brobj_name,
                 const char *kmobj_name,
                 int force_panel,
                 KmWidget widget,
                 KmCalldata calldata )
{
  kc_break.intr_pending = 0;

  if( bract == NULL ) return;
  if( bract->exec != NULL ) {
    int status;
    int want_prompt = force_panel ? -1 : 1;
    char cmd[256];
    char *s, *p;
    int n;

    for( s = bract->exec, n = 0; *s != '\0' && n < (sizeof cmd) - 1; ) {
      if( *s == '[' ) {           /* variable substitution */
        if( (p = strchr( s, ']' )) != NULL ) {
          int len = p - s - 1;
          char *var = strndup( s + 1, len );
          const char *val = NULL;
          int lval = 0;

          if( bract->tag == BRACT_GRAF ) {
            /*
             * in the graf window kmobj_name contains the comma separated
             * list of the object hierarchy returned by IGOBJ
             * "this" refers to the lowest level
             * "this1", "this2", ... refer to the higher levels
             */
            if( strcasecmp( var, "this" ) == 0
               || ( len == 5 && isdigit( var[4] )
                   && strncasecmp( var, "this", 4 ) == 0 ) ) {
              int k;
              for( k = (len == 4) ? 0 : atoi( &var[4] ), val = kmobj_name;
                  k > 0 && val != NULL; k-- ) {
                val = strchr( val, ',' );
                if( val != NULL )
                  val++;
              }
              if( val != NULL ) {
                const char *comma = strchr( val, ',' );
                if( comma != NULL )
                  lval = comma - val;
              }
            }
          }

          else if( strcasecmp( var, "this" ) == 0 )
            val = kmobj_name;

          else if( bract->tag == BRACT_CONT
                  && strcasecmp( var, "that" ) == 0 ) {
            /* "that" is replaced by the short text used as alias name */
            KmObject *kmobj = find_kmobject( kmobj_name );
            if( kmobj != NULL )
              val = kmobj->stext;
          }

          else if( brobj_name != NULL )
            val = get_variable( brobj_name, var );

          free( var );

          if( val != NULL ) {
            int quote = 0;
            if( lval == 0 ) {
              lval = strlen( val );
              quote = (strchr( val, ' ' ) != NULL);
            }
            if( quote )
              cmd[n++] = '\'';
            strncpy( &cmd[n], val, lval );
            n += lval;
            if( quote )
              cmd[n++] = '\'';
            s = p + 1;
            continue;
          }
        }
      }
      cmd[n++] = *s++;
    }
    cmd[n] = '\0';
    if( kc_flags.echo_command > 0 )
      printf( "%s> %s\n", kc_flags.echo_prompt, cmd );

    status = exec_cmd_string( cmd, want_prompt, kjmpaddr.disp_panel_C );
    if( status == KUMAC_UNWIND )
      return;                   /* do not go into callback function */
  }
  if( bract->call_F != NULL || bract->call_C != NULL ) {
    BrObject *brobj;
    BrClass  *brcls;
    char     *brcls_name;
    char     *path;
    KmClass  *kmcls;
    const char *kmcls_name;
    KmObject *kmobj;
    char     *short_text;
    char     *long_text;
    BrClientdata clientdata;

    if( bract->call_C != NULL ) {
      clientdata.tag = bract->tag;
      clientdata.brobj = NULL;
      clientdata.brcls = NULL;
      clientdata.path = NULL;
      clientdata.kmobj = NULL;
      clientdata.kmcls = NULL;
      clientdata.stext = NULL;
      clientdata.ltext = NULL;
      clientdata.mtext = bract->text;
    }

    switch( bract->tag ) {

    case BRACT_OPEN:
      brcls_name = bract->class->name;
      if( bract->call_F != NULL ) {
        int lbrcls_name = strlen( brcls_name );
        F77_CALL_C(bract->call_F,brcls_name,lbrcls_name);
      }
      else if( bract->call_C != NULL ) {
        clientdata.brcls = brcls_name;
        (*bract->call_C)(widget,&clientdata,calldata);
      }
      break;

    case BRACT_ROOT:
      brobj = find_brobject( brobj_name );
      brcls = brobj->class;
      brcls_name = brcls->name;
      path = get_variable( brobj_name, "path" );
      if( bract->call_F != NULL ) {
        int lbrobj_name = strlen( brobj_name );
        int lbrcls_name = strlen( brcls_name );
        int lpath = strlen( path );
        F77_CALL_C3(bract->call_F,brobj_name,lbrobj_name,
                    brcls_name,lbrcls_name,path,lpath);
      }
      else if( bract->call_C != NULL ) {
        clientdata.brobj = brobj_name;
        clientdata.brcls = brcls_name;
        clientdata.path = path;
        (*bract->call_C)(widget,&clientdata,calldata);
      }
      break;

    case BRACT_CONT:
      brobj = find_brobject( brobj_name );
      brcls = brobj->class;
      brcls_name = brcls->name;
      path = get_variable( brobj_name, "path" );
      kmobj = find_kmobject( kmobj_name );
      kmcls = kmobj->class;
      kmcls_name = kmcls->name;
      short_text = kmobj->stext;
      long_text = kmobj->ltext;
      if( bract->call_F != NULL ) {
        int lbrobj_name = strlen( brobj_name );
        int lbrcls_name = strlen( brcls_name );
        int lpath = strlen( path );
        int lkmobj_name = strlen( kmobj_name );
        int lkmcls_name = strlen( kmcls_name );
        int lshort_text;
        int llong_text;
        if( short_text == NULL )
          short_text = " ";
        lshort_text = strlen( short_text );
        if( long_text == NULL )
          long_text = " ";
        llong_text = strlen( long_text );
        F77_CALL_C7(bract->call_F,brobj_name,lbrobj_name,
                    brcls_name,lbrcls_name,path,lpath,
                    kmobj_name,lkmobj_name,kmcls_name,lkmcls_name,
                    short_text,lshort_text,long_text,llong_text);
      }
      else if( bract->call_C != NULL ) {
        clientdata.brobj = brobj_name;
        clientdata.brcls = brcls_name;
        clientdata.path = path;
        clientdata.kmobj = kmobj_name;
        clientdata.kmcls = kmcls_name;
        clientdata.stext = short_text;
        clientdata.ltext = long_text;
        (*bract->call_C)(widget,&clientdata,calldata);
      }
      break;

    case BRACT_GRAF:
      kmcls_name = brobj_name;  /* special !!! */
      if( bract->call_F != NULL ) {
        int lkmobj_name = strlen( kmobj_name );
        int lkmcls_name = strlen( kmcls_name );
        F77_CALL_CC(bract->call_F,kmobj_name,lkmobj_name,
                    kmcls_name,lkmcls_name);
      }
      else if( bract->call_C != NULL ) {
        clientdata.kmobj = kmobj_name;
        clientdata.kmcls = kmcls_name;
        (*bract->call_C)(widget,&clientdata,calldata);
      }
      break;
    }
  }
}


/*
 * find action structure with given menu text
 *
 * if user_text == NULL leave it unchanged
 * if user_text == "" revert to CDF text
 * otherwise set new user text
 *
 * if sensitive|toggle_on < 0 leave it unchanges
 * otherwise set new value
 *
 * return -1 if structure not found
 * otherwise return present sensitivity and toggle state
 */
int set_action( const char *class_name,
               int is_browsable, /* select object or browsable class */
               int graf_or_open, /* select CONT/GRAF or ROOT/OPEN */
               const char *text,
               const char *user_text,
               int sensitive,
               int toggle_on )
{
  BrAction *bract = NULL;
  int states = -1;

  if( is_browsable ) {
    BrClass *brcls = find_brclass( class_name );
    if( brcls != NULL )
      bract = graf_or_open ? brcls->open : brcls->root;
  }
  else {
    KmClass *kmcls = find_kmclass( class_name );
    if( kmcls != NULL )
      bract = graf_or_open ? kmcls->graf : kmcls->cont;
  }

  for( ; bract != NULL; bract = bract->next ) {
    if( strcasecmp( bract->text, text ) == 0 ) {

      /* prepare the return value of present states */
      states = 0;
      if( bract->flags & BrActSensitive )
        states |= 1;
      if( bract->flags & BrActToggleOn )
        states |= 2;

      if( sensitive >= 0 ) {
        bract->flags &= ~BrActSensitive;
        if( sensitive )
          bract->flags |= BrActSensitive;
      }
      if( toggle_on >= 0 ) {
        bract->flags &= ~BrActToggleOn;
        if( toggle_on )
          bract->flags |= BrActToggleOn;
      }
      if( user_text != NULL ) {
        if( bract->user_text != NULL )
          free( bract->user_text );
        bract->user_text = str0dup( user_text );
      }
      break;
    }
  }
  return states;
}


/*
 * build the list of all browsable objects to be displayed in the root window
 */
BrObject *scan_brobjects()
{
  BrObject *save_brobjects = brobjects;
  BrObject *brobj = NULL;
  BrClass *brcls;

  brobjects = NULL;

  /* scan through the list of browsable classes */
  for( brcls = brclasses; brcls != NULL; brcls = brcls->next ) {
    char *root;

    if( brcls->scan_br_F == NULL && brcls->scan_br_C == NULL ) {
      /* no scan function --> only one object with name = class name */
      if( brobjects == NULL )
        brobj = brobjects = (BrObject*)malloc( sizeof(BrObject) );
      else
        brobj = brobj->next = (BrObject*)malloc( sizeof(BrObject) );
      brobj->next = NULL;
      brobj->name = strdup( brcls->name );
      brobj->class = brcls;
      brobj->vars = NULL;
      set_variable( brobj, "name", brobj->name );
    }
    else {
      /* call the user scan function to retrieve the list of browsables */
      char *brcls_name = brcls->name;
      int  lbrcls_name = strlen( brcls_name );
      char brobj_name[32];
      int lbrobj_name = (sizeof brobj_name);
      char brvar_set[256];
      int lbrvar_set = (sizeof brvar_set);
      int first_call = 1;

      memset( brobj_name, ' ', lbrobj_name );
      while( 1 ) {
        char *name;
        char *vars;

        if( brcls->scan_br_F != NULL ) {
          memset( brvar_set, ' ', lbrvar_set );
          F77_CALL_C3(brcls->scan_br_F,brcls_name,lbrcls_name,
                      brobj_name,lbrobj_name,brvar_set,lbrvar_set);

          name = fstr0trim( brobj_name, lbrobj_name );
          if( name == NULL )
            break;              /* empty object name flags end of list */
          vars = fstr0dup( brvar_set, lbrvar_set );
        }
        else {
          char **brobj_dsc = (*brcls->scan_br_C)( brcls->name, first_call );
          if( brobj_dsc == NULL )
            break;
          first_call = 0;
          name = str0dup( brobj_dsc[0] );
          if( name == NULL )
            break;
          vars = str0dup( brobj_dsc[1] );
        }

        if( brobjects == NULL )
          brobj = brobjects = (BrObject*)malloc( sizeof(BrObject) );
        else
          brobj = brobj->next = (BrObject*)malloc( sizeof(BrObject) );

        brobj->next = NULL;
        brobj->name = name;
        brobj->class = brcls;
        brobj->vars = NULL;
        set_variable( brobj, "name", brobj->name );
        root = str2dup( "//", brobj->name );
        set_variable( brobj, "root", root );
        free( root );

        if( vars != NULL ) {
          decode_setting( brobj, vars );
          free( vars );
        }
      }
    }
  }

  for( brobj = save_brobjects; brobj != NULL; ) {
    /* free allocated space in the previous list of browsable objects */
    BrObject *next = brobj->next;
    BrObject *new_brobj = find_brobject( brobj->name );

    if( new_brobj != NULL ) {
      /* restore previous variable settings */
      BrVariable *brvar;
      for( brvar = brobj->vars; brvar != NULL; brvar = brvar->next ) {
        BrVariable *new_brvar = find_variable( new_brobj, brvar->name );
        if( new_brvar == NULL )
          set_variable( new_brobj, brvar->name, brvar->value );
      }
    }
    free_variables( brobj->vars );
    free( brobj->name );
    free( brobj );
    brobj = next;
  }
  return brobjects;
}


/*
 * build the list of all objects to be displayed in the content window
 */
KmObject *scan_kmobjects( const char *brobj_name,
                         const char *path_name,
                         int browser_number )
{
  static char dummy_path[] = "//...";
  static int ldummy_path = (sizeof dummy_path) - 1;
  int lbrobj_name = strlen( brobj_name );
  const char *path = path_name;
  int lpath;
  char kmobj_name[32];
  int lkmobj_name = (sizeof kmobj_name);
  char kmcls_name[32];
  int lkmcls_name = (sizeof kmcls_name);
  char short_text[80];
  int lshort_text = (sizeof short_text);
  char long_text[80];
  int llong_text = (sizeof long_text);
  char *brcls_name;
  int lbrcls_name;
  BrObject *brobj = find_brobject( brobj_name );
  KmObject *kmobj;
  KmClass  *kmcls;
  char path_n[16];
  int i;

  if( brobj == NULL ) {
    printf( "scan_kmobjects: browsable object %s not found\n", brobj_name );
    return NULL;
  }

  brcls_name = brobj->class->name;
  lbrcls_name = strlen( brcls_name );

  if( strncmp( path_name, dummy_path, ldummy_path ) == 0 ) {
    /* ignore the dummy path header */
    path = path_name + ldummy_path;
  }
  lpath = strlen( path );

  set_variable( brobj, "path", path );
  /* remember the last path setting in each browser */
  sprintf( path_n, "path_%d", browser_number );
  set_variable( brobj, path_n, path );

  /* reset the object counters in the class structure used for "summary" */
  for( kmcls = kmclasses; kmcls != NULL; kmcls = kmcls->next )
    kmcls->obj_count = 0;

  /* free allocated space in the existing list of objects */
  for( kmobj = kmobjects; kmobj != NULL; ) {
    KmObject *next = kmobj->next;
    free( kmobj->name );
    if( kmobj->stext != NULL )
      free( kmobj->stext );
    if( kmobj->ltext != NULL )
      free( kmobj->ltext );
    free( kmobj );
    kmobj = next;
  }
  kmobjects = NULL;

  /* call the user scan function to retrieve the list of objects */
/*
 *   SUBROUTINE scan_km(BRNAME,BRCLASS,PATH,OBJNAME,KMCLASS,STEXT,LTEXT)
 *   CHARACTER*(*) BRCLASS,BRNAME,PATH,OBJNAME,KMCLASS,STEXT,LTEXT
 *
 * input  BRNAME   : name of the browsable object, e.g. 'LUN1'
 * input  BRCLASS  : class name of the browsable object, e.g. 'HBOOK'
 * input  PATH     : path of selected directory, e.g '//LUN1' or '//LUN1/DIR'
 * in/out OBJNAME  : name of the next object
 * output KMCLASS  : class name of the object
 * output STEXT    : short text describing the object
 * output LTEXT    : long text describing the object
 *
 * If called with OBJNAME=' ' this routine has to return the first object
 * in the current directory of the browsable object BRNAME. Otherwise it has
 * to return the next object. If there is no next object it has to return
 * OBJNAME=' '.
 *
 *
 *   SUBROUTINE scan_br(BRCLASS,BRNAME,VARSET)
 *   CHARACTER*(*) BRCLASS,BRNAME,VARSET
 *
 */

  for( i = 0; 1; i++ ) {
    char *oname = NULL;
    char *cname = NULL;
    char *stext = NULL;
    char *ltext = NULL;

    if( brobj->class->scan_km_F != NULL ) {
      if( i == 0 )
        memset( kmobj_name, ' ', lkmobj_name );
      memset( short_text, ' ', lshort_text );
      memset( long_text, ' ', llong_text );
      F77_CALL_C7(brobj->class->scan_km_F,
                  brobj_name,lbrobj_name,brcls_name,lbrcls_name,path,lpath,
                  kmobj_name,lkmobj_name,kmcls_name,lkmcls_name,
                  short_text,lshort_text,long_text,llong_text);
      oname = fstr0trim( kmobj_name, lkmobj_name );
      cname = fstr0trim( kmcls_name, lkmcls_name );
      stext = fstr0trim( short_text, lshort_text );
      ltext = fstr0dup( long_text, llong_text );
    }
    else {
      char **obj_desc = (*brobj->class->scan_km_C)( brobj_name, brcls_name,
                                                   path, i );
      if( obj_desc != NULL ) {
        oname = str0dup( obj_desc[0] );
        cname = str0dup( obj_desc[1] );
        stext = str0dup( obj_desc[2] );
        ltext = str0dup( obj_desc[3] );
      }
    }

    if( oname == NULL ) {
      if( ltext == NULL ) {
        /* construct summary from object count in class structure */
        int  len = 0;
        ltext = malloc( 1 );
        ltext[0] = '\0';
        for( kmcls = kmclasses; kmcls != NULL; kmcls = kmcls->next ) {
          if( kmcls->obj_count != 0 ) {
            char *sp1 = len > 0 ? "   " : "";
            int  nsp1 = strlen( sp1 );
            char *txt = kmcls->title;
            int  ntxt = strlen( txt );
            char *sp2 = " : ";
            int  nsp2 = strlen( sp2 );
            char  cnt[32];
            int  ncnt;
            sprintf( cnt, "%d", kmcls->obj_count );
            ncnt = strlen( cnt );
            ltext = realloc( ltext, len + nsp1 + ntxt + nsp2 + ncnt + 1 );
            strcpy( &ltext[len], sp1 ); len += nsp1;
            strcpy( &ltext[len], txt ); len += ntxt;
            strcpy( &ltext[len], sp2 ); len += nsp2;
            strcpy( &ltext[len], cnt ); len += ncnt;
          }
        }
      }
      set_variable( brobj, "summary", ltext );
      free( ltext );
      break;                    /* empty object name flags end of list */
    }

    if( kmobjects == NULL )
      kmobj = kmobjects = (KmObject*)malloc( sizeof(KmObject) );
    else
      kmobj = kmobj->next = (KmObject*)malloc( sizeof(KmObject) );

    kmobj->next = NULL;
    kmobj->name = oname;
    kmobj->stext = stext;
    kmobj->ltext = ltext;

    if( (kmobj->class = find_kmclass( cname )) == NULL ) {
      printf( " *** Invalid class name %s\n", cname );
    }
    else if( strcasecmp( kmobj->class->name, "DirUpFile" ) != 0 ) {
      kmobj->class->obj_count++; /* summary count */
    }
    free( cname );
  }
  return kmobjects;
}


/*
 * return the value of variable or NULL if name not found
 */
char *get_variable( const char *brobj_name,
                   const char *brvar_name )
{
  BrObject *brobj = find_brobject( brobj_name );
  BrVariable *brvar = find_variable( brobj, brvar_name );

  if( brvar == NULL )
    return NULL;
  return brvar->value;
}


/*
 * return pointer to browsable class structure or NULL if name not found
 */
static
BrClass *find_brclass( const char *brcls_name )
{
  BrClass *brcls;

  for( brcls = brclasses; brcls != NULL; brcls = brcls->next )
    if( strcasecmp( brcls->name, brcls_name ) == 0 )
      break;
  return brcls;
}


/*
 * return pointer to browsable object structure or NULL if name not found
 */
BrObject *find_brobject( const char *brobj_name )
{
  BrObject *brobj;

  for( brobj = brobjects; brobj != NULL; brobj = brobj->next )
    if( strcasecmp( brobj->name, brobj_name ) == 0 )
      break;
  return brobj;
}


/*
 * return pointer to object class structure or NULL if name not found
 */
KmClass *find_kmclass( const char *kmcls_name )
{
  KmClass *kmcls;

  for( kmcls = kmclasses; kmcls != NULL; kmcls = kmcls->next )
    if( strcasecmp( kmcls->name, kmcls_name ) == 0 )
      break;
  return kmcls;
}


/*
 * return pointer to object structure or NULL if name not found
 */
static
KmObject *find_kmobject( const char *kmobj_name )
{
  KmObject *kmobj;

  for( kmobj = kmobjects; kmobj != NULL; kmobj = kmobj->next )
    if( strcasecmp( kmobj->name, kmobj_name ) == 0 )
      break;
  return kmobj;
}


/*
 * return pointer to variable structure or NULL if name not found
 */
static
BrVariable *find_variable( BrObject *brobj,
                          const char *brvar_name )
{
  BrVariable *brvar;

  for( brvar = brobj->vars; brvar != NULL; brvar = brvar->next )
    if( strcasecmp( brvar->name, brvar_name ) == 0 )
      break;
  return brvar;
}


/*
 * free the space allocated for a variable list
 */
static
void free_variables( BrVariable *brvar )
{
  while( brvar != NULL ) {
    BrVariable *next = brvar->next;
    free( brvar->name );
    free( brvar->value );
    free( brvar );
    brvar = next;
  }
}


/*
 * set the variable name to value
 */
void set_variable( BrObject *brobj,
                  const char *brvar_name,
                  const char *brvar_value )
{
  BrVariable *brvar = find_variable( brobj, brvar_name );

  if( brvar == NULL ) {
    /* allocate a new variable */
    if( brobj->vars == NULL )
      brvar = brobj->vars = (BrVariable*)malloc( sizeof(BrVariable) );
    else {
      for( brvar = brobj->vars; brvar->next != NULL; brvar = brvar->next ) ;
      brvar = brvar->next = (BrVariable*)malloc( sizeof(BrVariable) );
    }
    brvar->next = NULL;
    brvar->name = strdup( brvar_name );
  }
  else if( brvar->value != NULL )       /* change value of existing variable */
    free( brvar->value );

  if( brvar_value == NULL )
    brvar->value = NULL;
  else
    brvar->value = strdup( brvar_value );
}


/*
 * decode blank separated variable settings "name1=value1 name2=value2 ..."
 * single quotes can be used to include blanks
 */
static
void decode_setting( BrObject *brobj,
                    const char *brvar_set )
{
  char *line = strdup( brvar_set ); /* make a local copy for strqtok() */
  char *p;
  char *name;

  for( name = strqtok( line ); name != NULL; name = strqtok( NULL ) ) {
    /* name points to the next "name=value" */
    char *value;

    name = strdup( name );
    if( (value = strchr( name, '=' )) != NULL ) {
      *value++ = '\0';
      /* name points to "name" and value points to "value" */

      if( *value == '\'' ) {
        /* remove quotes */
        if( (p = strrchr( ++value, '\'' )) != NULL )
          *p = '\0';
      }
      set_variable( brobj, name, value );
    }
    free( name );
  }
  free( line );
}
